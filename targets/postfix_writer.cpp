#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

//---------------------------------------------------------------------------
bool visitedFunction = false;

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.DOUBLE(node->value()); // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
}
void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
   ASSERT_SAFE_EXPRESSIONS;
   node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value()); // integer literal is on the stack: push an integer
  } else {
    _pf.SINT(node->value()); // integer literal is on the DATA segment
  }
}
void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  /* leave the address on the stack */
  if (_inFunctionBody) {
  _pf.TEXT(); // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
  }
  else{
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.ADD();
}
void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.SUB();
}
void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MUL();
}
void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.DIV();
}
void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GE();
}
void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GT();
}
void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol->global()) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  _pf.LDINT(); // depends on type size
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
 
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_program_node(mml::program_node * const node, int lvl) {
  // Note that MML doesn't have functions. Thus, it doesn't need
  // a function node. However, it must start in the main function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.

  // generate the main function (RTS mandates that its name be "_main")
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");
  _pf.ENTER(1000);  // MML doesn't implement local variables

  _inFunctionBody = true;
  node->block()->accept(this, lvl);

  // end the main function
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();

  // these are just a few library function imports
  _pf.EXTERN("readi");
  _pf.EXTERN("printi");
  _pf.EXTERN("printd");
  _pf.EXTERN("prints");
  _pf.EXTERN("println");

}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    _pf.TRASH(4); // delete the evaluated value
  } else if (node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.TRASH(8); // delete the evaluated value's address
  } else if (node->argument()->is_typed(cdk::TYPE_STRING)) {
    _pf.TRASH(4); // delete the evaluated value's address
  } else {
    std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
    exit(1);
  }
}

void mml::postfix_writer::do_print_node(mml::print_node *const node, int lvl){
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));
    child->accept(this, lvl); // determine the value to print

    if (child->is_typed(cdk::TYPE_INT)) {
      _pf.CALL("printi");
      _pf.TRASH(4); // delete the printed value
    }else if (child->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.CALL("printd");
        _pf.TRASH(8); // trash double
    } else if (child->is_typed(cdk::TYPE_STRING)) {
      _pf.CALL("prints");
      _pf.TRASH(4); // delete the printed value's address
    } else {
      std::cerr << "Illegal" << std::endl;
      exit(1);
    }
  }

  if (node->newline()) {
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_read_node(mml::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.CALL("readi");
  _pf.LDFVAL32();
  node->argument()->accept(this, lvl);
  _pf.STINT();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _whileCond.push(++_lbl);
  _whileEnd.push(++_lbl);

  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileCond.top()));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(_whileEnd.top()));


  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(_whileCond.top()));

  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileEnd.top()));

  _whileEnd.pop();
  _whileCond.pop();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS  ;
  node->lvalue()->accept(this, level + 2);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_block_node(mml::block_node * const node, int level){
  _symtab.push(); // for block-local vars
  if (node->declarations()) node->declarations()->accept(this, level+ 2);
  _inFunctionBody = true;
  if (node->instructions()) node->instructions()->accept(this, level + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_call_node(mml::function_call_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;

  auto symbol = _symtab.find("_flevel0");
  
  visitedFunction = true;
  size_t argsSize = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, level + 2);
      if (symbol->argument_is_typed(ax, cdk::TYPE_DOUBLE) && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      argsSize += symbol->argument_size(ax);
    }
  }
  _pf.CALL(symbol->name());
  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_STRING)) {
    _pf.LDFVAL32();
  } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  }
  visitedFunction = false;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_definition_node(mml::function_definition_node * const node, int level){
ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody || _inFunctionArgs) {
    error(node->lineno(), "cannot define function in body or in arguments");
    return;
  }

  // remember symbol so that args and body know
  _function = new_symbol();
  visitedFunction = true;

  _currentBodyRetLabel = mklbl(++_lbl);

  _offset = 8; // prepare for arguments (4: remember to account for return address)
  _symtab.push(); // scope of args

  if (node->arguments()->size() > 0) {
    _inFunctionArgs = true;
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      cdk::basic_node *arg = node->arguments()->node(ix);
      if (arg == nullptr) break; // this means an empty sequence of arguments
      arg->accept(this, 0); // the function symbol is at the top of the stack
    }
    _inFunctionArgs = false;
  }

  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL(_function->name(), _pf.FUNC());
  _pf.LABEL(_function->name());

  _pf.ENTER(1000); // total stack size reserved for local variables

  _offset = 0; // prepare for local variable

  _inFunctionBody = true;
  os() << "        ;; before body " << std::endl;
  node->block()->accept(this, level + 4);
  os() << "        ;; after body " << std::endl;
  _inFunctionBody = false;

  _pf.LABEL(_currentBodyRetLabel);
  _pf.LEAVE();
  _pf.RET();

  _symtab.pop();
  visitedFunction = false;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_index_node(mml::index_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;
  if (node->base()) {
    node->base()->accept(this, level);
  } else {
    if (_function) {
      _pf.LOCV(-_function->type()->size());
    } else {
      error(node->lineno(), "trying to use return value outside function");
    }
  }
  node->index()->accept(this, level);
  if (node->is_typed(cdk::TYPE_DOUBLE)) _pf.INT(3);
  else _pf.INT(2);
  _pf.SHTL();
  _pf.ADD(); // add pointer and index
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_next_node(mml::next_node * const node, int level){
  /*
  if (_whileCond.size() != 0) {
    _pf.JMP(mklbl(_whileCond.top())); // jump to next cycle
  } else
    error(node->lineno(), "'restart' outside 'while'");
  */
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_return_node(mml::return_node * const node, int level){
  // should not reach here without returning a value (if not void)
  if(visitedFunction == true){
    ASSERT_SAFE_EXPRESSIONS;
    if (_function->type()->name() != cdk::TYPE_VOID) {
      node->retval()->accept(this, level + 2);

      if (_function->type()->name() == cdk::TYPE_INT || _function->type()->name() == cdk::TYPE_STRING
          || _function->type()->name() == cdk::TYPE_POINTER) {
        _pf.STFVAL32();
      } else if (_function->type()->name() == cdk::TYPE_DOUBLE) {
        if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
        _pf.STFVAL64();
      } else {
        std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
      }
    }
  
    _pf.JMP(_currentBodyRetLabel);
  }
}


//---------------------------------------------------------------------------

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, level);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ALLOC(); // allocate
  _pf.SP(); // put base pointer in stack
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stop_node(mml::stop_node * const node, int level){
  if (_whileEnd.size() != 0) {
    _pf.JMP(mklbl(_whileEnd.top())); // jump to for end
  } else
    error(node->lineno(), "'break' outside 'while");
}


//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_declaration_node(mml::variable_declaration_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;
  auto id = node->identifier();
  int typesize = node->type()->size();

  int offset = 0;
  if (_inFunctionBody) {
    _offset -= typesize;
    offset = _offset;
  } else if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else {
    offset = 0; // global variable
  }

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (_inFunctionBody) {
    // if we are dealing with local variables, then no action is needed
    // unless an initializer exists
    if (node->initializer()) {
      node->initializer()->accept(this, level);
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } 
      else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT))
          _pf.I2D();
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } 
      else {
        std::cerr << "cannot initialize" << std::endl;
      }
    }
  } 
  else {
    if (!_function) {
      if (node->initializer() == nullptr) {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(id);
        _pf.SALLOC(typesize);
      } 
      else {
        if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);

          if (node->is_typed(cdk::TYPE_INT)) {
            node->initializer()->accept(this, level);
          } 
          else if (node->is_typed(cdk::TYPE_POINTER)) {
            node->initializer()->accept(this, level);
          } 
          else if (node->is_typed(cdk::TYPE_DOUBLE)) {
            if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
              node->initializer()->accept(this, level);
            } 
            else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
              cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node->initializer());
              cdk::double_node ddi(dclini->lineno(), dclini->value());
              ddi.accept(this, level);
            } 
            else {
              std::cerr << node->lineno() << ": '" << id << "' has bad initializer for real value\n";
            }
          }
        } 
        else if (node->is_typed(cdk::TYPE_STRING)) {
            _pf.DATA();
            _pf.ALIGN();
            _pf.LABEL(id);
            node->initializer()->accept(this, level);
        } 
        else if (node->is_typed(cdk::TYPE_FUNCTIONAL)){
          node->initializer()->accept(this,level);
        }
        else {
          std::cerr << node->lineno() << ": '" << id << "' has unexpected initializer\n";
        }

      }
    }

  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_null_node(mml::null_node * const node, int level){
  ASSERT_SAFE_EXPRESSIONS;
  // a pointer is a 32-bit integer
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}