#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

std::string mml::type_checker::pointRefTypeToString(std::shared_ptr<cdk::basic_type> type) {
  if (!type) 
    return "null";
  if (type->name() == cdk::TYPE_VOID) 
    return "void";
  if (type->name() == cdk::TYPE_UNSPEC) 
    return "var";
  if (type->name() == cdk::TYPE_INT) 
    return "int";
  if (type->name() == cdk::TYPE_DOUBLE) 
    return "double";
  if (type->name() == cdk::TYPE_STRING) 
    return "text";
  if (type->name() == cdk::TYPE_POINTER) {
    return "[" + pointRefTypeToString(cdk::reference_type::cast(type)->referenced()) + "]";
  }
  return "type doesn't exist";
}

void mml::type_checker::do_BooleanLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void mml::type_checker::processPIDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

void mml::type_checker::processIDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

void mml::type_checker::processIntExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (left)");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::processScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT) && !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer or double expression expected in binary logical expression (left)");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer expression expected in binary logical expression (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::processGeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->type() != node->right()->type()) {
    throw std::string("same type expected on both sides of equality operator");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}
void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in unary logical expression");
  }
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processPIDExpression(node, lvl);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processPIDExpression(node, lvl);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processIDExpression(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processIDExpression(node, lvl);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processIntExpression(node, lvl);
}
void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processGeneralLogicalExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processGeneralLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      throw std::string("wrong assignment to integer");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->lvalue()->type());
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      cdk::integer_node *i = dynamic_cast<cdk::integer_node *>(node->rvalue());
      if(i != nullptr && i->value() == 0) {
        node->type(node->lvalue()->type());
      }
      else {
        throw std::string("rvalue must be a pointer of the same type or integer literal 0");
      }
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } 
    else {
      throw std::string("wrong assignment to pointer");
    }

  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {

    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else {
      throw std::string("wrong assignment to real");
    }

  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {
    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else {
      throw std::string("wrong assignment to string");
    }
  } 
  else if(node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)){
    // std::cout << node->rvalue()->label() << "\n";
    if(node->rvalue()->label() != "function_definition_node"){
      throw std::string("assign: wrong type for initializer (function expected).");
    }
  }
  else {
    throw std::string("wrong types in assignment");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_program_node(mml::program_node *const node, int lvl) {
  //EMPTY
}

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_read_node(mml::read_node *const node, int lvl) {
  try {
    node->argument()->accept(this, lvl);
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_address_of_node(mml::address_of_node * const node, int level){
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, level + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_block_node(mml::block_node * const node, int level){
  if(node->declarations()) {
    node->declarations()->accept(this, level + 2);
  }
  if(node->instructions()) {
    node->instructions()->accept(this, level + 2);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_call_node(mml::function_call_node * const node, int level){
  ASSERT_UNSPEC;

  if(node->identifier() == nullptr && _funclevel == 0){
    throw std::string("cannot make recursive function call outside of function");
  }
  else{
    auto symbol = _symtab.find("_flevel"+std::to_string(_funclevel));;
    
    if (symbol == nullptr) throw std::string("symbol '" + symbol->name() + "' is undeclared.");

    node->type(symbol->type());

    if (node->arguments()->size() == symbol->number_of_arguments()) {
      node->arguments()->accept(this, level + 4);
      for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
        if (node->argument(ax)->type() == symbol->argument_type(ax)) continue;
        if (symbol->argument_is_typed(ax, cdk::TYPE_DOUBLE) && node->argument(ax)->is_typed(cdk::TYPE_INT)) continue;
        throw std::string("type mismatch for argument " + std::to_string(ax + 1) + " of '" + symbol->name() + "'.");
      }
    } else {
      throw std::string(
          "number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
              + std::to_string(symbol->number_of_arguments()) + ").");
    }
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_definition_node(mml::function_definition_node * const node, int level){
  std::string id = "_flevel" + std::to_string(_funclevel);
  _funclevel += 1;
  auto function = mml::make_symbol(node->type(), id, 0, false, 0,false);

  std::vector < std::shared_ptr < cdk::basic_type >> argtypes;

  for (size_t ax = 0; ax < node->arguments()->size(); ax++)
    argtypes.push_back(node->argument(ax)->type());
  function->set_argument_types(argtypes);

  if(!_symtab.insert(function->name(),function))
    _symtab.replace(function->name(),function);
  
  _parent->set_new_symbol(function);

}

//---------------------------------------------------------------------------

void mml::type_checker::do_index_node(mml::index_node * const node, int level){
  ASSERT_UNSPEC;
  std::shared_ptr < cdk::reference_type > btype;

  if (node->base()) {
    node->base()->accept(this, level + 2);
    btype = cdk::reference_type::cast(node->base()->type()); 
    if (!node->base()->is_typed(cdk::TYPE_POINTER)) throw std::string("pointer expression expected in index left-value");
    
  } else {
    btype = cdk::reference_type::cast(_function->type());
    if (!_function->is_typed(cdk::TYPE_POINTER)) throw std::string("return pointer expression expected in index left-value");
  }

  node->index()->accept(this, level + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT)) throw std::string("integer expression expected in left-value index");

  node->type(btype->referenced());
}

//---------------------------------------------------------------------------

void mml::type_checker::do_next_node(mml::next_node * const node, int level){
  //EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_return_node(mml::return_node * const node, int level){
  std::shared_ptr<mml::symbol> func; 
  
  func = _symtab.find("_flevel"+std::to_string(_funclevel));
  if (node->retval()) {
    if(func){
     
      node->retval()->accept(this, level + 2);
    
      if (func->is_typed(cdk::TYPE_INT)) {
        if (!node->retval()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for return.");
      } 
      else if (func->is_typed(cdk::TYPE_DOUBLE)) {
        if (!node->retval()->is_typed(cdk::TYPE_INT) && !node->retval()->is_typed(cdk::TYPE_DOUBLE)) {
          throw std::string("wrong type for return.");
        }
      } 
      else if (func->is_typed(cdk::TYPE_STRING)) {
        if (!node->retval()->is_typed(cdk::TYPE_STRING)) {
          throw std::string("wrong type for return.");
        }
      } 
      else if (func->is_typed(cdk::TYPE_POINTER)) {
        if(pointRefTypeToString(func->type()) != pointRefTypeToString(node->retval()->type())){
          throw std::string("wrong type for return.");
        }
      }
      else{
        if (!node->retval()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for return.");
      }
    } else {
      throw std::string("unknown type for initializer.");
    }
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sizeof_node(mml::sizeof_node * const node, int level){
  ASSERT_UNSPEC;
  node->expression()->accept(this, level + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node * const node, int level){
  ASSERT_UNSPEC;
  node->argument()->accept(this, level + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in allocation expression");
  }
  //DAVID TODO FIXME
  auto mytype = cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  node->type(mytype);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_stop_node(mml::stop_node * const node, int level){
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_declaration_node(mml::variable_declaration_node * const node, int level){
  const std::string &id = node->identifier();
  std::shared_ptr<mml::symbol> prev;

  if(node->type() == nullptr){
    prev = _symtab.find(node->identifier());
  }

  if (node->initializer() != nullptr) {
    int visitedFunction = _funclevel;
    node->initializer()->accept(this, level + 2);

    std::shared_ptr<cdk::basic_type> auxtype;
    if(node->type() != nullptr){
      auxtype = node->type();
    }
    else if(prev != nullptr && prev->type() != nullptr){
      auxtype = prev->type();
    }
    else{
      throw std::string("There is no type declared;");
    }

    if(_funclevel > visitedFunction){
      visitedFunction = -1;  //visited function
    }
    else{
      visitedFunction = 0;  //didn't visit function
    }

    if(auxtype->name() == cdk::TYPE_UNSPEC){
      if(visitedFunction == -1){
        _funclevel -= 1;

        std::vector < std::shared_ptr < cdk::basic_type >> argtypes;
        std::shared_ptr<mml::symbol> func = _symtab.find("_flevel" + std::to_string(_funclevel));
        
        for (size_t ax = 0; ax < func->number_of_arguments(); ax++)
          argtypes.push_back(func->argument_type(ax));
        
        node->type(cdk::functional_type::create(argtypes,node->initializer()->type()));
      }
      else{
        node->type(node->initializer()->type());
      }
    }
    else if (auxtype->name() == cdk::TYPE_INT) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for initializer (integer expected).");
    } 
    else if (auxtype->name() == cdk::TYPE_DOUBLE) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type for initializer (integer or double expected).");
      }
    } 
    else if (auxtype->name() ==cdk::TYPE_STRING) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("wrong type for initializer (string expected).");
      }
    }
    else if (auxtype->name() == cdk::TYPE_POINTER) {
      std::string initializerType = pointRefTypeToString(node->initializer()->type());
      if(initializerType != pointRefTypeToString(auxtype) && initializerType != "[null]" && initializerType != "[var]"){
        throw std::string("wrong type for initializer.");
      }
    }
    else if(auxtype->name() ==cdk::TYPE_FUNCTIONAL){
      std::cout << node->initializer()->label();
      if(node->initializer()->label() != "function_definition_node"){ // nao tenho a certeza
        throw std::string("wrong type for initializer (function expected).");
      }
    }
    else {
      throw std::string("unknown type for initializer.");
    }
  }
  if(_funclevel != 0 && (node->qualifier() == 3 || node->qualifier() == 1 || node->qualifier() == 2)){
    throw std::string("Foreign,use or public used inside a function.");
  }

  auto symbol = mml::make_symbol(node->type(), id, 0, false, node->qualifier(),(bool)node->initializer());
  if(_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
  } 
  else {
    if(prev){
      auto symbol = mml::make_symbol(prev->type(), id, 0, false, node->qualifier(),(bool)node->initializer());
      if(!_symtab.replace(id,symbol)){
        throw std::string("variable '" + id + "' redeclared");
      }
    }
  }
}
//---------------------------------------------------------------------------

void mml::type_checker::do_null_node(mml::null_node * const node, int level){
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}