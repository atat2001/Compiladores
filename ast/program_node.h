#ifndef __MML_AST_PROGRAM_NODE_H__
#define __MML_AST_PROGRAM_NODE_H__

#include <cdk/ast/basic_node.h>

namespace mml {

  /**
   * Class for describing program nodes.
   */
  class program_node: public cdk::basic_node {
    mml::block_node *_block;

  public:
    inline program_node(int lineno, mml::block_node *block) :
        cdk::basic_node(lineno), _block(block) {
    }

  public:
    inline mml::block_node *block() {
      return _block;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_program_node(this, level);
    }

  };

} // mml

#endif
