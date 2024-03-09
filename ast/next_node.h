#ifndef __MML_NEXT_NODE_H__
#define __MML_NEXT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {
  /**
   * Class for describing next nodes.
   */
  class next_node: public cdk::basic_node {
    int _number;
  
    public:
    inline next_node(int lineno, int number = 1) :
      cdk::basic_node(lineno), _number(number) {}

    public:
    int number() const { return _number; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_next_node(this, level);
    }

  };

} // mml

#endif