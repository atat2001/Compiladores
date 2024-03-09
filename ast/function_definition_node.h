#ifndef __MML_AST_FUNCTION_DEFINITION_NODE_H__
#define __MML_AST_FUNCTION_DEFINITION_NODE_H__

#include <cdk/types/basic_type.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"

namespace mml {

    /**
    * Class for describing function definition nodes.
    */
   
    class function_definition_node: public cdk::expression_node {
        cdk::sequence_node *_arguments;
        mml::block_node *_block;
        std::shared_ptr<cdk::basic_type> _type;

    public:
        inline function_definition_node(int lineno, mml::block_node *block) :
            cdk::expression_node(lineno), _arguments(nullptr), _block(block) {
          type(cdk::primitive_type::create(0, cdk::TYPE_INT)); 
        }

        inline function_definition_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, std::shared_ptr<cdk::basic_type> outputType) :
            cdk::expression_node(lineno), _arguments(arguments), _block(block) {
          type(outputType);
        }
     
    public:
        inline cdk::sequence_node* arguments() {
            return _arguments;
        }

        inline mml::block_node* block() {
            return _block;
        }

        std::shared_ptr<cdk::basic_type> returnType() {
            return _type;
        }

        void setType(std::shared_ptr<cdk::basic_type> type){
            _type = type;
        }

        cdk::typed_node* argument(size_t ax) {
            return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
        }

        void accept(basic_ast_visitor *sp, int level) {
            sp->do_function_definition_node(this, level);
        }

    };

} // mml

#endif