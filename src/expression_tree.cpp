#include "expression_tree.h"
#include "resolver.h"

namespace custom_hdl {

double ReferenceNode::evaluate() const {
    return reference->value;
}

} // namespace custom_hdl
