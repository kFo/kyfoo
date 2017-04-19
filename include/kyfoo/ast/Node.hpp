#pragma once

#include <kyfoo/ast/IO.hpp>

namespace kyfoo {
    namespace ast {

class INode : public IIO
{
public:
    virtual ~INode() = default;
};

    } // namespace ast
} // namespace kyfoo
