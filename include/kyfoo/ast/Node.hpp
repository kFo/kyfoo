#pragma once

#include <kyfoo/ast/IO.hpp>

namespace kyfoo {
    namespace ast {

class Semantics;
class INode : public IIO
{
public:
    virtual ~INode() = default;

    virtual void resolveSymbols(Semantics& semantic) = 0;
};

    } // namespace ast
} // namespace kyfoo
