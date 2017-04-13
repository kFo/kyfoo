#pragma once

#include <kyfoo/ast/IO.hpp>

namespace kyfoo {
    namespace ast {

class Diagnostics;
class INode : public IIO
{
public:
    virtual ~INode() = default;

    virtual void resolveSymbols(Diagnostics& dgn) = 0;
};

    } // namespace ast
} // namespace kyfoo
