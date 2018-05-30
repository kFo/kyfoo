#pragma once

#include <memory>

namespace kyfoo {

template <typename T>
using Box = std::unique_ptr<T>;

template <typename T, typename... Args>
Box<T> mk(Args&&... args)
{
    return std::make_unique<T>(std::forward<Args>(args)...);
}

} // namespace kyfoo
