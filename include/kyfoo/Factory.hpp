#pragma once

namespace kyfoo {

template <typename T, typename Allocator>
class AscendingFactory : private Allocator
{
public:
    template <typename... Args>
    explicit AscendingFactory(Args&&... args)
        : Allocator(std::forward<Args>(args)...)
    {
    }

public:
    template <typename... Args>
    T const& mk(Args&&... args) noexcept
    {
        auto m = allocate<T>(*static_cast<Allocator*>(this), 1);
        new (m.data()) T(std::forward<Args>(args)...);
        return m.front();
    }
};

} // namespace kyfoo
