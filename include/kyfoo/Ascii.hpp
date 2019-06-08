#pragma once

#include <filesystem>
#include <limits>
#include <type_traits>

#include <kyfoo/Slice.hpp>
#include <kyfoo/String.hpp>
#include <kyfoo/SystemError.hpp>

namespace kyfoo {

namespace {
    template <typename T>
    std::enable_if_t<std::is_unsigned_v<T>,
        Slice<char>> renderBackwards(Slice<char> window, T rhs)
    {
        if ( !rhs ) {
            window[0] = '0';
            return window(0, 1);
        }

        auto const end = window.end();
        auto c = end;
        do {
            --c;
            *c = '0' + (rhs % 10);
            rhs /= 10;
        } while ( rhs );

        return Slice(c, end - c);
    }
}

namespace ascii {
    template <typename Sink>
    void write(Sink& sink, char rhs)
    {
        sink.write(slice(rhs));
    }

    template <typename Sink, typename T>
    std::enable_if_t<std::is_integral_v<T> && std::is_unsigned_v<T>>
        write(Sink& sink, T rhs)
    {
        char buffer[std::numeric_limits<T>::digits10];
        sink.write(renderBackwards(Slice(buffer, sizeof(buffer)), rhs));
    }

    template <typename Sink, typename T>
    std::enable_if_t<std::is_integral_v<T> && std::is_signed_v<T>>
        write(Sink& sink, T rhs)
    {
        char buffer[std::numeric_limits<T>::digits10 + 1];
        bool neg = false;
        if ( rhs < 0 ) {
            neg = true;
            rhs = -rhs;
        }

        auto w = renderBackwards(Slice(buffer, sizeof(buffer)), std::make_unsigned_t<T>(rhs));
        if ( neg ) {
            w = Slice(w.data()-1, w.card()+1);
            w.front() = '-';
        }

        sink.write(w);
    }

    template <typename Sink, typename T>
    std::enable_if_t<std::is_floating_point_v<T>>
        write(Sink& sink, T rhs)
    {
        char buffer[32];
        auto n = snprintf(buffer, sizeof(buffer), "%f", rhs);
        sink.write(Slice(buffer, n));
    }

    template <typename Sink>
    void write(Sink& sink, const char* rhs)
    {
        sink.write(sliceCString(rhs));
    }

    template <typename Sink>
    void write(Sink& sink, Slice<char> rhs)
    {
        sink.write(rhs.cast<void const>());
    }

    template <typename Sink>
    void write(Sink& sink, Slice<char const> rhs)
    {
        sink.write(rhs.cast<void const>());
    }

    template <typename Sink>
    void write(Sink& sink, Slice<wchar_t> rhs)
    {
        sink.write(rhs.cast<void const>());
    }

    template <typename Sink>
    void write(Sink& sink, Slice<wchar_t const> rhs)
    {
        sink.write(rhs.cast<void const>());
    }

    template <typename Sink>
    void write(Sink& sink, std::string const& rhs)
    {
        sink.write(slice(rhs.data(), rhs.data() + rhs.size()));
    }

    template <typename Sink>
    void write(Sink& sink, std::filesystem::path const& rhs)
    {
        sink.write(Slice("\""));
        auto str = rhs.string(); // todo: avoid allocation
        sink.write(Slice(str));
        sink.write(Slice("\""));
    }

    template <typename Sink>
    void write(Sink& sink, SystemError err)
    {
        SystemErrorMessage msg(err);
        sink.write(Slice(msg.data(), msg.card()));
    }

    template <typename Sink>
    class Formatter : public Sink
    {
    public:
        template <typename... Args>
        Formatter(Args... args) noexcept
            : Sink(std::forward<Args>(args)...)
        {
        }

        Formatter(Sink&& rhs) noexcept
            : Sink(std::move(rhs))
        {
        }

        Formatter& operator = (Sink&& rhs) noexcept
        {
            Sink.operator=(std::move(rhs));
            return *this;
        }

    public:
        template <typename T>
        Formatter& operator () (T const& rhs) noexcept
        {
            write(rhs);
            return *this;
        }

        Formatter& operator () () noexcept
        {
            write("\n");
            // todo: flush
            return *this;
        }

    public:
        using Sink::write;

        void write(char rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        template <typename T>
        std::enable_if_t<std::is_integral_v<T> && std::is_unsigned_v<T>>
            write(T rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        template <typename T>
        std::enable_if_t<std::is_integral_v<T> && std::is_signed_v<T>>
            write(T rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        template <typename T>
        std::enable_if_t<std::is_floating_point_v<T>>
            write(T rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        void write(const char* rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        void write(Slice<char> rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        void write(Slice<char const> rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        void write(std::string const& rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        void write(std::filesystem::path const& rhs) noexcept
        {
            ascii::write(*this, rhs);
        }

        template <typename T>
        std::enable_if_t<!std::is_integral_v<T> && !std::is_floating_point_v<T>>
            write(T const& rhs) noexcept
        {
            ascii::write(*this, rhs);
        }
    };
} // namespace ascii

} // namespace kyfoo
