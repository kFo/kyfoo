#pragma once

#include <experimental/filesystem>

namespace kyfoo {
    namespace codegen {

const char* const EXTENSION_OBJECTFILE = ".o";

struct CustomData
{
    virtual ~CustomData() = default;
};

inline std::experimental::filesystem::path toObjectFilepath(std::experimental::filesystem::path const& rhs)
{
    auto ret = rhs;
    return ret.replace_extension(EXTENSION_OBJECTFILE);
}

    } // namespace codegen
} // namespace kyfoo
