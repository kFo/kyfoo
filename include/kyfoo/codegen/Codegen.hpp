#pragma once

#include <filesystem>

namespace kyfoo::codegen {

const char* const EXTENSION_OBJECTFILE = ".o";

struct CustomData
{
    virtual ~CustomData() = default;
};

inline std::filesystem::path toObjectFilepath(std::filesystem::path const& rhs)
{
    auto ret = rhs;
    return ret.replace_extension(EXTENSION_OBJECTFILE);
}

} // namespace kyfoo::codegen
