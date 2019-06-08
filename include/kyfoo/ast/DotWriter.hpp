#pragma once

#include <filesystem>

#include <kyfoo/Stream.hpp>

namespace kyfoo::ast {

class Module;

void writeDot(Module const& mod, std::filesystem::path const& path);
DefaultOutStream& writeDot(DefaultOutStream& stream, Module const& mod);

} // namespace kyfoo::ast
