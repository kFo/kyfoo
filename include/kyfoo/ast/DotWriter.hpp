#pragma once

#include <filesystem>
#include <ostream>

namespace kyfoo::ast {

class Module;

void writeDot(Module const& mod, std::filesystem::path const& path);
std::ostream& writeDot(std::ostream& stream, Module const& mod);

} // namespace kyfoo::ast
