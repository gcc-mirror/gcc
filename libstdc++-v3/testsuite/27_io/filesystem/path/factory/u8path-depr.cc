// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <filesystem>

namespace fs = std::filesystem;

const char* s = "";
auto p1 = fs::u8path(s); // { dg-warning "deprecated" }
auto p2 = fs::u8path(s, s); // { dg-warning "deprecated" }

#if __cpp_lib_char8_t
const char8_t* u = u8"";
auto p3 = fs::u8path(u); // { dg-warning "deprecated" }
auto p4 = fs::u8path(u, u); // { dg-warning "deprecated" }
#endif
