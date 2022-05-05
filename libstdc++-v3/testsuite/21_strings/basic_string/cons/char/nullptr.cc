// { dg-do compile { target c++11 } }
#include <string>

std::string s = nullptr; // { dg-error "deleted" "P2166R1" { target c++23 } }

struct S
{
  operator const char*() const { return ""; }
  operator std::nullptr_t() const { return {}; }
};

std::string s2{ S{} }; // { dg-error "deleted" "PR 104099" { target c++23 } }

#if __cpp_concepts
struct J
{
  // In C++20 this selects basic_string(const char*),
  // in C++23 it's ambiguous due to basic_string(nullptr_t).
  template<typename T>
    requires (!std::is_same_v<std::allocator<char>, T>)
    && (!std::is_same_v<std::string, T>)
    && (!std::is_same_v<char, T>)
    && (!std::is_same_v<std::string_view, T>)
    operator T() const { return {}; }
};

std::string s3{ J{} }; // { dg-error "ambiguous" "PR 104099" { target c++23 } }
#endif
