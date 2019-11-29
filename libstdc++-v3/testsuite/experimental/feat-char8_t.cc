// { dg-do preprocess { target c++11 } }
// { dg-options "-fchar8_t" }

#include <atomic>
#include <filesystem>
#include <istream>
#include <limits>
#include <locale>
#include <ostream>
#include <string>
#include <string_view>

#ifndef  __cpp_lib_char8_t
#  error "__cpp_lib_char8_t"
#elif  __cpp_lib_char8_t != 201907L
#  error "__cpp_lib_char8_t != 201907L"
#endif
