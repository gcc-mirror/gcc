// { dg-do run { target c++17 } }

#include <charconv>
#include <string>

int main()
{
#if __cpp_lib_to_chars >= 201611L // FP from_char not available otherwise.
  // PR libstdc++/105324
  // std::from_chars() assertion at floating_from_chars.cc:78
  std::string s(512, '1');
  s[1] = '.';
  long double d;
  std::from_chars(s.data(), s.data() + s.size(), d);
#endif
}
