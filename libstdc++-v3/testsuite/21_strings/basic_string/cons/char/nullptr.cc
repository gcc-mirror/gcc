// { dg-do compile { target c++11 } }
#include <string>

std::string s = nullptr; // { dg-error "deleted" "P2166R1" }
