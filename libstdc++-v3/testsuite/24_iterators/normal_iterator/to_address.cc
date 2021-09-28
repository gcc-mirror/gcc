// { dg-do compile { target { c++11 } } }
#include <string>
#include <memory>

char* p = std::__to_address(std::string("1").begin());
const char* q = std::__to_address(std::string("2").cbegin());
