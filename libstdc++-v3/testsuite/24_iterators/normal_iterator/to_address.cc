// { dg-do compile { target { c++11 } } }
#include <string>
#include <vector>
#include <memory>

#include <testsuite_allocator.h>

char* p __attribute__((unused))
  = std::__to_address(std::string("1").begin());
const char* q __attribute__((unused))
  = std::__to_address(std::string("2").cbegin());
int* r __attribute__((unused))
  = std::__to_address(std::vector<int>(1, 1).begin());
const int* s __attribute__((unused))
  = std::__to_address(std::vector<int>(1, 1).cbegin());
int* t __attribute__((unused))
  = std::__to_address(std::vector<int, __gnu_test::CustomPointerAlloc<int>>(1, 1).begin());
const int* u __attribute__((unused))
  = std::__to_address(std::vector<int, __gnu_test::CustomPointerAlloc<int>>(1, 1).cbegin());
