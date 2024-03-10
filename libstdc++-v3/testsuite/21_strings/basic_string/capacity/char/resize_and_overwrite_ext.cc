// { dg-do run { target c++11 } }

#include <string>
// Run ./resize_and_overwrite.cc tests using __resize_and_overwrite instead.
#define resize_and_overwrite __resize_and_overwrite
#include "resize_and_overwrite.cc"
