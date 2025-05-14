// { dg-do compile { target c++11 } }

// LWG 4148. unique_ptr::operator* should not allow dangling references

#include <memory>

struct pointer
{
  pointer() { }
  pointer(std::nullptr_t) { }
  int operator*() const { return 0; }
  bool operator==(pointer) const { return true; }
  bool operator==(std::nullptr_t) const { return false; }
#ifndef __cpp_lib_three_way_comparison
  bool operator!=(pointer) const { return false; }
  bool operator!=(std::nullptr_t) const { return true; }
#endif
};

struct Deleter
{
  using pointer = ::pointer;
  void operator()(pointer) const { }
};

std::unique_ptr<const int, Deleter> up;
int i = *up; // { dg-error "here" }
// { dg-error "dangling reference" "" { target *-*-* } 0 }

// { dg-warning "returning reference to temporary" "" { target c++23_down } 0 }
// { dg-error "returning reference to temporary" "" { target c++26 } 0 }
