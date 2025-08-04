// { dg-do compile { target c++26 } }
#include<mdspan>

#include <cstdint>

std::aligned_accessor<uint32_t, 0> a;          // { dg-error "required from here" }
std::aligned_accessor<uint32_t, 7> b;          // { dg-error "required from here" }
std::aligned_accessor<uint32_t, size_t(-1)> c; // { dg-error "required from here" }

std::aligned_accessor<uint32_t, 2> d;          // { dg-error "required from here" }

std::aligned_accessor<int[2], 32> e;           // { dg-error "required from here" }

class Abstract
{
  virtual void
  foo() const = 0;
};

class Derived : public Abstract
{
  void
  foo() const override
  { }
};

std::aligned_accessor<Derived, alignof(int)> f_ok;
std::aligned_accessor<Abstract, alignof(int)> f_err; // { dg-error "required from here" }

// { dg-prune-output "ByteAlignment must be a power of two" }
// { dg-prune-output "ElementType must not be an array type" }
// { dg-prune-output "ElementType must not be an abstract" }
// { dg-prune-output "static assertion failed" } // no message for alignment being too small
