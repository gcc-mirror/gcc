// { dg-do compile { target c++23 } }
#include<mdspan>

std::default_accessor<int[3]> a; // { dg-error "required from here" }

class AbstractBase
{
  virtual void
  foo() const = 0;
};

class Derived : public AbstractBase
{
  void
  foo() const override
  { }
};

std::default_accessor<Derived> b_ok;
std::default_accessor<AbstractBase> b_err; // { dg-error "required from here"}

// { dg-prune-output "ElementType must not be an array type" }
// { dg-prune-output "ElementType must not be an abstract" }
