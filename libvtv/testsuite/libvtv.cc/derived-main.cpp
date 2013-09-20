// { dg-do run }

#include "lib.h"

struct Derived: public Base 
{
  virtual ~Derived() 
  { printf("In Derived destructor\n"); }
};

int main()
{
  Derived * d = new Derived;
  Destroy(d);
  Base * pp = GetPrivate();
  delete pp;  // Virtual call #2
}

