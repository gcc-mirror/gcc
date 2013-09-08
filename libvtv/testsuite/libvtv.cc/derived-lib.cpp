#include "lib.h"

struct Derived_Private : public Base
{
  virtual ~Derived_Private() 
  { printf("in Derived_Private destructor\n"); }
};

Base * GetPrivate()
{
  return new Derived_Private();
}

void Destroy(Base * pb)
{
  delete pb;   // Virtual call #1
}

