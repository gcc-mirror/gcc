// { dg-do run }

#include <assert.h>
struct A {
  A():value(123) {}
    int value;
    virtual int access() { return this->value; }
};
struct B {
  B():value(456) {}
    int value;
    virtual int access() { return this->value; }
};
struct C : public A, public B {
  C():better_value(789) {}
    int better_value;
    virtual int access() { return this->better_value; }
};
struct D: public C {
  D():other_value(987) {}
  int other_value;
  virtual int access() { return this->other_value; }
};

int use(B *b)
{
    return b->access();
}

int main()
{
  C c;
  assert(use(&c) == 789);
  D d;
  assert(use(&d) == 987);
  return 0;
}
