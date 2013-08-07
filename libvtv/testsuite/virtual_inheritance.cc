#include <assert.h>
struct V {
  V(): virtual_value(-123) {}
  int virtual_value;
  virtual int access_vv() { return virtual_value; }
};

struct A: virtual public V {
  A():value(123) {}
    int value;
    virtual int access() { return value; }
};
struct B: virtual public V {
  B():value(456) {}
    int value;
    virtual int access() { return value; }
};
struct C : public A, public B {
  C():better_value(789) {}
    int better_value;
    virtual int access() { return better_value; }
};
struct D: public A, public B {
  D():better_virtual_value(-345) {}
  int better_virtual_value;
  virtual int access_vv() { return better_virtual_value; }
};

int use(B *b)
{
    return b->access();
}

int v_use(V * v)
{
  return v->access_vv();
}

int main()
{
  C c;
  assert(v_use(&c) == -123);
  D d;
  assert(v_use(&d) == -345);
  return 0;
}
