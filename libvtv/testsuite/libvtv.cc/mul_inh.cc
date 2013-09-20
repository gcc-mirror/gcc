// { dg-do run }

extern "C" int printf(const char *, ...);

struct A {
  virtual ~A() {}
};

struct B {
  virtual ~B() {}
};

struct C: public A {
  virtual ~C() {}
};

struct D: public C, B {
  virtual ~D() {}
};

D d;

int main()
{
  printf ("%p\n", &d);
  return 0;
}
