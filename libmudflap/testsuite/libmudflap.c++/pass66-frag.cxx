// PR c++/37568
// { dg-do compile }
// { dg-options "-fmudflap -O" }

struct A
{
  int i;
};

A
foo ()
{
  A a = { 1 };
  return a;
}

A a = foo ();
