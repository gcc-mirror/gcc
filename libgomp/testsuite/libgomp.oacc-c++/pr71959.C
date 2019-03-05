// { dg-additional-sources "pr71959-aux.cc" }

// PR lto/71959 ICEd LTO due to mismatch between writing & reading behaviour

struct Iter
{
  int *cursor;

  Iter(int *cursor_) : cursor(cursor_) {}

  int *point() const { return cursor; }
};

#pragma acc routine seq
int one () { return 1; }

struct Apply
{
  static void apply (int (*fn)(), Iter out)
  { *out.point() = fn (); }
};

int main ()
{
  int x;

#pragma acc parallel copyout(x)
  Apply::apply (one, Iter (&x));

  return x != 1;
}
