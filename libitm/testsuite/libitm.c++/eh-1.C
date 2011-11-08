// { dg-do run }

extern "C" void abort ();

int dothrow;
int g;

static void f1()
{
  g++;
  if (dothrow)
    throw 1;
}

static void f2()
{
  __transaction_atomic {
    f1();
  }
}

int main()
{
  dothrow = 0;
  f2();

  dothrow = 1;
  try {
    f2();
  } catch (...) {
  }

  if (g != 2)
    abort ();
  return 0;
}
