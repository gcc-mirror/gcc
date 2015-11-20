// Test throwing an exception whose constructor might throw.  This tests that
// _cxa_free_exception is instrumented.

// { dg-do run }
// { dg-options "-fgnu-tm" }

void __attribute__ ((transaction_pure,noinline)) dontoptimize (int *i)
{ }

struct test
{
  int* data;
  test (int i)
  {
    // new may throw
    data = new int[1];
    data[0] = i;
    dontoptimize (data);
  }
  test (const test& t) : test (t.data[0])
  { }
  ~test ()
  {
    delete data;
  }
  bool operator !=(const test& other)
  {
    return data[0] != other.data[0];
  }
};

int main()
{
  try
    {
      atomic_commit
      {
	throw test(23);
      }
    }
  catch (test ex)
    {
      if (ex.data[0] != 23) __builtin_abort ();
    }
  return 0;
}
