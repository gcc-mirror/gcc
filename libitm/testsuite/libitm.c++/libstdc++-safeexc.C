// Tests that the exceptions declared by the TM TS (N4514) as transaction_safe
// are indeed that.  Thus, this also tests the transactional clones in
// libstdc++ and libsupc++.

// Not supported on Darwin nor AIX because those lack the support for
// weak references to undefined functions that we need in libstdc++ to make
// exceptions transaction-safe.
// { dg-do run { target { ! { *-*-darwin* powerpc-ibm-aix* } } } }

#include <iostream>
#include <exception>
#include <stdexcept>
#include <string>

using namespace std;

template<typename T> void thrower(const T& t)
{
  try
    {
      atomic_commit
      {
	throw t;
      }
    }
  catch (T ex)
    {
      if (ex != t) abort ();
    }
}

template<typename T> void thrower1(const string& what)
{
  try
    {
      atomic_commit
      {
	throw T ();
      }
    }
  catch (T ex)
    {
      if (what != ex.what()) abort ();
    }
}

template<typename T> void thrower2(const string& what)
{
  try
    {
      atomic_commit
      {
	throw T (what);
      }
    }
  catch (T ex)
    {
      if (what != ex.what()) abort ();
    }
}


int main ()
{
  thrower<unsigned int> (23);
  thrower<int> (23);
  thrower<unsigned short> (23);
  thrower<short> (23);
  thrower<unsigned char> (23);
  thrower<char> (23);
  thrower<unsigned long int> (42);
  thrower<long int> (42);
  thrower<unsigned long long int> (42);
  thrower<long long int> (42);
  thrower<double> (23.42);
  thrower<long double> (23.42);
  thrower<float> (23.42);
  thrower<void*> (0);
  thrower<void**> (0);
  thrower1<exception> ("std::exception");
  thrower1<bad_exception> ("std::bad_exception");
  thrower2<logic_error> ("test");
  thrower2<domain_error> ("test");
  thrower2<invalid_argument> ("test");
  thrower2<length_error> ("test");
  thrower2<out_of_range> ("test");
  thrower2<runtime_error> ("test");
  thrower2<range_error> ("test");
  thrower2<overflow_error> ("test");
  thrower2<underflow_error> ("test");
  return 0;
}
