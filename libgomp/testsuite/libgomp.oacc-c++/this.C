#include <cstdlib>
#include <iostream>
using namespace std;

class test {
  public:
  int a;

  test ()
  {
    a = -1;
#pragma acc enter data copyin (this[0:1])
  }

  ~test ()
  {
#pragma acc exit data delete (this[0:1])
  }

  void set (int i)
  {
    a = i;
#pragma acc update device (this[0:1])
  }

  int get ()
  {
#pragma acc update host (this[0:1])
    return a;
  }
};

int
main ()
{
  test t;

  t.set (4);
  if (t.get () != 4)
    abort ();

  return 0;
}
