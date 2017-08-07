/* { dg-do run } */

#include <assert.h>

#define DO_PRAGMA(x) _Pragma (#x)

#define test(idx,type,ngangs)                           \
  void                                                  \
  test_##idx ()                                         \
  {                                                     \
    int b[100];                                         \
                                                        \
    for (unsigned int i = 0; i < 100; i++)              \
      b[i] = 0;                                         \
                                                        \
    DO_PRAGMA(acc parallel num_gangs (ngangs) copy (b)) \
      {                                                 \
        _Pragma("acc loop gang")                        \
          for (type j = 0; j < 5; j++)                  \
            {                                           \
              _Pragma("acc loop vector")                \
                for (unsigned int i = 0; i < 20; i++)   \
                  b[j * 20 + i] = -2;                   \
            }                                           \
      }                                                 \
                                                        \
    for (unsigned int i = 0; i < 100; i++)              \
      assert (b[i] == -2);                              \
  }

test (0, signed char, 256)
test (1, unsigned char, 256)
test (2, signed short, 65535)
test (3, unsigned short, 65535)

int
main ()
{
  test_0 ();
  test_1 ();
  test_2 ();
  test_3 ();

  return 0;
}
