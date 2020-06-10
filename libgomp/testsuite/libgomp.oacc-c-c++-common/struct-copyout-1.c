#include <assert.h>

struct str1 {
  int a;
  int b;
};

struct str2 {
  int c;
  int d;
  struct str1 s;
};

int
main (int argc, char *argv[])
{
  struct str2 t;

  t.c = 1;
  t.d = 2;
  t.s.a = 3;
  t.s.b = 4;

  #pragma acc enter data copyin(t.s)

  #pragma acc serial present(t.s) /* { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } } */
  {
    t.s.a = 5;
    t.s.b = 6;
  }

  #pragma acc exit data copyout(t.s)

  assert (t.s.a == 5);
  assert (t.s.b == 6);

  return 0;
}
