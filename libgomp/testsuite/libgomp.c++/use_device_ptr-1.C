/* PR c++/110347 */

#include <omp.h>

#define N 30

struct t {
  int *A;
  void f (int dev);
};

void
t::f (int dev)
{
  int *ptr;
  int B[N];
  for (int i = 0; i < N; i++)
    B[i] = 1 + i;
  ptr = A = (int *) omp_target_alloc (sizeof (int) * N, dev);
  omp_target_memcpy (A, B, sizeof (int) * N, 0, 0, dev, omp_initial_device);

  #pragma omp target is_device_ptr (A) device(dev)
  {
    for (int i = 0; i < N; i++)
      if (A[i] != 1 + i)
	__builtin_abort ();
    for (int i = 0; i < N; i++)
      A[i] = (-2-i)*10;
    A = (int *) 0x12345;
  }
  if (ptr != A)
    __builtin_abort ();

  #pragma omp target is_device_ptr (A) device(dev)
  {
    for (int i = 0; i < N; i++)
      if (A[i] != (-2-i)*10)
	__builtin_abort ();
    for (int i = 0; i < N; i++)
      A[i] = (3+i)*11;
    A = (int *) 0x12345;
  }
  if (ptr != A)
    __builtin_abort ();

  int *C = (int *) __builtin_malloc (sizeof(int)*N);
  omp_target_memcpy (C, A, sizeof (int) * N, 0, 0, omp_initial_device, dev);
  for (int i = 0; i < N; i++)
    if (C[i] != (3+i)*11)
      __builtin_abort ();
  __builtin_free (C);
  omp_target_free (A, dev);
}

template <typename T>
struct tt {
  T *D;
  void g (int dev);
};

template <typename T>
void
tt<T>::g (int dev)
{
  T *ptr;
  T E[N];
  for (int i = 0; i < N; i++)
    E[i] = 1 + i;
  ptr = D = (T *) omp_target_alloc (sizeof (T) * N, dev);
  omp_target_memcpy (D, E, sizeof (T) * N, 0, 0, dev, omp_initial_device);

  #pragma omp target is_device_ptr (D) device(dev)
  {
    for (int i = 0; i < N; i++)
      if (D[i] != 1 + i)
	__builtin_abort ();
    for (int i = 0; i < N; i++)
      D[i] = (-2-i)*10;
    D = (T *) 0x12345;
  }
  if (ptr != D)
    __builtin_abort ();

  #pragma omp target is_device_ptr (D) device(dev)
  {
    for (int i = 0; i < N; i++)
      if (D[i] != (-2-i)*10)
	__builtin_abort ();
    for (int i = 0; i < N; i++)
      D[i] = (3+i)*11;
    D = (T *) 0x12345;
  }
  if (ptr != D)
    __builtin_abort ();

  T *F = (T *) __builtin_malloc (sizeof(T)*N);
  omp_target_memcpy (F, D, sizeof (T) * N, 0, 0, omp_initial_device, dev);
  for (int i = 0; i < N; i++)
    if (F[i] != (3+i)*11)
      __builtin_abort ();
  __builtin_free (F);
  omp_target_free (D, dev);
}

void
foo ()
{
  struct t x;
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    x.f (dev);
}

void
bar ()
{
  struct tt<int> y;
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    y.g (dev);
}

int
main ()
{
  foo ();
  bar ();
}
