/* PR c++/110347 */

#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

struct S {
  int A, B[10], *C;
  void f (int dev);
  void g (int dev);
};

template<typename T>
struct St {
  T A, B[10], *C;
  void ft (int dev);
  void gt (int dev);
};


void
S::f (int dev)
{
  A = 5;
  C = (int *) malloc (sizeof (int) * 10);
  uintptr_t c_saved = (uintptr_t) C;
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;

  #pragma omp target firstprivate(A) firstprivate(B) firstprivate(C) \
                     firstprivate(c_saved) device(dev)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  #pragma omp parallel if (0) firstprivate(A) firstprivate(B) firstprivate(C)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  free (C);
}

void
S::g (int dev)
{
  A = 5;
  C = (int *) malloc (sizeof (int) * 10);
  uintptr_t c_saved = (uintptr_t) C;
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;

  #pragma omp target firstprivate(A) firstprivate(B) firstprivate(C) \
                      allocate(allocator(omp_low_lat_mem_alloc), align(128): A, B, C) \
                      device(dev)
    {
#if 0  /* FIXME: The following is disabled because of PR middle-end/113436.  */
      if (((uintptr_t) &A) % 128  != 0)
	abort ();
      if (((uintptr_t) &B) % 128  != 0)
	abort ();
      if (((uintptr_t) &C) % 128  != 0)
	abort ();
#endif
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  #pragma omp parallel if (0) firstprivate(A) firstprivate(B) firstprivate(C) \
                       allocate(allocator(omp_low_lat_mem_alloc), align(128): A, B, C)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      if (((uintptr_t) &A) % 128  != 0)
	abort ();
      if (((uintptr_t) &B) % 128  != 0)
	abort ();
      if (((uintptr_t) &C) % 128  != 0)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  free (C);
}


template<typename T>
void
St<T>::ft (int dev)
{
  A = 5;
  C = (T *) malloc (sizeof (T) * 10);
  uintptr_t c_saved = (uintptr_t) C;
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;

  #pragma omp target firstprivate(A) firstprivate(B) firstprivate(C) \
                     firstprivate(c_saved) device(dev)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  #pragma omp parallel if (0) firstprivate(A) firstprivate(B) firstprivate(C)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  free (C);
}

template<typename T>
void
St<T>::gt (int dev)
{
  A = 5;
  C = (T *) malloc (sizeof (T) * 10);
  uintptr_t c_saved = (uintptr_t) C;
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;

  #pragma omp target firstprivate(A) firstprivate(B) firstprivate(C) \
                     allocate(allocator(omp_low_lat_mem_alloc), align(128): A, B, C) \
                     device(dev)
    {
#if 0  /* FIXME: The following is disabled because of PR middle-end/113436.  */
      if (((uintptr_t) &A) % 128  != 0)
	abort ();
      if (((uintptr_t) &B) % 128  != 0)
	abort ();
      if (((uintptr_t) &C) % 128  != 0)
	abort ();
#endif
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  #pragma omp parallel if (0) firstprivate(A) firstprivate(B) firstprivate(C) \
                       allocate(allocator(omp_low_lat_mem_alloc), align(128): A, B, C)
    {
      if (A != 5)
	abort ();
      for (int i = 0; i < 10; i++)
	if (B[i] != i + 5)
	  abort ();
      if (c_saved != (uintptr_t) C)
	abort ();
      if (((uintptr_t) &A) % 128  != 0)
	abort ();
      if (((uintptr_t) &B) % 128  != 0)
	abort ();
      if (((uintptr_t) &C) % 128  != 0)
	abort ();
      A = 99;
      for (int i = 0; i < 10; i++)
	B[i] = -i-23;
      C = &A;
    }

  if (A != 5)
    abort ();
  if (c_saved != (uintptr_t) C)
    abort ();
  for (int i = 0; i < 10; i++)
    if (B[i] != i + 5 || C[i] != i+5)
      abort ();

  free (C);
}

int
main ()
{
  struct S s;
  struct St<int> st;
  for (int dev = 0; dev <= omp_get_num_devices(); dev++)
    {
      s.f (dev);
      st.ft (dev);
      s.g (dev);
      st.gt (dev);
    }
  return 0;
}
