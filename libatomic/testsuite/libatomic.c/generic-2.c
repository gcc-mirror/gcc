/* { dg-do run } */

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* Avoid using the builtins, calling directly to the library functions
   of the same name, so that we get direct access to the size_t and
   don't have to create myriad types of different sizes.  */

#define C2_(X,Y)	X ## Y
#define C2(X,Y)		C2_(X,Y)

#define S2(X)		#X
#define S(X)		S2(X)

#define ASMNAME(X)	__asm__(S(C2(__USER_LABEL_PREFIX__,X)))
#define MAN(X)		ASMNAME(C2(__atomic_,X))

void libat_load (size_t, void *, void *, int) MAN(load);
void libat_store (size_t, void *, void *, int) MAN(store);
void libat_exchange (size_t, void *, void *, void *, int) MAN(exchange);
bool libat_compare_exchange (size_t, void *, void *, void *, int, int)
	MAN(compare_exchange);
bool libat_is_lock_free (size_t, void *) MAN(is_lock_free);


#define ALIGN  16
#define MAX    4*ALIGN

static char a[MAX];
static char b[MAX];
static char c[MAX];
static char pa[MAX];
static char pb[MAX];

static void test_load(void)
{
  int i, j;
  for (i = ALIGN; i < 2*ALIGN; ++i)
    for (j = 1; j <= 2*ALIGN; ++j)
      {
        memcpy(b, pa, MAX);
        memcpy(b + i, pb, j);
        libat_load (j, b + i, a, 0);
        if (memcmp (a, pb, j) != 0) abort ();
      }
}

static void test_store(void)
{
  int i, j;
  for (i = ALIGN; i < 2*ALIGN; ++i)
    for (j = 1; j <= 2*ALIGN; ++j)
      {
        memcpy(a, pa, MAX);
        memcpy(b, pa, MAX);
        memcpy(b + i, pb, j);
        libat_store (j, a + i, pb, 0);
        if (memcmp (a, b, MAX) != 0) abort ();
      }
}

static void test_exch(void)
{
  int i, j;
  for (i = ALIGN; i < 2 * ALIGN; ++i)
    for (j = 1; j <= 2*ALIGN; ++j)
      {
	memcpy(a, pa, MAX);
        memcpy(b, pa, MAX);
        memcpy(b + i, pb, j);
        libat_exchange (j, a + i, pb, c, 0);
        if (memcmp (a, b, MAX) != 0) abort ();
        if (memcmp (c, pa + i, j) != 0) abort ();

        memcpy(a, pa, MAX);
        memcpy(c, pb, MAX);
        libat_exchange (j, a + i, c + i, c + i, 0);
        memcpy(b, pa, MAX);
        memcpy(b + i, pb + i, j);
        if (memcmp (b, a, MAX) != 0) abort ();
        memcpy(b, pb, MAX);
        memcpy(b + i, pa + i, j);
	if (memcmp (b, c, MAX) != 0) abort ();
      }
}

static void test_cas(void)
{
  int i, j;
  for (i = ALIGN; i < 2 * ALIGN; ++i)
    for (j = 1; j <= 2*ALIGN; ++j)
      {
	memcpy(a, pa, MAX);
	memcpy(b, pa, MAX);
	memcpy(c, pa, MAX);
	memcpy(b + i, pb, j);
        if (!libat_compare_exchange (j, a + i, c + i, pb, 0, 0)) abort ();
	if (memcmp (c, pa, MAX) != 0) abort ();
        if (memcmp (a, b, MAX) != 0) abort ();

	memcpy(a, pb, MAX);
	memcpy(b, pa, MAX);
	memcpy(c, pa, MAX);
	memcpy(b + i, pb + i, j);
        if (libat_compare_exchange (j, a + i, c + i, pb, 0, 0)) abort ();
        if (memcmp (a, pb, MAX) != 0) abort ();
        if (memcmp (b, c, MAX) != 0) abort ();
      }
}

int main (void)
{
  int i;
  for (i = 0; i < MAX; ++i)
    {
      pa[i] = i * 2;
      pb[i] = i * 2 + 1;
    }

  test_load ();
  test_store ();
  test_exch ();
  test_cas ();

  return 0;
}
