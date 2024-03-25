// { dg-do run }

#include <cstring>
#include <cassert>

#define MAP_DECLS

#define NONREF_DECL_BASE
#define REF_DECL_BASE
#define PTR_DECL_BASE
#define REF2PTR_DECL_BASE

#define ARRAY_DECL_BASE
#define REF2ARRAY_DECL_BASE
#define PTR_OFFSET_DECL_BASE
#define REF2PTR_OFFSET_DECL_BASE

#define MAP_SECTIONS

#define NONREF_DECL_MEMBER_SLICE
#define NONREF_DECL_MEMBER_SLICE_BASEPTR
#define REF_DECL_MEMBER_SLICE
#define REF_DECL_MEMBER_SLICE_BASEPTR
#define PTR_DECL_MEMBER_SLICE
#define PTR_DECL_MEMBER_SLICE_BASEPTR
#define REF2PTR_DECL_MEMBER_SLICE
#define REF2PTR_DECL_MEMBER_SLICE_BASEPTR

#define ARRAY_DECL_MEMBER_SLICE
#define ARRAY_DECL_MEMBER_SLICE_BASEPTR
#define REF2ARRAY_DECL_MEMBER_SLICE
#define REF2ARRAY_DECL_MEMBER_SLICE_BASEPTR
#define PTR_OFFSET_DECL_MEMBER_SLICE
#define PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
#define REF2PTR_OFFSET_DECL_MEMBER_SLICE
#define REF2PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR

#define PTRARRAY_DECL_MEMBER_SLICE
#define PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
#define REF2PTRARRAY_DECL_MEMBER_SLICE
#define REF2PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
#define PTRPTR_OFFSET_DECL_MEMBER_SLICE
#define PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
#define REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE
#define REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR

#define NONREF_COMPONENT_BASE
#define NONREF_COMPONENT_MEMBER_SLICE
#define NONREF_COMPONENT_MEMBER_SLICE_BASEPTR

#define REF_COMPONENT_BASE
#define REF_COMPONENT_MEMBER_SLICE
#define REF_COMPONENT_MEMBER_SLICE_BASEPTR

#define PTR_COMPONENT_BASE
#define PTR_COMPONENT_MEMBER_SLICE
#define PTR_COMPONENT_MEMBER_SLICE_BASEPTR

#define REF2PTR_COMPONENT_BASE
#define REF2PTR_COMPONENT_MEMBER_SLICE
#define REF2PTR_COMPONENT_MEMBER_SLICE_BASEPTR

#ifdef MAP_DECLS
void
map_decls (void)
{
  int x = 0;
  int &y = x;
  int arr[4];
  int (&arrref)[4] = arr;
  int *z = &arr[0];
  int *&t = z;

  memset (arr, 0, sizeof arr);

  #pragma omp target map(x)
  {
    x++;
  }

  #pragma omp target map(y)
  {
    y++;
  }

  assert (x == 2);
  assert (y == 2);

  /* "A variable that is of type pointer is treated as if it is the base
      pointer of a zero-length array section that appeared as a list item in a
      map clause."  */
  #pragma omp target map(z)
  {
    z++;
  }

  /* "A variable that is of type reference to pointer is treated as if it had
      appeared in a map clause as a zero-length array section."

     The pointer here is *not* associated with a target address, so we're not
     disallowed from modifying it.  */
  #pragma omp target map(t)
  {
    t++;
  }

  assert (z == &arr[2]);
  assert (t == &arr[2]);

  #pragma omp target map(arr)
  {
    arr[2]++;
  }

  #pragma omp target map(arrref)
  {
    arrref[2]++;
  }

  assert (arr[2] == 2);
  assert (arrref[2] == 2);
}
#endif

struct S {
  int a;
  int &b;
  int *c;
  int *&d;
  int e[4];
  int (&f)[4];

  S(int a1, int &b1, int *c1, int *&d1) :
    a(a1), b(b1), c(c1), d(d1), f(e)
  {
    memset (e, 0, sizeof e);
  }
};

#ifdef NONREF_DECL_BASE
void
nonref_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys(a, b, &c, d);

  #pragma omp target map(mys.a)
  {
    mys.a++;
  }

  #pragma omp target map(mys.b)
  {
    mys.b++;
  }

  assert (mys.a == 1);
  assert (mys.b == 1);

  #pragma omp target map(mys.c)
  {
    mys.c++;
  }

  #pragma omp target map(mys.d)
  {
    mys.d++;
  }

  assert (mys.c == &c + 1);
  assert (mys.d == &c + 1);

  #pragma omp target map(mys.e)
  {
    mys.e[0]++;
  }

  #pragma omp target map(mys.f)
  {
    mys.f[0]++;
  }

  assert (mys.e[0] == 2);
  assert (mys.f[0] == 2);
}
#endif

#ifdef REF_DECL_BASE
void
ref_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig(a, b, &c, d);
  S &mys = mys_orig;

  #pragma omp target map(mys.a)
  {
    mys.a++;
  }

  #pragma omp target map(mys.b)
  {
    mys.b++;
  }

  assert (mys.a == 1);
  assert (mys.b == 1);

  #pragma omp target map(mys.c)
  {
    mys.c++;
  }

  #pragma omp target map(mys.d)
  {
    mys.d++;
  }

  assert (mys.c == &c + 1);
  assert (mys.d == &c + 1);

  #pragma omp target map(mys.e)
  {
    mys.e[0]++;
  }

  #pragma omp target map(mys.f)
  {
    mys.f[0]++;
  }

  assert (mys.e[0] == 2);
  assert (mys.f[0] == 2);
}
#endif

#ifdef PTR_DECL_BASE
void
ptr_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig(a, b, &c, d);
  S *mys = &mys_orig;

  #pragma omp target map(mys->a)
  {
    mys->a++;
  }

  #pragma omp target map(mys->b)
  {
    mys->b++;
  }

  assert (mys->a == 1);
  assert (mys->b == 1);

  #pragma omp target map(mys->c)
  {
    mys->c++;
  }

  #pragma omp target map(mys->d)
  {
    mys->d++;
  }

  assert (mys->c == &c + 1);
  assert (mys->d == &c + 1);

  #pragma omp target map(mys->e)
  {
    mys->e[0]++;
  }

  #pragma omp target map(mys->f)
  {
    mys->f[0]++;
  }

  assert (mys->e[0] == 2);
  assert (mys->f[0] == 2);
}
#endif

#ifdef REF2PTR_DECL_BASE
void
ref2ptr_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig(a, b, &c, d);
  S *mysp = &mys_orig;
  S *&mys = mysp;

  #pragma omp target map(mys->a)
  {
    mys->a++;
  }

  #pragma omp target map(mys->b)
  {
    mys->b++;
  }

  assert (mys->a == 1);
  assert (mys->b == 1);

  #pragma omp target map(mys->c)
  {
    mys->c++;
  }

  #pragma omp target map(mys->d)
  {
    mys->d++;
  }

  assert (mys->c == &c + 1);
  assert (mys->d == &c + 1);

  #pragma omp target map(mys->e)
  {
    mys->e[0]++;
  }

  #pragma omp target map(mys->f)
  {
    mys->f[0]++;
  }

  assert (mys->e[0] == 2);
  assert (mys->f[0] == 2);
}
#endif

#ifdef ARRAY_DECL_BASE
void
array_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys[4] =
    {
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d)
    };

  #pragma omp target map(mys[2].a)
  {
    mys[2].a++;
  }

  #pragma omp target map(mys[2].b)
  {
    mys[2].b++;
  }

  assert (mys[2].a == 1);
  assert (mys[2].b == 1);

  #pragma omp target map(mys[2].c)
  {
    mys[2].c++;
  }

  #pragma omp target map(mys[2].d)
  {
    mys[2].d++;
  }

  assert (mys[2].c == &c + 1);
  assert (mys[2].d == &c + 1);

  #pragma omp target map(mys[2].e)
  {
    mys[2].e[0]++;
  }

  #pragma omp target map(mys[2].f)
  {
    mys[2].f[0]++;
  }

  assert (mys[2].e[0] == 2);
  assert (mys[2].f[0] == 2);
}
#endif

#ifdef REF2ARRAY_DECL_BASE
void
ref2array_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig[4] =
    {
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d)
    };
  S (&mys)[4] = mys_orig;

  #pragma omp target map(mys[2].a)
  {
    mys[2].a++;
  }

  #pragma omp target map(mys[2].b)
  {
    mys[2].b++;
  }

  assert (mys[2].a == 1);
  assert (mys[2].b == 1);

  #pragma omp target map(mys[2].c)
  {
    mys[2].c++;
  }

  #pragma omp target map(mys[2].d)
  {
    mys[2].d++;
  }

  assert (mys[2].c == &c + 1);
  assert (mys[2].d == &c + 1);

  #pragma omp target map(mys[2].e)
  {
    mys[2].e[0]++;
  }

  #pragma omp target map(mys[2].f)
  {
    mys[2].f[0]++;
  }

  assert (mys[2].e[0] == 2);
  assert (mys[2].f[0] == 2);
}
#endif

#ifdef PTR_OFFSET_DECL_BASE
void
ptr_offset_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig[4] =
    {
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d)
    };
  S *mys = &mys_orig[0];

  #pragma omp target map(mys[2].a)
  {
    mys[2].a++;
  }

  #pragma omp target map(mys[2].b)
  {
    mys[2].b++;
  }

  assert (mys[2].a == 1);
  assert (mys[2].b == 1);

  #pragma omp target map(mys[2].c)
  {
    mys[2].c++;
  }

  #pragma omp target map(mys[2].d)
  {
    mys[2].d++;
  }

  assert (mys[2].c == &c + 1);
  assert (mys[2].d == &c + 1);

  #pragma omp target map(mys[2].e)
  {
    mys[2].e[0]++;
  }

  #pragma omp target map(mys[2].f)
  {
    mys[2].f[0]++;
  }

  assert (mys[2].e[0] == 2);
  assert (mys[2].f[0] == 2);
}
#endif

#ifdef REF2PTR_OFFSET_DECL_BASE
void
ref2ptr_offset_decl_base (void)
{
  int a = 0, b = 0, c, *d = &c;
  S mys_orig[4] =
    {
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d),
      S(a, b, &c, d)
    };
  S *mys_ptr = &mys_orig[0];
  S *&mys = mys_ptr;

  #pragma omp target map(mys[2].a)
  {
    mys[2].a++;
  }

  #pragma omp target map(mys[2].b)
  {
    mys[2].b++;
  }

  assert (mys[2].a == 1);
  assert (mys[2].b == 1);

  #pragma omp target map(mys[2].c)
  {
    mys[2].c++;
  }

  #pragma omp target map(mys[2].d)
  {
    mys[2].d++;
  }

  assert (mys[2].c == &c + 1);
  assert (mys[2].d == &c + 1);

  #pragma omp target map(mys[2].e)
  {
    mys[2].e[0]++;
  }

  #pragma omp target map(mys[2].f)
  {
    mys[2].f[0]++;
  }

  assert (mys[2].e[0] == 2);
  assert (mys[2].f[0] == 2);
}
#endif

#ifdef MAP_SECTIONS
void
map_sections (void)
{
  int arr[10];
  int *ptr;
  int (&arrref)[10] = arr;
  int *&ptrref = ptr;

  ptr = new int[10];
  memset (ptr, 0, sizeof (int) * 10);
  memset (arr, 0, sizeof (int) * 10);

  #pragma omp target map(arr[0:10])
  {
    arr[2]++;
  }

  #pragma omp target map(ptr[0:10])
  {
    ptr[2]++;
  }

  #pragma omp target map(arrref[0:10])
  {
    arrref[2]++;
  }

  #pragma omp target map(ptrref[0:10])
  {
    ptrref[2]++;
  }

  assert (arr[2] == 2);
  assert (ptr[2] == 2);

  delete ptr;
}
#endif

struct T {
  int a[10];
  int (&b)[10];
  int *c;
  int *&d;

  T(int (&b1)[10], int *c1, int *&d1) : b(b1), c(c1), d(d1)
  {
    memset (a, 0, sizeof a);
  }
};

#ifdef NONREF_DECL_MEMBER_SLICE
void
nonref_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt(c, &c[0], d);

  memset (c, 0, sizeof c);

  #pragma omp target map(myt.a[0:10])
  {
    myt.a[2]++;
  }

  #pragma omp target map(myt.b[0:10])
  {
    myt.b[2]++;
  }

  #pragma omp target enter data map(to: myt.c)

  #pragma omp target map(myt.c[0:10])
  {
    myt.c[2]++;
  }

  #pragma omp target exit data map(release: myt.c)

  #pragma omp target enter data map(to: myt.d)

  #pragma omp target map(myt.d[0:10])
  {
    myt.d[2]++;
  }

  #pragma omp target exit data map(from: myt.d)

  assert (myt.a[2] == 1);
  assert (myt.b[2] == 3);
  assert (myt.c[2] == 3);
  assert (myt.d[2] == 3);
}
#endif

#ifdef NONREF_DECL_MEMBER_SLICE_BASEPTR
void
nonref_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt(c, &c[0], d);

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt.c) map(myt.c[0:10])
  {
    myt.c[2]++;
  }

  #pragma omp target map(to:myt.d) map(myt.d[0:10])
  {
    myt.d[2]++;
  }

  assert (myt.c[2] == 2);
  assert (myt.d[2] == 2);
}
#endif

#ifdef REF_DECL_MEMBER_SLICE
void
ref_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T &myt = myt_real;

  memset (c, 0, sizeof c);

  #pragma omp target map(myt.a[0:10])
  {
    myt.a[2]++;
  }

  #pragma omp target map(myt.b[0:10])
  {
    myt.b[2]++;
  }

  #pragma omp target enter data map(to: myt.c)

  #pragma omp target map(myt.c[0:10])
  {
    myt.c[2]++;
  }

  #pragma omp target exit data map(release: myt.c)

  #pragma omp target enter data map(to: myt.d)

  #pragma omp target map(myt.d[0:10])
  {
    myt.d[2]++;
  }

  #pragma omp target exit data map(release: myt.d)

  assert (myt.a[2] == 1);
  assert (myt.b[2] == 3);
  assert (myt.c[2] == 3);
  assert (myt.d[2] == 3);
}
#endif

#ifdef REF_DECL_MEMBER_SLICE_BASEPTR
void
ref_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T &myt = myt_real;

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt.c) map(myt.c[0:10])
  {
    myt.c[2]++;
  }

  #pragma omp target map(to:myt.d) map(myt.d[0:10])
  {
    myt.d[2]++;
  }

  assert (myt.c[2] == 2);
  assert (myt.d[2] == 2);
}
#endif

#ifdef PTR_DECL_MEMBER_SLICE
void
ptr_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt = &myt_real;

  memset (c, 0, sizeof c);

  #pragma omp target enter data map(to: myt)

  #pragma omp target map(myt->a[0:10])
  {
    myt->a[2]++;
  }

  #pragma omp target map(myt->b[0:10])
  {
    myt->b[2]++;
  }

  #pragma omp target enter data map(to: myt->c)

  #pragma omp target map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target exit data map(release: myt->c)

  #pragma omp target enter data map(to: myt->d)

  #pragma omp target map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  #pragma omp target exit data map(release: myt, myt->d)

  assert (myt->a[2] == 1);
  assert (myt->b[2] == 3);
  assert (myt->c[2] == 3);
  assert (myt->d[2] == 3);
}
#endif

#ifdef PTR_DECL_MEMBER_SLICE_BASEPTR
void
ptr_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt = &myt_real;

  memset (c, 0, sizeof c);

  // These ones have an implicit firstprivate for 'myt'.
  #pragma omp target map(to:myt->c) map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target map(to:myt->d) map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  // These ones have an explicit "TO" mapping for 'myt'.
  #pragma omp target map(to:myt) map(to:myt->c) map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target map(to:myt) map(to:myt->d) map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  assert (myt->c[2] == 4);
  assert (myt->d[2] == 4);
}
#endif

#ifdef REF2PTR_DECL_MEMBER_SLICE
void
ref2ptr_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptr = &myt_real;
  T *&myt = myt_ptr;

  memset (c, 0, sizeof c);

  #pragma omp target enter data map(to: myt)

  #pragma omp target map(myt->a[0:10])
  {
    myt->a[2]++;
  }

  #pragma omp target map(myt->b[0:10])
  {
    myt->b[2]++;
  }

  #pragma omp target enter data map(to: myt->c)

  #pragma omp target map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target exit data map(release: myt->c)

  #pragma omp target enter data map(to: myt->d)

  #pragma omp target map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  #pragma omp target exit data map(from: myt, myt->d)

  assert (myt->a[2] == 1);
  assert (myt->b[2] == 3);
  assert (myt->c[2] == 3);
  assert (myt->d[2] == 3);
}
#endif

#ifdef REF2PTR_DECL_MEMBER_SLICE_BASEPTR
void
ref2ptr_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptr = &myt_real;
  T *&myt = myt_ptr;

  memset (c, 0, sizeof c);

  // These ones have an implicit firstprivate for 'myt'.
  #pragma omp target map(to:myt->c) map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target map(to:myt->d) map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  // These ones have an explicit "TO" mapping for 'myt'.
  #pragma omp target map(to:myt) map(to:myt->c) map(myt->c[0:10])
  {
    myt->c[2]++;
  }

  #pragma omp target map(to:myt) map(to:myt->d) map(myt->d[0:10])
  {
    myt->d[2]++;
  }

  assert (myt->c[2] == 4);
  assert (myt->d[2] == 4);
}
#endif

#ifdef ARRAY_DECL_MEMBER_SLICE
void
array_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };

  memset (c, 0, sizeof c);

  #pragma omp target map(myt[2].a[0:10])
  {
    myt[2].a[2]++;
  }

  #pragma omp target map(myt[2].b[0:10])
  {
    myt[2].b[2]++;
  }

  #pragma omp target enter data map(to: myt[2].c)

  #pragma omp target map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target exit data map(release: myt[2].c)

  #pragma omp target enter data map(to: myt[2].d)

  #pragma omp target map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  #pragma omp target exit data map(release: myt[2].d)

  assert (myt[2].a[2] == 1);
  assert (myt[2].b[2] == 3);
  assert (myt[2].c[2] == 3);
  assert (myt[2].d[2] == 3);
}
#endif

#ifdef ARRAY_DECL_MEMBER_SLICE_BASEPTR
void
array_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  assert (myt[2].c[2] == 2);
  assert (myt[2].d[2] == 2);
}
#endif

#ifdef REF2ARRAY_DECL_MEMBER_SLICE
void
ref2array_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T (&myt)[4] = myt_real;

  memset (c, 0, sizeof c);

  #pragma omp target map(myt[2].a[0:10])
  {
    myt[2].a[2]++;
  }

  #pragma omp target map(myt[2].b[0:10])
  {
    myt[2].b[2]++;
  }

  #pragma omp target enter data map(to: myt[2].c)

  #pragma omp target map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target exit data map(release: myt[2].c)

  #pragma omp target enter data map(to: myt[2].d)

  #pragma omp target map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  #pragma omp target exit data map(release: myt[2].d)

  assert (myt[2].a[2] == 1);
  assert (myt[2].b[2] == 3);
  assert (myt[2].c[2] == 3);
  assert (myt[2].d[2] == 3);
}
#endif

#ifdef REF2ARRAY_DECL_MEMBER_SLICE_BASEPTR
void
ref2array_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T (&myt)[4] = myt_real;

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  assert (myt[2].c[2] == 2);
  assert (myt[2].d[2] == 2);
}
#endif

#ifdef PTR_OFFSET_DECL_MEMBER_SLICE
void
ptr_offset_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T *myt = &myt_real[0];

  memset (c, 0, sizeof c);

  #pragma omp target map(myt[2].a[0:10])
  {
    myt[2].a[2]++;
  }

  #pragma omp target map(myt[2].b[0:10])
  {
    myt[2].b[2]++;
  }

  #pragma omp target enter data map(to: myt[2].c)

  #pragma omp target map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target exit data map(release: myt[2].c)

  #pragma omp target enter data map(to: myt[2].d)

  #pragma omp target map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  #pragma omp target exit data map(release: myt[2].d)

  assert (myt[2].a[2] == 1);
  assert (myt[2].b[2] == 3);
  assert (myt[2].c[2] == 3);
  assert (myt[2].d[2] == 3);
}
#endif

#ifdef PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
void
ptr_offset_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T *myt = &myt_real[0];

  memset (c, 0, sizeof c);

  /* Implicit 'myt'.  */
  #pragma omp target map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  /* Explicit 'to'-mapped 'myt'.  */
  #pragma omp target map(to:myt) map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt) map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  assert (myt[2].c[2] == 4);
  assert (myt[2].d[2] == 4);
}
#endif

#ifdef REF2PTR_OFFSET_DECL_MEMBER_SLICE
void
ref2ptr_offset_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T *myt_ptr = &myt_real[0];
  T *&myt = myt_ptr;

  memset (c, 0, sizeof c);

  #pragma omp target map(myt[2].a[0:10])
  {
    myt[2].a[2]++;
  }

  #pragma omp target map(myt[2].b[0:10])
  {
    myt[2].b[2]++;
  }

  #pragma omp target enter data map(to: myt[2].c)

  #pragma omp target map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target exit data map(release: myt[2].c)

  #pragma omp target enter data map(to: myt[2].d)

  #pragma omp target map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  #pragma omp target exit data map(release: myt[2].d)

  assert (myt[2].a[2] == 1);
  assert (myt[2].b[2] == 3);
  assert (myt[2].c[2] == 3);
  assert (myt[2].d[2] == 3);
}
#endif

#ifdef REF2PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
void
ref2ptr_offset_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real[4] =
    {
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d),
      T (c, &c[0], d)
    };
  T *myt_ptr = &myt_real[0];
  T *&myt = myt_ptr;

  memset (c, 0, sizeof c);

  /* Implicit 'myt'.  */
  #pragma omp target map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  /* Explicit 'to'-mapped 'myt'.  */
  #pragma omp target map(to:myt) map(to:myt[2].c) map(myt[2].c[0:10])
  {
    myt[2].c[2]++;
  }

  #pragma omp target map(to:myt) map(to:myt[2].d) map(myt[2].d[0:10])
  {
    myt[2].d[2]++;
  }

  assert (myt[2].c[2] == 4);
  assert (myt[2].d[2] == 4);
}
#endif

#ifdef PTRARRAY_DECL_MEMBER_SLICE
void
ptrarray_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt[4] =
    {
      &myt_real,
      &myt_real,
      &myt_real,
      &myt_real
    };

  memset (c, 0, sizeof c);

  #pragma omp target enter data map(to: myt[2])

  #pragma omp target map(myt[2]->a[0:10])
  {
    myt[2]->a[2]++;
  }

  #pragma omp target map(myt[2]->b[0:10])
  {
    myt[2]->b[2]++;
  }

  #pragma omp target enter data map(to: myt[2]->c)

  #pragma omp target map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target exit data map(from: myt[2]->c)

  #pragma omp target enter data map(to: myt[2]->d)

  #pragma omp target map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target exit data map(from: myt[2]->d)

  #pragma omp target exit data map(release: myt[2])

  assert (myt[2]->a[2] == 1);
  assert (myt[2]->b[2] == 3);
  assert (myt[2]->c[2] == 3);
  assert (myt[2]->d[2] == 3);
}
#endif

#ifdef PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
void
ptrarray_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt[4] =
    {
      &myt_real,
      &myt_real,
      &myt_real,
      &myt_real
    };

  memset (c, 0, sizeof c);

  // Implicit 'myt'
  #pragma omp target map(to: myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to: myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  // One element of 'myt'
  #pragma omp target map(to:myt[2], myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt[2], myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  // Explicit map of all of 'myt'
  #pragma omp target map(to:myt, myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt, myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  // Explicit map slice of 'myt'
  #pragma omp target map(to:myt[1:3], myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt[1:3], myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  assert (myt[2]->c[2] == 8);
  assert (myt[2]->d[2] == 8);
}
#endif

#ifdef REF2PTRARRAY_DECL_MEMBER_SLICE
void
ref2ptrarray_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      &myt_real,
      &myt_real,
      &myt_real,
      &myt_real
    };
  T *(&myt)[4] = myt_ptrarr;

  memset (c, 0, sizeof c);

  #pragma omp target enter data map(to: myt[2])

  #pragma omp target map(myt[2]->a[0:10])
  {
    myt[2]->a[2]++;
  }

  #pragma omp target map(myt[2]->b[0:10])
  {
    myt[2]->b[2]++;
  }

  #pragma omp target enter data map(to: myt[2]->c)

  #pragma omp target map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target exit data map(release: myt[2]->c)

  #pragma omp target enter data map(to: myt[2]->d)

  #pragma omp target map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target exit data map(release: myt[2]->d)

  #pragma omp target exit data map(release: myt[2])

  assert (myt[2]->a[2] == 1);
  assert (myt[2]->b[2] == 3);
  assert (myt[2]->c[2] == 3);
  assert (myt[2]->d[2] == 3);
}
#endif

#ifdef REF2PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
void
ref2ptrarray_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      &myt_real,
      &myt_real,
      &myt_real,
      &myt_real
    };
  T *(&myt)[4] = myt_ptrarr;

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt[2], myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt[2], myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target map(to:myt, myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt, myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  assert (myt[2]->c[2] == 4);
  assert (myt[2]->d[2] == 4);
}
#endif

#ifdef PTRPTR_OFFSET_DECL_MEMBER_SLICE
void
ptrptr_offset_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      &myt_real,
      &myt_real,
      &myt_real,
      &myt_real
    };
  T **myt = &myt_ptrarr[0];

  memset (c, 0, sizeof c);

  #pragma omp target enter data map(to: myt[0:3])

  /* NOTE: For the implicit firstprivate 'myt' to work, the zeroth element of
     myt[] must be mapped above -- otherwise the zero-length array section
     lookup fails.  */
  #pragma omp target map(myt[2]->a[0:10])
  {
    myt[2]->a[2]++;
  }

  #pragma omp target map(myt[2]->b[0:10])
  {
    myt[2]->b[2]++;
  }

  #pragma omp target enter data map(to: myt[2]->c)

  #pragma omp target map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target exit data map(from: myt[2]->c)

  #pragma omp target enter data map(to: myt[2]->d)

  #pragma omp target map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target exit data map(from: myt[0:3], myt[2]->d)

  assert (myt[2]->a[2] == 1);
  assert (myt[2]->b[2] == 3);
  assert (myt[2]->c[2] == 3);
  assert (myt[2]->d[2] == 3);
}
#endif

#ifdef PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
void
ptrptr_offset_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      0,
      0,
      0,
      &myt_real
    };
  T **myt = &myt_ptrarr[0];

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt[3], myt[3]->c) map(myt[3]->c[0:10])
  {
    myt[3]->c[2]++;
  }

  #pragma omp target map(to:myt[3], myt[3]->d) map(myt[3]->d[0:10])
  {
    myt[3]->d[2]++;
  }

  #pragma omp target map(to:myt, myt[3], myt[3]->c) map(myt[3]->c[0:10])
  {
    myt[3]->c[2]++;
  }

  #pragma omp target map(to:myt, myt[3], myt[3]->d) map(myt[3]->d[0:10])
  {
    myt[3]->d[2]++;
  }

  assert (myt[3]->c[2] == 4);
  assert (myt[3]->d[2] == 4);
}
#endif

#ifdef REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE
void
ref2ptrptr_offset_decl_member_slice (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      0,
      0,
      &myt_real,
      0
    };
  T **myt_ptrptr = &myt_ptrarr[0];
  T **&myt = myt_ptrptr;

  memset (c, 0, sizeof c);

  /* NOTE: It's possible that this should work without explicitly
     mapping "myt" here, allowing it to be implicitly
     mapped "firstprivate" by the "omp target" directive
     below.  That doesn't work for now though (see comment
     in gimplify.cc:gimplify_adjust_omp_clauses_1).  See also
     ptrptr_offset_decl_member_slice above (where the explicit mapping
     can be omitted).  */
  #pragma omp target enter data map(to: myt, myt[0:3])

  #pragma omp target map(myt[2]->a[0:10])
  {
    myt[2]->a[2]++;
  }

  #pragma omp target map(myt[2]->b[0:10])
  {
    myt[2]->b[2]++;
  }

  #pragma omp target enter data map(to:myt[2]->c)

  #pragma omp target map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target exit data map(release:myt[2]->c)

  #pragma omp target enter data map(to:myt[2]->d)

  #pragma omp target map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target exit data map(release: myt, myt[0:3], myt[2]->d)

  assert (myt[2]->a[2] == 1);
  assert (myt[2]->b[2] == 3);
  assert (myt[2]->c[2] == 3);
  assert (myt[2]->d[2] == 3);
}
#endif

#ifdef REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
void
ref2ptrptr_offset_decl_member_slice_baseptr (void)
{
  int c[10];
  int *d = &c[0];
  T myt_real(c, &c[0], d);
  T *myt_ptrarr[4] =
    {
      0,
      0,
      &myt_real,
      0
    };
  T **myt_ptrptr = &myt_ptrarr[0];
  T **&myt = myt_ptrptr;

  memset (c, 0, sizeof c);

  #pragma omp target map(to:myt[2], myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt[2], myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  #pragma omp target map(to:myt, myt[2], myt[2]->c) map(myt[2]->c[0:10])
  {
    myt[2]->c[2]++;
  }

  #pragma omp target map(to:myt, myt[2], myt[2]->d) map(myt[2]->d[0:10])
  {
    myt[2]->d[2]++;
  }

  assert (myt[2]->c[2] == 4);
  assert (myt[2]->d[2] == 4);
}
#endif

struct U
{
  S s1;
  T t1;
  S &s2;
  T &t2;
  S *s3;
  T *t3;
  S *&s4;
  T *&t4;

  U(S &sptr1, T &tptr1, S &sptr2, T &tptr2, S *sptr3, T *tptr3,
    S *&sptr4, T *&tptr4)
    : s1(sptr1), t1(tptr1), s2(sptr2), t2(tptr2), s3(sptr3), t3(tptr3),
      s4(sptr4), t4(tptr4)
  {
  }
};

#define INIT_S(N)				\
  int a##N = 0, b##N = 0, c##N = 0, d##N = 0;	\
  int *d##N##ptr = &d##N;			\
  S s##N(a##N, b##N, &c##N, d##N##ptr)

#define INIT_T(N)				\
  int arr##N[10];				\
  int *ptr##N = &arr##N[0];			\
  T t##N(arr##N, &arr##N[0], ptr##N);		\
  memset (arr##N, 0, sizeof arr##N)

#define INIT_ST				\
  INIT_S(1);				\
  INIT_T(1);				\
  INIT_S(2);				\
  INIT_T(2);				\
  INIT_S(3);				\
  INIT_T(3);				\
  int a4 = 0, b4 = 0, c4 = 0, d4 = 0;	\
  int *d4ptr = &d4;			\
  S *s4 = new S(a4, b4, &c4, d4ptr);	\
  int arr4[10];				\
  int *ptr4 = &arr4[0];			\
  T *t4 = new T(arr4, &arr4[0], ptr4);	\
  memset (arr4, 0, sizeof arr4)

#ifdef NONREF_COMPONENT_BASE
void
nonref_component_base (void)
{
  INIT_ST;
  U myu(s1, t1, s2, t2, &s3, &t3, s4, t4);

  #pragma omp target map(myu.s1.a, myu.s1.b, myu.s1.c, myu.s1.d)
  {
    myu.s1.a++;
    myu.s1.b++;
    myu.s1.c++;
    myu.s1.d++;
  }

  assert (myu.s1.a == 1);
  assert (myu.s1.b == 1);
  assert (myu.s1.c == &c1 + 1);
  assert (myu.s1.d == &d1 + 1);

  #pragma omp target map(myu.s2.a, myu.s2.b, myu.s2.c, myu.s2.d)
  {
    myu.s2.a++;
    myu.s2.b++;
    myu.s2.c++;
    myu.s2.d++;
  }

  assert (myu.s2.a == 1);
  assert (myu.s2.b == 1);
  assert (myu.s2.c == &c2 + 1);
  assert (myu.s2.d == &d2 + 1);

  #pragma omp target map(to:myu.s3) \
		     map(myu.s3->a, myu.s3->b, myu.s3->c, myu.s3->d)
  {
    myu.s3->a++;
    myu.s3->b++;
    myu.s3->c++;
    myu.s3->d++;
  }

  assert (myu.s3->a == 1);
  assert (myu.s3->b == 1);
  assert (myu.s3->c == &c3 + 1);
  assert (myu.s3->d == &d3 + 1);

  #pragma omp target map(to:myu.s4) \
		     map(myu.s4->a, myu.s4->b, myu.s4->c, myu.s4->d)
  {
    myu.s4->a++;
    myu.s4->b++;
    myu.s4->c++;
    myu.s4->d++;
  }

  assert (myu.s4->a == 1);
  assert (myu.s4->b == 1);
  assert (myu.s4->c == &c4 + 1);
  assert (myu.s4->d == &d4 + 1);

  delete s4;
  delete t4;
}
#endif

#ifdef NONREF_COMPONENT_MEMBER_SLICE
void
nonref_component_member_slice (void)
{
  INIT_ST;
  U myu(s1, t1, s2, t2, &s3, &t3, s4, t4);

  #pragma omp target map(myu.t1.a[2:5])
  {
    myu.t1.a[2]++;
  }

  #pragma omp target map(myu.t1.b[2:5])
  {
    myu.t1.b[2]++;
  }

  #pragma omp target enter data map(to: myu.t1.c)

  #pragma omp target map(myu.t1.c[2:5])
  {
    myu.t1.c[2]++;
  }

  #pragma omp target exit data map(release: myu.t1.c)

  #pragma omp target enter data map(to: myu.t1.d)

  #pragma omp target map(myu.t1.d[2:5])
  {
    myu.t1.d[2]++;
  }

  #pragma omp target exit data map(from: myu.t1.d)

  assert (myu.t1.a[2] == 1);
  assert (myu.t1.b[2] == 3);
  assert (myu.t1.c[2] == 3);
  assert (myu.t1.d[2] == 3);

  #pragma omp target map(myu.t2.a[2:5])
  {
    myu.t2.a[2]++;
  }

  #pragma omp target map(myu.t2.b[2:5])
  {
    myu.t2.b[2]++;
  }

  #pragma omp target enter data map(to: myu.t2.c)

  #pragma omp target map(myu.t2.c[2:5])
  {
    myu.t2.c[2]++;
  }

  #pragma omp target exit data map(release: myu.t2.c)

  #pragma omp target enter data map(to: myu.t2.d)

  #pragma omp target map(myu.t2.d[2:5])
  {
    myu.t2.d[2]++;
  }

  #pragma omp target exit data map(release: myu.t2.d)

  assert (myu.t2.a[2] == 1);
  assert (myu.t2.b[2] == 3);
  assert (myu.t2.c[2] == 3);
  assert (myu.t2.d[2] == 3);

  #pragma omp target enter data map(to: myu.t3)

  #pragma omp target map(myu.t3->a[2:5])
  {
    myu.t3->a[2]++;
  }

  #pragma omp target map(myu.t3->b[2:5])
  {
    myu.t3->b[2]++;
  }

  #pragma omp target enter data map(to: myu.t3->c)

  #pragma omp target map(myu.t3->c[2:5])
  {
    myu.t3->c[2]++;
  }

  #pragma omp target exit data map(release: myu.t3->c)

  #pragma omp target enter data map(to: myu.t3->d)

  #pragma omp target map(myu.t3->d[2:5])
  {
    myu.t3->d[2]++;
  }

  #pragma omp target exit data map(release: myu.t3, myu.t3->d)

  assert (myu.t3->a[2] == 1);
  assert (myu.t3->b[2] == 3);
  assert (myu.t3->c[2] == 3);
  assert (myu.t3->d[2] == 3);

  #pragma omp target enter data map(to: myu.t4)

  #pragma omp target map(myu.t4->a[2:5])
  {
    myu.t4->a[2]++;
  }

  #pragma omp target map(myu.t4->b[2:5])
  {
    myu.t4->b[2]++;
  }

  #pragma omp target enter data map(to: myu.t4->c)

  #pragma omp target map(myu.t4->c[2:5])
  {
    myu.t4->c[2]++;
  }

  #pragma omp target exit data map(release: myu.t4->c)

  #pragma omp target enter data map(to: myu.t4->d)

  #pragma omp target map(myu.t4->d[2:5])
  {
    myu.t4->d[2]++;
  }

  #pragma omp target exit data map(release: myu.t4, myu.t4->d)

  assert (myu.t4->a[2] == 1);
  assert (myu.t4->b[2] == 3);
  assert (myu.t4->c[2] == 3);
  assert (myu.t4->d[2] == 3);

  delete s4;
  delete t4;
}
#endif

#ifdef NONREF_COMPONENT_MEMBER_SLICE_BASEPTR
void
nonref_component_member_slice_baseptr (void)
{
  INIT_ST;
  U myu(s1, t1, s2, t2, &s3, &t3, s4, t4);

  #pragma omp target map(to: myu.t1.c) map(myu.t1.c[2:5])
  {
    myu.t1.c[2]++;
  }

  #pragma omp target map(to: myu.t1.d) map(myu.t1.d[2:5])
  {
    myu.t1.d[2]++;
  }

  assert (myu.t1.c[2] == 2);
  assert (myu.t1.d[2] == 2);

  #pragma omp target map(to: myu.t2.c) map(myu.t2.c[2:5])
  {
    myu.t2.c[2]++;
  }

  #pragma omp target map(to: myu.t2.d) map(myu.t2.d[2:5])
  {
    myu.t2.d[2]++;
  }

  assert (myu.t2.c[2] == 2);
  assert (myu.t2.d[2] == 2);

  #pragma omp target map(to: myu.t3, myu.t3->c) map(myu.t3->c[2:5])
  {
    myu.t3->c[2]++;
  }

  #pragma omp target map(to: myu.t3, myu.t3->d) map(myu.t3->d[2:5])
  {
    myu.t3->d[2]++;
  }

  assert (myu.t3->c[2] == 2);
  assert (myu.t3->d[2] == 2);

  #pragma omp target map(to: myu.t4, myu.t4->c) map(myu.t4->c[2:5])
  {
    myu.t4->c[2]++;
  }

  #pragma omp target map(to: myu.t4, myu.t4->d) map(myu.t4->d[2:5])
  {
    myu.t4->d[2]++;
  }

  assert (myu.t4->c[2] == 2);
  assert (myu.t4->d[2] == 2);

  delete s4;
  delete t4;
}
#endif

#ifdef REF_COMPONENT_BASE
void
ref_component_base (void)
{
  INIT_ST;
  U myu_real(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U &myu = myu_real;

  #pragma omp target map(myu.s1.a, myu.s1.b, myu.s1.c, myu.s1.d)
  {
    myu.s1.a++;
    myu.s1.b++;
    myu.s1.c++;
    myu.s1.d++;
  }

  assert (myu.s1.a == 1);
  assert (myu.s1.b == 1);
  assert (myu.s1.c == &c1 + 1);
  assert (myu.s1.d == &d1 + 1);

  #pragma omp target map(myu.s2.a, myu.s2.b, myu.s2.c, myu.s2.d)
  {
    myu.s2.a++;
    myu.s2.b++;
    myu.s2.c++;
    myu.s2.d++;
  }

  assert (myu.s2.a == 1);
  assert (myu.s2.b == 1);
  assert (myu.s2.c == &c2 + 1);
  assert (myu.s2.d == &d2 + 1);

  #pragma omp target map(to:myu.s3) \
		     map(myu.s3->a, myu.s3->b, myu.s3->c, myu.s3->d)
  {
    myu.s3->a++;
    myu.s3->b++;
    myu.s3->c++;
    myu.s3->d++;
  }

  assert (myu.s3->a == 1);
  assert (myu.s3->b == 1);
  assert (myu.s3->c == &c3 + 1);
  assert (myu.s3->d == &d3 + 1);

  #pragma omp target map(to:myu.s4) \
		     map(myu.s4->a, myu.s4->b, myu.s4->c, myu.s4->d)
  {
    myu.s4->a++;
    myu.s4->b++;
    myu.s4->c++;
    myu.s4->d++;
  }

  assert (myu.s4->a == 1);
  assert (myu.s4->b == 1);
  assert (myu.s4->c == &c4 + 1);
  assert (myu.s4->d == &d4 + 1);

  delete s4;
  delete t4;
}
#endif

#ifdef REF_COMPONENT_MEMBER_SLICE
void
ref_component_member_slice (void)
{
  INIT_ST;
  U myu_real(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U &myu = myu_real;

  #pragma omp target map(myu.t1.a[2:5])
  {
    myu.t1.a[2]++;
  }

  #pragma omp target map(myu.t1.b[2:5])
  {
    myu.t1.b[2]++;
  }

  #pragma omp target enter data map(to: myu.t1.c)

  #pragma omp target map(myu.t1.c[2:5])
  {
    myu.t1.c[2]++;
  }

  #pragma omp target exit data map(release: myu.t1.c)

  #pragma omp target enter data map(to: myu.t1.d)

  #pragma omp target map(myu.t1.d[2:5])
  {
    myu.t1.d[2]++;
  }

  #pragma omp target exit data map(release: myu.t1.d)

  assert (myu.t1.a[2] == 1);
  assert (myu.t1.b[2] == 3);
  assert (myu.t1.c[2] == 3);
  assert (myu.t1.d[2] == 3);

  #pragma omp target map(myu.t2.a[2:5])
  {
    myu.t2.a[2]++;
  }

  #pragma omp target map(myu.t2.b[2:5])
  {
    myu.t2.b[2]++;
  }

  #pragma omp target enter data map(to: myu.t2.c)

  #pragma omp target map(myu.t2.c[2:5])
  {
    myu.t2.c[2]++;
  }

  #pragma omp target exit data map(release: myu.t2.c)

  #pragma omp target enter data map(to: myu.t2.d)

  #pragma omp target map(myu.t2.d[2:5])
  {
    myu.t2.d[2]++;
  }

  #pragma omp target exit data map(release: myu.t2.d)

  assert (myu.t2.a[2] == 1);
  assert (myu.t2.b[2] == 3);
  assert (myu.t2.c[2] == 3);
  assert (myu.t2.d[2] == 3);

  #pragma omp target enter data map(to: myu.t3)

  #pragma omp target map(myu.t3->a[2:5])
  {
    myu.t3->a[2]++;
  }

  #pragma omp target map(myu.t3->b[2:5])
  {
    myu.t3->b[2]++;
  }

  #pragma omp target enter data map(to: myu.t3->c)

  #pragma omp target map(myu.t3->c[2:5])
  {
    myu.t3->c[2]++;
  }

  #pragma omp target exit data map(release: myu.t3->c)

  #pragma omp target enter data map(to: myu.t3->d)

  #pragma omp target map(myu.t3->d[2:5])
  {
    myu.t3->d[2]++;
  }

  #pragma omp target exit data map(release: myu.t3, myu.t3->d)

  assert (myu.t3->a[2] == 1);
  assert (myu.t3->b[2] == 3);
  assert (myu.t3->c[2] == 3);
  assert (myu.t3->d[2] == 3);

  #pragma omp target enter data map(to: myu.t4)

  #pragma omp target map(myu.t4->a[2:5])
  {
    myu.t4->a[2]++;
  }

  #pragma omp target map(myu.t4->b[2:5])
  {
    myu.t4->b[2]++;
  }

  #pragma omp target enter data map(to: myu.t4->c)

  #pragma omp target map(myu.t4->c[2:5])
  {
    myu.t4->c[2]++;
  }

  #pragma omp target exit data map(release: myu.t4->c)

  #pragma omp target enter data map(to: myu.t4->d)

  #pragma omp target map(myu.t4->d[2:5])
  {
    myu.t4->d[2]++;
  }

  #pragma omp target exit data map(release: myu.t4, myu.t4->d)

  assert (myu.t4->a[2] == 1);
  assert (myu.t4->b[2] == 3);
  assert (myu.t4->c[2] == 3);
  assert (myu.t4->d[2] == 3);

  delete s4;
  delete t4;
}
#endif

#ifdef REF_COMPONENT_MEMBER_SLICE_BASEPTR
void
ref_component_member_slice_baseptr (void)
{
  INIT_ST;
  U myu_real(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U &myu = myu_real;

  #pragma omp target map(to: myu.t1.c) map(myu.t1.c[2:5])
  {
    myu.t1.c[2]++;
  }

  #pragma omp target map(to: myu.t1.d) map(myu.t1.d[2:5])
  {
    myu.t1.d[2]++;
  }

  assert (myu.t1.c[2] == 2);
  assert (myu.t1.d[2] == 2);

  #pragma omp target map(to: myu.t2.c) map(myu.t2.c[2:5])
  {
    myu.t2.c[2]++;
  }

  #pragma omp target map(to: myu.t2.d) map(myu.t2.d[2:5])
  {
    myu.t2.d[2]++;
  }

  assert (myu.t2.c[2] == 2);
  assert (myu.t2.d[2] == 2);

  #pragma omp target map(to: myu.t3, myu.t3->c) map(myu.t3->c[2:5])
  {
    myu.t3->c[2]++;
  }

  #pragma omp target map(to: myu.t3, myu.t3->d) map(myu.t3->d[2:5])
  {
    myu.t3->d[2]++;
  }

  assert (myu.t3->c[2] == 2);
  assert (myu.t3->d[2] == 2);

  #pragma omp target map(to: myu.t4, myu.t4->c) map(myu.t4->c[2:5])
  {
    myu.t4->c[2]++;
  }

  #pragma omp target map(to: myu.t4, myu.t4->d) map(myu.t4->d[2:5])
  {
    myu.t4->d[2]++;
  }

  assert (myu.t4->c[2] == 2);
  assert (myu.t4->d[2] == 2);

  delete s4;
  delete t4;
}
#endif

#ifdef PTR_COMPONENT_BASE
void
ptr_component_base (void)
{
  INIT_ST;
  U *myu = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);

  #pragma omp target map(myu->s1.a, myu->s1.b, myu->s1.c, myu->s1.d)
  {
    myu->s1.a++;
    myu->s1.b++;
    myu->s1.c++;
    myu->s1.d++;
  }

  assert (myu->s1.a == 1);
  assert (myu->s1.b == 1);
  assert (myu->s1.c == &c1 + 1);
  assert (myu->s1.d == &d1 + 1);

  #pragma omp target map(myu->s2.a, myu->s2.b, myu->s2.c, myu->s2.d)
  {
    myu->s2.a++;
    myu->s2.b++;
    myu->s2.c++;
    myu->s2.d++;
  }

  assert (myu->s2.a == 1);
  assert (myu->s2.b == 1);
  assert (myu->s2.c == &c2 + 1);
  assert (myu->s2.d == &d2 + 1);

  #pragma omp target map(to:myu->s3) \
		     map(myu->s3->a, myu->s3->b, myu->s3->c, myu->s3->d)
  {
    myu->s3->a++;
    myu->s3->b++;
    myu->s3->c++;
    myu->s3->d++;
  }

  assert (myu->s3->a == 1);
  assert (myu->s3->b == 1);
  assert (myu->s3->c == &c3 + 1);
  assert (myu->s3->d == &d3 + 1);

  #pragma omp target map(to:myu->s4) \
		     map(myu->s4->a, myu->s4->b, myu->s4->c, myu->s4->d)
  {
    myu->s4->a++;
    myu->s4->b++;
    myu->s4->c++;
    myu->s4->d++;
  }

  assert (myu->s4->a == 1);
  assert (myu->s4->b == 1);
  assert (myu->s4->c == &c4 + 1);
  assert (myu->s4->d == &d4 + 1);

  delete s4;
  delete t4;
  delete myu;
}
#endif

#ifdef PTR_COMPONENT_MEMBER_SLICE
void
ptr_component_member_slice (void)
{
  INIT_ST;
  U *myu = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);

  #pragma omp target map(myu->t1.a[2:5])
  {
    myu->t1.a[2]++;
  }

  #pragma omp target map(myu->t1.b[2:5])
  {
    myu->t1.b[2]++;
  }

  #pragma omp target enter data map(to: myu->t1.c)

  #pragma omp target map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target exit data map(release: myu->t1.c)

  #pragma omp target enter data map(to: myu->t1.d)

  #pragma omp target map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  #pragma omp target exit data map(release: myu->t1.d)

  assert (myu->t1.a[2] == 1);
  assert (myu->t1.b[2] == 3);
  assert (myu->t1.c[2] == 3);
  assert (myu->t1.d[2] == 3);

  #pragma omp target map(myu->t2.a[2:5])
  {
    myu->t2.a[2]++;
  }

  #pragma omp target map(myu->t2.b[2:5])
  {
    myu->t2.b[2]++;
  }

  #pragma omp target enter data map(to: myu->t2.c)

  #pragma omp target map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target exit data map(release: myu->t2.c)

  #pragma omp target enter data map(to: myu->t2.d)

  #pragma omp target map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  #pragma omp target exit data map(release: myu->t2.d)

  assert (myu->t2.a[2] == 1);
  assert (myu->t2.b[2] == 3);
  assert (myu->t2.c[2] == 3);
  assert (myu->t2.d[2] == 3);

  #pragma omp target enter data map(to: myu->t3)

  #pragma omp target map(myu->t3->a[2:5])
  {
    myu->t3->a[2]++;
  }

  #pragma omp target map(myu->t3->b[2:5])
  {
    myu->t3->b[2]++;
  }

  #pragma omp target enter data map(to: myu->t3->c)

  #pragma omp target map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target exit data map(release: myu->t3->c)

  #pragma omp target enter data map(to: myu->t3->d)

  #pragma omp target map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  #pragma omp target exit data map(release: myu->t3, myu->t3->d)

  assert (myu->t3->a[2] == 1);
  assert (myu->t3->b[2] == 3);
  assert (myu->t3->c[2] == 3);
  assert (myu->t3->d[2] == 3);

  #pragma omp target enter data map(to: myu->t4)

  #pragma omp target map(myu->t4->a[2:5])
  {
    myu->t4->a[2]++;
  }

  #pragma omp target map(myu->t4->b[2:5])
  {
    myu->t4->b[2]++;
  }

  #pragma omp target enter data map(to: myu->t4->c)

  #pragma omp target map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target exit data map(release: myu->t4->c)

  #pragma omp target enter data map(to: myu->t4->d)

  #pragma omp target map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  #pragma omp target exit data map(release: myu->t4, myu->t4->d)

  assert (myu->t4->a[2] == 1);
  assert (myu->t4->b[2] == 3);
  assert (myu->t4->c[2] == 3);
  assert (myu->t4->d[2] == 3);

  delete s4;
  delete t4;
  delete myu;
}
#endif

#ifdef PTR_COMPONENT_MEMBER_SLICE_BASEPTR
void
ptr_component_member_slice_baseptr (void)
{
  INIT_ST;
  U *myu = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t1.c) map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target map(to: myu->t1.d) map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  assert (myu->t1.c[2] == 2);
  assert (myu->t1.d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t1.c) map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target map(to: myu, myu->t1.d) map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  assert (myu->t1.c[2] == 4);
  assert (myu->t1.d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t2.c) map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target map(to: myu->t2.d) map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  assert (myu->t2.c[2] == 2);
  assert (myu->t2.d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t2.c) map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target map(to: myu, myu->t2.d) map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  assert (myu->t2.c[2] == 4);
  assert (myu->t2.d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t3, myu->t3->c) map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target map(to: myu->t3, myu->t3->d) map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  assert (myu->t3->c[2] == 2);
  assert (myu->t3->d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t3, myu->t3->c) map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target map(to: myu, myu->t3, myu->t3->d) map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  assert (myu->t3->c[2] == 4);
  assert (myu->t3->d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t4, myu->t4->c) map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target map(to: myu->t4, myu->t4->d) map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  assert (myu->t4->c[2] == 2);
  assert (myu->t4->d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t4, myu->t4->c) map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target map(to: myu, myu->t4, myu->t4->d) map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  assert (myu->t4->c[2] == 4);
  assert (myu->t4->d[2] == 4);

  delete s4;
  delete t4;
  delete myu;
}
#endif

#ifdef REF2PTR_COMPONENT_BASE
void
ref2ptr_component_base (void)
{
  INIT_ST;
  U *myu_ptr = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U *&myu = myu_ptr;

  #pragma omp target map(myu->s1.a, myu->s1.b, myu->s1.c, myu->s1.d)
  {
    myu->s1.a++;
    myu->s1.b++;
    myu->s1.c++;
    myu->s1.d++;
  }

  assert (myu->s1.a == 1);
  assert (myu->s1.b == 1);
  assert (myu->s1.c == &c1 + 1);
  assert (myu->s1.d == &d1 + 1);

  #pragma omp target map(myu->s2.a, myu->s2.b, myu->s2.c, myu->s2.d)
  {
    myu->s2.a++;
    myu->s2.b++;
    myu->s2.c++;
    myu->s2.d++;
  }

  assert (myu->s2.a == 1);
  assert (myu->s2.b == 1);
  assert (myu->s2.c == &c2 + 1);
  assert (myu->s2.d == &d2 + 1);

  #pragma omp target map(to:myu->s3) \
		     map(myu->s3->a, myu->s3->b, myu->s3->c, myu->s3->d)
  {
    myu->s3->a++;
    myu->s3->b++;
    myu->s3->c++;
    myu->s3->d++;
  }

  assert (myu->s3->a == 1);
  assert (myu->s3->b == 1);
  assert (myu->s3->c == &c3 + 1);
  assert (myu->s3->d == &d3 + 1);

  #pragma omp target map(to:myu->s4) \
		     map(myu->s4->a, myu->s4->b, myu->s4->c, myu->s4->d)
  {
    myu->s4->a++;
    myu->s4->b++;
    myu->s4->c++;
    myu->s4->d++;
  }

  assert (myu->s4->a == 1);
  assert (myu->s4->b == 1);
  assert (myu->s4->c == &c4 + 1);
  assert (myu->s4->d == &d4 + 1);

  delete s4;
  delete t4;
  delete myu_ptr;
}
#endif

#ifdef REF2PTR_COMPONENT_MEMBER_SLICE
void
ref2ptr_component_member_slice (void)
{
  INIT_ST;
  U *myu_ptr = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U *&myu = myu_ptr;

  #pragma omp target map(myu->t1.a[2:5])
  {
    myu->t1.a[2]++;
  }

  #pragma omp target map(myu->t1.b[2:5])
  {
    myu->t1.b[2]++;
  }

  #pragma omp target enter data map(to: myu->t1.c)

  #pragma omp target map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target exit data map(release: myu->t1.c)

  #pragma omp target enter data map(to: myu->t1.d)

  #pragma omp target map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  #pragma omp target exit data map(release: myu->t1.d)

  assert (myu->t1.a[2] == 1);
  assert (myu->t1.b[2] == 3);
  assert (myu->t1.c[2] == 3);
  assert (myu->t1.d[2] == 3);

  #pragma omp target map(myu->t2.a[2:5])
  {
    myu->t2.a[2]++;
  }

  #pragma omp target map(myu->t2.b[2:5])
  {
    myu->t2.b[2]++;
  }

  #pragma omp target enter data map(to: myu->t2.c)

  #pragma omp target map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target exit data map(release: myu->t2.c)

  #pragma omp target enter data map(to: myu->t2.d)

  #pragma omp target map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  #pragma omp target exit data map(release: myu->t2.d)

  assert (myu->t2.a[2] == 1);
  assert (myu->t2.b[2] == 3);
  assert (myu->t2.c[2] == 3);
  assert (myu->t2.d[2] == 3);

  #pragma omp target enter data map(to: myu->t3)

  #pragma omp target map(myu->t3->a[2:5])
  {
    myu->t3->a[2]++;
  }

  #pragma omp target map(myu->t3->b[2:5])
  {
    myu->t3->b[2]++;
  }

  #pragma omp target enter data map(to: myu->t3->c)

  #pragma omp target map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target exit data map(release: myu->t3->c)

  #pragma omp target enter data map(to: myu->t3->d)

  #pragma omp target map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  #pragma omp target exit data map(release: myu->t3, myu->t3->d)

  assert (myu->t3->a[2] == 1);
  assert (myu->t3->b[2] == 3);
  assert (myu->t3->c[2] == 3);
  assert (myu->t3->d[2] == 3);

  #pragma omp target enter data map(to: myu->t4)

  #pragma omp target map(myu->t4->a[2:5])
  {
    myu->t4->a[2]++;
  }

  #pragma omp target map(myu->t4->b[2:5])
  {
    myu->t4->b[2]++;
  }

  #pragma omp target enter data map(to: myu->t4->c)

  #pragma omp target map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target exit data map(release: myu->t4->c)

  #pragma omp target enter data map(to: myu->t4->d)

  #pragma omp target map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  #pragma omp target exit data map(release: myu->t4, myu->t4->d)

  assert (myu->t4->a[2] == 1);
  assert (myu->t4->b[2] == 3);
  assert (myu->t4->c[2] == 3);
  assert (myu->t4->d[2] == 3);

  delete s4;
  delete t4;
  delete myu_ptr;
}
#endif

#ifdef REF2PTR_COMPONENT_MEMBER_SLICE_BASEPTR
void
ref2ptr_component_member_slice_baseptr (void)
{
  INIT_ST;
  U *myu_ptr = new U(s1, t1, s2, t2, &s3, &t3, s4, t4);
  U *&myu = myu_ptr;

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t1.c) map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target map(to: myu->t1.d) map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  assert (myu->t1.c[2] == 2);
  assert (myu->t1.d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t1.c) map(myu->t1.c[2:5])
  {
    myu->t1.c[2]++;
  }

  #pragma omp target map(to: myu, myu->t1.d) map(myu->t1.d[2:5])
  {
    myu->t1.d[2]++;
  }

  assert (myu->t1.c[2] == 4);
  assert (myu->t1.d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t2.c) map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target map(to: myu->t2.d) map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  assert (myu->t2.c[2] == 2);
  assert (myu->t2.d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t2.c) map(myu->t2.c[2:5])
  {
    myu->t2.c[2]++;
  }

  #pragma omp target map(to: myu, myu->t2.d) map(myu->t2.d[2:5])
  {
    myu->t2.d[2]++;
  }

  assert (myu->t2.c[2] == 4);
  assert (myu->t2.d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t3, myu->t3->c) map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target map(to: myu->t3, myu->t3->d) map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  assert (myu->t3->c[2] == 2);
  assert (myu->t3->d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t3, myu->t3->c) map(myu->t3->c[2:5])
  {
    myu->t3->c[2]++;
  }

  #pragma omp target map(to: myu, myu->t3, myu->t3->d) map(myu->t3->d[2:5])
  {
    myu->t3->d[2]++;
  }

  assert (myu->t3->c[2] == 4);
  assert (myu->t3->d[2] == 4);

  /* Implicit firstprivate 'myu'.  */
  #pragma omp target map(to: myu->t4, myu->t4->c) map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target map(to: myu->t4, myu->t4->d) map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  assert (myu->t4->c[2] == 2);
  assert (myu->t4->d[2] == 2);

  /* Explicitly-mapped 'myu'.  */
  #pragma omp target map(to: myu, myu->t4, myu->t4->c) map(myu->t4->c[2:5])
  {
    myu->t4->c[2]++;
  }

  #pragma omp target map(to: myu, myu->t4, myu->t4->d) map(myu->t4->d[2:5])
  {
    myu->t4->d[2]++;
  }

  assert (myu->t4->c[2] == 4);
  assert (myu->t4->d[2] == 4);

  delete s4;
  delete t4;
  delete myu_ptr;
}
#endif

int main (int argc, char *argv[])
{
#ifdef MAP_DECLS
  map_decls ();
#endif

#ifdef NONREF_DECL_BASE
  nonref_decl_base ();
#endif
#ifdef REF_DECL_BASE
  ref_decl_base ();
#endif
#ifdef PTR_DECL_BASE
  ptr_decl_base ();
#endif
#ifdef REF2PTR_DECL_BASE
  ref2ptr_decl_base ();
#endif

#ifdef ARRAY_DECL_BASE
  array_decl_base ();
#endif
#ifdef REF2ARRAY_DECL_BASE
  ref2array_decl_base ();
#endif
#ifdef PTR_OFFSET_DECL_BASE
  ptr_offset_decl_base ();
#endif
#ifdef REF2PTR_OFFSET_DECL_BASE
  ref2ptr_offset_decl_base ();
#endif

#ifdef MAP_SECTIONS
  map_sections ();
#endif

#ifdef NONREF_DECL_MEMBER_SLICE
  nonref_decl_member_slice ();
#endif
#ifdef NONREF_DECL_MEMBER_SLICE_BASEPTR
  nonref_decl_member_slice_baseptr ();
#endif
#ifdef REF_DECL_MEMBER_SLICE
  ref_decl_member_slice ();
#endif
#ifdef REF_DECL_MEMBER_SLICE_BASEPTR
  ref_decl_member_slice_baseptr ();
#endif
#ifdef PTR_DECL_MEMBER_SLICE
  ptr_decl_member_slice ();
#endif
#ifdef PTR_DECL_MEMBER_SLICE_BASEPTR
  ptr_decl_member_slice_baseptr ();
#endif
#ifdef REF2PTR_DECL_MEMBER_SLICE
  ref2ptr_decl_member_slice ();
#endif
#ifdef REF2PTR_DECL_MEMBER_SLICE_BASEPTR
  ref2ptr_decl_member_slice_baseptr ();
#endif

#ifdef ARRAY_DECL_MEMBER_SLICE
  array_decl_member_slice ();
#endif
#ifdef ARRAY_DECL_MEMBER_SLICE_BASEPTR
  array_decl_member_slice_baseptr ();
#endif
#ifdef REF2ARRAY_DECL_MEMBER_SLICE
  ref2array_decl_member_slice ();
#endif
#ifdef REF2ARRAY_DECL_MEMBER_SLICE_BASEPTR
  ref2array_decl_member_slice_baseptr ();
#endif
#ifdef PTR_OFFSET_DECL_MEMBER_SLICE
  ptr_offset_decl_member_slice ();
#endif
#ifdef PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
  ptr_offset_decl_member_slice_baseptr ();
#endif
#ifdef REF2PTR_OFFSET_DECL_MEMBER_SLICE
  ref2ptr_offset_decl_member_slice ();
#endif
#ifdef REF2PTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
  ref2ptr_offset_decl_member_slice_baseptr ();
#endif

#ifdef PTRARRAY_DECL_MEMBER_SLICE
  ptrarray_decl_member_slice ();
#endif
#ifdef PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
  ptrarray_decl_member_slice_baseptr ();
#endif
#ifdef REF2PTRARRAY_DECL_MEMBER_SLICE
  ref2ptrarray_decl_member_slice ();
#endif
#ifdef REF2PTRARRAY_DECL_MEMBER_SLICE_BASEPTR
  ref2ptrarray_decl_member_slice_baseptr ();
#endif
#ifdef PTRPTR_OFFSET_DECL_MEMBER_SLICE
  ptrptr_offset_decl_member_slice ();
#endif
#ifdef PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
  ptrptr_offset_decl_member_slice_baseptr ();
#endif
#ifdef REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE
  ref2ptrptr_offset_decl_member_slice ();
#endif
#ifdef REF2PTRPTR_OFFSET_DECL_MEMBER_SLICE_BASEPTR
  ref2ptrptr_offset_decl_member_slice_baseptr ();
#endif

#ifdef NONREF_COMPONENT_BASE
  nonref_component_base ();
#endif
#ifdef NONREF_COMPONENT_MEMBER_SLICE
  nonref_component_member_slice ();
#endif
#ifdef NONREF_COMPONENT_MEMBER_SLICE_BASEPTR
  nonref_component_member_slice_baseptr ();
#endif

#ifdef REF_COMPONENT_BASE
  ref_component_base ();
#endif
#ifdef REF_COMPONENT_MEMBER_SLICE
  ref_component_member_slice ();
#endif
#ifdef REF_COMPONENT_MEMBER_SLICE_BASEPTR
  ref_component_member_slice_baseptr ();
#endif

#ifdef PTR_COMPONENT_BASE
  ptr_component_base ();
#endif
#ifdef PTR_COMPONENT_MEMBER_SLICE
  ptr_component_member_slice ();
#endif
#ifdef PTR_COMPONENT_MEMBER_SLICE_BASEPTR
  ptr_component_member_slice_baseptr ();
#endif

#ifdef REF2PTR_COMPONENT_BASE
  ref2ptr_component_base ();
#endif
#ifdef REF2PTR_COMPONENT_MEMBER_SLICE
  ref2ptr_component_member_slice ();
#endif
#ifdef REF2PTR_COMPONENT_MEMBER_SLICE_BASEPTR
  ref2ptr_component_member_slice_baseptr ();
#endif

  return 0;
}
