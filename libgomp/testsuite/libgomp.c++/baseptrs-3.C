#include <cstdlib>
#include <cstring>
#include <cassert>

struct sa0
{
  int *ptr;
};

struct sb0
{
  int arr[10];
};

struct sc0
{
  sa0 a;
  sb0 b;
  sc0 (sa0 &my_a, sb0 &my_b) : a(my_a), b(my_b) {}
};

void
foo0 ()
{
  sa0 my_a;
  sb0 my_b;

  my_a.ptr = (int *) malloc (sizeof (int) * 10);
  sc0 my_c(my_a, my_b);

  memset (my_c.a.ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c.a.ptr, my_c.a.ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c.a.ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c.a.ptr[i] == i);

  memset (my_c.b.arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c.b.arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c.b.arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c.b.arr[i] == i);

  free (my_a.ptr);
}

struct sa
{
  int *ptr;
};

struct sb
{
  int arr[10];
};

struct sc
{
  sa &a;
  sb &b;
  sc (sa &my_a, sb &my_b) : a(my_a), b(my_b) {}
};

void
foo ()
{
  sa my_a;
  sb my_b;

  my_a.ptr = (int *) malloc (sizeof (int) * 10);
  sc my_c(my_a, my_b);

  memset (my_c.a.ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c.a.ptr, my_c.a.ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c.a.ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c.a.ptr[i] == i);

  memset (my_c.b.arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c.b.arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c.b.arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c.b.arr[i] == i);

  free (my_a.ptr);
}

void
bar ()
{
  sa my_a;
  sb my_b;

  my_a.ptr = (int *) malloc (sizeof (int) * 10);
  sc my_c(my_a, my_b);
  sc &my_cref = my_c;

  memset (my_cref.a.ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_cref.a.ptr, my_cref.a.ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_cref.a.ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_cref.a.ptr[i] == i);

  memset (my_cref.b.arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_cref.b.arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_cref.b.arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_cref.b.arr[i] == i);

  free (my_a.ptr);
}

struct scp0
{
  sa *a;
  sb *b;
  scp0 (sa *my_a, sb *my_b) : a(my_a), b(my_b) {}
};

void
foop0 ()
{
  sa *my_a = new sa;
  sb *my_b = new sb;

  my_a->ptr = new int[10];
  scp0 *my_c = new scp0(my_a, my_b);

  memset (my_c->a->ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c->a, my_c->a[:1], my_c->a->ptr, my_c->a->ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c->a->ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c->a->ptr[i] == i);

  memset (my_c->b->arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c->b, my_c->b[:1], my_c->b->arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c->b->arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c->b->arr[i] == i);

  delete[] my_a->ptr;
  delete my_a;
  delete my_b;
}

struct scp
{
  sa *&a;
  sb *&b;
  scp (sa *&my_a, sb *&my_b) : a(my_a), b(my_b) {}
};

void
foop ()
{
  sa *my_a = new sa;
  sb *my_b = new sb;

  my_a->ptr = new int[10];
  scp *my_c = new scp(my_a, my_b);

  memset (my_c->a->ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c->a, my_c->a[:1], my_c->a->ptr, my_c->a->ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c->a->ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c->a->ptr[i] == i);

  memset (my_c->b->arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_c->b, my_c->b[:1], my_c->b->arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_c->b->arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_c->b->arr[i] == i);

  delete[] my_a->ptr;
  delete my_a;
  delete my_b;
}

void
barp ()
{
  sa *my_a = new sa;
  sb *my_b = new sb;

  my_a->ptr = new int[10];
  scp *my_c = new scp(my_a, my_b);
  scp *&my_cref = my_c;

  memset (my_cref->a->ptr, 0, sizeof (int) * 10);

  #pragma omp target map (my_cref->a, my_cref->a[:1], my_cref->a->ptr, \
			  my_cref->a->ptr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_cref->a->ptr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_cref->a->ptr[i] == i);

  memset (my_cref->b->arr, 0, sizeof (int) * 10);

  #pragma omp target map (my_cref->b, my_cref->b[:1], my_cref->b->arr[:10])
  {
    for (int i = 0; i < 10; i++)
      my_cref->b->arr[i] = i;
  }

  for (int i = 0; i < 10; i++)
    assert (my_cref->b->arr[i] == i);

  delete my_a->ptr;
  delete my_a;
  delete my_b;
}

int main (int argc, char *argv[])
{
  foo0 ();
  foo ();
  bar ();
  foop0 ();
  foop ();
  barp ();
  return 0;
}
