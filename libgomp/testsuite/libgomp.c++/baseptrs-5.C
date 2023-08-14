// { dg-do run }

#include <cstring>
#include <cassert>

struct sa
{
  int *ptr;
  int *ptr2;
};

struct sb
{
  int arr[10];
};

struct scp
{
  sa *&a;
  sb *&b;
  scp (sa *&my_a, sb *&my_b) : a(my_a), b(my_b) {}
};

int
main ()
{
  sa *my_a = new sa;
  sb *my_b = new sb;

  my_a->ptr = new int[10];
  my_a->ptr2 = new int[10];
  scp *my_c = new scp(my_a, my_b);

  memset (my_c->a->ptr, 0, sizeof (int) * 10);
  memset (my_c->a->ptr2, 0, sizeof (int) * 10);

  #pragma omp target map (my_c->a, \
			  my_c->a->ptr, my_c->a->ptr[:10], \
			  my_c->a->ptr2, my_c->a->ptr2[:10])
  {
    for (int i = 0; i < 10; i++)
      {
	my_c->a->ptr[i] = i;
	my_c->a->ptr2[i] = i * 2;
      }
  }

  for (int i = 0; i < 10; i++)
    {
      assert (my_c->a->ptr[i] == i);
      assert (my_c->a->ptr2[i] == i * 2);
    }

  delete[] my_a->ptr;
  delete[] my_a->ptr2;
  delete my_a;
  delete my_b;
  delete my_c;

  return 0;
}

