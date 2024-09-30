// { dg-do run }

#include <cassert>

#pragma omp declare target

struct item {
  item(item *p, int v) : prev(p), val(v) { }
  int get() { return prev ? prev->get() * val : val; }
  item *prev;
  int val;
};

/* This case demonstrates why constructing on the host and then copying to
   the target would be less desirable.  With on-target construction, "prev"
   for each 'item' will be a device pointer, not a host pointer.  */
item hubert1(nullptr, 3);
item hubert2(&hubert1, 5);
item hubert3(&hubert2, 7);
item hubert4(&hubert3, 11);

#pragma omp end declare target

int main()
{
  int res = -1;

#pragma omp target map(from:res)
  {
    res = hubert4.get ();
  }

  assert (res == 1155);

  return 0;
}
