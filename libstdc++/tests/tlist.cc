/*
 test/demo of generic lists
*/

#include <assert.h>

#define tassert(ex) {if ((ex)) cerr << #ex << "\n"; \
                       else _assert(#ex, __FILE__,__LINE__); }

#include <iostream.h>
#include "list.h"
#include "algo.h"

bool int_compare(int a, int b)
{
  return a < b;
}

int inc(int x)
{
  return x + 1;
}

void print(list<int>& l)
{
  for (list<int>::iterator it = l.begin(); it != l.end(); it++)
    cout << *it << " ";
  cout << "\n";
}

int is_odd(int x)
{
  return x & 1;
}

int is_even(int x)
{
  return (x & 1) == 0;
}

void sequence(list<int>& a, int lo, int hi)
{
  back_insert_iterator<list<int> > it(a);
  while (lo <= hi)
    *it++ = lo++;
}

int old_rand = 9999;

int get_rand()
{
    old_rand = ((long)old_rand * (long)1243) % (long)971;
    return old_rand;
}

void randseq(list<int>& a, int n)
{
  back_insert_iterator<list<int> > it(a);
  while (--n >= 0)
    *it++ = get_rand() % 50;
}

int array1 [] = { 9, 16, 36 };
int array2 [] = { 1, 4 };

int test_splice ()
{
  list<int> l1 (array1, array1 + 3);
  list<int> l2 (array2, array2 + 2);
  list<int>::iterator i1 = l1.begin ();
  l1.splice (i1, l2);
  list<int>::iterator i2 = l1.begin ();
  while (i2 != l1.end ())
    cout << *i2++ << endl;
  return 0;
}

main()
{
  list<int> a;  int i;
  list<int>::iterator it, bit;
  sequence(a, 1, 20);
  cout << "\nlist<int> a = sequence(1, 20);\n"; print(a);
  for (it = a.begin (), i = 0; it != a.end (); it++, i++)
    assert (*it == i + 1);
  list<int> b;
  randseq(b, 20);
  cout << "\nlist<int> b = randseq(20);\n"; print(b);
  list<int> c;
  c.insert (c.end(), a.begin(), a.end());
  c.insert (c.end(), b.begin(), b.end());
  cout << "\nlist<int> c = a and b;\n"; print(c);

  list<int> d;
  for (it = a.begin(); it != a.end(); it++)
    d.insert(d.end (), inc(*it));
  cout << "\nlist<int> d = map(inc, a);\n"; print(d);

  list<int> e;
  back_insert_iterator<list<int> > e_insertor (e);
  reverse_copy (a.begin(), a.end (), e_insertor);
  cout << "\nlist<int> e = reverse(a);\n"; print(e);

  list<int> f;
  for (it = a.begin(); it != a.end(); it++)
    if (is_odd (*it))
      f.insert(f.end (), *it);
  cout << "\nlist<int> f = select(is_odd, a);\n"; print(f);
  list<int> ff;
  for (it = f.begin(); it != f.end(); it++)
    if (is_even (*it))
      ff.insert(ff.end (), *it);
  assert(ff.empty());

  int red = 0;
  for (it = a.begin(); it != a.end(); it++)
    red += *it;
  cout << "\nint  red = a.reduce(plus, 0);\n"; cout << red;
  it = a.begin(); ++it; ++it;
  int second = *it;
  cout << "\nint second = a[2];\n"; cout << second;
  list<int> g;
  for (it = a.begin(), bit = b.begin(); it != a.end () && bit != b.end (); )
    g.insert (g.end (), *it++ + *bit++);
  cout << "\nlist<int> g = combine(plus, a, b);\n"; print(g);
#if 1
  for (it = g.begin(); it != g.end(); )
    {
      bit = it++;
      if (is_odd (*bit))
	g.erase (bit);
    }
#else
  g.remove_if (is_odd);
#endif
  cout << "\ng.del(is_odd);\n"; print(g);

  ff.erase (ff.begin (), ff.end());
  for (it = g.begin(); it != g.end(); it++)
    if (is_odd (*it))
      ff.insert (ff.end (), *it);
  assert(ff.empty());

  b.sort();
  for (it = b.begin(); bit = it++, it != b.end (); ) assert (*it >= *bit);
  cout << "\nb.sort(int_compare);\n"; print(b);

  list<int> h;
  back_insert_iterator<list<int> > h_insertor (h);
  merge (a.begin (), a.end (), b.begin (), b.end (), h_insertor, int_compare);
  cout << "\nlist<int> h = merge(a, b, int_compare);\n"; print(h);
  for (it = h.begin(); bit = it++, it != h.end (); ) assert (*it >= *bit);

  cout << "\nh via iterator:\n";
  for (it = h.begin(); it != h.end (); it++)
    cout << *it << ", ";
  cout << "\n";

  test_splice ();

  cout << "\ndone\n";
}
