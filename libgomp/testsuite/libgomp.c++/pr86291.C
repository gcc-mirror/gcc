// PR c++/86291
// { dg-do run }
// { dg-additional-options "-std=c++11" }

extern "C" void abort ();

struct I
{
  using size_type = __SIZE_TYPE__;
  using difference_type = __PTRDIFF_TYPE__;
  using value_type = int;
  using reference = int &;
  using pointer = int *;
  static I begin () { return I{}; }
  static I end () { I res; res.pos = res.num; return res; }
  I &operator++ () { ++pos; return *this; }
  reference operator* () const { return val; }
  I &operator+= (size_type diff) { pos += diff; return *this; }
  friend bool operator< (const I &a, const I &b) { return a.pos < b.pos; }
  friend difference_type operator- (const I &a, const I &b) { return a.pos - b.pos; }
  size_type pos = 0;
  size_type num = 1;
  mutable int val = 0;
};

int c;

int
main ()
{
#pragma omp parallel for collapse(10)
  for (auto i = I::begin (); i < I::end (); ++i)
    for (auto j = I::begin (); j < I::end (); ++j)
      for (auto k = I::begin (); k < I::end (); ++k)
	for (auto l = I::begin (); l < I::end (); ++l)
	  for (auto m = I::begin (); m < I::end (); ++m)
	    for (auto n = I::begin (); n < I::end (); ++n)
	      for (auto o = I::begin (); o < I::end (); ++o)
		for (auto p = I::begin (); p < I::end (); ++p)
		  for (auto q = I::begin (); q < I::end (); ++q)
		    for (auto r = I::begin (); r < I::end (); ++r)
		      {
			if (*i != 0 || *j != 0 || *k != 0 || *l != 0 || *m != 0
			    || *n != 0 || *o != 0 || *p != 0 || *q != 0 || *r != 0)
			  abort ();
			#pragma omp atomic
			  c++;
		      }
  if (c != 1)
    abort ();
}
