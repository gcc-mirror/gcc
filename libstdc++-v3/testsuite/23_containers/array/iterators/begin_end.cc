// { dg-do compile { target c++11 } }

#include <array>

#pragma GCC push_options
#pragma GCC optimize "0"

extern void
sink (const void*, ...);

void
test01()
{
  {
    const std::size_t len = 1;
    typedef std::array<int, len> array_type;
    typedef array_type::iterator iterator;;
    array_type a;

    iterator b = a.begin();           // { dg-bogus "-Wmaybe-uninitialized" }
    iterator e = a.end();             // { dg-bogus "-Wmaybe-uninitialized" }

    sink(&b, &e);
  }

  {
    const std::size_t len = 3;
    typedef std::array<int, len> array_type;
    typedef array_type::reverse_iterator reverse_iterator;
    array_type a;

    reverse_iterator b = a.rbegin();  // { dg-bogus "-Wmaybe-uninitialized" }
    reverse_iterator e = a.rend();    // { dg-bogus "-Wmaybe-uninitialized" }

    sink(&b, &e);
  }
}

#pragma GCC pop_options
