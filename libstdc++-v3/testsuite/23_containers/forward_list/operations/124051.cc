// { dg-do run { target c++11 } }
// { dg-require-effective-target exceptions_enabled }

#include <forward_list>
#include <testsuite_hooks.h>

// PR libstdc++/124051 - forward_list::sort is not exception-safe

struct exception { };

struct throwing_less
{
  unsigned* countdown;

  bool operator()(int lhs, int rhs) const
  {
    if (--*countdown == 0)
      throw exception();
    return lhs < rhs;
  }
};

typedef std::forward_list<int> list_type;

void verify_list(const list_type& list, const int* const* addresses,
		 const list_type::iterator* iterators, unsigned size)
{
  for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *iterators[i] == static_cast<int>(i) );
      VERIFY( &*iterators[i] == addresses[i] );
    }

  unsigned seen = 0;
  unsigned count = 0;
  for (const int& value : list)
    {
      VERIFY( count < size );
      VERIFY( value >= 0 && value < static_cast<int>(size) );
      const unsigned bit = 1u << value;
      VERIFY( (seen & bit) == 0 );
      VERIFY( &value == addresses[value] );
      seen |= bit;
      ++count;
    }
  VERIFY( count == size );
  VERIFY( seen == (1u << size) - 1 );
}

void test01()
{
  const int values[] = { 6, 2, 8, 4, 11, 1, 12, 7, 3, 9, 5, 0, 10 };
  const unsigned size = sizeof(values) / sizeof(values[0]);

  for (unsigned throw_after = 1; ; ++throw_after)
    {
      list_type list(values, values + size);
      const int* addresses[size];
      list_type::iterator iterators[size];
      for (list_type::iterator i = list.begin(); i != list.end(); ++i)
	{
	  addresses[*i] = &*i;
	  iterators[*i] = i;
	}

      unsigned countdown = throw_after;
      bool caught = false;
      try
	{
	  list.sort(throwing_less{&countdown});
	}
      catch (const exception&)
	{
	  caught = true;
	}

      verify_list(list, addresses, iterators, size);
      if (caught)
	list.sort();

      int expected = 0;
      for (int value : list)
	VERIFY( value == expected++ );

      if (!caught)
	break;
      VERIFY( throw_after < 100 );
    }
}

int main()
{
  test01();
  return 0;
}
