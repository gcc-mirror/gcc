// C++26 [istringstream.general]

// { dg-do run { target c++26 } }

#include <sstream>
#include <string>
#include <string_view>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// Check C++26 P2495 istringstream ctors and members str(s) that accept a
// string_view, or anything convertible to a string_view, in place of a
// string object. Mostly just verify plumbing.

#ifndef C
# define C char
# define L(a) a
#endif

using string = std::basic_string<C>;
using string_view = std::basic_string_view<C>;
using istringstream = std::basic_istringstream<C>;

struct convertible_to_string_view {
  string s;
  operator string_view() const { return s; }
};

const string str(L("This is a test string"));
convertible_to_string_view cstr{str};  // a copy
const convertible_to_string_view ccstr{str};  // another copy

template <typename istringstream = std::basic_istringstream<C>>
void
test01()
{
  // Test C++26 constructor and str(s) taking a generalized string_view

  static_assert(! requires { istringstream(1); },
      "istringstream ctor should reject what cannot be converted to a string_view");
  static_assert(! requires { istringstream().str(1); },
      "istringstream::str(s) should reject what cannot be converted to a string_view");

  static_assert(!std::is_convertible_v<string_view, istringstream>,
      "istringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const string_view, istringstream>,
      "istringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<convertible_to_string_view, istringstream>,
      "istringstream(convertible_to_string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const convertible_to_string_view, istringstream>,
      "istringstream(convertible_to_string_view, ios::openmode) is explicit");

  {
    istringstream istr(cstr);
    VERIFY( istr.str() == cstr.s );
    VERIFY( istr.get() == cstr.s[0] );
  }
  {
    istringstream istr(ccstr);
    VERIFY( istr.str() == ccstr.s );
    VERIFY( istr.get() == ccstr.s[0] );
  }
  {
    istringstream istr(cstr, std::ios_base::in);
    VERIFY( istr.str() == cstr.s );
    VERIFY( istr.get() == cstr.s[0] );
    VERIFY( istr.rdbuf()->sputc('X') != 'X' );
  }
  {
    istringstream istr(cstr, std::ios_base::out);
    VERIFY( istr.str() == cstr.s );
    VERIFY( istr.get() == cstr.s[0] );
    VERIFY( istr.rdbuf()->sputc('X') == 'X' );
  }
}

void
test02()
{
  // Test various C++26 constructors taking string views
  // and mix of other arguments

  auto const mode = std::ios_base::in | std::ios_base::out;

  {
    // template <typename T>
    // basic_istringstream(const T&, ios_base::openmode, const allocator_type&)

    istringstream::allocator_type a;
    {
      istringstream istr(cstr, mode, a); // ={} checks for non-explicit ctor
      VERIFY( istr.str() == cstr.s );
    }
    {
      istringstream istr(cstr, std::ios::in, a);
      VERIFY( istr.str() == cstr.s );
      VERIFY( istr.get() == cstr.s[0] );
      VERIFY( istr.rdbuf()->sputc('X') != 'X' );
    }
    {
      istringstream istr(cstr, std::ios::out, a);
      VERIFY( istr.str() == cstr.s );
      VERIFY( istr.get() == cstr.s[0] );
      VERIFY( istr.rdbuf()->sputc('X') == 'X' );
    }
  }

  {
    // template <typename T>
    // basic_istringstream(const T&, ios_base::openmode)
    {
      istringstream istr(cstr, mode);
      VERIFY( istr.str() == cstr.s );
      VERIFY( istr.get() == cstr.s[0] );
      VERIFY( istr.rdbuf()->sputc('X') == 'X' );
    }
    {
      istringstream istr(cstr, std::ios::in);
      VERIFY( istr.str() == cstr.s );
      VERIFY( istr.get() == cstr.s[0] );
      VERIFY( istr.rdbuf()->sputc('X') != 'X' );
    }
    {
      istringstream istr(cstr, std::ios::out);
      VERIFY( istr.str() == cstr.s );
      VERIFY( istr.get() == cstr.s[0] );
      VERIFY( istr.rdbuf()->sputc('X') == 'X' );
    }
  }

  {
    // template <typename T>
    // explicit
    // basic_istringstream(const T&, ios_base::openmode = ios_base::in)

    istringstream istr(cstr);
    VERIFY( istr.str() == cstr.s );
    VERIFY( istr.get() == cstr.s[0] );
    VERIFY( istr.rdbuf()->sputc('X') != 'X' );
  }
}

using alloc_type = __gnu_test::uneq_allocator<C>;

template<typename Alloc, typename CC = typename Alloc::value_type>
  using istringstream_with_alloc
    = std::basic_istringstream<CC, std::char_traits<CC>, Alloc>;

void test03()
{
  alloc_type a{1};
  {
    istringstream_with_alloc<alloc_type> istr(cstr, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( istr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{istr.str()} == cstr );
    VERIFY( istr.get() == cstr.s[0] );
  }
  {
    istringstream_with_alloc<alloc_type> istr(cstr, std::ios::in, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( istr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{istr.str()} == cstr );
    VERIFY( istr.get() == cstr.s[0] );
    VERIFY( istr.rdbuf()->sputc('X') != 'X' );
  }
  {
    istringstream_with_alloc<alloc_type> istr(cstr, std::ios::out, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( istr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{istr.str()} == cstr );
    VERIFY( istr.get() == cstr.s[0] );
    VERIFY( istr.rdbuf()->sputc('X') == 'X' );
  }
}

void test04()
{
  {
    istringstream istr;
    istr.str( cstr );
    VERIFY( istr.str() == cstr.s );
  }
  {
    istringstream istr;
    istr.str( ccstr );
    VERIFY( istr.str() == ccstr.s );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
