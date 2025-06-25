// C++26 [ostringstream.general]

// { dg-do run { target c++26 } }

#include <sstream>
#include <string>
#include <string_view>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// Check C++26 P2495 ostringstream ctors and members str(s) that accept a
// string_view, or anything convertible to a string_view, in place of a
// string object. Mostly just verify plumbing.

#ifndef C
# define C char
# define L(a) a
#endif

using string = std::basic_string<C>;
using string_view = std::basic_string_view<C>;
using ostringstream = std::basic_ostringstream<C>;

struct convertible_to_string_view {
  string s;
  operator string_view() const { return s; }
};

const string str(L("This is a test string"));
convertible_to_string_view cstr{str};  // a copy
const convertible_to_string_view ccstr{str};  // another copy

template <typename ostringstream = std::basic_ostringstream<C>>
void
test01()
{
  // Test C++26 constructor and str(s) taking a generalized string_view

  static_assert(! requires { ostringstream(1); },
      "ostringstream ctor should reject what cannot be converted to a string_view");
  static_assert(! requires { ostringstream().str(1); },
      "ostringstream::str(s) should reject what cannot be converted to a string_view");

  static_assert(!std::is_convertible_v<string_view, ostringstream>,
      "ostringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const string_view, ostringstream>,
      "ostringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<convertible_to_string_view, ostringstream>,
      "ostringstream(convertible_to_string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const convertible_to_string_view, ostringstream>,
      "ostringstream(convertible_to_string_view, ios::openmode) is explicit");

  {
    ostringstream ostrstr(cstr);
    VERIFY( ostrstr.str() == cstr.s );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
  }
  {
    ostringstream ostrstr(ccstr);
    VERIFY( ostrstr.str() == ccstr.s );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
  }
  {
    ostringstream ostrstr(cstr, std::ios_base::in);
    VERIFY( ostrstr.str() == cstr.s );
    VERIFY( ostrstr.rdbuf()->sgetc() == cstr.s[0]);
    VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
  }
  {
    ostringstream ostrstr(cstr, std::ios_base::out);
    VERIFY( ostrstr.str() == cstr.s );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
    VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
  }
}

void
test02()
{
  // Test plumbing of C++26 various constructors taking string views

  auto const mode = std::ios_base::in | std::ios_base::out;

  {
    ostringstream::allocator_type a;
    // template <typename T>
    // basic_ostringstream(const T&, ios_base::openmode, const allocator_type&)
    {
      ostringstream ostrstr(cstr, mode, a); // ={} checks for non-explicit ctor
      VERIFY( ostrstr.str() == cstr.s );
    }
    {
      ostringstream ostrstr(cstr, std::ios::in, a);
      VERIFY( ostrstr.str() == cstr.s );
      VERIFY( ostrstr.rdbuf()->sgetc() == cstr.s[0]);
      VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
    }
    {
      ostringstream ostrstr(cstr, std::ios::out, a);
      VERIFY( ostrstr.str() == cstr.s );
      VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
      VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
    }
  }

  {
    // template <typename T>
    // basic_ostringstream(const T&, ios_base::openmode)
    {
      ostringstream ostrstr(cstr, mode);
      VERIFY( ostrstr.str() == cstr.s );
      VERIFY( ostrstr.rdbuf()->sgetc() == cstr.s[0]);
      VERIFY( ostrstr.put('Y').good() );
    }
    {
      ostringstream ostrstr(cstr, std::ios::in);
      VERIFY( ostrstr.str() == cstr.s );
      VERIFY( ostrstr.rdbuf()->sgetc() == cstr.s[0]);
      VERIFY( ostrstr.put('X').good() );
    }
    {
      ostringstream ostrstr(cstr, std::ios::out);
      VERIFY( ostrstr.str() == cstr.s );
      VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
      VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
    }
  }

  {
    // template <typename T>
    // explicit
    // basic_ostringstream(const T&, ios_base::openmode = ios_base::out)

    ostringstream ostrstr(cstr);
    VERIFY( ostrstr.str() == cstr.s );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
    VERIFY( ostrstr.put('Y').good() );
  }
}

using alloc_type = __gnu_test::uneq_allocator<C>;

template<typename Alloc, typename CC = typename Alloc::value_type>
  using ostringstream_with_alloc
    = std::basic_ostringstream<CC, std::char_traits<CC>, Alloc>;

void test03()
{
  alloc_type a{1};
  {
    ostringstream_with_alloc<alloc_type> ostrstr(cstr, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( ostrstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{ostrstr.str()} == cstr );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
    VERIFY( ostrstr.put('X').good() );
  }
  {
    ostringstream_with_alloc<alloc_type> ostrstr(cstr, std::ios::in, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( ostrstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{ostrstr.str()} == cstr );
    VERIFY( ostrstr.rdbuf()->sgetc() == cstr.s[0]);
    VERIFY( ostrstr.put('X').good() );
  }
  {
    ostringstream_with_alloc<alloc_type> ostrstr(cstr, std::ios::out, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( ostrstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{ostrstr.str()} == cstr );
    VERIFY( ostrstr.rdbuf()->sgetc() == ostringstream::traits_type::eof() );
    VERIFY( ostrstr.put('Y').rdstate() == ostrstr.goodbit );
  }
}

void test04()
{
  {
    ostringstream ostrstr;
    ostrstr.str(cstr);
    VERIFY( ostrstr.str() == cstr.s );
  }
  {
    ostringstream ostrstr;
    ostrstr.str(ccstr);
    VERIFY( ostrstr.str() == ccstr.s );
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
