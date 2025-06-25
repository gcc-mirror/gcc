// C++26 31.8.2.1 [stringstream.general]

// { dg-do run { target c++26 } }

#include <sstream>
#include <string>
#include <string_view>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// Check C++26 P2495 stringstream ctors and members str(s) that accept a
// string_view, or anything convertible to a string_view, in place of a
// string object. Mostly just verify plumbing.

#ifndef C
# define C char
# define L(a) a
#endif

using string = std::basic_string<C>;
using string_view = std::basic_string_view<C>;
using stringstream = std::basic_stringstream<C>;

struct convertible_to_string_view {
  string s;
  operator string_view() const { return s; }
};

const string str(L("This is a test string"));
convertible_to_string_view cstr{str};  // a copy
const convertible_to_string_view ccstr{str};  // another copy

template <typename stringstream = std::basic_stringstream<C>>
void
test01()
{
  // Test C++26 constructor and str(s) taking a generalized string_view

  static_assert(! requires { stringstream(1); },
      "stringstream ctor should reject what cannot be converted to a string_view");
  static_assert(! requires { stringstream().str(1); },
      "stringstream::str(s) should reject what cannot be converted to a string_view");

  static_assert(!std::is_convertible_v<string_view, stringstream>,
      "stringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const string_view, stringstream>,
      "stringstream(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<convertible_to_string_view, stringstream>,
      "stringstream(convertible_to_string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const convertible_to_string_view, stringstream>,
      "stringstream(convertible_to_string_view, ios::openmode) is explicit");

  {
    stringstream strstr(cstr);
    VERIFY( strstr.str() == cstr.s );
    VERIFY( strstr.get() == cstr.s[0] );
  }
  {
    stringstream strstr(ccstr);
    VERIFY( strstr.str() == ccstr.s );
    VERIFY( strstr.get() == ccstr.s[0] );
  }
  {
    stringstream strstr(cstr, std::ios_base::in);
    VERIFY( strstr.str() == cstr.s );
    VERIFY( strstr.get() == cstr.s[0] );
    VERIFY( strstr.put('X').rdstate() == strstr.badbit );
  }
  {
    stringstream strstr(cstr, std::ios_base::out);
    VERIFY( strstr.str() == cstr.s );
    VERIFY( strstr.put('Y').good() );
    VERIFY( strstr.get() == stringstream::traits_type::eof());
  }
}

void
test02()
{
  // Test C++26 various constructors taking string views

  auto const mode = std::ios_base::in | std::ios_base::out;

  {
    // template <typename T>
    // basic_stringstream(const T&, ios_base::openmode, const allocator_type&)

    stringstream::allocator_type a;
    {
      stringstream strstr(cstr, mode, a); // ={} checks for non-explicit ctor
      VERIFY( strstr.str() == cstr.s );
    }
    {
      stringstream strstr(cstr, std::ios::in, a);
      VERIFY( strstr.str() == cstr.s );
      VERIFY( strstr.get() == cstr.s[0] );
      VERIFY( strstr.put('X').rdstate() == strstr.badbit );
    }
    {
      stringstream strstr(cstr, std::ios::out, a);
      VERIFY( strstr.str() == cstr.s );
      VERIFY( strstr.put('X').good() );
      VERIFY( strstr.get() == stringstream::traits_type::eof());
    }
  }

  {
    // template <typename T>
    // basic_stringstream(const T&, ios_base::openmode)

    {
      stringstream strstr(cstr, mode);
      VERIFY( strstr.str() == cstr.s );
      VERIFY( strstr.get() == cstr.s[0] );
      VERIFY( strstr.put('X').good() );
    }
    {
      stringstream strstr(cstr, std::ios::in);
      VERIFY( strstr.str() == cstr.s );
      VERIFY( strstr.get() == cstr.s[0] );
      VERIFY( strstr.put('X').rdstate() == strstr.badbit );
    }
    {
      stringstream strstr(cstr, std::ios::out);
      VERIFY( strstr.str() == cstr.s );
      VERIFY( strstr.put('X').good() );
      VERIFY( strstr.get() == stringstream::traits_type::eof());
    }
  }

  {
    // template <typename T>
    // explicit
    // basic_stringstream(const T&, ios_base::openmode = ios_base::in|ios_base::out)

    stringstream strstr(cstr);
    VERIFY( strstr.str() == cstr.s );
    VERIFY( strstr.get() == cstr.s[0] );
    VERIFY( strstr.put('X').good() );
  }
}

// A minimal allocator with no default constructor
template<typename T>
  struct NoDefaultCons : __gnu_test::SimpleAllocator<T>
  {
    using __gnu_test::SimpleAllocator<T>::SimpleAllocator;
    NoDefaultCons() = delete;
    NoDefaultCons(int) { }
  };

using alloc_type = __gnu_test::uneq_allocator<C>;

template<typename Alloc, typename CC = typename Alloc::value_type>
  using stringstream_with_alloc
    = std::basic_stringstream<CC, std::char_traits<CC>, Alloc>;

void test03()
{
  alloc_type a{1};
  {
    stringstream_with_alloc<alloc_type> strstr(cstr, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( strstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{strstr.str()} == cstr );
    VERIFY( strstr.get() == cstr.s[0] );
  }
  {
    stringstream_with_alloc<alloc_type> strstr(cstr, std::ios::in, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( strstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{strstr.str()} == cstr );
    VERIFY( strstr.get() == cstr.s[0] );
    VERIFY( strstr.put('X').rdstate() == strstr.badbit );
  }
  {
    stringstream_with_alloc<alloc_type> strstr(cstr, std::ios::out, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( strstr.rdbuf()->get_allocator() == a );
#endif
    VERIFY( string_view{strstr.str()} == cstr );
    VERIFY( strstr.put('X').good() );
    VERIFY( strstr.get() == stringstream::traits_type::eof());
  }
}

void test04()
{
  {
    stringstream strstr;
    strstr.str( cstr );
    VERIFY( strstr.str() == cstr.s );
  }
  {
    stringstream strstr;
    strstr.str( ccstr );
    VERIFY( strstr.str() == ccstr.s );
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
