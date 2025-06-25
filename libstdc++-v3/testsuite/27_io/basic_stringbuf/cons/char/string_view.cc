// C++26 31.8.2.1 [stringbuf.general]

// { dg-do run { target c++26 } }

#include <sstream>
#include <string>
#include <string_view>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// Check C++26 P2495 stringbuf ctors and members str(s) that accept a
// string_view, or anything convertible to a string_view, in place of a
// string object.

#ifndef C
# define C char
# define L(a) a
#endif

using string = std::basic_string<C>;
using string_view = std::basic_string_view<C>;
using stringbuf = std::basic_stringbuf<C>;

struct convertible_to_string_view {
  string s;
  operator string_view() const { return s; }
};

const string str(L("This is a test string"));
convertible_to_string_view cstr{str};  // a copy
const convertible_to_string_view ccstr{str};  // another copy

template <typename stringbuf = std::basic_stringbuf<C>>
void
test01()
{
  // Test C++26 constructor and str(s) taking a generalized string_view

  static_assert(! requires { stringbuf(1); },
      "stringbuf ctor should reject what cannot be converted to a string_view");
  static_assert(! requires { stringbuf().str(1); },
      "stringbuf::str(s) should reject what cannot be converted to a string_view");

  static_assert(!std::is_convertible_v<string_view, stringbuf>,
      "stringbuf(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<const string_view, stringbuf>,
      "stringbuf(string_view, ios::openmode) is explicit");
  static_assert(!std::is_convertible_v<convertible_to_string_view, stringbuf>,
      "stringbuf(convertible_to_string_view, ios::openmode) is explicit");
  static_assert(
      !std::is_convertible_v<const convertible_to_string_view, stringbuf>,
      "stringbuf(convertible_to_string_view, ios::openmode) is explicit");

  {
    stringbuf sbuf(cstr);
    VERIFY( sbuf.str() == cstr.s );
    VERIFY( sbuf.sgetc() == cstr.s[0] );
  }
  {
    stringbuf sbuf(ccstr);
    VERIFY( sbuf.str() == ccstr.s );
    VERIFY( sbuf.sgetc() == ccstr.s[0] );
  }
  {
    stringbuf sbuf(cstr, std::ios_base::in);
    VERIFY( sbuf.str() == cstr.s );
    VERIFY( sbuf.sgetc() == cstr.s[0] );
    VERIFY( sbuf.sputc('X') == stringbuf::traits_type::eof() );
  }
  {
    stringbuf sbuf(ccstr, std::ios_base::in);
    VERIFY( sbuf.str() == ccstr.s );
    VERIFY( sbuf.sgetc() == ccstr.s[0] );
    VERIFY( sbuf.sputc('X') == stringbuf::traits_type::eof() );
  }
  {
    stringbuf sbuf(cstr, std::ios_base::out);
    VERIFY( sbuf.str() == cstr.s );
    VERIFY( sbuf.sputc('Y') == 'Y' );
    VERIFY( sbuf.sgetc() == stringbuf::traits_type::eof() );
  }
  {
    stringbuf sbuf(ccstr, std::ios_base::out);
    VERIFY( sbuf.str() == ccstr.s );
    VERIFY( sbuf.sputc('Y') == 'Y' );
    VERIFY( sbuf.sgetc() == stringbuf::traits_type::eof() );
  }
}

void
test02()
{
  // Test C++26 constructors taking string views using different allocators

  auto const mode = std::ios_base::in | std::ios_base::out;

  {
    // template <typename T>
    // basic_stringbuf(const T&, ios_base::openmode, const allocator_type&)

    stringbuf::allocator_type a;
    {
      stringbuf sbuf(cstr, mode, a); // ={} checks for non-explicit ctor
      VERIFY( sbuf.str() == cstr.s );
    }
    {
      stringbuf sbuf(cstr, std::ios::in, a);
      VERIFY( sbuf.str() == cstr.s );
      VERIFY( sbuf.sgetc() == cstr.s[0] );
      VERIFY( sbuf.sputc('X') == stringbuf::traits_type::eof() );
    }

    {
      stringbuf sbuf(cstr, std::ios::out, a);
      VERIFY( sbuf.str() == cstr.s );
      VERIFY( sbuf.sputc('X') == 'X' );
      VERIFY( sbuf.sgetc() == stringbuf::traits_type::eof() );
    }
  }

  {
    // template <typename T>
    // basic_stringbuf(const T&, ios_base::openmode)
    {
      stringbuf sbuf(cstr, mode);
      VERIFY( sbuf.str() == cstr.s );
    }
    {
      stringbuf sbuf(cstr, std::ios::in);
      VERIFY( sbuf.str() == cstr.s );
      VERIFY( sbuf.sgetc() == cstr.s[0] );
      VERIFY( sbuf.sputc('X') == stringbuf::traits_type::eof() );
    }
    {
      stringbuf sbuf(cstr, std::ios::out);
      VERIFY( sbuf.str() == cstr.s );
      VERIFY( sbuf.sputc('X') == 'X' );
      VERIFY( sbuf.sgetc() == stringbuf::traits_type::eof() );
    }
  }

  {
    // template <typename T>
    // explicit
    // basic_stringbuf(const T&, ios_base::openmode = ios_base::in|ios_base::out)

    stringbuf sbuf(cstr);
    VERIFY( sbuf.str() == cstr.s );
    VERIFY( sbuf.sgetc() == cstr.s[0] );
  }
}

using alloc_type = __gnu_test::uneq_allocator<C>;

template<typename Alloc, typename CC = typename Alloc::value_type>
  using stringbuf_with_alloc
    = std::basic_stringbuf<CC, std::char_traits<CC>, Alloc>;

void test03()
{
  alloc_type a{1};
  {
    stringbuf_with_alloc<alloc_type> sbuf(cstr, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( sbuf.get_allocator() == a );
#endif
    VERIFY( string_view{sbuf.str()} == cstr );
    VERIFY( sbuf.sgetc() == cstr.s[0] );
  }
  {
    stringbuf_with_alloc<alloc_type> sbuf(cstr, std::ios::in, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( sbuf.get_allocator() == a );
#endif
    VERIFY( string_view{sbuf.str()} == cstr );
    VERIFY( sbuf.sgetc() == cstr.s[0] );
    VERIFY( sbuf.sputc('X') == stringbuf::traits_type::eof() );
  }
  {
    stringbuf_with_alloc<alloc_type> sbuf(cstr, std::ios::out, a);
#if _GLIBCXX_USE_CXX11_ABI
    VERIFY( sbuf.get_allocator() == a );
#endif
    VERIFY( string_view{sbuf.str()} == cstr );
    VERIFY( sbuf.sputc('X') == 'X' );
    VERIFY( sbuf.sgetc() == stringbuf::traits_type::eof() );
  }
}

void test04()
{
  {
    stringbuf sbuf;
    sbuf.str(cstr);
    VERIFY( sbuf.str() == cstr.s );
  }
  {
    stringbuf sbuf;
    sbuf.str(ccstr);
    VERIFY( sbuf.str() == ccstr.s );
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
