// { dg-do compile }
// { dg-require-normal-namespace "" }

#include <iosfwd>

namespace std
{
  // [iosfwd.syn]

  template<class charT> struct char_traits;
  template<> struct char_traits<char>;
#if __cplusplus >= 202002L
  template<> struct char_traits<char8_t>;
#endif
#if __cplusplus >= 201103L
  template<> struct char_traits<char16_t>;
  template<> struct char_traits<char32_t>;
#endif
  template<> struct char_traits<wchar_t>;

  template<class T> class allocator;

  template<class charT, class traits>
    class basic_ios;
  template<class charT, class traits>
    class basic_streambuf;
  template<class charT, class traits>
    class basic_istream;
  template<class charT, class traits>
    class basic_ostream;
  template<class charT, class traits>
    class basic_iostream;

_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template<class charT, class traits, class Allocator>
    class basic_stringbuf;
  template<class charT, class traits, class Allocator>
    class basic_istringstream;
  template<class charT, class traits, class Allocator>
    class basic_ostringstream;
  template<class charT, class traits, class Allocator>
    class basic_stringstream;
_GLIBCXX_END_NAMESPACE_CXX11

  template<class charT, class traits>
    class basic_filebuf;
  template<class charT, class traits>
    class basic_ifstream;
  template<class charT, class traits>
    class basic_ofstream;
  template<class charT, class traits>
    class basic_fstream;

#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
  template<class charT, class traits, class Allocator>
    class basic_syncbuf;
  template<class charT, class traits, class Allocator>
    class basic_osyncstream;
#endif

  template<class charT, class traits>
    class istreambuf_iterator;
  template<class charT, class traits>
    class ostreambuf_iterator;

  typedef basic_ios<char>    ios;
  typedef basic_ios<wchar_t> wios;

  typedef basic_streambuf<char> streambuf;
  typedef basic_istream<char>   istream;
  typedef basic_ostream<char>   ostream;
  typedef basic_iostream<char>  iostream;

  typedef basic_stringbuf<char>     stringbuf;
  typedef basic_istringstream<char> istringstream;
  typedef basic_ostringstream<char> ostringstream;
  typedef basic_stringstream<char>  stringstream;

#if __cplusplus > 202002L
  typedef basic_spanbuf<char>     spanbuf;
  typedef basic_ispanstream<char> ispanstream;
  typedef basic_ospanstream<char> ospanstream;
  typedef basic_spanstream<char>  spanstream;
#endif

  typedef basic_filebuf<char>  filebuf;
  typedef basic_ifstream<char> ifstream;
  typedef basic_ofstream<char> ofstream;
  typedef basic_fstream<char>  fstream;

#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
  typedef basic_syncbuf<char>     syncbuf;
  typedef basic_osyncstream<char> osyncstream;
#endif

  typedef basic_streambuf<wchar_t> wstreambuf;
  typedef basic_istream<wchar_t>   wistream;
  typedef basic_ostream<wchar_t>   wostream;
  typedef basic_iostream<wchar_t>  wiostream;

  typedef basic_stringbuf<wchar_t>     wstringbuf;
  typedef basic_istringstream<wchar_t> wistringstream;
  typedef basic_ostringstream<wchar_t> wostringstream;
  typedef basic_stringstream<wchar_t>  wstringstream;

#if __cplusplus > 202002L
  typedef basic_spanbuf<wchar_t>     wspanbuf;
  typedef basic_ispanstream<wchar_t> wispanstream;
  typedef basic_ospanstream<wchar_t> wospanstream;
  typedef basic_spanstream<wchar_t>  wspanstream;
#endif

  typedef basic_filebuf<wchar_t>  wfilebuf;
  typedef basic_ifstream<wchar_t> wifstream;
  typedef basic_ofstream<wchar_t> wofstream;
  typedef basic_fstream<wchar_t>  wfstream;

#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
  typedef basic_syncbuf<wchar_t>     wsyncbuf;
  typedef basic_osyncstream<wchar_t> wosyncstream;
#endif

  template<class state> class fpos;
  typedef fpos<std::mbstate_t> streampos;
  typedef fpos<std::mbstate_t> wstreampos;
#if __cplusplus >= 202002L
  typedef fpos<std::mbstate_t> u8streampos;
#endif
#if __cplusplus >= 201103L
  typedef fpos<std::mbstate_t> u16streampos;
  typedef fpos<std::mbstate_t> u32streampos;
#endif
}
