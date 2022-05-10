// { dg-do compile { target c++11 } }

#include <iosfwd>

template<class, class> struct Same { static constexpr bool value = false; };
template<class T> struct Same<T, T> { static constexpr bool value = true; };

template<class T> using Traits = std::char_traits<T>;
template<class T> using Alloc = std::allocator<T>;

using std::basic_ios;

static_assert(Same<basic_ios<char>,
		   basic_ios<char, Traits<char>>
		   >::value,
    "std::basic_ios<char> has the correct default template argument");

static_assert(Same<basic_ios<wchar_t>,
		   basic_ios<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ios<wchar_t> has the correct default template argument");

using std::basic_streambuf;

static_assert(Same<basic_streambuf<char>,
		   basic_streambuf<char, Traits<char>>
		   >::value,
    "std::basic_streambuf<char> has the correct default template argument");

static_assert(Same<basic_streambuf<wchar_t>,
		   basic_streambuf<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_streambuf<wchar_t> has the correct default template argument");

using std::basic_istream;

static_assert(Same<basic_istream<char>,
		   basic_istream<char, Traits<char>>
		   >::value,
    "std::basic_istream<char> has the correct default template argument");

static_assert(Same<basic_istream<wchar_t>,
		   basic_istream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_istream<wchar_t> has the correct default template argument");

using std::basic_ostream;

static_assert(Same<basic_ostream<char>,
		   basic_ostream<char, Traits<char>>
		   >::value,
    "std::basic_ostream<char> has the correct default template argument");

static_assert(Same<basic_ostream<wchar_t>,
		   basic_ostream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ostream<wchar_t> has the correct default template argument");

using std::basic_iostream;

static_assert(Same<basic_iostream<char>,
		   basic_iostream<char, Traits<char>>>::value,
    "std::basic_iostream<char> has the correct default template argument");

static_assert(Same<basic_iostream<wchar_t>,
		   basic_iostream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_iostream<wchar_t> has the correct default template argument");

using std::basic_stringbuf;

static_assert(Same<basic_stringbuf<char>,
		   basic_stringbuf<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_stringbuf<char> has the correct default template argument");

static_assert(Same<basic_stringbuf<wchar_t>,
		   basic_stringbuf<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_stringbuf<wchar_t> has the correct default template argument");

using std::basic_istringstream;

static_assert(Same<basic_istringstream<char>,
		   basic_istringstream<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_istringstream<char> has the correct default template argument");

static_assert(Same<basic_istringstream<wchar_t>,
		   basic_istringstream<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_istringstream<wchar_t> has the correct default template argument");

using std::basic_ostringstream;

static_assert(Same<basic_ostringstream<char>,
		   basic_ostringstream<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_ostringstream<char> has the correct default template argument");

static_assert(Same<basic_ostringstream<wchar_t>,
		   basic_ostringstream<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_ostringstream<wchar_t> has the correct default template argument");

using std::basic_stringstream;

static_assert(Same<basic_stringstream<char>,
		   basic_stringstream<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_stringstream<char> has the correct default template argument");

static_assert(Same<basic_stringstream<wchar_t>,
		   basic_stringstream<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_stringstream<wchar_t> has the correct default template argument");

#if __cplusplus > 202002L
using std::basic_spanbuf;

static_assert(Same<basic_spanbuf<char>,
		   basic_spanbuf<char, Traits<char>>
		   >::value,
    "std::basic_spanbuf<char> has the correct default template argument");

static_assert(Same<basic_spanbuf<wchar_t>,
		   basic_spanbuf<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_spanbuf<wchar_t> has the correct default template argument");

using std::basic_ispanstream;

static_assert(Same<basic_ispanstream<char>,
		   basic_ispanstream<char, Traits<char>>
		   >::value,
    "std::basic_ispanstream<char> has the correct default template argument");

static_assert(Same<basic_ispanstream<wchar_t>,
		   basic_ispanstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ispanstream<wchar_t> has the correct default template argument");

using std::basic_ospanstream;

static_assert(Same<basic_ospanstream<char>,
		   basic_ospanstream<char, Traits<char>>
		   >::value,
    "std::basic_ospanstream<char> has the correct default template argument");

static_assert(Same<basic_ospanstream<wchar_t>,
		   basic_ospanstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ospanstream<wchar_t> has the correct default template argument");

using std::basic_spanstream;

static_assert(Same<basic_spanstream<char>,
		   basic_spanstream<char, Traits<char>>
		   >::value,
    "std::basic_spanstream<char> has the correct default template argument");

static_assert(Same<basic_spanstream<wchar_t>,
		   basic_spanstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_spanstream<wchar_t> has the correct default template argument");
#endif

using std::basic_filebuf;

static_assert(Same<basic_filebuf<char>,
		   basic_filebuf<char, Traits<char>>
		   >::value,
    "std::basic_filebuf<char> has the correct default template argument");

static_assert(Same<basic_filebuf<wchar_t>,
		   basic_filebuf<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_filebuf<wchar_t> has the correct default template argument");

using std::basic_ifstream;

static_assert(Same<basic_ifstream<char>,
		   basic_ifstream<char, Traits<char>>
		   >::value,
    "std::basic_ifstream<char> has the correct default template argument");

static_assert(Same<basic_ifstream<wchar_t>,
		   basic_ifstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ifstream<wchar_t> has the correct default template argument");

using std::basic_ofstream;

static_assert(Same<basic_ofstream<char>,
		   basic_ofstream<char, Traits<char>>
		   >::value,
    "std::basic_ofstream<char> has the correct default template argument");

static_assert(Same<basic_ofstream<wchar_t>,
		   basic_ofstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_ofstream<wchar_t> has the correct default template argument");

using std::basic_fstream;

static_assert(Same<basic_fstream<char>,
		   basic_fstream<char, Traits<char>>
		   >::value,
    "std::basic_fstream<char> has the correct default template argument");

static_assert(Same<basic_fstream<wchar_t>,
		   basic_fstream<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::basic_fstream<wchar_t> has the correct default template argument");

#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
using std::basic_syncbuf;

static_assert(Same<basic_syncbuf<char>,
		   basic_syncbuf<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_syncbuf<char> has the correct default template argument");

static_assert(Same<basic_syncbuf<wchar_t>,
		   basic_syncbuf<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_syncbuf<wchar_t> has the correct default template argument");

using std::basic_osyncstream;

static_assert(Same<basic_osyncstream<char>,
		   basic_osyncstream<char, Traits<char>, Alloc<char>>
		   >::value,
    "std::basic_osyncstream<char> has the correct default template argument");

static_assert(Same<basic_osyncstream<wchar_t>,
		   basic_osyncstream<wchar_t, Traits<wchar_t>, Alloc<wchar_t>>
		   >::value,
    "std::basic_osyncstream<wchar_t> has the correct default template argument");
#endif

using std::istreambuf_iterator;

static_assert(Same<istreambuf_iterator<char>,
		   istreambuf_iterator<char, Traits<char>>
		   >::value,
    "std::istreambuf_iterator<char> has the correct default template argument");

static_assert(Same<istreambuf_iterator<wchar_t>,
		   istreambuf_iterator<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::istreambuf_iterator<wchar_t> has the correct default template argument");

using std::ostreambuf_iterator;

static_assert(Same<ostreambuf_iterator<char>,
		   ostreambuf_iterator<char, Traits<char>>
		   >::value,
    "std::ostreambuf_iterator<char> has the correct default template argument");

static_assert(Same<ostreambuf_iterator<wchar_t>,
		   ostreambuf_iterator<wchar_t, Traits<wchar_t>>
		   >::value,
    "std::ostreambuf_iterator<wchar_t> has the correct default template argument");
