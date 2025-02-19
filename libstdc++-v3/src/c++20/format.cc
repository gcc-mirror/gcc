// Definitions for <chrono> formatting -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#define _GLIBCXX_USE_CXX11_ABI 1
#include "../c++26/text_encoding.cc"

#ifdef _GLIBCXX_USE_NL_LANGINFO_L
# include <format>
# include <chrono>
# include <memory>   // make_unique
# include <mutex>    // mutex, lock_guard
# include <string.h> // strlen, strcpy
# ifdef _GLIBCXX_HAVE_ICONV
#  include <iconv.h>
#  include <errno.h>
# endif
#endif

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __format
{
// Helpers for P2419R2
// (Clarify handling of encodings in localized formatting of chrono types)
// Convert a string from the locale's charset to UTF-8.

#if defined _GLIBCXX_USE_NL_LANGINFO_L && __CHAR_BIT__ == 8
namespace
{
#ifndef _GLIBCXX_HAS_GTHREADS
// Dummy mutex
struct mutex
{
  void lock() const { }
  void unlock() const { }
};
#endif

// A non-standard locale::facet that caches the locale's std::text_encoding
// and an iconv descriptor for converting from that encoding to UTF-8.
struct __encoding : locale::facet
{
  static locale::id id;

  explicit
  __encoding(const text_encoding& enc, size_t refs = 0)
  : facet(refs), _M_enc(enc)
  {
#ifdef _GLIBCXX_HAVE_ICONV
    using enum text_encoding::id;
    switch (_M_enc.mib())
      {
      case UTF8:
      case ASCII:
	break;
      default:
	_M_cd = ::iconv_open("UTF-8", _M_enc.name());
      }
#endif
  }

  ~__encoding()
  {
#ifdef _GLIBCXX_HAVE_ICONV
    if (_M_cd != (::iconv_t)-1)
      ::iconv_close(_M_cd);
#endif
  }

  text_encoding _M_enc;
#ifdef _GLIBCXX_HAVE_ICONV
  ::iconv_t _M_cd = (::iconv_t)-1;
  mutable mutex mx;
#endif

  // Convert `input` to UTF-8, using `out` to hold the result.
  codecvt_base::result
  conv(string_view input, [[maybe_unused]] string& out) const
  {
    if (input.empty()) [[unlikely]]
      return codecvt_base::noconv;

#ifdef _GLIBCXX_HAVE_ICONV
    if (_M_cd == (::iconv_t)-1)
      return codecvt_base::error;

    size_t inbytesleft = input.size();
    size_t written = 0;
    bool done = false;

    auto overwrite = [&](char* p, size_t n) {
      auto inbytes
	= const_cast<char*>(input.data()) + input.size() - inbytesleft;
      char* outbytes = p + written;
      size_t outbytesleft = n - written;
      size_t res = ::iconv(_M_cd, &inbytes, &inbytesleft,
			   &outbytes, &outbytesleft);
      if (res == (size_t)-1)
	{
	  if (errno != E2BIG)
	    {
	      ::iconv(_M_cd, nullptr, 0, nullptr, 0); // reset
	      done = true;
	      return 0zu;
	    }
	}
      else
	done = true;
      written = outbytes - p;
      return written;
    };

    size_t mult = 1;
    lock_guard<mutex> lock(mx);
    do
      {
	// Estimate that we need 1.5 UTF-8 code units per char, but increase
	// that every time the conversion fails due to insufficient space.
	out.resize_and_overwrite((inbytesleft * 3 / 2) * mult, overwrite);
	++mult;
      }
    while (!done);

    return out.empty() ? codecvt_base::error : codecvt_base::ok;
#else
    return codecvt_base::error;
#endif
  }
};

locale::id __encoding::id;

inline const __encoding*
__get_encoding_facet(const locale& loc)
{
  // Don't need to use __try_use_facet with its dynamic_cast<const __encoding*>
  // because we know there are no types derived from __encoding. We have the
  // facet if the id is within the array bounds and the element is non-null.
  const auto id = __encoding::id._M_id();
  if (id >= loc._M_impl->_M_facets_size)
    return nullptr;
  return static_cast<const __encoding*>(loc._M_impl->_M_facets[id]);
}

} // namespace

locale
__with_encoding_conversion(const locale& loc)
{
  if (__get_encoding_facet(loc))
    return loc;

  string name = loc.name();
  if (name == "C" || name == "*")
    return loc;

  text_encoding locenc = __locale_encoding(name.c_str());

  if (locenc == text_encoding::UTF8 || locenc == text_encoding::ASCII
     || locenc == text_encoding::unknown)
    return loc;

  auto facetp = std::make_unique<__encoding>(locenc);
  locale loc2(loc, facetp.get()); // FIXME: PR libstdc++/113704
  facetp.release();
  // FIXME: Ideally we wouldn't need to reallocate this string again,
  // just don't delete[] it in the locale(locale, Facet*) constructor.
  if (const char* name = loc._M_impl->_M_names[0])
    {
      loc2._M_impl->_M_names[0] = new char[strlen(name) + 1];
      strcpy(loc2._M_impl->_M_names[0], name);
    }
  return loc2;
}

string_view
__locale_encoding_to_utf8(const locale& loc, string_view str, void* poutbuf)
{
  string& outbuf = *static_cast<string*>(poutbuf);
  if (auto enc_facet = __get_encoding_facet(loc))
    {
      auto result = enc_facet->conv(str, outbuf);
      if (result == codecvt_base::ok)
	str = outbuf; // UTF-8 output was written to outbuf.
      // else result was noconv or error, return str unchanged.
    }
  return str;
}
#else
locale
__with_encoding_conversion(const locale& loc)
{ return loc; }

string_view
__locale_encoding_to_utf8(const locale&, string_view str, void*)
{ return str; }
#endif // USE_NL_LANGINFO_L && CHAR_BIT == 8
} // namespace __format
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
