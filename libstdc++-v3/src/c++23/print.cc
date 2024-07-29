// std::print -*- C++ -*-

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

#include <span>
#include <string>
#include <streambuf>
#include <system_error>
#include <cstdio>
#include <cstdint> // uint32_t
#include <fstream>
#include <ext/stdio_filebuf.h>
#include <ext/stdio_sync_filebuf.h>
#include <ext/numeric_traits.h>

#ifdef _WIN32
# include <stdio.h>   // _fileno
# include <io.h>      // _get_osfhandle, _open_osfhandle, _write
# include <fcntl.h>   // _O_APPEND
# include <windows.h> // GetLastError, WriteConsoleW
#elifdef _GLIBCXX_HAVE_UNISTD_H
# include <stdio.h>   // fileno
# include <unistd.h>  // isatty
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef _WIN32
namespace
{
  void*
  check_for_console(void* handle)
  {
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE)
      {
	unsigned long mode; // unused
	if (::GetConsoleMode(handle, &mode))
	  return handle;
      }
    return nullptr;
  }
} // namespace
#endif

  // This returns intptr_t that is either a Windows HANDLE
  // or 1 + a POSIX file descriptor. A zero return indicates failure.
  void*
  __open_terminal([[maybe_unused]] FILE* f)
  {
#ifndef _GLIBCXX_USE_STDIO_PURE
    if (f)
      {
#ifdef _WIN32
	if (int fd = ::_fileno(f); fd >= 0)
	  return check_for_console((void*)_get_osfhandle(fd));
#elif defined _GLIBCXX_HAVE_UNISTD_H && ! defined __AVR__
	if (int fd = (::fileno)(f); fd >= 0 && ::isatty(fd))
	  return f;
#endif
      }
#endif
    return nullptr;
  }

  void*
  __open_terminal([[maybe_unused]] std::streambuf* sb)
  {
#if ! defined _GLIBCXX_USE_STDIO_PURE && defined __cpp_rtti
    using namespace __gnu_cxx;

    if (auto fb = dynamic_cast<stdio_sync_filebuf<char>*>(sb))
      return __open_terminal(fb->file());

    if (auto fb = dynamic_cast<stdio_filebuf<char>*>(sb))
      return __open_terminal(fb->file());

#ifdef __glibcxx_fstream_native_handle
#ifdef _WIN32
    if (auto fb = dynamic_cast<filebuf*>(sb))
      return check_for_console(fb->native_handle());
#elif defined _GLIBCXX_HAVE_UNISTD_H && ! defined __AVR__
    if (auto fb = dynamic_cast<filebuf*>(sb))
      if (int fd = fb->native_handle(); fd >= 0 && ::isatty(fd))
	return ::fdopen(::dup(fd), "w"); // Caller must call fclose.
#endif
#endif
#endif // ! _GLIBCXX_USE_STDIO_PURE

    return nullptr;
  }

namespace
{
  // Validate UTF-8 string, replacing invalid sequences with U+FFFD.
  //
  // Return true if the input is valid UTF-8, false otherwise.
  //
  // If sizeof(_CharT) > 1, then transcode a valid string into out,
  // using either UTF-16 or UTF-32 as determined by sizeof(_CharT).
  //
  // If sizeof(_CharT) == 1 and the input is valid UTF-8, both s and out will
  // be unchanged. Otherwise, each invalid sequence in s will be overwritten
  // with a single 0xFF byte followed by zero or more 0xFE bytes, and then
  // a valid UTF-8 string will be produced in out (replacing invalid
  // sequences with U+FFFD).
  template<typename _CharT>
    bool
    to_valid_unicode(span<char> s, basic_string<_CharT>& out)
    {
      constexpr bool transcode = sizeof(_CharT) > 1;

      unsigned seen = 0, needed = 0;
      unsigned char lo_bound = 0x80, hi_bound = 0xBF;
      size_t errors = 0;

      [[maybe_unused]] uint32_t code_point{};
      if constexpr (transcode)
	{
	  out.clear();
	  // XXX: count code points in s instead of bytes?
	  out.reserve(s.size());
	}

      auto q = s.data(), eoq = q + s.size();
      while (q != eoq)
	{
	  unsigned char byte = *q;
	  if (needed == 0)
	    {
	      if (byte <= 0x7F) [[likely]]      // 0x00 to 0x7F
		{
		  if constexpr (transcode)
		    out.push_back(_CharT(byte));

		  // Fast forward to the next non-ASCII character.
		  while (++q != eoq && (unsigned char)*q <= 0x7F)
		    {
		      if constexpr (transcode)
			out.push_back(*q);
		    }
		  continue;
		}
	      else if (byte < 0xC2) [[unlikely]]
		{
		  if constexpr (transcode)
		    out.push_back(0xFFFD);
		  else
		    *q = 0xFF;
		  ++errors;
		}
	      else if (byte <= 0xDF) // 0xC2 to 0xDF
		{
		  needed = 1;
		  if constexpr (transcode)
		    code_point = byte & 0x1F;
		}
	      else if (byte <= 0xEF) // 0xE0 to 0xEF
		{
		  if (byte == 0xE0)
		    lo_bound = 0xA0;
		  else if (byte == 0xED)
		    hi_bound = 0x9F;

		  needed = 2;
		  if constexpr (transcode)
		    code_point = byte & 0x0F;
		}
	      else if (byte <= 0xF4) // 0xF0 to 0xF4
		{
		  if (byte == 0xF0)
		    lo_bound = 0x90;
		  else if (byte == 0xF4)
		    hi_bound = 0x8F;

		  needed = 3;
		  if constexpr (transcode)
		    code_point = byte & 0x07;
		}
	      else [[unlikely]]
		{
		  if constexpr (transcode)
		    out.push_back(0xFFFD);
		  else
		    *q = 0xFF;
		  ++errors;
		}
	    }
	  else
	    {
	      if (byte < lo_bound || byte > hi_bound) [[unlikely]]
		{
		  if constexpr (transcode)
		    out.push_back(0xFFFD);
		  else
		    {
		      *(q - seen - 1) = 0xFF;
		      __builtin_memset(q - seen, 0xFE, seen);
		    }
		  ++errors;
		  needed = seen = 0;
		  lo_bound = 0x80;
		  hi_bound = 0xBF;
		  continue; // Reprocess the current character.
		}

	      if constexpr (transcode)
		code_point = (code_point << 6) | (byte & 0x3f);

	      lo_bound = 0x80;
	      hi_bound = 0xBF;
	      ++seen;
	      if (seen == needed) [[likely]]
		{
		  if constexpr (transcode)
		    {
		      if (code_point <= __gnu_cxx::__int_traits<_CharT>::__max)
			out.push_back(code_point);
		      else
			{
			  // Algorithm from
			  // http://www.unicode.org/faq/utf_bom.html#utf16-4
			  const char32_t LEAD_OFFSET = 0xD800 - (0x10000 >> 10);
			  char16_t lead = LEAD_OFFSET + (code_point >> 10);
			  char16_t trail = 0xDC00 + (code_point & 0x3FF);
			  out.push_back(lead);
			  out.push_back(trail);
			}
		    }
		  needed = seen = 0;
		}
	    }
	  ++q;
	}

      if (needed) [[unlikely]]
	{
	  // The string ends with an incomplete multibyte sequence.
	  if constexpr (transcode)
	    out.push_back(0xFFFD);
	  else
	    {
	      // Truncate the incomplete sequence to a single byte.
	      if (seen)
		s = s.first(s.size() - seen);
	      s.back() = 0xFF;
	    }
	  ++errors;
	}

      if (errors == 0) [[likely]]
	return true;
      else if constexpr (!transcode)
	{
	  out.reserve(s.size() + errors * 2);
	  for (unsigned char byte : s)
	    {
	      if (byte < 0xFE) [[likely]]
		out += (char)byte;
	      else if (byte == 0xFF)
		out += "\xef\xbf\xbd"; // U+FFFD in UTF-8
	    }
	}
      return false;
    }

  // Validate UTF-8 string.
  // Returns true if s is valid UTF-8, otherwise returns false and stores
  // a valid UTF-8 string in err.
  [[__gnu__::__always_inline__]]
  inline bool
  to_valid_utf8(span<char> s, string& err)
  {
    return to_valid_unicode(s, err);
  }

  // Transcode UTF-8 string to UTF-16.
  // Returns true if s is valid UTF-8, otherwise returns false.
  // In either case, a valid UTF-16 string is stored in u16.
  [[__gnu__::__always_inline__]]
  inline bool
  to_valid_utf16(span<char> s, u16string& u16)
  {
    return to_valid_unicode(s, u16);
  }
} // namespace

  // Write a UTF-8 string to a file descriptor/handle.
  // Ill-formed sequences in the string will be substituted with U+FFFD.
  error_code
  __write_to_terminal(void* term, span<char> str)
  {
    if (term == nullptr) [[unlikely]]
      return std::make_error_code(std::errc::invalid_argument);

    error_code ec;

#ifdef _WIN32
    // We could use std::wstring here instead of std::u16string. In general
    // char_traits<wchar_t> is more optimized than char_traits<char16_t> but
    // for the purposes of to_valid_unicode only char_traits::copy matters,
    // and char_traits<char16_t>::copy uses memcpy so is OK.
    u16string wstr;
    if (!to_valid_utf16(str, wstr))
      ec = std::make_error_code(errc::illegal_byte_sequence);

    // This allows us to test this function with a normal file,
    // see testsuite/27_io/print/2.cc
    if (!check_for_console(term))
      {
	int fd = _open_osfhandle((intptr_t)term, _O_APPEND);
	if (_write(fd, wstr.data(), wstr.size() * 2) == -1)
	  ec = {errno, generic_category()};
	return ec;
      }

    unsigned long nchars = 0;
    WriteConsoleW(term, wstr.data(), wstr.size(), &nchars, nullptr);
    if (nchars != wstr.size())
      return {(int)GetLastError(), system_category()};
#elifdef _GLIBCXX_HAVE_UNISTD_H
    string out;
    if (!to_valid_utf8(str, out))
      {
	str = out;
	ec = std::make_error_code(errc::illegal_byte_sequence);
      }

    auto n = std::fwrite(str.data(), 1, str.size(), (FILE*)term);
    if (n != str.size())
      ec = std::make_error_code(errc::io_error);
#else
    ec = std::make_error_code(std::errc::function_not_supported);
#endif
    return ec;
  }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
