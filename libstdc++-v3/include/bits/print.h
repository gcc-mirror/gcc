// Inline implementation details for std::print functions -*- C++ -*-

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

/** @file include/bits/print.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{print}
 *
 *  This file contains the parts of `<print>` which are currently defined
 *  inline, but should be moved into the library eventually.
 */

#ifndef _GLIBCXX_PRINT_H
#define _GLIBCXX_PRINT_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/requires_hosted.h> // for std::format

#include <bits/version.h>

#ifdef __glibcxx_print // C++ >= 23

#include <format>
#include <cstdio>             // FILE, EOF, flockfile, etc.
#include <cerrno>             // EACCES, EIO
#include <bits/functexcept.h> // __throw_system_error

#ifdef _WIN32
# include <system_error> // system_error
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace __format
{
#if _GLIBCXX_USE_STDIO_LOCKING && _GLIBCXX_USE_GLIBC_STDIO_EXT
  // These are defined in <stdio_ext.h> but we don't want to include that.
  extern "C" int __fwritable(FILE*) noexcept;
  extern "C" int __flbf(FILE*) noexcept;
  extern "C" size_t __fbufsize(FILE*) noexcept;

  // A format sink that writes directly to a Glibc FILE.
  // The file is locked on construction and its buffer is accessed directly.
  class _File_sink final : _Buf_sink<char>
  {
    struct _File
    {
      explicit
      _File(FILE* __f) : _M_file(__f)
      {
	::flockfile(__f);
	// Ensure stream is in write mode
	if (!__fwritable(__f))
	  {
	    ::funlockfile(__f);
	    __throw_system_error(EACCES);
	  }
      }

      ~_File() { ::funlockfile(_M_file); }

      _File(_File&&) = delete;

      // Allocate FILE's output buffer if needed, and return a span
      // viewing unused portion of it.
      std::span<char>
      _M_init_write_buf()
      {
	// After setvbuf glibc pre-allocates the buffer but _IO_write_ptr
	// remains null until the first write.
	if (!_M_file->_IO_write_ptr || _M_write_buf().empty())
	  if (::__overflow(_M_file, EOF) == EOF)
	    __throw_system_error(errno);
	return _M_write_buf();
      }

      // A span viewing the unused portion of the stream's output buffer.
      std::span<char>
      _M_write_buf() noexcept
      {
	return {_M_file->_IO_write_ptr,
		size_t(_M_file->_IO_buf_end - _M_file->_IO_write_ptr)};
      }

      // Flush the output buffer to the file so we can write to it again.
      void
      _M_flush()
      {
	if (::fflush_unlocked(_M_file))
	  __throw_system_error(errno);
      }

      // Update the current position in the output buffer.
      // __n is the number of characters written to the _M_write_buf() span,
      // so will not exceed the size of the output buffer.
      void
      _M_bump(size_t __n) noexcept
      { _M_file->_IO_write_ptr += __n; }

      bool
      _M_line_buffered() const noexcept
      { return __flbf(_M_file); } // Or: _M_file->_flags & 0x200

      bool
      _M_unbuffered() const noexcept
      { return __fbufsize(_M_file) == 1; } // Or: _M_file->_flags & 0x2

      FILE* _M_file;
    } _M_file;

    bool _M_add_newline; // True for std::println, false for std::print.

    // Flush the stream's put area so it can be refilled.
    void
    _M_overflow() override
    {
      auto __s = this->_M_used();
      if (__s.data() == this->_M_buf)
	{
	  // Characters in internal buffer need to be transferred to the FILE.
	  auto __n = ::fwrite_unlocked(__s.data(), 1, __s.size(),
				       _M_file._M_file);
	  if (__n != __s.size())
	    __throw_system_error(errno);
	  this->_M_reset(this->_M_buf);
	}
      else
	{
	  // Characters were written directly to the FILE's output buffer.
	  _M_file._M_bump(__s.size());
	  _M_file._M_flush();
	  this->_M_reset(_M_file._M_write_buf());
	}
    }

  public:
    _File_sink(FILE* __f, bool __add_newline)
    : _M_file(__f), _M_add_newline(__add_newline)
    {
      if (!_M_file._M_unbuffered())
	// Allocate FILE's output buffer if needed, and write directly to it.
	this->_M_reset(_M_file._M_init_write_buf());
    }

    // This calls I/O functions which are cancellation points, so they
    // could exit with a __forced_unwind exception. The noexcept(false)
    // allows that to propagate instead of terminating the process.
    ~_File_sink() noexcept(false)
    {
      auto __s = this->_M_used();
      if (__s.data() == this->_M_buf) // Unbuffered stream
	{
	  _File_sink::_M_overflow(); // Transfer _M_buf to stream.
	  if (_M_add_newline)
	    ::putc_unlocked('\n', _M_file._M_file);
	}
      else
	{
	  _M_file._M_bump(__s.size());
	  if (_M_add_newline)
	    ::putc_unlocked('\n', _M_file._M_file); // '\n' triggers a flush
	  else if (_M_file._M_line_buffered() && __s.size()
		     && (__s.back() == '\n'
			   || __builtin_memchr(__s.data(), '\n', __s.size())))
	    _M_file._M_flush();
	}
    }

    using _Sink<char>::out;
  };
#elif _GLIBCXX_USE_STDIO_LOCKING
  // A format sink that buffers output and then copies it to a stdio FILE.
  // The file is locked on construction and written to using fwrite_unlocked.
  class _File_sink final : _Buf_sink<char>
  {
    struct _File // RAII type to lock/unlock the file.
    {
      explicit _File(FILE* __f) : _M_file(__f) { ::flockfile(_M_file); }
      ~_File() { ::funlockfile(_M_file); }
      FILE* _M_file;
    } _M_file;

    bool _M_add_newline;

    // Transfer buffer contents to the FILE, so buffer can be refilled.
    void
    _M_overflow() override
    {
      auto __s = this->_M_used();
#if _GLIBCXX_HAVE_FWRITE_UNLOCKED
      auto __n = ::fwrite_unlocked(__s.data(), 1, __s.size(), _M_file._M_file);
      if (__n != __s.size())
	__throw_system_error(errno);
#else
      for (char __c : __s)
	::putc_unlocked(__c, _M_file._M_file);
      if (::ferror(_M_file._M_file))
	__throw_system_error(errno);
#endif
      this->_M_reset(this->_M_buf);
    }

  public:
    _File_sink(FILE* __f, bool __add_newline) noexcept
    : _Buf_sink<char>(), _M_file(__f), _M_add_newline(__add_newline)
    { }

    ~_File_sink() noexcept(false) // See above for noexcept(false) rationale.
    {
      _File_sink::_M_overflow();
      if (_M_add_newline)
	::putc_unlocked('\n', _M_file._M_file);
    }

    using _Sink<char>::out;
  };
#else
  // A wrapper around a format sink that copies the output to a stdio FILE.
  // This is not actually a _Sink itself, but it creates one to hold the
  // formatted characters and then copies them to the file when finished.
  class _File_sink final
  {
    FILE* _M_file;
    _Str_sink<char> _M_sink;
    bool _M_add_newline;

  public:
    _File_sink(FILE* __f, bool __add_newline) noexcept
    : _M_file(__f), _M_add_newline(__add_newline)
    { }

    ~_File_sink() noexcept(false) // See above for noexcept(false) rationale.
    {
      string __s = std::move(_M_sink).get();
      if (_M_add_newline)
	__s += '\n';
      auto __n = std::fwrite(__s.data(), 1, __s.size(), _M_file);
      if (__n < __s.size())
	__throw_system_error(EIO); // Non-POSIX fwrite doesn't set errno.
    }

    auto out() { return _M_sink.out(); }
  };
#endif
} // namespace __format

#ifdef _GLIBCXX_NO_INLINE_PRINT
# define _GLIBCXX_PRINT_INLINE_USED [[__gnu__::__used__]]
#else
# define _GLIBCXX_PRINT_INLINE_USED
#endif

  _GLIBCXX_PRINT_INLINE_USED
  inline void
  vprint_nonunicode(FILE* __stream, string_view __fmt, format_args __args)
  {
    std::vformat_to(__format::_File_sink(__stream, false).out(), __fmt, __args);
  }

  _GLIBCXX_PRINT_INLINE_USED
  inline void
  vprint_nonunicode_buffered(FILE* __stream, string_view __fmt,
			     format_args __args)
  {
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 4549. vprint_nonunicode_buffered ignores its stream parameter
    __format::_Str_sink<char> __buf;
    std::vformat_to(__buf.out(), __fmt, __args);
    auto __out = __buf.view();
    if (std::fwrite(__out.data(), 1, __out.size(), __stream) != __out.size())
      __throw_system_error(EIO);
  }

  _GLIBCXX_PRINT_INLINE_USED
  inline void
  vprint_unicode(FILE* __stream, string_view __fmt, format_args __args)
  {
#if !defined(_WIN32) || defined(__CYGWIN__)
    // For most targets we don't need to do anything special to write
    // Unicode to a terminal.
    std::vprint_nonunicode(__stream, __fmt, __args);
#else
    __format::_Str_sink<char> __buf;
    std::vformat_to(__buf.out(), __fmt, __args);
    auto __out = __buf._M_span();

    void* __open_terminal(FILE*);
    error_code __write_to_terminal(void*, span<char>);
    // If stream refers to a terminal, write a native Unicode string to it.
    if (auto __term = __open_terminal(__stream))
      {
	error_code __e;
	if (!std::fflush(__stream))
	  {
	    __e = __write_to_terminal(__term, __out);
	    if (!__e)
	      return;
	    if (__e == std::make_error_code(errc::illegal_byte_sequence))
	      return;
	  }
	else
	  __e = error_code(errno, generic_category());
	_GLIBCXX_THROW_OR_ABORT(system_error(__e, "std::vprint_unicode"));
      }

    // Otherwise just write the string to the file.
    if (std::fwrite(__out.data(), 1, __out.size(), __stream) != __out.size())
      __throw_system_error(EIO);
#endif
  }

  _GLIBCXX_PRINT_INLINE_USED
  inline void
  vprint_unicode_buffered(FILE* __stream, string_view __fmt, format_args __args)
  {
#if !defined(_WIN32) || defined(__CYGWIN__)
    // For most targets we don't need to do anything special to write
    // Unicode to a terminal. Just use the nonunicode function.
    std::vprint_nonunicode_buffered(__stream, __fmt, __args);
#else
    // For Windows the locking function formats everything first anyway,
    // so no formatting happens while a lock is taken. Just use that.
    std::vprint_unicode(__stream, __fmt, __args);
#endif
  }
#undef _GLIBCXX_PRINT_INLINE_USED

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_print
#endif // _GLIBCXX_PRINT_H
