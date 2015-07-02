// wstring_convert implementation -*- C++ -*-

// Copyright (C) 2015 Free Software Foundation, Inc.
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

/** @file bits/locale_conv.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{locale}
 */

#ifndef _LOCALE_CONV_H
#define _LOCALE_CONV_H 1

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <streambuf>
#include "stringfwd.h"
#include "allocator.h"
#include "codecvt.h"
#include "unique_ptr.h"

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef _GLIBCXX_USE_WCHAR_T

  /**
   * @addtogroup locales
   * @{
   */

_GLIBCXX_BEGIN_NAMESPACE_CXX11
  /// String conversions
  template<typename _Codecvt, typename _Elem = wchar_t,
	   typename _Wide_alloc = allocator<_Elem>,
	   typename _Byte_alloc = allocator<char>>
    class wstring_convert
    {
    public:
      typedef basic_string<char, char_traits<char>, _Byte_alloc>   byte_string;
      typedef basic_string<_Elem, char_traits<_Elem>, _Wide_alloc> wide_string;
      typedef typename _Codecvt::state_type 			   state_type;
      typedef typename wide_string::traits_type::int_type	   int_type;

      /** Default constructor.
       *
       * @param  __pcvt The facet to use for conversions.
       *
       * Takes ownership of @p __pcvt and will delete it in the destructor.
       */
      explicit
      wstring_convert(_Codecvt* __pcvt = new _Codecvt()) : _M_cvt(__pcvt)
      {
	if (!_M_cvt)
	  __throw_logic_error("wstring_convert");
      }

      /** Construct with an initial converstion state.
       *
       * @param  __pcvt The facet to use for conversions.
       * @param  __state Initial conversion state.
       *
       * Takes ownership of @p __pcvt and will delete it in the destructor.
       * The object's conversion state will persist between conversions.
       */
      wstring_convert(_Codecvt* __pcvt, state_type __state)
      : _M_cvt(__pcvt), _M_state(__state), _M_with_cvtstate(true)
      {
	if (!_M_cvt)
	  __throw_logic_error("wstring_convert");
      }

      /** Construct with error strings.
       *
       * @param  __byte_err A string to return on failed conversions.
       * @param  __wide_err A wide string to return on failed conversions.
       */
      explicit
      wstring_convert(const byte_string& __byte_err,
		      const wide_string& __wide_err = wide_string())
      : _M_cvt(new _Codecvt),
	_M_byte_err_string(__byte_err), _M_wide_err_string(__wide_err),
	_M_with_strings(true)
      {
	if (!_M_cvt)
	  __throw_logic_error("wstring_convert");
      }

      ~wstring_convert() = default;

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2176. Special members for wstring_convert and wbuffer_convert
      wstring_convert(const wstring_convert&) = delete;
      wstring_convert& operator=(const wstring_convert&) = delete;

      /// @{ Convert from bytes.
      wide_string
      from_bytes(char __byte)
      {
	char __bytes[2] = { __byte };
	return from_bytes(__bytes, __bytes+1);
      }

      wide_string
      from_bytes(const char* __ptr)
      { return from_bytes(__ptr, __ptr+char_traits<char>::length(__ptr)); }

      wide_string
      from_bytes(const byte_string& __str)
      {
	auto __ptr = __str.data();
	return from_bytes(__ptr, __ptr + __str.size());
      }

      wide_string
      from_bytes(const char* __first, const char* __last)
      {
	auto __errstr = _M_with_strings ? &_M_wide_err_string : nullptr;
	_ConvFn<char, _Elem> __fn = &_Codecvt::in;
	return _M_conv(__first, __last, __errstr, __fn);
      }
      /// @}

      /// @{ Convert to bytes.
      byte_string
      to_bytes(_Elem __wchar)
      {
	_Elem __wchars[2] = { __wchar };
	return to_bytes(__wchars, __wchars+1);
      }

      byte_string
      to_bytes(const _Elem* __ptr)
      {
	return to_bytes(__ptr, __ptr+wide_string::traits_type::length(__ptr));
      }

      byte_string
      to_bytes(const wide_string& __wstr)
      {
	auto __ptr = __wstr.data();
	return to_bytes(__ptr, __ptr + __wstr.size());
      }

      byte_string
      to_bytes(const _Elem* __first, const _Elem* __last)
      {
	auto __errstr = _M_with_strings ? &_M_byte_err_string : nullptr;
	_ConvFn<_Elem, char> __fn = &_Codecvt::out;
	return _M_conv(__first, __last, __errstr, __fn);
      }
      /// @}

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2174. wstring_convert::converted() should be noexcept
      /// The number of elements successfully converted in the last conversion.
      size_t converted() const noexcept { return _M_count; }

      /// The final conversion state of the last conversion.
      state_type state() const { return _M_state; }

    private:
      template<typename _InC, typename _OutC>
	using _ConvFn
	  = codecvt_base::result
	    (_Codecvt::*)(state_type&, const _InC*, const _InC*, const _InC*&,
			  _OutC*, _OutC*, _OutC*&) const;

      template<typename _InChar, typename _OutStr, typename _MemFn>
	_OutStr
	_M_conv(const _InChar* __first, const _InChar* __last,
		const _OutStr* __err, _MemFn __memfn)
	{
	  auto __outstr = __err ? _OutStr(__err->get_allocator()) : _OutStr();

	  if (__first == __last)
	    {
	      _M_count = 0;
	      return __outstr;
	    }

	  if (!_M_with_cvtstate)
	    _M_state = state_type();

	  size_t __outchars = 0;
	  auto __next = __first;
	  const auto __maxlen = _M_cvt->max_length() + 1;

	  codecvt_base::result __result;
	  do
	    {
	      __outstr.resize(__outstr.size() + (__last - __next) * __maxlen);
	      auto __outnext = &__outstr.front() + __outchars;
	      auto const __outlast = &__outstr.back() + 1;
	      __result = ((*_M_cvt).*__memfn)(_M_state, __next, __last, __next,
					    __outnext, __outlast, __outnext);
	      __outchars = __outnext - &__outstr.front();
	    }
	  while (__result == codecvt_base::partial && __next != __last
		 && (__outstr.size() - __outchars) < __maxlen);

	  if (__result == codecvt_base::noconv)
	    {
	      __outstr.assign(__first, __last);
	      _M_count = __outstr.size();
	      return __outstr;
	    }

	  __outstr.resize(__outchars);
	  _M_count = __next - __first;

	  if (__result != codecvt_base::error)
	    return __outstr;
	  else if (__err)
	    return *__err;
	  else
	    __throw_range_error("wstring_convert");
	}

      unique_ptr<_Codecvt>	_M_cvt;
      byte_string		_M_byte_err_string;
      wide_string		_M_wide_err_string;
      state_type		_M_state = state_type();
      size_t			_M_count = 0;
      bool			_M_with_cvtstate = false;
      bool			_M_with_strings = false;
    };
_GLIBCXX_END_NAMESPACE_CXX11

  /// Buffer conversions
  template<typename _Codecvt, typename _Elem = wchar_t,
	   typename _Tr = char_traits<_Elem>>
    class wbuffer_convert : public basic_streambuf<_Elem, _Tr>
    {
      typedef basic_streambuf<_Elem, _Tr> _Wide_streambuf;

    public:
      typedef typename _Codecvt::state_type state_type;

      /** Default constructor.
       *
       * @param  __bytebuf The underlying byte stream buffer.
       * @param  __pcvt    The facet to use for conversions.
       * @param  __state   Initial conversion state.
       *
       * Takes ownership of @p __pcvt and will delete it in the destructor.
       */
      explicit
      wbuffer_convert(streambuf* __bytebuf = 0, _Codecvt* __pcvt = new _Codecvt,
		      state_type __state = state_type())
      : _M_buf(__bytebuf), _M_cvt(__pcvt), _M_state(__state)
      {
	if (!_M_cvt)
	  __throw_logic_error("wbuffer_convert");

	_M_always_noconv = _M_cvt->always_noconv();

	if (_M_buf)
	  {
	    this->setp(_M_put_area, _M_put_area + _S_buffer_length);
	    this->setg(_M_get_area + _S_putback_length,
		       _M_get_area + _S_putback_length,
		       _M_get_area + _S_putback_length);
	  }
      }

      ~wbuffer_convert() = default;

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2176. Special members for wstring_convert and wbuffer_convert
      wbuffer_convert(const wbuffer_convert&) = delete;
      wbuffer_convert& operator=(const wbuffer_convert&) = delete;

      streambuf* rdbuf() const noexcept { return _M_buf; }

      streambuf*
      rdbuf(streambuf *__bytebuf) noexcept
      {
	auto __prev = _M_buf;
	_M_buf = __bytebuf;
	return __prev;
      }

      /// The conversion state following the last conversion.
      state_type state() const noexcept { return _M_state; }

    protected:
      int
      sync()
      { return _M_buf && _M_conv_put() && _M_buf->pubsync() ? 0 : -1; }

      typename _Wide_streambuf::int_type
      overflow(typename _Wide_streambuf::int_type __out)
      {
	if (!_M_buf || !_M_conv_put())
	  return _Tr::eof();
	else if (!_Tr::eq_int_type(__out, _Tr::eof()))
	  return this->sputc(__out);
	return _Tr::not_eof(__out);
      }

      typename _Wide_streambuf::int_type
      underflow()
      {
	if (!_M_buf)
	  return _Tr::eof();

	if (this->gptr() < this->egptr() || (_M_buf && _M_conv_get()))
	  return _Tr::to_int_type(*this->gptr());
	else
	  return _Tr::eof();
      }

      streamsize
      xsputn(const typename _Wide_streambuf::char_type* __s, streamsize __n)
      {
	if (!_M_buf || __n == 0)
	  return 0;
	streamsize __done = 0;
	do
	{
	  auto __nn = std::min<streamsize>(this->epptr() - this->pptr(),
					   __n - __done);
	  _Tr::copy(this->pptr(), __s + __done, __nn);
	  this->pbump(__nn);
	  __done += __nn;
	} while (__done < __n && _M_conv_put());
	return __done;
      }

    private:
      // fill the get area from converted contents of the byte stream buffer
      bool
      _M_conv_get()
      {
	const streamsize __pb1 = this->gptr() - this->eback();
	const streamsize __pb2 = _S_putback_length;
	const streamsize __npb = std::min(__pb1, __pb2);

	_Tr::move(_M_get_area + _S_putback_length - __npb,
		  this->gptr() - __npb, __npb);

	streamsize __nbytes = sizeof(_M_get_buf) - _M_unconv;
	__nbytes = std::min(__nbytes, _M_buf->in_avail());
	if (__nbytes < 1)
	  __nbytes == 1;
	__nbytes = _M_buf->sgetn(_M_get_buf + _M_unconv, __nbytes);
	if (__nbytes < 1)
	  return false;
	__nbytes += _M_unconv;

	// convert _M_get_buf into _M_get_area

	_Elem* __outbuf = _M_get_area + _S_putback_length;
	_Elem* __outnext = __outbuf;
	const char* __bnext = _M_get_buf;

	codecvt_base::result __result;
	if (_M_always_noconv)
	  __result = codecvt_base::noconv;
	else
	  {
	    _Elem* __outend = _M_get_area + _S_buffer_length;

	    __result = _M_cvt->in(_M_state,
				  __bnext, __bnext + __nbytes, __bnext,
				  __outbuf, __outend, __outnext);
	  }

	if (__result == codecvt_base::noconv)
	  {
	    // cast is safe because noconv means _Elem is same type as char
	    auto __get_buf = reinterpret_cast<const _Elem*>(_M_get_buf);
	    _Tr::copy(__outbuf, __get_buf, __nbytes);
	    _M_unconv = 0;
	    return true;
	  }

	if ((_M_unconv = _M_get_buf + __nbytes - __bnext))
	  char_traits<char>::move(_M_get_buf, __bnext, _M_unconv);

	this->setg(__outbuf, __outbuf, __outnext);

	return __result != codecvt_base::error;
      }

      // unused
      bool
      _M_put(...)
      { return false; }

      bool
      _M_put(const char* __p, streamsize __n)
      {
	if (_M_buf->sputn(__p, __n) < __n)
	  return false;
      }

      // convert the put area and write to the byte stream buffer
      bool
      _M_conv_put()
      {
	_Elem* const __first = this->pbase();
	const _Elem* const __last = this->pptr();
	const streamsize __pending = __last - __first;

	if (_M_always_noconv)
	  return _M_put(__first, __pending);

	char __outbuf[2 * _S_buffer_length];

	const _Elem* __next = __first;
	const _Elem* __start;
	do
	  {
	    __start = __next;
	    char* __outnext = __outbuf;
	    char* const __outlast = __outbuf + sizeof(__outbuf);
	    auto __result = _M_cvt->out(_M_state, __next, __last, __next,
					__outnext, __outlast, __outnext);
	    if (__result == codecvt_base::error)
	      return false;
	    else if (__result == codecvt_base::noconv)
	      return _M_put(__next, __pending);

	    if (!_M_put(__outbuf, __outnext - __outbuf))
	      return false;
	  }
	while (__next != __last && __next != __start);

	if (__next != __last)
	  _Tr::move(__first, __next, __last - __next);

	this->pbump(__first - __next);
	return __next != __first;
      }

      streambuf*		_M_buf;
      unique_ptr<_Codecvt>	_M_cvt;
      state_type		_M_state;

      static const streamsize	_S_buffer_length = 32;
      static const streamsize	_S_putback_length = 3;
      _Elem                     _M_put_area[_S_buffer_length];
      _Elem                     _M_get_area[_S_buffer_length];
      streamsize		_M_unconv = 0;
      char			_M_get_buf[_S_buffer_length-_S_putback_length];
      bool			_M_always_noconv;
    };

  /// @} group locales

#endif  // _GLIBCXX_USE_WCHAR_T

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // __cplusplus

#endif /* _LOCALE_CONV_H */
