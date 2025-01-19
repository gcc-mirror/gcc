// Output streams -*- C++ -*-

// Copyright (C) 1997-2024 Free Software Foundation, Inc.
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

/** @file bits/ostream.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ostream}
 */

//
// ISO C++ 14882: 27.6.2  Output streams
//

#ifndef _GLIBCXX_OSTREAM_H
#define _GLIBCXX_OSTREAM_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/requires_hosted.h> // iostreams

#include <ios>
#include <bits/ostream_insert.h>

# define __glibcxx_want_print
#include <bits/version.h> // __glibcxx_syncbuf

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief  Template class basic_ostream.
   *  @ingroup io
   *
   *  @tparam _CharT  Type of character stream.
   *  @tparam _Traits  Traits for character type, defaults to
   *                   char_traits<_CharT>.
   *
   *  This is the base class for all output streams.  It provides text
   *  formatting of all builtin types, and communicates with any class
   *  derived from basic_streambuf to do the actual output.
  */
  template<typename _CharT, typename _Traits>
    class basic_ostream : virtual public basic_ios<_CharT, _Traits>
    {
    public:
      // Types (inherited from basic_ios):
      typedef _CharT			 		char_type;
      typedef typename _Traits::int_type 		int_type;
      typedef typename _Traits::pos_type 		pos_type;
      typedef typename _Traits::off_type 		off_type;
      typedef _Traits			 		traits_type;

      // Non-standard Types:
      typedef basic_streambuf<_CharT, _Traits> 		__streambuf_type;
      typedef basic_ios<_CharT, _Traits>		__ios_type;
      typedef basic_ostream<_CharT, _Traits>		__ostream_type;
      typedef num_put<_CharT, ostreambuf_iterator<_CharT, _Traits> >
							__num_put_type;
      typedef ctype<_CharT>	      			__ctype_type;

      /**
       *  @brief  Base constructor.
       *
       *  This ctor is almost never called by the user directly, rather from
       *  derived classes' initialization lists, which pass a pointer to
       *  their own stream buffer.
      */
      explicit
      basic_ostream(__streambuf_type* __sb)
      { this->init(__sb); }

      /**
       *  @brief  Base destructor.
       *
       *  This does very little apart from providing a virtual base dtor.
      */
      virtual
      ~basic_ostream() { }

      /// Safe prefix/suffix operations.
      class sentry;
      friend class sentry;

      ///@{
      /**
       *  @brief  Interface for manipulators.
       *
       *  Manipulators such as @c std::endl and @c std::hex use these
       *  functions in constructs like "std::cout << std::endl".  For more
       *  information, see the iomanip header.
      */
      __ostream_type&
      operator<<(__ostream_type& (*__pf)(__ostream_type&))
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// DR 60. What is a formatted input function?
	// The inserters for manipulators are *not* formatted output functions.
	return __pf(*this);
      }

      __ostream_type&
      operator<<(__ios_type& (*__pf)(__ios_type&))
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// DR 60. What is a formatted input function?
	// The inserters for manipulators are *not* formatted output functions.
	__pf(*this);
	return *this;
      }

      __ostream_type&
      operator<<(ios_base& (*__pf) (ios_base&))
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// DR 60. What is a formatted input function?
	// The inserters for manipulators are *not* formatted output functions.
	__pf(*this);
	return *this;
      }
      ///@}

      ///@{
      /**
       *  @name Inserters
       *
       *  All the @c operator<< functions (aka <em>formatted output
       *  functions</em>) have some common behavior.  Each starts by
       *  constructing a temporary object of type std::basic_ostream::sentry.
       *  This can have several effects, concluding with the setting of a
       *  status flag; see the sentry documentation for more.
       *
       *  If the sentry status is good, the function tries to generate
       *  whatever data is appropriate for the type of the argument.
       *
       *  If an exception is thrown during insertion, ios_base::badbit
       *  will be turned on in the stream's error state without causing an
       *  ios_base::failure to be thrown.  The original exception will then
       *  be rethrown.
      */

      ///@{
      /**
       *  @brief Integer arithmetic inserters
       *  @param  __n A variable of builtin integral type.
       *  @return  @c *this if successful
       *
       *  These functions use the stream's current locale (specifically, the
       *  @c num_get facet) to perform numeric formatting.
      */
      __ostream_type&
      operator<<(long __n)
      { return _M_insert(__n); }

      __ostream_type&
      operator<<(unsigned long __n)
      { return _M_insert(__n); }

      __ostream_type&
      operator<<(bool __n)
      { return _M_insert(__n); }

      __ostream_type&
      operator<<(short __n);

      __ostream_type&
      operator<<(unsigned short __n)
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 117. basic_ostream uses nonexistent num_put member functions.
	return _M_insert(static_cast<unsigned long>(__n));
      }

      __ostream_type&
      operator<<(int __n);

      __ostream_type&
      operator<<(unsigned int __n)
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 117. basic_ostream uses nonexistent num_put member functions.
	return _M_insert(static_cast<unsigned long>(__n));
      }

#ifdef _GLIBCXX_USE_LONG_LONG
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wlong-long"
      __ostream_type&
      operator<<(long long __n)
      { return _M_insert(__n); }

      __ostream_type&
      operator<<(unsigned long long __n)
      { return _M_insert(__n); }
#pragma GCC diagnostic pop
#endif
      ///@}

      ///@{
      /**
       *  @brief  Floating point arithmetic inserters
       *  @param  __f A variable of builtin floating point type.
       *  @return  @c *this if successful
       *
       *  These functions use the stream's current locale (specifically, the
       *  @c num_get facet) to perform numeric formatting.
      */
      __ostream_type&
      operator<<(double __f)
      { return _M_insert(__f); }

      __ostream_type&
      operator<<(float __f)
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 117. basic_ostream uses nonexistent num_put member functions.
	return _M_insert(_S_cast_flt<double>(__f));
      }

      __ostream_type&
      operator<<(long double __f)
      { return _M_insert(__f); }
      ///@}

#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(_Float16 __f)
      {
	return _M_insert(_S_cast_flt<double>(__f));
      }
#endif

#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(_Float32 __f)
      {
	return _M_insert(_S_cast_flt<double>(__f));
      }
#endif

#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(_Float64 __f)
      {
	return _M_insert(_S_cast_flt<double>(__f));
      }
#endif

#if defined(__STDCPP_FLOAT128_T__) && defined(_GLIBCXX_LDOUBLE_IS_IEEE_BINARY128)
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(_Float128 __f)
      {
	return _M_insert(_S_cast_flt<long double>(__f));
      }
#endif

#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(__gnu_cxx::__bfloat16_t __f)
      {
	return _M_insert(_S_cast_flt<double>(__f));
      }
#endif

      /**
       *  @brief  Pointer arithmetic inserters
       *  @param  __p A variable of pointer type.
       *  @return  @c *this if successful
       *
       *  These functions use the stream's current locale (specifically, the
       *  @c num_get facet) to perform numeric formatting.
      */
      __ostream_type&
      operator<<(const void* __p)
      { return _M_insert(__p); }

#if __cplusplus >= 201703L
      __ostream_type&
      operator<<(nullptr_t)
      { return *this << "nullptr"; }
#endif

#if __cplusplus > 202002L
      __attribute__((__always_inline__))
      __ostream_type&
      operator<<(const volatile void* __p)
      { return _M_insert(const_cast<const void*>(__p)); }
#endif

      /**
       *  @brief  Extracting from another streambuf.
       *  @param  __sb  A pointer to a streambuf
       *
       *  This function behaves like one of the basic arithmetic extractors,
       *  in that it also constructs a sentry object and has the same error
       *  handling behavior.
       *
       *  If @p __sb is NULL, the stream will set failbit in its error state.
       *
       *  Characters are extracted from @p __sb and inserted into @c *this
       *  until one of the following occurs:
       *
       *  - the input stream reaches end-of-file,
       *  - insertion into the output sequence fails (in this case, the
       *    character that would have been inserted is not extracted), or
       *  - an exception occurs while getting a character from @p __sb, which
       *    sets failbit in the error state
       *
       *  If the function inserts no characters, failbit is set.
      */
      __ostream_type&
      operator<<(__streambuf_type* __sb);
      ///@}

      ///@{
      /**
       *  @name Unformatted Output Functions
       *
       *  All the unformatted output functions have some common behavior.
       *  Each starts by constructing a temporary object of type
       *  std::basic_ostream::sentry.  This has several effects, concluding
       *  with the setting of a status flag; see the sentry documentation
       *  for more.
       *
       *  If the sentry status is good, the function tries to generate
       *  whatever data is appropriate for the type of the argument.
       *
       *  If an exception is thrown during insertion, ios_base::badbit
       *  will be turned on in the stream's error state.  If badbit is on in
       *  the stream's exceptions mask, the exception will be rethrown
       *  without completing its actions.
      */

      /**
       *  @brief  Simple insertion.
       *  @param  __c  The character to insert.
       *  @return  *this
       *
       *  Tries to insert @p __c.
       *
       *  @note  This function is not overloaded on signed char and
       *         unsigned char.
      */
      __ostream_type&
      put(char_type __c);

      /**
       *  @brief  Character string insertion.
       *  @param  __s  The array to insert.
       *  @param  __n  Maximum number of characters to insert.
       *  @return  *this
       *
       *  Characters are copied from @p __s and inserted into the stream until
       *  one of the following happens:
       *
       *  - @p __n characters are inserted
       *  - inserting into the output sequence fails (in this case, badbit
       *    will be set in the stream's error state)
       *
       *  @note  This function is not overloaded on signed char and
       *         unsigned char.
      */
      __ostream_type&
      write(const char_type* __s, streamsize __n);
      ///@}

      /**
       *  @brief  Synchronizing the stream buffer.
       *  @return  *this
       *
       *  If @c rdbuf() is a null pointer, changes nothing.
       *
       *  Otherwise, calls @c rdbuf()->pubsync(), and if that returns -1,
       *  sets badbit.
      */
      __ostream_type&
      flush();

      /**
       *  @brief  Getting the current write position.
       *  @return  A file position object.
       *
       *  If @c fail() is not false, returns @c pos_type(-1) to indicate
       *  failure.  Otherwise returns @c rdbuf()->pubseekoff(0,cur,out).
      */
      pos_type
      tellp();

      /**
       *  @brief  Changing the current write position.
       *  @param  __pos  A file position object.
       *  @return  *this
       *
       *  If @c fail() is not true, calls @c rdbuf()->pubseekpos(pos).  If
       *  that function fails, sets failbit.
      */
      __ostream_type&
      seekp(pos_type);

      /**
       *  @brief  Changing the current write position.
       *  @param  __off  A file offset object.
       *  @param  __dir  The direction in which to seek.
       *  @return  *this
       *
       *  If @c fail() is not true, calls @c rdbuf()->pubseekoff(off,dir).
       *  If that function fails, sets failbit.
      */
       __ostream_type&
      seekp(off_type, ios_base::seekdir);

    protected:
      basic_ostream()
      { this->init(0); }

#if __cplusplus >= 201103L
      // Non-standard constructor that does not call init()
      basic_ostream(basic_iostream<_CharT, _Traits>&) { }

      basic_ostream(const basic_ostream&) = delete;

      basic_ostream(basic_ostream&& __rhs)
      : __ios_type()
      { __ios_type::move(__rhs); }

      // 27.7.3.3 Assign/swap

      basic_ostream& operator=(const basic_ostream&) = delete;

      basic_ostream&
      operator=(basic_ostream&& __rhs)
      {
	swap(__rhs);
	return *this;
      }

      void
      swap(basic_ostream& __rhs)
      { __ios_type::swap(__rhs); }
#endif

      template<typename _ValueT>
	__ostream_type&
	_M_insert(_ValueT __v);

    private:
#if !_GLIBCXX_INLINE_VERSION
      void
      _M_write(const char_type* __s, streamsize __n)
      { std::__ostream_insert(*this, __s, __n); }
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // for if-constexpr
      template<typename _To, typename _From>
	static _To
	_S_cast_flt(_From __f)
	{
	  _To __d = static_cast<_To>(__f);
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 4101: LWG 117 loses the sign for negative NaN on some arches.
#if defined __riscv
	  _To __sign;
#if __cpp_constexpr && __has_builtin(__builtin_bit_cast)
	  if constexpr (sizeof(__f) == sizeof(short))
	    __sign = static_cast<_To>(__builtin_bit_cast(short, __f));
	  else if constexpr (sizeof(__f) == sizeof(int))
	    __sign = static_cast<_To>(__builtin_bit_cast(int, __f));
	  else if constexpr (sizeof(__f) == sizeof(long long))
	    __sign = static_cast<_To>(__builtin_bit_cast(long long, __f));
	  else
#endif
	  __sign = __builtin_signbit(__f) ? _To(-1.0) : _To(+1.0);

	  if _GLIBCXX17_CONSTEXPR (__is_same(_To, double))
	    __d = __builtin_copysign(__d, __sign);
	  else if _GLIBCXX17_CONSTEXPR (__is_same(_To, long double))
	    __d = __builtin_copysignl(__d, __sign);
#endif
	  return __d;
	}
#pragma GCC diagnostic pop
    };

  /**
   *  @brief  Performs setup work for output streams.
   *
   *  Objects of this class are created before all of the standard
   *  inserters are run.  It is responsible for <em>exception-safe prefix and
   *  suffix operations</em>.
  */
  template <typename _CharT, typename _Traits>
    class basic_ostream<_CharT, _Traits>::sentry
    {
      // Data Members.
      bool 				_M_ok;
      basic_ostream<_CharT, _Traits>& 	_M_os;

    public:
      /**
       *  @brief  The constructor performs preparatory work.
       *  @param  __os  The output stream to guard.
       *
       *  If the stream state is good (@a __os.good() is true), then if the
       *  stream is tied to another output stream, @c is.tie()->flush()
       *  is called to synchronize the output sequences.
       *
       *  If the stream state is still good, then the sentry state becomes
       *  true (@a okay).
      */
      explicit
      sentry(basic_ostream<_CharT, _Traits>& __os);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
      /**
       *  @brief  Possibly flushes the stream.
       *
       *  If @c ios_base::unitbuf is set in @c os.flags(), and
       *  @c std::uncaught_exception() is true, the sentry destructor calls
       *  @c flush() on the output stream.
      */
      ~sentry()
      {
	// XXX MT
	if (bool(_M_os.flags() & ios_base::unitbuf) && !uncaught_exception())
	  {
	    // Can't call flush directly or else will get into recursive lock.
	    if (_M_os.rdbuf() && _M_os.rdbuf()->pubsync() == -1)
	      _M_os.setstate(ios_base::badbit);
	  }
      }
#pragma GCC diagnostic pop

      /**
       *  @brief  Quick status checking.
       *  @return  The sentry state.
       *
       *  For ease of use, sentries may be converted to booleans.  The
       *  return value is that of the sentry state (true == okay).
      */
#if __cplusplus >= 201103L
      explicit
#endif
      operator bool() const
      { return _M_ok; }
    };

  ///@{
  /**
   *  @brief  Character inserters
   *  @param  __out  An output stream.
   *  @param  __c  A character.
   *  @return  out
   *
   *  Behaves like one of the formatted arithmetic inserters described in
   *  std::basic_ostream.  After constructing a sentry object with good
   *  status, this function inserts a single character and any required
   *  padding (as determined by [22.2.2.2.2]).  @c __out.width(0) is then
   *  called.
   *
   *  If @p __c is of type @c char and the character type of the stream is not
   *  @c char, the character is widened before insertion.
  */
  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, _CharT __c)
    {
      if (__out.width() != 0)
	return __ostream_insert(__out, &__c, 1);
      __out.put(__c);
      return __out;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, char __c)
    { return (__out << __out.widen(__c)); }

  // Specialization
  template<typename _Traits>
    inline basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, char __c)
    {
      if (__out.width() != 0)
	return __ostream_insert(__out, &__c, 1);
      __out.put(__c);
      return __out;
    }

  // Signed and unsigned
  template<typename _Traits>
    inline basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, signed char __c)
    { return (__out << static_cast<char>(__c)); }

  template<typename _Traits>
    inline basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, unsigned char __c)
    { return (__out << static_cast<char>(__c)); }

#if __cplusplus > 201703L
  // The following deleted overloads prevent formatting character values as
  // numeric values.

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, wchar_t) = delete;

#ifdef _GLIBCXX_USE_CHAR8_T
  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, char8_t) = delete;
#endif

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, char16_t) = delete;

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, char32_t) = delete;

#ifdef _GLIBCXX_USE_WCHAR_T
#ifdef _GLIBCXX_USE_CHAR8_T
  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, char8_t) = delete;
#endif // _GLIBCXX_USE_CHAR8_T

  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, char16_t) = delete;

  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, char32_t) = delete;
#endif // _GLIBCXX_USE_WCHAR_T
#endif // C++20
  ///@}

  ///@{
  /**
   *  @brief  String inserters
   *  @param  __out  An output stream.
   *  @param  __s  A character string.
   *  @return  out
   *  @pre  @p __s must be a non-NULL pointer
   *
   *  Behaves like one of the formatted arithmetic inserters described in
   *  std::basic_ostream.  After constructing a sentry object with good
   *  status, this function inserts @c traits::length(__s) characters starting
   *  at @p __s, widened if necessary, followed by any required padding (as
   *  determined by [22.2.2.2.2]).  @c __out.width(0) is then called.
  */
  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, const _CharT* __s)
    {
      if (!__s)
	__out.setstate(ios_base::badbit);
      else
	__ostream_insert(__out, __s,
			 static_cast<streamsize>(_Traits::length(__s)));
      return __out;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits> &
    operator<<(basic_ostream<_CharT, _Traits>& __out, const char* __s);

  // Partial specializations
  template<typename _Traits>
    inline basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, const char* __s)
    {
      if (!__s)
	__out.setstate(ios_base::badbit);
      else
	__ostream_insert(__out, __s,
			 static_cast<streamsize>(_Traits::length(__s)));
      return __out;
    }

  // Signed and unsigned
  template<typename _Traits>
    inline basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, const signed char* __s)
    { return (__out << reinterpret_cast<const char*>(__s)); }

  template<typename _Traits>
    inline basic_ostream<char, _Traits> &
    operator<<(basic_ostream<char, _Traits>& __out, const unsigned char* __s)
    { return (__out << reinterpret_cast<const char*>(__s)); }

#if __cplusplus > 201703L
   // The following deleted overloads prevent formatting strings as
   // pointer values.

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, const wchar_t*) = delete;

#ifdef _GLIBCXX_USE_CHAR8_T
  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, const char8_t*) = delete;
#endif // _GLIBCXX_USE_CHAR8_T

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, const char16_t*) = delete;

  template<typename _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>&, const char32_t*) = delete;

#ifdef _GLIBCXX_USE_WCHAR_T
#ifdef _GLIBCXX_USE_CHAR8_T
  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, const char8_t*) = delete;
#endif

  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, const char16_t*) = delete;

  template<typename _Traits>
    basic_ostream<wchar_t, _Traits>&
    operator<<(basic_ostream<wchar_t, _Traits>&, const char32_t*) = delete;
#endif // _GLIBCXX_USE_WCHAR_T
#endif // C++20
  ///@}

#if __cplusplus >= 201103L
  // C++11 27.7.3.9 Rvalue stream insertion [ostream.rvalue]
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 1203. More useful rvalue stream insertion

#if __cpp_concepts >= 201907L && __glibcxx_type_trait_variable_templates
  // Use concepts if possible because they're cheaper to evaluate.
  template<typename _Tp>
    concept __derived_from_ios_base = is_class_v<_Tp>
      && (!is_same_v<_Tp, ios_base>)
      && requires (_Tp* __t, ios_base* __b) { __b = __t; };

  template<typename _Os, typename _Tp>
    requires __derived_from_ios_base<_Os>
      && requires (_Os& __os, const _Tp& __t) { __os << __t; }
    using __rvalue_stream_insertion_t = _Os&&;
#else
  template<typename _Tp>
    using _Require_derived_from_ios_base
      = _Require<is_class<_Tp>, __not_<is_same<_Tp, ios_base>>,
		 is_convertible<typename add_pointer<_Tp>::type, ios_base*>>;

  template<typename _Os, typename _Tp,
	   typename = _Require_derived_from_ios_base<_Os>,
	   typename
	     = decltype(std::declval<_Os&>() << std::declval<const _Tp&>())>
    using __rvalue_stream_insertion_t = _Os&&;
#endif

  /**
   *  @brief  Generic inserter for rvalue stream
   *  @param  __os  An input stream.
   *  @param  __x  A reference to the object being inserted.
   *  @return  __os
   *
   *  This is just a forwarding function to allow insertion to
   *  rvalue streams since they won't bind to the inserter functions
   *  that take an lvalue reference.
  */
  template<typename _Ostream, typename _Tp>
    inline __rvalue_stream_insertion_t<_Ostream, _Tp>
    operator<<(_Ostream&& __os, const _Tp& __x)
    {
      __os << __x;
      return std::move(__os);
    }
#endif // C++11

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif	/* _GLIBCXX_OSTREAM_H */
