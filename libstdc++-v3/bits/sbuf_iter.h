// Streambuf iterators

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// XXX Should specialize copy, find algorithms for streambuf iterators.

#ifndef _CPP_BITS_SBUF_ITER_H
#define _CPP_BITS_SBUF_ITER_H 1

namespace std
{

  template<typename _CharT, typename _Traits>
    class ostreambuf_iterator
#if 0      // XXX this is standard:
    : public iterator<output_iterator_tag,_CharT,void,void,void>
#else
    : public output_iterator
#endif
    {
    public:

      // Types:
      typedef _CharT                       	 char_type;
      typedef _Traits                          traits_type;
      typedef basic_streambuf<_CharT, _Traits> streambuf_type;
      typedef basic_ostream<_CharT, _Traits>   ostream_type;
      
      inline 
      ostreambuf_iterator(ostream_type& __s) throw ()
      : _M_sbuf(__s.rdbuf()), _M_failed(false) { }
      
      ostreambuf_iterator(streambuf_type* __s) throw ()
      : _M_sbuf(__s), _M_failed(false) { }

      ostreambuf_iterator& 
      operator=(_CharT __c);

      ostreambuf_iterator& 
      operator*() throw()
      { return *this; }

      ostreambuf_iterator& 
      operator++(int) throw()
      { return *this; }

      ostreambuf_iterator& 
      operator++() throw()
      { return *this; }

      bool 
      failed() const throw()
      { return _M_failed; }

    private:
      streambuf_type* 	_M_sbuf;
      bool 		_M_failed;

#if 0
      template<>
        friend char const*
        copy(char const* __first, char const* __last,
             ostreambuf_iterator<char,char_traits<char> > __to);
      template<>
        friend wchar_t const*
        copy(wchar_t const* __first, wchar_t const* __last,
             ostreambuf_iterator<wchar_t,char_traits<wchar_t> > __to);
#endif
    };

  template<typename _CharT, typename _Traits>
    inline ostreambuf_iterator<_CharT, _Traits>&
    ostreambuf_iterator<_CharT, _Traits>::operator=(_CharT __c)
    {
      if (!_M_failed &&
          _Traits::eq_int_type(_M_sbuf->sputc(__c),_Traits::eof()))
      _M_failed = true;
      return *this;
    }


#if 0
  // Optimized specializations of standard algorithms
  // These are specialized only for standard types
  // (with no unbound arguments) to avoid creating
  // overload problems with user specializations.

  template<>
    char const*
    copy(char const* __first, char const* __last,
	 ostreambuf_iterator<char,char_traits<char> > __to)
    {
      if (!__to._M_failed)
	__to._M_sbuf->sputn(__first, __last-__first);
      return __last;
    }

  template<>
    wchar_t const*
    copy(wchar_t const* __first, wchar_t const* __last,
	 ostreambuf_iterator<whar_t,char_traits<wchar_t> > __to)
    {
      if (!__to._M_failed)
	__to._M_sbuf->sputn(__first, __last-__first);
      return __last;
    }
#endif

  // 24.5.3 Template class istreambuf_iterator
  template<class _CharT, class _Traits>
    class istreambuf_iterator
    : public iterator<input_iterator_tag, _CharT, typename _Traits::off_type,
    		      _CharT*, _CharT&>
    {
    public:

      // Types:
      typedef _CharT                         		char_type;
      typedef _Traits                        		traits_type;
      typedef typename _Traits::int_type     		int_type;
      typedef basic_streambuf<_CharT, _Traits> 		streambuf_type;
      typedef basic_istream<_CharT, _Traits>         	istream_type;
      // Non-standard Types:
      typedef istreambuf_iterator<_CharT, _Traits>	__istreambufiter_type;

      istreambuf_iterator() throw() 
      : _M_istreambuf(NULL), _M_c(-2) { }
      
      istreambuf_iterator(istream_type& __s) throw()
      : _M_istreambuf(__s.rdbuf()), _M_c(-2) { }

      istreambuf_iterator(streambuf_type* __s) throw()
      : _M_istreambuf(__s), _M_c(-2) { }
       
      // NB: This should really have an int_type return
      // value, so "end of stream" postion can be checked without
      // hacking.
      char_type 
      operator*() const
      { 
	// The result of operator*() on an end of stream is undefined.
	char_type __retval;
	if (_M_istreambuf && _M_c != static_cast<int_type>(-2))
	  __retval = _M_c;
	else if (_M_istreambuf)
	  __retval = traits_type::to_char_type(_M_istreambuf->sgetc()); 
	else
	  __retval = static_cast<char_type>(traits_type::eof());
	return __retval;
      }
	
      __istreambufiter_type& 
      operator++()
      { 
	if (_M_istreambuf)
	  _M_istreambuf->sbumpc();
	_M_c = -2;
	return *this; 
      }

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      // 14882 says return a proxy object. It should be a const
      // proxy object, but since this class is not mandated, it
      // should allow this signature:
      const __istreambufiter_type
      operator++(int)
      {
	if (_M_istreambuf)
	  _M_c = _M_istreambuf->sbumpc();
	return *this; 
      }
#endif

      bool 
      equal(const __istreambufiter_type& __b)
      { 
	int_type __eof = traits_type::eof();
	bool __thiseof = !_M_istreambuf || _M_istreambuf->sgetc() == __eof;
	bool __beof = !__b._M_istreambuf 
	  	      || __b._M_istreambuf->sgetc() == __eof;
	return (__thiseof && __beof || (!__thiseof && !__beof));
      }

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      // 110 istreambuf_iterator::equal not const
      // NB: there is also number 111 pending on this function.
      bool 
      equal(const __istreambufiter_type& __b) const
      {
	int_type __eof = traits_type::eof();
	bool __thiseof = !_M_istreambuf || _M_istreambuf->sgetc() == __eof;
	bool __beof = !__b._M_istreambuf 
	  	      || __b._M_istreambuf->sgetc() == __eof;
	return (__thiseof && __beof || (!__thiseof && !__beof));
      }
#endif

    private:
      // 24.5.3 istreambuf_iterator 
      // p 1 
      // If the end of stream is reached (streambuf_type::sgetc()
      // returns traits_type::eof()), the iterator becomes equal to
      // the "end of stream" iterator value.
      // NB: This implementation assumes the "end of stream" value
      // is EOF, or -1.
      streambuf_type* 		_M_istreambuf;  
      int_type 			_M_c;
    };

  template<typename _CharT, typename _Traits>
    inline bool 
    operator==(const istreambuf_iterator<_CharT, _Traits>& __a,
	       const istreambuf_iterator<_CharT, _Traits>& __b)
    { return __a.equal(__b); }

  template<typename _CharT, typename _Traits>
    inline bool 
    operator!=(const istreambuf_iterator<_CharT, _Traits>& __a,
	       const istreambuf_iterator<_CharT, _Traits>& __b)
    { return !__a.equal(__b); }

} // std::

#endif /* _CPP_BITS_SBUF_ITER_H */















