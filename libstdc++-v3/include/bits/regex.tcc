// class template regex -*- C++ -*-

// Copyright (C) 2013 Free Software Foundation, Inc.
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

/**
 *  @file bits/regex.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Result of merging regex_match and regex_search.
  //
  // __policy now can be _S_auto(auto dispatch by checking back-references)
  // and _S_force_dfs(just use _DFSExecutor).
  //
  // That __match_mode is true means regex_match, else regex_search.
  template<typename _BiIter, typename _Alloc,
	   typename _CharT, typename _TraitsT,
	   _RegexExecutorPolicy __policy,
	   bool __match_mode>
    bool
    __regex_algo_impl(_BiIter                              __s,
		      _BiIter                              __e,
		      match_results<_BiIter, _Alloc>&      __m,
		      const basic_regex<_CharT, _TraitsT>& __re,
		      regex_constants::match_flag_type     __flags)
    {
      if (__re._M_automaton == nullptr)
	return false;

      typename match_results<_BiIter, _Alloc>::_Base_type& __res = __m;
      __res.resize(__re._M_automaton->_M_sub_count() + 2);
      for (auto& __it : __res)
	__it.matched = false;

      typedef std::unique_ptr<_Executor<_BiIter, _Alloc, _CharT, _TraitsT>>
	_ExecutorPtr;
      typedef _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _DFSExecutorT;
      typedef _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _BFSExecutorT;

      _ExecutorPtr __executor =
	__get_executor<_BiIter, _Alloc, _CharT, _TraitsT,
	  __policy>(__s, __e, __res, __re, __flags);

      bool __ret;
      if (__match_mode)
	__ret = __executor->_M_match();
      else
	__ret = __executor->_M_search();
      if (__ret)
	{
	  for (auto __it : __res)
	    if (!__it.matched)
	      __it.first = __it.second = __e;
	  auto& __pre = __res[__res.size()-2];
	  auto& __suf = __res[__res.size()-1];
	  if (__match_mode)
	    {
	      __pre.matched = false;
	      __pre.first = __s;
	      __pre.second = __s;
	      __suf.matched = false;
	      __suf.first = __e;
	      __suf.second = __e;
	    }
	  else
	    {
	      __pre.first = __s;
	      __pre.second = __res[0].first;
	      __pre.matched = (__pre.first != __pre.second);
	      __suf.first = __res[0].second;
	      __suf.second = __e;
	      __suf.matched = (__suf.first != __suf.second);
	    }
	  if (__re.flags() & regex_constants::nosubs)
	    __res.resize(3);
	}
      return __ret;
    }

_GLIBCXX_END_NAMESPACE_VERSION
}

  template<typename _Ch_type>
  template<typename _Fwd_iter>
    typename regex_traits<_Ch_type>::string_type
    regex_traits<_Ch_type>::
    lookup_collatename(_Fwd_iter __first, _Fwd_iter __last) const
    {
      typedef std::ctype<char_type> __ctype_type;
      const __ctype_type& __fctyp(use_facet<__ctype_type>(_M_locale));

      static const char* __collatenames[] =
	{
	  "NUL",
	  "SOH",
	  "STX",
	  "ETX",
	  "EOT",
	  "ENQ",
	  "ACK",
	  "alert",
	  "backspace",
	  "tab",
	  "newline",
	  "vertical-tab",
	  "form-feed",
	  "carriage-return",
	  "SO",
	  "SI",
	  "DLE",
	  "DC1",
	  "DC2",
	  "DC3",
	  "DC4",
	  "NAK",
	  "SYN",
	  "ETB",
	  "CAN",
	  "EM",
	  "SUB",
	  "ESC",
	  "IS4",
	  "IS3",
	  "IS2",
	  "IS1",
	  "space",
	  "exclamation-mark",
	  "quotation-mark",
	  "number-sign",
	  "dollar-sign",
	  "percent-sign",
	  "ampersand",
	  "apostrophe",
	  "left-parenthesis",
	  "right-parenthesis",
	  "asterisk",
	  "plus-sign",
	  "comma",
	  "hyphen",
	  "period",
	  "slash",
	  "zero",
	  "one",
	  "two",
	  "three",
	  "four",
	  "five",
	  "six",
	  "seven",
	  "eight",
	  "nine",
	  "colon",
	  "semicolon",
	  "less-than-sign",
	  "equals-sign",
	  "greater-than-sign",
	  "question-mark",
	  "commercial-at",
	  "A",
	  "B",
	  "C",
	  "D",
	  "E",
	  "F",
	  "G",
	  "H",
	  "I",
	  "J",
	  "K",
	  "L",
	  "M",
	  "N",
	  "O",
	  "P",
	  "Q",
	  "R",
	  "S",
	  "T",
	  "U",
	  "V",
	  "W",
	  "X",
	  "Y",
	  "Z",
	  "left-square-bracket",
	  "backslash",
	  "right-square-bracket",
	  "circumflex",
	  "underscore",
	  "grave-accent",
	  "a",
	  "b",
	  "c",
	  "d",
	  "e",
	  "f",
	  "g",
	  "h",
	  "i",
	  "j",
	  "k",
	  "l",
	  "m",
	  "n",
	  "o",
	  "p",
	  "q",
	  "r",
	  "s",
	  "t",
	  "u",
	  "v",
	  "w",
	  "x",
	  "y",
	  "z",
	  "left-curly-bracket",
	  "vertical-line",
	  "right-curly-bracket",
	  "tilde",
	  "DEL",
	  ""
	};

      // same as boost
      //static const char* __digraphs[] =
      //  {
      //    "ae",
      //    "Ae",
      //    "AE",
      //    "ch",
      //    "Ch",
      //    "CH",
      //    "ll",
      //    "Ll",
      //    "LL",
      //    "ss",
      //    "Ss",
      //    "SS",
      //    "nj",
      //    "Nj",
      //    "NJ",
      //    "dz",
      //    "Dz",
      //    "DZ",
      //    "lj",
      //    "Lj",
      //    "LJ",
      //    ""
      //  };

      std::string __s(__last - __first, '?');
      __fctyp.narrow(__first, __last, '?', &*__s.begin());

      for (unsigned int __i = 0; *__collatenames[__i]; __i++)
	if (__s == __collatenames[__i])
	  return string_type(1, __fctyp.widen(static_cast<char>(__i)));

      //for (unsigned int __i = 0; *__digraphs[__i]; __i++)
      //  {
      //    const char* __now = __digraphs[__i];
      //    if (__s == __now)
      //      {
      //	string_type ret(__s.size(), __fctyp.widen('?'));
      //	__fctyp.widen(__now, __now + 2/* ouch */, &*ret.begin());
      //	return ret;
      //      }
      //  }
      return string_type();
    }

  template<typename _Ch_type>
  template<typename _Fwd_iter>
    typename regex_traits<_Ch_type>::char_class_type
    regex_traits<_Ch_type>::
    lookup_classname(_Fwd_iter __first, _Fwd_iter __last, bool __icase) const
    {
      typedef std::ctype<char_type> __ctype_type;
      typedef std::ctype<char> __cctype_type;
      typedef const pair<const char*, char_class_type> _ClassnameEntry;
      const __ctype_type& __fctyp(use_facet<__ctype_type>(_M_locale));
      const __cctype_type& __cctyp(use_facet<__cctype_type>(_M_locale));

      static _ClassnameEntry __classnames[] =
      {
	{"d", ctype_base::digit},
	{"w", {ctype_base::alnum, _RegexMask::_S_under}},
	{"s", ctype_base::space},
	{"alnum", ctype_base::alnum},
	{"alpha", ctype_base::alpha},
	{"blank", {0, _RegexMask::_S_blank}},
	{"cntrl", ctype_base::cntrl},
	{"digit", ctype_base::digit},
	{"graph", ctype_base::graph},
	{"lower", ctype_base::lower},
	{"print", ctype_base::print},
	{"punct", ctype_base::punct},
	{"space", ctype_base::space},
	{"upper", ctype_base::upper},
	{"xdigit", ctype_base::xdigit},
      };

      std::string __s(__last - __first, '?');
      __fctyp.narrow(__first, __last, '?', &__s[0]);
      __cctyp.tolower(&*__s.begin(), &*__s.begin() + __s.size());
      for (_ClassnameEntry* __it = __classnames;
	   __it < *(&__classnames + 1);
	   ++__it)
	{
	  if (__s == __it->first)
	    {
	      if (__icase
		  && ((__it->second
		       & (ctype_base::lower | ctype_base::upper)) != 0))
		return ctype_base::alpha;
	      return __it->second;
	    }
	}
      return 0;
    }

  template<typename _Ch_type>
    bool
    regex_traits<_Ch_type>::
    isctype(_Ch_type __c, char_class_type __f) const
    {
      typedef std::ctype<char_type> __ctype_type;
      const __ctype_type& __fctyp(use_facet<__ctype_type>(_M_locale));

      return __fctyp.is(__f._M_base, __c)
	// [[:w:]]
	|| ((__f._M_extended & _RegexMask::_S_under)
	    && __c == __fctyp.widen('_'))
	// [[:blank:]]
	|| ((__f._M_extended & _RegexMask::_S_blank)
	    && (__c == __fctyp.widen(' ')
		|| __c == __fctyp.widen('\t')));
    }

  template<typename _Ch_type>
    int
    regex_traits<_Ch_type>::
    value(_Ch_type __ch, int __radix) const
    {
      std::basic_istringstream<char_type> __is(string_type(1, __ch));
      long __v;
      if (__radix == 8)
	__is >> std::oct;
      else if (__radix == 16)
	__is >> std::hex;
      __is >> __v;
      return __is.fail() ? -1 : __v;
    }

  template<typename _Bi_iter, typename _Alloc>
  template<typename _Out_iter>
    _Out_iter match_results<_Bi_iter, _Alloc>::
    format(_Out_iter __out,
	   const match_results<_Bi_iter, _Alloc>::char_type* __fmt_first,
	   const match_results<_Bi_iter, _Alloc>::char_type* __fmt_last,
	   match_flag_type __flags) const
    {
      _GLIBCXX_DEBUG_ASSERT( ready() );
      regex_traits<char_type> __traits;
      typedef std::ctype<char_type> __ctype_type;
      const __ctype_type&
	__fctyp(use_facet<__ctype_type>(__traits.getloc()));

      auto __output = [&](size_t __idx)
	{
	  auto& __sub = _Base_type::operator[](__idx);
	  if (__sub.matched)
	    std::copy(__sub.first, __sub.second, __out);
	};

      if (__flags & regex_constants::format_sed)
	{
	  for (; __fmt_first != __fmt_last;)
	    if (*__fmt_first == '&')
	      {
		__output(0);
		++__fmt_first;
	      }
	    else if (*__fmt_first == '\\')
	      {
		if (++__fmt_first != __fmt_last
		    && __fctyp.is(__ctype_type::digit, *__fmt_first))
		  __output(__traits.value(*__fmt_first++, 10));
		else
		  *__out++ = '\\';
	      }
	    else
	      *__out++ = *__fmt_first++;
	}
      else
	{
	  while (1)
	    {
	      auto __next = std::find(__fmt_first, __fmt_last, '$');
	      if (__next == __fmt_last)
		break;

	      std::copy(__fmt_first, __next, __out);

	      auto __eat = [&](char __ch) -> bool
		{
		  if (*__next == __ch)
		    {
		      ++__next;
		      return true;
		    }
		  return false;
		};

	      if (++__next == __fmt_last)
		*__out++ = '$';
	      else if (__eat('$'))
		*__out++ = '$';
	      else if (__eat('&'))
		__output(0);
	      else if (__eat('`'))
		__output(_Base_type::size()-2);
	      else if (__eat('\''))
		__output(_Base_type::size()-1);
	      else if (__fctyp.is(__ctype_type::digit, *__next))
		{
		  long __num = __traits.value(*__next, 10);
		  if (++__next != __fmt_last
		      && __fctyp.is(__ctype_type::digit, *__next))
		    {
		      __num *= 10;
		      __num += __traits.value(*__next++, 10);
		    }
		  if (0 <= __num && __num < this->size())
		    __output(__num);
		}
	      else
		*__out++ = '$';
	      __fmt_first = __next;
	    }
	  std::copy(__fmt_first, __fmt_last, __out);
	}
      return __out;
    }

  template<typename _Out_iter, typename _Bi_iter,
	   typename _Rx_traits, typename _Ch_type>
    _Out_iter
    regex_replace(_Out_iter __out, _Bi_iter __first, _Bi_iter __last,
		  const basic_regex<_Ch_type, _Rx_traits>& __e,
		  const _Ch_type* __fmt,
		  regex_constants::match_flag_type __flags)
    {
      typedef regex_iterator<_Bi_iter, _Ch_type, _Rx_traits> _IterT;
      _IterT __i(__first, __last, __e, __flags);
      _IterT __end;
      if (__i == __end)
	{
	  if (!(__flags & regex_constants::format_no_copy))
	    std::copy(__first, __last, __out);
	}
      else
	{
	  sub_match<_Bi_iter> __last;
	  auto __len = char_traits<_Ch_type>::length(__fmt);
	  for (; __i != __end; ++__i)
	    {
	      if (!(__flags & regex_constants::format_no_copy))
		std::copy(__i->prefix().first, __i->prefix().second, __out);
	      __out = __i->format(__out, __fmt, __fmt + __len, __flags);
	      __last = __i->suffix();
	      if (__flags & regex_constants::format_first_only)
		break;
	    }
	  if (!(__flags & regex_constants::format_no_copy))
	    std::copy(__last.first, __last.second, __out);
	}
      return __out;
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    bool
    regex_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    operator==(const regex_iterator& __rhs) const
    {
      return (_M_match.empty() && __rhs._M_match.empty())
	|| (_M_begin == __rhs._M_begin
	    && _M_end == __rhs._M_end
	    && _M_pregex == __rhs._M_pregex
	    && _M_flags == __rhs._M_flags
	    && _M_match[0] == __rhs._M_match[0]);
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    regex_iterator<_Bi_iter, _Ch_type, _Rx_traits>&
    regex_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    operator++()
    {
      // In all cases in which the call to regex_search returns true,
      // match.prefix().first shall be equal to the previous value of
      // match[0].second, and for each index i in the half-open range
      // [0, match.size()) for which match[i].matched is true,
      // match[i].position() shall return distance(begin, match[i].first).
      // [28.12.1.4.5]
      if (_M_match[0].matched)
	{
	  auto __start = _M_match[0].second;
	  auto __prefix_first = _M_match[0].second;
	  if (_M_match[0].first == _M_match[0].second)
	    if (__start == _M_end)
	      {
		_M_match = value_type();
		return *this;
	      }
	    else
	      {
		if (regex_search(__start, _M_end, _M_match, *_M_pregex, _M_flags
				 | regex_constants::match_not_null
				 | regex_constants::match_continuous))
		  {
		    _GLIBCXX_DEBUG_ASSERT(_M_match[0].matched);
		    _M_match.at(_M_match.size()).first = __prefix_first;
		    _M_match._M_in_iterator = true;
		    _M_match._M_begin = _M_begin;
		    return *this;
		  }
		else
		  ++__start;
	      }
	  _M_flags |= regex_constants::match_prev_avail;
	  if (regex_search(__start, _M_end, _M_match, *_M_pregex, _M_flags))
	    {
	      _GLIBCXX_DEBUG_ASSERT(_M_match[0].matched);
	      _M_match.at(_M_match.size()).first = __prefix_first;
	      _M_match._M_in_iterator = true;
	      _M_match._M_begin = _M_begin;
	    }
	  else
	    _M_match = value_type();
	}
      return *this;
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>&
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    operator=(const regex_token_iterator& __rhs)
    {
      _M_position = __rhs._M_position;
      _M_subs = __rhs._M_subs;
      _M_n = __rhs._M_n;
      _M_result = __rhs._M_result;
      _M_suffix = __rhs._M_suffix;
      _M_has_m1 = __rhs._M_has_m1;
      if (__rhs._M_result == &__rhs._M_suffix)
	_M_result = &_M_suffix;
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    bool
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    operator==(const regex_token_iterator& __rhs) const
    {
      if (_M_end_of_seq() && __rhs._M_end_of_seq())
	return true;
      if (_M_suffix.matched && __rhs._M_suffix.matched
	  && _M_suffix == __rhs._M_suffix)
	return true;
      if (_M_end_of_seq() || _M_suffix.matched
	  || __rhs._M_end_of_seq() || __rhs._M_suffix.matched)
	return false;
      return _M_position == __rhs._M_position
	&& _M_n == __rhs._M_n
	&& _M_subs == __rhs._M_subs;
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>&
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    operator++()
    {
      _Position __prev = _M_position;
      if (_M_suffix.matched)
	*this = regex_token_iterator();
      else if (_M_n + 1 < _M_subs.size())
	{
	  _M_n++;
	  _M_result = &_M_current_match();
	}
      else
	{
	  _M_n = 0;
	  ++_M_position;
	  if (_M_position != _Position())
	    _M_result = &_M_current_match();
	  else if (_M_has_m1 && __prev->suffix().length() != 0)
	    {
	      _M_suffix.matched = true;
	      _M_suffix.first = __prev->suffix().first;
	      _M_suffix.second = __prev->suffix().second;
	      _M_result = &_M_suffix;
	    }
	  else
	    *this = regex_token_iterator();
	}
      return *this;
    }

  template<typename _Bi_iter,
	   typename _Ch_type,
	   typename _Rx_traits>
    void
    regex_token_iterator<_Bi_iter, _Ch_type, _Rx_traits>::
    _M_init(_Bi_iter __a, _Bi_iter __b)
    {
      _M_has_m1 = false;
      for (auto __it : _M_subs)
	if (__it == -1)
	  {
	    _M_has_m1 = true;
	    break;
	  }
      if (_M_position != _Position())
	_M_result = &_M_current_match();
      else if (_M_has_m1)
	{
	  _M_suffix.matched = true;
	  _M_suffix.first = __a;
	  _M_suffix.second = __b;
	  _M_result = &_M_suffix;
	}
      else
	_M_result = nullptr;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

