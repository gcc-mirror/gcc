// class template regex -*- C++ -*-

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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
 *  @file bits/regex_compiler.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @addtogroup regex-detail
   * @{
   */

  template<typename, bool, bool>
    struct _BracketMatcher;

  /**
   * @brief Builds an NFA from an input iterator interval.
   *
   * The %_TraitsT type should fulfill requirements [28.3].
   */
  template<typename _TraitsT>
    class _Compiler
    {
    public:
      typedef typename _TraitsT::char_type        _CharT;
      typedef const _CharT*                       _IterT;
      typedef _NFA<_TraitsT>              	  _RegexT;
      typedef regex_constants::syntax_option_type _FlagT;

      _Compiler(_IterT __b, _IterT __e,
		const _TraitsT& __traits, _FlagT __flags);

      std::shared_ptr<_RegexT>
      _M_get_nfa()
      { return make_shared<_RegexT>(std::move(_M_nfa)); }

    private:
      typedef _Scanner<_CharT>               _ScannerT;
      typedef typename _TraitsT::string_type _StringT;
      typedef typename _ScannerT::_TokenT    _TokenT;
      typedef _StateSeq<_TraitsT>            _StateSeqT;
      typedef std::stack<_StateSeqT>         _StackT;
      typedef std::ctype<_CharT>             _CtypeT;

      // accepts a specific token or returns false.
      bool
      _M_match_token(_TokenT __token);

      void
      _M_disjunction();

      void
      _M_alternative();

      bool
      _M_term();

      bool
      _M_assertion();

      bool
      _M_quantifier();

      bool
      _M_atom();

      bool
      _M_bracket_expression();

      template<bool __icase, bool __collate>
	void
	_M_insert_any_matcher_ecma();

      template<bool __icase, bool __collate>
	void
	_M_insert_any_matcher_posix();

      template<bool __icase, bool __collate>
	void
	_M_insert_char_matcher();

      template<bool __icase, bool __collate>
	void
	_M_insert_character_class_matcher();

      template<bool __icase, bool __collate>
	void
	_M_insert_bracket_matcher(bool __neg);

      template<bool __icase, bool __collate>
	void
	_M_expression_term(_BracketMatcher<_TraitsT, __icase, __collate>&
			   __matcher);

      int
      _M_cur_int_value(int __radix);

      bool
      _M_try_char();

      _StateSeqT
      _M_pop()
      {
	auto ret = _M_stack.top();
	_M_stack.pop();
	return ret;
      }

      _FlagT          _M_flags;
      const _TraitsT& _M_traits;
      const _CtypeT&  _M_ctype;
      _ScannerT       _M_scanner;
      _RegexT         _M_nfa;
      _StringT        _M_value;
      _StackT         _M_stack;
    };

  template<typename _TraitsT>
    inline std::shared_ptr<_NFA<_TraitsT>>
    __compile_nfa(const typename _TraitsT::char_type* __first,
		  const typename _TraitsT::char_type* __last,
		  const _TraitsT& __traits,
		  regex_constants::syntax_option_type __flags)
    {
      using _Cmplr = _Compiler<_TraitsT>;
      return _Cmplr(__first, __last, __traits, __flags)._M_get_nfa();
    }

  // [28.13.14]
  template<typename _TraitsT, bool __icase, bool __collate>
    class _RegexTranslator
    {
    public:
      typedef typename _TraitsT::char_type	      _CharT;
      typedef typename _TraitsT::string_type	      _StringT;
      typedef typename std::conditional<__collate,
					_StringT,
					_CharT>::type _StrTransT;

      explicit
      _RegexTranslator(const _TraitsT& __traits)
      : _M_traits(__traits)
      { }

      _CharT
      _M_translate(_CharT __ch) const
      {
	if (__icase)
	  return _M_traits.translate_nocase(__ch);
	else if (__collate)
	  return _M_traits.translate(__ch);
	else
	  return __ch;
      }

      _StrTransT
      _M_transform(_CharT __ch) const
      {
	return _M_transform_impl(__ch, typename integral_constant<bool,
				 __collate>::type());
      }

    private:
      _StrTransT
      _M_transform_impl(_CharT __ch, false_type) const
      { return __ch; }

      _StrTransT
      _M_transform_impl(_CharT __ch, true_type) const
      {
	_StrTransT __str = _StrTransT(1, _M_translate(__ch));
	return _M_traits.transform(__str.begin(), __str.end());
      }

      const _TraitsT& _M_traits;
    };

  template<typename _TraitsT>
    class _RegexTranslator<_TraitsT, false, false>
    {
    public:
      typedef typename _TraitsT::char_type _CharT;
      typedef _CharT                       _StrTransT;

      explicit
      _RegexTranslator(const _TraitsT& __traits)
      { }

      _CharT
      _M_translate(_CharT __ch) const
      { return __ch; }

      _StrTransT
      _M_transform(_CharT __ch) const
      { return __ch; }
    };

  template<typename _TraitsT, bool __is_ecma, bool __icase, bool __collate>
    struct _AnyMatcher;

  template<typename _TraitsT, bool __icase, bool __collate>
    struct _AnyMatcher<_TraitsT, false, __icase, __collate>
    {
      typedef _RegexTranslator<_TraitsT, __icase, __collate> _TransT;
      typedef typename _TransT::_CharT                       _CharT;

      explicit
      _AnyMatcher(const _TraitsT& __traits)
      : _M_translator(__traits)
      { }

      bool
      operator()(_CharT __ch) const
      {
	static auto __nul = _M_translator._M_translate('\0');
	return _M_translator._M_translate(__ch) != __nul;
      }

      _TransT _M_translator;
    };

  template<typename _TraitsT, bool __icase, bool __collate>
    struct _AnyMatcher<_TraitsT, true, __icase, __collate>
    {
      typedef _RegexTranslator<_TraitsT, __icase, __collate> _TransT;
      typedef typename _TransT::_CharT                       _CharT;

      explicit
      _AnyMatcher(const _TraitsT& __traits)
      : _M_translator(__traits)
      { }

      bool
      operator()(_CharT __ch) const
      { return _M_apply(__ch, typename is_same<_CharT, char>::type()); }

      bool
      _M_apply(_CharT __ch, true_type) const
      {
	auto __c = _M_translator._M_translate(__ch);
	auto __n = _M_translator._M_translate('\n');
	auto __r = _M_translator._M_translate('\r');
	return __c != __n && __c != __r;
      }

      bool
      _M_apply(_CharT __ch, false_type) const
      {
	auto __c = _M_translator._M_translate(__ch);
	auto __n = _M_translator._M_translate('\n');
	auto __r = _M_translator._M_translate('\r');
	auto __u2028 = _M_translator._M_translate(u'\u2028');
	auto __u2029 = _M_translator._M_translate(u'\u2029');
	return __c != __n && __c != __r && __c != __u2028 && __c != __u2029;
      }

      _TransT _M_translator;
    };

  template<typename _TraitsT, bool __icase, bool __collate>
    struct _CharMatcher
    {
      typedef _RegexTranslator<_TraitsT, __icase, __collate> _TransT;
      typedef typename _TransT::_CharT                       _CharT;

      _CharMatcher(_CharT __ch, const _TraitsT& __traits)
      : _M_translator(__traits), _M_ch(_M_translator._M_translate(__ch))
      { }

      bool
      operator()(_CharT __ch) const
      { return _M_ch == _M_translator._M_translate(__ch); }

      _TransT _M_translator;
      _CharT  _M_ch;
    };

  /// Matches a character range (bracket expression)
  template<typename _TraitsT, bool __icase, bool __collate>
    struct _BracketMatcher
    {
    public:
      typedef _RegexTranslator<_TraitsT, __icase, __collate> _TransT;
      typedef typename _TransT::_CharT                       _CharT;
      typedef typename _TransT::_StrTransT                   _StrTransT;
      typedef typename _TraitsT::string_type                 _StringT;
      typedef typename _TraitsT::char_class_type             _CharClassT;

    public:
      _BracketMatcher(bool __is_non_matching,
		      const _TraitsT& __traits)
      : _M_class_set(0), _M_translator(__traits), _M_traits(__traits),
      _M_is_non_matching(__is_non_matching)
#ifdef _GLIBCXX_DEBUG
      , _M_is_ready(false)
#endif
      { }

      bool
      operator()(_CharT __ch) const
      {
	_GLIBCXX_DEBUG_ASSERT(_M_is_ready);
	return _M_apply(__ch, _IsChar());
      }

      void
      _M_add_char(_CharT __c)
      {
	_M_char_set.push_back(_M_translator._M_translate(__c));
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = false;
#endif
      }

      void
      _M_add_collating_element(const _StringT& __s)
      {
	auto __st = _M_traits.lookup_collatename(__s.data(),
						 __s.data() + __s.size());
	if (__st.empty())
	  __throw_regex_error(regex_constants::error_collate);
	_M_char_set.push_back(_M_translator._M_translate(__st[0]));
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = false;
#endif
      }

      void
      _M_add_equivalence_class(const _StringT& __s)
      {
	auto __st = _M_traits.lookup_collatename(__s.data(),
						 __s.data() + __s.size());
	if (__st.empty())
	  __throw_regex_error(regex_constants::error_collate);
	__st = _M_traits.transform_primary(__st.data(),
					   __st.data() + __st.size());
	_M_equiv_set.push_back(__st);
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = false;
#endif
      }

      void
      _M_add_character_class(const _StringT& __s)
      {
	auto __mask = _M_traits.lookup_classname(__s.data(),
						 __s.data() + __s.size(),
						 __icase);
	if (__mask == 0)
	  __throw_regex_error(regex_constants::error_ctype);
	_M_class_set |= __mask;
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = false;
#endif
      }

      void
      _M_make_range(_CharT __l, _CharT __r)
      {
	_M_range_set.push_back(make_pair(_M_translator._M_transform(__l),
				      _M_translator._M_transform(__r)));
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = false;
#endif
      }

      void
      _M_ready()
      {
	_M_make_cache(_IsChar());
#ifdef _GLIBCXX_DEBUG
	_M_is_ready = true;
#endif
      }

    private:
      typedef typename is_same<_CharT, char>::type _IsChar;
      struct _Dummy { };
      typedef typename conditional<_IsChar::value,
				   std::bitset<1 << (8 * sizeof(_CharT))>,
				   _Dummy>::type _CacheT;
      typedef typename make_unsigned<_CharT>::type _UnsignedCharT;

    private:
      bool
      _M_apply(_CharT __ch, false_type) const;

      bool
      _M_apply(_CharT __ch, true_type) const
      { return _M_cache[static_cast<_UnsignedCharT>(__ch)]; }

      void
      _M_make_cache(true_type)
      {
	for (int __i = 0; __i < _M_cache.size(); __i++)
	  _M_cache[static_cast<_UnsignedCharT>(__i)] =
	    _M_apply(__i, false_type());
      }

      void
      _M_make_cache(false_type)
      { }

    private:
      _CacheT                                   _M_cache;
      std::vector<_CharT>                       _M_char_set;
      std::vector<_StringT>                     _M_equiv_set;
      std::vector<pair<_StrTransT, _StrTransT>> _M_range_set;
      _CharClassT                               _M_class_set;
      _TransT                                   _M_translator;
      const _TraitsT&                           _M_traits;
      bool                                      _M_is_non_matching;
#ifdef _GLIBCXX_DEBUG
      bool                                      _M_is_ready;
#endif
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_compiler.tcc>
