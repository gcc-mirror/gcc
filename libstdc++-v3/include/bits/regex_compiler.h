// class template regex -*- C++ -*-

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

  /// Matches a character range (bracket expression)
  template<typename _CharT, typename _TraitsT>
    struct _BracketMatcher
    {
      typedef typename _TraitsT::char_class_type  _CharClassT;
      typedef typename _TraitsT::string_type      _StringT;
      typedef regex_constants::syntax_option_type _FlagT;

      explicit
      _BracketMatcher(bool __is_non_matching,
		      const _TraitsT& __t,
		      _FlagT __flags)
      : _M_is_non_matching(__is_non_matching), _M_traits(__t),
	_M_flags(__flags), _M_class_set(0)
      { }

      bool
      operator()(_CharT) const;

      void
      _M_add_char(_CharT __c)
      {
	if (_M_flags & regex_constants::collate)
	  if (_M_is_icase())
	    _M_char_set.push_back(_M_traits.translate_nocase(__c));
	  else
	    _M_char_set.push_back(_M_traits.translate(__c));
	else
	  _M_char_set.push_back(__c);
      }

      void
      _M_add_collating_element(const _StringT& __s)
      {
	auto __st = _M_traits.lookup_collatename(&*__s.begin(), &*__s.end());
	if (__st.empty())
	  __throw_regex_error(regex_constants::error_collate);
	// TODO: digraph
	_M_char_set.push_back(__st[0]);
      }

      void
      _M_add_equivalence_class(const _StringT& __s)
      {
	_M_add_character_class(
	  _M_traits.transform_primary(&*__s.begin(), &*__s.end()));
      }

      void
      _M_add_character_class(const _StringT& __s)
      {
	auto __st = _M_traits.
	  lookup_classname(&*__s.begin(), &*__s.end(), _M_is_icase());
	if (__st == 0)
	  __throw_regex_error(regex_constants::error_ctype);
	_M_class_set |= __st;
      }

      void
      _M_make_range(_CharT __l, _CharT __r)
      { _M_range_set.push_back(make_pair(_M_get_str(__l), _M_get_str(__r))); }

      bool
      _M_is_icase() const
      { return _M_flags & regex_constants::icase; }

      _StringT
      _M_get_str(_CharT __c) const
      {
	auto __s = _StringT(1,
			    _M_is_icase()
			    ? _M_traits.translate_nocase(__c)
			    : _M_traits.translate(__c));
	return _M_traits.transform(__s.begin(), __s.end());
      }

      _TraitsT                              _M_traits;
      _FlagT                                _M_flags;
      bool                                  _M_is_non_matching;
      std::vector<_CharT>                   _M_char_set;
      std::vector<pair<_StringT, _StringT>> _M_range_set;
      _CharClassT                           _M_class_set;
    };

  /**
   * @brief struct _Scanner. Scans an input range for regex tokens.
   *
   * The %_Scanner class interprets the regular expression pattern in
   * the input range passed to its constructor as a sequence of parse
   * tokens passed to the regular expression compiler.  The sequence
   * of tokens provided depends on the flag settings passed to the
   * constructor: different regular expression grammars will interpret
   * the same input pattern in syntactically different ways.
   */
  template<typename _InputIter>
    class _Scanner
    {
    public:
      typedef unsigned int                                          _StateT;
      typedef typename std::iterator_traits<_InputIter>::value_type _CharT;
      typedef std::basic_string<_CharT>                             _StringT;
      typedef regex_constants::syntax_option_type                   _FlagT;
      typedef const std::ctype<_CharT>                              _CtypeT;

      /// Token types returned from the scanner.
      enum _TokenT
      {
	_S_token_anychar,
	_S_token_backref,
	_S_token_bracket_begin,
	_S_token_bracket_inverse_begin,
	_S_token_bracket_end,
	_S_token_char_class_name,
	_S_token_closure0,
	_S_token_closure1,
	_S_token_collelem_multi,
	_S_token_collelem_single,
	_S_token_collsymbol,
	_S_token_comma,
	_S_token_dash,
	_S_token_dup_count,
	_S_token_eof,
	_S_token_equiv_class_name,
	_S_token_interval_begin,
	_S_token_interval_end,
	_S_token_line_begin,
	_S_token_line_end,
	_S_token_opt,
	_S_token_or,
	_S_token_ord_char,
	_S_token_subexpr_begin,
	_S_token_subexpr_end,
	_S_token_word_begin,
	_S_token_word_end,
	_S_token_unknown
      };

      _Scanner(_InputIter __begin, _InputIter __end,
	       _FlagT __flags, std::locale __loc)
      : _M_current(__begin) , _M_end(__end) , _M_flags(__flags),
	_M_ctype(std::use_facet<_CtypeT>(__loc)), _M_state(0)
      { _M_advance(); }

      void
      _M_advance();

      _TokenT
      _M_token() const
      { return _M_curToken; }

      const _StringT&
      _M_value() const
      { return _M_curValue; }

#ifdef _GLIBCXX_DEBUG
      std::ostream&
      _M_print(std::ostream&);
#endif

    private:
      void
      _M_eat_escape();

      void
      _M_scan_in_brace();

      void
      _M_scan_in_bracket();

      void
      _M_eat_charclass();

      void
      _M_eat_equivclass();

      void
      _M_eat_collsymbol();

      static constexpr _StateT _S_state_in_brace    = 1 << 0;
      static constexpr _StateT _S_state_in_bracket  = 1 << 1;
      _InputIter  _M_current;
      _InputIter  _M_end;
      _FlagT      _M_flags;
      _CtypeT&    _M_ctype;
      _TokenT     _M_curToken;
      _StringT    _M_curValue;
      _StateT     _M_state;
    };

  /// Builds an NFA from an input iterator interval.
  template<typename _InputIter, typename _CharT, typename _TraitsT>
    class _Compiler
    {
    public:
      typedef typename _TraitsT::string_type      _StringT;
      typedef _NFA<_CharT, _TraitsT>              _RegexT;
      typedef regex_constants::syntax_option_type _FlagT;

      _Compiler(_InputIter __b, _InputIter __e,
		const _TraitsT& __traits, _FlagT __flags);

      std::shared_ptr<_RegexT>
      _M_get_nfa() const
      { return std::shared_ptr<_RegexT>(new _RegexT(_M_state_store)); }

    private:
      typedef _Scanner<_InputIter>                            _ScannerT;
      typedef typename _ScannerT::_TokenT                     _TokenT;
      typedef _StateSeq<_CharT, _TraitsT>                     _StateSeqT;
      typedef std::stack<_StateSeqT, std::vector<_StateSeqT>> _StackT;
      typedef _BracketMatcher<_CharT, _TraitsT>               _BMatcherT;

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

      void
      _M_quantifier();

      bool
      _M_atom();

      bool
      _M_bracket_expression();

      bool
      _M_bracket_list(_BMatcherT& __matcher);

      bool
      _M_follow_list(_BMatcherT& __matcher);

      void
      _M_expression_term(_BMatcherT& __matcher);

      bool
      _M_range_expression(_BMatcherT& __matcher);

      bool
      _M_start_range(_BMatcherT& __matcher);

      bool
      _M_collating_symbol(_BMatcherT& __matcher);

      bool
      _M_equivalence_class(_BMatcherT& __matcher);

      bool
      _M_character_class(_BMatcherT& __matcher);

      int
      _M_cur_int_value(int __radix);

      const _TraitsT& _M_traits;
      _ScannerT       _M_scanner;
      _StringT        _M_cur_value;
      _RegexT         _M_state_store;
      _StackT         _M_stack;
      _FlagT          _M_flags;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_compiler.tcc>
