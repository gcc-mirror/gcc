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
 *  @file bits/regex_compiler.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

// TODO make comments doxygen format.

// This compiler refers to "Regular Expression Matching Can Be Simple And Fast"
// (http://swtch.com/~rsc/regexp/regexp1.html"),
// but doesn't strictly follow it.
//
// When compiling, states are *chained* instead of tree- or graph-constructed.
// It's more like structured programs: there's if statement and loop statement.
//
// For alternative structure(say "a|b"), aka "if statement", two branchs should
// be constructed. However, these two shall merge to an "end_tag" at the end of
// this operator:
//
//                branch1
//              /        \
// => begin_tag            end_tag =>
//              \        /
//                branch2
//
// This is the difference between this implementation and that in Russ's
// article.
//
// That's why we introduced dummy node here ------ "end_tag" is a dummy node.
// All dummy node will be eliminated at the end of compiling process.

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _Compiler(_FwdIter __b, _FwdIter __e,
	      const _TraitsT& __traits, _FlagT __flags)
    : _M_traits(__traits), _M_scanner(__b, __e, __flags, _M_traits.getloc()),
      _M_ctype(std::use_facet<std::ctype<_CharT>>(_M_traits.getloc())),
      _M_nfa(__flags), _M_flags(__flags)
    {
      _StateSeqT __r(_M_nfa, _M_nfa._M_start());
      __r._M_append(_M_nfa._M_insert_subexpr_begin());
      this->_M_disjunction();
      if (!_M_match_token(_ScannerT::_S_token_eof))
	__throw_regex_error(regex_constants::error_paren);
      __r._M_append(_M_pop());
      _GLIBCXX_DEBUG_ASSERT(_M_stack.empty());
      __r._M_append(_M_nfa._M_insert_subexpr_end());
      __r._M_append(_M_nfa._M_insert_accept());
      _M_nfa._M_eliminate_dummy();
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_disjunction()
    {
      this->_M_alternative();
      // TODO empty alternative like, um, "(|asdf)"
      while (_M_match_token(_ScannerT::_S_token_or))
	{
	  _StateSeqT __alt1 = _M_pop();
	  this->_M_alternative();
	  _StateSeqT __alt2 = _M_pop();
	  auto __end = _M_nfa._M_insert_dummy();
	  __alt1._M_append(__end);
	  __alt2._M_append(__end);
	  _M_stack.push(_StateSeqT(_M_nfa,
				   _M_nfa._M_insert_alt(__alt1._M_start,
							__alt2._M_start, false),
				   __end));
	}
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_alternative()
    {
      if (this->_M_term())
	{
	  _StateSeqT __re = _M_pop();
	  this->_M_alternative();
	  __re._M_append(_M_pop());
	  _M_stack.push(__re);
	}
      else
	_M_stack.push(_StateSeqT(_M_nfa, _M_nfa._M_insert_dummy()));
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_term()
    {
      if (this->_M_assertion())
	return true;
      if (this->_M_atom())
	{
	  this->_M_quantifier();
	  return true;
	}
      return false;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_assertion()
    {
      if (_M_match_token(_ScannerT::_S_token_line_begin))
	_M_stack.push(_StateSeqT(_M_nfa, _M_nfa.
	      _M_insert_line_begin()));
      else if (_M_match_token(_ScannerT::_S_token_line_end))
	_M_stack.push(_StateSeqT(_M_nfa, _M_nfa.
	      _M_insert_line_end()));
      else if (_M_match_token(_ScannerT::_S_token_word_bound))
	// _M_value[0] == 'n' means it's negtive, say "not word boundary".
	_M_stack.push(_StateSeqT(_M_nfa, _M_nfa.
	      _M_insert_word_bound(_M_value[0] == 'n')));
      else if (_M_match_token(_ScannerT::_S_token_subexpr_lookahead_begin))
	{
	  auto __neg = _M_value[0] == 'n';
	  this->_M_disjunction();
	  if (!_M_match_token(_ScannerT::_S_token_subexpr_end))
	    __throw_regex_error(regex_constants::error_paren);
	  auto __tmp = _M_pop();
	  __tmp._M_append(_M_nfa._M_insert_accept());
	  _M_stack.push(
	      _StateSeqT(
		_M_nfa,
		_M_nfa._M_insert_lookahead(__tmp._M_start, __neg)));
	}
      else
	return false;
      return true;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_quantifier()
    {
      bool __neg = regex_constants::ECMAScript;
      auto __init = [this, &__neg]()
	{
	  if (_M_stack.empty())
	    __throw_regex_error(regex_constants::error_badrepeat);
	  __neg = __neg && _M_match_token(_ScannerT::_S_token_opt);
	};
      if (_M_match_token(_ScannerT::_S_token_closure0))
	{
	  __init();
	  auto __e = _M_pop();
	  _StateSeqT __r(_M_nfa, _M_nfa._M_insert_alt(_S_invalid_state_id,
						      __e._M_start, __neg));
	  __e._M_append(__r);
	  _M_stack.push(__r);
	}
      else if (_M_match_token(_ScannerT::_S_token_closure1))
	{
	  __init();
	  auto __e = _M_pop();
	  __e._M_append(_M_nfa._M_insert_alt(_S_invalid_state_id, __e._M_start,
					     __neg));
	  _M_stack.push(__e);
	}
      else if (_M_match_token(_ScannerT::_S_token_opt))
	{
	  __init();
	  auto __e = _M_pop();
	  auto __end = _M_nfa._M_insert_dummy();
	  _StateSeqT __r(_M_nfa, _M_nfa._M_insert_alt(_S_invalid_state_id,
						      __e._M_start, __neg));
	  __e._M_append(__end);
	  __r._M_append(__end);
	  _M_stack.push(__r);
	}
      else if (_M_match_token(_ScannerT::_S_token_interval_begin))
	{
	  __init();
	  if (!_M_match_token(_ScannerT::_S_token_dup_count))
	    __throw_regex_error(regex_constants::error_badbrace);
	  _StateSeqT __r(_M_pop());
	  _StateSeqT __e(_M_nfa, _M_nfa._M_insert_dummy());
	  int __min_rep = _M_cur_int_value(10);
	  // {3
	  for (int __i = 0; __i < __min_rep; ++__i)
	    __e._M_append(__r._M_clone());
	  if (_M_match_token(_ScannerT::_S_token_comma))
	    if (_M_match_token(_ScannerT::_S_token_dup_count)) // {3,7}
	      {
		int __n = _M_cur_int_value(10) - __min_rep;
		if (__n < 0)
		  __throw_regex_error(regex_constants::error_badbrace);
		auto __end = _M_nfa._M_insert_dummy();
		for (int __i = 0; __i < __n; ++__i)
		  {
		    auto __tmp = __r._M_clone();
		    __e._M_append
		      (_StateSeqT(_M_nfa,
				  _M_nfa._M_insert_alt(__tmp._M_start,
						       __end, __neg),
				  __tmp._M_end));
		  }
		__e._M_append(__end);
	      }
	    else // {3,}
	      {
		auto __tmp = __r._M_clone();
		_StateSeqT __s(_M_nfa,
			       _M_nfa._M_insert_alt(_S_invalid_state_id,
						    __tmp._M_start, __neg));
		__tmp._M_append(__s);
		__e._M_append(__s);
	      }
	  if (!_M_match_token(_ScannerT::_S_token_interval_end))
	    __throw_regex_error(regex_constants::error_brace);
	  _M_stack.push(__e);
	}
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_atom()
    {
      if (_M_match_token(_ScannerT::_S_token_anychar))
	_M_stack.push(_StateSeqT(_M_nfa,
				_M_nfa._M_insert_matcher
				(_AnyMatcher<_CharT, _TraitsT>(_M_traits))));
      else if (_M_try_char())
	_M_stack.push(_StateSeqT(_M_nfa,
				 _M_nfa._M_insert_matcher
				 (_CharMatcher<_CharT, _TraitsT>(_M_value[0],
								 _M_traits,
								 _M_flags))));
      else if (_M_match_token(_ScannerT::_S_token_backref))
	_M_stack.push(_StateSeqT(_M_nfa, _M_nfa.
				 _M_insert_backref(_M_cur_int_value(10))));
      else if (_M_match_token(_ScannerT::_S_token_quoted_class))
	{
	  _GLIBCXX_DEBUG_ASSERT(_M_value.size() == 1);
	  _BMatcherT __matcher(_M_ctype.is(_CtypeT::upper, _M_value[0]),
			       _M_traits, _M_flags);
	  __matcher._M_add_character_class(_M_value);
	  _M_stack.push(_StateSeqT(_M_nfa,
		_M_nfa._M_insert_matcher(__matcher)));
	}
      else if (_M_match_token(_ScannerT::_S_token_subexpr_no_group_begin))
	{
	  _StateSeqT __r(_M_nfa, _M_nfa._M_insert_dummy());
	  this->_M_disjunction();
	  if (!_M_match_token(_ScannerT::_S_token_subexpr_end))
	    __throw_regex_error(regex_constants::error_paren);
	  __r._M_append(_M_pop());
	  _M_stack.push(__r);
	}
      else if (_M_match_token(_ScannerT::_S_token_subexpr_begin))
	{
	  int __mark = _M_nfa._M_sub_count();
	  _StateSeqT __r(_M_nfa, _M_nfa._M_insert_subexpr_begin());
	  this->_M_disjunction();
	  if (!_M_match_token(_ScannerT::_S_token_subexpr_end))
	    __throw_regex_error(regex_constants::error_paren);
	  __r._M_append(_M_pop());
	  __r._M_append(_M_nfa._M_insert_subexpr_end());
	  _M_stack.push(__r);
	}
      else if (!_M_bracket_expression())
	return false;
      return true;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_bracket_expression()
    {
      bool __neg =
	_M_match_token(_ScannerT::_S_token_bracket_neg_begin);
      if (!(__neg || _M_match_token(_ScannerT::_S_token_bracket_begin)))
	return false;
      _BMatcherT __matcher(__neg, _M_traits, _M_flags);
      while (!_M_match_token(_ScannerT::_S_token_bracket_end))
	_M_expression_term(__matcher);
      _M_stack.push(_StateSeqT(_M_nfa, _M_nfa._M_insert_matcher(__matcher)));
      return true;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_expression_term(_BMatcherT& __matcher)
    {
      if (_M_match_token(_ScannerT::_S_token_collsymbol))
	__matcher._M_add_collating_element(_M_value);
      else if (_M_match_token(_ScannerT::_S_token_equiv_class_name))
	__matcher._M_add_equivalence_class(_M_value);
      else if (_M_match_token(_ScannerT::_S_token_char_class_name))
	__matcher._M_add_character_class(_M_value);
      else if (_M_try_char()) // [a
	{
	  auto __ch = _M_value[0];
	  if (_M_try_char())
	    {
	      if (_M_value[0] == '-') // [a-
		{
		  if (_M_try_char()) // [a-z]
		    {
		      __matcher._M_make_range(__ch, _M_value[0]);
		      return;
		    }
		  // If the dash is the last character in the bracket
		  // expression, it is not special.
		  if (_M_scanner._M_get_token()
		      != _ScannerT::_S_token_bracket_end)
		    __throw_regex_error(regex_constants::error_range);
		}
	      __matcher._M_add_char(_M_value[0]);
	    }
	  __matcher._M_add_char(__ch);
	}
      else
	__throw_regex_error(regex_constants::error_brack);
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_try_char()
    {
      bool __is_char = false;
      if (_M_match_token(_ScannerT::_S_token_oct_num))
	{
	  __is_char = true;
	  _M_value.assign(1, _M_cur_int_value(8));
	}
      else if (_M_match_token(_ScannerT::_S_token_hex_num))
	{
	  __is_char = true;
	  _M_value.assign(1, _M_cur_int_value(16));
	}
      else if (_M_match_token(_ScannerT::_S_token_ord_char))
	__is_char = true;
      return __is_char;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_match_token(_TokenT token)
    {
      if (token == _M_scanner._M_get_token())
	{
	  _M_value = _M_scanner._M_get_value();
	  _M_scanner._M_advance();
	  return true;
	}
      return false;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    int
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_cur_int_value(int __radix)
    {
      int __v = 0;
      for (typename _StringT::size_type __i = 0;
	   __i < _M_value.length(); ++__i)
	__v =__v * __radix + _M_traits.value(_M_value[__i], __radix);
      return __v;
    }

  template<typename _CharT, typename _TraitsT>
    bool _BracketMatcher<_CharT, _TraitsT>::
    operator()(_CharT __ch) const
    {
      bool __ret = false;
      if (_M_traits.isctype(__ch, _M_class_set))
	__ret = true;
      else if (_M_char_set.count(_M_translate(__ch)))
	__ret = true;
      else
	{
	  _StringT __s = _M_get_str(_M_flags & regex_constants::collate
				    ? _M_translate(__ch) : __ch);
	  for (auto& __it : _M_range_set)
	    if (__it.first <= __s && __s <= __it.second)
	      {
		__ret = true;
		break;
	      }
	}
      if (_M_is_non_matching)
	return !__ret;
      else
	return __ret;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
