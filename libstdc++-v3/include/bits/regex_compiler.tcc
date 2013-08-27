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
      _M_state_store(__flags), _M_flags(__flags)
    {
      _StateSeqT __r(_M_state_store,
		     _M_state_store._M_insert_subexpr_begin());
      _M_disjunction();
      if (!_M_stack.empty())
	{
	  __r._M_append(_M_stack.top());
	  _M_stack.pop();
	}
      __r._M_append(_M_state_store._M_insert_subexpr_end());
      __r._M_append(_M_state_store._M_insert_accept());
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
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_disjunction()
    {
      this->_M_alternative();
      if (_M_match_token(_ScannerT::_S_token_or))
	{
	  _StateSeqT __alt1 = _M_stack.top(); _M_stack.pop();
	  this->_M_disjunction();
	  _StateSeqT __alt2 = _M_stack.top(); _M_stack.pop();
	  _M_stack.push(_StateSeqT(__alt1, __alt2));
	}
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_alternative()
    {
      if (this->_M_term())
	{
	  _StateSeqT __re = _M_stack.top(); _M_stack.pop();
	  this->_M_alternative();
	  if (!_M_stack.empty())
	    {
	      __re._M_append(_M_stack.top());
	      _M_stack.pop();
	    }
	  _M_stack.push(__re);
	}
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

  // TODO Implement it.
  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_assertion()
    {
      return false;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_quantifier()
    {
      if (_M_match_token(_ScannerT::_S_token_closure0))
	{
	  if (_M_stack.empty())
	    __throw_regex_error(regex_constants::error_badrepeat);
	  _StateSeqT __r(_M_stack.top(), -1);
	  __r._M_append(__r._M_front());
	  _M_stack.pop();
	  _M_stack.push(__r);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_closure1))
	{
	  if (_M_stack.empty())
	    __throw_regex_error(regex_constants::error_badrepeat);
	  _StateSeqT __r(_M_state_store,
			_M_state_store.
			_M_insert_alt(_S_invalid_state_id,
				      _M_stack.top()._M_front()));
	  _M_stack.top()._M_append(__r);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_opt))
	{
	  if (_M_stack.empty())
	  __throw_regex_error(regex_constants::error_badrepeat);
	  _StateSeqT __r(_M_stack.top(), -1);
	  _M_stack.pop();
	  _M_stack.push(__r);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_interval_begin))
	{
	  if (_M_stack.empty())
	    __throw_regex_error(regex_constants::error_badrepeat);
	  if (!_M_match_token(_ScannerT::_S_token_dup_count))
	    __throw_regex_error(regex_constants::error_badbrace);
	  _StateSeqT __r(_M_stack.top());
	  int __min_rep = _M_cur_int_value(10);
	  for (int __i = 1; __i < __min_rep; ++__i)
	    _M_stack.top()._M_append(__r._M_clone());
	  if (_M_match_token(_ScannerT::_S_token_comma))
	    if (_M_match_token(_ScannerT::_S_token_dup_count))
	      {
		int __n = _M_cur_int_value(10) - __min_rep;
		if (__n < 0)
		  __throw_regex_error(regex_constants::error_badbrace);
		for (int __i = 0; __i < __n; ++__i)
		  {
		    _StateSeqT __r(_M_state_store,
				  _M_state_store.
				  _M_insert_alt(_S_invalid_state_id,
						_M_stack.top()._M_front()));
		    _M_stack.top()._M_append(__r);
		  }
	      }
	    else
	      {
		_StateSeqT __r(_M_stack.top(), -1);
		__r._M_push_back(__r._M_front());
		_M_stack.pop();
		_M_stack.push(__r);
	      }
	  if (!_M_match_token(_ScannerT::_S_token_interval_end))
	    __throw_regex_error(regex_constants::error_brace);
	  return;
	}
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_atom()
    {
      if (_M_match_token(_ScannerT::_S_token_anychar))
	{
	  const static auto&
	  __any_matcher = [](_CharT __ch) -> bool
	  { return true; };

	  _M_stack.push(_StateSeqT(_M_state_store,
				  _M_state_store._M_insert_matcher
				  (__any_matcher)));
	  return true;
	}
      if (_M_try_char())
	{
	  _CharT __c = _M_value[0];
	  __detail::_Matcher<_CharT> f;
	  if (_M_flags & regex_constants::icase)
	    {
	      auto __traits = this->_M_traits;
	      __c = __traits.translate_nocase(__c);
	      f = [__traits, __c](_CharT __ch) -> bool
	      { return __traits.translate_nocase(__ch) == __c; };
	    }
	  else
	    f = [__c](_CharT __ch) -> bool
	    { return __ch == __c; };

	  _M_stack.push(_StateSeqT(_M_state_store,
				   _M_state_store._M_insert_matcher(f)));
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_backref))
	{
	  _M_stack.push(_StateSeqT(_M_state_store, _M_state_store.
				   _M_insert_backref(_M_cur_int_value(10))));
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_subexpr_begin))
	{
	  int __mark = _M_state_store._M_sub_count();
	  _StateSeqT __r(_M_state_store,
			_M_state_store.
			_M_insert_subexpr_begin());
	  this->_M_disjunction();
	  if (!_M_match_token(_ScannerT::_S_token_subexpr_end))
	    __throw_regex_error(regex_constants::error_paren);
	  if (!_M_stack.empty())
	    {
	      __r._M_append(_M_stack.top());
	      _M_stack.pop();
	    }
	  __r._M_append(_M_state_store._M_insert_subexpr_end());
	  _M_stack.push(__r);
	  return true;
	}
      return _M_bracket_expression();
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
      _M_bracket_list(__matcher);
      _M_stack.push(_StateSeqT(_M_state_store,
			      _M_state_store._M_insert_matcher(__matcher)));
      return true;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_bracket_list(_BMatcherT& __matcher)
    {
      if (_M_match_token(_ScannerT::_S_token_bracket_end))
	return;
      _M_expression_term(__matcher);
      _M_bracket_list(__matcher);
      return;
    }

  template<typename _FwdIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_FwdIter, _CharT, _TraitsT>::
    _M_expression_term(_BMatcherT& __matcher)
    {
      if (_M_match_token(_ScannerT::_S_token_collsymbol))
	{
	  __matcher._M_add_collating_element(_M_value);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_equiv_class_name))
	{
	  __matcher._M_add_equivalence_class(_M_value);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_char_class_name))
	{
	  __matcher._M_add_character_class(_M_value);
	  return;
	}
      if (_M_try_char()) // [a
	{
	  auto __ch = _M_value[0];
	  if (_M_try_char())
	    {
	      if (_M_value[0] == std::use_facet<std::ctype<_CharT>>
		   (_M_traits.getloc()).widen('-')) // [a-
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
	  return;
	}
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
      else
	{
	  __ch = _M_translate(__ch);

	  for (auto __c : _M_char_set)
	    if (__c == __ch)
	      {
		__ret = true;
		break;
	      }
	  if (!__ret)
	    {
	      _StringT __s = _M_get_str(__ch);
	      for (auto& __it : _M_range_set)
		if (__it.first <= __s && __s <= __it.second)
		  {
		    __ret = true;
		    break;
		  }
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
