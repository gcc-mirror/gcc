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

  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_advance()
    {
      if (_M_current == _M_end)
	{
	  _M_curToken = _S_token_eof;
	  return;
	}

      _CharT __c = *_M_current;
      if (_M_state & _S_state_in_bracket)
	{
	  _M_scan_in_bracket();
	  return;
	}
      if (_M_state & _S_state_in_brace)
	{
	  _M_scan_in_brace();
	  return;
	}
#if 0
      // TODO: re-enable line anchors when _M_assertion is implemented.
      // See PR libstdc++/47724
      else if (_M_state & _S_state_at_start && __c == _M_ctype.widen('^'))
	{
	  _M_curToken = _S_token_line_begin;
	  ++_M_current;
	  return;
	}
      else if (__c == _M_ctype.widen('$'))
	{
	  _M_curToken = _S_token_line_end;
	  ++_M_current;
	  return;
	}
#endif
      else if (__c == _M_ctype.widen('.'))
	{
	  _M_curToken = _S_token_anychar;
	  ++_M_current;
	  return;
	}
      else if (__c == _M_ctype.widen('*'))
	{
	  _M_curToken = _S_token_closure0;
	  ++_M_current;
	  return;
	}
      else if (__c == _M_ctype.widen('+'))
	{
	  _M_curToken = _S_token_closure1;
	  ++_M_current;
	  return;
	}
      else if (__c == _M_ctype.widen('|'))
	{
	  _M_curToken = _S_token_or;
	  ++_M_current;
	  return;
	}
      else if (__c == _M_ctype.widen('['))
	{
	  if (*++_M_current == _M_ctype.widen('^'))
	    {
	      _M_curToken = _S_token_bracket_inverse_begin;
	      ++_M_current;
	    }
	  else
	    _M_curToken = _S_token_bracket_begin;
	  _M_state |= _S_state_in_bracket;
	  return;
	}
      else if (__c == _M_ctype.widen('\\'))
	{
	  _M_eat_escape();
	  return;
	}
      else if (!(_M_flags & (regex_constants::basic | regex_constants::grep)))
	{
	  if (__c == _M_ctype.widen('('))
	    {
	      _M_curToken = _S_token_subexpr_begin;
	      ++_M_current;
	      return;
	    }
	  else if (__c == _M_ctype.widen(')'))
	    {
	      _M_curToken = _S_token_subexpr_end;
	      ++_M_current;
	      return;
	    }
	  else if (__c == _M_ctype.widen('{'))
	    {
	      _M_curToken = _S_token_interval_begin;
	      _M_state |= _S_state_in_brace;
	      ++_M_current;
	      return;
	    }
	}

      _M_curToken = _S_token_ord_char;
      _M_curValue.assign(1, __c);
      ++_M_current;
    }

  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_scan_in_brace()
    {
      if (_M_ctype.is(_CtypeT::digit, *_M_current))
	{
	  _M_curToken = _S_token_dup_count;
	  _M_curValue.assign(1, *_M_current);
	  ++_M_current;
	  while (_M_current != _M_end
		 && _M_ctype.is(_CtypeT::digit, *_M_current))
	    {
	      _M_curValue += *_M_current;
	      ++_M_current;
	    }
	  return;
	}
      else if (*_M_current == _M_ctype.widen(','))
	{
	  _M_curToken = _S_token_comma;
	  ++_M_current;
	  return;
	}
      if (_M_flags & (regex_constants::basic | regex_constants::grep))
	{
	  if (*_M_current == _M_ctype.widen('\\'))
	    _M_eat_escape();
	}
      else
	{
	  if (*_M_current == _M_ctype.widen('}'))
	    {
	      _M_curToken = _S_token_interval_end;
	      _M_state &= ~_S_state_in_brace;
	      ++_M_current;
	      return;
	    }
	}
    }

  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_scan_in_bracket()
    {
      if (*_M_current == _M_ctype.widen('['))
	{
	  ++_M_current;
	  if (_M_current == _M_end)
	    {
	      _M_curToken = _S_token_eof;
	      return;
	    }

	  if (*_M_current == _M_ctype.widen('.'))
	    {
	      _M_curToken = _S_token_collsymbol;
	      _M_eat_collsymbol();
	      return;
	    }
	  else if (*_M_current == _M_ctype.widen(':'))
	    {
	      _M_curToken = _S_token_char_class_name;
	      _M_eat_charclass();
	      return;
	    }
	  else if (*_M_current == _M_ctype.widen('='))
	    {
	      _M_curToken = _S_token_equiv_class_name;
	      _M_eat_equivclass();
	      return;
	    }
	}
      else if (*_M_current == _M_ctype.widen('-'))
	{
	  _M_curToken = _S_token_dash;
	  ++_M_current;
	  return;
	}
      else if (*_M_current == _M_ctype.widen(']'))
	{
	  _M_curToken = _S_token_bracket_end;
	  _M_state &= ~_S_state_in_bracket;
	  ++_M_current;
	  return;
	}
      else if (*_M_current == _M_ctype.widen('\\'))
	{
	  _M_eat_escape();
	  return;
	}
      _M_curToken = _S_token_collelem_single;
      _M_curValue.assign(1, *_M_current);
      ++_M_current;
    }

  // TODO Complete it.
  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_eat_escape()
    {
      ++_M_current;
      if (_M_current == _M_end)
	{
	  _M_curToken = _S_token_eof;
	  return;
	}
      _CharT __c = *_M_current;
      ++_M_current;

      if (__c == _M_ctype.widen('('))
	{
	  if (!(_M_flags & (regex_constants::basic | regex_constants::grep)))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, __c);
	    }
	  else
	    _M_curToken = _S_token_subexpr_begin;
	}
      else if (__c == _M_ctype.widen(')'))
	{
	  if (!(_M_flags & (regex_constants::basic | regex_constants::grep)))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, __c);
	    }
	  else
	    _M_curToken = _S_token_subexpr_end;
	}
      else if (__c == _M_ctype.widen('{'))
	{
	  if (!(_M_flags & (regex_constants::basic | regex_constants::grep)))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, __c);
	    }
	  else
	    {
	      _M_curToken = _S_token_interval_begin;
	      _M_state |= _S_state_in_brace;
	    }
	}
      else if (__c == _M_ctype.widen('}'))
	{
	  if (!(_M_flags & (regex_constants::basic | regex_constants::grep)))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, __c);
	    }
	  else
	    {
	      if (!(_M_state && _S_state_in_brace))
		__throw_regex_error(regex_constants::error_badbrace);
	      _M_state &= ~_S_state_in_brace;
	      _M_curToken = _S_token_interval_end;
	    }
	}
      else if (__c == _M_ctype.widen('x'))
	{
	  ++_M_current;
	  if (_M_current == _M_end)
	    {
	      _M_curToken = _S_token_eof;
	      return;
	    }
	  if (_M_ctype.is(_CtypeT::digit, *_M_current))
	    {
	      _M_curValue.assign(1, *_M_current);
	      ++_M_current;
	      if (_M_current == _M_end)
		{
		  _M_curToken = _S_token_eof;
		  return;
		}
	      if (_M_ctype.is(_CtypeT::digit, *_M_current))
		{
		  _M_curValue += *_M_current;
		  ++_M_current;
		  return;
		}
	    }
	}
      else if (__c == _M_ctype.widen('^')
	       || __c == _M_ctype.widen('.')
	       || __c == _M_ctype.widen('*')
	       || __c == _M_ctype.widen('$')
	       || __c == _M_ctype.widen('\\'))
	{
	  _M_curToken = _S_token_ord_char;
	  _M_curValue.assign(1, __c);
	}
      else if (_M_ctype.is(_CtypeT::digit, __c))
	{
	  _M_curToken = _S_token_backref;
	  _M_curValue.assign(1, __c);
	}
      else if (_M_state & _S_state_in_bracket)
	{
	  if (__c == _M_ctype.widen('-')
	      || __c == _M_ctype.widen('[')
	      || __c == _M_ctype.widen(']'))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, __c);
	    }
	  else if ((_M_flags & regex_constants::ECMAScript)
		   && __c == _M_ctype.widen('b'))
	    {
	      _M_curToken = _S_token_ord_char;
	      _M_curValue.assign(1, _M_ctype.widen(' '));
	    }
	  else
	    __throw_regex_error(regex_constants::error_escape);
	}
      else
	__throw_regex_error(regex_constants::error_escape);
    }

  // Eats a character class or throwns an exception.
  // current point to ':' delimiter on entry, char after ']' on return
  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_eat_charclass()
    {
      ++_M_current; // skip ':'
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_ctype);
      for (_M_curValue.clear();
	   _M_current != _M_end && *_M_current != _M_ctype.widen(':');
	   ++_M_current)
	_M_curValue += *_M_current;
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_ctype);
      ++_M_current; // skip ':'
      if (*_M_current != _M_ctype.widen(']'))
	__throw_regex_error(regex_constants::error_ctype);
      ++_M_current; // skip ']'
    }


  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_eat_equivclass()
    {
      ++_M_current; // skip '='
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_collate);
      for (_M_curValue.clear();
	   _M_current != _M_end && *_M_current != _M_ctype.widen('=');
	   ++_M_current)
	_M_curValue += *_M_current;
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_collate);
      ++_M_current; // skip '='
      if (*_M_current != _M_ctype.widen(']'))
	__throw_regex_error(regex_constants::error_collate);
      ++_M_current; // skip ']'
    }


  template<typename _BiIter>
    void
    _Scanner<_BiIter>::
    _M_eat_collsymbol()
    {
      ++_M_current; // skip '.'
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_collate);
      for (_M_curValue.clear();
	   _M_current != _M_end && *_M_current != _M_ctype.widen('.');
	   ++_M_current)
	_M_curValue += *_M_current;
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_collate);
      ++_M_current; // skip '.'
      if (*_M_current != _M_ctype.widen(']'))
	__throw_regex_error(regex_constants::error_collate);
      ++_M_current; // skip ']'
    }

#ifdef _GLIBCXX_DEBUG
  template<typename _BiIter>
    std::ostream&
    _Scanner<_BiIter>::
    _M_print(std::ostream& ostr)
    {
      switch (_M_curToken)
      {
	case _S_token_anychar:
	  ostr << "any-character\n";
	  break;
	case _S_token_backref:
	  ostr << "backref\n";
	  break;
	case _S_token_bracket_begin:
	  ostr << "bracket-begin\n";
	  break;
	case _S_token_bracket_inverse_begin:
	  ostr << "bracket-inverse-begin\n";
	  break;
	case _S_token_bracket_end:
	  ostr << "bracket-end\n";
	  break;
	case _S_token_char_class_name:
	  ostr << "char-class-name \"" << _M_curValue << "\"\n";
	  break;
	case _S_token_closure0:
	  ostr << "closure0\n";
	  break;
	case _S_token_closure1:
	  ostr << "closure1\n";
	  break;
	case _S_token_collelem_multi:
	  ostr << "coll-elem-multi \"" << _M_curValue << "\"\n";
	  break;
	case _S_token_collelem_single:
	  ostr << "coll-elem-single \"" << _M_curValue << "\"\n";
	  break;
	case _S_token_collsymbol:
	  ostr << "collsymbol \"" << _M_curValue << "\"\n";
	  break;
	case _S_token_comma:
	  ostr << "comma\n";
	  break;
	case _S_token_dash:
	  ostr << "dash\n";
	  break;
	case _S_token_dup_count:
	  ostr << "dup count: " << _M_curValue << "\n";
	  break;
	case _S_token_eof:
	  ostr << "EOF\n";
	  break;
	case _S_token_equiv_class_name:
	  ostr << "equiv-class-name \"" << _M_curValue << "\"\n";
	  break;
	case _S_token_interval_begin:
	  ostr << "interval begin\n";
	  break;
	case _S_token_interval_end:
	  ostr << "interval end\n";
	  break;
	case _S_token_line_begin:
	  ostr << "line begin\n";
	  break;
	case _S_token_line_end:
	  ostr << "line end\n";
	  break;
	case _S_token_opt:
	  ostr << "opt\n";
	  break;
	case _S_token_or:
	  ostr << "or\n";
	  break;
	case _S_token_ord_char:
	  ostr << "ordinary character: \"" << _M_value() << "\"\n";
	  break;
	case _S_token_subexpr_begin:
	  ostr << "subexpr begin\n";
	  break;
	case _S_token_subexpr_end:
	  ostr << "subexpr end\n";
	  break;
	case _S_token_word_begin:
	  ostr << "word begin\n";
	  break;
	case _S_token_word_end:
	  ostr << "word end\n";
	  break;
	case _S_token_unknown:
	  ostr << "-- unknown token --\n";
	  break;
	default:
	  _GLIBCXX_DEBUG_ASSERT(false);
      }
      return ostr;
    }
#endif

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _Compiler(_InputIter __b, _InputIter __e,
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_match_token(_Compiler<_InputIter, _CharT, _TraitsT>::_TokenT token)
    {
      if (token == _M_scanner._M_token())
	{
	  _M_cur_value = _M_scanner._M_value();
	  _M_scanner._M_advance();
	  return true;
	}
      return false;
    }

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_InputIter, _CharT, _TraitsT>::
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_InputIter, _CharT, _TraitsT>::
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_InputIter, _CharT, _TraitsT>::
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_assertion()
    {
      if (_M_match_token(_ScannerT::_S_token_line_begin))
	{
	  // __m.push(_Matcher::_S_opcode_line_begin);
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_line_end))
	{
	  // __m.push(_Matcher::_S_opcode_line_end);
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_word_begin))
	{
	  // __m.push(_Matcher::_S_opcode_word_begin);
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_word_end))
	{
	  // __m.push(_Matcher::_S_opcode_word_end);
	  return true;
	}
      return false;
    }

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_InputIter, _CharT, _TraitsT>::
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_atom()
    {
      if (_M_match_token(_ScannerT::_S_token_anychar))
	{
	  const static auto&
	  __any_matcher = [](_CharT) -> bool
	  { return true; };

	  _M_stack.push(_StateSeqT(_M_state_store,
				  _M_state_store._M_insert_matcher
				  (__any_matcher)));
	  return true;
	}
      if (_M_match_token(_ScannerT::_S_token_ord_char))
	{
	  auto __c = _M_cur_value[0];
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
	  // __m.push(_Matcher::_S_opcode_ordchar, _M_cur_value);
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

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_bracket_expression()
    {
      bool __inverse =
	_M_match_token(_ScannerT::_S_token_bracket_inverse_begin);
      if (!(__inverse || _M_match_token(_ScannerT::_S_token_bracket_begin)))
	return false;
      _BMatcherT __matcher( __inverse, _M_traits, _M_flags);
      // special case: only if  _not_ chr first after
      // '[' or '[^' or if ECMAscript
      if (!_M_bracket_list(__matcher) // list is empty
	  && !(_M_flags & regex_constants::ECMAScript))
	__throw_regex_error(regex_constants::error_brack);
      _M_stack.push(_StateSeqT(_M_state_store,
			      _M_state_store._M_insert_matcher(__matcher)));
      return true;
    }

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    bool // list is non-empty
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_bracket_list(_BMatcherT& __matcher)
    {
      if (_M_match_token(_ScannerT::_S_token_bracket_end))
	return false;
      _M_expression_term(__matcher);
      _M_bracket_list(__matcher);
      return true;
    }

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    void
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_expression_term(_BMatcherT& __matcher)
    {
      if (_M_match_token(_ScannerT::_S_token_collsymbol))
	{
	  __matcher._M_add_collating_element(_M_cur_value);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_equiv_class_name))
	{
	  __matcher._M_add_equivalence_class(_M_cur_value);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_char_class_name))
	{
	  __matcher._M_add_character_class(_M_cur_value);
	  return;
	}
      if (_M_match_token(_ScannerT::_S_token_collelem_single)) // [a
	{
	  auto __ch = _M_cur_value[0];
	  if (_M_match_token(_ScannerT::_S_token_dash)) // [a-
	    {
	      // If the dash is the last character in the bracket expression,
	      // it is not special.
	      if (_M_scanner._M_token() == _ScannerT::_S_token_bracket_end)
		__matcher._M_add_char(_M_cur_value[0]); // [a-] <=> [a\-]
	      else // [a-z]
		{
		  if (!_M_match_token(_ScannerT::_S_token_collelem_single))
		    __throw_regex_error(regex_constants::error_range);
		  __matcher._M_make_range(__ch, _M_cur_value[0]);
		}
	    }
	  else // [a]
	    __matcher._M_add_char(__ch);
	  return;
	}
      __throw_regex_error(regex_constants::error_brack);
    }

  template<typename _InputIter, typename _CharT, typename _TraitsT>
    int
    _Compiler<_InputIter, _CharT, _TraitsT>::
    _M_cur_int_value(int __radix)
    {
      int __v = 0;
      for (typename _StringT::size_type __i = 0;
	   __i < _M_cur_value.length(); ++__i)
	__v =__v * __radix + _M_traits.value(_M_cur_value[__i], __radix);
      return __v;
    }

  template<typename _CharT, typename _TraitsT>
    bool _BracketMatcher<_CharT, _TraitsT>::
    operator()(_CharT __ch) const
    {
      auto __oldch = __ch;
      if (_M_flags & regex_constants::collate)
	if (_M_is_icase())
	  __ch = _M_traits.translate_nocase(__ch);
	else
	  __ch = _M_traits.translate(__ch);

      bool __ret = false;
      for (auto __c : _M_char_set)
	if (__c == __ch)
	  {
	    __ret = true;
	    break;
	  }
      if (!__ret && _M_traits.isctype(__oldch, _M_class_set))
	__ret = true;
      else
	{
	  _StringT __s = _M_get_str(__ch);
	  for (auto& __it : _M_range_set)
	    if (__it.first <= __s && __s <= __it.second)
	      {
		__ret = true;
		break;
	      }
	}
      if (_M_is_non_matching)
	__ret = !__ret;
      return __ret;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
