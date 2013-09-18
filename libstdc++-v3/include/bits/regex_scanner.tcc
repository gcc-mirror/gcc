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
 *  @file bits/regex_scanner.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

// FIXME make comments doxygen format.

// N3376 specified 6 regex styles: ECMAScript, basic, extended, grep, egrep
// and awk
// 1) grep is basic except '\n' is treated as '|'
// 2) egrep is extended except '\n' is treated as '|'
// 3) awk is extended except special escaping rules, and there's no
//    back-reference.
//
// References:
//
// ECMAScript: ECMA-262 15.10
//
// basic, extended:
// http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html
//
// awk: http://pubs.opengroup.org/onlinepubs/000095399/utilities/awk.html

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _FwdIter>
    _Scanner<_FwdIter>::
    _Scanner(_FwdIter __begin, _FwdIter __end,
	     _FlagT __flags, std::locale __loc)
    : _M_current(__begin) , _M_end(__end) , _M_flags(__flags),
      _M_ctype(std::use_facet<_CtypeT>(__loc)), _M_state(_S_state_normal),
      _M_at_bracket_start(false),
      _M_token_map
	{
	  {'^', _S_token_line_begin},
	  {'$', _S_token_line_end},
	  {'.', _S_token_anychar},
	  {'*', _S_token_closure0},
	  {'+', _S_token_closure1},
	  {'?', _S_token_opt},
	  {'|', _S_token_or},
	  // grep and egrep
	  {'\n', _S_token_or},
	},
      _M_ecma_escape_map
	{
	  {'0', '\0'},
	  {'b', '\b'},
	  {'f', '\f'},
	  {'n', '\n'},
	  {'r', '\r'},
	  {'t', '\t'},
	  {'v', '\v'},
	},
      _M_awk_escape_map
	{
	  {'"', '"'},
	  {'/', '/'},
	  {'\\', '\\'},
	  {'a', '\a'},
	  {'b', '\b'},
	  {'f', '\f'},
	  {'n', '\n'},
	  {'r', '\r'},
	  {'t', '\t'},
	  {'v', '\v'},
	},
      _M_escape_map(_M_is_ecma()
		    ? _M_ecma_escape_map
		    : _M_awk_escape_map),
      _M_ecma_spec_char
	{
	  '^',
	  '$',
	  '\\',
	  '.',
	  '*',
	  '+',
	  '?',
	  '(',
	  ')',
	  '[',
	  ']',
	  '{',
	  '}',
	  '|',
	},
      _M_basic_spec_char
	{
	  '.',
	  '[',
	  '\\',
	  '*',
	  '^',
	  '$',
	},
      _M_extended_spec_char
	{
	  '.',
	  '[',
	  '\\',
	  '(',
	  ')',
	  '*',
	  '+',
	  '?',
	  '{',
	  '|',
	  '^',
	  '$',
	},
      _M_eat_escape(_M_is_ecma()
		    ? &_Scanner::_M_eat_escape_ecma
		    : &_Scanner::_M_eat_escape_posix),
      _M_spec_char(_M_is_ecma()
		   ? _M_ecma_spec_char
		   : _M_is_basic()
		   ? _M_basic_spec_char
		   : _M_extended_spec_char)
    { _M_advance(); }

  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_advance()
    {
      if (_M_current == _M_end)
	{
	  _M_token = _S_token_eof;
	  return;
	}

      if (_M_state == _S_state_normal)
	_M_scan_normal();
      else if (_M_state == _S_state_in_bracket)
	_M_scan_in_bracket();
      else if (_M_state == _S_state_in_brace)
	_M_scan_in_brace();
      else
	_GLIBCXX_DEBUG_ASSERT(false);
    }

  // Differences between styles:
  // 1) "\(", "\)", "\{" in basic. It's not escaping.
  // 2) "(?:", "(?=", "(?!" in ECMAScript.
  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_scan_normal()
    {
      auto __c = *_M_current++;

      if (__c == '\\')
	{
	  if (_M_current == _M_end)
	    __throw_regex_error(regex_constants::error_escape);

	  if (!_M_is_basic()
	      || (*_M_current != '('
		  && *_M_current != ')'
		  && *_M_current != '{'))
	    {
	      (this->*_M_eat_escape)();
	      return;
	    }
	  __c = *_M_current++;
	}
      if (__c == '(')
	{
	  if (_M_is_ecma() && *_M_current == '?')
	    {
	      if (++_M_current == _M_end)
		__throw_regex_error(regex_constants::error_paren);

	      if (*_M_current == ':')
		{
		  ++_M_current;
		  _M_token = _S_token_subexpr_no_group_begin;
		}
	      else if (*_M_current == '=')
		{
		  ++_M_current;
		  _M_token = _S_token_subexpr_lookahead_begin;
		  _M_value.assign(1, 'p');
		}
	      else if (*_M_current == '!')
		{
		  ++_M_current;
		  _M_token = _S_token_subexpr_lookahead_begin;
		  _M_value.assign(1, 'n');
		}
	      else
		__throw_regex_error(regex_constants::error_paren);
	    }
	  else
	    _M_token = _S_token_subexpr_begin;
	}
      else if (__c == ')')
	_M_token = _S_token_subexpr_end;
      else if (__c == '[')
	{
	  _M_state = _S_state_in_bracket;
	  _M_at_bracket_start = true;
	  if (_M_current != _M_end && *_M_current == '^')
	    {
	      _M_token = _S_token_bracket_neg_begin;
	      ++_M_current;
	    }
	  else
	    _M_token = _S_token_bracket_begin;
	}
      else if (__c == '{')
	{
	  _M_state = _S_state_in_brace;
	  _M_token = _S_token_interval_begin;
	}
      else if (_M_spec_char.count(__c)
	       && __c != ']'
	       && __c != '}'
	       || (_M_is_grep() && __c == '\n'))
	_M_token = _M_token_map.at(__c);
      else
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, __c);
	}
    }

  // Differences between styles:
  // 1) different semantics of "[]" and "[^]".
  // 2) Escaping in bracket expr.
  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_scan_in_bracket()
    {
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_brack);

      auto __c = *_M_current++;

      if (__c == '[')
	{
	  if (_M_current == _M_end)
	    __throw_regex_error(regex_constants::error_brack);

	  if (*_M_current == '.')
	    {
	      _M_token = _S_token_collsymbol;
	      _M_eat_class(*_M_current++);
	    }
	  else if (*_M_current == ':')
	    {
	      _M_token = _S_token_char_class_name;
	      _M_eat_class(*_M_current++);
	    }
	  else if (*_M_current == '=')
	    {
	      _M_token = _S_token_equiv_class_name;
	      _M_eat_class(*_M_current++);
	    }
	  else
	    {
	      _M_token = _S_token_ord_char;
	      _M_value.assign(1, __c);
	    }
	}
      // In POSIX, when encountering "[]" or "[^]", the ']' is interpreted
      // literally. So "[]]" or "[^]]" is valid regex. See the testcases
      // `*/empty_range.cc`.
      else if (__c == ']' && (_M_is_ecma() || !_M_at_bracket_start))
	{
	  _M_token = _S_token_bracket_end;
	  _M_state = _S_state_normal;
	}
      // ECMAScirpt and awk permmits escaping in bracket.
      else if (__c == '\\' && (_M_is_ecma() || _M_is_awk()))
	(this->*_M_eat_escape)();
      else
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, __c);
	}
      _M_at_bracket_start = false;
    }

  // Differences between styles:
  // 1) "\}" in basic style.
  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_scan_in_brace()
    {
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_brace);

      auto __c = *_M_current++;

      if (_M_ctype.is(_CtypeT::digit, __c))
	{
	  _M_token = _S_token_dup_count;
	  _M_value.assign(1, __c);
	  while (_M_current != _M_end
		 && _M_ctype.is(_CtypeT::digit, *_M_current))
	    _M_value += *_M_current++;
	}
      else if (__c == ',')
	_M_token = _S_token_comma;
      // basic use \}.
      else if (_M_is_basic())
	{
	  if (__c == '\\' && _M_current != _M_end && *_M_current == '}')
	    {
	      _M_state = _S_state_normal;
	      _M_token = _S_token_interval_end;
	      ++_M_current;
	    }
	  else
	    __throw_regex_error(regex_constants::error_brace);
	}
      else if (__c == '}')
	{
	  _M_state = _S_state_normal;
	  _M_token = _S_token_interval_end;
	}
      else
	__throw_regex_error(regex_constants::error_brace);
    }

  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_eat_escape_ecma()
    {
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_escape);

      auto __c = *_M_current++;

      if (_M_escape_map.count(__c)
	  && (__c != 'b' || _M_state == _S_state_in_bracket))
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, _M_escape_map.at(__c));
	}
      else if (__c == 'b')
	{
	  _M_token = _S_token_word_bound;
	  _M_value.assign(1, 'p');
	}
      else if (__c == 'B')
	{
	  _M_token = _S_token_word_bound;
	  _M_value.assign(1, 'n');
	}
      // N3376 28.13
      else if (__c == 'd'
	       || __c == 'D'
	       || __c == 's'
	       || __c == 'S'
	       || __c == 'w'
	       || __c == 'W')
	{
	  _M_token = _S_token_quoted_class;
	  _M_value.assign(1, __c);
	}
      else if (__c == 'c')
	{
	  if (_M_current == _M_end)
	    __throw_regex_error(regex_constants::error_escape);
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, *_M_current++);
	}
      else if (__c == 'x' || __c == 'u')
	{
	  _M_value.erase();
	  for (int i = 0; i < (__c == 'x' ? 2 : 4); i++)
	    {
	      if (_M_current == _M_end
		  || !_M_ctype.is(_CtypeT::xdigit, *_M_current))
		__throw_regex_error(regex_constants::error_escape);
	      _M_value += *_M_current++;
	    }
	  _M_token = _S_token_hex_num;
	}
      // ECMAScript recongnizes multi-digit back-references.
      else if (_M_ctype.is(_CtypeT::digit, __c))
	{
	  _M_value.assign(1, __c);
	  while (_M_current != _M_end
		 && _M_ctype.is(_CtypeT::digit, *_M_current))
	    _M_value += *_M_current++;
	  _M_token = _S_token_backref;
	}
      else
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, __c);
	}
    }

  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_eat_escape_posix()
    {
      if (_M_current == _M_end)
	__throw_regex_error(regex_constants::error_escape);

      auto __c = *_M_current;

      if (_M_spec_char.count(__c))
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, __c);
	}
      // We MUST judge awk before handling backrefs. There's no backref in awk.
      else if (_M_is_awk())
	{
	  _M_eat_escape_awk();
	  return;
	}
      else if (_M_ctype.is(_CtypeT::digit, __c) && __c != '0')
	{
	  _M_token = _S_token_backref;
	  _M_value.assign(1, __c);
	}
      else
	__throw_regex_error(regex_constants::error_escape);
      ++_M_current;
    }

  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_eat_escape_awk()
    {
      auto __c = *_M_current++;

      if (_M_escape_map.count(__c))
	{
	  _M_token = _S_token_ord_char;
	  _M_value.assign(1, _M_escape_map.at(__c));
	}
      // \ddd for oct representation
      else if (_M_ctype.is(_CtypeT::digit, __c)
	       && __c != '8'
	       && __c != '9')
	{
	  _M_value.assign(1,  __c);
	  for (int __i = 0;
	       __i < 2
	       && _M_current != _M_end
	       && _M_ctype.is(_CtypeT::digit, *_M_current)
	       && *_M_current != '8'
	       && *_M_current != '9';
	       __i++)
	    _M_value += *_M_current++;
	  _M_token = _S_token_oct_num;
	  return;
	}
      else
	__throw_regex_error(regex_constants::error_escape);
    }

  // Eats a character class or throwns an exception.
  // __ch cound be ':', '.' or '=', _M_current is the char after ']' when
  // returning.
  template<typename _FwdIter>
    void
    _Scanner<_FwdIter>::
    _M_eat_class(char __ch)
    {
      for (_M_value.clear(); _M_current != _M_end && *_M_current != __ch;)
	_M_value += *_M_current++;
      if (_M_current == _M_end
	  || *_M_current++ != __ch
	  || _M_current == _M_end // skip __ch
	  || *_M_current++ != ']') // skip ']'
	if (__ch == ':')
	  __throw_regex_error(regex_constants::error_ctype);
	else
	  __throw_regex_error(regex_constants::error_collate);
    }

#ifdef _GLIBCXX_DEBUG
  template<typename _FwdIter>
    std::ostream&
    _Scanner<_FwdIter>::
    _M_print(std::ostream& ostr)
    {
      switch (_M_token)
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
      case _S_token_bracket_neg_begin:
	ostr << "bracket-neg-begin\n";
	break;
      case _S_token_bracket_end:
	ostr << "bracket-end\n";
	break;
      case _S_token_char_class_name:
	ostr << "char-class-name \"" << _M_value << "\"\n";
	break;
      case _S_token_closure0:
	ostr << "closure0\n";
	break;
      case _S_token_closure1:
	ostr << "closure1\n";
	break;
      case _S_token_collsymbol:
	ostr << "collsymbol \"" << _M_value << "\"\n";
	break;
      case _S_token_comma:
	ostr << "comma\n";
	break;
      case _S_token_dup_count:
	ostr << "dup count: " << _M_value << "\n";
	break;
      case _S_token_eof:
	ostr << "EOF\n";
	break;
      case _S_token_equiv_class_name:
	ostr << "equiv-class-name \"" << _M_value << "\"\n";
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
	ostr << "ordinary character: \"" << _M_value << "\"\n";
	break;
      case _S_token_subexpr_begin:
	ostr << "subexpr begin\n";
	break;
      case _S_token_subexpr_no_group_begin:
	ostr << "no grouping subexpr begin\n";
	break;
      case _S_token_subexpr_lookahead_begin:
	ostr << "lookahead subexpr begin\n";
	break;
      case _S_token_subexpr_end:
	ostr << "subexpr end\n";
	break;
      case _S_token_unknown:
	ostr << "-- unknown token --\n";
	break;
      case _S_token_oct_num:
	ostr << "oct number " << _M_value << "\n";
	break;
      case _S_token_hex_num:
	ostr << "hex number " << _M_value << "\n";
	break;
      case _S_token_quoted_class:
	ostr << "quoted class " << "\\" << _M_value << "\n";
	break;
      default:
	_GLIBCXX_DEBUG_ASSERT(false);
      }
      return ostr;
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
