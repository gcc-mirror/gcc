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
 *  @file bits/regex_scanner.h
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
  template<typename _FwdIter>
    class _Scanner
    {
    public:
      typedef typename std::iterator_traits<_FwdIter>::value_type _CharT;
      typedef std::basic_string<_CharT>                           _StringT;
      typedef regex_constants::syntax_option_type                 _FlagT;
      typedef const std::ctype<_CharT>                            _CtypeT;

      /// Token types returned from the scanner.
      enum _TokenT
      {
	_S_token_anychar,
	_S_token_ord_char,
	_S_token_oct_num,
	_S_token_hex_num,
	_S_token_backref,
	_S_token_subexpr_begin,
	_S_token_subexpr_no_group_begin,
	_S_token_subexpr_lookahead_begin,
	_S_token_subexpr_end,
	_S_token_bracket_begin,
	_S_token_bracket_neg_begin,
	_S_token_bracket_end,
	_S_token_interval_begin,
	_S_token_interval_end,
	_S_token_quoted_class,
	_S_token_char_class_name,
	_S_token_collsymbol,
	_S_token_equiv_class_name,
	_S_token_opt,
	_S_token_or,
	_S_token_closure0,
	_S_token_closure1,
	_S_token_ungreedy,
	_S_token_line_begin,
	_S_token_line_end,
	_S_token_word_bound,
	_S_token_comma,
	_S_token_dup_count,
	_S_token_eof,
	_S_token_unknown
      };

      _Scanner(_FwdIter __begin, _FwdIter __end,
	       _FlagT __flags, std::locale __loc);

      void
      _M_advance();

      _TokenT
      _M_get_token() const
      { return _M_token; }

      const _StringT&
      _M_get_value() const
      { return _M_value; }

#ifdef _GLIBCXX_DEBUG
      std::ostream&
      _M_print(std::ostream&);
#endif

    private:
      enum _StateT
      {
	_S_state_normal,
	_S_state_in_brace,
	_S_state_in_bracket,
      };

      void
      _M_scan_normal();

      void
      _M_scan_in_bracket();

      void
      _M_scan_in_brace();

      void
      _M_eat_escape_ecma();

      void
      _M_eat_escape_posix();

      void
      _M_eat_escape_awk();

      void
      _M_eat_class(char);

      constexpr bool
      _M_is_ecma()
      { return _M_flags & regex_constants::ECMAScript; }

      constexpr bool
      _M_is_basic()
      { return _M_flags & (regex_constants::basic | regex_constants::grep); }

      constexpr bool
      _M_is_extended()
      {
	return _M_flags & (regex_constants::extended
			   | regex_constants::egrep
			   | regex_constants::awk);
      }

      constexpr bool
      _M_is_grep()
      { return _M_flags & (regex_constants::grep | regex_constants::egrep); }

      constexpr bool
      _M_is_awk()
      { return _M_flags & regex_constants::awk; }

      _StateT                       _M_state;
      _FwdIter                      _M_current;
      _FwdIter                      _M_end;
      _FlagT                        _M_flags;
      _CtypeT&                      _M_ctype;
      _TokenT                       _M_token;
      _StringT                      _M_value;
      bool                          _M_at_bracket_start;
    public:
      // TODO: make them static when this file is stable.
      const std::map<char, _TokenT> _M_token_map;
      const std::map<char, char>    _M_ecma_escape_map;
      const std::map<char, char>    _M_awk_escape_map;
      const std::set<char>          _M_ecma_spec_char;
      const std::set<char>          _M_basic_spec_char;
      const std::set<char>          _M_extended_spec_char;

      const std::map<char, char>&   _M_escape_map;
      const std::set<char>&         _M_spec_char;
      void (_Scanner::* _M_eat_escape)();
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_scanner.tcc>
