// class template regex -*- C++ -*-

// Copyright (C) 2013-2026 Free Software Foundation, Inc.
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
 *  @file bits/regex_executor.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

// FIXME convert comments to doxygen format.

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace __detail
{
  /**
   * @addtogroup regex-detail
   * @{
   */

  template<typename _BiIter, bool _Trivial = is_trivially_copyable<_BiIter>::value>
    struct _ExecutorFrame;

_GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(_V2)
  /**
   * @brief Takes a regex and an input string and does the matching.
   *
   * The %_Executor class has two modes: DFS mode and BFS mode, controlled
   * by the function parameter %__search_mode.
   */
  template<typename _BiIter, typename _Alloc, typename _TraitsT>
    class _Executor
    {
      enum class _Search_mode : unsigned char { _BFS = 0, _DFS = 1 };
      enum class _Match_mode : unsigned char { _Exact, _Prefix };

    public:
      typedef typename iterator_traits<_BiIter>::value_type _CharT;
      typedef basic_regex<_CharT, _TraitsT>                 _RegexT;
      typedef _GLIBCXX_STD_C::vector<sub_match<_BiIter>, _Alloc> _ResultsVec;
      typedef regex_constants::match_flag_type              _FlagT;
      typedef typename _TraitsT::char_class_type            _ClassT;
      typedef _NFA<_TraitsT>                                _NFAT;

    public:
      _Executor(_BiIter         __begin,
		_BiIter         __end,
		_ResultsVec&    __results,
		const _RegexT&  __re,
		_FlagT          __flags,
		bool            __use_dfs)
      : _M_cur_results(__results.get_allocator()),
	_M_begin(__begin),
	_M_end(__end),
	_M_re(__re),
	_M_nfa(*__re._M_automaton),
	_M_results(__results),
	_M_rep_count(_M_nfa.size()),
	_M_start(_M_nfa._M_start()),
	_M_visited_states(nullptr),
	_M_flags(__flags),
	_M_search_mode(__use_dfs ? _Search_mode::_DFS : _Search_mode::_BFS)
      {
	using namespace regex_constants;
	if (__flags & match_prev_avail) // ignore not_bol and not_bow
	  _M_flags &= ~(match_not_bol | match_not_bow);
	if (_M_search_mode == _Search_mode::_BFS)
	  _M_visited_states = new bool[_M_nfa.size()];
      }

      ~_Executor()
      { delete[] _M_visited_states; }

      // Set matched when string exactly matches the pattern.
      bool
      _M_match()
      {
	_M_current = _M_begin;
	return _M_main(_Match_mode::_Exact);
      }

      // Set matched when some prefix of the string matches the pattern.
      bool
      _M_search_from_first()
      {
	_M_current = _M_begin;
	return _M_main(_Match_mode::_Prefix);
      }

      bool
      _M_search();

    private:
      _StateIdT
      _M_rep_once_more(_Match_mode __match_mode, _StateIdT);

      _StateIdT
      _M_handle_repeat(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_subexpr_begin(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_subexpr_end(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_line_begin_assertion(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_line_end_assertion(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_word_boundary(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_subexpr_lookahead(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_match(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_backref(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_accept(_Match_mode, _StateIdT);

      _StateIdT
      _M_handle_alternative(_Match_mode, _StateIdT);

      _StateIdT
      _M_node(_Match_mode, _StateIdT);

      void
      _M_dfs(_Match_mode __match_mode, _StateIdT __start);

      bool
      _M_main(_Match_mode __match_mode)
      {
	if (_M_search_mode == _Search_mode::_DFS)
	  return _M_main_dfs(__match_mode);
	else
	  return _M_main_bfs(__match_mode);
      }

      bool
      _M_main_dfs(_Match_mode __match_mode);

      bool
      _M_main_bfs(_Match_mode __match_mode);

      bool
      _M_is_word(_CharT __ch) const
      {
	static const _CharT __s[2] = { 'w' };
	return _M_re._M_automaton->_M_traits.isctype
	  (__ch, _M_re._M_automaton->_M_traits.lookup_classname(__s, __s+1));
      }

      bool
      _M_at_begin() const
      {
	if (_M_current == _M_begin)
	  {
	    // match_not_bol means ^ does not match [_M_begin,_M_begin)
	    if (_M_flags & regex_constants::match_not_bol)
	      return false;
	    // match_prev_avail means _M_begin is not the start of the input.
	    if (_M_flags & regex_constants::match_prev_avail)
	      {
		// For ECMAScript multiline matches, check if the previous
		// character is a line terminator.
		if (_M_match_multiline())
		  return _M_is_line_terminator(*std::prev(_M_current));
		else
		  return false;
	      }
	    else // ^ matches at _M_begin
	      return true;
	  }
	else if (_M_match_multiline())
	  return _M_is_line_terminator(*std::prev(_M_current));
	else
	  return false;
      }

      bool
      _M_at_end() const
      {
	if (_M_current == _M_end)
	  return !(_M_flags & regex_constants::match_not_eol);
	else if (_M_match_multiline())
	  return _M_is_line_terminator(*_M_current);
	else
	  return false;
      }

      bool
      _M_word_boundary() const;

      bool
      _M_lookahead(_StateIdT __next);

      bool
      _M_is_line_terminator(_CharT __c) const
      {
	const auto& __traits = _M_re._M_automaton->_M_traits;
	const auto& __ct = use_facet<ctype<_CharT>>(__traits.getloc());
	const char __n{ __ct.narrow(__c, ' ') };
	if (__n == '\n')
	  return true;
	if (_M_re._M_automaton->_M_options() & regex_constants::ECMAScript)
	  {
	    if (__n == '\r')
	      return true;
	    // FIXME: U+2028 (line separator) and U+2029 (paragraph separator)
	  }
	return false;
      }

      bool
      _M_match_multiline() const noexcept
      {
	constexpr auto __m
	  = regex_constants::ECMAScript | regex_constants::__multiline;
	return (_M_re._M_automaton->_M_options() & __m) == __m;
      }

      inline bool
      _M_visited(_StateIdT __i)
      {
	if (_M_visited_states)
	  {
	    if (_M_visited_states[__i])
	      return true;
	    _M_visited_states[__i] = true;
	  }
	return false;
      }

      _BiIter* _M_get_sol_pos() { return &_M_sol_pos; }

    public:
      _GLIBCXX_STD_C::vector<_ExecutorFrame<_BiIter>>       _M_frames;
      _ResultsVec                                           _M_cur_results;
      _BiIter                                               _M_current;
      _BiIter                                               _M_begin;
      const _BiIter                                         _M_end;
      const _RegexT&                                        _M_re;
      const _NFAT&                                          _M_nfa;
      _ResultsVec&                                          _M_results;
      _GLIBCXX_STD_C::vector<pair<_BiIter, int>>            _M_rep_count;
      // To record current solution.
      _StateIdT                                             _M_start;
      _BiIter                                               _M_sol_pos;
      // (BFS only) Saves states that need to be considered for the next character.
      _GLIBCXX_STD_C::vector<pair<_StateIdT, _ResultsVec>>  _M_match_queue;
      // (BFS only) Indicates which states are already visited.
      bool*                                                 _M_visited_states;
      _FlagT                                                _M_flags;
      const _Search_mode                                    _M_search_mode;
      // Do we have a solution so far?
      bool                                                  _M_has_sol;
    };
_GLIBCXX_END_INLINE_ABI_NAMESPACE(_V2)

 ///@} regex-detail
} // namespace __detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#include <bits/regex_executor.tcc>
