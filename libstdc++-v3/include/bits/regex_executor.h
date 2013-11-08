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
 *  @file bits/regex_executor.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

// FIXME convert comments to doxygen format.

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  template<typename, typename>
    class basic_regex;

  template<typename>
    class sub_match;

  template<typename, typename>
    class match_results;
_GLIBCXX_END_NAMESPACE_VERSION

namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @addtogroup regex-detail
   * @{
   */

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    class _Executor
    {
    public:
      typedef typename iterator_traits<_BiIter>::value_type _CharT;
      typedef basic_regex<_CharT, _TraitsT>                 _RegexT;
      typedef std::vector<sub_match<_BiIter>, _Alloc>       _ResultsVec;
      typedef regex_constants::match_flag_type              _FlagT;
      typedef typename _TraitsT::char_class_type            _ClassT;
      typedef _NFA<_CharT, _TraitsT>                        _NFAT;

    public:
      _Executor(_BiIter         __begin,
		_BiIter         __end,
		_ResultsVec&    __results,
		const _RegexT&  __re,
		_FlagT          __flags)
      : _M_begin(__begin),
      _M_end(__end),
      _M_re(__re),
      _M_nfa(*__re._M_automaton),
      _M_results(__results),
      _M_match_queue(__dfs_mode ? nullptr
		     : new queue<pair<_StateIdT, _ResultsVec>>()),
      _M_visited(__dfs_mode ? nullptr : new vector<bool>(_M_nfa.size())),
      _M_flags((__flags & regex_constants::match_prev_avail)
	       ? (__flags
		  & ~regex_constants::match_not_bol
		  & ~regex_constants::match_not_bow)
	       : __flags),
      _M_start_state(_M_nfa._M_start())
      { }

      // Set matched when string exactly match the pattern.
      bool
      _M_match()
      {
	_M_current = _M_begin;
	return _M_main<true>();
      }

      // Set matched when some prefix of the string matches the pattern.
      bool
      _M_search_from_first()
      {
	_M_current = _M_begin;
	return _M_main<false>();
      }

      bool
      _M_search();

    private:
      template<bool __match_mode>
	void
	_M_dfs(_StateIdT __start);

      template<bool __match_mode>
	bool
	_M_main();

      bool
      _M_is_word(_CharT __ch) const
      {
	static const _CharT __s[2] = { 'w' };
	return _M_re._M_traits.isctype
	  (__ch, _M_re._M_traits.lookup_classname(__s, __s+1));
      }

      bool
      _M_at_begin() const
      {
	return _M_current == _M_begin
	  && !(_M_flags & (regex_constants::match_not_bol
			   | regex_constants::match_prev_avail));
      }

      bool
      _M_at_end() const
      {
	return _M_current == _M_end
	  && !(_M_flags & regex_constants::match_not_eol);
      }

      bool
      _M_word_boundary(_State<_CharT, _TraitsT> __state) const;

      bool
      _M_lookahead(_State<_CharT, _TraitsT> __state);

    public:
      _ResultsVec                                          _M_cur_results;
      _BiIter                                              _M_current;
      const _BiIter                                        _M_begin;
      const _BiIter                                        _M_end;
      const _RegexT&                                       _M_re;
      const _NFAT&                                         _M_nfa;
      _ResultsVec&                                         _M_results;
      // Used in BFS, saving states that need to be considered for the next
      // character.
      std::unique_ptr<queue<pair<_StateIdT, _ResultsVec>>> _M_match_queue;
      // Used in BFS, indicating that which state is already visited.
      std::unique_ptr<vector<bool>>                        _M_visited;
      _FlagT                                               _M_flags;
      // To record current solution.
      _StateIdT                                            _M_start_state;
      // Do we have a solution so far?
      bool                                                 _M_has_sol;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_executor.tcc>
