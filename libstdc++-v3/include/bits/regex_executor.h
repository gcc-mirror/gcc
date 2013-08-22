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

// TODO: convert comments to doxygen format.

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

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    class _Executor
    {
    public:
      typedef match_results<_BiIter, _Alloc>          _ResultsT;
      typedef std::vector<sub_match<_BiIter>, _Alloc> _ResultsVec;
      typedef regex_constants::match_flag_type        _FlagT;

      virtual
      ~_Executor()
      { }

      // Set matched when string exactly match the pattern.
      virtual void
      _M_match() = 0;

      // Set matched when some prefix of the string matches the pattern.
      virtual void
      _M_search_from_first() = 0;

    protected:
      typedef typename _NFA<_CharT, _TraitsT>::_SizeT _SizeT;
      _Executor(_BiIter    __begin,
		_BiIter    __end,
		_ResultsT& __results,
		_FlagT     __flags,
		_SizeT     __size)
      : _M_current(__begin), _M_end(__end), _M_results(__results),
	_M_flags(__flags)
      {
	__size += 2;
	_M_results.resize(__size);
	for (auto __i = 0; __i < __size; __i++)
	  _M_results[__i].matched = false;
      }

      _BiIter       _M_current;
      _BiIter       _M_end;
      _ResultsVec&  _M_results;
      _FlagT        _M_flags;
    };

  // A _DFSExecutor perform a DFS on given NFA and input string. At the very
  // beginning the executor stands in the start state, then it try every
  // possible state transition in current state recursively. Some state
  // transitions consume input string, say, a single-char-matcher or a
  // back-reference matcher; some not, like assertion or other anchor nodes.
  // When the input is exhausted and the current state is an accepting state,
  // the whole executor return true.
  //
  // TODO: This approach is exponentially slow for certain input.
  //       Try to compile the NFA to a DFA.
  //
  // Time complexity: exponential
  // Space complexity: O(__end - __begin)
  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    class _DFSExecutor
    : public _Executor<_BiIter, _Alloc, _CharT, _TraitsT>
    {
    public:
      typedef _Executor<_BiIter, _Alloc, _CharT, _TraitsT> _BaseT;
      typedef _NFA<_CharT, _TraitsT>                       _RegexT;
      typedef typename _BaseT::_ResultsT                   _ResultsT;
      typedef typename _BaseT::_ResultsVec                 _ResultsVec;
      typedef regex_constants::match_flag_type             _FlagT;

      _DFSExecutor(_BiIter        __begin,
		   _BiIter        __end,
		   _ResultsT&     __results,
		   const _RegexT& __nfa,
		   _FlagT         __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count()),
	_M_traits(_TraitsT()), _M_nfa(__nfa), _M_results_ret(this->_M_results)
      { }

      void
      _M_match()
      { _M_dfs<true>(_M_nfa._M_start()); }

      void
      _M_search_from_first()
      { _M_dfs<false>(_M_nfa._M_start()); }

    private:
      template<bool __match_mode>
	bool
	_M_dfs(_StateIdT __i);

      _ResultsVec    _M_results_ret;
      _TraitsT       _M_traits;
      const _RegexT& _M_nfa;
    };

  // Like the DFS approach, it try every possible state transition; Unlike DFS,
  // it uses a queue instead of a stack to store matching states. It's a BFS
  // approach.
  //
  // Russ Cox's article(http://swtch.com/~rsc/regexp/regexp1.html) explained
  // this algorithm clearly.
  //
  // Every entry of _M_covered saves the solution(grouping status) for every
  // matching head. When states transit, solutions will be compared and
  // deduplicated(based on which greedy mode we have).
  //
  // Time complexity: O((__end - __begin) * _M_nfa.size())
  // Space complexity: O(_M_nfa.size() * _M_nfa.mark_count())
  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    class _BFSExecutor
    : public _Executor<_BiIter, _Alloc, _CharT, _TraitsT>
    {
    public:
      typedef _Executor<_BiIter, _Alloc, _CharT, _TraitsT> _BaseT;
      typedef _NFA<_CharT, _TraitsT>                       _RegexT;
      typedef typename _BaseT::_ResultsT                   _ResultsT;
      typedef typename _BaseT::_ResultsVec                 _ResultsVec;
      typedef std::unique_ptr<_ResultsVec>                 _ResultsPtr;
      typedef regex_constants::match_flag_type             _FlagT;

      _BFSExecutor(_BiIter        __begin,
		   _BiIter        __end,
		   _ResultsT&     __results,
		   const _RegexT& __nfa,
		   _FlagT         __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count()),
	_M_nfa(__nfa)
      {
	if (_M_nfa._M_start() != _S_invalid_state_id)
	  _M_covered[_M_nfa._M_start()] =
	    _ResultsPtr(new _ResultsVec(this->_M_results));
	_M_e_closure();
      }

      void
      _M_match()
      { _M_main_loop<true>(); }

      void
      _M_search_from_first()
      { _M_main_loop<false>(); }

    private:
      template<bool __match_mode>
	void
	_M_main_loop();

      void
      _M_e_closure();

      void
      _M_move();

      bool
      _M_match_less_than(const _ResultsVec& __u, const _ResultsVec& __v) const;

      bool
      _M_includes_some() const;

      std::map<_StateIdT, _ResultsPtr>     _M_covered;
      const _RegexT&                       _M_nfa;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_executor.tcc>
