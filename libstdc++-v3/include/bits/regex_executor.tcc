// class template regex -*- C++ -*-

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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
 *  @file bits/regex_executor.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
    bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_search()
    {
      if (_M_flags & regex_constants::match_continuous)
	return _M_search_from_first();
      auto __cur = _M_begin;
      do
	{
	  _M_current = __cur;
	  if (_M_main<false>())
	    return true;
	}
      // Continue when __cur == _M_end
      while (__cur++ != _M_end);
      return false;
    }

  // This function operates in different modes, DFS mode or BFS mode, indicated
  // by template parameter __dfs_mode. See _M_main for details.
  //
  // ------------------------------------------------------------
  //
  // DFS mode:
  //
  // It applies a Depth-First-Search (aka backtracking) on given NFA and input
  // string.
  // At the very beginning the executor stands in the start state, then it tries
  // every possible state transition in current state recursively. Some state
  // transitions consume input string, say, a single-char-matcher or a
  // back-reference matcher; some don't, like assertion or other anchor nodes.
  // When the input is exhausted and/or the current state is an accepting state,
  // the whole executor returns true.
  //
  // TODO: This approach is exponentially slow for certain input.
  //       Try to compile the NFA to a DFA.
  //
  // Time complexity: \Omega(match_length), O(2^(_M_nfa.size()))
  // Space complexity: \theta(match_results.size() + match_length)
  //
  // ------------------------------------------------------------
  //
  // BFS mode:
  //
  // Russ Cox's article (http://swtch.com/~rsc/regexp/regexp1.html)
  // explained this algorithm clearly.
  //
  // It first computes epsilon closure (states that can be achieved without
  // consuming characters) for every state that's still matching,
  // using the same DFS algorithm, but doesn't re-enter states (find a true in
  // _M_visited), nor follows _S_opcode_match.
  //
  // Then apply DFS using every _S_opcode_match (in _M_match_queue) as the start
  // state.
  //
  // It significantly reduces potential duplicate states, so has a better
  // upper bound; but it requires more overhead.
  //
  // Time complexity: \Omega(match_length * match_results.size())
  //                  O(match_length * _M_nfa.size() * match_results.size())
  // Space complexity: \Omega(_M_nfa.size() + match_results.size())
  //                   O(_M_nfa.size() * match_results.size())
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
    bool __dfs_mode>
  template<bool __match_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_main()
    {
      if (__dfs_mode)
	{
	  _M_has_sol = false;
	  _M_cur_results = _M_results;
	  _M_dfs<__match_mode>(_M_start_state);
	  return _M_has_sol;
	}
      else
	{
	  _M_match_queue->push_back(make_pair(_M_start_state, _M_results));
	  bool __ret = false;
	  while (1)
	    {
	      _M_has_sol = false;
	      if (_M_match_queue->empty())
		break;
	      _M_visited->assign(_M_visited->size(), false);
	      auto __old_queue = std::move(*_M_match_queue);
	      for (auto& __task : __old_queue)
		{
		  _M_cur_results = std::move(__task.second);
		  _M_dfs<__match_mode>(__task.first);
		}
	      if (!__match_mode)
		__ret |= _M_has_sol;
	      if (_M_current == _M_end)
		break;
	      ++_M_current;
	    }
	  if (__match_mode)
	    __ret = _M_has_sol;
	  return __ret;
	}
    }

  // Return whether now match the given sub-NFA.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
    bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_lookahead(_State<_TraitsT> __state)
    {
      _ResultsVec __what(_M_cur_results.size());
      auto __sub = std::unique_ptr<_Executor>(new _Executor(_M_current,
							    _M_end,
							    __what,
							    _M_re,
							    _M_flags));
      __sub->_M_start_state = __state._M_alt;
      if (__sub->_M_search_from_first())
	{
	  for (size_t __i = 0; __i < __what.size(); __i++)
	    if (__what[__i].matched)
	      _M_cur_results[__i] = __what[__i];
	  return true;
	}
      return false;
    }

  // TODO: Use a function vector to dispatch, instead of using switch-case.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
    bool __dfs_mode>
  template<bool __match_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_dfs(_StateIdT __i)
    {
      if (!__dfs_mode)
	{
	  if ((*_M_visited)[__i])
	    return;
	  (*_M_visited)[__i] = true;
	}

      const auto& __state = _M_nfa[__i];
      // Every change on _M_cur_results and _M_current will be rolled back after
      // finishing the recursion step.
      switch (__state._M_opcode)
	{
	// _M_alt branch is "match once more", while _M_next is "get me out
	// of this quantifier". Executing _M_next first or _M_alt first don't
	// mean the same thing, and we need to choose the correct order under
	// given greedy mode.
	case _S_opcode_alternative:
	  // Greedy.
	  if (!__state._M_neg)
	    {
	      // "Once more" is preferred in greedy mode.
	      _M_dfs<__match_mode>(__state._M_alt);
	      // If it's DFS executor and already accepted, we're done.
	      if (!__dfs_mode || !_M_has_sol)
		_M_dfs<__match_mode>(__state._M_next);
	    }
	  else // Non-greedy mode
	    {
	      if (__dfs_mode)
		{
		  // vice-versa.
		  _M_dfs<__match_mode>(__state._M_next);
		  if (!_M_has_sol)
		    _M_dfs<__match_mode>(__state._M_alt);
		}
	      else
		{
		  // DON'T attempt anything, because there's already another
		  // state with higher priority accepted. This state cannot be
		  // better by attempting its next node.
		  if (!_M_has_sol)
		    {
		      _M_dfs<__match_mode>(__state._M_next);
		      // DON'T attempt anything if it's already accepted. An
		      // accepted state *must* be better than a solution that
		      // matches a non-greedy quantifier one more time.
		      if (!_M_has_sol)
			_M_dfs<__match_mode>(__state._M_alt);
		    }
		}
	    }
	  break;
	case _S_opcode_subexpr_begin:
	  // If there's nothing changed since last visit, do NOT continue.
	  // This prevents the executor from get into infinite loop when using
	  // "()*" to match "".
	  if (!_M_cur_results[__state._M_subexpr].matched
	      || _M_cur_results[__state._M_subexpr].first != _M_current)
	    {
	      auto& __res = _M_cur_results[__state._M_subexpr];
	      auto __back = __res.first;
	      __res.first = _M_current;
	      _M_dfs<__match_mode>(__state._M_next);
	      __res.first = __back;
	    }
	  break;
	case _S_opcode_subexpr_end:
	  if (_M_cur_results[__state._M_subexpr].second != _M_current
	      || _M_cur_results[__state._M_subexpr].matched != true)
	    {
	      auto& __res = _M_cur_results[__state._M_subexpr];
	      auto __back = __res;
	      __res.second = _M_current;
	      __res.matched = true;
	      _M_dfs<__match_mode>(__state._M_next);
	      __res = __back;
	    }
	  else
	    _M_dfs<__match_mode>(__state._M_next);
	  break;
	case _S_opcode_line_begin_assertion:
	  if (_M_at_begin())
	    _M_dfs<__match_mode>(__state._M_next);
	  break;
	case _S_opcode_line_end_assertion:
	  if (_M_at_end())
	    _M_dfs<__match_mode>(__state._M_next);
	  break;
	case _S_opcode_word_boundary:
	  if (_M_word_boundary(__state) == !__state._M_neg)
	    _M_dfs<__match_mode>(__state._M_next);
	  break;
	// Here __state._M_alt offers a single start node for a sub-NFA.
	// We recursively invoke our algorithm to match the sub-NFA.
	case _S_opcode_subexpr_lookahead:
	  if (_M_lookahead(__state) == !__state._M_neg)
	    _M_dfs<__match_mode>(__state._M_next);
	  break;
	case _S_opcode_match:
	  if (_M_current == _M_end)
	    break;
	  if (__dfs_mode)
	    {
	      if (__state._M_matches(*_M_current))
		{
		  ++_M_current;
		  _M_dfs<__match_mode>(__state._M_next);
		  --_M_current;
		}
	    }
	  else
	    if (__state._M_matches(*_M_current))
	      _M_match_queue->push_back(make_pair(__state._M_next,
						  _M_cur_results));
	  break;
	// First fetch the matched result from _M_cur_results as __submatch;
	// then compare it with
	// (_M_current, _M_current + (__submatch.second - __submatch.first)).
	// If matched, keep going; else just return and try another state.
	case _S_opcode_backref:
	  {
	    _GLIBCXX_DEBUG_ASSERT(__dfs_mode);
	    auto& __submatch = _M_cur_results[__state._M_backref_index];
	    if (!__submatch.matched)
	      break;
	    auto __last = _M_current;
	    for (auto __tmp = __submatch.first;
		 __last != _M_end && __tmp != __submatch.second;
		 ++__tmp)
	      ++__last;
	    if (_M_re._M_traits.transform(__submatch.first,
						__submatch.second)
		== _M_re._M_traits.transform(_M_current, __last))
	      {
		if (__last != _M_current)
		  {
		    auto __backup = _M_current;
		    _M_current = __last;
		    _M_dfs<__match_mode>(__state._M_next);
		    _M_current = __backup;
		  }
		else
		  _M_dfs<__match_mode>(__state._M_next);
	      }
	  }
	  break;
	case _S_opcode_accept:
	  if (__dfs_mode)
	    {
	      _GLIBCXX_DEBUG_ASSERT(!_M_has_sol);
	      if (__match_mode)
		_M_has_sol = _M_current == _M_end;
	      else
		_M_has_sol = true;
	      if (_M_current == _M_begin
		  && (_M_flags & regex_constants::match_not_null))
		_M_has_sol = false;
	      if (_M_has_sol)
		_M_results = _M_cur_results;
	    }
	  else
	    {
	      if (_M_current == _M_begin
		  && (_M_flags & regex_constants::match_not_null))
		break;
	      if (!__match_mode || _M_current == _M_end)
		if (!_M_has_sol)
		  {
		    _M_has_sol = true;
		    _M_results = _M_cur_results;
		  }
	    }
	  break;
	default:
	  _GLIBCXX_DEBUG_ASSERT(false);
	}
    }

  // Return whether now is at some word boundary.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
    bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_word_boundary(_State<_TraitsT> __state) const
    {
      bool __left_is_word = false;
      if (_M_current != _M_begin
	  || (_M_flags & regex_constants::match_prev_avail))
	{
	  auto __prev = _M_current;
	  if (_M_is_word(*std::prev(__prev)))
	    __left_is_word = true;
	}
      bool __right_is_word =
	_M_current != _M_end && _M_is_word(*_M_current);

      if (__left_is_word == __right_is_word)
	return false;
      if (__left_is_word && !(_M_flags & regex_constants::match_not_eow))
	return true;
      if (__right_is_word && !(_M_flags & regex_constants::match_not_bow))
	return true;
      return false;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
