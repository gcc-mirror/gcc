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
 *  @file bits/regex_executor.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
namespace __detail
{
_GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(_V2)
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_search()
    {
      if (_M_search_from_first())
	return true;
      if (_M_flags & regex_constants::match_continuous)
	return false;
      _M_flags |= regex_constants::match_prev_avail;
      while (_M_begin != _M_end)
	{
	  ++_M_begin;
	  if (_M_search_from_first())
	    return true;
	}
      return false;
    }

  enum _ExecutorFrameOpcode : unsigned char
  {
    _S_fopcode_next,
    _S_fopcode_fallback_next,
    _S_fopcode_rep_once_more,
    _S_fopcode_fallback_rep_once_more,
    _S_fopcode_posix_alternative,
    _S_fopcode_merge_sol,
    _S_fopcode_restore_cur_results,
    _S_fopcode_restore_rep_count,
    _S_fopcode_decrement_rep_count,
  };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic" // anon struct
  struct _ExecutorFrameBase
  {
    _ExecutorFrameBase(_ExecutorFrameOpcode __op, _StateIdT __i)
    : _M_op(__op), _M_state_id(__i)
    { }

    _ExecutorFrameOpcode _M_op;
    union {
      unsigned char _M_byte0;
      struct { // Used by restore_rep_count frame
	unsigned char _M_count : 2;
      };
      struct { // Used by restore_cur_results frame
	unsigned char _M_end : 1;
	unsigned char _M_matched : 1;
      };
    };
    unsigned char _M_bytes[6];
    _StateIdT _M_state_id;
  };
#pragma GCC diagnostic pop

  template<typename _BiIter, bool _Trivial /* = is_trivially_copyable<_BiIter>::value */>
    struct _ExecutorFrame : _ExecutorFrameBase
    {
      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i)
      : _ExecutorFrameBase(__op, __i)
      { }

      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i, _BiIter __p)
      : _ExecutorFrameBase(__op, __i), _M_pos(__p)
      { }

      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i, long __v)
      : _ExecutorFrameBase(__op, __i), _M_val(__v)
      { }

      // _M_pos and _M_val are mutually exclusive, which the optimized
      // partial specialization below depends on.
      _BiIter _M_pos = _BiIter();
      long _M_val = 0;
    };

  // Space-optimized partial specialization for when the input iterator is
  // trivially copyable.
  template<typename _BiIter>
    struct _ExecutorFrame<_BiIter, true> : _ExecutorFrameBase
    {
      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i)
      : _ExecutorFrameBase(__op, __i)
      { }

      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i, _BiIter __p)
      : _ExecutorFrameBase(__op, __i), _M_pos(__p)
      { }

      _ExecutorFrame(_ExecutorFrameOpcode __op, _StateIdT __i, long __v)
      : _ExecutorFrameBase(__op, __i), _M_val(__v)
      { }

      union {
	_BiIter _M_pos;
	long _M_val;
      };
    };

  // The _M_main function operates in different modes, DFS mode or BFS mode,
  // indicated by template parameter __dfs_mode, and dispatches to one of the
  // _M_main_dispatch overloads.
  //
  // ------------------------------------------------------------
  //
  // DFS mode:
  //
  // It applies a Depth-First-Search (aka backtracking) on given NFA and input
  // string.
  // At the very beginning the executor stands in the start state, then it
  // tries every possible state transition in current state recursively. Some
  // state transitions consume input string, say, a single-char-matcher or a
  // back-reference matcher; some don't, like assertion or other anchor nodes.
  // When the input is exhausted and/or the current state is an accepting
  // state, the whole executor returns true.
  //
  // TODO: This approach is exponentially slow for certain input.
  //       Try to compile the NFA to a DFA.
  //
  // Time complexity: \Omega(match_length), O(2^(_M_nfa.size()))
  // Space complexity: \theta(match_results.size() + match_length)
  //
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_main_dispatch(_Match_mode __match_mode, __dfs)
    {
      _M_has_sol = false;
      *_M_states._M_get_sol_pos() = _BiIter();
      _M_cur_results = _M_results;
      _M_dfs(__match_mode, _M_states._M_start);
      return _M_has_sol;
    }

  // ------------------------------------------------------------
  //
  // BFS mode:
  //
  // Russ Cox's article (http://swtch.com/~rsc/regexp/regexp1.html)
  // explained this algorithm clearly.
  //
  // It first computes epsilon closure (states that can be achieved without
  // consuming characters) for every state that's still matching,
  // using the same DFS algorithm, but doesn't re-enter states (using
  // _M_states._M_visited to check), nor follow _S_opcode_match.
  //
  // Then apply DFS using every _S_opcode_match (in _M_states._M_match_queue)
  // as the start state.
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
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_main_dispatch(_Match_mode __match_mode, __bfs)
    {
      _M_states._M_queue(_M_states._M_start, _M_results);
      bool __ret = false;
      while (1)
	{
	  _M_has_sol = false;
	  if (_M_states._M_match_queue.empty())
	    break;
	  std::fill_n(_M_states._M_visited_states, _M_nfa.size(), false);
	  auto __old_queue = std::move(_M_states._M_match_queue);
	  auto __alloc = _M_cur_results.get_allocator();
	  for (auto& __task : __old_queue)
	    {
	      _M_cur_results = _ResultsVec(std::move(__task.second), __alloc);
	      _M_dfs(__match_mode, __task.first);
	    }
	  if (__match_mode == _Match_mode::_Prefix)
	    __ret |= _M_has_sol;
	  if (_M_current == _M_end)
	    break;
	  ++_M_current;
	}
      if (__match_mode == _Match_mode::_Exact)
	__ret = _M_has_sol;
      _M_states._M_match_queue.clear();
      return __ret;
    }

  // Return whether now match the given sub-NFA.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_lookahead(_StateIdT __next)
    {
      // Backreferences may refer to captured content.
      // We may want to make this faster by not copying,
      // but let's not be clever prematurely.
      _ResultsVec __what(_M_cur_results);
      _Executor __sub(_M_current, _M_end, __what, _M_re, _M_flags);
      __sub._M_states._M_start = __next;
      if (__sub._M_search_from_first())
	{
	  for (size_t __i = 0; __i < __what.size(); __i++)
	    if (__what[__i].matched)
	      _M_cur_results[__i] = __what[__i];
	  return true;
	}
      return false;
    }

  // __rep_count records how many times (__rep_count.second)
  // this node is visited under certain input iterator
  // (__rep_count.first). This prevent the executor from entering
  // infinite loop by refusing to continue when it's already been
  // visited more than twice. It's `twice` instead of `once` because
  // we need to spare one more time for potential group capture.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_rep_once_more(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      auto& __rep_count = _M_rep_count[__i];
      if (__rep_count.second == 0 || __rep_count.first != _M_current)
	{
	  _M_frames.emplace_back(_S_fopcode_restore_rep_count,
				 __i, __rep_count.first);
	  _M_frames.back()._M_count = __rep_count.second;
	  __rep_count.first = _M_current;
	  __rep_count.second = 1;
	  _M_frames.emplace_back(_S_fopcode_next, __state._M_alt);
	}
      else
	{
	  if (__rep_count.second < 2)
	    {
	      __rep_count.second++;
	      _M_frames.emplace_back(_S_fopcode_decrement_rep_count, __i);
	      _M_frames.emplace_back(_S_fopcode_next, __state._M_alt);
	    }
	}
    }

  // _M_alt branch is "match once more", while _M_next is "get me out
  // of this quantifier". Executing _M_next first or _M_alt first don't
  // mean the same thing, and we need to choose the correct order under
  // given greedy mode.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_repeat(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      // Greedy.
      if (!__state._M_neg)
	{
	  if constexpr (__dfs_mode)
	    // If it's DFS executor and already accepted, we're done.
	    _M_frames.emplace_back(_S_fopcode_fallback_next, __state._M_next,
				   _M_current);
	  else
	    _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
	  _M_frames.emplace_back(_S_fopcode_rep_once_more, __i);
	}
      else // Non-greedy mode
	{
	  if constexpr (__dfs_mode)
	    {
	      // vice-versa.
	      _M_frames.emplace_back(_S_fopcode_fallback_rep_once_more, __i,
				     _M_current);
	      _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
	    }
	  else
	    {
	      // DON'T attempt anything, because there's already another
	      // state with higher priority accepted. This state cannot
	      // be better by attempting its next node.
	      if (!_M_has_sol)
		{
		  // DON'T attempt anything if it's already accepted. An
		  // accepted state *must* be better than a solution that
		  // matches a non-greedy quantifier one more time.
		  _M_frames.emplace_back(_S_fopcode_fallback_rep_once_more, __i);
		  _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
		}
	    }
	}
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_subexpr_begin(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      auto& __res = _M_cur_results[__state._M_subexpr];
      _M_frames.emplace_back(_S_fopcode_restore_cur_results,
			     static_cast<_StateIdT>(__state._M_subexpr),
			     __res.first);
      _M_frames.back()._M_end = false;
      __res.first = _M_current;
      _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_subexpr_end(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      auto& __res = _M_cur_results[__state._M_subexpr];
      _M_frames.emplace_back(_S_fopcode_restore_cur_results,
			     static_cast<_StateIdT>(__state._M_subexpr),
			     __res.second);
      _M_frames.back()._M_end = true;
      _M_frames.back()._M_matched = __res.matched;
      __res.second = _M_current;
      __res.matched = true;
      _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    inline void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_line_begin_assertion(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_at_begin())
	_M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    inline void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_line_end_assertion(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_at_end())
	_M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    inline void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_word_boundary(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_word_boundary() == !__state._M_neg)
	_M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  // Here __state._M_alt offers a single start node for a sub-NFA.
  // We recursively invoke our algorithm to match the sub-NFA.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_subexpr_lookahead(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_lookahead(__state._M_alt) == !__state._M_neg)
	_M_frames.emplace_back(_S_fopcode_next, __state._M_next);
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_match(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_current == _M_end)
	return;
      if constexpr (__dfs_mode)
	{
	  if (__state._M_matches(*_M_current))
	    {
	      ++_M_current;
	      _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
	    }
	}
      else
	if (__state._M_matches(*_M_current))
	  _M_states._M_queue(__state._M_next, _M_cur_results);
    }

  template<typename _BiIter, typename _TraitsT>
    struct _Backref_matcher
    {
      _Backref_matcher(bool /* __icase */, const _TraitsT& __traits)
      : _M_traits(__traits) { }

      bool
      _M_apply(_BiIter __expected_begin,
	       _BiIter __expected_end, _BiIter __actual_begin,
	       _BiIter __actual_end)
      {
	return _M_traits.transform(__expected_begin, __expected_end)
	    == _M_traits.transform(__actual_begin, __actual_end);
      }

      const _TraitsT& _M_traits;
    };

  template<typename _BiIter, typename _CharT>
    struct _Backref_matcher<_BiIter, std::regex_traits<_CharT>>
    {
      using _TraitsT = std::regex_traits<_CharT>;
      _Backref_matcher(bool __icase, const _TraitsT& __traits)
      : _M_icase(__icase), _M_traits(__traits) { }

      bool
      _M_apply(_BiIter __expected_begin,
	       _BiIter __expected_end, _BiIter __actual_begin,
	       _BiIter __actual_end)
      {
	if (!_M_icase)
	  return _GLIBCXX_STD_A::__equal4(__expected_begin, __expected_end,
			       __actual_begin, __actual_end);
	typedef std::ctype<_CharT> __ctype_type;
	const auto& __fctyp = use_facet<__ctype_type>(_M_traits.getloc());
	return _GLIBCXX_STD_A::__equal4(__expected_begin, __expected_end,
			     __actual_begin, __actual_end,
			     [this, &__fctyp](_CharT __lhs, _CharT __rhs)
			     {
			       return __fctyp.tolower(__lhs)
				 == __fctyp.tolower(__rhs);
			     });
      }

      bool _M_icase;
      const _TraitsT& _M_traits;
    };

  // First fetch the matched result from _M_cur_results as __submatch;
  // then compare it with
  // (_M_current, _M_current + (__submatch.second - __submatch.first)).
  // If matched, keep going; else just return and try another state.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_backref(_Match_mode __match_mode, _StateIdT __i)
    {
      static_assert(__dfs_mode, "this should never be instantiated");

      const auto& __state = _M_nfa[__i];
      auto& __submatch = _M_cur_results[__state._M_backref_index];
      if (!__submatch.matched)
	return;
      auto __last = _M_current;
      for (auto __tmp = __submatch.first;
	   __last != _M_end && __tmp != __submatch.second;
	   ++__tmp)
	++__last;
      if (_Backref_matcher<_BiIter, _TraitsT>(
	      _M_re.flags() & regex_constants::icase,
	      _M_re._M_automaton->_M_traits)._M_apply(
		  __submatch.first, __submatch.second, _M_current, __last))
	{
	  _M_current = __last;
	  _M_frames.emplace_back(_S_fopcode_next, __state._M_next);
	}
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_accept(_Match_mode __match_mode, _StateIdT)
    {
      if constexpr (__dfs_mode)
	{
	  __glibcxx_assert(!_M_has_sol);
	  if (__match_mode == _Match_mode::_Exact)
	    _M_has_sol = _M_current == _M_end;
	  else
	    _M_has_sol = true;
	  if (_M_current == _M_begin
	      && (_M_flags & regex_constants::match_not_null))
	    _M_has_sol = false;
	  if (_M_has_sol)
	    {
	      if (_M_nfa._M_flags & regex_constants::ECMAScript)
		_M_results = _M_cur_results;
	      else // POSIX
		{
		  __glibcxx_assert(_M_states._M_get_sol_pos());
		  // Here's POSIX's logic: match the longest one. However
		  // we never know which one (lhs or rhs of "|") is longer
		  // unless we try both of them and compare the results.
		  // The member variable _M_sol_pos records the end
		  // position of the last successful match. It's better
		  // to be larger, because POSIX regex is always greedy.
		  // TODO: This could be slow.
		  if (*_M_states._M_get_sol_pos() == _BiIter()
		      || std::distance(_M_begin,
				       *_M_states._M_get_sol_pos())
			 < std::distance(_M_begin, _M_current))
		    {
		      *_M_states._M_get_sol_pos() = _M_current;
		      _M_results = _M_cur_results;
		    }
		}
	    }
	}
      else
	{
	  if (_M_current == _M_begin
	      && (_M_flags & regex_constants::match_not_null))
	    return;
	  if (__match_mode == _Match_mode::_Prefix || _M_current == _M_end)
	    if (!_M_has_sol)
	      {
		_M_has_sol = true;
		_M_results = _M_cur_results;
	      }
	}
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_handle_alternative(_Match_mode __match_mode, _StateIdT __i)
    {
      const auto& __state = _M_nfa[__i];
      if (_M_nfa._M_flags & regex_constants::ECMAScript)
	{
	  // TODO: Fix BFS support. It is wrong.
	  // Pick lhs if it matches. Only try rhs if it doesn't.
	  _M_frames.emplace_back(_S_fopcode_fallback_next, __state._M_next,
				 _M_current);
	  _M_frames.emplace_back(_S_fopcode_next, __state._M_alt);
	}
      else
	{
	  // Try both and compare the result.
	  // See "case _S_opcode_accept:" handling above.
	  _M_frames.emplace_back(_S_fopcode_posix_alternative, __state._M_next,
				 _M_current);
	  _M_frames.emplace_back(_S_fopcode_next, __state._M_alt);
	}
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
#ifdef __OPTIMIZE__
    [[__gnu__::__always_inline__]]
#endif
    inline void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_node(_Match_mode __match_mode, _StateIdT __i)
    {
      if (_M_states._M_visited(__i))
	return;

      switch (_M_nfa[__i]._M_opcode())
	{
	case _S_opcode_repeat:
	  _M_handle_repeat(__match_mode, __i); break;
	case _S_opcode_subexpr_begin:
	  _M_handle_subexpr_begin(__match_mode, __i); break;
	case _S_opcode_subexpr_end:
	  _M_handle_subexpr_end(__match_mode, __i); break;
	case _S_opcode_line_begin_assertion:
	  _M_handle_line_begin_assertion(__match_mode, __i); break;
	case _S_opcode_line_end_assertion:
	  _M_handle_line_end_assertion(__match_mode, __i); break;
	case _S_opcode_word_boundary:
	  _M_handle_word_boundary(__match_mode, __i); break;
	case _S_opcode_subexpr_lookahead:
	  _M_handle_subexpr_lookahead(__match_mode, __i); break;
	case _S_opcode_match:
	  _M_handle_match(__match_mode, __i); break;
	case _S_opcode_backref:
	  if constexpr (__dfs_mode)
	    _M_handle_backref(__match_mode, __i);
	  else
	    __builtin_unreachable();
	  break;
	case _S_opcode_accept:
	  _M_handle_accept(__match_mode, __i); break;
	case _S_opcode_alternative:
	  _M_handle_alternative(__match_mode, __i); break;
	default:
	  __glibcxx_assert(false);
	}
    }

  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_dfs(_Match_mode __match_mode, _StateIdT __start)
    {
      _M_frames.emplace_back(_S_fopcode_next, __start);

      while (!_M_frames.empty())
	{
	  _ExecutorFrame<_BiIter> __frame = std::move(_M_frames.back());
	  _M_frames.pop_back();

	  switch (__frame._M_op)
	    {
	    case _S_fopcode_fallback_next:
	      if (_M_has_sol)
		break;
	      if constexpr (__dfs_mode)
		_M_current = __frame._M_pos;
	      [[__fallthrough__]];
	    case _S_fopcode_next:
	      _M_node(__match_mode, __frame._M_state_id);
	      break;

	    case _S_fopcode_fallback_rep_once_more:
	      if (_M_has_sol)
		break;
	      if constexpr (__dfs_mode)
		_M_current = __frame._M_pos;
	      [[__fallthrough__]];
	    case _S_fopcode_rep_once_more:
	      _M_rep_once_more(__match_mode, __frame._M_state_id);
	      break;

	    case _S_fopcode_posix_alternative:
	      _M_frames.emplace_back(_S_fopcode_merge_sol, 0, _M_has_sol);
	      _M_frames.emplace_back(_S_fopcode_next, __frame._M_state_id);
	      if constexpr (__dfs_mode)
		_M_current = __frame._M_pos;
	      _M_has_sol = false;
	      break;

	    case _S_fopcode_merge_sol:
	      _M_has_sol |= __frame._M_val;
	      break;

	    case _S_fopcode_restore_cur_results:
	      if (!__frame._M_end)
		_M_cur_results[__frame._M_state_id].first = __frame._M_pos;
	      else
		{
		  _M_cur_results[__frame._M_state_id].second = __frame._M_pos;
		  _M_cur_results[__frame._M_state_id].matched = __frame._M_matched;
		}
	      break;

	    case _S_fopcode_restore_rep_count:
	      _M_rep_count[__frame._M_state_id].first = __frame._M_pos;
	      _M_rep_count[__frame._M_state_id].second = __frame._M_count;
	      break;

	    case _S_fopcode_decrement_rep_count:
	      _M_rep_count[__frame._M_state_id].second--;
	      break;
	    }
	}
    }

  // Return whether now is at some word boundary.
  template<typename _BiIter, typename _Alloc, typename _TraitsT,
	   bool __dfs_mode>
    bool _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::
    _M_word_boundary() const
    {
      if (_M_current == _M_begin && (_M_flags & regex_constants::match_not_bow))
	return false;
      if (_M_current == _M_end && (_M_flags & regex_constants::match_not_eow))
	return false;

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

      return __left_is_word != __right_is_word;
    }
_GLIBCXX_END_INLINE_ABI_NAMESPACE(_V2)
} // namespace __detail
#pragma GCC diagnostic pop

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
