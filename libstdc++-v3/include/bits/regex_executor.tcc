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
 *  @file bits/regex_executor.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _Executor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_search()
    {
      if (_M_flags & regex_constants::match_continuous)
	return _M_search_from_first();
      auto __cur = _M_begin;
      do
	{
	  _M_match_mode = false;
	  _M_init(__cur);
	  if (_M_main())
	    return true;
	}
      // Continue when __cur == _M_end
      while (__cur++ != _M_end);
      return false;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_dfs(_StateIdT __i)
    {
      auto& __current = this->_M_current;
      const auto& __state = _M_nfa[__i];
      bool __ret = false;
      switch (__state._M_opcode)
	{
	case _S_opcode_alternative:
	  // Greedy or not, this is a question ;)
	  if (!__state._M_neg)
	    __ret = _M_dfs(__state._M_alt)
	      || _M_dfs(__state._M_next);
	  else
	    __ret = _M_dfs(__state._M_next)
	      || _M_dfs(__state._M_alt);
	  break;
	case _S_opcode_subexpr_begin:
	  // Here's the critical part: if there's nothing changed since last
	  // visit, do NOT continue. This prevents the executor from get into
	  // infinite loop when use "()*" to match "".
	  //
	  // Every change on _M_cur_results will be roll back after the
	  // recursion step finished.
	  if (!_M_cur_results[__state._M_subexpr].matched
	      || _M_cur_results[__state._M_subexpr].first != __current)
	    {
	      auto __back = _M_cur_results[__state._M_subexpr].first;
	      _M_cur_results[__state._M_subexpr].first = __current;
	      __ret = _M_dfs(__state._M_next);
	      _M_cur_results[__state._M_subexpr].first = __back;
	    }
	  break;
	case _S_opcode_subexpr_end:
	  if (_M_cur_results[__state._M_subexpr].second != __current
	      || _M_cur_results[__state._M_subexpr].matched != true)
	    {
	      auto __back = _M_cur_results[__state._M_subexpr];
	      _M_cur_results[__state._M_subexpr].second = __current;
	      _M_cur_results[__state._M_subexpr].matched = true;
	      __ret = _M_dfs(__state._M_next);
	      _M_cur_results[__state._M_subexpr] = __back;
	    }
	  else
	    __ret = _M_dfs(__state._M_next);
	  break;
	case _S_opcode_line_begin_assertion:
	  if (this->_M_at_begin())
	    __ret = _M_dfs(__state._M_next);
	  break;
	case _S_opcode_line_end_assertion:
	  if (this->_M_at_end())
	    __ret = _M_dfs(__state._M_next);
	  break;
	case _S_opcode_word_boundry:
	  if (this->_M_word_boundry(__state) == !__state._M_neg)
	    __ret = _M_dfs(__state._M_next);
	  break;
	  // Here __state._M_alt offers a single start node for a sub-NFA.
	  // We recursivly invoke our algorithm to match the sub-NFA.
	case _S_opcode_subexpr_lookahead:
	  if (this->_M_lookahead(__state) == !__state._M_neg)
	    __ret = _M_dfs(__state._M_next);
	  break;
	case _S_opcode_match:
	  if (__current != this->_M_end && __state._M_matches(*__current))
	    {
	      ++__current;
	      __ret = _M_dfs(__state._M_next);
	      --__current;
	    }
	  break;
	// First fetch the matched result from _M_cur_results as __submatch;
	// then compare it with
	// (__current, __current + (__submatch.second - __submatch.first))
	// If matched, keep going; else just return to try another state.
	case _S_opcode_backref:
	  {
	    auto& __submatch = _M_cur_results[__state._M_backref_index];
	    if (!__submatch.matched)
	      break;
	    auto __last = __current;
	    for (auto __tmp = __submatch.first;
		 __last != this->_M_end && __tmp != __submatch.second;
		 ++__tmp)
	      ++__last;
	    if (this->_M_re._M_traits.transform(__submatch.first,
						__submatch.second)
		== this->_M_re._M_traits.transform(__current, __last))
	      {
		if (__last != __current)
		  {
		    auto __backup = __current;
		    __current = __last;
		    __ret = _M_dfs(__state._M_next);
		    __current = __backup;
		  }
		else
		  __ret = _M_dfs(__state._M_next);
	      }
	  }
	  break;
	case _S_opcode_accept:
	  if (this->_M_match_mode)
	    __ret = __current == this->_M_end;
	  else
	    __ret = true;
	  if (__current == this->_M_begin
	      && (this->_M_flags & regex_constants::match_not_null))
	    __ret = false;
	  if (__ret)
	    this->_M_set_results(_M_cur_results);
	  break;
	default:
	  _GLIBCXX_DEBUG_ASSERT(false);
	}
      return __ret;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_main()
    {
      _M_e_closure();
      bool __ret = false;
      if (!this->_M_match_mode
	  && !(this->_M_flags & regex_constants::match_not_null))
	__ret = _M_includes_some() || __ret;
      while (this->_M_current != this->_M_end)
	{
	  _M_move();
	  ++this->_M_current;
	  if (_M_stack._M_empty())
	    break;
	  _M_e_closure();
	  if (!this->_M_match_mode)
	    // To keep regex_search greedy, no "return true" here.
	    __ret = _M_includes_some() || __ret;
	}
      if (this->_M_match_mode)
	__ret = _M_includes_some();
      if (__ret)
	this->_M_set_results(_M_cur_results->_M_get());
      _M_match_stack._M_clear();
      _GLIBCXX_DEBUG_ASSERT(_M_stack._M_empty());
      return __ret;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    void _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_e_closure()
    {
      auto& __current = this->_M_current;

      while (!_M_stack._M_empty())
	{
	  auto __u = _M_stack._M_pop();
	  _GLIBCXX_DEBUG_ASSERT(_M_covered.count(__u));
	  const auto& __state = _M_nfa[__u];

	  // Can be implemented using method, but there will be too many
	  // arguments. I would use macro function before C++11, but lambda is
	  // a better choice, since hopefully compiler can inline it.
	  auto __add_visited_state = [=](_StateIdT __v)
	  {
	    if (_M_covered.count(__v) == 0)
	      {
		_M_covered[__v] =
		  _ResultsPtr(new _ResultsEntry(*_M_covered[__u]));
		_M_stack._M_push(__v);
		return;
	      }
	    auto& __cu = _M_covered[__u];
	    auto& __cv = _M_covered[__v];
	    if (*__cu < *__cv)
	      {
		__cv = _ResultsPtr(new _ResultsEntry(*__cu));
		// if a state is updated, it's outgoing neighbors should be
		// reconsidered too. Push them to the queue.
		_M_stack._M_push(__v);
	      }
	  };

	  // Identical to DFS's switch part.
	  switch (__state._M_opcode)
	    {
	      // Needs to maintain quantifier count vector here. A quantifier
	      // must be concerned with a alt node.
	      case _S_opcode_alternative:
		{
		  __add_visited_state(__state._M_next);
		  auto& __cu = *_M_covered[__u];
		  auto __back = __cu._M_quant_keys[__state._M_quant_index];
		  __cu._M_inc(__state._M_quant_index, __state._M_neg);
		  __add_visited_state(__state._M_alt);
		  __cu._M_quant_keys[__state._M_quant_index] = __back;
		}
		break;
	      case _S_opcode_subexpr_begin:
		{
		  auto& __sub = (*_M_covered[__u])[__state._M_subexpr];
		  if (!__sub.matched || __sub.first != __current)
		    {
		      auto __back = __sub.first;
		      __sub.first = __current;
		      __add_visited_state(__state._M_next);
		      __sub.first = __back;
		    }
		}
		break;
	      case _S_opcode_subexpr_end:
		{
		  auto& __cu = *_M_covered[__u];
		  auto __back = __cu[__state._M_subexpr];
		  __cu[__state._M_subexpr].second = __current;
		  __cu[__state._M_subexpr].matched = true;
		  __add_visited_state(__state._M_next);
		  __cu[__state._M_subexpr] = __back;
		}
		break;
	      case _S_opcode_line_begin_assertion:
		if (this->_M_at_begin())
		  __add_visited_state(__state._M_next);
		break;
	      case _S_opcode_line_end_assertion:
		if (this->_M_at_end())
		  __add_visited_state(__state._M_next);
		break;
	      case _S_opcode_word_boundry:
		if (this->_M_word_boundry(__state) == !__state._M_neg)
		  __add_visited_state(__state._M_next);
		break;
	      case _S_opcode_subexpr_lookahead:
		if (this->_M_lookahead(__state) == !__state._M_neg)
		  __add_visited_state(__state._M_next);
		break;
	      case _S_opcode_match:
		_M_match_stack._M_push(__u);
		break;
	      case _S_opcode_accept:
		break;
	      default:
		_GLIBCXX_DEBUG_ASSERT(false);
	    }
	}
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    void _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_move()
    {
      decltype(_M_covered) __next;
      while (!_M_match_stack._M_empty())
	{
	  auto __u = _M_match_stack._M_pop();
	  const auto& __state = _M_nfa[__u];
	  auto& __cu = _M_covered[__u];
	  if (__state._M_matches(*this->_M_current)
	      && (__next.count(__state._M_next) == 0
		  || *__cu < *__next[__state._M_next]))
	    {
	      __next[__state._M_next] = std::move(__cu);
	      _M_stack._M_push(__state._M_next);
	    }
	}
      _M_covered = move(__next);
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_includes_some()
    {
      bool __succ = false;
      for (auto __u : _M_nfa._M_final_states())
	if (_M_covered.count(__u))
	  {
	    __succ = true;
	    auto& __cu = _M_covered[__u];
	    if (_M_cur_results == nullptr || *__cu < *_M_cur_results)
	      _M_cur_results = _ResultsPtr(new _ResultsEntry(*__cu));
	  }
      return __succ;
    }

  // Return whether now is at some word boundry.
  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _Executor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_word_boundry(_State<_CharT, _TraitsT> __state) const
    {
      // By definition.
      bool __ans = false;
      auto __pre = _M_current;
      --__pre;
      if (!(_M_at_begin() && _M_at_end()))
	{
	  if (_M_at_begin())
	    __ans = _M_is_word(*_M_current)
	      && !(_M_flags & regex_constants::match_not_bow);
	  else if (_M_at_end())
	    __ans = _M_is_word(*__pre)
	      && !(_M_flags & regex_constants::match_not_eow);
	  else
	    __ans = _M_is_word(*_M_current)
	      != _M_is_word(*__pre);
	}
      return __ans;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    void _Executor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_set_results(_ResultsVec& __cur_results)
    {
      for (size_t __i = 0; __i < __cur_results.size(); ++__i)
	if (__cur_results[__i].matched)
	  _M_results[__i] = __cur_results[__i];
    }

  enum class _RegexExecutorPolicy : int
    { _S_auto, _S_force_dfs };

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT,
    _RegexExecutorPolicy __policy>
    std::unique_ptr<_Executor<_BiIter, _Alloc, _CharT, _TraitsT>>
    __get_executor(_BiIter __b,
		   _BiIter __e,
		   std::vector<sub_match<_BiIter>, _Alloc>& __m,
		   const basic_regex<_CharT, _TraitsT>& __re,
		   regex_constants::match_flag_type __flags)
    {
      typedef std::unique_ptr<_Executor<_BiIter, _Alloc, _CharT, _TraitsT>>
	_ExecutorPtr;
      typedef _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _DFSExecutorT;
      typedef _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _BFSExecutorT;
      auto __p = std::static_pointer_cast<_NFA<_CharT, _TraitsT>>
	(__re._M_automaton);
      if (__policy == _RegexExecutorPolicy::_S_force_dfs
	  || (__policy == _RegexExecutorPolicy::_S_auto && __p->_M_has_backref))
	return _ExecutorPtr(new _DFSExecutorT(__b, __e, __m, __re, __flags));
      return _ExecutorPtr(new _BFSExecutorT(__b, __e, __m, __re, __flags));
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
