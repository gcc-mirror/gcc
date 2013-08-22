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
  template<bool __match_mode>
    bool _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_dfs(_StateIdT __i)
    {
      if (__i == _S_invalid_state_id)
        // This is not that certain. Need deeper investigate.
        return false;
      auto& __current = this->_M_current;
      auto& __end = this->_M_end;
      auto& __results = _M_results_ret;
      const auto& __state = _M_nfa[__i];
      bool __ret = false;
      switch (__state._M_opcode)
        {
        case _S_opcode_alternative:
          // Greedy mode by default. For non-greedy mode,
          // swap _M_alt and _M_next.
          // TODO: Add greedy mode option.
          __ret = _M_dfs<__match_mode>(__state._M_alt)
            || _M_dfs<__match_mode>(__state._M_next);
          break;
        case _S_opcode_subexpr_begin:
          // Here's the critical part: if there's nothing changed since last
          // visit, do NOT continue. This prevents the executor from get into
          // infinite loop when use "()*" to match "".
          //
          // Every change on __results will be roll back after the recursion
          // step finished.
          if (!__results[__state._M_subexpr].matched
              || __results[__state._M_subexpr].first != __current)
            {
              auto __back = __current;
              __results[__state._M_subexpr].first = __current;
              __ret = _M_dfs<__match_mode>(__state._M_next);
              __results[__state._M_subexpr].first = __back;
            }
          break;
        case _S_opcode_subexpr_end:
          if (__results[__state._M_subexpr].second != __current
              || __results[__state._M_subexpr].matched != true)
            {
              auto __back = __results[__state._M_subexpr];
              __results[__state._M_subexpr].second = __current;
              __results[__state._M_subexpr].matched = true;
              __ret = _M_dfs<__match_mode>(__state._M_next);
              __results[__state._M_subexpr] = __back;
            }
          else
            __ret = _M_dfs<__match_mode>(__state._M_next);
          break;
        case _S_opcode_match:
          if (__current != __end && __state._M_matches(*__current))
            {
              ++__current;
              __ret = _M_dfs<__match_mode>(__state._M_next);
              --__current;
            }
          break;
        // First fetch the matched result from __results as __submatch;
        // then compare it with
        // (__current, __current + (__submatch.second - __submatch.first))
        // If matched, keep going; else just return to try another state.
        case _S_opcode_backref:
          {
            auto& __submatch = __results[__state._M_backref_index];
            if (!__submatch.matched)
              break;
            auto __last = __current;
            for (auto __tmp = __submatch.first;
                 __last != __end && __tmp != __submatch.second;
                 ++__tmp)
              ++__last;
            if (_M_traits.transform(__submatch.first, __submatch.second)
                == _M_traits.transform(__current, __last))
              if (__last != __current)
                {
                  auto __backup = __current;
                  __current = __last;
                  __ret = _M_dfs<__match_mode>(__state._M_next);
                  __current = __backup;
                }
              else
                __ret = _M_dfs<__match_mode>(__state._M_next);
          }
          break;
        case _S_opcode_accept:
          if (__match_mode)
            __ret = __current == __end;
          else
            __ret = true;
          if (__ret)
            this->_M_results = __results;
          break;
        default:
          _GLIBCXX_DEBUG_ASSERT(false);
        }
      return __ret;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
  template<bool __match_mode>
    void _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_main_loop()
    {
      while (this->_M_current != this->_M_end)
        {
          if (!__match_mode)
            if (_M_includes_some())
              return;
          _M_move();
          ++this->_M_current;
          _M_e_closure();
        }
      _M_includes_some();
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    void _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_e_closure()
    {
      auto& __current = this->_M_current;
      std::queue<_StateIdT> __q;
      std::vector<bool> __in_q(_M_nfa.size(), false);
      for (auto& __it : _M_covered)
        {
          __in_q[__it.first] = true;
          __q.push(__it.first);
        }
      while (!__q.empty())
        {
          auto __u = __q.front();
          __q.pop();
          __in_q[__u] = false;
          const auto& __state = _M_nfa[__u];

          // Can be implemented using method, but there're too much arguments.
          // I would use macro function before C++11, but lambda is a better
          // choice, since hopefully compiler can inline it.
          auto __add_visited_state = [&](_StateIdT __v)
          {
            if (__v == _S_invalid_state_id)
              return;
            if (_M_covered.count(__u) != 0
                && (_M_covered.count(__v) == 0
                    || _M_match_less_than(*_M_covered[__u], *_M_covered[__v])))
              {
                _M_covered[__v] = _ResultsPtr(new _ResultsVec(*_M_covered[__u]));
                // if a state is updated, it's outgoing neighbors should be
                // reconsidered too. Push them to the queue.
                if (!__in_q[__v])
                  {
                    __in_q[__v] = true;
                    __q.push(__v);
                  }
              }
          };

          switch (__state._M_opcode)
            {
              case _S_opcode_alternative:
                __add_visited_state(__state._M_next);
                __add_visited_state(__state._M_alt);
                break;
              case _S_opcode_subexpr_begin:
                {
                  auto& __cu = *_M_covered[__u];
                  auto __back = __cu[__state._M_subexpr].first;
                  __cu[__state._M_subexpr].first = __current;
                  __add_visited_state(__state._M_next);
                  __cu[__state._M_subexpr].first = __back;
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
              case _S_opcode_match:
                break;
              case _S_opcode_accept:
                __add_visited_state(__state._M_next);
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
      for (auto& __it : _M_covered)
        {
          const auto& __state = _M_nfa[__it.first];
          if (__state._M_opcode == _S_opcode_match
              && __state._M_matches(*this->_M_current))
            if (__state._M_next != _S_invalid_state_id)
              if (__next.count(__state._M_next) == 0
                  || _M_match_less_than(*__it.second, *__next[__state._M_next]))
                __next[__state._M_next] = move(__it.second);
        }
      _M_covered = move(__next);
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_match_less_than(const _ResultsVec& __u, const _ResultsVec& __v) const
    {
      // TODO: Greedy and Non-greedy support
      _GLIBCXX_DEBUG_ASSERT(__u.size() == __v.size());
      auto __size = __u.size();
      for (auto __i = 0; __i < __size; __i++)
        {
          auto& __uit = __u[__i], __vit = __v[__i];
          if (__uit.matched && !__vit.matched)
            return true;
          if (!__uit.matched && __vit.matched)
            return false;
          if (__uit.matched && __vit.matched)
            {
              // GREEDY
              if (__uit.first != __vit.first)
                return __uit.first < __vit.first;
              if (__uit.second != __vit.second)
                return __uit.second > __vit.second;
            }
        }
      return false;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_includes_some() const
    {
      auto& __s = _M_nfa._M_final_states();
      auto& __t = _M_covered;
      if (__s.size() > 0 && __t.size() > 0)
        {
          auto __first = __s.begin();
          auto __second = __t.begin();
          while (__first != __s.end() && __second != __t.end())
            {
              if (*__first < __second->first)
                ++__first;
              else if (__second->first < *__first)
                ++__second;
              else
                {
                  this->_M_results = *__second->second;
                  return true;
                }
            }
        }
      return false;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    std::unique_ptr<_Executor<_BiIter, _Alloc, _CharT, _TraitsT>>
    __get_executor(_BiIter __b,
                   _BiIter __e,
                   match_results<_BiIter, _Alloc>& __m,
                   const basic_regex<_CharT, _TraitsT>& __re,
                   regex_constants::match_flag_type __flags)
    {
      typedef std::unique_ptr<_Executor<_BiIter, _Alloc, _CharT, _TraitsT>>
        _ExecutorPtr;
      typedef _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _DFSExecutorT;
      typedef _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT> _BFSExecutorT;
      auto __p = std::static_pointer_cast<_NFA<_CharT, _TraitsT>>
        (__re._M_automaton);
      if (__p->_M_has_backref)
        return _ExecutorPtr(new _DFSExecutorT(__b, __e, __m, *__p, __flags));
      return _ExecutorPtr(new _BFSExecutorT(__b, __e, __m, *__p, __flags));
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
