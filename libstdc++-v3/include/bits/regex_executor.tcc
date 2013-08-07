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

  // TODO: This is too slow. Try to compile the NFA to a DFA.
  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
  template<bool __match_mode>
    bool _DFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_dfs(_StateIdT __i)
    {
      auto& __current = this->_M_current;
      auto& __end = this->_M_end;
      auto& __results = this->_M_results;
      if (__i == _S_invalid_state_id)
        // This is not that certain. Need deeper investigate.
        return false;
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
          __results.at(__state._M_subexpr).first = __current;
          __ret = _M_dfs<__match_mode>(__state._M_next);
          break;
        case _S_opcode_subexpr_end:
          __ret = _M_dfs<__match_mode>(__state._M_next);
          __results.at(__state._M_subexpr).second = __current;
          __results.at(__state._M_subexpr).matched = __ret;
          break;
        case _S_opcode_match:
          if (__current != __end && __state._M_matches(*__current))
            {
              ++__current;
              __ret = _M_dfs<__match_mode>(__state._M_next);
              --__current;
            }
          break;
        case _S_opcode_accept:
          if (__match_mode)
            __ret = __current == __end;
          else
            __ret = true;
          break;
        default:
          _GLIBCXX_DEBUG_ASSERT(false);
        }
      return __ret;
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
  template<bool __match_mode>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_main_loop()
    {
      while (this->_M_current != this->_M_end)
        {
          if (!__match_mode)
            if (_M_includes_some())
              return true;
          _M_move();
          ++this->_M_current;
          _M_e_closure();
        }
      return _M_includes_some();
    }

  // The SPFA approach.
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
          auto __add_visited_state = [&](_StateIdT __v)
          {
            if (__v == _S_invalid_state_id)
              return;
            if (_M_match_less_than(__u, __v))
              {
                _M_covered[__v] = _ResultsPtr(new _ResultsT(*_M_covered[__u]));
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
                _M_covered[__u]->at(__state._M_subexpr).first = __current;
                __add_visited_state(__state._M_next);
                break;
              case _S_opcode_subexpr_end:
                _M_covered[__u]->at(__state._M_subexpr).second = __current;
                _M_covered[__u]->at(__state._M_subexpr).matched = true;
                __add_visited_state(__state._M_next);
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
            if (_M_match_less_than(__it.first, __state._M_next)
                && __state._M_next != _S_invalid_state_id)
              __next[__state._M_next] = move(__it.second);
        }
      _M_covered = move(__next);
    }

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    bool _BFSExecutor<_BiIter, _Alloc, _CharT, _TraitsT>::
    _M_match_less_than(_StateIdT __u, _StateIdT __v) const
    {
      if (_M_covered.count(__u) == 0)
        return false;
      if (_M_covered.count(__v) > 0)
        return true;
      // TODO: Greedy and Non-greedy support
      return true;
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
      auto __p = std::static_pointer_cast<_NFA<_CharT, _TraitsT>>
        (__re._M_automaton);
      if (__p->_M_has_backref)
        return _ExecutorPtr(new _DFSExecutorT(__b, __e, __m, *__p, __flags));
      return _ExecutorPtr(new _DFSExecutorT(__b, __e, __m, *__p, __flags));
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace
