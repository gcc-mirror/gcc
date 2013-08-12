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

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  template<typename, typename>
    class basic_regex;

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
      typedef match_results<_BiIter, _Alloc>   _ResultsT;
      typedef regex_constants::match_flag_type _FlagT;

      virtual
      ~_Executor()
      { }

      // Set matched when string exactly match the pattern.
      virtual bool
      _M_match() = 0;

      // Set matched when some prefix of the string matches the pattern.
      virtual bool
      _M_search_from_first() = 0;

    protected:
      typedef typename _NFA<_CharT, _TraitsT>::_SizeT _SizeT;
      _Executor(_BiIter    __begin,
                _BiIter    __end,
                _ResultsT& __results,
                _FlagT     __flags,
                _SizeT     __size)
      : _M_current(__begin), _M_end(__end),
        _M_results(__results), _M_flags(__flags)
      {
        __results.resize(__size + 2);
        for (auto __it : __results)
          __it.matched = false;
      }

      _BiIter   _M_current;
      _BiIter   _M_end;
      _ResultsT& _M_results;
      _FlagT    _M_flags;
    };

  template<typename _BiIter, typename _Alloc,
    typename _CharT, typename _TraitsT>
    class _DFSExecutor
    : public _Executor<_BiIter, _Alloc, _CharT, _TraitsT>
    {
    public:
      typedef _Executor<_BiIter, _Alloc, _CharT, _TraitsT> _BaseT;
      typedef _NFA<_CharT, _TraitsT>                       _RegexT;
      typedef typename _BaseT::_ResultsT                    _ResultsT;
      typedef regex_constants::match_flag_type             _FlagT;

      _DFSExecutor(_BiIter        __begin,
                   _BiIter        __end,
                   _ResultsT&      __results,
                   const _RegexT& __nfa,
                   _FlagT         __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count()),
        _M_nfa(__nfa)
      { }

      bool
      _M_match()
      { return _M_dfs<true>(_M_nfa._M_start()); }

      bool
      _M_search_from_first()
      { return _M_dfs<false>(_M_nfa._M_start()); }

    private:
      template<bool __match_mode>
        bool
        _M_dfs(_StateIdT __i);

      const _RegexT& _M_nfa;
    };

  // It's essentially a variant of Single-Source-Shortest-Path problem, where,
  // the matching results is the final distance and should be minimized.
  // Instead of using Dijkstra Algorithm, I pick up the queue-optimizaed
  // (BFS-like) Bellman-Ford algorithm,
  // SPFA(http://en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm).
  //
  // Every entry of _M_covered saves the solution(grouping status) for every
  // matching head. When states transfer, solutions will be compared and
  // deduplicated(based on which greedy mode we have).
  //
  // Time complexity: O(_M_str_cur.size() * _M_nfa.size())
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
      typedef std::unique_ptr<_ResultsT>                   _ResultsPtr;
      typedef regex_constants::match_flag_type             _FlagT;

      _BFSExecutor(_BiIter        __begin,
                   _BiIter        __end,
                   _ResultsT&      __results,
                   const _RegexT& __nfa,
                   _FlagT         __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count()),
        _M_nfa(__nfa)
      {
        if (_M_nfa._M_start() != _S_invalid_state_id)
          _M_covered[_M_nfa._M_start()] =
            _ResultsPtr(new _ResultsT(this->_M_results));
        _M_e_closure();
      }

      bool
      _M_match()
      { return _M_main_loop<true>(); }

      bool
      _M_search_from_first()
      { return _M_main_loop<false>(); }

    private:
      template<bool __match_mode>
        bool
        _M_main_loop();

      void
      _M_e_closure();

      void
      _M_move();

      bool
      _M_match_less_than(_StateIdT __u, _StateIdT __v) const;

      bool
      _M_includes_some() const;

      std::map<_StateIdT, _ResultsPtr>     _M_covered;
      const _RegexT& _M_nfa;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_executor.tcc>
