// class template regex -*- C++ -*-

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
 *  @file bits/regex_grep_matcher.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _BiIter>
    class sub_match;

  template<typename _Bi_iter, typename _Allocator>
    class match_results;

_GLIBCXX_END_NAMESPACE_VERSION
  
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @defgroup regex-detail Base and Implementation Classes
   *  @ingroup regex
   *  @{
   */

  /// A _Results facade specialized for wrapping a templated match_results.
  template<typename _FwdIterT, typename _Alloc>
    class _SpecializedResults
    : public _Results
    {
    public:
      _SpecializedResults(const _Automaton::_SizeT __size,
			  const _SpecializedCursor<_FwdIterT>& __cursor,
			  match_results<_FwdIterT, _Alloc>& __m);

      ~_SpecializedResults()
      {
        if (_M_managed)
          delete &_M_results;
      }

    private:
      _SpecializedResults(const _SpecializedResults& __rhs)
      : _M_results(*new match_results<_FwdIterT, _Alloc>(__rhs._M_results)),
      _M_managed(true)
      { }

    public:
      void
      _M_set_pos(int __i, int __j, const _PatternCursor& __pc);

      void
      _M_set_range(int __i, const _PatternCursor& __pc)
      {
        typedef const _SpecializedCursor<_FwdIterT>& _CursorT;
        _CursorT __c = static_cast<_CursorT>(__pc);
        _M_results.at(__i).first = __c._M_begin();
        _M_results.at(__i).second = __c._M_end();
      }

      void
      _M_set_matched(int __i, bool __is_matched)
      { _M_results.at(__i).matched = __is_matched; }

      std::unique_ptr<_Results>
      _M_clone() const
      { return unique_ptr<_Results>(new _SpecializedResults(*this)); }

      void
      _M_assign(const _Results& __rhs)
      {
        auto __r = static_cast<const _SpecializedResults*>(&__rhs);
        _M_results = __r->_M_results;
      }

    private:
      match_results<_FwdIterT, _Alloc>& _M_results;
      bool                              _M_managed;
    };

  template<typename _FwdIterT, typename _Alloc>
    _SpecializedResults<_FwdIterT, _Alloc>::
    _SpecializedResults(const _Automaton::_SizeT __size,
    			const _SpecializedCursor<_FwdIterT>& __cursor,
                        match_results<_FwdIterT, _Alloc>& __m)
    : _M_results(__m), _M_managed(false)
    {
      _M_results.clear();
      _M_results.reserve(__size + 2);
      _M_results.resize(__size);
      typename match_results<_FwdIterT, _Alloc>::value_type __sm;
      __sm.first = __sm.second = __cursor._M_begin();
      _M_results.push_back(__sm);
      __sm.first = __sm.second = __cursor._M_end();
      _M_results.push_back(__sm);
    }

  template<typename _FwdIterT, typename _Alloc>
    void
    _SpecializedResults<_FwdIterT, _Alloc>::
    _M_set_pos(int __i, int __j, const _PatternCursor& __pc)
    { 
      typedef const _SpecializedCursor<_FwdIterT>& _CursorT;
      _CursorT __c = static_cast<_CursorT>(__pc);
      if (__j == 0)
        _M_results.at(__i).first = __c._M_pos();
      else
        _M_results.at(__i).second = __c._M_pos();
    }

  /// Executes a regular expression NFA/DFA over a range using a
  /// variant of the parallel execution algorithm featured in the grep
  /// utility, modified to use Laurikari tags.
  class _Grep_matcher
  {
  public:
    _Grep_matcher(_PatternCursor&                   __p,
                  _Results&                         __r,
                  const _AutomatonPtr&              __automaton,
                  regex_constants::match_flag_type  __flags)
    : _M_nfa(static_pointer_cast<_Nfa>(__automaton)),
      _M_str_cur(__p), _M_results(__r)
    { }

    virtual
    ~_Grep_matcher()
    { }

    // Set matched when string exactly match the pattern.
    virtual bool
    _M_match() = 0;

    // Set matched when some prefix of the string matches the pattern.
    virtual bool
    _M_search_from_first() = 0;

  protected:
    const std::shared_ptr<_Nfa>        _M_nfa;
    _PatternCursor&                    _M_str_cur;
    _Results&                          _M_results;
  };

  // Time complexity: exponential
  // Space complexity: O(_M_str_cur.size())
  // _M_dfs() take a state, along with current string cursor(_M_str_cur),
  // trying to match current state with current character.
  // Only _S_opcode_match will consume a character.
  class _DFSMatcher
  : public _Grep_matcher
  {
  public:
    _DFSMatcher(_PatternCursor&                   __p,
                _Results&                         __r,
                const _AutomatonPtr&              __automaton,
                regex_constants::match_flag_type  __flags)
    : _Grep_matcher(__p, __r, __automaton, __flags)
    { }

    bool
    _M_match()
    { return _M_dfs<true>(_M_nfa->_M_start()); }

    bool
    _M_search_from_first()
    { return _M_dfs<false>(_M_nfa->_M_start()); }

  private:
    template<bool __match_mode>
      bool
      _M_dfs(_StateIdT __i);
  };

  // It's essentially a variant of Single-Source-Shortest-Path problem, where,
  // the matching results is the final distance and should be minimized.
  // Instead of using Dijkstra Algorithm, I pick up the queue-optimizaed
  // (BFS-like) Bellman-Ford algorithm,
  // SPFA(http://en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm).
  //
  // Every entry of _M_current saves the solution(grouping status) for every
  // matching head. When states transfer, solutions will be compared and
  // deduplicated(based on which greedy mode we have).
  //
  // Time complexity: O(_M_str_cur.size() * _M_nfa.size())
  // Space complexity: O(_M_nfa.size() * _M_nfa.mark_count())
  class _BFSMatcher
  : public _Grep_matcher
  {
  public:
    _BFSMatcher(_PatternCursor&                   __p,
                _Results&                         __r,
                const _AutomatonPtr&              __automaton,
                regex_constants::match_flag_type  __flags)
    : _Grep_matcher(__p, __r, __automaton, __flags)
    {
      if (_M_nfa->_M_start() != _S_invalid_state_id)
        _M_current[_M_nfa->_M_start()] = _M_results._M_clone();
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

    std::map<_StateIdT, std::unique_ptr<_Results>>     _M_current;
  };

  std::unique_ptr<_Grep_matcher> _Nfa::
  _M_get_matcher(_PatternCursor&                   __p,
                 _Results&                         __r,
                 const _AutomatonPtr&              __a,
                 regex_constants::match_flag_type  __flags)
  {
    if (_M_has_back_ref)
      return unique_ptr<_Grep_matcher>(
        new _DFSMatcher(__p, __r, __a, __flags));
    else
      return unique_ptr<_Grep_matcher>(
        new _BFSMatcher(__p, __r, __a, __flags));
  }

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_grep_matcher.tcc>
