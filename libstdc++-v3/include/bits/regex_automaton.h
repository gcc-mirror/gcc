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
 *  @file bits/regex_automaton.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{regex}
 */

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __detail
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @defgroup regex-detail Base and Implementation Classes
   *  @ingroup regex
   *  @{
   */

  typedef int _StateIdT;
  typedef std::set<_StateIdT> _StateSet;
  static const _StateIdT _S_invalid_state_id  = -1;

  template<typename _CharT>
    using _Matcher = std::function<bool (_CharT)>;

  /// Operation codes that define the type of transitions within the base NFA
  /// that represents the regular expression.
  enum _Opcode
  {
      _S_opcode_unknown       =   0,
      _S_opcode_alternative   =   1,
      _S_opcode_backref       =   2,
      _S_opcode_subexpr_begin =   4,
      _S_opcode_subexpr_end   =   5,
      _S_opcode_match         = 100,
      _S_opcode_accept        = 255
  };

  template<typename _CharT, typename _TraitsT>
    class _State
    {
    public:
      typedef int                        _OpcodeT;
      typedef _Matcher<_CharT>           _MatcherT;

      _OpcodeT     _M_opcode;           // type of outgoing transition
      _StateIdT    _M_next;             // outgoing transition
      union // Since they are mutual exclusive.
      {
        _StateIdT    _M_alt;            // for _S_opcode_alternative
        unsigned int _M_subexpr;        // for _S_opcode_subexpr_*
        unsigned int _M_backref_index;  // for _S_opcode_backref
      };
      _MatcherT    _M_matches;          // for _S_opcode_match

      explicit _State(_OpcodeT __opcode)
      : _M_opcode(__opcode), _M_next(_S_invalid_state_id)
      { }

      _State(const _MatcherT& __m)
      : _M_opcode(_S_opcode_match), _M_next(_S_invalid_state_id),
        _M_matches(__m)
      { }

      _State(_OpcodeT __opcode, unsigned __index)
      : _M_opcode(__opcode), _M_next(_S_invalid_state_id)
      {
        if (__opcode == _S_opcode_subexpr_begin
            || __opcode == _S_opcode_subexpr_end)
          _M_subexpr = __index;
        else if (__opcode == _S_opcode_backref)
          _M_backref_index = __index;
      }

      _State(_StateIdT __next, _StateIdT __alt)
      : _M_opcode(_S_opcode_alternative), _M_next(__next), _M_alt(__alt)
      { }

#ifdef _GLIBCXX_DEBUG
      std::ostream&
      _M_print(std::ostream& ostr) const;

      // Prints graphviz dot commands for state.
      std::ostream&
      _M_dot(std::ostream& __ostr, _StateIdT __id) const;
#endif
    };

  /// Base class for, um, automata.  Could be an NFA or a DFA.  Your choice.
  template<typename _CharT, typename _TraitsT>
    class _Automaton
    {
    public:
      typedef unsigned int _SizeT;

    public:
      virtual _SizeT
      _M_sub_count() const = 0;

#ifdef _GLIBCXX_DEBUG
      virtual std::ostream&
      _M_dot(std::ostream& __ostr) const = 0;
#endif
    };

  template<typename _CharT, typename _TraitsT>
    class _NFA
    : public _Automaton<_CharT, _TraitsT>,
      public std::vector<_State<_CharT, _TraitsT>>
    {
    public:
      typedef _State<_CharT, _TraitsT>            _StateT;
      typedef const _Matcher<_CharT>&             _MatcherT;
      typedef unsigned int                        _SizeT;
      typedef regex_constants::syntax_option_type _FlagT;

      _NFA(_FlagT __f)
      : _M_flags(__f), _M_start_state(0), _M_subexpr_count(0),
      _M_has_backref(false)
      { }

      _FlagT
      _M_options() const
      { return _M_flags; }

      _StateIdT
      _M_start() const
      { return _M_start_state; }

      const _StateSet&
      _M_final_states() const
      { return _M_accepting_states; }

      _SizeT
      _M_sub_count() const
      { return _M_subexpr_count; }

      _StateIdT
      _M_insert_accept()
      {
        this->push_back(_StateT(_S_opcode_accept));
        _M_accepting_states.insert(this->size()-1);
        return this->size()-1;
      }

      _StateIdT
      _M_insert_alt(_StateIdT __next, _StateIdT __alt)
      {
        this->push_back(_StateT(__next, __alt));
        return this->size()-1;
      }

      _StateIdT
      _M_insert_matcher(_MatcherT __m)
      {
        this->push_back(_StateT(__m));
        return this->size()-1;
      }

      _StateIdT
      _M_insert_subexpr_begin()
      {
        auto __id = _M_subexpr_count++;
        _M_paren_stack.push_back(__id);
        this->push_back(_StateT(_S_opcode_subexpr_begin, __id));
        return this->size()-1;
      }

      _StateIdT
      _M_insert_subexpr_end()
      {
        this->push_back(_StateT(_S_opcode_subexpr_end, _M_paren_stack.back()));
        _M_paren_stack.pop_back();
        return this->size()-1;
      }

      _StateIdT
      _M_insert_backref(unsigned int __index);

#ifdef _GLIBCXX_DEBUG
      std::ostream&
      _M_dot(std::ostream& __ostr) const;
#endif

      _FlagT                    _M_flags;
      _StateIdT                 _M_start_state;
      _StateSet                 _M_accepting_states;
      _SizeT                    _M_subexpr_count;
      bool                      _M_has_backref;
      std::vector<unsigned int> _M_paren_stack;
    };

  /// Describes a sequence of one or more %_State, its current start
  /// and end(s).  This structure contains fragments of an NFA during
  /// construction.
  template<typename _CharT, typename _TraitsT>
    class _StateSeq
    {
    public:
      typedef _NFA<_CharT, _TraitsT> _RegexT;
    public:
      // Constructs a single-node sequence
      _StateSeq(_RegexT& __ss, _StateIdT __s,
                _StateIdT __e = _S_invalid_state_id)
      : _M_nfa(__ss), _M_start(__s), _M_end1(__s), _M_end2(__e)
      { }
      // Constructs a split sequence from two other sequencces
      _StateSeq(const _StateSeq& __e1, const _StateSeq& __e2)
      : _M_nfa(__e1._M_nfa),
        _M_start(_M_nfa._M_insert_alt(__e1._M_start, __e2._M_start)),
        _M_end1(__e1._M_end1), _M_end2(__e2._M_end1)
      { }

      // Constructs a split sequence from a single sequence
      _StateSeq(const _StateSeq& __e, _StateIdT __id)
      : _M_nfa(__e._M_nfa),
        _M_start(_M_nfa._M_insert_alt(__id, __e._M_start)),
        _M_end1(__id), _M_end2(__e._M_end1)
      { }

      // Constructs a copy of a %_StateSeq
      _StateSeq(const _StateSeq& __rhs)
      : _M_nfa(__rhs._M_nfa), _M_start(__rhs._M_start),
        _M_end1(__rhs._M_end1), _M_end2(__rhs._M_end2)
      { }

      _StateSeq& operator=(const _StateSeq& __rhs);

      _StateIdT
      _M_front() const
      { return _M_start; }

      // Extends a sequence by one.
      void
      _M_push_back(_StateIdT __id);

      // Extends and maybe joins a sequence.
      void
      _M_append(_StateIdT __id);

      void
      _M_append(_StateSeq& __rhs);

      // Clones an entire sequence.
      _StateIdT
      _M_clone();

    private:
      _RegexT&  _M_nfa;
      _StateIdT _M_start;
      _StateIdT _M_end1;
      _StateIdT _M_end2;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_automaton.tcc>
