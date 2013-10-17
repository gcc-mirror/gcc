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

// TODO Put _DFSExecutor and _BFSExecutor into one class. They are becoming
// much more similar. Also, make grouping seperated. The
// regex_constants::nosubs enables much more simpler execution.

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
      typedef basic_regex<_CharT, _TraitsT>           _RegexT;
      typedef std::vector<sub_match<_BiIter>, _Alloc> _ResultsVec;
      typedef regex_constants::match_flag_type        _FlagT;
      typedef typename _TraitsT::char_class_type      _ClassT;

    public:
      _Executor(_BiIter         __begin,
		_BiIter         __end,
		_ResultsVec&    __results,
		const _RegexT&  __re,
		_FlagT          __flags)
      : _M_begin(__begin),
      _M_end(__end),
      _M_re(__re),
      _M_results(__results),
      _M_flags((__flags & regex_constants::match_prev_avail)
	       ? (__flags
		  & ~regex_constants::match_not_bol
		  & ~regex_constants::match_not_bow)
	       : __flags)
      { }

      virtual
      ~_Executor()
      { }

      // Set matched when string exactly match the pattern.
      bool
      _M_match()
      {
	_M_match_mode = true;
	_M_init(_M_begin);
	return _M_main();
      }

      // Set matched when some prefix of the string matches the pattern.
      bool
      _M_search_from_first()
      {
	_M_match_mode = false;
	_M_init(_M_begin);
	return _M_main();
      }

      bool
      _M_search();

      bool
      _M_is_word(_CharT __ch) const
      {
	static const _CharT __s = 'w';
	return _M_re._M_traits.isctype
	  (__ch, _M_re._M_traits.lookup_classname(&__s, &__s+1));
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
      _M_word_boundry(_State<_CharT, _TraitsT> __state) const;

      virtual std::unique_ptr<_Executor>
      _M_clone() const = 0;

      // Return whether now match the given sub-NFA.
      bool
      _M_lookahead(_State<_CharT, _TraitsT> __state) const
      {
	auto __sub = this->_M_clone();
	__sub->_M_set_start(__state._M_alt);
	return __sub->_M_search_from_first();
      }

      void
      _M_set_results(_ResultsVec& __cur_results);

    public:
      virtual void
      _M_init(_BiIter __cur) = 0;

      virtual void
      _M_set_start(_StateIdT __start) = 0;

      virtual bool
      _M_main() = 0;

      _BiIter         _M_current;
      const _BiIter   _M_begin;
      const _BiIter   _M_end;
      const _RegexT&  _M_re;
      _ResultsVec&    _M_results;
      _FlagT          _M_flags;
      bool            _M_match_mode;
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
      typedef _NFA<_CharT, _TraitsT>                       _NFAT;
      typedef typename _BaseT::_RegexT                     _RegexT;
      typedef typename _BaseT::_ResultsVec                 _ResultsVec;
      typedef typename _BaseT::_FlagT                      _FlagT;

    public:
      _DFSExecutor(_BiIter         __begin,
		   _BiIter         __end,
		   _ResultsVec&    __results,
		   const _RegexT&  __re,
		   _FlagT          __flags)
      : _BaseT(__begin, __end, __results, __re, __flags),
      _M_nfa(*std::static_pointer_cast<_NFA<_CharT, _TraitsT>>
	     (__re._M_automaton)),
      _M_start_state(_M_nfa._M_start())
      { }

    private:
      void
      _M_init(_BiIter __cur)
      {
	_M_cur_results.resize(_M_nfa._M_sub_count() + 2);
	this->_M_current = __cur;
      }

      void
      _M_set_start(_StateIdT __start)
      { _M_start_state = __start; }

      bool
      _M_main()
      { return _M_dfs(this->_M_start_state); }

      bool
      _M_dfs(_StateIdT __start);

      std::unique_ptr<_BaseT>
      _M_clone() const
      {
	return std::unique_ptr<_BaseT>(new _DFSExecutor(this->_M_current,
							this->_M_end,
							this->_M_results,
							this->_M_re,
							this->_M_flags));
      }

      // To record current solution.
      _ResultsVec     _M_cur_results;
      const _NFAT&    _M_nfa;
      _StateIdT       _M_start_state;
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
      typedef _NFA<_CharT, _TraitsT>                       _NFAT;
      typedef typename _BaseT::_RegexT                     _RegexT;
      typedef typename _BaseT::_ResultsVec                 _ResultsVec;
      typedef typename _BaseT::_FlagT                      _FlagT;
      // Here's a solution for greedy/ungreedy mode in BFS approach. We need to
      // carefully work out how to compare to conflict matching states.
      //
      // A matching state is a pair(where, when); `where` is a NFA node; `when`
      // is a _BiIter, indicating which char is the next to be matched. Two
      // matching states conflict if they have equivalent `where` and `when`.
      //
      // Now we need to drop one and keep another, because at most one of them
      // could be the final optimal solution. This behavior is affected by
      // greedy policy.
      //
      // The definition of `greedy`:
      // For the sequence of quantifiers in NFA sorted by their start positions,
      // now maintain a vector in every matching state, with length equal to
      // quantifier seq, recording repeating times of every quantifier. Now to
      // compare two matching states, we just lexically compare these two
      // vectors. To win the compare(to survive), one matching state needs to
      // make its greedy quantifier count larger, and ungreedy quantifiers
      // count smaller.
      //
      // In the implementation, we recorded negtive counts for greedy
      // quantifiers and positive counts of ungreedy ones. Now the implicit
      // operator<() for lexicographical_compare will emit the answer.
      //
      // When two vectors equal, it means the `where`, `when` and quantifier
      // counts are identical, and indicates the same solution; so
      // _ResultsEntry::operator<() just return false.
      struct _ResultsEntry
      : private _ResultsVec
      {
      public:
	_ResultsEntry(size_t __res_sz, size_t __sz)
	: _ResultsVec(__res_sz), _M_quant_keys(__sz)
	{ }

	void
	resize(size_t __n)
	{ _ResultsVec::resize(__n); }

	size_t
	size()
	{ return _ResultsVec::size(); }

	sub_match<_BiIter>&
	operator[](size_t __idx)
	{ return _ResultsVec::operator[](__idx); }

	bool
	operator<(const _ResultsEntry& __rhs) const
	{
	  _GLIBCXX_DEBUG_ASSERT(_M_quant_keys.size()
				== __rhs._M_quant_keys.size());
	  return lexicographical_compare(_M_quant_keys.begin(),
					 _M_quant_keys.end(),
					 __rhs._M_quant_keys.begin(),
					 __rhs._M_quant_keys.end());
	}

	void
	_M_inc(size_t __idx, bool __neg)
	{ _M_quant_keys[__idx] += __neg ? 1 : -1; }

	_ResultsVec&
	_M_get()
	{ return *this; }

      public:
	std::vector<int> _M_quant_keys;
      };
      typedef std::unique_ptr<_ResultsEntry>               _ResultsPtr;

      class _TodoList
      {
      public:
	explicit
	_TodoList(size_t __sz)
	: _M_states(), _M_exists(__sz, false)
	{ }

	void _M_push(_StateIdT __u)
	{
	  _GLIBCXX_DEBUG_ASSERT(__u < _M_exists.size());
	  if (!_M_exists[__u])
	    {
	      _M_exists[__u] = true;
	      _M_states.push_back(__u);
	    }
	}

	_StateIdT _M_pop()
	{
	  auto __ret = _M_states.back();
	  _M_states.pop_back();
	  _M_exists[__ret] = false;
	  return __ret;
	}

	bool _M_empty() const
	{ return _M_states.empty(); }

	void _M_clear()
	{
	  _M_states.clear();
	  _M_exists.assign(_M_exists.size(), false);
	}

      private:
	std::vector<_StateIdT>         _M_states;
	std::vector<bool>              _M_exists;
      };

    public:
      _BFSExecutor(_BiIter         __begin,
		   _BiIter         __end,
		   _ResultsVec&    __results,
		   const _RegexT&  __re,
		   _FlagT          __flags)
      : _BaseT(__begin, __end, __results, __re, __flags),
      _M_nfa(*std::static_pointer_cast<_NFA<_CharT, _TraitsT>>
	     (__re._M_automaton)),
      _M_match_stack(_M_nfa.size()),
      _M_stack(_M_nfa.size()),
      _M_start_state(_M_nfa._M_start())
      { }

    private:
      void
      _M_init(_BiIter __cur)
      {
	this->_M_current = __cur;
	_M_covered.clear();
	_ResultsVec& __res(this->_M_results);
	_M_covered[this->_M_start_state] =
	  _ResultsPtr(new _ResultsEntry(__res.size(),
					_M_nfa._M_quant_count));
	_M_stack._M_push(this->_M_start_state);
      }

      void
      _M_set_start(_StateIdT __start)
      { _M_start_state = __start; }

      bool
      _M_main();

      void
      _M_e_closure();

      void
      _M_move();

      bool
      _M_includes_some();

      std::unique_ptr<_BaseT>
      _M_clone() const
      {
	return std::unique_ptr<_BaseT>(new _BFSExecutor(this->_M_current,
							this->_M_end,
							this->_M_results,
							this->_M_re,
							this->_M_flags));
      }

      const _NFAT&                     _M_nfa;
      std::map<_StateIdT, _ResultsPtr> _M_covered;
      _TodoList                        _M_match_stack;
      _TodoList                        _M_stack;
      _StateIdT                        _M_start_state;
      // To record global optimal solution.
      _ResultsPtr                      _M_cur_results;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_executor.tcc>
