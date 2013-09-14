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
      virtual bool
      _M_match() = 0;

      // Set matched when some prefix of the string matches the pattern.
      virtual bool
      _M_search() = 0;

    protected:
      typedef typename _NFA<_CharT, _TraitsT>::_SizeT _SizeT;
      typedef typename _TraitsT::char_class_type      _ClassT;

      _Executor(_BiIter         __begin,
		_BiIter         __end,
		_ResultsT&      __results,
		_FlagT          __flags,
		_SizeT          __size,
		const _TraitsT& __traits)
      : _M_current(__begin), _M_begin(__begin), _M_end(__end),
      _M_results(__results), _M_flags(__flags), _M_traits(__traits)
      {
	__size += 2;
	_M_results.resize(__size);
	for (_SizeT __i = 0; __i < __size; ++__i)
	  _M_results[__i].matched = false;
      }

      bool
      _M_is_word(_CharT __ch)
      {
	static const _CharT __s = 'w';
	return _M_traits.isctype(__ch,
				 _M_traits.lookup_classname(&__s, &__s+1));
      }

      _BiIter         _M_current;
      const _BiIter   _M_begin;
      const _BiIter   _M_end;
      _ResultsVec&    _M_results;
      const _TraitsT& _M_traits;
      _FlagT          _M_flags;
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

      _DFSExecutor(_BiIter         __begin,
		   _BiIter         __end,
		   _ResultsT&      __results,
		   const _RegexT&  __nfa,
		   const _TraitsT& __traits,
		   _FlagT          __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count(),
	       __traits),
	_M_traits(__traits), _M_nfa(__nfa), _M_cur_results(this->_M_results),
	_M_start_state(__nfa._M_start())
      { }

      bool
      _M_match()
      {
	this->_M_current = this->_M_begin;
	return _M_dfs<true>(_M_start_state);
      }

      bool
      _M_search_from_first()
      {
	this->_M_current = this->_M_begin;
	return _M_dfs<false>(_M_start_state);
      }

      bool
      _M_search()
      {
	auto __cur = this->_M_begin;
	do
	  {
	    this->_M_current = __cur;
	    if (_M_dfs<false>(_M_start_state))
	      return true;
	  }
	// Continue when __cur == _M_end
	while (__cur++ != this->_M_end);
	return false;
      }

    private:
      template<bool __match_mode>
	bool
	_M_dfs(_StateIdT __i);

      // To record current solution.
      _ResultsVec     _M_cur_results;
      const _TraitsT& _M_traits;
      const _RegexT&  _M_nfa;
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
      typedef _NFA<_CharT, _TraitsT>                       _RegexT;
      typedef typename _BaseT::_ResultsT                   _ResultsT;
      // Here's a solution for greedy/ungreedy mode in BFS approach. We need to
      // carefully work out how to compare to conflict matching states.
      //
      // A matching state is a pair(where, when); `where` is a NFA node; `when`
      // is a _BiIter, indicating which char is the next to be mathed one.  Two
      // matching states conflict means that they have equivalent `where` and
      // `when`.
      //
      // Now since we need to drop one and keep another, because at most one of
      // them could be the final optimal solution. This behavior is affected by
      // greedy policy.
      //
      // The definition of `greedy`:
      // For the sequence of quantifiers in NFA sorted by there start position,
      // now maintain a vector in a matching state, with equal length to
      // quantifier seq, recording repeating times of every quantifier. Now to
      // compare two matching states, we just lexically compare these two
      // vectors. To win the compare(to survive), one matching state needs to
      // make its greedy quantifier count larger, and ungreedy quantifiers
      // count smaller.
      //
      // In the implementation, we recorded negtive numbers for greedy
      // quantifiers and positive numbers of ungreedy ones. Now a simple
      // operator<() for lexicographical_compare will emit the answer.
      //
      // When two vectors equal, it means the `where`, `when` and quantifier
      // counts are identical, it indicates the same answer, so just return
      // false.
      struct _ResultsEntry
      : private _BaseT::_ResultsVec
      {
      public:
	_ResultsEntry(unsigned int __res_sz, unsigned int __sz)
	: _BaseT::_ResultsVec(__res_sz), _M_quant_keys(__sz)
	{ }

	sub_match<_BiIter>&
	operator[](unsigned int __idx)
	{ return this->_BaseT::_ResultsVec::operator[](__idx); }

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
	_M_inc(unsigned int __idx, bool __neg)
	{ _M_quant_keys[__idx] += __neg ? 1 : -1; }

	typename _BaseT::_ResultsVec
	_M_get()
	{ return *this; }

      public:
	std::vector<int> _M_quant_keys;
      };

      typedef std::unique_ptr<_ResultsEntry>               _ResultsPtr;
      typedef regex_constants::match_flag_type             _FlagT;

      _BFSExecutor(_BiIter         __begin,
		   _BiIter         __end,
		   _ResultsT&      __results,
		   const _RegexT&  __nfa,
		   const _TraitsT& __traits,
		   _FlagT          __flags)
      : _BaseT(__begin, __end, __results, __flags, __nfa._M_sub_count(),
	       __traits),
	_M_nfa(__nfa),
	_M_cur_results(nullptr),
	_M_start_state(__nfa._M_start())
      { }

      bool
      _M_match()
      {
	_M_init(this->_M_begin);
	return _M_main_loop<true>();
      }

      bool
      _M_search_from_first()
      {
	_M_init(this->_M_begin);
	return _M_main_loop<false>();
      }

      bool
      _M_search()
      {
	auto __cur = this->_M_begin;
	do
	  {
	    _M_init(__cur);
	    if (_M_main_loop<false>())
	      return true;
	  }
	// Continue when __cur == _M_end
	while (__cur++ != this->_M_end);
	return false;
      }

    private:
      void
      _M_init(_BiIter __cur)
      {
	_GLIBCXX_DEBUG_ASSERT(_M_start_state != _S_invalid_state_id);
	this->_M_current = __cur;
	_M_covered.clear();
	_M_covered[_M_start_state] =
	  _ResultsPtr(new _ResultsEntry(this->_M_results.size(),
					_M_nfa._M_quant_count));
	_M_e_closure();
      }

      template<bool __match_mode>
	bool
	_M_main_loop();

      void
      _M_e_closure();

      void
      _M_move();

      bool
      _M_includes_some();

      std::map<_StateIdT, _ResultsPtr> _M_covered;
      // To record global optimal solution.
      _ResultsPtr                      _M_cur_results;
      const _RegexT&                   _M_nfa;
      _StateIdT                        _M_start_state;
    };

 //@} regex-detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __detail
} // namespace std

#include <bits/regex_executor.tcc>
