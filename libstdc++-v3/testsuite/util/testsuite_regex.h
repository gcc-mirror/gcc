// -*- C++ -*-
// regex utils for the C++ library testsuite.
//
// Copyright (C) 2012-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <regex>
#include <stdexcept>
#include <iostream>

#ifndef _TESTSUITE_REGEX_H
#define _TESTSUITE_REGEX_H 1

namespace __gnu_test
{
  // Test on a compilation of simple expressions, throw regex_error on error.
  typedef std::regex				regex_type;
  typedef regex_type::flag_type			flag_type;
  typedef std::regex_constants::match_flag_type	match_flag_type;
  typedef std::regex_constants::error_type	error_type;
  typedef std::size_t				size_type;
  typedef std::string				string_type;
  using std::basic_regex;
  using std::match_results;

  // Utilities
  struct regex_expected_fail { };

  const error_type regex_error_internal(static_cast<error_type>(-1));

  // Stringify error codes for text logging.
  const char* regex_error_codes[] =
    {
    "error_collate",
    "error_ctype",
    "error_escape",
    "error_backref",
    "error_brack",
    "error_paren",
    "error_brace",
    "error_badbrace",
    "error_range",
    "error_space",
    "error_badrepeat",
    "error_complexity",
    "error_stack"
  };

  void
  show_regex_error_codes()
  {
    using namespace std;
    using namespace std::regex_constants;
    const char tab('\t');
    cout << "error_collate =   " << tab << error_collate << endl;
    cout << "error_ctype =     " << tab << error_ctype << endl;
    cout << "error_escape =    " << tab << error_escape << endl;
    cout << "error_backref =   " << tab << error_backref << endl;
    cout << "error_brack =     " << tab << error_brack << endl;
    cout << "error_paren =     " << tab << error_paren << endl;
    cout << "error_brace =     " << tab << error_brace << endl;
    cout << "error_badbrace =  " << tab << error_badbrace << endl;
    cout << "error_range =     " << tab << error_range << endl;
    cout << "error_space =     " << tab << error_space << endl;
    cout << "error_badrepeat = " << tab << error_badrepeat << endl;
    cout << "error_complexity =" << tab << error_complexity << endl;
    cout << "error_stack =     " << tab << error_stack << endl;
  }

  // Arguments
  // string __res: the regular expression string
  // flag_type __f: flag
  // __error: expected error, if any
  void
  regex_sanity_check(const string_type& __res,
		     flag_type __f = regex_type::basic,
		     error_type __error =  regex_error_internal)
  {
    using namespace std;

    try
      {
	regex_type reo(__res, __f);
	auto n = reo.mark_count();
	cout << "regex_type::mark_count " << n << endl;
      }
    catch (const regex_error& e)
      {
	cout << "regex_sanity_check: "  << __res << endl;
	cout << "regex_error::what " << e.what() << endl;

	show_regex_error_codes();
	cout << "regex_error::code " << regex_error_codes[e.code()] << endl;
	
	if (__error != regex_error_internal)
	  {
	    // Then expected error_type is __error. Check.
	    if (__error != e.code())
	      {
		throw regex_expected_fail();
	      }
	  }
	throw;
      }
    catch (const logic_error& e)
      {
	cout << "logic_error::what " << e.what() << endl;
	throw;
      }
    catch (const std::exception& e)
      {
	cout << "exception: " << endl;
	throw;
      }
  }

  // regex_match_debug behaves like regex_match, but will run *two* executors
  // (if there's no back-reference) and check if their results agree. If not,
  // an exception throws. One can use them just in the way of using regex_match.
  template<typename _Bi_iter, typename _Alloc,
	   typename _Ch_type, typename _Rx_traits>
    bool
    regex_match_debug(_Bi_iter                                 __s,
		      _Bi_iter                                 __e,
		      match_results<_Bi_iter, _Alloc>&         __m,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __flags
		      = std::regex_constants::match_default)
    {
      using namespace std::__detail;
      auto __res1 = __regex_algo_impl<_Bi_iter, _Alloc, _Ch_type, _Rx_traits,
	   _RegexExecutorPolicy::_S_auto, true>
	(__s, __e, __m, __re, __flags);
      match_results<_Bi_iter, _Alloc> __mm;
      auto __res2 = __regex_algo_impl<_Bi_iter, _Alloc, _Ch_type, _Rx_traits,
	   _RegexExecutorPolicy::_S_alternate, true>
	(__s, __e, __mm, __re, __flags);
      // __m is unspecified if return value is false.
      if (__res1 == __res2 && (!__res1 || __m == __mm))
	return __res1;
      throw(std::exception());
    }

  // No match_results version
  template<typename _Bi_iter, typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_match_debug(_Bi_iter                                 __first,
		      _Bi_iter                                 __last,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __flags
		      = std::regex_constants::match_default)
    {
      match_results<_Bi_iter> __what;
      return regex_match_debug(__first, __last, __what, __re, __flags);
    }

  // C-string version
  template<typename _Ch_type, typename _Alloc, typename _Rx_traits>
    inline bool
    regex_match_debug(const _Ch_type*                          __s,
		      match_results<const _Ch_type*, _Alloc>&  __m,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __f
		      = std::regex_constants::match_default)
      { return regex_match_debug(__s, __s + _Rx_traits::length(__s),
				 __m, __re, __f); }

  // C-string version without match_results
  template<typename _Ch_type, class _Rx_traits>
    inline bool
    regex_match_debug(const _Ch_type*                          __s,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __f
		      = std::regex_constants::match_default)
      { return regex_match_debug(__s, __s + _Rx_traits::length(__s),
				 __re, __f); }

  // std::basic_string version
  template<typename _Ch_traits, typename _Ch_alloc,
           typename _Alloc, typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_match_debug(const std::basic_string<_Ch_type, _Ch_traits,
			_Ch_alloc>& __s,
		      match_results<typename std::basic_string<_Ch_type,
			_Ch_traits, _Ch_alloc>::const_iterator,
			_Alloc>& __m,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __flags
		      = std::regex_constants::match_default)
      { return regex_match_debug(__s.begin(), __s.end(),
				 __m, __re, __flags); }

  // std::basic_string version without match_results
  template<typename _Ch_traits, typename _Str_allocator,
           typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_match_debug(const std::basic_string<_Ch_type, _Ch_traits,
		      _Str_allocator>&                         __s,
		      const basic_regex<_Ch_type, _Rx_traits>& __re,
		      match_flag_type                          __flags
		      = std::regex_constants::match_default)
    { return regex_match_debug(__s.begin(), __s.end(), __re, __flags); }

  // regex_match_debug behaves like regex_match, but will run *two* executors
  // (if there's no back-reference) and check if their results agree. If not,
  // an exception throws. One can use them just in the way of using regex_match.
  template<typename _Bi_iter, typename _Alloc,
           typename _Ch_type, typename _Rx_traits>
    bool
    regex_search_debug(_Bi_iter                                 __s,
		       _Bi_iter                                 __e,
		       match_results<_Bi_iter, _Alloc>&         __m,
		       const basic_regex<_Ch_type, _Rx_traits>& __re,
		       match_flag_type   __flags
		       = std::regex_constants::match_default)
    {
      using namespace std::__detail;
      auto __res1 = __regex_algo_impl<_Bi_iter, _Alloc, _Ch_type, _Rx_traits,
	   _RegexExecutorPolicy::_S_auto, false>
        (__s, __e, __m, __re, __flags);
      match_results<_Bi_iter, _Alloc> __mm;
      auto __res2 = __regex_algo_impl<_Bi_iter, _Alloc, _Ch_type, _Rx_traits,
	   _RegexExecutorPolicy::_S_alternate, false>
        (__s, __e, __mm, __re, __flags);
      if (__res1 == __res2 && __m == __mm)
        return __res1;
      throw(std::exception()); // Let test fail. Give it a name.
    }

  // No match_results version
  template<typename _Bi_iter, typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_search_debug(_Bi_iter                                 __first,
		       _Bi_iter                                 __last,
		       const basic_regex<_Ch_type, _Rx_traits>& __re,
		       match_flag_type                          __flags
		       = std::regex_constants::match_default)
    {
      match_results<_Bi_iter> __what;
      return regex_search_debug(__first, __last, __what, __re, __flags);
    }

  // C-string version
  template<typename _Ch_type, class _Alloc, class _Rx_traits>
    inline bool
    regex_search_debug(const _Ch_type*                          __s,
		       match_results<const _Ch_type*, _Alloc>&  __m,
		       const basic_regex<_Ch_type, _Rx_traits>& __e,
		       match_flag_type                          __f
		       = std::regex_constants::match_default)
    { return regex_search_debug(__s, __s + _Rx_traits::length(__s),
				__m, __e, __f); }

  // C-string version without match_results
  template<typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_search_debug(const _Ch_type*                          __s,
		       const basic_regex<_Ch_type, _Rx_traits>& __e,
		       match_flag_type                          __f
		       = std::regex_constants::match_default)
    { return regex_search_debug(__s, __s + _Rx_traits::length(__s),
				__e, __f); }

  // std::basic_string version
  template<typename _Ch_traits, typename _Ch_alloc,
           typename _Alloc, typename _Ch_type,
           typename _Rx_traits>
    inline bool
    regex_search_debug(const std::basic_string<_Ch_type, _Ch_traits,
		       _Ch_alloc>& __s,
		       match_results<typename std::basic_string<_Ch_type,
		       _Ch_traits, _Ch_alloc>::const_iterator, _Alloc>&
		       __m,
		       const basic_regex<_Ch_type, _Rx_traits>& __e,
		       match_flag_type                          __f
		       = std::regex_constants::match_default)
    { return regex_search_debug(__s.begin(), __s.end(), __m, __e, __f); }

  // std::basic_string version without match_results
  template<typename _Ch_traits, typename _String_allocator,
           typename _Ch_type, typename _Rx_traits>
    inline bool
    regex_search_debug(const std::basic_string<_Ch_type, _Ch_traits,
		       _String_allocator>&                      __s,
		       const basic_regex<_Ch_type, _Rx_traits>& __e,
		       match_flag_type                          __f
		       = std::regex_constants::match_default)
    { return regex_search_debug(__s.begin(), __s.end(), __e, __f); }

} // namespace __gnu_test
#endif
