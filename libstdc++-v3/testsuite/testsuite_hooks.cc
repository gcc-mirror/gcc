// -*- C++ -*-
// Utility subroutines for the C++ library testsuite. 
//
// Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <testsuite_hooks.h>

#ifdef _GLIBCXX_RES_LIMITS
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include <list>
#include <string>
#include <stdexcept>
#include <clocale>
#include <locale>
#include <cxxabi.h>

namespace __gnu_test
{
#ifdef _GLIBCXX_RES_LIMITS
  void 
  set_memory_limits(float size)
  {
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit = (__typeof__ (r.rlim_cur))(size * 1048576);

    // Heap size, seems to be common.
#if _GLIBCXX_HAVE_LIMIT_DATA
    getrlimit(RLIMIT_DATA, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_DATA, &r);
#endif

    // Resident set size.
#if _GLIBCXX_HAVE_LIMIT_RSS
    getrlimit(RLIMIT_RSS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_RSS, &r);
#endif

    // Mapped memory (brk + mmap).
#if _GLIBCXX_HAVE_LIMIT_VMEM
    getrlimit(RLIMIT_VMEM, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_VMEM, &r);
#endif

    // Virtual memory.
    // On HP-UX 11.23, a trivial C++ program that sets RLIMIT_AS to
    // anything less than 128MB cannot "malloc" even 1K of memory.
    // Therefore, we skip RLIMIT_AS on HP-UX.
#if _GLIBCXX_HAVE_LIMIT_AS && !defined(__hpux__)
    getrlimit(RLIMIT_AS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_AS, &r);
#endif
  }

#else
  void
  set_memory_limits(float) { }
#endif 

#ifdef _GLIBCXX_RES_LIMITS
  void 
  set_file_limit(unsigned long size)
  {
#if _GLIBCXX_HAVE_LIMIT_FSIZE
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit = (__typeof__ (r.rlim_cur))(size);

    getrlimit(RLIMIT_FSIZE, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_FSIZE, &r);
#endif
  }

#else
  void
  set_file_limit(unsigned long) { }
#endif 

  void 
  verify_demangle(const char* mangled, const char* wanted)
  {
    int status = 0;
    const char* s = abi::__cxa_demangle(mangled, 0, 0, &status);
    if (!s)
      {
	switch (status)
	  {
	  case 0:
	    s = "error code = 0: success";
	    break;
	  case -1:
	    s = "error code = -1: memory allocation failure";
	    break;
	  case -2:
	    s = "error code = -2: invalid mangled name";
	    break;
	  case -3:
	    s = "error code = -3: invalid arguments";
	    break;
	  default:
	    s = "error code unknown - who knows what happened";
	  }
      }

    std::string w(wanted);
    if (w != s)
      __throw_exception_again std::runtime_error(std::string(s));
  }

  
  // Useful exceptions.
  class locale_data : public std::runtime_error 
  {
  public:
    explicit 
    locale_data(const std::string&  __arg) : runtime_error(__arg) { }
  };

  class environment_variable: public std::runtime_error 
  {
  public:
    explicit 
    environment_variable(const std::string&  __arg) : runtime_error(__arg) { }
  };

  class not_found : public std::runtime_error 
  {
  public:
    explicit 
    not_found(const std::string&  __arg) : runtime_error(__arg) { }
  };

  void 
  run_tests_wrapped_locale(const char* name, const func_callback& l)
  {
    using namespace std;
    bool test = true;
    
    // Set the global locale. 
    locale loc_name = try_named_locale(name);
    locale orig = locale::global(loc_name);

    const char* res = setlocale(LC_ALL, name);
    if (res != NULL)
      {
	string preLC_ALL = res;
	const func_callback::test_type* tests = l.tests();
	for (int i = 0; i < l.size(); ++i)
	  (*tests[i])();
	string postLC_ALL= setlocale(LC_ALL, NULL);
	VERIFY( preLC_ALL == postLC_ALL );
      }
    else
      __throw_exception_again
	environment_variable(string("LC_ALL for ") + string(name));
  }
  
  void 
  run_tests_wrapped_env(const char* name, const char* env,
			const func_callback& l)
  {
    using namespace std;
    bool test = true;
    
#ifdef _GLIBCXX_HAVE_SETENV 
    // Set the global locale. 
    locale loc_name = try_named_locale(name);
    locale orig = locale::global(loc_name);

    // Set environment variable env to value in name. 
    const char* oldENV = getenv(env);
    if (!setenv(env, name, 1))
      {
	const func_callback::test_type* tests = l.tests();
	for (int i = 0; i < l.size(); ++i)
	  (*tests[i])();
	setenv(env, oldENV ? oldENV : "", 1);
      }
    else
      __throw_exception_again
	environment_variable(string(env) + string(" to ") + string(name));
#endif
  }

  std::locale 
  try_named_locale(const char* name)
  {
    try
      {
	return std::locale(name);
      }
#ifdef __EXCEPTIONS
    catch (std::runtime_error& ex)
      {
	// Thrown by generic and gnu implemenation if named locale fails.
	if (std::strstr(ex.what(), "name not valid"))
	  exit(0);
	else
	  throw;
      }
#endif
  }

  int
  try_mkfifo (const char* filename, mode_t mode)
  {
#if defined (_NEWLIB_VERSION) || defined (__MINGW32_VERSION)
    /* Newlib and MinGW32 do not have mkfifo.  */
    exit(0);
#else
    return mkfifo(filename, mode);
#endif
  }

  counter::size_type  counter::count = 0;
  unsigned int copy_constructor::count_ = 0;
  unsigned int copy_constructor::throw_on_ = 0;
  unsigned int assignment_operator::count_ = 0;
  unsigned int assignment_operator::throw_on_ = 0;
  unsigned int destructor::_M_count = 0;
  int copy_tracker::next_id_ = 0;
}; // namespace __gnu_test

namespace std
{
  // Member specializations for the existing facet classes.  
  // NB: This isn't especially portable. Perhaps a better way would be
  // to just specialize all of numpunct and ctype.
  using __gnu_test::int_type;
  using __gnu_test::value_type;
  using __gnu_test::pod_type;

  template<>
    bool 
    ctype<pod_type>::
    do_is(mask, char_type) const { return true; }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_is(const char_type* __lo, const char_type*, mask*) const
    { return __lo; }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_scan_is(mask, const char_type* __lo, const char_type*) const
    { return __lo; }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_scan_not(mask, const char_type* __lo, const char_type*) const
    { return __lo; }

  template<>
    pod_type 
    ctype<pod_type>::
    do_toupper(char_type __c) const
    { return __c; }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_toupper(char_type*, const char_type* __hi) const
    { return __hi; }

  template<>
    pod_type 
    ctype<pod_type>::
    do_tolower(char_type __c) const
    { return __c; }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_tolower(char_type*, const char_type* __hi) const
    { return __hi; }

  template<>
    pod_type
    ctype<pod_type>::
    do_widen(char __c) const
    { 
      char_type ret = { value_type(__c) };
      return ret;
    }

  template<>
    const char*
    ctype<pod_type>::
    do_widen(const char* __lo, const char* __hi, char_type* __dest) const
    {
      while (__lo < __hi)
	{
	  *__dest = this->do_widen(*__lo);
	  ++__lo;
	  ++__dest;
	}
      return __hi;
    }

  template<>
    char
    ctype<pod_type>::
    do_narrow(char_type __wc, char) const
    { return static_cast<char>(__wc.value); }

  template<>
    const pod_type*
    ctype<pod_type>::
    do_narrow(const pod_type* __lo, const pod_type* __hi, 
	      char, char* __dest) const
    {
      while (__lo < __hi)
	{
	  *__dest = this->do_narrow(*__lo, char());
	  ++__lo;
	  ++__dest;
	}
      return __hi;
    }

  template<>
    ctype<pod_type>::~ctype() { }

  template<>
    void
    numpunct<pod_type>::_M_initialize_numpunct(__c_locale)
    {
      if (!_M_data)
	_M_data = new __numpunct_cache<pod_type>;

      _M_data->_M_grouping = "";
      _M_data->_M_use_grouping = false;

      _M_data->_M_decimal_point.value =  value_type('.');
      _M_data->_M_thousands_sep.value = value_type(',');
      
      for (size_t i = 0; i < __num_base::_S_oend; ++i)
	{
	  value_type v = __num_base::_S_atoms_out[i];
	  _M_data->_M_atoms_out[i].value = v;
	}
      _M_data->_M_atoms_out[__num_base::_S_oend] = pod_type();
      
      for (size_t j = 0; j < __num_base::_S_iend; ++j)
	_M_data->_M_atoms_in[j].value = value_type(__num_base::_S_atoms_in[j]);
      _M_data->_M_atoms_in[__num_base::_S_iend] = pod_type();

      // "true"
      pod_type* __truename = new pod_type[4 + 1];
      __truename[0].value = value_type('t');
      __truename[1].value = value_type('r');
      __truename[2].value = value_type('u');
      __truename[3].value = value_type('e');
      __truename[4] = pod_type();
      _M_data->_M_truename = __truename;

      // "false"
      pod_type* __falsename = new pod_type[5 + 1];
      __falsename[0].value = value_type('f');
      __falsename[1].value = value_type('a');
      __falsename[2].value = value_type('l');
      __falsename[3].value = value_type('s');
      __falsename[4].value = value_type('e');
      __falsename[5] = pod_type();
      _M_data->_M_falsename = __falsename;
    }

  template<>
    numpunct<pod_type>::~numpunct()
    { delete _M_data; }
} // namespace std
