/*
 * Copyright (c) 1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#ifndef _CPP_STDEXCEPT
#define _CPP_STDEXCEPT 1

#include <bits/std_exception.h>

#if defined(__STL_USE_EXCEPTIONS) || \
    !(defined(_MIPS_SIM) && defined(_ABIO32) && _MIPS_SIM == _ABIO32)

#include <bits/stl_string_fwd.h>

__STL_BEGIN_NAMESPACE

class __Named_exception : public exception {
public:
  __Named_exception(const string& __str);
  virtual const char* what() const __STL_NOTHROW { return _M_name; }

private:
  enum { _S_bufsize = 256 };
  char _M_name[_S_bufsize];
};

class logic_error : public __Named_exception {
public:
  logic_error(const string& __s) : __Named_exception(__s) {}
};

class runtime_error : public __Named_exception {
public:
  runtime_error(const string& __s) : __Named_exception(__s) {}
};

class domain_error : public logic_error {
public:
  domain_error(const string& __arg) : logic_error(__arg) {}
};

class invalid_argument : public logic_error {
public:
  invalid_argument(const string& __arg) : logic_error(__arg) {}
};

class length_error : public logic_error {
public:
  length_error(const string& __arg) : logic_error(__arg) {}
};

class out_of_range : public logic_error {
public:
  out_of_range(const string& __arg) : logic_error(__arg) {}
};

class range_error : public runtime_error {
public:
  range_error(const string& __arg) : runtime_error(__arg) {}
};

class overflow_error : public runtime_error {
public:
  overflow_error(const string& __arg) : runtime_error(__arg) {}
};

class underflow_error : public runtime_error {
public:
  underflow_error(const string& __arg) : runtime_error(__arg) {}
};

__STL_END_NAMESPACE

#endif /* Not o32, and no exceptions */

#endif /* _CPP_STDEXCEPT */

// Local Variables:
// mode:C++
// End:

