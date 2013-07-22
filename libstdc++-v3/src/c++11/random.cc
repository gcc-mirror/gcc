// random -*- C++ -*-

// Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include <random>

#ifdef  _GLIBCXX_USE_C99_STDINT_TR1

#if defined __i386__ || defined __x86_64__
# include <cpuid.h>
#endif

#include <cstdio>

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{

  namespace
  {
    static unsigned long
    _M_strtoul(const std::string& __str)
    {
      unsigned long __ret = 5489UL;
      if (__str != "mt19937")
	{
	  const char* __nptr = __str.c_str();
	  char* __endptr;
	  __ret = std::strtoul(__nptr, &__endptr, 0);
	  if (*__nptr == '\0' || *__endptr != '\0')
	    std::__throw_runtime_error(__N("random_device::_M_strtoul"
					   "(const std::string&)"));
	}
      return __ret;
    }

#if (defined __i386__ || defined __x86_64__) && defined _GLIBCXX_X86_RDRAND
    unsigned int
    __attribute__ ((target("rdrnd")))
    __x86_rdrand(void)
    {
      unsigned int retries = 100;
      unsigned int val;

      while (__builtin_ia32_rdrand32_step(&val) == 0)
	if (--retries == 0)
	  std::__throw_runtime_error(__N("random_device::__x86_rdrand(void)"));

      return val;
    }
#endif
  }


  void
  random_device::_M_init(const std::string& token)
  {
    const char *fname = token.c_str();

    if (token == "default")
      {
#if (defined __i386__ || defined __x86_64__) && defined _GLIBCXX_X86_RDRAND
	unsigned int eax, ebx, ecx, edx;
	// Check availability of cpuid and, for now at least, also the
	// CPU signature for Intel's
	if (__get_cpuid_max(0, &ebx) > 0 && ebx == signature_INTEL_ebx)
	  {
	    __cpuid(1, eax, ebx, ecx, edx);
	    if (ecx & bit_RDRND)
	      {
		_M_file = nullptr;
		return;
	      }
	  }
#endif

	fname = "/dev/urandom";
      }
    else if (token != "/dev/urandom" && token != "/dev/random")
    fail:
      std::__throw_runtime_error(__N("random_device::"
				     "random_device(const std::string&)"));

    _M_file = std::fopen(fname, "rb");
    if (! _M_file)
      goto fail;
  }

  void
  random_device::_M_init_pretr1(const std::string& token)
  {
    _M_mt.seed(_M_strtoul(token));
  }

  void
  random_device::_M_fini()
  {
    if (_M_file)
      std::fclose(_M_file);
  }

  random_device::result_type
  random_device::_M_getval()
  {
#if (defined __i386__ || defined __x86_64__) && defined _GLIBCXX_X86_RDRAND
    if (! _M_file)
      return __x86_rdrand();
#endif

    result_type __ret;
#ifdef _GLIBCXX_HAVE_UNISTD_H
    read(fileno(_M_file), reinterpret_cast<void*>(&__ret), sizeof(result_type));
#else
    std::fread(reinterpret_cast<void*>(&__ret), sizeof(result_type),
	       1, _M_file);
#endif
    return __ret;
  }

  random_device::result_type
  random_device::_M_getval_pretr1()
  {
    return _M_mt();
  }

  template class mersenne_twister_engine<
    uint_fast32_t,
    32, 624, 397, 31,
    0x9908b0dfUL, 11,
    0xffffffffUL, 7,
    0x9d2c5680UL, 15,
    0xefc60000UL, 18, 1812433253UL>;
}
#endif
