// random -*- C++ -*-

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

#define _GLIBCXX_USE_CXX11_ABI 1
#define _CRT_RAND_S // define this before including <stdlib.h> to get rand_s

#include <random>

#ifdef  _GLIBCXX_USE_C99_STDINT_TR1

#if defined __i386__ || defined __x86_64__
# include <cpuid.h>
#endif

#include <cerrno>
#include <cstdio>

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef _GLIBCXX_HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef _GLIBCXX_HAVE_LINUX_TYPES_H
# include <linux/types.h>
#endif

#ifdef _GLIBCXX_HAVE_LINUX_RANDOM_H
# include <linux/random.h>
#endif

#ifdef _GLIBCXX_USE_CRT_RAND_S
# include <stdlib.h>
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

#ifdef _GLIBCXX_USE_CRT_RAND_S
# pragma GCC poison _M_mt
    unsigned int
    __winxp_rand_s()
    {
      unsigned int val;
      if (::rand_s(&val) != 0)
	std::__throw_runtime_error(__N("random_device: rand_s failed"));
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

    _M_file = static_cast<void*>(std::fopen(fname, "rb"));
    if (!_M_file)
      goto fail;
  }

  void
  random_device::_M_init_pretr1(const std::string& token [[gnu::unused]])
  {
#ifndef _GLIBCXX_USE_CRT_RAND_S
    _M_mt.seed(_M_strtoul(token));
#endif
  }

  void
  random_device::_M_fini()
  {
    if (_M_file)
      std::fclose(static_cast<FILE*>(_M_file));
  }

  random_device::result_type
  random_device::_M_getval()
  {
#if (defined __i386__ || defined __x86_64__) && defined _GLIBCXX_X86_RDRAND
    if (!_M_file)
      return __x86_rdrand();
#endif

    result_type __ret;
    void* p = &__ret;
    size_t n = sizeof(result_type);
#ifdef _GLIBCXX_HAVE_UNISTD_H
    do
      {
	const int e = read(fileno(static_cast<FILE*>(_M_file)), p, n);
	if (e > 0)
	  {
	    n -= e;
	    p = static_cast<char*>(p) + e;
	  }
	else if (e != -1 || errno != EINTR)
	  __throw_runtime_error(__N("random_device could not be read"));
      }
    while (n > 0);
#else
    const size_t e = std::fread(p, n, 1, static_cast<FILE*>(_M_file));
    if (e != 1)
      __throw_runtime_error(__N("random_device could not be read"));
#endif

    return __ret;
  }

  random_device::result_type
  random_device::_M_getval_pretr1()
  {
#ifdef _GLIBCXX_USE_CRT_RAND_S
    return __winxp_rand_s();
#else
    return _M_mt();
#endif
  }

  double
  random_device::_M_getentropy() const noexcept
  {
#if defined _GLIBCXX_HAVE_SYS_IOCTL_H && defined RNDGETENTCNT
    if (!_M_file)
      return 0.0;

    const int fd = fileno(static_cast<FILE*>(_M_file));
    if (fd < 0)
      return 0.0;

    int ent;
    if (ioctl(fd, RNDGETENTCNT, &ent) < 0)
      return 0.0;

    if (ent < 0)
      return 0.0;

    const int max = sizeof(result_type) * __CHAR_BIT__;
    if (ent > max)
      ent = max;

    return static_cast<double>(ent);
#else
    return 0.0;
#endif
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
