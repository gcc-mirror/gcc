// Reference-counted COW string instantiations -*- C++ -*-

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 21  Strings library
//

#define _GLIBCXX_USE_CXX11_ABI 0
#include "string-inst.cc"

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

#ifdef  _GLIBCXX_USE_C99_STDINT_TR1
#include <random>
#if defined __i386__ || defined __x86_64__
# include <cpuid.h>
#endif
#include <cstdio>

namespace std _GLIBCXX_VISIBILITY(default)
{
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
    unsigned long __seed = 5489UL;
    if (token != "mt19937")
      {
	const char* __nptr = token.c_str();
	char* __endptr;
	__seed = std::strtoul(__nptr, &__endptr, 0);
	if (*__nptr == '\0' || *__endptr != '\0')
	  std::__throw_runtime_error(__N("random_device::random_device"
					 "(const std::string&)"));
      }
    _M_mt.seed(__seed);
#endif
  }
} // namespace
#endif
