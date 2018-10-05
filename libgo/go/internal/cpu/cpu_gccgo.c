// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>

#if defined(__i386__) || defined(__x86_64__)
#include <cpuid.h>
#include <x86intrin.h>
#endif

#include "runtime.h"

#if defined(__i386__) || defined(__x86_64__)

struct cpuid_ret {
	uint32_t eax;
	uint32_t ebx;
	uint32_t ecx;
	uint32_t edx;
};

struct cpuid_ret cpuid(uint32_t, uint32_t)
  __asm__(GOSYM_PREFIX "internal_cpu.cpuid")
  __attribute__((no_split_stack));

struct cpuid_ret cpuid(uint32_t eaxArg, uint32_t ecxArg) {
	unsigned int eax = 0;
	unsigned int ebx = 0;
	unsigned int ecx = 0;
	unsigned int edx = 0;
	struct cpuid_ret ret;

	__get_cpuid_count(eaxArg, ecxArg, &eax, &ebx, &ecx, &edx);
	ret.eax = (uint32_t)(eax);
	ret.ebx = (uint32_t)(ebx);
	ret.ecx = (uint32_t)(ecx);
	ret.edx = (uint32_t)(edx);
	return ret;
}

struct xgetbv_ret {
	uint32_t eax;
	uint32_t edx;
};

struct xgetbv_ret xgetbv(void)
  __asm__(GOSYM_PREFIX "internal_cpu.xgetbv")
  __attribute__((no_split_stack));

#pragma GCC push_options
#pragma GCC target("xsave")

struct xgetbv_ret xgetbv(void) {
	struct xgetbv_ret ret;

        // At some point, use call to _xgetbv() instead:
        //
        //       long long r = _xgetbv(0);
        //       ret.eax = r & 0xffffffff;
        //       ret.edx = r >> 32;
        //
        unsigned int __eax, __edx, __xcr_no = 0;
        __asm__ ("xgetbv" : "=a" (__eax), "=d" (__edx) : "c" (__xcr_no));
        ret.eax = __eax;
        ret.edx = __edx;
	return ret;
}

#pragma GCC pop_options

#endif /* defined(__i386__) || defined(__x86_64__)  */
