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
  __asm__(GOSYM_PREFIX "internal_1cpu.cpuid")
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
  __asm__(GOSYM_PREFIX "internal_1cpu.xgetbv")
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

#ifdef __s390x__

struct facilityList {
	uint64_t bits[4];
};

struct queryResult {
	uint64_t bits[2];
};

struct facilityList stfle(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.stfle")
  __attribute__((no_split_stack));

struct facilityList stfle(void) {
    struct facilityList ret;
    __asm__ ("la    %%r1, %[ret]\t\n"
	     "lghi  %%r0, 3\t\n" // last doubleword index to store
	     "xc    0(32,%%r1), 0(%%r1)\t\n" // clear 4 doublewords (32 bytes)
	     ".long 0xb2b01000\t\n"  // store facility list extended (STFLE)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");
    return ret;
}

struct queryResult kmQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kmQuery")
  __attribute__((no_split_stack));

struct queryResult kmQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n" // set function code to 0 (KM-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb92e0024\t\n" // cipher message (KM)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");
    return ret;
}

struct queryResult kmcQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kmcQuery")
  __attribute__((no_split_stack));

struct queryResult kmcQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n" // set function code to 0 (KMC-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb92f0024\t\n"  // cipher message with chaining (KMC)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");

    return ret;
}

struct queryResult kmctrQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kmctrQuery")
  __attribute__((no_split_stack));

struct queryResult kmctrQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n" // set function code to 0 (KMCTR-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb92d4024\t\n" // cipher message with counter (KMCTR)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");

    return ret;
}

struct queryResult kmaQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kmaQuery")
  __attribute__((no_split_stack));

struct queryResult kmaQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n" // set function code to 0 (KMA-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb9296024\t\n" // cipher message with authentication (KMA)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");

    return ret;
}

struct queryResult kimdQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kimdQuery")
  __attribute__((no_split_stack));

struct queryResult kimdQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n"  // set function code to 0 (KIMD-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb93e0024\t\n"  // compute intermediate message digest (KIMD)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");

    return ret;
}

struct queryResult klmdQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.klmdQuery")
  __attribute__((no_split_stack));

struct queryResult klmdQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n"  // set function code to 0 (KLMD-Query)
	     "la     %%r1, %[ret]\t\n"
	     ".long  0xb93f0024\t\n"  // compute last message digest (KLMD)
	     :[ret] "=Q" (ret) : : "r0", "r1", "cc");

    return ret;
}

struct queryResult kdsaQuery(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.kdsaQuery")
  __attribute__((no_split_stack));

struct queryResult kdsaQuery() {
    struct queryResult ret;

    __asm__ ("lghi   %%r0, 0\t\n"  // set function code to 0 (KDSA-Query)
            "la     %%r1, %[ret]\t\n"
            ".long  0xb93a0024\t\n"  // kdsa
            :[ret] "=QRST" (ret) : : "r0", "r1", "cc");

    return ret;
}

#endif /* defined(__s390x__)  */

#ifdef __aarch64__

uint64_t getisar0(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.getisar0")
  __attribute__((no_split_stack));

uint64_t getisar0() {
       uint64_t isar0;

       __asm__("mrs %0,id_aa64isar0_el1" : "=r"(isar0));
       return isar0;
}

uint64_t getMIDR(void)
  __asm__(GOSYM_PREFIX "internal_1cpu.getMIDR")
  __attribute__((no_split_stack));

uint64_t getMIDR() {
       uint64_t MIDR;

       __asm__("mrs %0,midr_el1" : "=r"(MIDR));
       return MIDR;
}

#endif /* defined(__aarch64__) */
