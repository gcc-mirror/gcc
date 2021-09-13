// random -*- C++ -*-

// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
# ifdef _GLIBCXX_X86_RDRAND
#  define USE_RDRAND 1
# endif
# ifdef _GLIBCXX_X86_RDSEED
#  define USE_RDSEED 1
# endif
#endif

#include <cerrno>
#include <cstdio>
#include <cctype> // For std::isdigit.

#if defined _GLIBCXX_HAVE_UNISTD_H && defined _GLIBCXX_HAVE_FCNTL_H
# include <unistd.h>
# include <fcntl.h>
// Use POSIX open, close, read etc. instead of ISO fopen, fclose, fread
# define USE_POSIX_FILE_IO
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

#if defined _GLIBCXX_USE_CRT_RAND_S || defined _GLIBCXX_USE_DEV_RANDOM
// The OS provides a source of randomness we can use.
# pragma GCC poison _M_mt
#elif defined USE_RDRAND || defined USE_RDSEED
// Hardware instructions might be available, but use cpuid checks at runtime.
# pragma GCC poison _M_mt
// If the runtime cpuid checks fail we'll use a linear congruential engine.
# define USE_LCG 1
#else
// Use the mt19937 member of the union, as in previous GCC releases.
# define USE_MT19937 1
#endif

#ifdef USE_LCG
# include <chrono>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
  namespace
  {
#if USE_RDRAND
    unsigned int
    __attribute__ ((target("rdrnd")))
    __x86_rdrand(void*)
    {
      unsigned int retries = 100;
      unsigned int val;

      while (__builtin_ia32_rdrand32_step(&val) == 0)
	if (--retries == 0)
	  std::__throw_runtime_error(__N("random_device: rdrand failed"));

      return val;
    }
#endif

#if USE_RDSEED
    unsigned int
    __attribute__ ((target("rdseed")))
    __x86_rdseed(void* fallback)
    {
      unsigned int retries = 100;
      unsigned int val;

      while (__builtin_ia32_rdseed_si_step(&val) == 0)
	{
	  if (--retries == 0)
	    {
	      if (auto f = reinterpret_cast<unsigned int(*)(void*)>(fallback))
		return f(nullptr);
	      std::__throw_runtime_error(__N("random_device: rdseed failed"));
	    }
	  __builtin_ia32_pause();
	}

      return val;
    }

#if USE_RDRAND
    unsigned int
    __attribute__ ((target("rdseed,rdrnd")))
    __x86_rdseed_rdrand(void*)
    {
      return __x86_rdseed(reinterpret_cast<void*>(&__x86_rdrand));
    }
#endif
#endif

#ifdef _GLIBCXX_USE_CRT_RAND_S
    unsigned int
    __winxp_rand_s(void*)
    {
      unsigned int val;
      if (::rand_s(&val) != 0)
	std::__throw_runtime_error(__N("random_device: rand_s failed"));
      return val;
    }
#endif

#ifdef USE_LCG
    // TODO: use this to seed std::mt19937 engine too.
    unsigned
    bad_seed(void* p) noexcept
    {
      // Poor quality seed based on hash of the current time and the address
      // of the object being seeded. Better than using the same default seed
      // for every object though.
      const uint64_t bits[] = {
	(uint64_t) chrono::system_clock::now().time_since_epoch().count(),
	(uint64_t) reinterpret_cast<uintptr_t>(p)
      };
      auto bytes = reinterpret_cast<const unsigned char*>(bits);
      // 32-bit FNV-1a hash
      uint32_t h = 2166136261u;
      for (unsigned i = 0; i < sizeof(bits); ++i)
	{
	  h ^= *bytes++;
	  h *= 16777619u;
	}
      return h;
    }

    // Same as std::minstd_rand0 but using unsigned not uint_fast32_t.
    using lcg_type
      = linear_congruential_engine<unsigned, 16807UL, 0UL, 2147483647UL>;

    inline lcg_type*
    construct_lcg_at(void* addr) noexcept
    {
      return ::new(addr) lcg_type(bad_seed(addr));
    }

    inline void
    destroy_lcg_at(void* addr) noexcept
    {
      static_cast<lcg_type*>(addr)->~lcg_type();
    }

    unsigned int
    __lcg(void* ptr) noexcept
    {
      auto& lcg = *static_cast<lcg_type*>(ptr);
      return lcg();
    }
#endif
  }

  void
  random_device::_M_init(const std::string& token)
  {
#ifdef USE_MT19937
    // If no real random device is supported then use the mt19937 engine.
    _M_init_pretr1(token);
    return;
#else

    _M_file = nullptr;
    _M_func = nullptr;
    _M_fd = -1;

    const char* fname [[gnu::unused]] = nullptr;

    enum {
	rand_s = 1, rdseed = 2, rdrand = 4, device_file = 8, prng = 16,
	any = 0xffff
    } which;

    if (token == "default")
      {
	which = any;
	fname = "/dev/urandom";
      }
#ifdef USE_RDSEED
    else if (token == "rdseed")
      which = rdseed;
#endif // USE_RDSEED
#ifdef USE_RDRAND
    else if (token == "rdrand" || token == "rdrnd")
      which = rdrand;
#endif // USE_RDRAND
#ifdef _GLIBCXX_USE_CRT_RAND_S
    else if (token == "rand_s")
      which = rand_s;
#endif // _GLIBCXX_USE_CRT_RAND_S
#ifdef _GLIBCXX_USE_DEV_RANDOM
    else if (token == "/dev/urandom" || token == "/dev/random")
      {
	fname = token.c_str();
	which = device_file;
      }
#endif // _GLIBCXX_USE_DEV_RANDOM
#ifdef USE_LCG
    else if (token == "prng")
      which = prng;
#endif
    else
      std::__throw_runtime_error(
	  __N("random_device::random_device(const std::string&):"
	      " unsupported token"));

#ifdef _GLIBCXX_USE_CRT_RAND_S
    if (which & rand_s)
    {
      _M_func = &__winxp_rand_s;
      return;
    }
#endif // _GLIBCXX_USE_CRT_RAND_S

#ifdef USE_RDSEED
    if (which & rdseed)
    {
      unsigned int eax, ebx, ecx, edx;
      // Check availability of cpuid and, for now at least, also the
      // CPU signature for Intel and AMD.
      if (__get_cpuid_max(0, &ebx) > 0
	  && (ebx == signature_INTEL_ebx || ebx == signature_AMD_ebx))
	{
	  // CPUID.(EAX=07H, ECX=0H):EBX.RDSEED[bit 18]
	  __cpuid_count(7, 0, eax, ebx, ecx, edx);
	  if (ebx & bit_RDSEED)
	    {
#ifdef USE_RDRAND
	      // CPUID.01H:ECX.RDRAND[bit 30]
	      __cpuid(1, eax, ebx, ecx, edx);
	      if (ecx & bit_RDRND)
		{
		  _M_func = &__x86_rdseed_rdrand;
		  return;
		}
#endif
	      _M_func = &__x86_rdseed;
	      return;
	    }
	}
    }
#endif // USE_RDSEED

#ifdef USE_RDRAND
    if (which & rdrand)
    {
      unsigned int eax, ebx, ecx, edx;
      // Check availability of cpuid and, for now at least, also the
      // CPU signature for Intel and AMD.
      if (__get_cpuid_max(0, &ebx) > 0
	  && (ebx == signature_INTEL_ebx || ebx == signature_AMD_ebx))
	{
	  // CPUID.01H:ECX.RDRAND[bit 30]
	  __cpuid(1, eax, ebx, ecx, edx);
	  if (ecx & bit_RDRND)
	    {
	      _M_func = &__x86_rdrand;
	      return;
	    }
	}
    }
#endif // USE_RDRAND

#ifdef _GLIBCXX_USE_DEV_RANDOM
    if (which & device_file)
    {
#ifdef USE_POSIX_FILE_IO
      _M_fd = ::open(fname, O_RDONLY);
      if (_M_fd != -1)
	{
	  // Set _M_file to non-null so that _M_fini() will do clean up.
	  _M_file = &_M_fd;
	  return;
	}
#else // USE_POSIX_FILE_IO
      _M_file = static_cast<void*>(std::fopen(fname, "rb"));
      if (_M_file)
	return;
#endif // USE_POSIX_FILE_IO
    }
#endif // _GLIBCXX_USE_DEV_RANDOM

#ifdef USE_LCG
    // Either "prng" was requested explicitly, or "default" was requested
    // but nothing above worked, use a PRNG.
    if (which & prng)
    {
      static_assert(sizeof(lcg_type) <= sizeof(_M_fd), "");
      static_assert(alignof(lcg_type) <= alignof(_M_fd), "");
      _M_file = construct_lcg_at(&_M_fd);
      _M_func = &__lcg;
      return;
    }
#endif

    std::__throw_runtime_error(
	__N("random_device::random_device(const std::string&):"
	    " device not available"));
#endif // USE_MT19937
  }

  // This function is called by _M_init for targets that use mt19937 for
  // randomness, and by code compiled against old releases of libstdc++.
  void
  random_device::_M_init_pretr1(const std::string& token)
  {
#ifdef USE_MT19937
    unsigned long seed = 5489UL;
    if (token != "default" && token != "mt19937" && token != "prng")
      {
	const char* nptr = token.c_str();
	char* endptr;
	seed = std::strtoul(nptr, &endptr, 0);
	if (*nptr == '\0' || *endptr != '\0')
	  std::__throw_runtime_error(__N("random_device::_M_init_pretr1"
					 "(const std::string&)"));
      }
    _M_mt.seed(seed);
#else
    // Convert old default token "mt19937" or numeric seed tokens to "default".
    if (token == "mt19937" || std::isdigit((unsigned char)token[0]))
      _M_init("default");
    else
      _M_init(token);
#endif
  }

  // Called by old ABI version of random_device::_M_init(const std::string&).
  void
  random_device::_M_init(const char* s, size_t len)
  {
    const std::string token(s, len);
#ifdef USE_MT19937
    _M_init_pretr1(token);
#else
    _M_init(token);
#endif
  }

  void
  random_device::_M_fini()
  {
    // _M_file == nullptr means no resources to free.
    if (!_M_file)
      return;

#if USE_LCG
    if (_M_func == &__lcg)
      {
	destroy_lcg_at(_M_file);
	return;
      }
#endif

#ifdef USE_POSIX_FILE_IO
    ::close(_M_fd);
    _M_fd = -1;
#else
    std::fclose(static_cast<FILE*>(_M_file));
#endif
    _M_file = nullptr;
  }

  random_device::result_type
  random_device::_M_getval()
  {
#ifdef USE_MT19937
    return _M_mt();
#else

    if (_M_func)
      return _M_func(_M_file);

    result_type ret;
    void* p = &ret;
    size_t n = sizeof(result_type);
#ifdef USE_POSIX_FILE_IO
    do
      {
	const int e = ::read(_M_fd, p, n);
	if (e > 0)
	  {
	    n -= e;
	    p = static_cast<char*>(p) + e;
	  }
	else if (e != -1 || errno != EINTR)
	  __throw_runtime_error(__N("random_device could not be read"));
      }
    while (n > 0);
#else // USE_POSIX_FILE_IO
    const size_t e = std::fread(p, n, 1, static_cast<FILE*>(_M_file));
    if (e != 1)
      __throw_runtime_error(__N("random_device could not be read"));
#endif // USE_POSIX_FILE_IO

    return ret;
#endif // USE_MT19937
  }

  // Only called by code compiled against old releases of libstdc++.
  // Forward the call to _M_getval() and let it decide what to do.
  random_device::result_type
  random_device::_M_getval_pretr1()
  { return _M_getval(); }

  double
  random_device::_M_getentropy() const noexcept
  {
#if defined _GLIBCXX_USE_DEV_RANDOM \
    && defined _GLIBCXX_HAVE_SYS_IOCTL_H && defined RNDGETENTCNT
    if (!_M_file)
      return 0.0;

#ifdef USE_POSIX_FILE_IO
    const int fd = _M_fd;
#else
    const int fd = ::fileno(static_cast<FILE*>(_M_file));
#endif
    if (fd < 0)
      return 0.0;

    int ent;
    if (::ioctl(fd, RNDGETENTCNT, &ent) < 0)
      return 0.0;

    if (ent < 0)
      return 0.0;

    const int max = sizeof(result_type) * __CHAR_BIT__;
    if (ent > max)
      ent = max;

    return static_cast<double>(ent);
#else
    return 0.0;
#endif // _GLIBCXX_USE_DEV_RANDOM && _GLIBCXX_HAVE_SYS_IOCTL_H && RNDGETENTCNT
  }

#ifdef USE_MT19937
  template class mersenne_twister_engine<
    uint_fast32_t,
    32, 624, 397, 31,
    0x9908b0dfUL, 11,
    0xffffffffUL, 7,
    0x9d2c5680UL, 15,
    0xefc60000UL, 18, 1812433253UL>;
#endif // USE_MT19937

#ifdef USE_LCG
  template class
    linear_congruential_engine<unsigned, 16807UL, 0UL, 2147483647UL>;
  template struct __detail::_Mod<unsigned, 2147483647UL, 16807UL, 0UL>;
#endif
}
#endif // _GLIBCXX_USE_C99_STDINT_TR1
