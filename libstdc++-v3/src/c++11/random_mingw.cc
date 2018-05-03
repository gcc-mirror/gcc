// random_device -*- C++ -*-

// Copyright (C) 2018 Free Software Foundation, Inc.
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

#ifdef __MINGW32__
#define _GLIBCXX_USE_CXX11_ABI 1
#include<random>
#include<array>
#include<windows.h>
#include<ntsecapi.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __random_device_details
{
namespace
{
using __result_type = random_device::result_type;
template<typename __T>
inline constexpr double __entropy(const __T&){return 32;}

inline constexpr double __entropy(const mt19937&){return 0;}

struct __rdseed
{
	__result_type operator()() const
	{
		for(size_t __i(0);__i!=100;++__i)
		{
			__result_type __r;
			uint8_t __k;
			__asm__ __volatile__("rdseed %0;setc %1":"=r"(__r),"=qm"(__k));
			if(__k)
				return __r;
			__asm__ __volatile__("pause");
		}
		throw runtime_error("std::random_device: hardware does not return any random value for the rdseed instruction");
	}
};

struct __rdrand
{
	__rdrand()
	{
		uint32_t __c;
		__asm__ __volatile__("cpuid":"=c"(__c):"a"(1):"%ebx","%edx");
		if(!(__c&(1<<30)))
			throw runtime_error("std::random_device: Your CPU does not support the rdrand instruction.");
	}
	__result_type operator()() const
	{
		for(size_t __i(0);__i!=100;++__i)
		{
			__result_type __r;
			uint8_t __k;
			__asm__ __volatile__("rdrand %0;setc %1":"=r"(__r),"=qm"(__k));
			if(__k)
				return __r;
		}
		throw runtime_error("std::random_device: hardware does not return any random value for the rdrand instruction");
	}
};

class __rtlgenrandom
{
	array<__result_type,((4096<sizeof(__result_type))?1:4096/sizeof(__result_type))> __a;
	size_t __i=__a.size();
public:
	__result_type operator()()
	{
		if(__i==__a.size())
		{
			if(!RtlGenRandom(__a.data(),sizeof(__a)))
				throw runtime_error("std::random_device: RtlGenRandom() failed");
			__i=1;
			return __a.front();
		}
		else
			return __a[__i++];
	}
};
struct __base
{
	virtual __result_type operator()() = 0;
	virtual ~__base()=default;
	virtual double __entropy() const noexcept = 0;
};
template<typename __T>
struct __derv:__base
{
	__T __t;
	__derv()=default;
	template<typename... __Args>
	__derv(__Args&& ...__args):__t(forward<__Args>(__args)...){}
	__result_type operator()()
	{
		return __t();
	}
	double __entropy() const noexcept
	{
		return __random_device_details::__entropy(__t);
	}
};
}
}

void random_device::_M_init(const string& __token)
{
	using namespace __random_device_details;
	if(__token=="default")
	{
		uint32_t __b;
		__asm__ __volatile__("cpuid":"=b"(__b):"a"(7),"c"(0):"%edx");
		if(__b&(1<<18))
			_M_file = new __derv<__rdseed>();
		else
			_M_file = new __derv<__rtlgenrandom>();
	}
	else if(__token=="rdseed")
	{
		uint32_t __b;
		__asm__ __volatile__("cpuid":"=b"(__b):"a"(7),"c"(0):"%edx");
		if(__b&(1<<18))
			_M_file = new __derv<__rdrand>();
		else
			throw runtime_error("std::random_device: Your CPU does not support the rdseed instruction.");
	}
	else if(__token=="rtlgenrandom")
		_M_file = new __derv<__rtlgenrandom>();
	else if(__token=="rdrand")
		_M_file = new __derv<__rdrand>();
	else if(__token=="mt19937")
		_M_file = new __derv<mt19937>();
	else
		throw runtime_error("std::random_device: unknown device: "+__token);
}


void random_device::_M_init_pretr1(const string& __str)
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
	_M_mt.seed(__ret);
}


void random_device::_M_fini()
{
	delete static_cast<__random_device_details::__base*>(_M_file);
}

random_device::result_type random_device::_M_getval()
{
	return (*static_cast<__random_device_details::__base*>(_M_file))();
}

double random_device::_M_getentropy() const noexcept
{
	return static_cast<__random_device_details::__base*>(_M_file)->__entropy();
}

random_device::result_type random_device::_M_getval_pretr1()
{
	return _M_mt();
}

}

#endif