// Implementation of <stacktrace> -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3.

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

#include <stacktrace>
#include <cstdlib>

#ifdef __cpp_lib_stacktrace // C++ >= 23 && hosted && HAVE_STACKTRACE
struct __glibcxx_backtrace_state;

extern "C"
{
__glibcxx_backtrace_state*
__glibcxx_backtrace_create_state(const char*, int,
				 void(*)(void*, const char*, int),
				 void*);

int
__glibcxx_backtrace_simple(__glibcxx_backtrace_state*, int,
			   int (*) (void*, __UINTPTR_TYPE__),
			   void(*)(void*, const char*, int),
			   void*);
int
__glibcxx_backtrace_pcinfo(__glibcxx_backtrace_state*, __UINTPTR_TYPE__,
			   int (*)(void*, __UINTPTR_TYPE__,
				   const char*, int, const char*),
			   void(*)(void*, const char*, int),
			   void*);

int
__glibcxx_backtrace_syminfo(__glibcxx_backtrace_state*, __UINTPTR_TYPE__ addr,
			    void (*) (void*, __UINTPTR_TYPE__, const char*,
				      __UINTPTR_TYPE__, __UINTPTR_TYPE__),
			    void(*)(void*, const char*, int),
			    void*);
}

namespace __cxxabiv1
{
  extern "C" char*
  __cxa_demangle(const char* mangled_name, char* output_buffer, size_t* length,
		 int* status);
}

namespace std
{
namespace
{
  char*
  demangle(const char* name)
  {
    int status;
    char* str = __cxxabiv1::__cxa_demangle(name, nullptr, nullptr, &status);
    if (status == 0)
      return str;
    else
      {
	std::free(str);
	return nullptr;
      }
  }

  void
  err_handler(void*, const char*, int)
  { }

  __glibcxx_backtrace_state*
  init()
  {
#if __GTHREADS && ! defined(__cpp_threadsafe_static_init)
# warning "std::stacktrace initialization will not be thread-safe"
#endif
    static __glibcxx_backtrace_state* state
      = __glibcxx_backtrace_create_state(nullptr, 1, err_handler, nullptr);
    return state;
  }
}

void
stacktrace_entry::_Info::_M_set_file(const char* filename)
{
  if (filename && _M_file)
    _M_set(_M_file, filename);
}

void
stacktrace_entry::_Info::_M_set_desc(const char* function)
{
  if (function && _M_desc)
    {
      if (char* s = demangle(function))
	{
	  _M_set(_M_desc, s);
	  std::free(s);
	}
      else
	_M_set(_M_desc, function);
    }
}

#pragma GCC diagnostic push
// The closure types below don't escape so we don't care about their mangling.
#pragma GCC diagnostic ignored "-Wabi"
bool
stacktrace_entry::_Info::_M_populate(native_handle_type pc)
{
  auto cb = [](void* self, uintptr_t, const char* filename, int lineno,
	       const char* function) -> int
  {
    auto& info = *static_cast<_Info*>(self);
    info._M_set_desc(function);
    info._M_set_file(filename);
    if (info._M_line)
      *info._M_line = lineno;
    return function != nullptr;
  };
  const auto state = init();
  if (::__glibcxx_backtrace_pcinfo(state, pc, +cb, err_handler, this))
    return true;

  // If we get here then backtrace_pcinfo did not give us a function name.
  // If the caller wanted a function name, try again using backtrace_syminfo.
  if (_M_desc)
    {
      auto cb2 = [](void* self, uintptr_t, const char* symname,
		    uintptr_t, uintptr_t)
      {
	static_cast<_Info*>(self)->_M_set_desc(symname);
      };
      if (::__glibcxx_backtrace_syminfo(state, pc, +cb2, err_handler, this))
	return true;
    }
  return false;
}
#pragma GCC diagnostic pop

// Ensure no tail-call optimization, so that this frame isn't reused for the
// backtrace_simple call, so that the number of frames to skip doesn't vary.
[[gnu::optimize("no-optimize-sibling-calls")]]
int
__stacktrace_impl::_S_current(int (*cb) (void*, __UINTPTR_TYPE__), void* obj,
			      int skip)
{
  const auto state = init();
  // skip+2 because we don't want this function or its caller to be included.
  int r = ::__glibcxx_backtrace_simple(state, skip + 2, cb, err_handler, obj);
  // Could also use this to prevent tail-call optim: __asm ("" : "+g" (r));
  return r;
}

}
#endif
