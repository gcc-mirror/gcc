// Compatibility symbols for previous versions, C++0x bits -*- C++ -*-

// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#if defined(_GLIBCXX_SHARED)
#define _GLIBCXX_ASYNC_ABI_COMPAT
#endif

#include <future>
#include <mutex>
#include <functional>

#if __cplusplus < 201103L
# error "compatibility-thread-c++0x.cc must be compiled with -std=gnu++0x"
#endif

#define _GLIBCXX_ASM_SYMVER(cur, old, version) \
   asm (".symver " #cur "," #old "@@@" #version);

// XXX GLIBCXX_ABI Deprecated
// gcc-4.6.0
// <future> export changes
#if defined(_GLIBCXX_SYMVER_GNU) && defined(_GLIBCXX_SHARED) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE) \
    && defined(_GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
  const std::error_category* future_category = &std::future_category();
}

_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx15future_categoryE, _ZSt15future_category, GLIBCXX_3.4.14)

#endif

// XXX GLIBCXX_ABI Deprecated
// gcc-4.6.0
// <mutex> export changes
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
#if defined(_GLIBCXX_SYMVER_GNU) && defined(_GLIBCXX_SHARED) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE) \
    && defined(_GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
  std::defer_lock_t defer_lock;
  std::try_to_lock_t try_to_lock;
  std::adopt_lock_t adopt_lock;
}

_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx10adopt_lockE, _ZSt10adopt_lock, GLIBCXX_3.4.11)
_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx10defer_lockE, _ZSt10defer_lock, GLIBCXX_3.4.11)
_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx11try_to_lockE, _ZSt11try_to_lock, GLIBCXX_3.4.11)


#endif
#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1


// XXX GLIBCXX_ABI Deprecated
// gcc-4.7.0, gcc-4.9.0
// <future> export changes
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1) \
  && (ATOMIC_INT_LOCK_FREE > 1)
#if defined(_GLIBCXX_SHARED)
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  // Replaced by _State_baseV2 in gcc-4.9.0
  class __future_base::_State_base
  {
    typedef _Ptr<_Result_base> _Ptr_type;

    _Ptr_type			_M_result;
    mutex               	_M_mutex;
    condition_variable  	_M_cond;
    atomic_flag         	_M_retrieved;
    once_flag			_M_once;
  public:
    virtual ~_State_base();
    virtual void _M_run_deferred() { }
  };
  __future_base::_State_base::~_State_base() { }

  // Replaced by _Async_state_commonV2 in gcc-4.9.0
  class __future_base::_Async_state_common : public __future_base::_State_base
  {
  protected:
    ~_Async_state_common();
    virtual void _M_run_deferred() { _M_join(); }
    void _M_join() { std::call_once(_M_once, &thread::join, ref(_M_thread)); }
    thread _M_thread;
    once_flag _M_once;
  };
#if defined(_GLIBCXX_HAVE_TLS)
  // Replaced with inline definition in gcc-4.8.0
  __future_base::_Async_state_common::~_Async_state_common() { _M_join(); }

  template<typename _Tp>
    struct _Maybe_wrap_member_pointer;

  template<typename _Tp, typename _Class>
    struct _Maybe_wrap_member_pointer<_Tp _Class::*>
    {
      typedef _Mem_fn<_Tp _Class::*> type;

      static constexpr type
      __do_wrap(_Tp _Class::* __pm)
      { return type(__pm); }
    };

  template<typename _Signature>
    struct _Bind_simple;

  template<typename _Callable, typename... _Args>
    struct _Bind_simple<_Callable(_Args...)>
    {
      typedef typename result_of<_Callable(_Args...)>::type result_type;

      template<typename _Tp, typename... _Up>
        explicit
        _Bind_simple(_Tp&& __f, _Up&&... __args)
        : _M_bound(std::forward<_Tp>(__f), std::forward<_Up>(__args)...)
        { }

      _Bind_simple(const _Bind_simple&) = default;
      _Bind_simple(_Bind_simple&&) = default;

      result_type
      operator()()
      {
        typedef typename _Build_index_tuple<sizeof...(_Args)>::__type _Indices;
        return _M_invoke(_Indices());
      }

    private:
      template<std::size_t... _Indices>
        typename result_of<_Callable(_Args...)>::type
        _M_invoke(_Index_tuple<_Indices...>)
        {
         // std::bind always forwards bound arguments as lvalues,
         // but this type can call functions which only accept rvalues.
          return std::forward<_Callable>(std::get<0>(_M_bound))(
              std::forward<_Args>(std::get<_Indices+1>(_M_bound))...);
        }

      std::tuple<_Callable, _Args...> _M_bound;
    };

  template<typename _Func, typename... _BoundArgs>
    struct _Bind_simple_helper
    {
      typedef _Maybe_wrap_member_pointer<typename decay<_Func>::type>
        __maybe_type;
      typedef typename __maybe_type::type __func_type;
      typedef _Bind_simple<__func_type(typename decay<_BoundArgs>::type...)>
               __type;
    };

  // Simplified version of std::bind for internal use, without support for
  // unbound arguments, placeholders or nested bind expressions.
  template<typename _Callable, typename... _Args>
    typename _Bind_simple_helper<_Callable, _Args...>::__type
    __bind_simple(_Callable&& __callable, _Args&&... __args)
    {
      typedef _Bind_simple_helper<_Callable, _Args...> __helper_type;
      typedef typename __helper_type::__maybe_type __maybe_type;
      typedef typename __helper_type::__type __result_type;
      return __result_type(
          __maybe_type::__do_wrap( std::forward<_Callable>(__callable)),
          std::forward<_Args>(__args)...);
    }

  // Explicit instantiation due to -fno-implicit-instantiation.
  template void call_once(once_flag&, void (thread::*&&)(), reference_wrapper<thread>&&);
  template _Bind_simple_helper<void (thread::*)(), reference_wrapper<thread>>::__type __bind_simple(void (thread::*&&)(), reference_wrapper<thread>&&);
#endif // _GLIBCXX_HAVE_TLS
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // _GLIBCXX_SHARED
#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
