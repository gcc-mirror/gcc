// Implementation of std::move_only_function and std::copyable_function -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

/** @file include/bits/move_only_function.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_MOVE_ONLY_FUNCTION_H
#define _GLIBCXX_MOVE_ONLY_FUNCTION_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/version.h>

#if defined(__glibcxx_move_only_function) || defined(__glibcxx_copyable_function)

#include <bits/invoke.h>
#include <bits/utility.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @cond undocumented
  template<typename _Tp>
    inline constexpr bool __is_polymorphic_function_v = false;

  namespace __polyfunc
  {
    union _Ptrs
    {
      void* _M_obj;
      void (*_M_func)();
    };

   struct _Storage
   {
     void*       _M_addr() noexcept       { return &_M_bytes[0]; }
     void const* _M_addr() const noexcept { return &_M_bytes[0]; }

     template<typename _Tp>
       static consteval bool
       _S_stored_locally() noexcept
       {
	 return sizeof(_Tp) <= sizeof(_Storage)
		&& alignof(_Tp) <= alignof(_Storage)
		&& is_nothrow_move_constructible_v<_Tp>;
       }

     template<typename _Tp, typename... _Args>
       static consteval bool
       _S_nothrow_init() noexcept
       {
	 if constexpr (_S_stored_locally<_Tp>())
	   return is_nothrow_constructible_v<_Tp, _Args...>;
	 return false;
       }

     template<typename _Tp, typename... _Args>
       void
       _M_init(_Args&&... __args) noexcept(_S_nothrow_init<_Tp, _Args...>())
       {
	 if constexpr (is_function_v<remove_pointer_t<_Tp>>)
	   {
	     static_assert( sizeof...(__args) <= 1 );
	     // __args can have up to one element, returns nullptr if empty.
	     _Tp __func = (nullptr, ..., __args);
	     _M_ptrs._M_func = reinterpret_cast<void(*)()>(__func);
	   }
	 else if constexpr (!_S_stored_locally<_Tp>())
	   _M_ptrs._M_obj = new _Tp(std::forward<_Args>(__args)...);
	 else
	   ::new (_M_addr()) _Tp(std::forward<_Args>(__args)...);
       }

     template<typename _Tp>
       [[__gnu__::__always_inline__]]
       _Tp*
       _M_ptr() const noexcept
       {
	 if constexpr (!_S_stored_locally<remove_const_t<_Tp>>())
	   return static_cast<_Tp*>(_M_ptrs._M_obj);
	 else if constexpr (is_const_v<_Tp>)
	   return static_cast<_Tp*>(_M_addr());
	 else
	   // _Manager and _Invoker pass _Storage by const&, even for mutable sources.
	   return static_cast<_Tp*>(const_cast<void*>(_M_addr()));
       }

     template<typename _Ref>
       [[__gnu__::__always_inline__]]
       _Ref
       _M_ref() const noexcept
       {
	 using _Tp = remove_reference_t<_Ref>;
	 if constexpr (is_function_v<remove_pointer_t<_Tp>>)
	   return reinterpret_cast<_Tp>(_M_ptrs._M_func);
	 else
	   return static_cast<_Ref>(*_M_ptr<_Tp>());
       }

     // We want to have enough space to store a simple delegate type.
     struct _Delegate { void (_Storage::*__pfm)(); _Storage* __obj; };
     union {
       _Ptrs _M_ptrs;
       alignas(_Delegate) alignas(void(*)())
       unsigned char _M_bytes[sizeof(_Delegate)];
     };
   };

   template<bool _Noex, typename _Ret, typename... _Args>
     struct _Base_invoker
     {
       using _Signature = _Ret(*)(_Args...) noexcept(_Noex);

       using __storage_func_t = _Ret(*)(const _Storage&, _Args...) noexcept(_Noex);
       template<typename _Tp>
	 static consteval __storage_func_t
	 _S_storage()
	 { return &_S_call_storage<_Adjust_target<_Tp>>; }

     private:
       template<typename _Tp, typename _Td = remove_cvref_t<_Tp>>
	 using _Adjust_target =
	   __conditional_t<is_pointer_v<_Td> || is_member_pointer_v<_Td>, _Td, _Tp>;

       template<typename _Tp>
	 static _Ret
	 _S_call_storage(const _Storage& __ref, _Args... __args) noexcept(_Noex)
	 {
	   return std::__invoke_r<_Ret>(__ref._M_ref<_Tp>(),
					std::forward<_Args>(__args)...);
	 }
     };

   template<typename _Tp>
     using __param_t = __conditional_t<is_scalar_v<_Tp>, _Tp, _Tp&&>;

   template<bool _Noex, typename _Ret, typename... _Args>
     using _Invoker = _Base_invoker<_Noex, remove_cv_t<_Ret>, __param_t<_Args>...>;

   template<typename _Func>
     auto&
     __invoker_of(_Func& __f) noexcept
     { return __f._M_invoke; }

   template<typename _Func>
     auto&
     __base_of(_Func& __f) noexcept
     { return static_cast<__like_t<_Func&, typename _Func::_Base>>(__f); }

   template<typename _Src, typename _Dst>
     consteval bool
     __is_invoker_convertible() noexcept
     {
       if constexpr (requires { typename _Src::_Signature; })
	 return is_convertible_v<typename _Src::_Signature,
				 typename _Dst::_Signature>;
       else
	 return false;
     }

   struct _Manager
   {
     enum class _Op
     {
       // saves address of entity in *__src to __target._M_ptrs,
       _Address,
       // moves entity stored in *__src to __target, __src becomes empty
       _Move,
       // copies entity stored in *__src to __target, supported only if
       // _ProvideCopy is specified.
       _Copy,
       // destroys entity stored in __target, __src is ignoring
       _Destroy,
     };

    // A function that performs operation __op on the __target and possibly __src.
    using _Func = void (*)(_Op __op, _Storage& __target, const _Storage* __src);

    // The no-op manager function for objects with no target.
    static void _S_empty(_Op, _Storage&, const _Storage*) noexcept { }

    template<bool _ProvideCopy, typename _Tp>
      consteval static auto
      _S_select()
      {
	if constexpr (is_function_v<remove_pointer_t<_Tp>>)
	  return &_S_func;
	else if constexpr (!_Storage::_S_stored_locally<_Tp>())
	  return &_S_ptr<_ProvideCopy, _Tp>;
	else if constexpr (is_trivially_copyable_v<_Tp>)
	  return &_S_trivial;
	else
	  return &_S_local<_ProvideCopy, _Tp>;
      }

   private:
     static void
     _S_func(_Op __op, _Storage& __target, const _Storage* __src) noexcept
     {
       switch (__op)
       {
	 case _Op::_Address:
	 case _Op::_Move:
	 case _Op::_Copy:
	   __target._M_ptrs._M_func = __src->_M_ptrs._M_func;
	   return;
	 case _Op::_Destroy:
	   return;
       }
     }

     static void
     _S_trivial(_Op __op, _Storage& __target, const _Storage* __src) noexcept
     {
       switch (__op)
       {
	 case _Op::_Address:
	   __target._M_ptrs._M_obj = const_cast<void*>(__src->_M_addr());
	   return;
	 case _Op::_Move:
	 case _Op::_Copy:
	   // N.B. Creating _Storage starts lifetime of _M_bytes char array,
	   // that implicitly creates, amongst other, all possible trivially
	   // copyable objects, so we copy any object present in __src._M_bytes.
	   ::new (&__target) _Storage(*__src);
	   return;
	 case _Op::_Destroy:
	   return;
       }
     }

     template<bool _Provide_copy, typename _Tp>
       static void
       _S_local(_Op __op, _Storage& __target, const _Storage* __src)
       noexcept(!_Provide_copy)
       {
	 switch (__op)
	 {
	   case _Op::_Address:
	     __target._M_ptrs._M_obj = __src->_M_ptr<_Tp>();
	     return;
	   case _Op::_Move:
	     {
	       _Tp* __obj = __src->_M_ptr<_Tp>();
	       ::new(__target._M_addr()) _Tp(std::move(*__obj));
	       __obj->~_Tp();
	     }
	     return;
	   case _Op::_Destroy:
	     __target._M_ptr<_Tp>()->~_Tp();
	     return;
	   case _Op::_Copy:
	     if constexpr (_Provide_copy)
	       ::new (__target._M_addr()) _Tp(__src->_M_ref<const _Tp&>());
	     else
	       __builtin_unreachable();
	     return;
	 }
       }

     template<bool _Provide_copy, typename _Tp>
       static void
       _S_ptr(_Op __op, _Storage& __target, const _Storage* __src)
       noexcept(!_Provide_copy)
       {
	 switch (__op)
	 {
	   case _Op::_Address:
	   case _Op::_Move:
	     __target._M_ptrs._M_obj = __src->_M_ptrs._M_obj;
	     return;
	   case _Op::_Destroy:
	     delete __target._M_ptr<_Tp>();
	     return;
	   case _Op::_Copy:
	     if constexpr (_Provide_copy)
	       __target._M_ptrs._M_obj = new _Tp(__src->_M_ref<const _Tp&>());
	     else
	       __builtin_unreachable();
	     return;
	  }
	}
   };

   class _Mo_base
   {
   protected:
     _Mo_base() noexcept
     : _M_manage(_Manager::_S_empty)
     { }

     _Mo_base(_Mo_base&& __x) noexcept
     { _M_move(__x); }

     template<typename _Tp, typename... _Args>
       static consteval bool
       _S_nothrow_init() noexcept
       { return _Storage::_S_nothrow_init<_Tp, _Args...>(); }

     template<typename _Tp, typename... _Args>
       void
       _M_init(_Args&&... __args)
       noexcept(_S_nothrow_init<_Tp, _Args...>())
       {
	 _M_storage._M_init<_Tp>(std::forward<_Args>(__args)...);
	 _M_manage = _Manager::_S_select<false, _Tp>();
       }

     void
     _M_move(_Mo_base& __x) noexcept
     {
       using _Op = _Manager::_Op;
       _M_manage = std::__exchange(__x._M_manage, _Manager::_S_empty);
       _M_manage(_Op::_Move, _M_storage, &__x._M_storage);
     }

     _Mo_base&
     operator=(_Mo_base&& __x) noexcept
     {
       _M_destroy();
       _M_move(__x);
       return *this;
     }

     void
     _M_reset() noexcept
     {
       _M_destroy();
       _M_manage = _Manager::_S_empty;
     }

     ~_Mo_base()
     { _M_destroy(); }

     void
     swap(_Mo_base& __x) noexcept
     {
       using _Op = _Manager::_Op;
       // Order of operations here is more efficient if __x is empty.
       _Storage __s;
       __x._M_manage(_Op::_Move, __s, &__x._M_storage);
       _M_manage(_Op::_Move, __x._M_storage, &_M_storage);
       __x._M_manage(_Op::_Move, _M_storage, &__s);
       std::swap(_M_manage, __x._M_manage);
     }

     _Storage _M_storage;

   private:
     void _M_destroy() noexcept
     { _M_manage(_Manager::_Op::_Destroy, _M_storage, nullptr); }

     _Manager::_Func _M_manage;

#ifdef __glibcxx_copyable_function // C++ >= 26 && HOSTED
     friend class _Cpy_base;
#endif // __glibcxx_copyable_function
   };

} // namespace __polyfunc
  /// @endcond

#ifdef __glibcxx_move_only_function // C++ >= 23 && HOSTED
  template<typename... _Signature>
    class move_only_function; // not defined

  /// @cond undocumented
  template<typename _Tp>
    constexpr bool __is_polymorphic_function_v<move_only_function<_Tp>> = true;

  namespace __detail::__variant
  {
    template<typename> struct _Never_valueless_alt; // see <variant>

    // Provide the strong exception-safety guarantee when emplacing a
    // move_only_function into a variant.
    template<typename... _Signature>
      struct _Never_valueless_alt<std::move_only_function<_Signature...>>
      : true_type
      { };
  }  // namespace __detail::__variant
  /// @endcond
#endif // __glibcxx_move_only_function

#ifdef __glibcxx_copyable_function // C++ >= 26 && HOSTED
  /// @cond undocumented
  namespace __polyfunc
  {
     class _Cpy_base : public _Mo_base
     {
     protected:
       _Cpy_base() = default;

       template<typename _Tp, typename... _Args>
	 void
	 _M_init(_Args&&... __args)
	 noexcept(_S_nothrow_init<_Tp, _Args...>())
	 {
	   _M_storage._M_init<_Tp>(std::forward<_Args>(__args)...);
	   _M_manage = _Manager::_S_select<true, _Tp>();
	 }

      void
      _M_copy(_Cpy_base const& __x)
      {
	using _Op = _Manager::_Op;
	__x._M_manage(_Op::_Copy, _M_storage, &__x._M_storage);
	_M_manage = __x._M_manage;
      }

      _Cpy_base(_Cpy_base&&) = default;

      _Cpy_base(_Cpy_base const& __x)
      { _M_copy(__x); }

      _Cpy_base&
      operator=(_Cpy_base&&) = default;

      _Cpy_base&
      // Needs to use copy and swap for exception guarantees.
      operator=(_Cpy_base const&) = delete;
    };
  } // namespace __polyfunc
  /// @endcond

  template<typename... _Signature>
    class copyable_function; // not defined

  template<typename _Tp>
    constexpr bool __is_polymorphic_function_v<copyable_function<_Tp>> = true;

    namespace __detail::__variant
  {
    template<typename> struct _Never_valueless_alt; // see <variant>

    // Provide the strong exception-safety guarantee when emplacing a
    // copyable_function into a variant.
    template<typename... _Signature>
      struct _Never_valueless_alt<std::copyable_function<_Signature...>>
      : true_type
      { };
  }  // namespace __detail::__variant
#endif // __glibcxx_copyable_function

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#ifdef __glibcxx_move_only_function // C++ >= 23 && HOSTED
#include "mofunc_impl.h"
#define _GLIBCXX_MOF_CV const
#include "mofunc_impl.h"
#define _GLIBCXX_MOF_REF &
#include "mofunc_impl.h"
#define _GLIBCXX_MOF_REF &&
#include "mofunc_impl.h"
#define _GLIBCXX_MOF_CV const
#define _GLIBCXX_MOF_REF &
#include "mofunc_impl.h"
#define _GLIBCXX_MOF_CV const
#define _GLIBCXX_MOF_REF &&
#include "mofunc_impl.h"
#endif // __glibcxx_move_only_function

#ifdef __glibcxx_copyable_function // C++ >= 26 && HOSTED
#include "cpyfunc_impl.h"
#define _GLIBCXX_MOF_CV const
#include "cpyfunc_impl.h"
#define _GLIBCXX_MOF_REF &
#include "cpyfunc_impl.h"
#define _GLIBCXX_MOF_REF &&
#include "cpyfunc_impl.h"
#define _GLIBCXX_MOF_CV const
#define _GLIBCXX_MOF_REF &
#include "cpyfunc_impl.h"
#define _GLIBCXX_MOF_CV const
#define _GLIBCXX_MOF_REF &&
#include "cpyfunc_impl.h"
#endif // __glibcxx_copyable_function

#endif // __glibcxx_copyable_function || __glibcxx_copyable_function
#endif // _GLIBCXX_MOVE_ONLY_FUNCTION_H
