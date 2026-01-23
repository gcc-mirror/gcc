// Implementation of std::move_only_function, std::copyable_function
// and std::function_ref  -*- C++ -*-

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

/** @file include/bits/funcwrap.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_FUNCWRAP_H
#define _GLIBCXX_FUNCWRAP_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/version.h>

#if __glibcxx_move_only_function || __glibcxx_copyable_function || __glibcxx_function_ref

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
      const void* _M_obj;
      void (*_M_func)();
    };

   template<typename _Tp>
     [[__gnu__::__always_inline__]]
     constexpr auto*
     __cast_to(_Ptrs __ptrs) noexcept
     {
       using _Td = remove_reference_t<_Tp>;
       if constexpr (is_function_v<_Td>)
	 return reinterpret_cast<_Td*>(__ptrs._M_func);
       else if constexpr (is_const_v<_Td>)
	 return static_cast<_Td*>(__ptrs._M_obj);
       else
	 return static_cast<_Td*>(const_cast<void*>(__ptrs._M_obj));
     }

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

       using __ptrs_func_t = _Ret(*)(_Ptrs, _Args...) noexcept(_Noex);
       template<typename _Tp>
	 static consteval __ptrs_func_t
	 _S_ptrs()
	 { return &_S_call_ptrs<_Adjust_target<_Tp>>; }

#ifdef __glibcxx_function_ref // C++ >= 26
       template<auto __fn>
	 static _Ret
	 _S_nttp(_Ptrs, _Args... __args) noexcept(_Noex)
	 { return std::__invoke_r<_Ret>(__fn, std::forward<_Args>(__args)...); }

       template<auto __fn, typename _Tp>
	 static _Ret
	 _S_bind_ptr(_Ptrs __ptrs, _Args... __args) noexcept(_Noex)
	 {
	   auto* __p = __polyfunc::__cast_to<_Tp>(__ptrs);
	   return std::__invoke_r<_Ret>(__fn, __p,
					std::forward<_Args>(__args)...);
	 }

       template<auto __fn, typename _Ref>
	 static _Ret
	 _S_bind_ref(_Ptrs __ptrs, _Args... __args) noexcept(_Noex)
	 {
	   auto* __p = __polyfunc::__cast_to<_Ref>(__ptrs);
	   return std::__invoke_r<_Ret>(__fn, static_cast<_Ref>(*__p),
					std::forward<_Args>(__args)...);
	 }
#endif // __glibcxx_function_ref

     private:
       template<typename _Tp, typename _Td = remove_cvref_t<_Tp>>
	 using _Adjust_target =
	   __conditional_t<is_pointer_v<_Td> || is_member_pointer_v<_Td>, _Td, _Tp>;

       template<typename _Tp>
	 static _Ret
	 _S_call_storage(const _Storage& __ref, _Args... __args) noexcept(_Noex)
	 {
	   _Ptrs __ptrs;
	   if constexpr (is_function_v<remove_pointer_t<_Tp>>)
	     __ptrs._M_func = __ref._M_ptrs._M_func;
	   else if constexpr (!_Storage::_S_stored_locally<remove_cvref_t<_Tp>>())
	     __ptrs._M_obj = __ref._M_ptrs._M_obj;
	   else
	     __ptrs._M_obj = __ref._M_addr();
	   return _S_call_ptrs<_Tp>(__ptrs, std::forward<_Args>(__args)...);
	 }

       template<typename _Tp>
	 static _Ret
	 _S_call_ptrs(_Ptrs __ptrs, _Args... __args) noexcept(_Noex)
	 {
	   if constexpr (is_function_v<remove_pointer_t<_Tp>>)
	     return std::__invoke_r<_Ret>(reinterpret_cast<_Tp>(__ptrs._M_func),
					  std::forward<_Args>(__args)...);
	   else
	     {
	       auto* __p = __polyfunc::__cast_to<_Tp>(__ptrs);
	       return std::__invoke_r<_Ret>(static_cast<_Tp>(*__p),
					    std::forward<_Args>(__args)...);
	     }
	 }
     };

   template<typename _Tp>
     consteval bool
     __pass_by_value()
     {
       // n.b. sizeof(Incomplete&) is ill-formed for incomplete types,
       // so we check is_reference_v first.
       if constexpr (is_reference_v<_Tp> || is_scalar_v<_Tp>)
	 return true;
       else
	 // n.b. we already asserted that types are complete in wrappers,
	 // avoid triggering additional errors from this function.
	 if constexpr (std::__is_complete_or_unbounded(__type_identity<_Tp>()))
	   if constexpr (sizeof(_Tp) <= 2 * sizeof(void*))
	     return is_trivially_move_constructible_v<_Tp>
		    && is_trivially_destructible_v<_Tp>;
       return false;
     }

   template<typename _Tp>
     using __param_t = __conditional_t<__pass_by_value<_Tp>(), _Tp, _Tp&&>;

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

#if __glibcxx_move_only_function || __glibcxx_copyable_function
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
	   __target._M_ptrs._M_obj = __src->_M_addr();
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
	     __target._M_ptrs._M_obj = __src->_M_addr();
	     return;
	   case _Op::_Move:
	     {
	       _Tp* __obj = static_cast<_Tp*>(const_cast<void*>(__src->_M_addr()));
	       ::new(__target._M_addr()) _Tp(std::move(*__obj));
	       __obj->~_Tp();
	     }
	     return;
	   case _Op::_Destroy:
	     static_cast<_Tp*>(__target._M_addr())->~_Tp();
	     return;
	   case _Op::_Copy:
	     if constexpr (_Provide_copy)
	       {
		 auto* __obj = static_cast<const _Tp*>(__src->_M_addr());
		 ::new (__target._M_addr()) _Tp(*__obj);
		 return;
	       }
	     __builtin_unreachable();
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
	     delete static_cast<const _Tp*>(__target._M_ptrs._M_obj);
	     return;
	   case _Op::_Copy:
	     if constexpr (_Provide_copy)
	       {
		 auto* __obj = static_cast<const _Tp*>(__src->_M_ptrs._M_obj);
		 __target._M_ptrs._M_obj = new _Tp(*__obj);
		 return;
	       }
	     __builtin_unreachable();
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

     void _M_destroy() noexcept
     { _M_manage(_Manager::_Op::_Destroy, _M_storage, nullptr); }

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

     _Manager::_Func _M_manage;
     _Storage _M_storage;
   };
#endif // __glibcxx_copyable_function || __glibcxx_copyable_function
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
      : _Mo_base()
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

#ifdef __glibcxx_function_ref  // C++ >= 26
  /// @cond undocumented
  namespace __polyfunc
  {
    template<typename _Sig>
      struct __skip_first_arg;

    // Additional partial specializations are defined in bits/funcref_impl.h
    template<bool _Noex, typename _Ret, typename _Arg, typename... _Args>
      struct __skip_first_arg<_Ret(*)(_Arg, _Args...) noexcept(_Noex)>
      { using type = _Ret(_Args...) noexcept(_Noex); };

    // Returns a function pointer to signature to be used with function_ref, or void.
    template<typename _Fn, typename _Tr>
      consteval auto
      __deduce_funcref()
      {
	if constexpr (is_member_object_pointer_v<_Fn>)
	  {
	    if constexpr (is_invocable_v<_Fn, _Tr>)
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 4425. CTAD function_ref from data member pointer should produce
	      //       noexcept signature
	      return static_cast<invoke_result_t<_Fn, _Tr>(*)() noexcept>(nullptr);
	  }
	else if constexpr (requires { typename __skip_first_arg<_Fn>::type; })
	  return static_cast<__skip_first_arg<_Fn>::type*>(nullptr);
      }
  } // namespace __polyfunc
  /// @endcond

  template<typename... _Signature>
    class function_ref; // not defined

  template<typename _Fn>
    requires is_function_v<_Fn>
    function_ref(_Fn*) -> function_ref<_Fn>;

  template<auto __f, class _Fn = remove_pointer_t<decltype(__f)>>
    requires is_function_v<_Fn>
    function_ref(nontype_t<__f>) -> function_ref<_Fn>;

  template<auto __f, typename _Tp,
	   typename _SignaturePtr =
	     decltype(__polyfunc::__deduce_funcref<decltype(__f), _Tp&>())>
    requires (!is_void_v<_SignaturePtr>)
    function_ref(nontype_t<__f>, _Tp&&)
      -> function_ref<remove_pointer_t<_SignaturePtr>>;

#endif // __glibcxx_function_ref

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

#ifdef __glibcxx_function_ref  // C++ >= 26
#include "funcref_impl.h"
#define _GLIBCXX_MOF_CV const
#include "funcref_impl.h"
#endif // __glibcxx_function_ref

#endif // move_only_function || copyable_function || function_ref
#endif // _GLIBCXX_FUNCWRAP_H
