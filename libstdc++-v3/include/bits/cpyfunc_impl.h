// Implementation of std::copyable_function -*- C++ -*-

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

/** @file include/bits/cpyfunc_impl.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_MOF_CV
# define _GLIBCXX_MOF_CV
#endif

#ifdef _GLIBCXX_MOF_REF
# define _GLIBCXX_MOF_INV_QUALS _GLIBCXX_MOF_CV _GLIBCXX_MOF_REF
#else
# define _GLIBCXX_MOF_REF
# define _GLIBCXX_MOF_INV_QUALS _GLIBCXX_MOF_CV &
#endif

#define _GLIBCXX_MOF_CV_REF _GLIBCXX_MOF_CV _GLIBCXX_MOF_REF

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief Polymorphic copyable function wrapper.
   *  @ingroup functors
   *  @since C++26
   *  @headerfile functional
   *
   *  The `std::copyable_function` class template is a call wrapper similar
   *  to `std::function`, but it does not provide information about it's
   *  target, and preserves constness.
   *
   *  It also supports const-qualification, ref-qualification, and
   *  no-throw guarantees. The qualifications and exception-specification
   *  of the `copyable_function::operator()` member function are respected
   *  when invoking the target function.
   */
  template<typename _Res, typename... _ArgTypes, bool _Noex>
    class copyable_function<_Res(_ArgTypes...) _GLIBCXX_MOF_CV
			    _GLIBCXX_MOF_REF noexcept(_Noex)>
    : __polyfunc::_Cpy_base
    {
      static_assert(
	(std::__is_complete_or_unbounded(__type_identity<_ArgTypes>()) && ...),
	"each parameter type must be a complete class");

      using _Base = __polyfunc::_Cpy_base;
      using _Invoker = __polyfunc::_Invoker<_Noex, _Res, _ArgTypes...>;
      using _Signature = _Invoker::_Signature;

      template<typename _Tp>
	using __callable
	  = __conditional_t<_Noex,
			    is_nothrow_invocable_r<_Res, _Tp, _ArgTypes...>,
			    is_invocable_r<_Res, _Tp, _ArgTypes...>>;

      // [func.wrap.copy.con]/1 is-callable-from<VT>
      template<typename _Vt>
	static constexpr bool __is_callable_from
	  = __and_v<__callable<_Vt _GLIBCXX_MOF_CV_REF>,
		    __callable<_Vt _GLIBCXX_MOF_INV_QUALS>>;

    public:
      using result_type = _Res;

      /// Creates an empty object.
      copyable_function() noexcept { }

      /// Creates an empty object.
      copyable_function(nullptr_t) noexcept { }

      /// Moves the target object, leaving the source empty.
      copyable_function(copyable_function&& __x) noexcept
      : _Base(static_cast<_Base&&>(__x)),
	_M_invoke(std::__exchange(__x._M_invoke, nullptr))
      { }

      /// Copies the target object.
      copyable_function(copyable_function const& __x)
      : _Base(static_cast<const _Base&>(__x)),
	_M_invoke(__x._M_invoke)
      { }

      /// Stores a target object initialized from the argument.
      template<typename _Fn, typename _Vt = decay_t<_Fn>>
	requires (!is_same_v<_Vt, copyable_function>)
	  && (!__is_in_place_type_v<_Vt>) && __is_callable_from<_Vt>
	copyable_function(_Fn&& __f) noexcept(_S_nothrow_init<_Vt, _Fn>())
	{
	  static_assert(is_copy_constructible_v<_Vt>);
	  if constexpr (is_function_v<remove_pointer_t<_Vt>>
			|| is_member_pointer_v<_Vt>
			|| __is_polymorphic_function_v<_Vt>)
	    {
	      if (__f == nullptr)
		return;
	    }

	  if constexpr (!__is_polymorphic_function_v<_Vt>
			  || !__polyfunc::__is_invoker_convertible<_Vt, copyable_function>())
	    {
	      _M_init<_Vt>(std::forward<_Fn>(__f));
	      _M_invoke = _Invoker::template _S_storage<_Vt _GLIBCXX_MOF_INV_QUALS>();
	    }
	  else if constexpr (is_lvalue_reference_v<_Fn>)
	    {
	      _M_copy(__polyfunc::__base_of(__f));
	      _M_invoke = __polyfunc::__invoker_of(__f);
	    }
	  else
	    {
	      _M_move(__polyfunc::__base_of(__f));
	      _M_invoke = std::__exchange(__polyfunc::__invoker_of(__f), nullptr);
	    }
	}

      /// Stores a target object initialized from the arguments.
      template<typename _Tp, typename... _Args>
	requires is_constructible_v<_Tp, _Args...>
	  && __is_callable_from<_Tp>
	explicit
	copyable_function(in_place_type_t<_Tp>, _Args&&... __args)
	noexcept(_S_nothrow_init<_Tp, _Args...>())
	: _M_invoke(_Invoker::template _S_storage<_Tp _GLIBCXX_MOF_INV_QUALS>())
	{
	  static_assert(is_same_v<decay_t<_Tp>, _Tp>);
	  static_assert(is_copy_constructible_v<_Tp>);
	  _M_init<_Tp>(std::forward<_Args>(__args)...);
	}

      /// Stores a target object initialized from the arguments.
      template<typename _Tp, typename _Up, typename... _Args>
	requires is_constructible_v<_Tp, initializer_list<_Up>&, _Args...>
	  && __is_callable_from<_Tp>
	explicit
	copyable_function(in_place_type_t<_Tp>, initializer_list<_Up> __il,
			   _Args&&... __args)
	noexcept(_S_nothrow_init<_Tp, initializer_list<_Up>&, _Args...>())
	: _M_invoke(_Invoker::template _S_storage<_Tp _GLIBCXX_MOF_INV_QUALS>())
	{
	  static_assert(is_same_v<decay_t<_Tp>, _Tp>);
	  static_assert(is_copy_constructible_v<_Tp>);
	  _M_init<_Tp>(__il, std::forward<_Args>(__args)...);
	}

      /// Stores a new target object, leaving `x` empty.
      copyable_function&
      operator=(copyable_function&& __x) noexcept
      {
	// Standard requires support of self assigment, by specifying it as
	// copy and swap.
	if (this != std::addressof(__x)) [[likely]]
	  {
	    _Base::operator=(static_cast<_Base&&>(__x));
	    _M_invoke = std::__exchange(__x._M_invoke, nullptr);
	  }
	return *this;
      }

      /// Stores a copy of the source target object
      copyable_function&
      operator=(const copyable_function& __x)
      {
	copyable_function(__x).swap(*this);
	return *this;
      }

      /// Destroys the target object (if any).
      copyable_function&
      operator=(nullptr_t) noexcept
      {
	_M_reset();
	_M_invoke = nullptr;
	return *this;
      }

      /// Stores a new target object, initialized from the argument.
      template<typename _Fn>
	requires is_constructible_v<copyable_function, _Fn>
	copyable_function&
	operator=(_Fn&& __f)
	noexcept(is_nothrow_constructible_v<copyable_function, _Fn>)
	{
	  copyable_function(std::forward<_Fn>(__f)).swap(*this);
	  return *this;
	}

      ~copyable_function() = default;

      /// True if a target object is present, false otherwise.
      explicit operator bool() const noexcept
      { return _M_invoke != nullptr; }

      /** Invoke the target object.
       *
       * The target object will be invoked using the supplied arguments,
       * and as an lvalue or rvalue, and as const or non-const, as dictated
       * by the template arguments of the `copyable_function` specialization.
       *
       * @pre Must not be empty.
       */
      _Res
      operator()(_ArgTypes... __args) _GLIBCXX_MOF_CV_REF noexcept(_Noex)
      {
	__glibcxx_assert(*this != nullptr);
	return _M_invoke(this->_M_storage, std::forward<_ArgTypes>(__args)...);
      }

      /// Exchange the target objects (if any).
      void
      swap(copyable_function& __x) noexcept
      {
	_Base::swap(__x);
	std::swap(_M_invoke, __x._M_invoke);
      }

      /// Exchange the target objects (if any).
      friend void
      swap(copyable_function& __x, copyable_function& __y) noexcept
      { __x.swap(__y); }

      /// Check for emptiness by comparing with `nullptr`.
      friend bool
      operator==(const copyable_function& __x, nullptr_t) noexcept
      { return __x._M_invoke == nullptr; }

    private:
      typename _Invoker::__storage_func_t _M_invoke = nullptr;

      template<typename _Func>
	friend auto&
	__polyfunc::__invoker_of(_Func&) noexcept;

      template<typename _Func>
	friend auto&
	__polyfunc::__base_of(_Func&) noexcept;

      template<typename _Dst, typename _Src>
	friend consteval bool
	__polyfunc::__is_invoker_convertible() noexcept;
    };

#undef _GLIBCXX_MOF_CV_REF
#undef _GLIBCXX_MOF_CV
#undef _GLIBCXX_MOF_REF
#undef _GLIBCXX_MOF_INV_QUALS

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
