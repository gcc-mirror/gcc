// Implementation of std::function_ref -*- C++ -*-

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

/** @file include/bits/funcref_impl.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_MOF_CV
# define _GLIBCXX_MOF_CV
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @cond undocumented
  namespace __polyfunc
  {
    template<bool _Noex, typename _Ret, typename _Class, typename... _Args>
      struct __skip_first_arg<_Ret(_Class::*)(_Args...) _GLIBCXX_MOF_CV
			      noexcept(_Noex)>
      { using type = _Ret(_Args...) noexcept(_Noex); };

    template<bool _Noex, typename _Ret, typename _Class, typename... _Args>
      struct __skip_first_arg<_Ret(_Class::*)(_Args...) _GLIBCXX_MOF_CV&
			      noexcept(_Noex)>
      { using type = _Ret(_Args...) noexcept(_Noex); };
  } // namespace __polyfunc
  /// @endcond

  /**
   *  @brief Non-owning polymorphic function wrapper.
   *  @ingroup functors
   *  @since C++26
   *  @headerfile functional
   *
   *  The `std::function_ref` class template is a non-owning call wrapper,
   *  that refers to a bound object. Using function_ref outside of the lifetime
   *  of the bound object has undefined behavior.
   *
   *  It supports const-qualification and no-throw guarantees. The
   *  qualifications and exception-specification of the signature are respected
   *  when invoking the reference function.
   */
  template<typename _Res, typename... _ArgTypes, bool _Noex>
    class function_ref<_Res(_ArgTypes...) _GLIBCXX_MOF_CV
		       noexcept(_Noex)>
    {
      static_assert(
	(std::__is_complete_or_unbounded(__type_identity<_ArgTypes>()) && ...),
	"each parameter type must be a complete class");

      using _Invoker = __polyfunc::_Invoker<_Noex, _Res, _ArgTypes...>;
      using _Signature = _Invoker::_Signature;

      // [func.wrap.ref.ctor]/1 is-invokable-using
      template<typename... _Tps>
	static constexpr bool __is_invocable_using
	  = __conditional_t<_Noex,
			    is_nothrow_invocable_r<_Res, _Tps..., _ArgTypes...>,
			    is_invocable_r<_Res, _Tps..., _ArgTypes...>>::value;

    public:
      /// Target and bound object is function pointed by parameter.
      template<typename _Fn>
	requires is_function_v<_Fn> && __is_invocable_using<_Fn*>
	function_ref(_Fn* __fn) noexcept
	{
	  __glibcxx_assert(__fn != nullptr);
	  _M_invoke = _Invoker::template _S_ptrs<_Fn*>();
	  _M_init(__fn);
	}

      /// Target and bound object is object referenced by parameter.
      template<typename _Fn, typename _Vt = remove_reference_t<_Fn>>
	requires (!is_same_v<remove_cv_t<_Vt>, function_ref>)
	       && (!is_member_pointer_v<_Vt>)
	       // We deviate from standard by having this condition, that forces
	       // function references to use _Fn* constructors. This simplies
	       // implementation and provide better diagnostic when used in
	       // constant expression (above constructor is not constexpr).
	       && (!is_function_v<_Vt>)
	       && __is_invocable_using<_Vt _GLIBCXX_MOF_CV&>
	constexpr
	function_ref(_Fn&& __f) noexcept
	{
	  _M_invoke = _Invoker::template _S_ptrs<_Vt _GLIBCXX_MOF_CV&>();
	  _M_init(std::addressof(__f));
	}

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 4256. Incorrect constrains for function_ref constructors from nontype
      /// Target object is __fn. There is no bound object.
      template<auto __fn>
	requires __is_invocable_using<const decltype(__fn)&>
	constexpr
	function_ref(nontype_t<__fn>) noexcept
	{
	  using _Fn = remove_cv_t<decltype(__fn)>;
	  if constexpr (is_pointer_v<_Fn> || is_member_pointer_v<_Fn>)
	    static_assert(__fn != nullptr);

	  _M_invoke = &_Invoker::template _S_nttp<__fn>;
	  _M_ptrs._M_obj = nullptr;
	}

      /// Target object is equivalent to std::bind_front<_fn>(std::ref(__ref)).
      /// Bound object is object referenced by second parameter.
      template<auto __fn, typename _Up, typename _Td = remove_reference_t<_Up>>
	requires (!is_rvalue_reference_v<_Up&&>)
	  && __is_invocable_using<const decltype(__fn)&, _Td _GLIBCXX_MOF_CV&>
	constexpr
	function_ref(nontype_t<__fn>, _Up&& __ref) noexcept
	{
	  using _Fn = remove_cv_t<decltype(__fn)>;
	  if constexpr (is_pointer_v<_Fn> || is_member_pointer_v<_Fn>)
	    static_assert(__fn != nullptr);

	  if constexpr (is_member_pointer_v<_Fn>
			  && same_as<_Td, typename __inv_unwrap<_Td>::type>)
	    // N.B. invoking member pointer on lvalue produces the same effects,
	    // as invoking it on pointer to that lvalue.
	    _M_invoke = &_Invoker::template _S_bind_ptr<__fn, _Td _GLIBCXX_MOF_CV>;
	  else
	    _M_invoke = &_Invoker::template _S_bind_ref<__fn, _Td _GLIBCXX_MOF_CV&>;
	  _M_init(std::addressof(__ref));
	}

      /// Target object is equivalent to std::bind_front<_fn>(__ptr).
      /// Bound object is object pointed by second parameter (if any).
      template<auto __fn, typename _Td>
	requires __is_invocable_using<const decltype(__fn)&, _Td _GLIBCXX_MOF_CV*>
	constexpr
	function_ref(nontype_t<__fn>, _Td _GLIBCXX_MOF_CV* __ptr) noexcept
	{
	  using _Fn = remove_cv_t<decltype(__fn)>;
	  if constexpr (is_pointer_v<_Fn> || is_member_pointer_v<_Fn>)
	    static_assert(__fn != nullptr);
	  if constexpr (is_member_pointer_v<_Fn>)
	    __glibcxx_assert(__ptr != nullptr);

	  _M_invoke = &_Invoker::template _S_bind_ptr<__fn, _Td _GLIBCXX_MOF_CV>;
	  _M_init(__ptr);
	}

      template<typename _Tp>
	requires (!is_same_v<_Tp, function_ref>)
	       && (!is_pointer_v<_Tp>) && (!__is_nontype_v<_Tp>)
	function_ref&
	operator=(_Tp) = delete;

      /** Invoke the target object.
       *
       * The bound object will be invoked using the supplied arguments,
       * and as const or non-const, as dictated by the template arguments
       * of the `function_ref` specialization.
       */
      _Res
      operator()(_ArgTypes... __args) const noexcept(_Noex)
      { return _M_invoke(_M_ptrs, std::forward<_ArgTypes>(__args)...); }

    private:
      template<typename _Tp>
	constexpr void
	_M_init(_Tp* __ptr) noexcept
	{
	  if constexpr (is_function_v<_Tp>)
	    _M_ptrs._M_func = reinterpret_cast<void(*)()>(__ptr);
	  else
	    _M_ptrs._M_obj = __ptr;
	}

      typename _Invoker::__ptrs_func_t _M_invoke;
      __polyfunc::_Ptrs _M_ptrs;
    };

#undef _GLIBCXX_MOF_CV

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
