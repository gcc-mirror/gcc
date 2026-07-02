// Implementation of <simd> -*- C++ -*-

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

#ifndef _GLIBCXX_SIMD_VEC_H
#define _GLIBCXX_SIMD_VEC_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#if __cplusplus >= 202400L

#include "simd_mask.h"
#include "simd_flags.h"

#include <bits/utility.h>
#include <bits/stl_function.h>
#include <cmath>

// psabi warnings are bogus because the ABI of the internal types never leaks into user code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpsabi"

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace simd
{
  // disabled basic_vec
  template <typename _Tp, typename _Ap>
    class basic_vec
    {
    public:
      using value_type = _Tp;

      using abi_type = _Ap;

      using mask_type = basic_mask<0, void>; // disabled

#define _GLIBCXX_DELETE_SIMD "This specialization is disabled because of an invalid combination "  \
			     "of template arguments to basic_vec."

      basic_vec() = delete(_GLIBCXX_DELETE_SIMD);

      ~basic_vec() = delete(_GLIBCXX_DELETE_SIMD);

      basic_vec(const basic_vec&) = delete(_GLIBCXX_DELETE_SIMD);

      basic_vec& operator=(const basic_vec&) = delete(_GLIBCXX_DELETE_SIMD);

#undef _GLIBCXX_DELETE_SIMD
    };

  template <typename _Tp, typename _Ap>
    class _VecBase
    {
      using _Vp = basic_vec<_Tp, _Ap>;

    public:
      using value_type = _Tp;

      using abi_type = _Ap;

      using mask_type = basic_mask<sizeof(_Tp), abi_type>;

      using iterator = __iterator<_Vp>;

      using const_iterator = __iterator<const _Vp>;

      constexpr iterator
      begin() noexcept
      { return {static_cast<_Vp&>(*this), 0}; }

      constexpr const_iterator
      begin() const noexcept
      { return cbegin(); }

      constexpr const_iterator
      cbegin() const noexcept
      { return {static_cast<const _Vp&>(*this), 0}; }

      constexpr default_sentinel_t
      end() const noexcept
      { return {}; }

      constexpr default_sentinel_t
      cend() const noexcept
      { return {}; }

      static constexpr auto size = __simd_size_c<_Ap::_S_size>;

      _VecBase() = default;

      // LWG issue from 2026-03-04 / P4042R0
      template <typename _Up, typename _UAbi>
	requires (_Ap::_S_size != _UAbi::_S_size)
	_VecBase(const basic_vec<_Up, _UAbi>&) = delete("size mismatch");

      template <typename _Up, typename _UAbi>
	requires (_Ap::_S_size == _UAbi::_S_size) && (!__explicitly_convertible_to<_Up, _Tp>)
	explicit
	_VecBase(const basic_vec<_Up, _UAbi>&)
	  = delete("the value types are not convertible");

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator+(const _Vp& __x, const _Vp& __y) noexcept
      {
	_Vp __r = __x;
	__r += __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator-(const _Vp& __x, const _Vp& __y) noexcept
      {
	_Vp __r = __x;
	__r -= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator*(const _Vp& __x, const _Vp& __y) noexcept
      {
	_Vp __r = __x;
	__r *= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator/(const _Vp& __x, const _Vp& __y) noexcept
      {
	_Vp __r = __x;
	__r /= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator%(const _Vp& __x, const _Vp& __y) noexcept
      requires requires (_Tp __a) { __a % __a; }
      {
	_Vp __r = __x;
	__r %= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator&(const _Vp& __x, const _Vp& __y) noexcept
      requires requires (_Tp __a) { __a & __a; }
      {
	_Vp __r = __x;
	__r &= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator|(const _Vp& __x, const _Vp& __y) noexcept
      requires requires (_Tp __a) { __a | __a; }
      {
	_Vp __r = __x;
	__r |= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator^(const _Vp& __x, const _Vp& __y) noexcept
      requires requires (_Tp __a) { __a ^ __a; }
      {
	_Vp __r = __x;
	__r ^= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator<<(const _Vp& __x, const _Vp& __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires (_Tp __a) { __a << __a; }
      {
	_Vp __r = __x;
	__r <<= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator<<(const _Vp& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires (_Tp __a, __simd_size_type __b) { __a << __b; }
      {
	_Vp __r = __x;
	__r <<= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator>>(const _Vp& __x, const _Vp& __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires (_Tp __a) { __a >> __a; }
      {
	_Vp __r = __x;
	__r >>= __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr _Vp
      operator>>(const _Vp& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires (_Tp __a, __simd_size_type __b) { __a >> __b; }
      {
	_Vp __r = __x;
	__r >>= __y;
	return __r;
      }
    };

  struct _LoadCtorTag
  {};

  template <integral _Tp>
    inline constexpr _Tp __max_shift
      = (sizeof(_Tp) < sizeof(int) ? sizeof(int) : sizeof(_Tp)) * __CHAR_BIT__;

  template <__vectorizable _Tp, __abi_tag _Ap>
    requires (_Ap::_S_nreg == 1)
      && (!__complex_like<_Tp>)
    class basic_vec<_Tp, _Ap>
    : public _VecBase<_Tp, _Ap>
    {
      template <typename, typename>
	friend class basic_vec;

      template <size_t, typename>
	friend class basic_mask;

      static constexpr int _S_size = _Ap::_S_size;

      static constexpr int _S_full_size = __bit_ceil(unsigned(_S_size));

      static constexpr bool _S_is_scalar = _S_size == 1;

      static constexpr bool _S_use_bitmask = _Ap::_S_is_bitmask && !_S_is_scalar;

      using _DataType = typename _Ap::template _DataType<_Tp>;

      /** @internal
       * @brief Underlying vector data storage.
       *
       * This member holds the vector object using a GNU vector type or a platform-specific vector
       * type determined by the ABI tag. For size 1 vectors, this is a single value (_Tp).
       */
      _DataType _M_data;

      static constexpr bool _S_is_partial = sizeof(_M_data) > sizeof(_Tp) * _S_size;

      using __canon_value_type = __canonical_vec_type_t<_Tp>;

    public:
      using value_type = _Tp;

      using mask_type = _VecBase<_Tp, _Ap>::mask_type;

      // internal but public API ----------------------------------------------
      [[__gnu__::__always_inline__]]
      static constexpr basic_vec
      _S_init(_DataType __x)
      {
	basic_vec __r;
	__r._M_data = __x;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr _DataType&
      _M_get() noexcept
      { return _M_data; }

      [[__gnu__::__always_inline__]]
      constexpr const _DataType&
      _M_get() const noexcept
      { return _M_data; }

      [[__gnu__::__always_inline__]]
      friend constexpr bool
      __is_const_known(const basic_vec& __x)
      { return __builtin_constant_p(__x._M_data); }

      [[__gnu__::__always_inline__]]
      constexpr auto
      _M_concat_data([[maybe_unused]] bool __do_sanitize = false) const
      {
	if constexpr (_S_is_scalar)
	  return __vec_builtin_type<__canon_value_type, 1>{_M_data};
	else
	  return _M_data;
      }

      template <int _Size = _S_size, int _Offset = 0, typename _A0, typename _Fp>
	[[__gnu__::__always_inline__]]
	static constexpr basic_vec
	_S_static_permute(const basic_vec<value_type, _A0>& __x, _Fp&& __idxmap)
	{
	  using _Xp = basic_vec<value_type, _A0>;
	  basic_vec __r;
	  if constexpr (_S_is_scalar)
	    {
	      constexpr __simd_size_type __j = [&] consteval {
		if constexpr (__index_permutation_function_sized<_Fp>)
		  return __idxmap(_Offset, _Size);
		else
		  return __idxmap(_Offset);
	      }();
	      if constexpr (__j == simd::zero_element || __j == simd::uninit_element)
		return basic_vec();
	      else
		static_assert(__j >= 0 && __j < _Xp::_S_size);
	      __r._M_data = __x[__j];
	    }
	  else
	    {
	      auto __idxmap2 = [=](auto __i) consteval {
		if constexpr (int(__i + _Offset) >= _Size) // _S_full_size > _Size
		  return __simd_size_c<simd::uninit_element>;
		else if constexpr (__index_permutation_function_sized<_Fp>)
		  return __simd_size_c<__idxmap(__i + _Offset, _Size)>;
		else
		  return __simd_size_c<__idxmap(__i + _Offset)>;
	      };
	      constexpr auto __adj_idx = [](auto __i) {
		constexpr int __j = __i;
		if constexpr (__j == simd::zero_element)
		  return __simd_size_c<__bit_ceil(unsigned(_Xp::_S_size))>;
		else if constexpr (__j == simd::uninit_element)
		  return __simd_size_c<-1>;
		else
		  {
		    static_assert(__j >= 0 && __j < _Xp::_S_size);
		    return __simd_size_c<__j>;
		  }
	      };
	      constexpr auto [...__is0] = _IotaArray<_S_size>;
	      constexpr bool __needs_zero_element
		= ((__idxmap2(__simd_size_c<__is0>).value == simd::zero_element) || ...);
	      constexpr auto [...__is_full] = _IotaArray<_S_full_size>;
	      if constexpr (_A0::_S_nreg == 2 && !__needs_zero_element)
		{
		  __r._M_data = __builtin_shufflevector(
				  __x._M_data0._M_data, __x._M_data1._M_data,
				  __adj_idx(__idxmap2(__simd_size_c<__is_full>)).value...);
		}
	      else
		{
		  __r._M_data = __builtin_shufflevector(
				  __x._M_concat_data(), decltype(__x._M_concat_data())(),
				  __adj_idx(__idxmap2(__simd_size_c<__is_full>)).value...);
		}
	    }
	  return __r;
	}

      template <typename _Vp>
	[[__gnu__::__always_inline__]]
	constexpr auto
	_M_chunk() const noexcept
	{
	  constexpr int __n = _S_size / _Vp::_S_size;
	  constexpr int __rem = _S_size % _Vp::_S_size;
	  constexpr auto [...__is] = _IotaArray<__n>;
	  if constexpr (__rem == 0)
	    return array<_Vp, __n> {__extract_simd_at<_Vp>(cw<_Vp::_S_size * __is>, *this)...};
	  else
	    {
	      using _Rest = resize_t<__rem, _Vp>;
	      return tuple(__extract_simd_at<_Vp>(cw<_Vp::_S_size * __is>, *this)...,
			   __extract_simd_at<_Rest>(cw<_Vp::_S_size * __n>, *this));
	    }
	}

      [[__gnu__::__always_inline__]]
      static constexpr basic_vec
      _S_concat(const basic_vec& __x0) noexcept
      { return __x0; }

      template <typename... _As>
	requires (sizeof...(_As) > 1)
	[[__gnu__::__always_inline__]]
	static constexpr basic_vec
	_S_concat(const basic_vec<value_type, _As>&... __xs) noexcept
	{
	  static_assert(_S_size == (_As::_S_size + ...));
	  return __extract_simd_at<basic_vec>(cw<0>, __xs...);
	}

      /** @internal
       * Shifts elements to the front by @p _Shift positions (or to the back for negative @p
       * _Shift).
       *
       * This function moves elements towards lower indices (front of the vector).
       * Elements that would shift beyond the vector bounds are replaced with zero. Negative shift
       * values shift in the opposite direction.
       *
       * @warning The naming can be confusing due to little-endian byte order:
       * - Despite the name "shifted_to_front", the underlying hardware instruction
       *   shifts bits to the right (psrl...)
       * - The function name refers to element indices, not bit positions
       *
       * @tparam _Shift Number of positions to shift elements towards the front.
       *                Must be -size() < _Shift < size().
       *
       * @return A new vector with elements shifted to front or back.
       *
       * Example:
       * @code
       * __iota<vec<int, 4>>._M_elements_shifted_to_front<2>(); // {2, 3, 0, 0}
       * __iota<vec<int, 4>>._M_elements_shifted_to_front<-2>(); // {0, 0, 0, 1}
       * @endcode
       */
      template <int _Shift, _ArchTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr basic_vec
	_M_elements_shifted_to_front() const
	{
	  static_assert(_Shift < _S_size && -_Shift < _S_size);
	  if constexpr (_Shift == 0)
	    return *this;
	  else
	    return _S_static_permute(*this, [](int __i) consteval {
		     int __off = __i + _Shift;
		     return __off >= _S_size || __off < 0 ? zero_element : __off;
		   });
	}

      /** @internal
       * @brief Set padding elements to @p __id; add more padding elements if necessary.
       *
       * @note This function can rearrange the element order since the result is only used for
       * reductions.
       */
      template <typename _Vp, __canon_value_type __id>
	[[__gnu__::__always_inline__]]
	constexpr _Vp
	_M_pad_to_T_with_value() const noexcept
	{
	  static_assert(!_Vp::_S_is_partial);
	  static_assert(_Ap::_S_nreg == 1);
	  if constexpr (sizeof(_Vp) == 32)
	    { // when we need to reduce from a 512-bit register
	      static_assert(sizeof(_M_data) == 32);
	      constexpr auto __k = _Vp::mask_type::_S_partial_mask_of_n(_S_size);
	      return __select_impl(__k, _Vp::_S_init(_M_data), __id);
	    }
	  else
	    {
	      static_assert(sizeof(_Vp) <= 16); // => max. 7 Bytes need to be zeroed
	      static_assert(sizeof(_M_data) <= sizeof(_Vp));
	      _Vp __v1 = __vec_zero_pad_to<sizeof(_Vp)>(_M_data);
	      if constexpr (__id == 0 && _S_is_partial)
		// cheapest solution: shift values to the back while shifting in zeros
		// This is valid because we shift out padding elements and use all elements in a
		// subsequent reduction.
		__v1 = __v1.template _M_elements_shifted_to_front<-(_Vp::_S_size - _S_size)>();
	      else if constexpr (_Vp::_S_size - _S_size == 1)
		// if a single element needs to be changed, use an insert instruction
		__vec_set(__v1._M_data, _Vp::_S_size - 1, __id);
	      else if constexpr (__has_single_bit(unsigned(_Vp::_S_size - _S_size)))
		{ // if 2^n elements need to be changed, use a single insert instruction
		  constexpr int __n = _Vp::_S_size - _S_size;
		  using _Ip = __integer_from<__n * sizeof(__canon_value_type)>;
		  constexpr auto [...__is] = _IotaArray<__n>;
		  constexpr __canon_value_type __idn[__n] = {((void)__is, __id)...};
		  auto __vn = __vec_bit_cast<_Ip>(__v1._M_data);
		  __vec_set(__vn, _Vp::_S_size / __n - 1, __builtin_bit_cast(_Ip, __idn));
		  __v1._M_data = reinterpret_cast<typename _Vp::_DataType>(__vn);
		}
	      else if constexpr (__id != 0 && !_S_is_partial)
		{ // if __vec_zero_pad_to added zeros in all the places where we need __id, a
		  // bitwise or is sufficient (needs a vector constant for the __id vector, which
		  // isn't optimal)
		  constexpr _Vp __idn([](int __i) {
				  return __i >= _S_size ? __id : __canon_value_type();
				});
		  __v1._M_data = __vec_or(__v1._M_data, __idn._M_data);
		}
	      else if constexpr (__id != 0 || _S_is_partial)
		{ // fallback
		  constexpr auto __k = _Vp::mask_type::_S_partial_mask_of_n(_S_size);
		  __v1 = __select_impl(__k, __v1, __id);
		}
	      return __v1;
	    }
	}

      [[__gnu__::__always_inline__]]
      constexpr auto
      _M_reduce_to_half(auto __binary_op) const
      {
	static_assert(__has_single_bit(unsigned(_S_size)));
	auto [__a, __b] = chunk<_S_size / 2>(*this);
	return __binary_op(__a, __b);
      }

      template <typename _Rest, typename _BinaryOp>
	[[__gnu__::__always_inline__]]
	constexpr value_type
	_M_reduce_tail(const _Rest& __rest, _BinaryOp __binary_op) const
	{
	  if constexpr (_S_is_scalar)
	    return __binary_op(*this, __rest)._M_data;
	  else if constexpr (_Rest::_S_size == _S_size)
	    return __binary_op(*this, __rest)._M_reduce(__binary_op);
	  else if constexpr (_Rest::_S_size > _S_size)
	    {
	      auto [__a, __b] = __rest.template _M_chunk<basic_vec>();
	      return __binary_op(*this, __a)._M_reduce_tail(__b, __binary_op);
	    }
	  else if constexpr (_Rest::_S_size == 1)
	    return __binary_op(_Rest(_M_reduce(__binary_op)), __rest)[0];
	  else if constexpr (sizeof(_M_data) <= 16
			       && requires { __default_identity_element<__canon_value_type, _BinaryOp>(); })
	    { // extend __rest with identity element for more parallelism
	      constexpr __canon_value_type __id
		= __default_identity_element<__canon_value_type, _BinaryOp>();
	      return __binary_op(_M_data, __rest.template _M_pad_to_T_with_value<basic_vec, __id>())
		       ._M_reduce(__binary_op);
	    }
	  else
	    return _M_reduce_to_half(__binary_op)._M_reduce_tail(__rest, __binary_op);
	}

      /** @internal
       * @brief Reduction over @p __binary_op of all (non-padding) elements.
       *
       * @note The implementation assumes it is most efficient to first reduce to one 128-bit SIMD
       * register and then shuffle elements while sticking to 128-bit registers.
       */
      template <typename _BinaryOp, _ArchTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr value_type
	_M_reduce(_BinaryOp __binary_op) const
	{
	  constexpr bool __have_id_elem
	    = requires { __default_identity_element<__canon_value_type, _BinaryOp>(); };
	  if constexpr (_S_size == 1)
	    return operator[](0);
	  else if constexpr (_Traits.template _M_eval_as_f32<value_type>()
			       && (is_same_v<_BinaryOp, plus<>>
				     || is_same_v<_BinaryOp, multiplies<>>))
	    return value_type(rebind_t<float, basic_vec>(*this)._M_reduce(__binary_op));
#ifdef __SSE2__
	  else if constexpr (is_integral_v<value_type> && sizeof(value_type) == 1
			       && is_same_v<decltype(__binary_op), multiplies<>>)
	    {
	      // convert to unsigned short because of missing 8-bit mul instruction
	      // we don't need to preserve the order of elements
	      //
	      // The left columns under Latency and Throughput show bit-cast to ushort with shift by
	      // 8. The right column uses the alternative in the else branch.
	      // Benchmark on Intel Ultra 7 165U (AVX2)
	      //   TYPE            Latency      Throughput
	      //             [cycles/call]   [cycles/call]
	      //schar, 2        9.11  7.73      3.17  3.21
	      //schar, 4        31.6  34.9      5.11  6.97
	      //schar, 8        35.7  41.5      7.77  7.17
	      //schar, 16       36.7  44.1      6.66  8.96
	      //schar, 32       42.2  61.1      8.82  10.1
	      if constexpr (!_S_is_partial)
		{ // If all elements participate in the reduction we can take this shortcut
		  using _V16 = resize_t<_S_size / 2, rebind_t<unsigned short, basic_vec>>;
		  auto __a = __builtin_bit_cast(_V16, *this);
		  return __binary_op(__a, __a >> 8)._M_reduce(__binary_op);
		}
	      else
		{
		  using _V16 = rebind_t<unsigned short, basic_vec>;
		  return _V16(*this)._M_reduce(__binary_op);
		}
	    }
#endif
	  else if constexpr (__has_single_bit(unsigned(_S_size)))
	    {
	      if constexpr (sizeof(_M_data) > 16)
		return _M_reduce_to_half(__binary_op)._M_reduce(__binary_op);
	      else if constexpr (_S_size == 2)
		return _M_reduce_to_half(__binary_op)[0];
	      else
		{
		  static_assert(_S_size <= 16);
		  auto __x = *this;
#ifdef __SSE2__
		  if constexpr (sizeof(_M_data) <= 16 && is_integral_v<value_type>)
		    {
		      if constexpr (_S_size > 8)
			__x = __binary_op(__x, __x.template _M_elements_shifted_to_front<8>());
		      if constexpr (_S_size > 4)
			__x = __binary_op(__x, __x.template _M_elements_shifted_to_front<4>());
		      if constexpr (_S_size > 2)
			__x = __binary_op(__x, __x.template _M_elements_shifted_to_front<2>());
		      // We could also call __binary_op with vec<T, 1> arguments. However,
		      // micro-benchmarking on Intel Ultra 7 165U showed this to be more efficient:
		      return __binary_op(__x, __x.template _M_elements_shifted_to_front<1>())[0];
		    }
#endif
		  if constexpr (_S_size > 8)
		    __x = __binary_op(__x, _S_static_permute(__x, _SwapNeighbors<8>()));
		  if constexpr (_S_size > 4)
		    __x = __binary_op(__x, _S_static_permute(__x, _SwapNeighbors<4>()));
#ifdef __SSE2__
		  // avoid pshufb by "promoting" to int
		  if constexpr (is_integral_v<value_type> && sizeof(value_type) <= 1)
		    return value_type(resize_t<4, rebind_t<int, basic_vec>>(chunk<4>(__x)[0])
					._M_reduce(__binary_op));
#endif
		  if constexpr (_S_size > 2)
		    __x = __binary_op(__x, _S_static_permute(__x, _SwapNeighbors<2>()));
		  if constexpr (is_integral_v<value_type> && sizeof(value_type) == 2)
		    return __binary_op(__x, _S_static_permute(__x, _SwapNeighbors<1>()))[0];
		  else
		    return __binary_op(vec<value_type, 1>(__x[0]), vec<value_type, 1>(__x[1]))[0];
		}
	    }
	  else if constexpr (sizeof(_M_data) == 32)
	    {
	      const auto [__lo, __hi] = chunk<__bit_floor(unsigned(_S_size))>(*this);
	      return __lo._M_reduce_tail(__hi, __binary_op);
	    }
	  else if constexpr (sizeof(_M_data) == 64)
	    {
	      // e.g. _S_size = 16 + 16 + 15 (vec<char, 47>)
	      // -> 8 + 8 + 7 -> 4 + 4 + 3 -> 2 + 2 + 1 -> 1
	      auto __chunked = chunk<__bit_floor(unsigned(_S_size)) / 2>(*this);
	      using _Cp = decltype(__chunked);
	      if constexpr (tuple_size_v<_Cp> == 4)
		{
		  const auto& [__a, __b, __c, __rest] = __chunked;
		  constexpr bool __amd_cpu = _Traits._M_have_sse4a();
		  if constexpr (__have_id_elem && __rest._S_size > 1 && __amd_cpu)
		    { // do one 256-bit op -> one 128-bit op
		      // 4 cycles on Zen4/5 until _M_reduce (short, 26, plus<>)
		      // 9 cycles on Skylake-AVX512 until _M_reduce
		      // 9 cycles on Zen4/5 until _M_reduce (short, 27, multiplies<>)
		      // 17 cycles on Skylake-AVX512 until _M_reduce (short, 27, multiplies<>)
		      const auto& [__a, __rest] = chunk<__bit_floor(unsigned(_S_size))>(*this);
		      using _Vp = remove_cvref_t<decltype(__a)>;
		      constexpr __canon_value_type __id
			= __default_identity_element<__canon_value_type, _BinaryOp>();
		      const _Vp __b = __rest.template _M_pad_to_T_with_value<_Vp, __id>();
		      return __binary_op(__a, __b)._M_reduce(__binary_op);
		    }
		  else if constexpr (__have_id_elem && __rest._S_size > 1)
		    { // do two 128-bit ops -> one 128-bit op
		      // 5 cycles on Zen4/5 until _M_reduce (short, 26, plus<>)
		      // 7 cycles on Skylake-AVX512 until _M_reduce (short, 26, plus<>)
		      // 9 cycles on Zen4/5 until _M_reduce (short, 27, multiplies<>)
		      // 16 cycles on Skylake-AVX512 until _M_reduce (short, 27, multiplies<>)
		      using _Vp = remove_cvref_t<decltype(__a)>;
		      constexpr __canon_value_type __id
			= __default_identity_element<__canon_value_type, _BinaryOp>();
		      const _Vp __d = __rest.template _M_pad_to_T_with_value<_Vp, __id>();
		      return __binary_op(__binary_op(__a, __b), __binary_op(__c, __d))
			       ._M_reduce(__binary_op);
		    }
		  else
		    return __binary_op(__binary_op(__a, __b), __c)
			     ._M_reduce_tail(__rest, __binary_op);
		}
	      else if constexpr (tuple_size_v<_Cp> == 3)
		{
		  const auto& [__a, __b, __rest] = __chunked;
		  return __binary_op(__a, __b)._M_reduce_tail(__rest, __binary_op);
		}
	      else
		static_assert(false);
	    }
	  else if constexpr (__have_id_elem)
	    {
	      constexpr __canon_value_type __id
		= __default_identity_element<__canon_value_type, _BinaryOp>();
	      using _Vp = resize_t<__bit_ceil(unsigned(_S_size)), basic_vec>;
	      return _M_pad_to_T_with_value<_Vp, __id>()._M_reduce(__binary_op);
	    }
	  else
	    {
	      const auto& [__a, __rest] = chunk<__bit_floor(unsigned(_S_size))>(*this);
	      return __a._M_reduce_tail(__rest, __binary_op);
	    }
	}

      // [simd.math] ----------------------------------------------------------
      //
      // ISO/IEC 60559 on the classification operations (5.7.2 General Operations):
      // "They are never exceptional, even for signaling NaNs."
      //
      template <_OptTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr mask_type
	_M_isnan() const requires is_floating_point_v<value_type>
	{
	  if constexpr (_Traits._M_finite_math_only())
	    return mask_type(false);
	  else if constexpr (_S_is_scalar)
	    return mask_type(std::isnan(_M_data));
	  else if constexpr (_S_use_bitmask)
	    return _M_isunordered(*this);
	  else if constexpr (!_Traits._M_support_snan())
	    return !(*this == *this);
	  else if (__is_const_known(_M_data))
	    return mask_type([&](int __i) { return std::isnan(_M_data[__i]); });
	  else
	    {
	      // 60559: NaN is represented as Inf + non-zero mantissa bits
	      using _Ip = __integer_from<sizeof(value_type)>;
	      return __builtin_bit_cast(_Ip, numeric_limits<value_type>::infinity())
		       < __builtin_bit_cast(rebind_t<_Ip, basic_vec>, _M_fabs());
	    }
	}

      template <_TargetTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr mask_type
	_M_isinf() const requires is_floating_point_v<value_type>
	{
	  if constexpr (_Traits._M_finite_math_only())
	    return mask_type(false);
	  else if constexpr (_S_is_scalar)
	    return mask_type(std::isinf(_M_data));
	  else if (__is_const_known(_M_data))
	    return mask_type([&](int __i) { return std::isinf(_M_data[__i]); });
#ifdef _GLIBCXX_X86
	  else if constexpr (_S_use_bitmask)
	    return mask_type::_S_init(__x86_bitmask_isinf(_M_data));
	  else if constexpr (_Traits._M_have_avx512dq())
	    return __x86_bit_to_vecmask<typename mask_type::_DataType>(
		     __x86_bitmask_isinf(_M_data));
#endif
	  else
	    {
	      using _Ip = __integer_from<sizeof(value_type)>;
	      return __vec_bit_cast<_Ip>(_M_fabs()._M_data)
		       == __builtin_bit_cast(_Ip, numeric_limits<value_type>::infinity());
	    }
	}

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      _M_abs() const requires signed_integral<value_type>
      { return _M_data < 0 ? -_M_data : _M_data; }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      _M_fabs() const requires floating_point<value_type>
      {
	if constexpr (_S_is_scalar)
	  return std::fabs(_M_data);
	else
	  return __vec_and(__vec_not(_S_signmask<_DataType>), _M_data);
      }

      template <_TargetTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr mask_type
	_M_isunordered(basic_vec __y) const requires is_floating_point_v<value_type>
	{
	  if constexpr (_Traits._M_finite_math_only())
	    return mask_type(false);
	  else if constexpr (_S_is_scalar)
	    return mask_type(std::isunordered(_M_data, __y._M_data));
#ifdef _GLIBCXX_X86
	  else if constexpr (_S_use_bitmask)
	    return _M_bitmask_cmp<_X86Cmp::_Unord>(__y._M_data);
#endif
	  else
	    return mask_type([&](int __i) {
		     return std::isunordered(_M_data[__i], __y._M_data[__i]);
		   });
	}

      /** @internal
       * Implementation of @ref partial_load.
       *
       * @param __mem  A pointer to an array of @p __n values. Can be complex or real.
       * @param __n    Read no more than @p __n values from memory. However, depending on @p __mem
       *               alignment, out of bounds reads are benign.
       */
      template <typename _Up, _ArchTraits _Traits = {}>
	static inline basic_vec
	_S_partial_load(const _Up* __mem, size_t __n)
	{
	  if constexpr (_S_is_scalar)
	    return __n == 0 ? basic_vec() : basic_vec(static_cast<value_type>(*__mem));
	  else if (__is_const_known_equal_to(__n >= size_t(_S_size), true))
	    return basic_vec(_LoadCtorTag(), __mem);
	  else if constexpr (!__converts_trivially<_Up, value_type>)
	    return static_cast<basic_vec>(rebind_t<_Up, basic_vec>::_S_partial_load(__mem, __n));
	  else
	    {
#if _GLIBCXX_X86
	      if constexpr (_Traits._M_have_avx512f()
			      || (_Traits._M_have_avx() && sizeof(_Up) >= 4))
		{
		  const auto __k = __n < _S_size ? mask_type::_S_partial_mask_of_n(int(__n))
						 : mask_type(true);
		  return _S_masked_load(__mem, mask_type::_S_partial_mask_of_n(int(__n)));
		}
#endif
	      if (__n >= size_t(_S_size)) [[unlikely]]
		return basic_vec(_LoadCtorTag(), __mem);
#if _GLIBCXX_X86 // TODO: where else is this "safe"?
	      // allow out-of-bounds read when it cannot lead to a #GP
	      else if (__is_const_known_equal_to(
			 is_sufficiently_aligned<sizeof(_Up) * _S_full_size>(__mem), true))
		return __select_impl(mask_type::_S_partial_mask_of_n(int(__n)),
				     basic_vec(_LoadCtorTag(), __mem), basic_vec());
#endif
	      else if constexpr (_S_size > 4)
		{
		  alignas(_DataType) byte __dst[sizeof(_DataType)] = {};
		  const byte* __src = reinterpret_cast<const byte*>(__mem);
		  __memcpy_chunks<sizeof(_Up), sizeof(_DataType)>(__dst, __src, __n);
		  return __builtin_bit_cast(_DataType, __dst);
		}
	      else if (__n == 0) [[unlikely]]
		return basic_vec();
	      else if constexpr (_S_size == 2)
		return _DataType {static_cast<value_type>(__mem[0]), 0};
	      else
		{
		  constexpr auto [...__is] = _IotaArray<_S_size - 2>;
		  return _DataType{
		    static_cast<value_type>(__mem[0]),
		    static_cast<value_type>(__is + 1 < __n ? __mem[__is + 1] : 0)...
		  };
		}
	    }
	}

      /** @internal
       * Loads elements from @p __mem according to mask @p __k.
       *
       * @param __mem Pointer (in)to array.
       * @param __k   Mask controlling which elements to load. For each bit i in the mask:
       *              - If bit i is 1: copy __mem[i] into result[i]
       *              - If bit i is 0: result[i] is default initialized
       *
       * @note This function assumes it's called after determining that no other method
       *       (like full load) is more appropriate. Calling with all mask bits set to 1
       *       is suboptimal for performance but still correct.
       */
      template <typename _Up, _ArchTraits _Traits = {}>
	static inline basic_vec
	_S_masked_load(const _Up* __mem, mask_type __k)
	{
	  if constexpr (_S_size == 1)
	    return __k[0] ? static_cast<value_type>(__mem[0]) : value_type();
#if _GLIBCXX_X86
	  else if constexpr (_Traits._M_have_avx512f())
	    return __x86_masked_load<_DataType>(__mem, __k._M_data);
	  else if constexpr (_Traits._M_have_avx() && (sizeof(_Up) == 4 || sizeof(_Up) == 8))
	    {
	      if constexpr (__converts_trivially<_Up, value_type>)
		return __x86_masked_load<_DataType>(__mem, __k._M_data);
	      else
		{
		  using _UV = rebind_t<_Up, basic_vec>;
		  return basic_vec(_UV::_S_masked_load(__mem, typename _UV::mask_type(__k)));
		}
	    }
#endif
	  else if (__k._M_none_of()) [[unlikely]]
	    return basic_vec();
	  else if constexpr (_S_is_scalar)
	    return basic_vec(static_cast<value_type>(*__mem));
	  else
	    {
	      // Use at least 4-byte __bits in __bit_foreach for better code-gen
	      _Bitmask<_S_size < 32 ? 32 : _S_size> __bits = __k._M_to_uint();
	      [[assume(__bits != 0)]]; // because of '__k._M_none_of()' branch above
	      if constexpr (__converts_trivially<_Up, value_type>)
		{
		  _DataType __r = {};
		  __bit_foreach(__bits, [&] [[__gnu__::__always_inline__]] (int __i) {
		    __r[__i] = __mem[__i];
		  });
		  return __r;
		}
	      else
		{
		  using _UV = rebind_t<_Up, basic_vec>;
		  alignas(_UV) _Up __tmp[sizeof(_UV) / sizeof(_Up)] = {};
		  __bit_foreach(__bits, [&] [[__gnu__::__always_inline__]] (int __i) {
		    __tmp[__i] = __mem[__i];
		  });
		  return basic_vec(__builtin_bit_cast(_UV, __tmp));
		}
	    }
	}

      template <typename _Up>
	[[__gnu__::__always_inline__]]
	inline void
	_M_store(_Up* __mem) const
	{
	  if constexpr (__converts_trivially<value_type, _Up>)
	    __builtin_memcpy(__mem, &_M_data, sizeof(_Up) * _S_size);
	  else
	    rebind_t<_Up, basic_vec>(*this)._M_store(__mem);
	}

      /** @internal
       * Implementation of @ref partial_store.
       *
       * @note This is a static function to allow passing @p __v via register in case the function
       * is not inlined.
       *
       * @note The function is not marked @c __always_inline__ since code-gen can become fairly
       * long.
       */
      template <typename _Up, _ArchTraits _Traits = {}>
	static inline void
	_S_partial_store(const basic_vec __v, _Up* __mem, size_t __n)
	{
	  if (__is_const_known_equal_to(__n >= _S_size, true))
	    __v._M_store(__mem);
#if _GLIBCXX_X86
	  else if constexpr (_Traits._M_have_avx512f() && !_S_is_scalar)
	    {
	      const auto __k = __n < _S_size ? mask_type::_S_partial_mask_of_n(int(__n))
					     : mask_type(true);
	      return _S_masked_store(__v, __mem, __k);
	    }
#endif
	  else if (__n >= _S_size) [[unlikely]]
	    __v._M_store(__mem);
	  else if (__n == 0) [[unlikely]]
	    return;
	  else if constexpr (__converts_trivially<value_type, _Up>)
	    {
	      byte* __dst = reinterpret_cast<byte*>(__mem);
	      const byte* __src = reinterpret_cast<const byte*>(&__v._M_data);
	      __memcpy_chunks<sizeof(_Up), sizeof(_M_data)>(__dst, __src, __n);
	    }
	  else
	    {
	      using _UV = rebind_t<_Up, basic_vec>;
	      _UV::_S_partial_store(_UV(__v), __mem, __n);
	    }
	}

      /** @internal
       * Stores elements of @p __v to @p __mem according to mask @p __k.
       *
       * @param __v   Values to store to @p __mem.
       * @param __mem Pointer (in)to array.
       * @param __k   Mask controlling which elements to store. For each bit i in the mask:
       *              - If bit i is 1: store __v[i] to __mem[i]
       *              - If bit i is 0: __mem[i] is left unchanged
       *
       * @note This function assumes it's called after determining that no other method
       *       (like full store) is more appropriate. Calling with all mask bits set to 1
       *       is suboptimal for performance but still correct.
       */
      template <typename _Up, _ArchTraits _Traits = {}>
	//[[__gnu__::__always_inline__]]
	static inline void
	_S_masked_store(const basic_vec __v, _Up* __mem, const mask_type __k)
	{
#if _GLIBCXX_X86
	  if constexpr (_Traits._M_have_avx512f())
	    {
	      __x86_masked_store(__v._M_data, __mem, __k._M_data);
	      return;
	    }
	  else if constexpr (_Traits._M_have_avx() && (sizeof(_Up) == 4 || sizeof(_Up) == 8))
	    {
	      if constexpr (__converts_trivially<value_type, _Up>)
		__x86_masked_store(__v._M_data, __mem, __k._M_data);
	      else
		{
		  using _UV = rebind_t<_Up, basic_vec>;
		  _UV::_S_masked_store(_UV(__v), __mem, typename _UV::mask_type(__k));
		}
	      return;
	    }
#endif
	  if (__k._M_none_of()) [[unlikely]]
	    return;
	  else if constexpr (_S_is_scalar)
	    __mem[0] = __v._M_data;
	  else
	    {
	      // Use at least 4-byte __bits in __bit_foreach for better code-gen
	      _Bitmask<_S_size < 32 ? 32 : _S_size> __bits = __k._M_to_uint();
	      [[assume(__bits != 0)]]; // because of '__k._M_none_of()' branch above
	      if constexpr (__converts_trivially<value_type, _Up>)
		{
		  __bit_foreach(__bits, [&] [[__gnu__::__always_inline__]] (int __i) {
		    __mem[__i] = __v[__i];
		  });
		}
	      else
		{
		  const rebind_t<_Up, basic_vec> __cvted(__v);
		  __bit_foreach(__bits, [&] [[__gnu__::__always_inline__]] (int __i) {
		    __mem[__i] = __cvted[__i];
		  });
		}
	    }
	}

      // [simd.overview] default constructor ----------------------------------
      basic_vec() = default;

      // [simd.overview] p2 impl-def conversions ------------------------------
      using _NativeVecType = decltype([] {
	    if constexpr (_S_is_scalar)
	      return __vec_builtin_type<__canon_value_type, 1>();
	    else
	      return _DataType();
	  }());
      /**
       * @brief Converting constructor from GCC vector builtins.
       *
       * This constructor enables direct construction from GCC vector builtins
       * (`[[gnu::vector_size(N)]]`).
       *
       * @param __x GCC vector builtin to convert from.
       *
       * @note This constructor is not available when size() equals 1.
       *
       * @see operator _NativeVecType() for the reverse conversion.
       */
      constexpr
      basic_vec(_NativeVecType __x)
      : _M_data([&] [[__gnu__::__always_inline__]] {
	  if constexpr (_S_is_scalar)
	    return __x[0];
	  else
	    return __x;
	}())
      {}

      /**
       * @brief Conversion operator to GCC vector builtins.
       *
       * This operator enables implicit conversion from basic_vec to GCC vector builtins.
       *
       * @note This operator is not available when size() equals 1.
       *
       * @see basic_vec(_NativeVecType) for the reverse conversion.
       */
      constexpr
      operator _NativeVecType() const
      {
	if constexpr (_S_is_scalar)
	  return _NativeVecType{_M_data};
	else
	  return _M_data;
      }

#if _GLIBCXX_X86
      /**
       * @brief Converting constructor from Intel Intrinsics (__m128, __m128i, ...).
       */
      template <__vec_builtin _IV>
	requires same_as<__x86_intel_intrin_value_type<value_type>, __vec_value_type<_IV>>
	  && (sizeof(_IV) == sizeof(_DataType) && sizeof(_IV) >= 16
		 && !is_same_v<_IV, _DataType>)
	constexpr
	basic_vec(_IV __x)
	: _M_data(reinterpret_cast<_DataType>(__x))
	{}

      /**
       * @brief Conversion operator to Intel Intrinsics (__m128, __m128i, ...).
       */
      template <__vec_builtin _IV>
	requires same_as<__x86_intel_intrin_value_type<value_type>, __vec_value_type<_IV>>
	  && (sizeof(_IV) == sizeof(_DataType) && sizeof(_IV) >= 16
		 && !is_same_v<_IV, _DataType>)
	constexpr
	operator _IV() const
	{ return reinterpret_cast<_IV>(_M_data); }
#endif

      // [simd.ctor] broadcast constructor ------------------------------------
      /**
       * @brief Broadcast constructor from scalar value.
       *
       * Constructs a vector where all elements are initialized to the same scalar value.
       * The scalar value is converted to the vector's element type.
       *
       * @param __x Scalar value to broadcast to all vector elements.
       * @tparam _Up Type of scalar value (must be explicitly convertible to value_type).
       *
       * @note The constructor is implicit if the conversion (if any) is value-preserving.
       */
      template <__broadcast_constructible<value_type> _Up>
	[[__gnu__::__always_inline__]]
	constexpr
	basic_vec(_Up&& __x) noexcept
	  : _M_data(_DataType() == _DataType() ? static_cast<value_type>(__x) : value_type())
	{}

      // [simd.ctor] conversion constructor -----------------------------------
      template <typename _Up, typename _UAbi, _TargetTraits _Traits = {}>
	requires (_S_size == _UAbi::_S_size)
	  && __explicitly_convertible_to<_Up, value_type>
	[[__gnu__::__always_inline__]]
	constexpr
	explicit(!__value_preserving_convertible_to<_Up, value_type>
		   || __higher_rank_than<_Up, value_type>)
	basic_vec(const basic_vec<_Up, _UAbi>& __x) noexcept
	: _M_data([&] [[__gnu__::__always_inline__]] {
	    if constexpr (_S_is_scalar)
	      return static_cast<value_type>(__x[0]);
	    else if constexpr (_UAbi::_S_nreg >= 2)
	      // __builtin_convertvector (__vec_cast) is inefficient for over-sized inputs.
	      // Also e.g. vec<float, 12> -> vec<char, 12> (with SSE2) would otherwise emit 4
	      // vcvttps2dq instructions, where only 3 are needed
	      return _S_concat(resize_t<__x._N0, basic_vec>(__x._M_data0),
			       resize_t<__x._N1, basic_vec>(__x._M_data1))._M_data;
	    else
	      return __vec_cast<_DataType>(__x._M_concat_data());
	  }())
	{}

      using _VecBase<_Tp, _Ap>::_VecBase;

      // [simd.ctor] generator constructor ------------------------------------
      template <__simd_generator_invokable<value_type, _S_size> _Fp>
	[[__gnu__::__always_inline__]]
	constexpr explicit
	basic_vec(_Fp&& __gen)
	: _M_data([&] [[__gnu__::__always_inline__]] {
	    constexpr auto [...__is] = _IotaArray<_S_size>;
	    return _DataType{static_cast<value_type>(__gen(__simd_size_c<__is>))...};
	  }())
	{}

      // [simd.ctor] load constructor -----------------------------------------
      template <typename _Up>
	[[__gnu__::__always_inline__]]
	constexpr
	basic_vec(_LoadCtorTag, const _Up* __ptr)
	  : _M_data()
	{
	  if constexpr (_S_is_scalar)
	    _M_data = static_cast<value_type>(__ptr[0]);
	  else if consteval
	    {
	      constexpr auto [...__is] = _IotaArray<_S_size>;
	      _M_data = _DataType{static_cast<value_type>(__ptr[__is])...};
	    }
	  else
	    {
	      if constexpr (__converts_trivially<_Up, value_type>)
		// This assumes std::floatN_t to be bitwise equal to float/double
		__builtin_memcpy(&_M_data, __ptr, sizeof(value_type) * _S_size);
	      else
		{
		  __vec_builtin_type<_Up, _S_full_size> __tmp = {};
		  __builtin_memcpy(&__tmp, __ptr, sizeof(_Up) * _S_size);
		  _M_data = __vec_cast<_DataType>(__tmp);
		}
	    }
	}

      template <ranges::contiguous_range _Rg, typename... _Flags>
	requires ranges::__static_sized_range<_Rg>
	  && __vectorizable<ranges::range_value_t<_Rg>>
	  && __explicitly_convertible_to<ranges::range_value_t<_Rg>, value_type>
	[[__gnu__::__always_inline__]]
	constexpr
	basic_vec(_Rg&& __range, flags<_Flags...> __flags = {})
	requires (ranges::size(__range) == _S_size)
	: basic_vec(_LoadCtorTag(), __flags.template _S_adjust_pointer<basic_vec>(
				      ranges::data(__range)))
	{
	  static_assert(__loadstore_convertible_to<ranges::range_value_t<_Rg>, value_type,
						   _Flags...>);
	}

      // [simd.subscr] --------------------------------------------------------
      /**
       * @brief Return the value of the element at index @p __i.
       *
       * @pre __i >= 0 && __i < size().
       */
      [[__gnu__::__always_inline__]]
      constexpr value_type
      operator[](__simd_size_type __i) const
      {
	__glibcxx_simd_precondition(__i >= 0 && __i < _S_size, "subscript is out of bounds");
	if constexpr (_S_is_scalar)
	  return _M_data;
	else
	  return _M_data[__i];
      }

      // [simd.unary] unary operators -----------------------------------------
      // increment and decrement are implemented in terms of operator+=/-= which avoids UB on
      // padding elements while not breaking UBsan
      [[__gnu__::__always_inline__]]
      constexpr basic_vec&
      operator++() noexcept requires requires(value_type __a) { ++__a; }
      { return *this += value_type(1); }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator++(int) noexcept requires requires(value_type __a) { __a++; }
      {
	basic_vec __r = *this;
	*this += value_type(1);
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec&
      operator--() noexcept requires requires(value_type __a) { --__a; }
      { return *this -= value_type(1); }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator--(int) noexcept requires requires(value_type __a) { __a--; }
      {
	basic_vec __r = *this;
	*this -= value_type(1);
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr mask_type
      operator!() const noexcept requires requires(value_type __a) { !__a; }
      { return *this == value_type(); }

      /**
       * @brief Unary plus operator (no-op).
       *
       * Returns an unchanged copy of the object.
       */
      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator+() const noexcept requires requires(value_type __a) { +__a; }
      { return *this; }

      /**
       * @brief Unary negation operator.
       *
       * Returns a new SIMD vector after element-wise negation.
       */
      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator-() const noexcept requires requires(value_type __a) { -__a; }
      { return _S_init(-_M_data); }

      /**
       * @brief Bitwise NOT / complement operator.
       *
       * Returns a new SIMD vector after element-wise complement.
       */
      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator~() const noexcept requires requires(value_type __a) { ~__a; }
      { return _S_init(~_M_data); }

      // [simd.cassign] binary operators
      /**
       * @brief Bitwise AND operator.
       *
       * Returns a new SIMD vector after element-wise AND.
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator&=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a & __a; }
      {
	__x._M_data &= __y._M_data;
	return __x;
      }

      /**
       * @brief Bitwise OR operator.
       *
       * Returns a new SIMD vector after element-wise OR.
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator|=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a | __a; }
      {
	__x._M_data |= __y._M_data;
	return __x;
      }

      /**
       * @brief Bitwise XOR operator.
       *
       * Returns a new SIMD vector after element-wise XOR.
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator^=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a ^ __a; }
      {
	__x._M_data ^= __y._M_data;
	return __x;
      }

      /**
       * @brief Applies the compound assignment operator element-wise.
       *
       * @pre If @c value_type is a signed integral type, the result is representable by @c
       * value_type. (This does not apply to padding elements the implementation might add for
       * non-power-of-2 widths.) UBsan will only see a call to @c unreachable() on overflow.
       *
       * @note The overflow detection code is discarded unless UBsan is active.
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator+=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a + __a; }
      {
	if constexpr (_S_is_partial && is_integral_v<value_type> && is_signed_v<value_type>)
	  { // avoid spurious UB on signed integer overflow of the padding element(s). But don't
	    // remove UB of the active elements (so that UBsan can still do its job).
	    //
	    // This check is essentially free (at runtime) because DCE removes everything except
	    // the final change to _M_data. The overflow check is only emitted if UBsan is active.
	    //
	    // The alternative would be to always zero padding elements after operations that can
	    // produce non-zero values. However, right now:
	    // - auto f(simd::mask<int, 3> k) { return +k; } is a single VPABSD and would have to
	    //   sanitize
	    // - bit_cast to basic_vec with non-zero padding elements is fine
	    // - conversion from intrinsics can create non-zero padding elements
	    // - shuffles are allowed to put whatever they want into padding elements for
	    //   optimization purposes (e.g. for better instruction selection)
	    using _UV = typename _Ap::template _DataType<make_unsigned_t<value_type>>;
	    const _DataType __result
	      = reinterpret_cast<_DataType>(reinterpret_cast<_UV>(__x._M_data)
					      + reinterpret_cast<_UV>(__y._M_data));
	    const auto __positive = __y > value_type();
	    const auto __overflow = __positive != (__result > __x);
	    if (__overflow._M_any_of())
	      __builtin_unreachable(); // trigger UBsan
	    __x._M_data = __result;
	  }
	else if constexpr (_TargetTraits()._M_eval_as_f32<value_type>())
	  __x = basic_vec(rebind_t<float, basic_vec>(__x) + __y);
	else
	  __x._M_data += __y._M_data;
	return __x;
      }

      /** @copydoc operator+=
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator-=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a - __a; }
      {
	if constexpr (_S_is_partial && is_integral_v<value_type> && is_signed_v<value_type>)
	  { // see comment on operator+=
	    using _UV = typename _Ap::template _DataType<make_unsigned_t<value_type>>;
	    const _DataType __result
	      = reinterpret_cast<_DataType>(reinterpret_cast<_UV>(__x._M_data)
					      - reinterpret_cast<_UV>(__y._M_data));
	    const auto __positive = __y > value_type();
	    const auto __overflow = __positive != (__result < __x);
	    if (__overflow._M_any_of())
	      __builtin_unreachable(); // trigger UBsan
	    __x._M_data = __result;
	  }
	else if constexpr (_TargetTraits()._M_eval_as_f32<value_type>())
	  __x = basic_vec(rebind_t<float, basic_vec>(__x) - __y);
	else
	  __x._M_data -= __y._M_data;
	return __x;
      }

      /** @copydoc operator+=
       */
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator*=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a * __a; }
      {
	if constexpr (_S_is_partial && is_integral_v<value_type> && is_signed_v<value_type>)
	  { // see comment on operator+=
	    for (int __i = 0; __i < _S_size; ++__i)
	      {
		if (__builtin_mul_overflow_p(__x._M_data[__i], __y._M_data[__i], value_type()))
		  __builtin_unreachable();
	      }
	    using _UV = typename _Ap::template _DataType<make_unsigned_t<value_type>>;
	    __x._M_data = reinterpret_cast<_DataType>(reinterpret_cast<_UV>(__x._M_data)
							* reinterpret_cast<_UV>(__y._M_data));
	  }

	// 'uint16 * uint16' promotes to int and can therefore lead to UB. The standard does not
	// require to avoid the undefined behavior. It's unnecessary and easy to avoid. It's also
	// unexpected because there's no UB on the vector types (which don't promote).
	else if constexpr (_S_is_scalar && is_unsigned_v<value_type>
			     && is_signed_v<decltype(value_type() * value_type())>)
	  __x._M_data = unsigned(__x._M_data) * unsigned(__y._M_data);

	else if constexpr (_TargetTraits()._M_eval_as_f32<value_type>())
	  __x = basic_vec(rebind_t<float, basic_vec>(__x) * __y);

	else
	  __x._M_data *= __y._M_data;
	return __x;
      }

      template <_TargetTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	friend constexpr basic_vec&
	operator/=(basic_vec& __x, const basic_vec& __y) noexcept
	requires requires(value_type __a) { __a / __a; }
	{
	  const basic_vec __result([&](int __i) -> value_type { return __x[__i] / __y[__i]; });
	  if (__is_const_known(__result))
	    // the optimizer already knows the values of the result
	    return __x = __result;

#ifdef __SSE2__
	  // x86 doesn't have integral SIMD division instructions
	  // While division is faster, the required conversions are still a problem:
	  // see PR121274, PR121284, and PR121296 for missed optimizations wrt. conversions
	  //
	  // With only 1 or 2 divisions, the conversion to and from fp is too expensive.
	  if constexpr (is_integral_v<value_type> && _S_size > 2
			  && __value_preserving_convertible_to<value_type, double>)
	    {
	      // If the denominator (y) is known to the optimizer, don't convert to fp because the
	      // integral division can be translated into shifts/multiplications.
	      if (!__is_const_known(__y))
		{
		  // With AVX512FP16 use vdivph for 8-bit integers
		  if constexpr (_Traits._M_have_avx512fp16()
				  && __value_preserving_convertible_to<value_type, _Float16>)
		    return __x = basic_vec(rebind_t<_Float16, basic_vec>(__x) / __y);
		  else if constexpr (__value_preserving_convertible_to<value_type, float>)
		    return __x = basic_vec(rebind_t<float, basic_vec>(__x) / __y);
		  else
		    return __x = basic_vec(rebind_t<double, basic_vec>(__x) / __y);
		}
	    }
#endif
	  if constexpr (_Traits._M_eval_as_f32<value_type>())
	    return __x = basic_vec(rebind_t<float, basic_vec>(__x) / __y);

	  basic_vec __y1 = __y;
	  if constexpr (_S_is_partial)
	    {
	      if constexpr (is_integral_v<value_type>)
		{
		  // Assume integral division doesn't have SIMD instructions and must be done per
		  // element anyway. Partial vectors should skip their padding elements.
		  for (int __i = 0; __i < _S_size; ++__i)
		    __x._M_data[__i] /= __y._M_data[__i];
		  return __x;
		}
	      else
		__y1 = __select_impl(mask_type::_S_init(mask_type::_S_implicit_mask),
				     __y, basic_vec(value_type(1)));
	    }
	  __x._M_data /= __y1._M_data;
	  return __x;
	}

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator%=(basic_vec& __x, const basic_vec& __y) noexcept
      requires requires(value_type __a) { __a % __a; }
      {
	static_assert(is_integral_v<value_type>);
	if constexpr (_S_is_partial)
	  {
	    const basic_vec __y1 = __select_impl(mask_type::_S_init(mask_type::_S_implicit_mask),
						 __y, basic_vec(value_type(1)));
	    if (__is_const_known(__y1))
	      __x._M_data %= __y1._M_data;
	    else
	      {
		// Assume integral division doesn't have SIMD instructions and must be done per
		// element anyway. Partial vectors should skip their padding elements.
		for (int __i = 0; __i < _S_size; ++__i)
		  __x._M_data[__i] %= __y._M_data[__i];
	      }
	  }
	else
	  __x._M_data %= __y._M_data;
	return __x;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator<<=(basic_vec& __x, const basic_vec& __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a) { __a << __a; }
      {
	__glibcxx_simd_precondition(is_unsigned_v<value_type> || all_of(__y >= value_type()),
				    "negative shift is undefined behavior");
	__glibcxx_simd_precondition(all_of(__y < __max_shift<value_type>),
				    "too large shift invokes undefined behavior");
	__x._M_data <<= __y._M_data;
	return __x;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator>>=(basic_vec& __x, const basic_vec& __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a) { __a >> __a; }
      {
	__glibcxx_simd_precondition(is_unsigned_v<value_type> || all_of(__y >= value_type()),
				    "negative shift is undefined behavior");
	__glibcxx_simd_precondition(all_of(__y < __max_shift<value_type>),
				    "too large shift invokes undefined behavior");
	__x._M_data >>= __y._M_data;
	return __x;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator<<=(basic_vec& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a, __simd_size_type __b) { __a << __b; }
      {
	__glibcxx_simd_precondition(__y >= 0, "negative shift is undefined behavior");
	__glibcxx_simd_precondition(__y < int(__max_shift<value_type>),
				    "too large shift invokes undefined behavior");
	__x._M_data <<= __y;
	return __x;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator>>=(basic_vec& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a, __simd_size_type __b) { __a >> __b; }
      {
	__glibcxx_simd_precondition(__y >= 0, "negative shift is undefined behavior");
	__glibcxx_simd_precondition(__y < int(__max_shift<value_type>),
				    "too large shift invokes undefined behavior");
	__x._M_data >>= __y;
	return __x;
      }

      // [simd.comparison] ----------------------------------------------------
#if _GLIBCXX_X86
      template <_X86Cmp _Cmp>
	[[__gnu__::__always_inline__]]
	constexpr mask_type
	_M_bitmask_cmp(_DataType __y) const
	{
	  static_assert(_S_use_bitmask);
	  if (__is_const_known(_M_data, __y))
	    {
	      constexpr auto [...__is] = _IotaArray<_S_size>;
	      constexpr auto __cmp_op = [] [[__gnu__::__always_inline__]]
					  (value_type __a, value_type __b) {
		if constexpr (_Cmp == _X86Cmp::_Eq)
		  return __a == __b;
		else if constexpr (_Cmp == _X86Cmp::_Lt)
		  return __a < __b;
		else if constexpr (_Cmp == _X86Cmp::_Le)
		  return __a <= __b;
		else if constexpr (_Cmp == _X86Cmp::_Unord)
		  return std::isunordered(__a, __b);
		else if constexpr (_Cmp == _X86Cmp::_Neq)
		  return __a != __b;
		else if constexpr (_Cmp == _X86Cmp::_Nlt)
		  return !(__a < __b);
		else if constexpr (_Cmp == _X86Cmp::_Nle)
		  return !(__a <= __b);
		else
		  static_assert(false);
	      };
	      const _Bitmask<_S_size> __bits
		= ((__cmp_op(__vec_get(_M_data, __is), __vec_get(__y, __is))
		      ? (1ULL << __is) : 0) | ...);
	      return mask_type::_S_init(__bits);
	    }
	  else
	    return mask_type::_S_init(__x86_bitmask_cmp<_Cmp>(_M_data, __y));
	}
#endif

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator==(const basic_vec& __x, const basic_vec& __y) noexcept
      {
#if _GLIBCXX_X86
	if constexpr (_S_use_bitmask)
	  return __x._M_bitmask_cmp<_X86Cmp::_Eq>(__y._M_data);
	else
#endif
	  return mask_type::_S_init(__x._M_data == __y._M_data);
      }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator!=(const basic_vec& __x, const basic_vec& __y) noexcept
      {
#if _GLIBCXX_X86
	if constexpr (_S_use_bitmask)
	  return __x._M_bitmask_cmp<_X86Cmp::_Neq>(__y._M_data);
	else
#endif
	  return mask_type::_S_init(__x._M_data != __y._M_data);
      }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator<(const basic_vec& __x, const basic_vec& __y) noexcept
      {
#if _GLIBCXX_X86
	if constexpr (_S_use_bitmask)
	  return __x._M_bitmask_cmp<_X86Cmp::_Lt>(__y._M_data);
	else
#endif
	  return mask_type::_S_init(__x._M_data < __y._M_data);
      }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator<=(const basic_vec& __x, const basic_vec& __y) noexcept
      {
#if _GLIBCXX_X86
	if constexpr (_S_use_bitmask)
	  return __x._M_bitmask_cmp<_X86Cmp::_Le>(__y._M_data);
	else
#endif
	  return mask_type::_S_init(__x._M_data <= __y._M_data);
      }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator>(const basic_vec& __x, const basic_vec& __y) noexcept
      { return __y < __x; }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator>=(const basic_vec& __x, const basic_vec& __y) noexcept
      { return __y <= __x; }

      // [simd.cond] ---------------------------------------------------------
      template <_TargetTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	friend constexpr basic_vec
	__select_impl(const mask_type& __k, const basic_vec& __t, const basic_vec& __f) noexcept
	{
	  if constexpr (_S_size == 1)
	    return __k[0] ? __t : __f;
	  else if constexpr (_S_use_bitmask)
	    {
#if _GLIBCXX_X86
	      if (__is_const_known(__k, __t, __f))
		return basic_vec([&](int __i) { return __k[__i] ? __t[__i] : __f[__i]; });
	      else
		return __x86_bitmask_blend(__k._M_data, __t._M_data, __f._M_data);
#else
	      static_assert(false, "TODO");
#endif
	    }
	  else if consteval
	    {
	      return __k._M_data ? __t._M_data : __f._M_data;
	    }
	  else
	    {
	      constexpr bool __uses_simd_register = sizeof(_M_data) >= 8;
	      using _VO = _VecOps<_DataType>;
	      if (_VO::_S_is_const_known_equal_to(__f._M_data, 0))
		{
		  if (is_integral_v<value_type> && __uses_simd_register
			&& _VO::_S_is_const_known_equal_to(__t._M_data, 1))
		    // This is equivalent to converting the mask into a vec of 0s and 1s. So +__k.
		    // However, basic_mask::operator+ arrives here; returning +__k would be
		    // recursive. Instead we use -__k (which is a no-op for vector-masks) and then
		    // flip all -1 elements to +1 by taking the absolute value.
		    return basic_vec((-__k)._M_abs());
		  else
		    return __vec_and(reinterpret_cast<_DataType>(__k._M_data), __t._M_data);
		}
	      else if (_VecOps<_DataType>::_S_is_const_known_equal_to(__t._M_data, 0))
		{
		  if (is_integral_v<value_type> && __uses_simd_register
			&& _VO::_S_is_const_known_equal_to(__f._M_data, 1))
		    return value_type(1) + basic_vec(-__k);
		  else
		    return __vec_and(reinterpret_cast<_DataType>(__vec_not(__k._M_data)), __f._M_data);
		}
	      else
		{
#if _GLIBCXX_X86
		  // this works around bad code-gen when the compiler can't see that __k is a vector-mask.
		  // This pattern, is recognized to match the x86 blend instructions, which only consider
		  // the sign bit of the mask register. Also, without SSE4, if the compiler knows that __k
		  // is a vector-mask, then the '< 0' is elided.
		  return __k._M_data < 0 ? __t._M_data : __f._M_data;
#endif
		  return __k._M_data ? __t._M_data : __f._M_data;
		}
	    }
	}
    };

  template <__vectorizable _Tp, __abi_tag _Ap>
    requires (_Ap::_S_nreg > 1)
      && (!__complex_like<_Tp>)
    class basic_vec<_Tp, _Ap>
    : public _VecBase<_Tp, _Ap>
    {
      template <typename, typename>
	friend class basic_vec;

      template <size_t, typename>
	friend class basic_mask;

      static constexpr int _S_size = _Ap::_S_size;

      static constexpr int _N0 = __bit_ceil(unsigned(_S_size)) / 2;

      static constexpr int _N1 = _S_size - _N0;

      using _DataType0 = __similar_vec<_Tp, _N0, _Ap>;

      // the implementation (and users) depend on elements being contiguous in memory
      static_assert(_N0 * sizeof(_Tp) == sizeof(_DataType0));

      using _DataType1 = __similar_vec<_Tp, _N1, _Ap>;

      static_assert(_DataType0::abi_type::_S_nreg + _DataType1::abi_type::_S_nreg == _Ap::_S_nreg);

      static constexpr bool _S_is_scalar = _DataType0::_S_is_scalar;

      _DataType0 _M_data0;

      _DataType1 _M_data1;

      static constexpr bool _S_use_bitmask = _DataType0::_S_use_bitmask;

      static constexpr bool _S_is_partial = _DataType1::_S_is_partial;

    public:
      using value_type = _Tp;

      using mask_type = _VecBase<_Tp, _Ap>::mask_type;

      [[__gnu__::__always_inline__]]
      static constexpr basic_vec
      _S_init(const _DataType0& __x, const _DataType1& __y)
      {
	basic_vec __r;
	__r._M_data0 = __x;
	__r._M_data1 = __y;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr _DataType0&
      _M_get_low() noexcept
      { return _M_data0; }

      [[__gnu__::__always_inline__]]
      constexpr const _DataType0&
      _M_get_low() const noexcept
      { return _M_data0; }

      [[__gnu__::__always_inline__]]
      constexpr _DataType1&
      _M_get_high() noexcept
      { return _M_data1; }

      [[__gnu__::__always_inline__]]
      constexpr const _DataType1&
      _M_get_high() const noexcept
      { return _M_data1; }

      [[__gnu__::__always_inline__]]
      friend constexpr bool
      __is_const_known(const basic_vec& __x)
      { return __is_const_known(__x._M_data0) && __is_const_known(__x._M_data1); }

      [[__gnu__::__always_inline__]]
      constexpr auto
      _M_concat_data([[maybe_unused]] bool __do_sanitize = false) const
      {
	return __vec_concat(_M_data0._M_concat_data(false),
			    __vec_zero_pad_to<sizeof(_M_data0)>(
			      _M_data1._M_concat_data(__do_sanitize)));
      }

      template <int _Size = _S_size, int _Offset = 0, typename _A0, typename _Fp>
	[[__gnu__::__always_inline__]]
	static constexpr basic_vec
	_S_static_permute(const basic_vec<value_type, _A0>& __x, _Fp&& __idxmap)
	{
	  return _S_init(
		   _DataType0::template _S_static_permute<_Size, _Offset>(__x, __idxmap),
		   _DataType1::template _S_static_permute<_Size, _Offset + _N0>(__x, __idxmap));
	}

      template <typename _Vp>
	[[__gnu__::__always_inline__]]
	constexpr auto
	_M_chunk() const noexcept
	{
	  constexpr int __n = _S_size / _Vp::_S_size;
	  constexpr int __rem = _S_size % _Vp::_S_size;
	  constexpr auto [...__is] = _IotaArray<__n>;
	  if constexpr (__rem == 0)
	    return array<_Vp, __n>{__extract_simd_at<_Vp>(cw<_Vp::_S_size * __is>,
							    _M_data0, _M_data1)...};
	  else
	    {
	      using _Rest = resize_t<__rem, _Vp>;
	      return tuple(__extract_simd_at<_Vp>(cw<_Vp::_S_size * __is>, _M_data0, _M_data1)...,
			   __extract_simd_at<_Rest>(cw<_Vp::_S_size * __n>, _M_data0, _M_data1));
	    }
	}

      [[__gnu__::__always_inline__]]
      static constexpr const basic_vec&
      _S_concat(const basic_vec& __x0) noexcept
      { return __x0; }

      template <typename... _As>
	requires (sizeof...(_As) >= 2)
	[[__gnu__::__always_inline__]]
	static constexpr basic_vec
	_S_concat(const basic_vec<value_type, _As>&... __xs) noexcept
	{
	  static_assert(_S_size == (_As::_S_size + ...));
	  return _S_init(__extract_simd_at<_DataType0>(cw<0>, __xs...),
			 __extract_simd_at<_DataType1>(cw<_N0>, __xs...));
	}

      [[__gnu__::__always_inline__]]
      constexpr auto
      _M_reduce_to_half(auto __binary_op) const requires (_N0 == _N1)
      { return __binary_op(_M_data0, _M_data1); }

      [[__gnu__::__always_inline__]]
      constexpr value_type
      _M_reduce_tail(const auto& __rest, auto __binary_op) const
      {
	if constexpr (__rest.size() > _S_size)
	  {
	    auto [__a, __b] = __rest.template _M_chunk<basic_vec>();
	    return __binary_op(*this, __a)._M_reduce_tail(__b, __binary_op);
	  }
	else if constexpr (__rest.size() == _S_size)
	  return __binary_op(*this, __rest)._M_reduce(__binary_op);
	else
	  return _M_reduce_to_half(__binary_op)._M_reduce_tail(__rest, __binary_op);
      }

      template <typename _BinaryOp, _TargetTraits _Traits = {}>
	[[__gnu__::__always_inline__]]
	constexpr value_type
	_M_reduce(_BinaryOp __binary_op) const
	{
	  if constexpr (_Traits.template _M_eval_as_f32<value_type>()
			  && (is_same_v<_BinaryOp, plus<>>
				 || is_same_v<_BinaryOp, multiplies<>>))
	    return value_type(rebind_t<float, basic_vec>(*this)._M_reduce(__binary_op));
#ifdef __SSE2__
	  else if constexpr (is_integral_v<value_type> && sizeof(value_type) == 1
			       && is_same_v<decltype(__binary_op), multiplies<>>)
	    {
	      // convert to unsigned short because of missing 8-bit mul instruction
	      // we don't need to preserve the order of elements
	      //
	      // The left columns under Latency and Throughput show bit-cast to ushort with shift by
	      // 8. The right column uses the alternative in the else branch.
	      // Benchmark on Intel Ultra 7 165U (AVX2)
	      //   TYPE             Latency           Throughput
	      //              [cycles/call]        [cycles/call]
	      //schar, 64        59.9  70.7           10.5  13.3
	      //schar, 128       81.4  97.2           12.2    21
	      //schar, 256       92.4   129           17.2  35.2
	      if constexpr (_DataType1::_S_is_scalar)
		return __binary_op(_DataType1(_M_data0._M_reduce(__binary_op)), _M_data1)[0];
	      // TODO: optimize trailing scalar (e.g. (8+8)+(8+1))
	      else if constexpr (_S_size % 2 == 0)
		{ // If all elements participate in the reduction we can take this shortcut
		  using _V16 = resize_t<_S_size / 2, rebind_t<unsigned short, basic_vec>>;
		  auto __a = __builtin_bit_cast(_V16, *this);
		  return __binary_op(__a, __a >> __CHAR_BIT__)._M_reduce(__binary_op);
		}
	      else
		{
		  using _V16 = rebind_t<unsigned short, basic_vec>;
		  return _V16(*this)._M_reduce(__binary_op);
		}
	    }
#endif
	  else
	    return _M_data0._M_reduce_tail(_M_data1, __binary_op);
	}

      [[__gnu__::__always_inline__]]
      constexpr mask_type
      _M_isnan() const requires is_floating_point_v<value_type>
      { return mask_type::_S_init(_M_data0._M_isnan(), _M_data1._M_isnan()); }

      [[__gnu__::__always_inline__]]
      constexpr mask_type
      _M_isinf() const requires is_floating_point_v<value_type>
      { return mask_type::_S_init(_M_data0._M_isinf(), _M_data1._M_isinf()); }

      [[__gnu__::__always_inline__]]
      constexpr mask_type
      _M_isunordered(basic_vec __y) const requires is_floating_point_v<value_type>
      {
	return mask_type::_S_init(_M_data0._M_isunordered(__y._M_data0),
				  _M_data1._M_isunordered(__y._M_data1));
      }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      _M_abs() const requires signed_integral<value_type>
      { return _S_init(_M_data0._M_abs(), _M_data1._M_abs()); }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      _M_fabs() const requires floating_point<value_type>
      { return _S_init(_M_data0._M_fabs(), _M_data1._M_fabs()); }

      template <typename _Up>
	[[__gnu__::__always_inline__]]
	static inline basic_vec
	_S_partial_load(const _Up* __mem, size_t __n)
	{
	  if (__n >= _N0)
	    return _S_init(_DataType0(_LoadCtorTag(), __mem),
			   _DataType1::_S_partial_load(__mem + _N0, __n - _N0));
	  else
	    return _S_init(_DataType0::_S_partial_load(__mem, __n),
			   _DataType1());
	}

      template <typename _Up, _ArchTraits _Traits = {}>
	static inline basic_vec
	_S_masked_load(const _Up* __mem, mask_type __k)
	{
	  return _S_init(_DataType0::_S_masked_load(__mem, __k._M_data0),
			 _DataType1::_S_masked_load(__mem + _N0, __k._M_data1));
	}

      template <typename _Up>
	[[__gnu__::__always_inline__]]
	inline void
	_M_store(_Up* __mem) const
	{
	  _M_data0._M_store(__mem);
	  _M_data1._M_store(__mem + _N0);
	}

      template <typename _Up>
	[[__gnu__::__always_inline__]]
	static inline void
	_S_partial_store(const basic_vec& __v, _Up* __mem, size_t __n)
	{
	  if (__n >= _N0)
	    {
	      __v._M_data0._M_store(__mem);
	      _DataType1::_S_partial_store(__v._M_data1, __mem + _N0, __n - _N0);
	    }
	  else
	    {
	      _DataType0::_S_partial_store(__v._M_data0, __mem, __n);
	    }
	}

      template <typename _Up>
	[[__gnu__::__always_inline__]]
	static inline void
	_S_masked_store(const basic_vec& __v, _Up* __mem, const mask_type& __k)
	{
	  _DataType0::_S_masked_store(__v._M_data0, __mem, __k._M_data0);
	  _DataType1::_S_masked_store(__v._M_data1, __mem + _N0, __k._M_data1);
	}

      basic_vec() = default;

      // [simd.overview] p2 impl-def conversions ------------------------------
      using _NativeVecType = __vec_builtin_type<value_type, __bit_ceil(unsigned(_S_size))>;

      [[__gnu__::__always_inline__]]
      constexpr
      basic_vec(const _NativeVecType& __x)
      : _M_data0(_VecOps<__vec_builtin_type<value_type, _N0>>::_S_extract(__x)),
	_M_data1(_VecOps<__vec_builtin_type<value_type, __bit_ceil(unsigned(_N1))>>
		   ::_S_extract(__x, integral_constant<int, _N0>()))
      {}

      [[__gnu__::__always_inline__]]
      constexpr
      operator _NativeVecType() const
      { return _M_concat_data(); }

      // [simd.ctor] broadcast constructor ------------------------------------
      template <__broadcast_constructible<value_type> _Up>
	[[__gnu__::__always_inline__]]
	constexpr
	basic_vec(_Up&& __x) noexcept
	  : _M_data0(static_cast<value_type>(__x)), _M_data1(static_cast<value_type>(__x))
	{}

      // [simd.ctor] conversion constructor -----------------------------------
      template <typename _Up, typename _UAbi>
	requires (_S_size == _UAbi::_S_size)
	  && __explicitly_convertible_to<_Up, value_type>
	[[__gnu__::__always_inline__]]
	constexpr
	explicit(!__value_preserving_convertible_to<_Up, value_type>
		   || __higher_rank_than<_Up, value_type>)
	basic_vec(const basic_vec<_Up, _UAbi>& __x) noexcept
	  : _M_data0(get<0>(chunk<_N0>(__x))),
	    _M_data1(get<1>(chunk<_N0>(__x)))
	{}

      using _VecBase<_Tp, _Ap>::_VecBase;

      // [simd.ctor] generator constructor ------------------------------------
      template <__simd_generator_invokable<value_type, _S_size> _Fp>
	[[__gnu__::__always_inline__]]
	constexpr explicit
	basic_vec(_Fp&& __gen)
	  : _M_data0(__gen), _M_data1([&] [[__gnu__::__always_inline__]] (auto __i) {
			       return __gen(__simd_size_c<__i + _N0>);
			     })
	{}

      // [simd.ctor] load constructor -----------------------------------------
      template <typename _Up>
	[[__gnu__::__always_inline__]]
	constexpr
	basic_vec(_LoadCtorTag, const _Up* __ptr)
	  : _M_data0(_LoadCtorTag(), __ptr),
	    _M_data1(_LoadCtorTag(), __ptr + _N0)
	{}

      template <ranges::contiguous_range _Rg, typename... _Flags>
	requires ranges::__static_sized_range<_Rg>
	  && __vectorizable<ranges::range_value_t<_Rg>>
	  && __explicitly_convertible_to<ranges::range_value_t<_Rg>, value_type>
	constexpr
	basic_vec(_Rg&& __range, flags<_Flags...> __flags = {})
	requires (ranges::size(__range) == _S_size)
	: basic_vec(_LoadCtorTag(),
		    __flags.template _S_adjust_pointer<basic_vec>(ranges::data(__range)))
	{
	  static_assert(__loadstore_convertible_to<ranges::range_value_t<_Rg>, value_type,
						   _Flags...>);
	}

      // [simd.subscr] --------------------------------------------------------
      [[__gnu__::__always_inline__]]
      constexpr value_type
      operator[](__simd_size_type __i) const
      {
	__glibcxx_simd_precondition(__i >= 0 && __i < _S_size, "subscript is out of bounds");
	if (__is_const_known(__i))
	  return __i < _N0 ? _M_data0[__i] : _M_data1[__i - _N0];
	else
	  {
	    using _AliasingT [[__gnu__::__may_alias__]] = value_type;
	    return reinterpret_cast<const _AliasingT*>(this)[__i];
	  }
      }

      // [simd.unary] unary operators -----------------------------------------
      [[__gnu__::__always_inline__]]
      constexpr basic_vec&
      operator++() noexcept requires requires(value_type __a) { ++__a; }
      {
	++_M_data0;
	++_M_data1;
	return *this;
      }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator++(int) noexcept requires requires(value_type __a) { __a++; }
      {
	basic_vec __r = *this;
	++_M_data0;
	++_M_data1;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec&
      operator--() noexcept requires requires(value_type __a) { --__a; }
      {
	--_M_data0;
	--_M_data1;
	return *this;
      }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator--(int) noexcept requires requires(value_type __a) { __a--; }
      {
	basic_vec __r = *this;
	--_M_data0;
	--_M_data1;
	return __r;
      }

      [[__gnu__::__always_inline__]]
      constexpr mask_type
      operator!() const noexcept requires requires(value_type __a) { !__a; }
      { return mask_type::_S_init(!_M_data0, !_M_data1); }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator+() const noexcept requires requires(value_type __a) { +__a; }
      { return *this; }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator-() const noexcept requires requires(value_type __a) { -__a; }
      { return _S_init(-_M_data0, -_M_data1); }

      [[__gnu__::__always_inline__]]
      constexpr basic_vec
      operator~() const noexcept requires requires(value_type __a) { ~__a; }
      { return _S_init(~_M_data0, ~_M_data1); }

      // [simd.cassign] -------------------------------------------------------
#define _GLIBCXX_SIMD_DEFINE_OP(sym)                                 \
      [[__gnu__::__always_inline__]]                                 \
      friend constexpr basic_vec&                                    \
      operator sym##=(basic_vec& __x, const basic_vec& __y) _GLIBCXX_SIMD_NOEXCEPT \
      {                                                              \
	__x._M_data0 sym##= __y._M_data0;                            \
	__x._M_data1 sym##= __y._M_data1;                            \
	return __x;                                                  \
      }

      _GLIBCXX_SIMD_DEFINE_OP(+)
      _GLIBCXX_SIMD_DEFINE_OP(-)
      _GLIBCXX_SIMD_DEFINE_OP(*)
      _GLIBCXX_SIMD_DEFINE_OP(/)
      _GLIBCXX_SIMD_DEFINE_OP(%)
      _GLIBCXX_SIMD_DEFINE_OP(&)
      _GLIBCXX_SIMD_DEFINE_OP(|)
      _GLIBCXX_SIMD_DEFINE_OP(^)
      _GLIBCXX_SIMD_DEFINE_OP(<<)
      _GLIBCXX_SIMD_DEFINE_OP(>>)

#undef _GLIBCXX_SIMD_DEFINE_OP

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator<<=(basic_vec& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a, __simd_size_type __b) { __a << __b; }
      {
	__x._M_data0 <<= __y;
	__x._M_data1 <<= __y;
	return __x;
      }

      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec&
      operator>>=(basic_vec& __x, __simd_size_type __y) _GLIBCXX_SIMD_NOEXCEPT
      requires requires(value_type __a, __simd_size_type __b) { __a >> __b; }
      {
	__x._M_data0 >>= __y;
	__x._M_data1 >>= __y;
	return __x;
      }

      // [simd.comparison] ----------------------------------------------------
      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator==(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 == __y._M_data0, __x._M_data1 == __y._M_data1); }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator!=(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 != __y._M_data0, __x._M_data1 != __y._M_data1); }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator<(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 < __y._M_data0, __x._M_data1 < __y._M_data1); }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator<=(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 <= __y._M_data0, __x._M_data1 <= __y._M_data1); }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator>(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 > __y._M_data0, __x._M_data1 > __y._M_data1); }

      [[__gnu__::__always_inline__]]
      friend constexpr mask_type
      operator>=(const basic_vec& __x, const basic_vec& __y) noexcept
      { return mask_type::_S_init(__x._M_data0 >= __y._M_data0, __x._M_data1 >= __y._M_data1); }

      // [simd.cond] ---------------------------------------------------------
      [[__gnu__::__always_inline__]]
      friend constexpr basic_vec
      __select_impl(const mask_type& __k, const basic_vec& __t, const basic_vec& __f) noexcept
      {
	return _S_init(__select_impl(__k._M_data0, __t._M_data0, __f._M_data0),
		       __select_impl(__k._M_data1, __t._M_data1, __f._M_data1));
      }
    };

  // [simd.overview] deduction guide ------------------------------------------
  template <ranges::contiguous_range _Rg, typename... _Ts>
    requires ranges::__static_sized_range<_Rg>
    basic_vec(_Rg&& __r, _Ts...)
    -> basic_vec<ranges::range_value_t<_Rg>,
		 __deduce_abi_t<ranges::range_value_t<_Rg>,
#if 0 // PR117849
				static_cast<__simd_size_type>(ranges::size(__r))>>;
#else
				static_cast<__simd_size_type>(decltype(std::span(__r))::extent)>>;
#endif

  template <size_t _Bytes, typename _Ap>
    basic_vec(basic_mask<_Bytes, _Ap>)
    -> basic_vec<__integer_from<_Bytes>,
		 decltype(__abi_rebind<__integer_from<_Bytes>, basic_mask<_Bytes, _Ap>::size.value,
				       _Ap>())>;

  // [P3319R5] ----------------------------------------------------------------
  template <__vectorizable _Tp>
    requires is_arithmetic_v<_Tp>
    inline constexpr _Tp
    __iota<_Tp> = _Tp();

  template <typename _Tp, typename _Ap>
    inline constexpr basic_vec<_Tp, _Ap>
    __iota<basic_vec<_Tp, _Ap>> = basic_vec<_Tp, _Ap>([](_Tp __i) -> _Tp {
      static_assert(_Ap::_S_size - 1 <= numeric_limits<_Tp>::max(),
		    "iota object would overflow");
      return __i;
    });
} // namespace simd
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#pragma GCC diagnostic pop
#endif // C++26
#endif // _GLIBCXX_SIMD_VEC_H
