// Simd SVE specific implementations -*- C++ -*-

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


#ifndef _GLIBCXX_EXPERIMENTAL_SIMD_SVE_H_
#define _GLIBCXX_EXPERIMENTAL_SIMD_SVE_H_

#if __cplusplus >= 201703L

#if !_GLIBCXX_SIMD_HAVE_SVE
#error "simd_sve.h may only be included when SVE on ARM is available"
#endif

_GLIBCXX_SIMD_BEGIN_NAMESPACE

// Helper function mapping to sve supported types
template <typename _Tp>
  constexpr auto
  __get_sve_value_type()
  {
    if constexpr (is_integral_v<_Tp>)
      {
	if constexpr (is_signed_v<_Tp>)
	  {
	    if constexpr (sizeof(_Tp) == 1)
	      return int8_t{};
	    else if constexpr (sizeof(_Tp) == 2)
	      return int16_t{};
	    else if constexpr (sizeof(_Tp) == 4)
	      return int32_t{};
	    else if constexpr (sizeof(_Tp) == 8)
	      return int64_t{};
	    else
	      return _Tp{};
	  }
	else
	  {
	    if constexpr (sizeof(_Tp) == 1)
	      return uint8_t{};
	    else if constexpr (sizeof(_Tp) == 2)
	      return uint16_t{};
	    else if constexpr (sizeof(_Tp) == 4)
	      return uint32_t{};
	    else if constexpr (sizeof(_Tp) == 8)
	      return uint64_t{};
	    else
	      return _Tp{};
	  }
      }
    else
      {
	if constexpr (is_floating_point_v<_Tp>)
	  {
	    if constexpr (sizeof(_Tp) == 4)
	      return float32_t{};
	    else if constexpr (sizeof(_Tp) == 8)
	      return float64_t{};
	    else
	      return _Tp{};
	  }
      }
  }

template <typename _Tp>
  using __get_sve_value_type_t = decltype(__get_sve_value_type<_Tp>());

typedef svbool_t __sve_bool_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

template <typename _Tp, size_t _Np>
  struct __sve_vector_type;

template <typename _Tp, size_t _Np>
  using __sve_vector_type_t = typename __sve_vector_type<_Tp, _Np>::type;

template <size_t _Np>
  struct __sve_vector_type<int8_t, _Np>
  {
    typedef svint8_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(int8_t __dup)
    { return svdup_s8(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b8(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<uint8_t, _Np>
  {
    typedef svuint8_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(uint8_t __dup)
    { return svdup_u8(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b8(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<int16_t, _Np>
  {
    typedef svint16_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(int16_t __dup)
    { return svdup_s16(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b16(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<uint16_t, _Np>
  {
    typedef svuint16_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(uint16_t __dup)
    { return svdup_u16(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b16(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<int32_t, _Np>
  {
    typedef svint32_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(int32_t __dup)
    { return svdup_s32(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b32(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<uint32_t, _Np>
  {
    typedef svuint32_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(uint32_t __dup)
    { return svdup_u32(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b32(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<int64_t, _Np>
  {
    typedef svint64_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(int64_t __dup)
    { return svdup_s64(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b64(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<uint64_t, _Np>
  {
    typedef svuint64_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(uint64_t __dup)
    { return svdup_u64(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b64(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<float, _Np>
  {
    typedef svfloat32_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(float __dup)
    { return svdup_f32(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b32(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <size_t _Np>
  struct __sve_vector_type<double, _Np>
  {
    typedef svfloat64_t __sve_vlst_type __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static __sve_vlst_type
    __sve_broadcast(double __dup)
    { return svdup_f64(__dup); }

    inline static __sve_bool_type
    __sve_active_mask()
    { return svwhilelt_b64(size_t(0), _Np); };

    using type = __sve_vlst_type;
  };

template <typename _Tp, size_t _Np>
  struct __sve_vector_type
  : __sve_vector_type<__get_sve_value_type_t<_Tp>, _Np>
  {};

template <size_t _Size>
  struct __sve_mask_type
  {
    static_assert((_Size & (_Size - 1)) != 0, "This trait may only be used for non-power-of-2 "
					      "sizes. Power-of-2 sizes must be specialized.");

    using type = typename __sve_mask_type<std::__bit_ceil(_Size)>::type;
  };

template <size_t _Size>
  using __sve_mask_type_t = typename __sve_mask_type<_Size>::type;

template <>
  struct __sve_mask_type<1>
  {
    using type = __sve_bool_type;

    using __sve_mask_uint_type = uint8_t;

    typedef svuint8_t __sve_mask_vector_type
    __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static auto
    __sve_mask_active_count(type __active_mask, type __pred)
    { return svcntp_b8(__active_mask, __pred); }

    inline static type
    __sve_mask_first_true()
    { return svptrue_pat_b8(SV_VL1); }

    inline static type
    __sve_mask_next_true(type __active_mask, type __pred)
    { return svpnext_b8(__active_mask, __pred); }

    inline static bool
    __sve_mask_get(type __active_mask, size_t __i)
    { return __sve_mask_vector_type(svdup_u8_z(__active_mask, 1))[__i]  != 0;}

    inline static const __sve_mask_vector_type __index0123 = svindex_u8(0, 1);
  };

template <>
  struct __sve_mask_type<2>
  {
    using type = __sve_bool_type;

    using __sve_mask_uint_type = uint16_t;

    typedef svuint16_t __sve_mask_vector_type
    __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static auto
    __sve_mask_active_count(type __active_mask, type __pred)
    { return svcntp_b16(__active_mask, __pred); }

    inline static type
    __sve_mask_first_true()
    { return svptrue_pat_b16(SV_VL1); }

    inline static type
    __sve_mask_next_true(type __active_mask, type __pred)
    { return svpnext_b16(__active_mask, __pred); }

    inline static bool
    __sve_mask_get(type __active_mask, size_t __i)
    { return __sve_mask_vector_type(svdup_u16_z(__active_mask, 1))[__i] != 0;}

    inline static const __sve_mask_vector_type __index0123 = svindex_u16(0, 1);
  };

template <>
  struct __sve_mask_type<4>
  {
    using type = __sve_bool_type;

    using __sve_mask_uint_type = uint32_t;

    typedef svuint32_t __sve_mask_vector_type
    __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static auto
    __sve_mask_active_count(type __active_mask, type __pred)
    { return svcntp_b32(__active_mask, __pred); }

    inline static type
    __sve_mask_first_true()
    { return svptrue_pat_b32(SV_VL1); }

    inline static type
    __sve_mask_next_true(type __active_mask, type __pred)
    { return svpnext_b32(__active_mask, __pred); }

    inline static bool
    __sve_mask_get(type __active_mask, size_t __i)
    { return __sve_mask_vector_type(svdup_u32_z(__active_mask, 1))[__i] != 0;}

    inline static const __sve_mask_vector_type __index0123 = svindex_u32(0, 1);
  };

template <>
  struct __sve_mask_type<8>
  {
    using type = __sve_bool_type;

    using __sve_mask_uint_type = uint64_t;

    typedef svuint64_t __sve_mask_vector_type
    __attribute__((arm_sve_vector_bits(__ARM_FEATURE_SVE_BITS)));

    inline static auto
    __sve_mask_active_count(type __active_mask, type __pred)
    { return svcntp_b64(__active_mask, __pred); }

    inline static type
    __sve_mask_first_true()
    { return svptrue_pat_b64(SV_VL1); }

    inline static type
    __sve_mask_next_true(type __active_mask, type __pred)
    { return svpnext_b64(__active_mask, __pred); }

    inline static bool
    __sve_mask_get(type __active_mask, size_t __i)
    { return __sve_mask_vector_type(svdup_u64_z(__active_mask, 1))[__i] != 0;}

    inline static const __sve_mask_vector_type __index0123 = svindex_u64(0, 1);
  };

template <typename _To, typename _From>
  _GLIBCXX_SIMD_INTRINSIC constexpr auto
  __sve_reinterpret_cast(_From __v)
  {
    if constexpr (std::is_same_v<_To, int32_t>)
      return svreinterpret_s32(__v);
    else if constexpr (std::is_same_v<_To, int64_t>)
      return svreinterpret_s64(__v);
    else if constexpr (std::is_same_v<_To, float32_t>)
      return svreinterpret_f32(__v);
    else if constexpr (std::is_same_v<_To, float64_t>)
      return svreinterpret_f64(__v);
    else
      __assert_unreachable<_To>(); // add more cases if needed.
  }

template <typename _Tp, size_t _Width>
  struct _SveSimdWrapper
  {
    static_assert(__is_vectorizable_v<_Tp>);

    static_assert(_Width >= 2); // 1 doesn't make sense, use _Tp directly then

    using _BuiltinType = __sve_vector_type_t<_Tp, _Width>;

    using value_type = _Tp;

    static inline constexpr size_t _S_full_size = sizeof(_BuiltinType) / sizeof(value_type);

    static inline constexpr int _S_size = _Width;

    static inline constexpr bool _S_is_partial = _S_full_size != _S_size;

    _BuiltinType _M_data;

    _GLIBCXX_SIMD_INTRINSIC constexpr _SveSimdWrapper<_Tp, _S_full_size>
    __as_full_vector() const
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveSimdWrapper(initializer_list<_Tp> __init)
    : _M_data(__generate_from_n_evaluations<_Width, _BuiltinType>(
		[&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
		  return __init.begin()[__i.value];
		}))
    {}

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveSimdWrapper() = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveSimdWrapper(const _SveSimdWrapper&) = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveSimdWrapper(_SveSimdWrapper&&) = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr _SveSimdWrapper&
    operator=(const _SveSimdWrapper&) = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr _SveSimdWrapper&
    operator=(_SveSimdWrapper&&) = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveSimdWrapper(__sve_vector_type_t<_Tp, _Width> __x)
    : _M_data(__x)
    {}

    template <typename... _As, typename = enable_if_t<((is_same_v<simd_abi::scalar, _As> && ...)
							 && sizeof...(_As) <= _Width)>>
      _GLIBCXX_SIMD_INTRINSIC constexpr
      operator _SimdTuple<_Tp, _As...>() const
      {
	return __generate_from_n_evaluations<sizeof...(_As), _SimdTuple<_Tp, _As...>>(
		 [&](auto __i) constexpr _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
		   return _M_data[int(__i)];
		 });
      }

    _GLIBCXX_SIMD_INTRINSIC constexpr
    operator const _BuiltinType&() const
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC constexpr
    operator _BuiltinType&()
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC constexpr _Tp
    operator[](size_t __i) const
    { return _M_data[__i]; }

    template <size_t __i>
      _GLIBCXX_SIMD_INTRINSIC constexpr _Tp
      operator[](_SizeConstant<__i>) const
      { return _M_data[__i]; }

    _GLIBCXX_SIMD_INTRINSIC constexpr void
    _M_set(size_t __i, _Tp __x)
    {
      _M_data[__i] = __x;
    }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop() const
    { return false; }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop_none_of() const
    { return false; }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop_all_of() const
    { return false; }
  };

template <size_t _Bits, size_t _Width>
  struct _SveMaskWrapper
  {
    using _BuiltinSveMaskType = __sve_mask_type<_Bits>;

    using _BuiltinSveVectorType = __sve_vector_type<__int_with_sizeof_t<_Bits>, _Width>;

    using _BuiltinType = typename _BuiltinSveMaskType::type;

    using value_type = bool;

    static constexpr size_t _S_full_size = sizeof(_BuiltinType);

    _GLIBCXX_SIMD_INTRINSIC constexpr _SveMaskWrapper<_Bits, _S_full_size>
    __as_full_vector() const
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveMaskWrapper() = default;

    _GLIBCXX_SIMD_INTRINSIC constexpr
    _SveMaskWrapper(_BuiltinType __k)
    : _M_data(__k)
    {};

    _GLIBCXX_SIMD_INTRINSIC
    operator const _BuiltinType&() const
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC
    operator _BuiltinType&()
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC _BuiltinType
    __intrin() const
    { return _M_data; }

    _GLIBCXX_SIMD_INTRINSIC constexpr value_type
    operator[](size_t __i) const
    {
      return _BuiltinSveMaskType::__sve_mask_get(_M_data, __i);
    }

    template <size_t __i>
      _GLIBCXX_SIMD_INTRINSIC constexpr value_type
      operator[](_SizeConstant<__i>) const
      {
	return _BuiltinSveMaskType::__sve_mask_get(_M_data, __i);
      }

    _GLIBCXX_SIMD_INTRINSIC constexpr void
    _M_set(size_t __i, value_type __x)
    {
      _BuiltinType __index
	= svcmpeq(_BuiltinSveVectorType::__sve_active_mask(), _BuiltinSveMaskType::__index0123,
		  typename _BuiltinSveMaskType::__sve_mask_uint_type(__i));

      if (__x)
	_M_data = svorr_z(_BuiltinSveVectorType::__sve_active_mask(), _M_data, __index);
      else
	_M_data = svbic_z(_BuiltinSveVectorType::__sve_active_mask(), _M_data, __index);
    }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop() const
    { return false; }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop_none_of() const
    { return false; }

    _GLIBCXX_SIMD_INTRINSIC constexpr bool
    _M_is_constprop_all_of() const
    { return false; }

    _BuiltinType _M_data;
  };

struct _CommonImplSve;

template <typename _Abi, typename = __detail::__odr_helper>
  struct _SimdImplSve;

template <typename _Abi, typename = __detail::__odr_helper>
  struct _MaskImplSve;

template <int _UsedBytes, int>
  struct simd_abi::_SveAbi
  {
    template <typename _Tp>
      static constexpr size_t _S_size = _UsedBytes / sizeof(_Tp);

    struct _IsValidAbiTag
    : __bool_constant<(_UsedBytes > 1)>
    {};

    template <typename _Tp>
      struct _IsValidSizeFor
      : __bool_constant<(_UsedBytes / sizeof(_Tp) > 1 && _UsedBytes % sizeof(_Tp) == 0
			   && _UsedBytes <= __sve_vectorized_size_bytes)>
      {};

    template <typename _Tp>
      struct _IsValid
      : conjunction<_IsValidAbiTag, __bool_constant<__have_sve>,
		    __bool_constant<(__vectorized_sizeof<_Tp>() > sizeof(_Tp))>,
		    _IsValidSizeFor<_Tp>>
      {};

    template <typename _Tp>
      static constexpr bool _S_is_valid_v = _IsValid<_Tp>::value;

    using _CommonImpl = _CommonImplSve;

    using _SimdImpl = _SimdImplSve<_SveAbi<_UsedBytes>>;

    using _MaskImpl = _MaskImplSve<_SveAbi<_UsedBytes>>;

    template <typename _Tp>
      using _MaskMember = _SveMaskWrapper<sizeof(_Tp), _S_size<_Tp>>;

    template <typename _Tp, bool = _S_is_valid_v<_Tp>>
      struct __traits : _InvalidTraits
      {};

    template <typename _Tp>
      struct __traits<_Tp, true>
      {
	using _IsValid = true_type;
	using _SimdImpl = _SimdImplSve<_SveAbi<_UsedBytes>>;
	using _MaskImpl = _MaskImplSve<_SveAbi<_UsedBytes>>;

	using _SimdMember = _SveSimdWrapper<_Tp, _S_size<_Tp>>;         // sve vector type
	using _MaskMember = _SveMaskWrapper<sizeof(_Tp), _S_size<_Tp>>; // sve mask type

	static constexpr size_t _S_simd_align = alignof(_SimdMember);
	static constexpr size_t _S_mask_align = alignof(_MaskMember);

	static constexpr size_t _S_full_size = _SimdMember::_S_full_size;
	static constexpr bool _S_is_partial = _SimdMember::_S_is_partial;

	struct _SimdBase
	{
	  _GLIBCXX_SIMD_ALWAYS_INLINE explicit
	  operator __sve_vector_type_t<_Tp, _S_size<_Tp>>() const
	  { return __data(*static_cast<const simd<_Tp, _SveAbi<_UsedBytes>>*>(this)); }
	};

	class _SimdCastType
	{
	  using _Ap = __sve_vector_type_t<_Tp, _S_size<_Tp>>;

	  _SimdMember _M_data;

	public:
	  _GLIBCXX_SIMD_ALWAYS_INLINE constexpr
	  _SimdCastType(_Ap __a)
	  : _M_data(__a)
	  {}

	  _GLIBCXX_SIMD_ALWAYS_INLINE constexpr
	  operator _SimdMember() const
	  { return _M_data; }
	};

	struct _MaskBase
	{
	  _GLIBCXX_SIMD_ALWAYS_INLINE explicit
	  operator __sve_mask_type_t<sizeof(_Tp)>() const
	  {
	    return __data(*static_cast<const simd_mask<_Tp, _SveAbi<_UsedBytes>>*>(this));
	  }
	};

	class _MaskCastType
	{
	  using _Ap = __sve_mask_type_t<sizeof(_Tp)>;

	  _Ap _M_data;

	public:
	  _GLIBCXX_SIMD_ALWAYS_INLINE constexpr
	  _MaskCastType(_Ap __a)
	  : _M_data(__a)
	  {}

	  _GLIBCXX_SIMD_ALWAYS_INLINE constexpr
	  operator _MaskMember() const
	  { return _M_data; }
	};
      };

    template <typename _Tp>
      static constexpr size_t _S_full_size = __traits<_Tp>::_S_full_size;

    template <typename _Tp>
      static constexpr bool _S_is_partial = __traits<_Tp>::_S_is_partial;
  };

template <typename _Tp, size_t _Np>
  using __sve_mask = __sve_mask_type<sizeof(_Tp)>;

struct _CommonImplSve
{
  // _S_converts_via_decomposition
  // This lists all cases where a __vector_convert needs to fall back to
  // conversion of individual scalars (i.e. decompose the input vector into
  // scalars, convert, compose output vector). In those cases, _S_masked_load &
  // _S_masked_store prefer to use the _S_bit_iteration implementation.
  template <typename _From, typename _To, size_t _ToSize>
    static inline constexpr bool __converts_via_decomposition_v = sizeof(_From) != sizeof(_To);

  template <typename _Tp, typename _Up, size_t _Np>
    _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
    _S_load(const _Up* __p, _SveMaskWrapper<sizeof(_Tp), _Np> __k)
    {
      using _STp = __get_sve_value_type_t<_Tp>;
      using _SUp = __get_sve_value_type_t<_Up>;
      using _V = __sve_vector_type_t<_Tp, _Np>;
      const _SUp* __up = reinterpret_cast<const _SUp*>(__p);

      if constexpr (std::is_same_v<_Tp, _Up>)
	return _V(svld1(__k._M_data, __up));
      if constexpr (std::is_integral_v<_Tp> && std::is_integral_v<_Up>
		      && (sizeof(_Tp) > sizeof(_Up)))
	{
	  if constexpr (std::is_same_v<_SUp, int8_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int16_t>)
		return _V(svld1sb_s16(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint16_t>)
		return _V(svld1sb_u16(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int32_t>)
		return _V(svld1sb_s32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint32_t>)
		return _V(svld1sb_u32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1sb_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1sb_u64(__k._M_data, __up));
	    }
	  if constexpr (std::is_same_v<_SUp, uint8_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int16_t>)
		return _V(svld1ub_s16(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint16_t>)
		return _V(svld1ub_u16(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int32_t>)
		return _V(svld1ub_s32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint32_t>)
		return _V(svld1ub_u32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1ub_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1ub_u64(__k._M_data, __up));
	    }
	  if constexpr (std::is_same_v<_SUp, int16_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int32_t>)
		return _V(svld1sh_s32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint32_t>)
		return _V(svld1sh_u32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1sh_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1sh_u64(__k._M_data, __up));
	    }
	  if constexpr (std::is_same_v<_SUp, uint16_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int32_t>)
		return _V(svld1uh_s32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint32_t>)
		return _V(svld1uh_u32(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1uh_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1uh_u64(__k._M_data, __up));
	    }
	  if constexpr (std::is_same_v<_SUp, int32_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1sw_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1sw_u64(__k._M_data, __up));
	    }
	  if constexpr (std::is_same_v<_SUp, uint32_t>)
	    {
	      if constexpr (std::is_same_v<_STp, int64_t>)
		return _V(svld1uw_s64(__k._M_data, __up));
	      if constexpr (std::is_same_v<_STp, uint64_t>)
		return _V(svld1uw_u64(__k._M_data, __up));
	    }
	}
      return __generate_from_n_evaluations<_Np, __sve_vector_type_t<_Tp, _Np>>(
	       [&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
		 return __k[__i] ? static_cast<_Tp>(__p[__i]) : _Tp{};
	       });
    }

  template <typename _Tp, typename _Up, size_t _Np>
    _GLIBCXX_SIMD_INTRINSIC static constexpr void
    _S_store(_Up* __p, _SveSimdWrapper<_Tp, _Np> __x, _SveMaskWrapper<sizeof(_Tp), _Np> __k)
    {
      using _SUp = __get_sve_value_type_t<_Up>;
      using _STp = __get_sve_value_type_t<_Tp>;

      _SUp* __up = reinterpret_cast<_SUp*>(__p);

      if constexpr (std::is_same_v<_Tp, _Up>)
	return svst1(__k._M_data, __up, __x);
      if constexpr (std::is_integral_v<_Tp> && std::is_integral_v<_Up>
		      && (sizeof(_Tp) > sizeof(_Up)))
	{
	  if constexpr (std::is_same_v<_SUp, int8_t> && std::is_signed_v<_STp>)
	    return svst1b(__k._M_data, __up, __x);
	  if constexpr (std::is_same_v<_SUp, uint8_t> && std::is_unsigned_v<_STp>)
	    return svst1b(__k._M_data, __up, __x);
	  if constexpr (std::is_same_v<_SUp, int16_t> && std::is_signed_v<_STp>)
	    return svst1h(__k._M_data, __up, __x);
	  if constexpr (std::is_same_v<_SUp, uint16_t> && std::is_unsigned_v<_STp>)
	    return svst1h(__k._M_data, __up, __x);
	  if constexpr (std::is_same_v<_SUp, int32_t> && std::is_signed_v<_STp>)
	    return svst1w(__k._M_data, __up, __x);
	  if constexpr (std::is_same_v<_SUp, uint32_t> && std::is_unsigned_v<_STp>)
	    return svst1w(__k._M_data, __up, __x);
	}

      __execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	if (__k[__i])
	  __p[__i] = static_cast<_Up>(__x[__i]);
      });
    }

  template <typename _Tp, size_t _Np>
    _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
    _S_blend(_SveMaskWrapper<sizeof(_Tp), _Np> __k, _SveSimdWrapper<_Tp, _Np> __at0,
	     _SveSimdWrapper<_Tp, _Np> __at1)
    { return svsel(__k._M_data, __at1._M_data, __at0._M_data); }

  template <size_t _Np, bool _Sanitized>
    _GLIBCXX_SIMD_INTRINSIC static constexpr void
    _S_store_bool_array(_BitMask<_Np, _Sanitized> __x, bool* __mem)
    {
      __execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	__mem[__i] = __x[__i];
      });
    }
};

template <typename _Abi, typename>
  struct _SimdImplSve
  {
    template <typename _Tp>
      using _MaskMember = typename _Abi::template _MaskMember<_Tp>;

    template <typename _Tp>
      using _SimdMember = typename _Abi::template __traits<_Tp>::_SimdMember;

    using _CommonImpl = typename _Abi::_CommonImpl;
    using _SuperImpl = typename _Abi::_SimdImpl;
    using _MaskImpl = typename _Abi::_MaskImpl;

    template <typename _Tp>
      static constexpr size_t _S_full_size = _Abi::template _S_full_size<_Tp>;

    template <typename _Tp>
      static constexpr size_t _S_size = _Abi::template _S_size<_Tp>;

    template <typename _Tp>
      using _TypeTag = _Tp*;

    using abi_type = _Abi;

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr auto
      _S_broadcast(_Tp __x) noexcept
      {
	return __sve_vector_type<_Tp, __sve_vectorized_size_bytes / sizeof(_Tp)>
		 ::__sve_broadcast(__x);
      }

    template <typename _Fp, typename _Tp>
      inline static constexpr _SimdMember<_Tp>
      _S_generator(_Fp&& __gen, _TypeTag<_Tp>)
      {
	constexpr size_t _Np = _S_size<_Tp>;
	_SveSimdWrapper<_Tp, _Np> __ret;
	__execute_n_times<_S_size<_Tp>>(
	  [&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA { __ret._M_set(__i, __gen(__i)); });
	return __ret;
      }

    template <typename _Tp, typename _Up>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SimdMember<_Tp>
      _S_load(const _Up* __mem, _TypeTag<_Tp>) noexcept
      {
	constexpr size_t _Np = _S_size<_Tp>;
	_SimdMember<_Tp> __ret = _CommonImpl::template _S_load<_Tp, _Up, _Np>(
				   __mem, _SveMaskWrapper<sizeof(_Tp), _Np>{
				     __sve_vector_type<_Tp, _Np>::__sve_active_mask()});
	return __ret;
      }

    template <typename _Tp, size_t _Np, typename _Up>
      static constexpr inline _SveSimdWrapper<_Tp, _Np>
      _S_masked_load(_SveSimdWrapper<_Tp, _Np> __merge, _MaskMember<_Tp> __k, const _Up* __mem)
      noexcept
      {
	__sve_vector_type_t<_Tp, _Np> __v
	  = _CommonImpl::template _S_load<_Tp, _Up, _Np>(__mem, __k);
	__sve_vector_type_t<_Tp, _Np> __ret = svsel(__k._M_data, __v, __merge._M_data);
	return __ret;
      }

    template <typename _Tp, typename _Up>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_store(_SimdMember<_Tp> __v, _Up* __mem, _TypeTag<_Tp>) noexcept
      {
	constexpr size_t _Np = _S_size<_Tp>;
	_CommonImpl::template _S_store<_Tp, _Up, _Np>(
	  __mem, __v, __sve_vector_type<_Tp, _Np>::__sve_active_mask());
      }

    template <typename _Tp, typename _Up, size_t _Np>
      static constexpr inline void
      _S_masked_store(const _SveSimdWrapper<_Tp, _Np> __v, _Up* __mem,
		      const _SveMaskWrapper<sizeof(_Tp), _Np> __k) noexcept
      { _CommonImpl::template _S_store<_Tp, _Up, _Np>(__mem, __v, __k); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_negate(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      {
	return svcmpeq(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data,
		       __sve_vector_type<_Tp, _Np>::__sve_broadcast(_Tp{}));
      }

    template <typename _Tp, typename _BinaryOperation>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, _BinaryOperation&& __binary_op)
      {
	auto __x_data = __x._M_data;
	constexpr size_t _Np = simd_size_v<_Tp, _Abi>;
	using __sve_vec_t = __sve_vector_type_t<_Tp, _Np>;
	std::size_t __i = __x.size();
	for (; (__i % 2) != 1; __i /= 2)
	  {
	    __x_data = __binary_op(simd<_Tp, _Abi>(
				     __private_init, _SveSimdWrapper<_Tp, _Np>(
						       __sve_vec_t(svuzp1(__x_data, __x_data)))),
				   simd<_Tp, _Abi>(
				     __private_init, _SveSimdWrapper<_Tp, _Np>(
						       __sve_vec_t(svuzp2(__x_data, __x_data))))
				  )._M_data;
	  }
	_Tp __res = __x_data[0];
	for (size_t __ri = 1; __ri != __i; __ri++)
	  __res = __binary_op(__x_data[__ri], __res);
	return __res;
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, plus<>)
      {
	return svaddv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, bit_and<>)
      {
	return svandv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, bit_or<>)
      {
	return svorv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, bit_xor<>)
      {
	return sveorv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, __detail::_Maximum())
      {
	return svmaxv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _Tp
      _S_reduce(simd<_Tp, _Abi> __x, __detail::_Minimum())
      {
	return svminv(__sve_vector_type<_Tp, _S_size<_Tp>>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_NORMAL_MATH _GLIBCXX_SIMD_INTRINSIC static constexpr
      __sve_vector_type_t<_Tp, _Np>
      _S_min(_SveSimdWrapper<_Tp, _Np> __a, _SveSimdWrapper<_Tp, _Np> __b)
      {
	return svmin_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __a._M_data, __b._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_NORMAL_MATH _GLIBCXX_SIMD_INTRINSIC static constexpr
      __sve_vector_type_t<_Tp, _Np>
      _S_max(_SveSimdWrapper<_Tp, _Np> __a, _SveSimdWrapper<_Tp, _Np> __b)
      {
	return svmax_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __a._M_data, __b._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_NORMAL_MATH _GLIBCXX_SIMD_INTRINSIC static constexpr
      pair<_SveSimdWrapper<_Tp, _Np>, _SveSimdWrapper<_Tp, _Np>>
      _S_minmax(_SveSimdWrapper<_Tp, _Np> __a, _SveSimdWrapper<_Tp, _Np> __b)
      {
	return {
	  svmin_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __a._M_data, __b._M_data),
	  svmax_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __a._M_data, __b._M_data)
	};
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_complement(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      {
	if constexpr (is_floating_point_v<_Tp>)
	  {
	    using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	    return __sve_reinterpret_cast<_Tp>(
		     svnot_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			     __sve_reinterpret_cast<_Ip>(__x)));
	  }
	else
	  return svnot_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveSimdWrapper<_Tp, _Np>
      _S_unary_minus(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      {
	return svmul_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data,
		       static_cast<_Tp>(-1));
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_plus(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      { return __x._M_data + __y._M_data; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_minus(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      { return __x._M_data - __y._M_data; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_multiplies(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      { return __x._M_data * __y._M_data; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_divides(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	__sve_vector_type_t<_Tp, _Np> __y_padded
	  = svsel(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
		  __y._M_data, __sve_vector_type<_Tp, _Np>::__sve_broadcast(1));
	return __x._M_data / __y_padded;
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_modulus(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	__sve_vector_type_t<_Tp, _Np> __y_padded
	  = svsel(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
		  __y._M_data, __sve_vector_type<_Tp, _Np>::__sve_broadcast(1));
	return __x._M_data % __y_padded;
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_bit_and(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	if constexpr (is_floating_point_v<_Tp>)
	  {
	    using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	    return __sve_reinterpret_cast<_Tp>(
		     svand_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			     __sve_reinterpret_cast<_Ip>(__x), __sve_reinterpret_cast<_Ip>(__y)));
	  }
	else
	  return svand_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			 __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_bit_or(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	if constexpr (is_floating_point_v<_Tp>)
	  {
	    using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	    return __sve_reinterpret_cast<_Tp>(
		     svorr_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			     __sve_reinterpret_cast<_Ip>(__x), __sve_reinterpret_cast<_Ip>(__y)));
	  }
	else
	  return svorr_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			 __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_bit_xor(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	if constexpr (is_floating_point_v<_Tp>)
	  {
	    using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	    return __sve_reinterpret_cast<_Tp>(
		     sveor_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			     __sve_reinterpret_cast<_Ip>(__x), __sve_reinterpret_cast<_Ip>(__y)));
	  }
	else
	  return sveor_x(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
			 __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static __sve_vector_type_t<_Tp, _Np>
      _S_bit_shift_left(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      { return __x._M_data << __y._M_data; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static __sve_vector_type_t<_Tp, _Np>
      _S_bit_shift_right(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      { return __x._M_data >> __y._M_data; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_bit_shift_left(_SveSimdWrapper<_Tp, _Np> __x, int __y)
      { return __x._M_data << __y; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr __sve_vector_type_t<_Tp, _Np>
      _S_bit_shift_right(_SveSimdWrapper<_Tp, _Np> __x, int __y)
      { return __x._M_data >> __y; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_increment(_SveSimdWrapper<_Tp, _Np>& __x)
      { __x = __x._M_data + 1; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_decrement(_SveSimdWrapper<_Tp, _Np>& __x)
      { __x = __x._M_data - 1; }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_equal_to(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svcmpeq(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_not_equal_to(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svcmpne(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_less(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svcmplt(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_less_equal(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svcmple(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    // simd.math
#define _GLIBCXX_SIMD_MATH_FALLBACK(__name)                                                        \
    template <typename _Tp, size_t _Np, typename... _More>                                         \
      static _SveSimdWrapper<_Tp, _Np> _S_##__name(const _SveSimdWrapper<_Tp, _Np>& __x,           \
						   const _More&... __more)                         \
      {                                                                                            \
	_SveSimdWrapper<_Tp, _Np> __r;                                                             \
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {                  \
	  __r._M_set(__i, __name(__x[__i], __more[__i]...));                                       \
	});                                                                                        \
	return __r;                                                                                \
      }

#define _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(_RetTp, __name)                                       \
    template <typename _Tp, typename... _More>                                                     \
      static auto _S_##__name(const _Tp& __x, const _More&... __more)                              \
      {                                                                                            \
	return __fixed_size_storage_t<_RetTp, _Tp::_S_size>::_S_generate(                          \
		 [&](auto __meta) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {                             \
		   return __meta._S_generator(                                                     \
			    [&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {                     \
			      return __name(__x[__meta._S_offset + __i],                           \
					    __more[__meta._S_offset + __i]...);                    \
			    }, static_cast<_RetTp*>(nullptr));                                     \
		 });                                                                               \
      }

    _GLIBCXX_SIMD_MATH_FALLBACK(acos)
    _GLIBCXX_SIMD_MATH_FALLBACK(asin)
    _GLIBCXX_SIMD_MATH_FALLBACK(atan)
    _GLIBCXX_SIMD_MATH_FALLBACK(atan2)
    _GLIBCXX_SIMD_MATH_FALLBACK(cos)
    _GLIBCXX_SIMD_MATH_FALLBACK(sin)
    _GLIBCXX_SIMD_MATH_FALLBACK(tan)
    _GLIBCXX_SIMD_MATH_FALLBACK(acosh)
    _GLIBCXX_SIMD_MATH_FALLBACK(asinh)
    _GLIBCXX_SIMD_MATH_FALLBACK(atanh)
    _GLIBCXX_SIMD_MATH_FALLBACK(cosh)
    _GLIBCXX_SIMD_MATH_FALLBACK(sinh)
    _GLIBCXX_SIMD_MATH_FALLBACK(tanh)
    _GLIBCXX_SIMD_MATH_FALLBACK(exp)
    _GLIBCXX_SIMD_MATH_FALLBACK(exp2)
    _GLIBCXX_SIMD_MATH_FALLBACK(expm1)
    _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(int, ilogb)
    _GLIBCXX_SIMD_MATH_FALLBACK(log)
    _GLIBCXX_SIMD_MATH_FALLBACK(log10)
    _GLIBCXX_SIMD_MATH_FALLBACK(log1p)
    _GLIBCXX_SIMD_MATH_FALLBACK(log2)
    _GLIBCXX_SIMD_MATH_FALLBACK(logb)

    // modf implemented in simd_math.h
    _GLIBCXX_SIMD_MATH_FALLBACK(scalbn)
    _GLIBCXX_SIMD_MATH_FALLBACK(scalbln)
    _GLIBCXX_SIMD_MATH_FALLBACK(cbrt)
    _GLIBCXX_SIMD_MATH_FALLBACK(pow)
    _GLIBCXX_SIMD_MATH_FALLBACK(erf)
    _GLIBCXX_SIMD_MATH_FALLBACK(erfc)
    _GLIBCXX_SIMD_MATH_FALLBACK(lgamma)
    _GLIBCXX_SIMD_MATH_FALLBACK(tgamma)

    _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(long, lrint)
    _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(long long, llrint)

    _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(long, lround)
    _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET(long long, llround)

    _GLIBCXX_SIMD_MATH_FALLBACK(fmod)
    _GLIBCXX_SIMD_MATH_FALLBACK(remainder)

    template <typename _Tp, size_t _Np>
      static _SveSimdWrapper<_Tp, _Np>
      _S_remquo(const _SveSimdWrapper<_Tp, _Np> __x, const _SveSimdWrapper<_Tp, _Np> __y,
		__fixed_size_storage_t<int, _Np>* __z)
      {
	_SveSimdWrapper<_Tp, _Np> __r{};
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  int __tmp;
	  __r._M_set(__i, remquo(__x[__i], __y[__i], &__tmp));
	  __z->_M_set(__i, __tmp);
	});
	return __r;
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static __fixed_size_storage_t<int, _Np>
      _S_fpclassify(_SveSimdWrapper<_Tp, _Np> __x)
      {
	__fixed_size_storage_t<int, _Np> __r{};
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  __r._M_set(__i, std::fpclassify(__x[__i]));
	});
	return __r;
      }

    // copysign in simd_math.h
    _GLIBCXX_SIMD_MATH_FALLBACK(nextafter)
    _GLIBCXX_SIMD_MATH_FALLBACK(fdim)

#undef _GLIBCXX_SIMD_MATH_FALLBACK
#undef _GLIBCXX_SIMD_MATH_FALLBACK_FIXEDRET

    template <typename _Tp, size_t _Np, typename _Op>
      static constexpr _MaskMember<_Tp>
      __fp_cmp(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y, _Op __op)
      {
	using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	using _VI = __sve_vector_type_t<_Ip, _Np>;
	using _WI = _SveSimdWrapper<_Ip, _Np>;
	const _WI __fmv = __sve_vector_type<_Ip, _Np>::__sve_broadcast(__finite_max_v<_Ip>);
	const _WI __zerov = __sve_vector_type<_Ip, _Np>::__sve_broadcast(0);
	const _WI __xn = _VI(__sve_reinterpret_cast<_Ip>(__x));
	const _WI __yn = _VI(__sve_reinterpret_cast<_Ip>(__y));

	const _WI __xp
	  = svsel(_S_less(__xn, __zerov), _S_unary_minus(_WI(_S_bit_and(__xn, __fmv))), __xn);
	const _WI __yp
	  = svsel(_S_less(__yn, __zerov), _S_unary_minus(_WI(_S_bit_and(__yn, __fmv))), __yn);
	return svbic_z(__sve_vector_type<_Ip, _Np>::__sve_active_mask(), __op(__xp, __yp)._M_data,
		       _SuperImpl::_S_isunordered(__x, __y)._M_data);
      }

    template <typename _Tp, size_t _Np>
      static constexpr _MaskMember<_Tp>
      _S_isgreater(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y) noexcept
      { return __fp_cmp(__x, __y, [](auto __xp, auto __yp) { return _S_less(__yp, __xp); }); }

    template <typename _Tp, size_t _Np>
      static constexpr _MaskMember<_Tp>
      _S_isgreaterequal(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y) noexcept
      { return __fp_cmp(__x, __y, [](auto __xp, auto __yp) { return _S_less_equal(__yp, __xp); }); }

    template <typename _Tp, size_t _Np>
      static constexpr _MaskMember<_Tp>
      _S_isless(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y) noexcept
      { return __fp_cmp(__x, __y, [](auto __xp, auto __yp) { return _S_less(__xp, __yp); }); }

    template <typename _Tp, size_t _Np>
      static constexpr _MaskMember<_Tp>
      _S_islessequal(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y) noexcept
      { return __fp_cmp(__x, __y, [](auto __xp, auto __yp) { return _S_less_equal(__xp, __yp); }); }

    template <typename _Tp, size_t _Np>
      static constexpr _MaskMember<_Tp>
      _S_islessgreater(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y) noexcept
      {
	return svbic_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(),
		       _SuperImpl::_S_not_equal_to(__x, __y)._M_data,
		       _SuperImpl::_S_isunordered(__x, __y)._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_abs(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svabs_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_fabs(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svabs_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_sqrt(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svsqrt_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_ldexp(_SveSimdWrapper<_Tp, _Np> __x, __fixed_size_storage_t<int, _Np> __y) noexcept
      {
	auto __sve_register = __y.first;
	if constexpr (std::is_same_v<_Tp, float>)
	  return svscale_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data,
			   __sve_register._M_data);
	else
	  {
	    __sve_vector_type_t<int64_t, _Np> __sve_d_register = svunpklo(__sve_register);
	    return svscale_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data,
			     __sve_d_register);
	  }
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_fma(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y,
	     _SveSimdWrapper<_Tp, _Np> __z)
      {
	return svmad_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data,
		       __z._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_fmax(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svmaxnm_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_fmin(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svminnm_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_isfinite([[maybe_unused]] _SveSimdWrapper<_Tp, _Np> __x)
      {
#if __FINITE_MATH_ONLY__
	return __sve_vector_type_t<_Tp, _Np>::__sve_all_true_mask();
#else
	// if all exponent bits are set, __x is either inf or NaN

	using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	const __sve_vector_type_t<_Ip, _Np> __absn = __sve_reinterpret_cast<_Ip>(_S_abs(__x));
	const __sve_vector_type_t<_Ip, _Np> __maxn
	  = __sve_reinterpret_cast<_Ip>(
	      __sve_vector_type<_Tp, _Np>::__sve_broadcast(__finite_max_v<_Tp>));

	return _S_less_equal(_SveSimdWrapper<_Ip, _Np>{__absn}, _SveSimdWrapper<_Ip, _Np>{__maxn});
#endif
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_isinf([[maybe_unused]] _SveSimdWrapper<_Tp, _Np> __x)
      {
#if __FINITE_MATH_ONLY__
	return {}; // false
#else
	return _S_equal_to<_Tp, _Np>(_S_abs(__x), _S_broadcast(__infinity_v<_Tp>));
#endif
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_isnan([[maybe_unused]] _SveSimdWrapper<_Tp, _Np> __x)
      {
#if __FINITE_MATH_ONLY__
	return {}; // false
#else
	return svcmpuo(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __x._M_data);
#endif
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_isnormal(_SveSimdWrapper<_Tp, _Np> __x)
      {
	using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	using _V = __sve_vector_type_t<_Ip, _Np>;
	using _VW = _SveSimdWrapper<_Ip, _Np>;

	const _V __absn = __sve_reinterpret_cast<_Ip>(_S_abs(__x));
	const _V __minn = __sve_reinterpret_cast<_Ip>(
			    __sve_vector_type<_Tp, _Np>::__sve_broadcast(__norm_min_v<_Tp>));
#if __FINITE_MATH_ONLY__
	return _S_greater_equal(_VW{__absn}, _VW{__minn});
#else
	const _V __maxn = __sve_reinterpret_cast<_Ip>(
			    __sve_vector_type<_Tp, _Np>::__sve_broadcast(__finite_max_v<_Tp>));
	return _MaskImpl::_S_bit_and(_S_less_equal(_VW{__minn}, _VW{__absn}),
				     _S_less_equal(_VW{__absn}, _VW{__maxn}));
#endif
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_signbit(_SveSimdWrapper<_Tp, _Np> __x)
      {
	using _Ip = __get_sve_value_type_t<__int_for_sizeof_t<_Tp>>;
	using _V = __sve_vector_type_t<_Ip, _Np>;
	using _VW = _SveSimdWrapper<_Ip, _Np>;

	const _V __xn = __sve_reinterpret_cast<_Ip>(__x);
	const _V __zeron = __sve_vector_type<_Ip, _Np>::__sve_broadcast(0);
	return _S_less(_VW{__xn}, _VW{__zeron});
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_isunordered(_SveSimdWrapper<_Tp, _Np> __x, _SveSimdWrapper<_Tp, _Np> __y)
      {
	return svcmpuo(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data, __y._M_data);
      }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_nearbyint(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svrinti_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_rint(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return _SuperImpl::_S_nearbyint(__x); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_trunc(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svrintz_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_round(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svrinta_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_floor(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svrintm_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static _SveSimdWrapper<_Tp, _Np>
      _S_ceil(_SveSimdWrapper<_Tp, _Np> __x) noexcept
      { return svrintp_z(__sve_vector_type<_Tp, _Np>::__sve_active_mask(), __x._M_data); }

    template <typename _Tp, size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_masked_assign(_SveMaskWrapper<_Bits, _Np> __k, _SveSimdWrapper<_Tp, _Np>& __lhs,
		       __type_identity_t<_SveSimdWrapper<_Tp, _Np>> __rhs)
      { __lhs = _CommonImpl::_S_blend(__k, __lhs, __rhs); }

    template <typename _Tp, size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_masked_assign(_SveMaskWrapper<_Bits, _Np> __k, _SveSimdWrapper<_Tp, _Np>& __lhs,
		       __type_identity_t<_Tp> __rhs)
      { __lhs = _CommonImpl::_S_blend(__k, __lhs, __data(simd<_Tp, _Abi>(__rhs))); }

    template <typename _Op, typename _Tp, size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_masked_cassign(const _SveMaskWrapper<_Bits, _Np> __k, _SveSimdWrapper<_Tp, _Np>& __lhs,
			const __type_identity_t<_SveSimdWrapper<_Tp, _Np>> __rhs, _Op __op)
      {
	__lhs = _CommonImpl::_S_blend(__k, __lhs,
				      _SveSimdWrapper<_Tp, _Np>(__op(_SuperImpl{}, __lhs, __rhs)));
      }

    template <typename _Op, typename _Tp, size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_masked_cassign(const _SveMaskWrapper<_Bits, _Np> __k, _SveSimdWrapper<_Tp, _Np>& __lhs,
			const __type_identity_t<_Tp> __rhs, _Op __op)
      { _S_masked_cassign(__k, __lhs, _S_broadcast(__rhs), __op); }

    template <typename _Tp, size_t _Np, typename _Up>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_set(_SveSimdWrapper<_Tp, _Np>& __v, int __i, _Up&& __x) noexcept
      { __v._M_set(__i, static_cast<_Up&&>(__x)); }

    template <template <typename> class _Op, typename _Tp, size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveSimdWrapper<_Tp, _Np>
      _S_masked_unary(const _SveMaskWrapper<_Bits, _Np> __k, const _SveSimdWrapper<_Tp, _Np> __v)
      {
	auto __vv = simd<_Tp, _Abi>{__private_init, __v};
	_Op<decltype(__vv)> __op;
	return _CommonImpl::_S_blend(__k, __v, __data(__op(__vv)));
      }
  };

template <typename _Abi, typename>
  struct _MaskImplSve
  {
    template <typename _Tp>
      using _MaskMember = typename _Abi::template _MaskMember<_Tp>;

    template <typename _Tp>
      using _TypeTag = _Tp*;

    template <typename _Tp>
      static constexpr size_t _S_size = simd_size_v<_Tp, _Abi>;

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_broadcast(bool __x)
      {
	constexpr size_t _Np = simd_size_v<_Tp, _Abi>;
	__sve_bool_type __tr = __sve_vector_type<_Tp, _Np>::__sve_active_mask();
	__sve_bool_type __fl = svpfalse_b();
	return __x ? __tr : __fl;
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_load(const bool* __mem)
      {
	constexpr size_t _Np = simd_size_v<_Tp, _Abi>;
	const uint8_t* __p = reinterpret_cast<const uint8_t*>(__mem);
	__sve_bool_type __u8_active_mask = __sve_vector_type<uint8_t, _Np>::__sve_active_mask();
	__sve_vector_type_t<uint8_t, _Np> __u8_vec_mask_load = svld1(__u8_active_mask, __p);
	__sve_bool_type __u8_mask = svcmpne(__u8_active_mask, __u8_vec_mask_load, 0);

	__sve_bool_type __tp_mask = __u8_mask;
	for (size_t __up_size = 1; __up_size != sizeof(_Tp); __up_size *= 2)
	  {
	    __tp_mask = svunpklo(__tp_mask);
	  }

	_SveMaskWrapper<sizeof(_Tp), simd_size_v<_Tp, _Abi>> __r{__tp_mask};
	return __r;
      }

    template <size_t _Bits, size_t _Np>
      static inline _SveMaskWrapper<_Bits, _Np>
      _S_masked_load(_SveMaskWrapper<_Bits, _Np> __merge, _SveMaskWrapper<_Bits, _Np> __mask,
		     const bool* __mem) noexcept
      {
	_SveMaskWrapper<_Bits, _Np> __r;

	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  if (__mask[__i])
	    __r._M_set(__i, __mem[__i]);
	  else
	    __r._M_set(__i, __merge[__i]);
	});

	return __r;
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_store(_SveMaskWrapper<_Bits, _Np> __v, bool* __mem) noexcept
      {
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  __mem[__i] = __v[__i];
	});
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr void
      _S_masked_store(const _SveMaskWrapper<_Bits, _Np> __v, bool* __mem,
		      const _SveMaskWrapper<_Bits, _Np> __k) noexcept
      {
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  if (__k[__i])
	    __mem[__i] = __v[__i];
	});
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SanitizedBitMask<_Np>
      _S_to_bits(_SveMaskWrapper<_Bits, _Np> __x)
      {
	_ULLong __r = 0;
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  __r |= _ULLong(__x[__i]) << __i;
	});
	return __r;
      }

    template <size_t _Np, typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static _MaskMember<_Tp>
      _S_from_bitmask(_SanitizedBitMask<_Np> __bits, _TypeTag<_Tp>)
      {
	_SveMaskWrapper<sizeof(_Tp), _Np> __r;
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  __r._M_set(__i, __bits[__i]);
	});
	return __r;
      }

    template <typename _Tp, typename _Up, typename _UAbi>
      _GLIBCXX_SIMD_INTRINSIC static constexpr auto
      _S_convert(simd_mask<_Up, _UAbi> __x)
      {
	using _R = _SveMaskWrapper<sizeof(_Tp), simd_size_v<_Tp, _Abi>>;
	if constexpr (__is_scalar_abi<_UAbi>())
	  {
	    _R __r{__sve_bool_type(svpfalse())};
	    __r._M_set(0, __data(__x));
	    return __r;
	  }
	if constexpr (__is_sve_abi<_UAbi>())
	  {
	    if constexpr (sizeof(_Up) == sizeof(_Tp))
	      return __data(__x);
	    if constexpr (sizeof(_Up) < sizeof(_Tp))
	      {
		__sve_bool_type __xmdata = __data(__x)._M_data;
		__sve_bool_type __r = __xmdata;
		for (size_t __up_size = sizeof(_Up); __up_size != sizeof(_Tp); __up_size *= 2)
		  {
		    __r = svunpklo(__r);
		  }
		return _R{__r};
	      }
	    else
	      {
		_R __r{__sve_bool_type(svpfalse())};
		constexpr size_t __min_size
		  = std::min(simd_size_v<_Tp, _Abi>, simd_mask<_Up, _UAbi>::size());
		__execute_n_times<__min_size>(
		  [&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA { __r._M_set(__i, __x[__i]); });
		return __r;
	      }
	  }
	if constexpr (__is_neon_abi<_UAbi>())
	  {
	    _R __r{__sve_bool_type(svpfalse())};
	    constexpr size_t __min_size
	      = std::min(simd_size_v<_Tp, _Abi>, simd_mask<_Up, _UAbi>::size());
	    __execute_n_times<__min_size>(
	      [&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA { __r._M_set(__i, __x[__i]); });
	    return __r;
	  }
	if constexpr (__is_fixed_size_abi<_UAbi>())
	  {
	    return _S_convert<_Tp>(__data(__x));
	  }
	return _R{};
      }

    template <typename _Tp, size_t _Np, bool _Sanitized>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _MaskMember<_Tp>
      _S_convert(_BitMask<_Np, _Sanitized> __x)
      {
	_MaskMember<_Tp> __r{};
	__execute_n_times<_Np>([&](auto __i) _GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA {
	  __r._M_set(__i, __x[__i]);
	});
	return __r;
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_logical_and(const _SveMaskWrapper<_Bits, _Np>& __x, const _SveMaskWrapper<_Bits, _Np>& __y)
      {
	return svand_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data, __y._M_data);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_logical_or(const _SveMaskWrapper<_Bits, _Np>& __x, const _SveMaskWrapper<_Bits, _Np>& __y)
      {
	return svorr_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data, __y._M_data);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_bit_not(const _SveMaskWrapper<_Bits, _Np>& __x)
      {
	return svnot_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_bit_and(const _SveMaskWrapper<_Bits, _Np>& __x, const _SveMaskWrapper<_Bits, _Np>& __y)
      {
	return svand_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data, __y._M_data);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_bit_or(const _SveMaskWrapper<_Bits, _Np>& __x, const _SveMaskWrapper<_Bits, _Np>& __y)
      {
	return svorr_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data, __y._M_data);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static constexpr _SveMaskWrapper<_Bits, _Np>
      _S_bit_xor(const _SveMaskWrapper<_Bits, _Np>& __x, const _SveMaskWrapper<_Bits, _Np>& __y)
      {
	return sveor_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
		       __x._M_data, __y._M_data);
      }

    template <size_t _Bits, size_t _Np>
      static constexpr void
      _S_set(_SveMaskWrapper<_Bits, _Np>& __k, int __i, bool __x) noexcept
      {
	auto __index = svcmpeq(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
			       __sve_mask_type<_Bits>::__index0123,
			       typename __sve_mask_type<_Bits>::__sve_mask_uint_type(__i));
	if (__x)
	  __k._M_data = svorr_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
				__k._M_data, __index);
	else
	  __k._M_data = svbic_z(_SveMaskWrapper<_Bits, _Np>::_BuiltinSveVectorType::__sve_active_mask(),
				__k._M_data, __index);
      }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static void
      _S_masked_assign(_SveMaskWrapper<_Bits, _Np> __k, _SveMaskWrapper<_Bits, _Np>& __lhs,
		       _SveMaskWrapper<_Bits, _Np> __rhs)
      { __lhs._M_data = svsel(__k._M_data, __rhs._M_data, __lhs._M_data); }

    template <size_t _Bits, size_t _Np>
      _GLIBCXX_SIMD_INTRINSIC static void
      _S_masked_assign(_SveMaskWrapper<_Bits, _Np> __k, _SveMaskWrapper<_Bits, _Np>& __lhs,
		       bool __rhs)
      {
	__lhs._M_data
	     = svsel(__k._M_data, _S_broadcast<__int_with_sizeof_t<_Bits>>(__rhs), __lhs._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static int
      _S_popcount(simd_mask<_Tp, _Abi> __k)
      {
	constexpr size_t _Np = simd_size_v<_Tp, _Abi>;

	return __sve_mask_type<sizeof(_Tp)>::__sve_mask_active_count(
		 __sve_vector_type<_Tp, _Np>::__sve_active_mask(), __k._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static bool
      _S_all_of(simd_mask<_Tp, _Abi> __k)
      { return _S_popcount(__k) == simd_size_v<_Tp, _Abi>; }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static bool
      _S_any_of(simd_mask<_Tp, _Abi> __k)
      {
	return svptest_any(__sve_vector_type<_Tp, simd_size_v<_Tp, _Abi>>::__sve_active_mask(),
			   __k._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static bool
      _S_none_of(simd_mask<_Tp, _Abi> __k)
      {
	return !svptest_any(__sve_vector_type<_Tp, simd_size_v<_Tp, _Abi>>::__sve_active_mask(),
			    __k._M_data);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static bool
      _S_some_of(simd_mask<_Tp, _Abi> __k)
      {
	int __msk_count = _S_popcount(__k);
	return (__msk_count > 0) && (__msk_count < (int) simd_size_v<_Tp, _Abi>);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static int
      _S_find_first_set(simd_mask<_Tp, _Abi> __k)
      {
	return svclastb(svpfirst(__k._M_data, svpfalse()),
			-1, __sve_mask_type<sizeof(_Tp)>::__index0123);
      }

    template <typename _Tp>
      _GLIBCXX_SIMD_INTRINSIC static int
      _S_find_last_set(simd_mask<_Tp, _Abi> __k)
      { return svclastb(__k._M_data, -1, __sve_mask_type<sizeof(_Tp)>::__index0123); }
  };

_GLIBCXX_SIMD_END_NAMESPACE
#endif // __cplusplus >= 201703L
#endif // _GLIBCXX_EXPERIMENTAL_SIMD_SVE_H_
// vim: sw=2 noet ts=8 sts=2 tw=100
