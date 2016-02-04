/* Copyright (C) 2007-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef _BID_CONF_H
#define _BID_CONF_H

// Name Changes

#define _IDEC_glbflags __bid_IDEC_glbflags
#define _IDEC_glbround __bid_IDEC_glbround
#define _IDEC_glbexcepthandling __bid_IDEC_glbexcepthandling
#define _IDEC_glbexceptionmasks __bid_IDEC_glbexceptionmasks
#define bid64_add __bid64_add
#define bid64_sub __bid64_sub
#define bid64_mul __bid64_mul
#define bid64_div __bid64_div
#define bid64dq_div __bid64dq_div
#define bid64qd_div __bid64qd_div
#define bid64qq_div __bid64qq_div
#define bid64q_sqrt __bid64q_sqrt
#define bid64_sqrt __bid64_sqrt
#define bid64_rem __bid64_rem
#define bid64_fma __bid64_fma
#define bid64_scalb __bid64_scalb
#define round128_19_38 __bid_round128_19_38
#define round192_39_57 __bid_round192_39_57
#define round256_58_76 __bid_round256_58_76
#define round64_2_18 __bid_round64_2_18
#define bid64_nextafter __bid64_nextafter
#define bid64_nextdown __bid64_nextdown
#define bid64_nextup __bid64_nextup
#define b2d __bid_b2d
#define b2d2 __bid_b2d2
#define b2d3 __bid_b2d3
#define b2d4 __bid_b2d4
#define b2d5 __bid_b2d5
#define bid128_canonize __bid128_canonize
#define bid32_canonize __bid32_canonize
#define bid64_canonize __bid64_canonize
#define bid_to_bid128 __bid_to_bid128
#define bid_to_bid32 __bid_to_bid32
#define bid_to_bid64 __bid_to_bid64
#define bid_to_dpd128 __bid_to_dpd128
#define bid_to_dpd32 __bid_to_dpd32
#define bid_to_dpd64 __bid_to_dpd64
#define d2b __bid_d2b
#define d2b2 __bid_d2b2
#define d2b3 __bid_d2b3
#define d2b4 __bid_d2b4
#define d2b5 __bid_d2b5
#define d2b6 __bid_d2b6
#define dpd_to_bid128 __bid_dpd_to_bid128
#define dpd_to_bid32 __bid_dpd_to_bid32
#define dpd_to_bid64 __bid_dpd_to_bid64
#define bid128_nextafter __bid128_nextafter
#define bid128_nextdown __bid128_nextdown
#define bid128_nextup __bid128_nextup
#define bid64_logb __bid64_logb
#define bid64_quantize __bid64_quantize
#define estimate_bin_expon __bid_estimate_bin_expon
#define estimate_decimal_digits __bid_estimate_decimal_digits
#define power10_index_binexp __bid_power10_index_binexp
#define power10_index_binexp_128 __bid_power10_index_binexp_128
#define power10_table_128 __bid_power10_table_128
#define reciprocals10_128 __bid_reciprocals10_128
#define reciprocals10_64 __bid_reciprocals10_64
#define recip_scale __bid_recip_scale
#define round_const_table __bid_round_const_table
#define round_const_table_128 __bid_round_const_table_128
#define short_recip_scale __bid_short_recip_scale
#define bid64_from_string __bid64_from_string
#define bid64_to_string __bid64_to_string
#define Inv_Tento9 __bid_Inv_Tento9
#define midi_tbl __bid_midi_tbl
#define Tento3 __bid_Tento3
#define Tento6 __bid_Tento6
#define Tento9 __bid_Tento9
#define Twoto30_m_10to9 __bid_Twoto30_m_10to9
#define Twoto60 __bid_Twoto60
#define Twoto60_m_10to18 __bid_Twoto60_m_10to18
#define convert_table __bid_convert_table
#define factors __bid_factors
#define packed_10000_zeros __bid_packed_10000_zeros
#define char_table2 __bid_char_table2
#define char_table3 __bid_char_table3
#define Ex128m128 __bid_Ex128m128
#define Ex192m192 __bid_Ex192m192
#define Ex256m256 __bid_Ex256m256
#define Ex64m64 __bid_Ex64m64
#define half128 __bid_half128
#define half192 __bid_half192
#define half256 __bid_half256
#define half64 __bid_half64
#define Kx128 __bid_Kx128
#define Kx192 __bid_Kx192
#define Kx256 __bid_Kx256
#define Kx64 __bid_Kx64
#define mask128 __bid_mask128
#define mask192 __bid_mask192
#define mask256 __bid_mask256
#define mask64 __bid_mask64
#define maskhigh128 __bid_maskhigh128
#define maskhigh128M __bid_maskhigh128M
#define maskhigh192M __bid_maskhigh192M
#define maskhigh256M __bid_maskhigh256M
#define midpoint128 __bid_midpoint128
#define midpoint192 __bid_midpoint192
#define midpoint256 __bid_midpoint256
#define midpoint64 __bid_midpoint64
#define nr_digits __bid_nr_digits
#define onehalf128 __bid_onehalf128
#define onehalf128M __bid_onehalf128M
#define onehalf192M __bid_onehalf192M
#define onehalf256M __bid_onehalf256M
#define shiftright128 __bid_shiftright128
#define shiftright128M __bid_shiftright128M
#define shiftright192M __bid_shiftright192M
#define shiftright256M __bid_shiftright256M
#define shift_ten2m3k128 __bid_shift_ten2m3k128
#define shift_ten2m3k64 __bid_shift_ten2m3k64
#define ten2k128 __bid_ten2k128
#define ten2k256 __bid_ten2k256
#define ten2k64 __bid_ten2k64
#define ten2m3k128 __bid_ten2m3k128
#define ten2m3k64 __bid_ten2m3k64
#define ten2mk128 __bid_ten2mk128
#define ten2mk128M __bid_ten2mk128M
#define ten2mk128trunc __bid_ten2mk128trunc
#define ten2mk128truncM __bid_ten2mk128truncM
#define ten2mk192M __bid_ten2mk192M
#define ten2mk192truncM __bid_ten2mk192truncM
#define ten2mk256M __bid_ten2mk256M
#define ten2mk256truncM __bid_ten2mk256truncM
#define ten2mk64 __bid_ten2mk64
#define ten2mxtrunc128 __bid_ten2mxtrunc128
#define ten2mxtrunc192 __bid_ten2mxtrunc192
#define ten2mxtrunc256 __bid_ten2mxtrunc256
#define ten2mxtrunc64 __bid_ten2mxtrunc64
#define bid128_add __bid128_add
#define bid128dd_add __bid128dd_add
#define bid128dd_sub __bid128dd_sub
#define bid128dq_add __bid128dq_add
#define bid128dq_sub __bid128dq_sub
#define bid128qd_add __bid128qd_add
#define bid128qd_sub __bid128qd_sub
#define bid128_sub __bid128_sub
#define bid64dq_add __bid64dq_add
#define bid64dq_sub __bid64dq_sub
#define bid64qd_add __bid64qd_add
#define bid64qd_sub __bid64qd_sub
#define bid64qq_add __bid64qq_add
#define bid64qq_sub __bid64qq_sub
#define bid128dd_mul __bid128dd_mul
#define bid128dq_mul __bid128dq_mul
#define bid128_mul __bid128_mul
#define bid128qd_mul __bid128qd_mul
#define bid64dq_mul __bid64dq_mul
#define bid64qd_mul __bid64qd_mul
#define bid64qq_mul __bid64qq_mul
#define bid128dd_div __bid128dd_div
#define bid128_div __bid128_div
#define bid128dq_div __bid128dq_div
#define bid128qd_div __bid128qd_div
#define bid128d_sqrt __bid128d_sqrt
#define bid128_sqrt __bid128_sqrt
#define bid128ddd_fma __bid128ddd_fma
#define bid128ddq_fma __bid128ddq_fma
#define bid128dqd_fma __bid128dqd_fma
#define bid128dqq_fma __bid128dqq_fma
#define bid128_fma __bid128_fma
#define bid128qdd_fma __bid128qdd_fma
#define bid128qdq_fma __bid128qdq_fma
#define bid128qqd_fma __bid128qqd_fma
#define bid64ddq_fma __bid64ddq_fma
#define bid64dqd_fma __bid64dqd_fma
#define bid64dqq_fma __bid64dqq_fma
#define bid64qdd_fma __bid64qdd_fma
#define bid64qdq_fma __bid64qdq_fma
#define bid64qqd_fma __bid64qqd_fma
#define bid64qqq_fma __bid64qqq_fma
#define bid128_round_integral_exact __bid128_round_integral_exact
#define bid128_round_integral_nearest_away __bid128_round_integral_nearest_away
#define bid128_round_integral_nearest_even __bid128_round_integral_nearest_even
#define bid128_round_integral_negative __bid128_round_integral_negative
#define bid128_round_integral_positive __bid128_round_integral_positive
#define bid128_round_integral_zero __bid128_round_integral_zero
#define bid64_round_integral_exact __bid64_round_integral_exact
#define bid64_round_integral_nearest_away __bid64_round_integral_nearest_away
#define bid64_round_integral_nearest_even __bid64_round_integral_nearest_even
#define bid64_round_integral_negative __bid64_round_integral_negative
#define bid64_round_integral_positive __bid64_round_integral_positive
#define bid64_round_integral_zero __bid64_round_integral_zero
#define bid128_quantize __bid128_quantize
#define bid128_scalb __bid128_scalb
#define bid64_maxnum __bid64_maxnum
#define bid64_maxnum_mag __bid64_maxnum_mag
#define bid64_minnum __bid64_minnum
#define bid64_minnum_mag __bid64_minnum_mag
#define bid128_maxnum __bid128_maxnum
#define bid128_maxnum_mag __bid128_maxnum_mag
#define bid128_minnum __bid128_minnum
#define bid128_minnum_mag __bid128_minnum_mag
#define bid128_rem __bid128_rem
#define bid128_logb __bid128_logb
#define getDecimalRoundingDirection __bid_getDecimalRoundingDirection
#define is754 __bid_is754
#define is754R __bid_is754R
#define signalException __bid_signalException
#define lowerFlags __bid_lowerFlags
#define restoreFlags __bid_restoreFlags
#define saveFlags __bid_saveFlags
#define setDecimalRoundingDirection __bid_setDecimalRoundingDirection
#define testFlags __bid_testFlags
#define testSavedFlags __bid_testSavedFlags
#define bid32_to_bid64 __bid32_to_bid64
#define bid64_to_bid32 __bid64_to_bid32
#define bid128_to_string __bid128_to_string
#define mod10_18_tbl __bid_mod10_18_tbl
#define bid128_to_bid32 __bid128_to_bid32
#define bid32_to_bid128 __bid32_to_bid128
#define bid128_to_bid64 __bid128_to_bid64
#define bid64_to_bid128 __bid64_to_bid128
#define bid128_from_string __bid128_from_string
#define bid128_from_int32 __bid128_from_int32
#define bid128_from_int64 __bid128_from_int64
#define bid128_from_uint32 __bid128_from_uint32
#define bid128_from_uint64 __bid128_from_uint64
#define bid64_from_int32 __bid64_from_int32
#define bid64_from_int64 __bid64_from_int64
#define bid64_from_uint32 __bid64_from_uint32
#define bid64_from_uint64 __bid64_from_uint64
#define bid64_abs __bid64_abs
#define bid64_class __bid64_class
#define bid64_copy __bid64_copy
#define bid64_copySign __bid64_copySign
#define bid64_isCanonical __bid64_isCanonical
#define bid64_isFinite __bid64_isFinite
#define bid64_isInf __bid64_isInf
#define bid64_isNaN __bid64_isNaN
#define bid64_isNormal __bid64_isNormal
#define bid64_isSignaling __bid64_isSignaling
#define bid64_isSigned __bid64_isSigned
#define bid64_isSubnormal __bid64_isSubnormal
#define bid64_isZero __bid64_isZero
#define bid64_negate __bid64_negate
#define bid64_radix __bid64_radix
#define bid64_sameQuantum __bid64_sameQuantum
#define bid64_totalOrder __bid64_totalOrder
#define bid64_totalOrderMag __bid64_totalOrderMag
#define bid128_abs __bid128_abs
#define bid128_class __bid128_class
#define bid128_copy __bid128_copy
#define bid128_copySign __bid128_copySign
#define bid128_isCanonical __bid128_isCanonical
#define bid128_isFinite __bid128_isFinite
#define bid128_isInf __bid128_isInf
#define bid128_isNaN __bid128_isNaN
#define bid128_isNormal __bid128_isNormal
#define bid128_isSignaling __bid128_isSignaling
#define bid128_isSigned __bid128_isSigned
#define bid128_isSubnormal __bid128_isSubnormal
#define bid128_isZero __bid128_isZero
#define bid128_negate __bid128_negate
#define bid128_radix __bid128_radix
#define bid128_sameQuantum __bid128_sameQuantum
#define bid128_totalOrder __bid128_totalOrder
#define bid128_totalOrderMag __bid128_totalOrderMag
#define bid64_quiet_equal __bid64_quiet_equal
#define bid64_quiet_greater __bid64_quiet_greater
#define bid64_quiet_greater_equal __bid64_quiet_greater_equal
#define bid64_quiet_greater_unordered __bid64_quiet_greater_unordered
#define bid64_quiet_less __bid64_quiet_less
#define bid64_quiet_less_equal __bid64_quiet_less_equal
#define bid64_quiet_less_unordered __bid64_quiet_less_unordered
#define bid64_quiet_not_equal __bid64_quiet_not_equal
#define bid64_quiet_not_greater __bid64_quiet_not_greater
#define bid64_quiet_not_less __bid64_quiet_not_less
#define bid64_quiet_ordered __bid64_quiet_ordered
#define bid64_quiet_unordered __bid64_quiet_unordered
#define bid64_signaling_greater __bid64_signaling_greater
#define bid64_signaling_greater_equal __bid64_signaling_greater_equal
#define bid64_signaling_greater_unordered __bid64_signaling_greater_unordered
#define bid64_signaling_less __bid64_signaling_less
#define bid64_signaling_less_equal __bid64_signaling_less_equal
#define bid64_signaling_less_unordered __bid64_signaling_less_unordered
#define bid64_signaling_not_greater __bid64_signaling_not_greater
#define bid64_signaling_not_less __bid64_signaling_not_less
#define bid128_quiet_equal __bid128_quiet_equal
#define bid128_quiet_greater __bid128_quiet_greater
#define bid128_quiet_greater_equal __bid128_quiet_greater_equal
#define bid128_quiet_greater_unordered __bid128_quiet_greater_unordered
#define bid128_quiet_less __bid128_quiet_less
#define bid128_quiet_less_equal __bid128_quiet_less_equal
#define bid128_quiet_less_unordered __bid128_quiet_less_unordered
#define bid128_quiet_not_equal __bid128_quiet_not_equal
#define bid128_quiet_not_greater __bid128_quiet_not_greater
#define bid128_quiet_not_less __bid128_quiet_not_less
#define bid128_quiet_ordered __bid128_quiet_ordered
#define bid128_quiet_unordered __bid128_quiet_unordered
#define bid128_signaling_greater __bid128_signaling_greater
#define bid128_signaling_greater_equal __bid128_signaling_greater_equal
#define bid128_signaling_greater_unordered __bid128_signaling_greater_unordered
#define bid128_signaling_less __bid128_signaling_less
#define bid128_signaling_less_equal __bid128_signaling_less_equal
#define bid128_signaling_less_unordered __bid128_signaling_less_unordered
#define bid128_signaling_not_greater __bid128_signaling_not_greater
#define bid128_signaling_not_less __bid128_signaling_not_less
#define bid64_to_int32_ceil __bid64_to_int32_ceil
#define bid64_to_int32_floor __bid64_to_int32_floor
#define bid64_to_int32_int __bid64_to_int32_int
#define bid64_to_int32_rnint __bid64_to_int32_rnint
#define bid64_to_int32_rninta __bid64_to_int32_rninta
#define bid64_to_int32_xceil __bid64_to_int32_xceil
#define bid64_to_int32_xfloor __bid64_to_int32_xfloor
#define bid64_to_int32_xint __bid64_to_int32_xint
#define bid64_to_int32_xrnint __bid64_to_int32_xrnint
#define bid64_to_int32_xrninta __bid64_to_int32_xrninta
#define bid64_to_uint32_ceil __bid64_to_uint32_ceil
#define bid64_to_uint32_floor __bid64_to_uint32_floor
#define bid64_to_uint32_int __bid64_to_uint32_int
#define bid64_to_uint32_rnint __bid64_to_uint32_rnint
#define bid64_to_uint32_rninta __bid64_to_uint32_rninta
#define bid64_to_uint32_xceil __bid64_to_uint32_xceil
#define bid64_to_uint32_xfloor __bid64_to_uint32_xfloor
#define bid64_to_uint32_xint __bid64_to_uint32_xint
#define bid64_to_uint32_xrnint __bid64_to_uint32_xrnint
#define bid64_to_uint32_xrninta __bid64_to_uint32_xrninta
#define bid64_to_int64_ceil __bid64_to_int64_ceil
#define bid64_to_int64_floor __bid64_to_int64_floor
#define bid64_to_int64_int __bid64_to_int64_int
#define bid64_to_int64_rnint __bid64_to_int64_rnint
#define bid64_to_int64_rninta __bid64_to_int64_rninta
#define bid64_to_int64_xceil __bid64_to_int64_xceil
#define bid64_to_int64_xfloor __bid64_to_int64_xfloor
#define bid64_to_int64_xint __bid64_to_int64_xint
#define bid64_to_int64_xrnint __bid64_to_int64_xrnint
#define bid64_to_int64_xrninta __bid64_to_int64_xrninta
#define bid64_to_uint64_ceil __bid64_to_uint64_ceil
#define bid64_to_uint64_floor __bid64_to_uint64_floor
#define bid64_to_uint64_int __bid64_to_uint64_int
#define bid64_to_uint64_rnint __bid64_to_uint64_rnint
#define bid64_to_uint64_rninta __bid64_to_uint64_rninta
#define bid64_to_uint64_xceil __bid64_to_uint64_xceil
#define bid64_to_uint64_xfloor __bid64_to_uint64_xfloor
#define bid64_to_uint64_xint __bid64_to_uint64_xint
#define bid64_to_uint64_xrnint __bid64_to_uint64_xrnint
#define bid64_to_uint64_xrninta __bid64_to_uint64_xrninta
#define bid128_to_int32_ceil __bid128_to_int32_ceil
#define bid128_to_int32_floor __bid128_to_int32_floor
#define bid128_to_int32_int __bid128_to_int32_int
#define bid128_to_int32_rnint __bid128_to_int32_rnint
#define bid128_to_int32_rninta __bid128_to_int32_rninta
#define bid128_to_int32_xceil __bid128_to_int32_xceil
#define bid128_to_int32_xfloor __bid128_to_int32_xfloor
#define bid128_to_int32_xint __bid128_to_int32_xint
#define bid128_to_int32_xrnint __bid128_to_int32_xrnint
#define bid128_to_int32_xrninta __bid128_to_int32_xrninta
#define bid128_to_uint32_ceil __bid128_to_uint32_ceil
#define bid128_to_uint32_floor __bid128_to_uint32_floor
#define bid128_to_uint32_int __bid128_to_uint32_int
#define bid128_to_uint32_rnint __bid128_to_uint32_rnint
#define bid128_to_uint32_rninta __bid128_to_uint32_rninta
#define bid128_to_uint32_xceil __bid128_to_uint32_xceil
#define bid128_to_uint32_xfloor __bid128_to_uint32_xfloor
#define bid128_to_uint32_xint __bid128_to_uint32_xint
#define bid128_to_uint32_xrnint __bid128_to_uint32_xrnint
#define bid128_to_uint32_xrninta __bid128_to_uint32_xrninta
#define bid128_to_int64_ceil __bid128_to_int64_ceil
#define bid128_to_int64_floor __bid128_to_int64_floor
#define bid128_to_int64_int __bid128_to_int64_int
#define bid128_to_int64_rnint __bid128_to_int64_rnint
#define bid128_to_int64_rninta __bid128_to_int64_rninta
#define bid128_to_int64_xceil __bid128_to_int64_xceil
#define bid128_to_int64_xfloor __bid128_to_int64_xfloor
#define bid128_to_int64_xint __bid128_to_int64_xint
#define bid128_to_int64_xrnint __bid128_to_int64_xrnint
#define bid128_to_int64_xrninta __bid128_to_int64_xrninta
#define bid128_to_uint64_ceil __bid128_to_uint64_ceil
#define bid128_to_uint64_floor __bid128_to_uint64_floor
#define bid128_to_uint64_int __bid128_to_uint64_int
#define bid128_to_uint64_rnint __bid128_to_uint64_rnint
#define bid128_to_uint64_rninta __bid128_to_uint64_rninta
#define bid128_to_uint64_xceil __bid128_to_uint64_xceil
#define bid128_to_uint64_xfloor __bid128_to_uint64_xfloor
#define bid128_to_uint64_xint __bid128_to_uint64_xint
#define bid128_to_uint64_xrnint __bid128_to_uint64_xrnint
#define bid128_to_uint64_xrninta __bid128_to_uint64_xrninta
#define bid128_to_binary128 __bid128_to_binary128
#define bid128_to_binary32 __bid128_to_binary32
#define bid128_to_binary64 __bid128_to_binary64
#define bid128_to_binary80 __bid128_to_binary80
#define bid32_to_binary128 __bid32_to_binary128
#define bid32_to_binary32 __bid32_to_binary32
#define bid32_to_binary64 __bid32_to_binary64
#define bid32_to_binary80 __bid32_to_binary80
#define bid64_to_binary128 __bid64_to_binary128
#define bid64_to_binary32 __bid64_to_binary32
#define bid64_to_binary64 __bid64_to_binary64
#define bid64_to_binary80 __bid64_to_binary80
#define binary128_to_bid128 __binary128_to_bid128
#define binary128_to_bid32 __binary128_to_bid32
#define binary128_to_bid64 __binary128_to_bid64
#define binary32_to_bid128 __binary32_to_bid128
#define binary32_to_bid32 __binary32_to_bid32
#define binary32_to_bid64 __binary32_to_bid64
#define binary64_to_bid128 __binary64_to_bid128
#define binary64_to_bid32 __binary64_to_bid32
#define binary64_to_bid64 __binary64_to_bid64
#define binary80_to_bid128 __binary80_to_bid128
#define binary80_to_bid32 __binary80_to_bid32
#define binary80_to_bid64 __binary80_to_bid64
#define bid64_to_uint16_ceil __bid64_to_uint16_ceil
#define bid64_to_uint16_floor __bid64_to_uint16_floor
#define bid64_to_uint16_int __bid64_to_uint16_int
#define bid64_to_uint16_rnint __bid64_to_uint16_rnint
#define bid64_to_uint16_rninta __bid64_to_uint16_rninta
#define bid64_to_uint16_xceil __bid64_to_uint16_xceil
#define bid64_to_uint16_xfloor __bid64_to_uint16_xfloor
#define bid64_to_uint16_xint __bid64_to_uint16_xint
#define bid64_to_uint16_xrnint __bid64_to_uint16_xrnint
#define bid64_to_uint16_xrninta __bid64_to_uint16_xrninta
#define bid64_to_int16_ceil __bid64_to_int16_ceil
#define bid64_to_int16_floor __bid64_to_int16_floor
#define bid64_to_int16_int __bid64_to_int16_int
#define bid64_to_int16_rnint __bid64_to_int16_rnint
#define bid64_to_int16_rninta __bid64_to_int16_rninta
#define bid64_to_int16_xceil __bid64_to_int16_xceil
#define bid64_to_int16_xfloor __bid64_to_int16_xfloor
#define bid64_to_int16_xint __bid64_to_int16_xint
#define bid64_to_int16_xrnint __bid64_to_int16_xrnint
#define bid64_to_int16_xrninta __bid64_to_int16_xrninta
#define bid128_to_uint16_ceil __bid128_to_uint16_ceil
#define bid128_to_uint16_floor __bid128_to_uint16_floor
#define bid128_to_uint16_int __bid128_to_uint16_int
#define bid128_to_uint16_rnint __bid128_to_uint16_rnint
#define bid128_to_uint16_rninta __bid128_to_uint16_rninta
#define bid128_to_uint16_xceil __bid128_to_uint16_xceil
#define bid128_to_uint16_xfloor __bid128_to_uint16_xfloor
#define bid128_to_uint16_xint __bid128_to_uint16_xint
#define bid128_to_uint16_xrnint __bid128_to_uint16_xrnint
#define bid128_to_uint16_xrninta __bid128_to_uint16_xrninta
#define bid128_to_int16_ceil __bid128_to_int16_ceil
#define bid128_to_int16_floor __bid128_to_int16_floor
#define bid128_to_int16_int __bid128_to_int16_int
#define bid128_to_int16_rnint __bid128_to_int16_rnint
#define bid128_to_int16_rninta __bid128_to_int16_rninta
#define bid128_to_int16_xceil __bid128_to_int16_xceil
#define bid128_to_int16_xfloor __bid128_to_int16_xfloor
#define bid128_to_int16_xint __bid128_to_int16_xint
#define bid128_to_int16_xrnint __bid128_to_int16_xrnint
#define bid128_to_int16_xrninta __bid128_to_int16_xrninta
#define bid64_to_uint8_ceil __bid64_to_uint8_ceil
#define bid64_to_uint8_floor __bid64_to_uint8_floor
#define bid64_to_uint8_int __bid64_to_uint8_int
#define bid64_to_uint8_rnint __bid64_to_uint8_rnint
#define bid64_to_uint8_rninta __bid64_to_uint8_rninta
#define bid64_to_uint8_xceil __bid64_to_uint8_xceil
#define bid64_to_uint8_xfloor __bid64_to_uint8_xfloor
#define bid64_to_uint8_xint __bid64_to_uint8_xint
#define bid64_to_uint8_xrnint __bid64_to_uint8_xrnint
#define bid64_to_uint8_xrninta __bid64_to_uint8_xrninta
#define bid64_to_int8_ceil __bid64_to_int8_ceil
#define bid64_to_int8_floor __bid64_to_int8_floor
#define bid64_to_int8_int __bid64_to_int8_int
#define bid64_to_int8_rnint __bid64_to_int8_rnint
#define bid64_to_int8_rninta __bid64_to_int8_rninta
#define bid64_to_int8_xceil __bid64_to_int8_xceil
#define bid64_to_int8_xfloor __bid64_to_int8_xfloor
#define bid64_to_int8_xint __bid64_to_int8_xint
#define bid64_to_int8_xrnint __bid64_to_int8_xrnint
#define bid64_to_int8_xrninta __bid64_to_int8_xrninta
#define bid128_to_uint8_ceil __bid128_to_uint8_ceil
#define bid128_to_uint8_floor __bid128_to_uint8_floor
#define bid128_to_uint8_int __bid128_to_uint8_int
#define bid128_to_uint8_rnint __bid128_to_uint8_rnint
#define bid128_to_uint8_rninta __bid128_to_uint8_rninta
#define bid128_to_uint8_xceil __bid128_to_uint8_xceil
#define bid128_to_uint8_xfloor __bid128_to_uint8_xfloor
#define bid128_to_uint8_xint __bid128_to_uint8_xint
#define bid128_to_uint8_xrnint __bid128_to_uint8_xrnint
#define bid128_to_uint8_xrninta __bid128_to_uint8_xrninta
#define bid128_to_int8_ceil __bid128_to_int8_ceil
#define bid128_to_int8_floor __bid128_to_int8_floor
#define bid128_to_int8_int __bid128_to_int8_int
#define bid128_to_int8_rnint __bid128_to_int8_rnint
#define bid128_to_int8_rninta __bid128_to_int8_rninta
#define bid128_to_int8_xceil __bid128_to_int8_xceil
#define bid128_to_int8_xfloor __bid128_to_int8_xfloor
#define bid128_to_int8_xint __bid128_to_int8_xint
#define bid128_to_int8_xrnint __bid128_to_int8_xrnint
#define bid128_to_int8_xrninta __bid128_to_int8_xrninta

#ifdef IN_LIBGCC2
#if !defined ENABLE_DECIMAL_BID_FORMAT || !ENABLE_DECIMAL_BID_FORMAT
#error BID not enabled in libbid
#endif

#ifndef BID_BIG_ENDIAN
#define BID_BIG_ENDIAN __FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__
#endif

#ifndef BID_THREAD
#if defined (HAVE_CC_TLS) && defined (USE_TLS)
#define BID_THREAD __thread
#endif
#endif

#define _intptr_t_defined
#define DECIMAL_CALL_BY_REFERENCE 0
#define DECIMAL_GLOBAL_ROUNDING 1
#define DECIMAL_GLOBAL_ROUNDING_ACCESS_FUNCTIONS 1
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS 1
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS_ACCESS_FUNCTIONS 1
#define BID_HAS_GCC_DECIMAL_INTRINSICS 1
#endif /* IN_LIBGCC2 */

// Configuration Options

#define SET_STATUS_FLAGS

#ifndef BID_THREAD
#define BID_THREAD
#endif

#ifndef BID_HAS_GCC_DECIMAL_INTRINSICS
#define BID_HAS_GCC_DECIMAL_INTRINSICS 0
#endif

#if !defined(WINDOWS) || defined(__INTEL_COMPILER)
// #define UNCHANGED_BINARY_STATUS_FLAGS
#endif
// #define HPUX_OS

// If DECIMAL_CALL_BY_REFERENCE is defined then numerical arguments and results
// are passed by reference otherwise they are passed by value (except that
// a pointer is always passed to the status flags)

#ifndef DECIMAL_CALL_BY_REFERENCE
#define DECIMAL_CALL_BY_REFERENCE 0
#endif

// If DECIMAL_GLOBAL_ROUNDING is defined then the rounding mode is a global 
// variable _IDEC_glbround, otherwise it is passed as a parameter when needed

#ifndef DECIMAL_GLOBAL_ROUNDING
#define DECIMAL_GLOBAL_ROUNDING 0
#endif

#ifndef DECIMAL_GLOBAL_ROUNDING_ACCESS_FUNCTIONS
#define DECIMAL_GLOBAL_ROUNDING_ACCESS_FUNCTIONS 0
#endif

// If DECIMAL_GLOBAL_EXCEPTION_FLAGS is defined then the exception status flags
// are represented by a global variable _IDEC_glbflags, otherwise they are 
// passed as a parameter when needed

#ifndef DECIMAL_GLOBAL_EXCEPTION_FLAGS
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS 0
#endif

#ifndef DECIMAL_GLOBAL_EXCEPTION_FLAGS_ACCESS_FUNCTIONS
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS_ACCESS_FUNCTIONS 0
#endif

// If DECIMAL_ALTERNATE_EXCEPTION_HANDLING is defined then the exception masks
// are examined and exception handling information is provided to the caller 
// if alternate exception handling is necessary

#ifndef DECIMAL_ALTERNATE_EXCEPTION_HANDLING
#define DECIMAL_ALTERNATE_EXCEPTION_HANDLING 0
#endif

typedef unsigned int _IDEC_round;
typedef unsigned int _IDEC_flags;	// could be a struct with diagnostic info

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING
  // If DECIMAL_GLOBAL_EXCEPTION_MASKS is defined then the exception mask bits
  // are represented by a global variable _IDEC_exceptionmasks, otherwise they
  // are passed as a parameter when needed; DECIMAL_GLOBAL_EXCEPTION_MASKS is 
  // ignored
  // if DECIMAL_ALTERNATE_EXCEPTION_HANDLING is not defined
  // **************************************************************************
#define DECIMAL_GLOBAL_EXCEPTION_MASKS 0
  // **************************************************************************

  // If DECIMAL_GLOBAL_EXCEPTION_INFO is defined then the alternate exception  
  // handling information is represented by a global data structure 
  // _IDEC_glbexcepthandling, otherwise it is passed by reference as a
  // parameter when needed; DECIMAL_GLOBAL_EXCEPTION_INFO is ignored
  // if DECIMAL_ALTERNATE_EXCEPTION_HANDLING is not defined
  // **************************************************************************
#define DECIMAL_GLOBAL_EXCEPTION_INFO 0
  // **************************************************************************
#endif

// Notes: 1) rnd_mode from _RND_MODE_ARG is used by the caller of a function
//           from this library, and can be any name
//        2) rnd_mode and prnd_mode from _RND_MODE_PARAM are fixed names 
//           and *must* be used in the library functions
//        3) _IDEC_glbround is the fixed name for the global variable holding 
//           the rounding mode

#if !DECIMAL_GLOBAL_ROUNDING
#if DECIMAL_CALL_BY_REFERENCE
#define _RND_MODE_ARG , &rnd_mode
#define _RND_MODE_PARAM , _IDEC_round *prnd_mode
#define _RND_MODE_ARG_ALONE &rnd_mode
#define _RND_MODE_PARAM_ALONE _IDEC_round *prnd_mode
#else
#define _RND_MODE_ARG , rnd_mode
#define _RND_MODE_PARAM , _IDEC_round rnd_mode
#define _RND_MODE_ARG_ALONE rnd_mode
#define _RND_MODE_PARAM_ALONE _IDEC_round rnd_mode
#endif
#else
#define _RND_MODE_ARG
#define _RND_MODE_PARAM
#define _RND_MODE_ARG_ALONE
#define _RND_MODE_PARAM_ALONE
#define rnd_mode _IDEC_glbround
#endif

// Notes: 1) pfpsf from _EXC_FLAGS_ARG is used by the caller of a function
//           from this library, and can be any name 
//        2) pfpsf from _EXC_FLAGS_PARAM is a fixed name and *must* be used  
//           in the library functions
//        3) _IDEC_glbflags is the fixed name for the global variable holding 
//           the floating-point status flags
#if !DECIMAL_GLOBAL_EXCEPTION_FLAGS
#define _EXC_FLAGS_ARG , pfpsf
#define _EXC_FLAGS_PARAM , _IDEC_flags *pfpsf
#else
#define _EXC_FLAGS_ARG
#define _EXC_FLAGS_PARAM
#define pfpsf &_IDEC_glbflags
#endif

#if DECIMAL_GLOBAL_ROUNDING
extern BID_THREAD _IDEC_round _IDEC_glbround;
#endif

#if DECIMAL_GLOBAL_EXCEPTION_FLAGS
extern BID_THREAD _IDEC_flags _IDEC_glbflags;
#endif

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING
#if DECIMAL_GLOBAL_EXCEPTION_MASKS
extern BID_THREAD _IDEC_exceptionmasks _IDEC_glbexceptionmasks;
#endif
#if DECIMAL_GLOBAL_EXCEPTION_INFO
extern BID_THREAD _IDEC_excepthandling _IDEC_glbexcepthandling;
#endif
#endif

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING

  // Notes: 1) exc_mask from _EXC_MASKS_ARG is used by the caller of a function
  //           from this library, and can be any name
  //        2) exc_mask and pexc_mask from _EXC_MASKS_PARAM are fixed names
  //           and *must* be used in the library functions
  //        3) _IDEC_glbexceptionmasks is the fixed name for the global 
  //           variable holding the floating-point exception masks
#if !DECIMAL_GLOBAL_EXCEPTION_MASKS
#if DECIMAL_CALL_BY_REFERENCE
#define _EXC_MASKS_ARG , &exc_mask
#define _EXC_MASKS_PARAM , _IDEC_exceptionmasks *pexc_mask
#else
#define _EXC_MASKS_ARG , exc_mask
#define _EXC_MASKS_PARAM , _IDEC_exceptionmasks exc_mask
#endif
#else
#define _EXC_MASKS_ARG
#define _EXC_MASKS_PARAM
#define exc_mask _IDEC_glbexceptionmasks
#endif

  // Notes: 1) pexc_info from _EXC_INFO_ARG is used by the caller of a function
  //           from this library, and can be any name
  //        2) pexc_info from _EXC_INFO_PARAM is a fixed name and *must* be  
  //           used in the library functions
  //        3) _IDEC_glbexcepthandling is the fixed name for the global  
  //           variable holding the floating-point exception information
#if !DECIMAL_GLOBAL_EXCEPTION_INFO
#define _EXC_INFO_ARG , pexc_info
#define _EXC_INFO_PARAM , _IDEC_excepthandling *pexc_info
#else
#define _EXC_INFO_ARG
#define _EXC_INFO_PARAM
#define pexc_info &_IDEC_glbexcepthandling
#endif
#else
#define _EXC_MASKS_ARG
#define _EXC_MASKS_PARAM
#define _EXC_INFO_ARG
#define _EXC_INFO_PARAM
#endif


#ifndef BID_BIG_ENDIAN
#define BID_BIG_ENDIAN 0
#endif

#if BID_BIG_ENDIAN
#define BID_SWAP128(x) {  \
  UINT64 sw;              \
  sw = (x).w[1];          \
  (x).w[1] = (x).w[0];    \
  (x).w[0] = sw;          \
  }
#else
#define BID_SWAP128(x)
#endif

#if DECIMAL_CALL_BY_REFERENCE
#define BID_RETURN_VAL(x) { *pres = (x); return; }
#if BID_BIG_ENDIAN && defined BID_128RES
#define BID_RETURN(x) { BID_SWAP128(x); *pres = (x); return; }
#else
#define BID_RETURN(x) { *pres = (x); return; }
#endif
#else
#define BID_RETURN_VAL(x) return(x);
#if BID_BIG_ENDIAN && defined BID_128RES
#define BID_RETURN(x) { BID_SWAP128(x); return(x); }
#else
#define BID_RETURN(x) return(x);
#endif
#endif

#if DECIMAL_CALL_BY_REFERENCE
#define BIDECIMAL_CALL1(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), &(_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESARG(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), (_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), &(_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOSTAT(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND_NOSTAT(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL3(_FUNC, _RES, _OP1, _OP2, _OP3) \
    _FUNC(&(_RES), &(_OP1), &(_OP2), &(_OP3) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_FLAGS_ARG )
#define BIDECIMAL_CALL1_NORND_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) )
#define BIDECIMAL_CALL2_NORND_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) )
#define BIDECIMAL_CALL1_NORND_NOMASK_NOINFO_RESVOID(_FUNC, _OP1) \
    _FUNC(&(_OP1) _EXC_FLAGS_ARG )
#define BIDECIMAL_CALL2_NORND_NOMASK_NOINFO_RESVOID(_FUNC, _OP1, _OP2) \
    _FUNC(&(_OP1), &(_OP2) _EXC_FLAGS_ARG )
#define BIDECIMAL_CALLV_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES) \
    _FUNC(&(_RES) _RND_MODE_ARG)
#define BIDECIMAL_CALL1_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _FUNC(&(_OP1) _RND_MODE_ARG)
#else
#define BIDECIMAL_CALL1(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), _OP1 _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESARG(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), _OP1 _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOSTAT(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND_NOSTAT(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL3(_FUNC, _RES, _OP1, _OP2, _OP3) \
    _RES = _FUNC((_OP1), (_OP2), (_OP3) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_FLAGS_ARG)
#define BIDECIMAL_CALL1_NORND_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) )
#define BIDECIMAL_CALL2_NORND_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) )
#define BIDECIMAL_CALL1_NORND_NOMASK_NOINFO_RESVOID(_FUNC, _OP1) \
    _FUNC((_OP1) _EXC_FLAGS_ARG)
#define BIDECIMAL_CALL2_NORND_NOMASK_NOINFO_RESVOID(_FUNC, _OP1, _OP2) \
    _FUNC((_OP1), (_OP2) _EXC_FLAGS_ARG)
#define BIDECIMAL_CALLV_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES) \
    _RES = _FUNC(_RND_MODE_ARG_ALONE)
#if !DECIMAL_GLOBAL_ROUNDING
#define BIDECIMAL_CALL1_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _RND_MODE_ARG)
#else
#define BIDECIMAL_CALL1_NOFLAGS_NOMASK_NOINFO(_FUNC, _RES, _OP1) \
    _FUNC((_OP1) _RND_MODE_ARG)
#endif
#endif

#if BID_BIG_ENDIAN
#define HIGH_128W 0
#define LOW_128W  1
#else
#define HIGH_128W 1
#define LOW_128W  0
#endif

#if BID_BIG_ENDIAN
#define COPY_ARG_REF(arg_name) \
       UINT128 arg_name={ pbid_##arg_name->w[1], pbid_##arg_name->w[0]};
#define COPY_ARG_VAL(arg_name) \
       UINT128 arg_name={ bid_##arg_name.w[1], bid_##arg_name.w[0]};
#else
#define COPY_ARG_REF(arg_name) \
       UINT128 arg_name=*pbid_##arg_name;
#define COPY_ARG_VAL(arg_name) \
       UINT128 arg_name= bid_##arg_name;
#endif

#define COPY_ARG_TYPE_REF(type, arg_name) \
       type arg_name=*pbid_##arg_name;
#define COPY_ARG_TYPE_VAL(type, arg_name) \
       type arg_name= bid_##arg_name;

#if !DECIMAL_GLOBAL_ROUNDING
#define SET_RND_MODE() \
  _IDEC_round rnd_mode = *prnd_mode;
#else
#define SET_RND_MODE()
#endif

#define PROLOG_REF(arg_name) \
       COPY_ARG_REF(arg_name)

#define PROLOG_VAL(arg_name) \
       COPY_ARG_VAL(arg_name)

#define PROLOG_TYPE_REF(type, arg_name) \
       COPY_ARG_TYPE_REF(type, arg_name)

#define PROLOG_TYPE_VAL(type, arg_name) \
      COPY_ARG_TYPE_VAL(type, arg_name)

#define OTHER_PROLOG_REF()
#define OTHER_PROLOG_VAL()

#if DECIMAL_CALL_BY_REFERENCE
#define       BID128_FUNCTION_ARG1(fn_name, arg_name)\
      void fn_name (UINT128 * pres, \
           UINT128 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARG1_NORND(fn_name, arg_name)\
      void fn_name (UINT128 * pres, \
           UINT128 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name)\
      void fn_name (restype * pres, \
           UINT128 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARG2(fn_name, arg_name1, arg_name2)\
      void fn_name (UINT128 * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARG2_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name1, arg_name2)\
      void fn_name (restype * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARG128_ARGTYPE2(fn_name, arg_name1, type2, arg_name2)\
      void fn_name (UINT128 * pres, \
           UINT128 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARGTYPE1_ARGTYPE2(type0, fn_name, type1, arg_name1, type2, arg_name2)\
      void fn_name (type0 *pres, \
           type1 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARGTYPE1_ARG128(fn_name, type1, arg_name1, arg_name2)\
      void fn_name (UINT128 * pres, \
           type1 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARG128_ARGTYPE2(type0, fn_name, arg_name1, type2, arg_name2)\
      void fn_name (type0 *pres, \
           UINT128 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARGTYPE1_ARG128(type0, fn_name, type1, arg_name1, arg_name2)\
      void fn_name (type0 *pres, \
           type1 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARG128_ARG128(type0, fn_name, arg_name1, arg_name2)\
      void fn_name (type0 * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARG1(type0, fn_name, arg_name)\
      void fn_name (type0 * pres, \
           UINT128 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       BID128_FUNCTION_ARGTYPE1(fn_name, type1, arg_name)\
      void fn_name (UINT128 * pres, \
           type1 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARGTYPE1(type0, fn_name, type1, arg_name)\
      void fn_name (type0 * pres, \
           type1 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()

#define       TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
      void fn_name (type0 * pres, \
           type1 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     OTHER_PROLOG_REF()

//////////////////////////////////////////
/////////////////////////////////////////
////////////////////////////////////////

#else

//////////////////////////////////////////
/////////////////////////////////////////
////////////////////////////////////////

#define       BID128_FUNCTION_ARG1(fn_name, arg_name)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARG1_NORND(fn_name, arg_name)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name)\
     restype                                     \
     fn_name (UINT128 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARG2(fn_name, arg_name1, arg_name2)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARG2_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name1, arg_name2)\
     restype                                    \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARG128_ARGTYPE2(fn_name, arg_name1, type2, arg_name2)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARGTYPE1_ARGTYPE2(type0, fn_name, type1, arg_name1, type2, arg_name2)\
     type0                                     \
     fn_name (type1 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARGTYPE1_ARG128(fn_name, type1, arg_name1, arg_name2)\
     UINT128                                     \
     fn_name (type1 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)          \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARG128_ARGTYPE2(type0, fn_name, arg_name1, type2, arg_name2)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARGTYPE1_ARG128(type0, fn_name, type1, arg_name1, arg_name2)\
     type0                                     \
     fn_name (type1 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)                      \
     PROLOG_VAL(arg_name2)          \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARG128_ARG128(type0, fn_name, arg_name1, arg_name2)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARG1(type0, fn_name, arg_name)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()

#define       BID128_FUNCTION_ARGTYPE1(fn_name, type1, arg_name)\
     UINT128                                     \
     fn_name (type1 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARGTYPE1(type0, fn_name, type1, arg_name)\
     type0                                     \
     fn_name (type1 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()

#define       TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
     type0                                     \
     fn_name (type1 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()

#endif



#define   BID_TO_SMALL_UINT_CVT_FUNCTION(type0, fn_name, type1, arg_name, cvt_fn_name, type2, size_mask, invalid_res)\
    TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
	type2 res;                                                    \
	_IDEC_flags saved_fpsc=*pfpsf;                                \
    BIDECIMAL_CALL1_NORND(cvt_fn_name, res, arg_name);            \
	if(res & size_mask) {                                         \
      *pfpsf = saved_fpsc | INVALID_EXCEPTION;                    \
	  res = invalid_res; }                                        \
    BID_RETURN_VAL((type0)res);                                   \
		   }

#define   BID_TO_SMALL_INT_CVT_FUNCTION(type0, fn_name, type1, arg_name, cvt_fn_name, type2, size_mask, invalid_res)\
    TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
	type2 res, sgn_mask;                                          \
	_IDEC_flags saved_fpsc=*pfpsf;                                \
    BIDECIMAL_CALL1_NORND(cvt_fn_name, res, arg_name);            \
	sgn_mask = res & size_mask;                                   \
	if(sgn_mask && (sgn_mask != (type2)size_mask)) {                     \
      *pfpsf = saved_fpsc | INVALID_EXCEPTION;                    \
	  res = invalid_res; }                                        \
    BID_RETURN_VAL((type0)res);                                   \
		   }
#endif
