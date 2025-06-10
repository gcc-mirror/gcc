#!/bin/sh

# Unless the user specified their desired m4 implementation through the
# M4 environment variable, find a GNU M4 in the PATH.

if [ -z "$M4" ] ; then
  for prog in gm4 gnum4 m4 ; do
    if $prog --version 2>/dev/null | grep -q 'GNU M4' ; then
      M4=${prog}
      break
    fi
  done
fi

if [ -z "$M4" ] ; then
  echo "GNU M4 not found"
  exit 1
else
  echo "Found GNU M4: ${M4}"
fi

# Make sure we run in the correct directory

if [ ! -e "./m4/pow.m4" ] ; then
  echo "This script should be run in the libgfortran/ directory"
  exit 1
fi


i_all_c="
generated/all_l1.c
generated/all_l2.c
generated/all_l4.c
generated/all_l8.c
generated/all_l16.c
"

i_any_c="
generated/any_l1.c
generated/any_l2.c
generated/any_l4.c
generated/any_l8.c
generated/any_l16.c
"

i_bessel_c="
generated/bessel_r4.c
generated/bessel_r8.c
generated/bessel_r10.c
generated/bessel_r16.c
generated/bessel_r17.c
"

i_count_c="
generated/count_1_l.c
generated/count_2_l.c
generated/count_4_l.c
generated/count_8_l.c
generated/count_16_l.c
"

i_iall_c="
generated/iall_i1.c
generated/iall_i2.c
generated/iall_i4.c
generated/iall_i8.c
generated/iall_i16.c
"

i_iany_c="
generated/iany_i1.c
generated/iany_i2.c
generated/iany_i4.c
generated/iany_i8.c
generated/iany_i16.c
"

i_iparity_c="
generated/iparity_i1.c
generated/iparity_i2.c
generated/iparity_i4.c
generated/iparity_i8.c
generated/iparity_i16.c
"

i_findloc0_c="
generated/findloc0_i1.c
generated/findloc0_i2.c
generated/findloc0_i4.c
generated/findloc0_i8.c
generated/findloc0_i16.c
generated/findloc0_r4.c
generated/findloc0_r8.c
generated/findloc0_r10.c
generated/findloc0_r16.c
generated/findloc0_r17.c
generated/findloc0_c4.c
generated/findloc0_c8.c
generated/findloc0_c10.c
generated/findloc0_c16.c
generated/findloc0_c17.c
"

i_findloc0s_c="
generated/findloc0_s1.c
generated/findloc0_s4.c
"

i_findloc1_c="
generated/findloc1_i1.c
generated/findloc1_i2.c
generated/findloc1_i4.c
generated/findloc1_i8.c
generated/findloc1_i16.c
generated/findloc1_r4.c
generated/findloc1_r8.c
generated/findloc1_r10.c
generated/findloc1_r16.c
generated/findloc1_r17.c
generated/findloc1_c4.c
generated/findloc1_c8.c
generated/findloc1_c10.c
generated/findloc1_c16.c
generated/findloc1_c17.c
"

i_findloc1s_c="
generated/findloc1_s1.c
generated/findloc1_s4.c
"

i_findloc2s_c="
generated/findloc2_s1.c
generated/findloc2_s4.c
"

i_maxloc0_c="
generated/maxloc0_4_i1.c
generated/maxloc0_8_i1.c
generated/maxloc0_16_i1.c
generated/maxloc0_4_i2.c
generated/maxloc0_8_i2.c
generated/maxloc0_16_i2.c
generated/maxloc0_4_i4.c
generated/maxloc0_8_i4.c
generated/maxloc0_16_i4.c
generated/maxloc0_4_i8.c
generated/maxloc0_8_i8.c
generated/maxloc0_16_i8.c
generated/maxloc0_4_i16.c
generated/maxloc0_8_i16.c
generated/maxloc0_16_i16.c
generated/maxloc0_4_m1.c
generated/maxloc0_8_m1.c
generated/maxloc0_16_m1.c
generated/maxloc0_4_m2.c
generated/maxloc0_8_m2.c
generated/maxloc0_16_m2.c
generated/maxloc0_4_m4.c
generated/maxloc0_8_m4.c
generated/maxloc0_16_m4.c
generated/maxloc0_4_m8.c
generated/maxloc0_8_m8.c
generated/maxloc0_16_m8.c
generated/maxloc0_4_m16.c
generated/maxloc0_8_m16.c
generated/maxloc0_16_m16.c
generated/maxloc0_4_r4.c
generated/maxloc0_8_r4.c
generated/maxloc0_16_r4.c
generated/maxloc0_4_r8.c
generated/maxloc0_8_r8.c
generated/maxloc0_16_r8.c
generated/maxloc0_4_r10.c
generated/maxloc0_8_r10.c
generated/maxloc0_16_r10.c
generated/maxloc0_4_r16.c
generated/maxloc0_8_r16.c
generated/maxloc0_16_r16.c
generated/maxloc0_4_r17.c
generated/maxloc0_8_r17.c
generated/maxloc0_16_r17.c
"

i_maxloc0s_c="
generated/maxloc0_4_s1.c
generated/maxloc0_4_s4.c
generated/maxloc0_8_s1.c
generated/maxloc0_8_s4.c
generated/maxloc0_16_s1.c
generated/maxloc0_16_s4.c
"

i_maxloc1_c="
generated/maxloc1_4_i1.c
generated/maxloc1_8_i1.c
generated/maxloc1_16_i1.c
generated/maxloc1_4_i2.c
generated/maxloc1_8_i2.c
generated/maxloc1_16_i2.c
generated/maxloc1_4_i4.c
generated/maxloc1_8_i4.c
generated/maxloc1_16_i4.c
generated/maxloc1_4_i8.c
generated/maxloc1_8_i8.c
generated/maxloc1_16_i8.c
generated/maxloc1_4_i16.c
generated/maxloc1_8_i16.c
generated/maxloc1_16_i16.c
generated/maxloc1_4_m1.c
generated/maxloc1_8_m1.c
generated/maxloc1_16_m1.c
generated/maxloc1_4_m2.c
generated/maxloc1_8_m2.c
generated/maxloc1_16_m2.c
generated/maxloc1_4_m4.c
generated/maxloc1_8_m4.c
generated/maxloc1_16_m4.c
generated/maxloc1_4_m8.c
generated/maxloc1_8_m8.c
generated/maxloc1_16_m8.c
generated/maxloc1_4_m16.c
generated/maxloc1_8_m16.c
generated/maxloc1_16_m16.c
generated/maxloc1_4_r4.c
generated/maxloc1_8_r4.c
generated/maxloc1_16_r4.c
generated/maxloc1_4_r8.c
generated/maxloc1_8_r8.c
generated/maxloc1_16_r8.c
generated/maxloc1_4_r10.c
generated/maxloc1_8_r10.c
generated/maxloc1_16_r10.c
generated/maxloc1_4_r16.c
generated/maxloc1_8_r16.c
generated/maxloc1_16_r16.c
generated/maxloc1_4_r17.c
generated/maxloc1_8_r17.c
generated/maxloc1_16_r17.c
"

i_maxloc1s_c="
generated/maxloc1_4_s1.c
generated/maxloc1_4_s4.c
generated/maxloc1_8_s1.c
generated/maxloc1_8_s4.c
generated/maxloc1_16_s1.c
generated/maxloc1_16_s4.c
"

i_maxloc2s_c="
generated/maxloc2_4_s1.c
generated/maxloc2_4_s4.c
generated/maxloc2_8_s1.c
generated/maxloc2_8_s4.c
generated/maxloc2_16_s1.c
generated/maxloc2_16_s4.c
"

i_maxval_c="
generated/maxval_i1.c
generated/maxval_i2.c
generated/maxval_i4.c
generated/maxval_i8.c
generated/maxval_i16.c
generated/maxval_m1.c
generated/maxval_m2.c
generated/maxval_m4.c
generated/maxval_m8.c
generated/maxval_m16.c
generated/maxval_r4.c
generated/maxval_r8.c
generated/maxval_r10.c
generated/maxval_r16.c
generated/maxval_r17.c
"

i_maxval0s_c="
generated/maxval0_s1.c
generated/maxval0_s4.c
"

i_maxval1s_c="
generated/maxval1_s1.c
generated/maxval1_s4.c
"

i_minloc0_c="
generated/minloc0_4_i1.c
generated/minloc0_8_i1.c
generated/minloc0_16_i1.c
generated/minloc0_4_i2.c
generated/minloc0_8_i2.c
generated/minloc0_16_i2.c
generated/minloc0_4_i4.c
generated/minloc0_8_i4.c
generated/minloc0_16_i4.c
generated/minloc0_4_i8.c
generated/minloc0_8_i8.c
generated/minloc0_16_i8.c
generated/minloc0_4_i16.c
generated/minloc0_8_i16.c
generated/minloc0_16_i16.c
generated/minloc0_4_m1.c
generated/minloc0_8_m1.c
generated/minloc0_16_m1.c
generated/minloc0_4_m2.c
generated/minloc0_8_m2.c
generated/minloc0_16_m2.c
generated/minloc0_4_m4.c
generated/minloc0_8_m4.c
generated/minloc0_16_m4.c
generated/minloc0_4_m8.c
generated/minloc0_8_m8.c
generated/minloc0_16_m8.c
generated/minloc0_4_m16.c
generated/minloc0_8_m16.c
generated/minloc0_16_m16.c
generated/minloc0_4_r4.c
generated/minloc0_8_r4.c
generated/minloc0_16_r4.c
generated/minloc0_4_r8.c
generated/minloc0_8_r8.c
generated/minloc0_16_r8.c
generated/minloc0_4_r10.c
generated/minloc0_8_r10.c
generated/minloc0_16_r10.c
generated/minloc0_4_r16.c
generated/minloc0_8_r16.c
generated/minloc0_16_r16.c
generated/minloc0_4_r17.c
generated/minloc0_8_r17.c
generated/minloc0_16_r17.c
"

i_minloc0s_c="
generated/minloc0_4_s1.c
generated/minloc0_4_s4.c
generated/minloc0_8_s1.c
generated/minloc0_8_s4.c
generated/minloc0_16_s1.c
generated/minloc0_16_s4.c
"

i_minloc1_c="
generated/minloc1_4_i1.c
generated/minloc1_8_i1.c
generated/minloc1_16_i1.c
generated/minloc1_4_i2.c
generated/minloc1_8_i2.c
generated/minloc1_16_i2.c
generated/minloc1_4_i4.c
generated/minloc1_8_i4.c
generated/minloc1_16_i4.c
generated/minloc1_4_i8.c
generated/minloc1_8_i8.c
generated/minloc1_16_i8.c
generated/minloc1_4_i16.c
generated/minloc1_8_i16.c
generated/minloc1_16_i16.c
generated/minloc1_4_m1.c
generated/minloc1_8_m1.c
generated/minloc1_16_m1.c
generated/minloc1_4_m2.c
generated/minloc1_8_m2.c
generated/minloc1_16_m2.c
generated/minloc1_4_m4.c
generated/minloc1_8_m4.c
generated/minloc1_16_m4.c
generated/minloc1_4_m8.c
generated/minloc1_8_m8.c
generated/minloc1_16_m8.c
generated/minloc1_4_m16.c
generated/minloc1_8_m16.c
generated/minloc1_16_m16.c
generated/minloc1_4_r4.c
generated/minloc1_8_r4.c
generated/minloc1_16_r4.c
generated/minloc1_4_r8.c
generated/minloc1_8_r8.c
generated/minloc1_16_r8.c
generated/minloc1_4_r10.c
generated/minloc1_8_r10.c
generated/minloc1_16_r10.c
generated/minloc1_4_r16.c
generated/minloc1_8_r16.c
generated/minloc1_16_r16.c
generated/minloc1_4_r17.c
generated/minloc1_8_r17.c
generated/minloc1_16_r17.c
"
i_minloc1s_c="
generated/minloc1_4_s1.c
generated/minloc1_4_s4.c
generated/minloc1_8_s1.c
generated/minloc1_8_s4.c
generated/minloc1_16_s1.c
generated/minloc1_16_s4.c
"

i_minloc2s_c="
generated/minloc2_4_s1.c
generated/minloc2_4_s4.c
generated/minloc2_8_s1.c
generated/minloc2_8_s4.c
generated/minloc2_16_s1.c
generated/minloc2_16_s4.c
"

i_minval_c="
generated/minval_i1.c
generated/minval_i2.c
generated/minval_i4.c
generated/minval_i8.c
generated/minval_i16.c
generated/minval_m1.c
generated/minval_m2.c
generated/minval_m4.c
generated/minval_m8.c
generated/minval_m16.c
generated/minval_r4.c
generated/minval_r8.c
generated/minval_r10.c
generated/minval_r16.c
generated/minval_r17.c
"

i_minval0s_c="
generated/minval0_s1.c
generated/minval0_s4.c
"

i_minval1s_c="
generated/minval1_s1.c
generated/minval1_s4.c
"

i_norm2_c="
generated/norm2_r4.c
generated/norm2_r8.c
generated/norm2_r10.c
generated/norm2_r16.c
generated/norm2_r17.c
"
i_parity_c="
generated/parity_l1.c
generated/parity_l2.c
generated/parity_l4.c
generated/parity_l8.c
generated/parity_l16.c
"

i_sum_c="
generated/sum_i1.c
generated/sum_i2.c
generated/sum_i4.c
generated/sum_i8.c
generated/sum_i16.c
generated/sum_r4.c
generated/sum_r8.c
generated/sum_r10.c
generated/sum_r16.c
generated/sum_r17.c
generated/sum_c4.c
generated/sum_c8.c
generated/sum_c10.c
generated/sum_c16.c
generated/sum_c17.c
"

i_product_c="
generated/product_i1.c
generated/product_i2.c
generated/product_i4.c
generated/product_i8.c
generated/product_i16.c
generated/product_r4.c
generated/product_r8.c
generated/product_r10.c
generated/product_r16.c
generated/product_r17.c
generated/product_c4.c
generated/product_c8.c
generated/product_c10.c
generated/product_c16.c
generated/product_c17.c
"

i_matmul_c="
generated/matmul_i1.c
generated/matmul_i2.c
generated/matmul_i4.c
generated/matmul_i8.c
generated/matmul_i16.c
generated/matmul_r4.c
generated/matmul_r8.c
generated/matmul_r10.c
generated/matmul_r16.c
generated/matmul_r17.c
generated/matmul_c4.c
generated/matmul_c8.c
generated/matmul_c10.c
generated/matmul_c16.c
generated/matmul_c17.c
"

i_matmulavx128_c="
generated/matmulavx128_i1.c
generated/matmulavx128_i2.c
generated/matmulavx128_i4.c
generated/matmulavx128_i8.c
generated/matmulavx128_i16.c
generated/matmulavx128_r4.c
generated/matmulavx128_r8.c
generated/matmulavx128_r10.c
generated/matmulavx128_r16.c
generated/matmulavx128_r17.c
generated/matmulavx128_c4.c
generated/matmulavx128_c8.c
generated/matmulavx128_c10.c
generated/matmulavx128_c16.c
generated/matmulavx128_c17.c
"

i_matmull_c="
generated/matmul_l4.c
generated/matmul_l8.c
generated/matmul_l16.c
"

i_shape_c="
generated/shape_i1.c
generated/shape_i2.c
generated/shape_i4.c
generated/shape_i8.c
generated/shape_i16.c
"

i_reshape_c="
generated/reshape_i4.c
generated/reshape_i8.c
generated/reshape_i16.c
generated/reshape_r4.c
generated/reshape_r8.c
generated/reshape_r10.c
generated/reshape_r16.c
generated/reshape_r17.c
generated/reshape_c4.c
generated/reshape_c8.c
generated/reshape_c10.c
generated/reshape_c16.c
generated/reshape_c17.c
"

i_eoshift1_c="
generated/eoshift1_4.c
generated/eoshift1_8.c
generated/eoshift1_16.c
"

i_eoshift3_c="
generated/eoshift3_4.c
generated/eoshift3_8.c
generated/eoshift3_16.c
"

i_cshift0_c="
generated/cshift0_i1.c
generated/cshift0_i2.c
generated/cshift0_i4.c
generated/cshift0_i8.c
generated/cshift0_i16.c
generated/cshift0_r4.c
generated/cshift0_r8.c
generated/cshift0_r10.c
generated/cshift0_r16.c
generated/cshift0_r17.c
generated/cshift0_c4.c
generated/cshift0_c8.c
generated/cshift0_c10.c
generated/cshift0_c16.c
generated/cshift0_c17.c
"

i_cshift1_c="
generated/cshift1_4.c
generated/cshift1_8.c
generated/cshift1_16.c
"

i_cshift1a_c="
generated/cshift1_4_i1.c
generated/cshift1_4_i2.c
generated/cshift1_4_i4.c
generated/cshift1_4_i8.c
generated/cshift1_4_i16.c
generated/cshift1_4_r4.c
generated/cshift1_4_r8.c
generated/cshift1_4_r10.c
generated/cshift1_4_r16.c
generated/cshift1_4_r17.c
generated/cshift1_4_c4.c
generated/cshift1_4_c8.c
generated/cshift1_4_c10.c
generated/cshift1_4_c16.c
generated/cshift1_4_c17.c
generated/cshift1_8_i1.c
generated/cshift1_8_i2.c
generated/cshift1_8_i4.c
generated/cshift1_8_i8.c
generated/cshift1_8_i16.c
generated/cshift1_8_r4.c
generated/cshift1_8_r8.c
generated/cshift1_8_r10.c
generated/cshift1_8_r16.c
generated/cshift1_8_r17.c
generated/cshift1_8_c4.c
generated/cshift1_8_c8.c
generated/cshift1_8_c10.c
generated/cshift1_8_c16.c
generated/cshift1_8_c17.c
generated/cshift1_16_i1.c
generated/cshift1_16_i2.c
generated/cshift1_16_i4.c
generated/cshift1_16_i8.c
generated/cshift1_16_i16.c
generated/cshift1_16_r4.c
generated/cshift1_16_r8.c
generated/cshift1_16_r10.c
generated/cshift1_16_r16.c
generated/cshift1_16_r17.c
generated/cshift1_16_c4.c
generated/cshift1_16_c8.c
generated/cshift1_16_c10.c
generated/cshift1_16_c16.c
generated/cshift1_16_c17.c
"

in_pack_c="
generated/in_pack_i1.c
generated/in_pack_i2.c
generated/in_pack_i4.c
generated/in_pack_i8.c
generated/in_pack_i16.c
generated/in_pack_r4.c
generated/in_pack_r8.c
generated/in_pack_r10.c
generated/in_pack_r16.c
generated/in_pack_r17.c
generated/in_pack_c4.c
generated/in_pack_c8.c
generated/in_pack_c10.c
generated/in_pack_c16.c
generated/in_pack_c17.c
"

in_unpack_c="
generated/in_unpack_i1.c
generated/in_unpack_i2.c
generated/in_unpack_i4.c
generated/in_unpack_i8.c
generated/in_unpack_i16.c
generated/in_unpack_r4.c
generated/in_unpack_r8.c
generated/in_unpack_r10.c
generated/in_unpack_r16.c
generated/in_unpack_r17.c
generated/in_unpack_c4.c
generated/in_unpack_c8.c
generated/in_unpack_c10.c
generated/in_unpack_c16.c
generated/in_unpack_c17.c
"

i_pow_c="
generated/pow_i4_i4.c
generated/pow_i8_i4.c
generated/pow_i16_i4.c
generated/pow_r16_i4.c
generated/pow_r17_i4.c
generated/pow_c4_i4.c
generated/pow_c8_i4.c
generated/pow_c10_i4.c
generated/pow_c16_i4.c
generated/pow_c17_i4.c
generated/pow_i4_i8.c
generated/pow_i8_i8.c
generated/pow_i16_i8.c
generated/pow_r4_i8.c
generated/pow_r8_i8.c
generated/pow_r10_i8.c
generated/pow_r16_i8.c
generated/pow_r17_i8.c
generated/pow_c4_i8.c
generated/pow_c8_i8.c
generated/pow_c10_i8.c
generated/pow_c16_i8.c
generated/pow_c17_i8.c
generated/pow_i4_i16.c
generated/pow_i8_i16.c
generated/pow_i16_i16.c
generated/pow_r4_i16.c
generated/pow_r8_i16.c
generated/pow_r10_i16.c
generated/pow_r16_i16.c
generated/pow_r17_i16.c
generated/pow_c4_i16.c
generated/pow_c8_i16.c
generated/pow_c10_i16.c
generated/pow_c16_i16.c
generated/pow_c17_i16.c
"

i_powu_c="
generated/pow_m1_m1.c
generated/pow_m1_m2.c
generated/pow_m1_m4.c
generated/pow_m1_m8.c
generated/pow_m1_m16.c
generated/pow_m2_m1.c
generated/pow_m2_m2.c
generated/pow_m2_m4.c
generated/pow_m2_m8.c
generated/pow_m2_m16.c
generated/pow_m4_m1.c
generated/pow_m4_m2.c
generated/pow_m4_m4.c
generated/pow_m4_m8.c
generated/pow_m4_m16.c
generated/pow_m8_m1.c
generated/pow_m8_m2.c
generated/pow_m8_m4.c
generated/pow_m8_m8.c
generated/pow_m8_m16.c
generated/pow_m16_m1.c
generated/pow_m16_m2.c
generated/pow_m16_m4.c
generated/pow_m16_m8.c
generated/pow_m16_m16.c
"

i_pack_c="
generated/pack_i1.c
generated/pack_i2.c
generated/pack_i4.c
generated/pack_i8.c
generated/pack_i16.c
generated/pack_r4.c
generated/pack_r8.c
generated/pack_r10.c
generated/pack_r16.c
generated/pack_r17.c
generated/pack_c4.c
generated/pack_c8.c
generated/pack_c10.c
generated/pack_c16.c
generated/pack_c17.c
"

i_unpack_c="
generated/unpack_i1.c
generated/unpack_i2.c
generated/unpack_i4.c
generated/unpack_i8.c
generated/unpack_i16.c
generated/unpack_r4.c
generated/unpack_r8.c
generated/unpack_r10.c
generated/unpack_r16.c
generated/unpack_r17.c
generated/unpack_c4.c
generated/unpack_c8.c
generated/unpack_c10.c
generated/unpack_c16.c
generated/unpack_c17.c
"

i_spread_c="
generated/spread_i1.c
generated/spread_i2.c
generated/spread_i4.c
generated/spread_i8.c
generated/spread_i16.c
generated/spread_r4.c
generated/spread_r8.c
generated/spread_r10.c
generated/spread_r16.c
generated/spread_r17.c
generated/spread_c4.c
generated/spread_c8.c
generated/spread_c10.c
generated/spread_c16.c
generated/spread_c17.c
"

gfor_built_specific_src="
generated/_abs_c4.F90
generated/_abs_c8.F90
generated/_abs_c10.F90
generated/_abs_c16.F90
generated/_abs_c17.F90
generated/_abs_i4.F90
generated/_abs_i8.F90
generated/_abs_i16.F90
generated/_abs_r4.F90
generated/_abs_r8.F90
generated/_abs_r10.F90
generated/_abs_r16.F90
generated/_abs_r17.F90
generated/_aimag_c4.F90
generated/_aimag_c8.F90
generated/_aimag_c10.F90
generated/_aimag_c16.F90
generated/_aimag_c17.F90
generated/_exp_r4.F90
generated/_exp_r8.F90
generated/_exp_r10.F90
generated/_exp_r16.F90
generated/_exp_r17.F90
generated/_exp_c4.F90
generated/_exp_c8.F90
generated/_exp_c10.F90
generated/_exp_c16.F90
generated/_exp_c17.F90
generated/_log_r4.F90
generated/_log_r8.F90
generated/_log_r10.F90
generated/_log_r16.F90
generated/_log_r17.F90
generated/_log_c4.F90
generated/_log_c8.F90
generated/_log_c10.F90
generated/_log_c16.F90
generated/_log_c17.F90
generated/_log10_r4.F90
generated/_log10_r8.F90
generated/_log10_r10.F90
generated/_log10_r16.F90
generated/_log10_r17.F90
generated/_sqrt_r4.F90
generated/_sqrt_r8.F90
generated/_sqrt_r10.F90
generated/_sqrt_r16.F90
generated/_sqrt_r17.F90
generated/_sqrt_c4.F90
generated/_sqrt_c8.F90
generated/_sqrt_c10.F90
generated/_sqrt_c16.F90
generated/_sqrt_c17.F90
generated/_asin_r4.F90
generated/_asin_r8.F90
generated/_asin_r10.F90
generated/_asin_r16.F90
generated/_asin_r17.F90
generated/_asinh_r4.F90
generated/_asinh_r8.F90
generated/_asinh_r10.F90
generated/_asinh_r16.F90
generated/_asinh_r17.F90
generated/_acos_r4.F90
generated/_acos_r8.F90
generated/_acos_r10.F90
generated/_acos_r16.F90
generated/_acos_r17.F90
generated/_acosh_r4.F90
generated/_acosh_r8.F90
generated/_acosh_r10.F90
generated/_acosh_r16.F90
generated/_acosh_r17.F90
generated/_atan_r4.F90
generated/_atan_r8.F90
generated/_atan_r10.F90
generated/_atan_r16.F90
generated/_atan_r17.F90
generated/_atanh_r4.F90
generated/_atanh_r8.F90
generated/_atanh_r10.F90
generated/_atanh_r16.F90
generated/_atanh_r17.F90
generated/_sin_r4.F90
generated/_sin_r8.F90
generated/_sin_r10.F90
generated/_sin_r16.F90
generated/_sin_r17.F90
generated/_sin_c4.F90
generated/_sin_c8.F90
generated/_sin_c10.F90
generated/_sin_c16.F90
generated/_sin_c17.F90
generated/_cos_r4.F90
generated/_cos_r8.F90
generated/_cos_r10.F90
generated/_cos_r16.F90
generated/_cos_r17.F90
generated/_cos_c4.F90
generated/_cos_c8.F90
generated/_cos_c10.F90
generated/_cos_c16.F90
generated/_cos_c17.F90
generated/_tan_r4.F90
generated/_tan_r8.F90
generated/_tan_r10.F90
generated/_tan_r16.F90
generated/_tan_r17.F90
generated/_sinh_r4.F90
generated/_sinh_r8.F90
generated/_sinh_r10.F90
generated/_sinh_r16.F90
generated/_sinh_r17.F90
generated/_cosh_r4.F90
generated/_cosh_r8.F90
generated/_cosh_r10.F90
generated/_cosh_r16.F90
generated/_cosh_r17.F90
generated/_tanh_r4.F90
generated/_tanh_r8.F90
generated/_tanh_r10.F90
generated/_tanh_r16.F90
generated/_tanh_r17.F90
generated/_conjg_c4.F90
generated/_conjg_c8.F90
generated/_conjg_c10.F90
generated/_conjg_c16.F90
generated/_conjg_c17.F90
generated/_aint_r4.F90
generated/_aint_r8.F90
generated/_aint_r10.F90
generated/_aint_r16.F90
generated/_aint_r17.F90
generated/_anint_r4.F90
generated/_anint_r8.F90
generated/_anint_r10.F90
generated/_anint_r16.F90
generated/_anint_r17.F90
"

gfor_built_specific2_src="
generated/_sign_i4.F90
generated/_sign_i8.F90
generated/_sign_i16.F90
generated/_sign_r4.F90
generated/_sign_r8.F90
generated/_sign_r10.F90
generated/_sign_r16.F90
generated/_sign_r17.F90
generated/_dim_i4.F90
generated/_dim_i8.F90
generated/_dim_i16.F90
generated/_dim_r4.F90
generated/_dim_r8.F90
generated/_dim_r10.F90
generated/_dim_r16.F90
generated/_dim_r17.F90
generated/_atan2_r4.F90
generated/_atan2_r8.F90
generated/_atan2_r10.F90
generated/_atan2_r16.F90
generated/_atan2_r17.F90
generated/_mod_i4.F90
generated/_mod_i8.F90
generated/_mod_i16.F90
generated/_mod_r4.F90
generated/_mod_r8.F90
generated/_mod_r10.F90
generated/_mod_r16.F90
generated/_mod_r17.F90
"

gfor_misc_specifics="
generated/misc_specifics.F90
"


for f in ${i_all_c} ; do
  ${M4} -Dfile=$f -I./m4 all.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_bessel_c} ; do
  ${M4} -Dfile=$f -I./m4 bessel.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_any_c} ; do
  ${M4} -Dfile=$f -I./m4 any.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_count_c} ; do
  ${M4} -Dfile=$f -I./m4 count.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_findloc0_c} ; do
  ${M4} -Dfile=$f -I./m4 findloc0.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_findloc0s_c} ; do
  ${M4} -Dfile=$f -I./m4 findloc0s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_findloc1_c} ; do
  ${M4} -Dfile=$f -I./m4 findloc1.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_findloc1s_c} ; do
  ${M4} -Dfile=$f -I./m4 findloc1s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_findloc2s_c} ; do
  ${M4} -Dfile=$f -I./m4 findloc2s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_iall_c} ; do
  ${M4} -Dfile=$f -I./m4 iall.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_iany_c} ; do
  ${M4} -Dfile=$f -I./m4 iany.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_iparity_c} ; do
  ${M4} -Dfile=$f -I./m4 iparity.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxloc0_c} ; do
  ${M4} -Dfile=$f -I./m4 maxloc0.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxloc0s_c} ; do
  ${M4} -Dfile=$f -I./m4 maxloc0s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxloc1_c} ; do
  ${M4} -Dfile=$f -I./m4 maxloc1.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxloc1s_c} ; do
  ${M4} -Dfile=$f -I./m4 maxloc1s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxloc2s_c} ; do
  ${M4} -Dfile=$f -I./m4 maxloc2s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxval_c} ; do
  ${M4} -Dfile=$f -I./m4 maxval.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxval0s_c} ; do
  ${M4} -Dfile=$f -I./m4 maxval0s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_maxval1s_c} ; do
  ${M4} -Dfile=$f -I./m4 maxval1s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minloc0_c} ; do
  ${M4} -Dfile=$f -I./m4 minloc0.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minloc0s_c} ; do
  ${M4} -Dfile=$f -I./m4 minloc0s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minloc1_c} ; do
  ${M4} -Dfile=$f -I./m4 minloc1.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minloc1s_c} ; do
  ${M4} -Dfile=$f -I./m4 minloc1s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minloc2s_c} ; do
  ${M4} -Dfile=$f -I./m4 minloc2s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minval_c} ; do
  ${M4} -Dfile=$f -I./m4 minval.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minval0s_c} ; do
  ${M4} -Dfile=$f -I./m4 minval0s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_minval1s_c} ; do
  ${M4} -Dfile=$f -I./m4 minval1s.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_product_c} ; do
  ${M4} -Dfile=$f -I./m4 product.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_sum_c} ; do
  ${M4} -Dfile=$f -I./m4 sum.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_matmul_c} ; do
  ${M4} -Dfile=$f -I./m4 matmul.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_matmulavx128_c} ; do
  ${M4} -Dfile=$f -I./m4 matmulavx128.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_matmull_c} ; do
  ${M4} -Dfile=$f -I./m4 matmull.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_norm2_c} ; do
  ${M4} -Dfile=$f -I./m4 norm2.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_parity_c} ; do
  ${M4} -Dfile=$f -I./m4 parity.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_shape_c} ; do
  ${M4} -Dfile=$f -I./m4 shape.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_reshape_c} ; do
  ${M4} -Dfile=$f -I./m4 reshape.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_eoshift1_c} ; do
  ${M4} -Dfile=$f -I./m4 eoshift1.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_eoshift3_c} ; do
  ${M4} -Dfile=$f -I./m4 eoshift3.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_cshift0_c} ; do
  ${M4} -Dfile=$f -I./m4 cshift0.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_cshift1_c} ; do
  ${M4} -Dfile=$f -I./m4 cshift1.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_cshift1a_c} ; do
  ${M4} -Dfile=$f -I./m4 cshift1a.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${in_pack_c} ; do
  ${M4} -Dfile=$f -I./m4 in_pack.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${in_unpack_c} ; do
  ${M4} -Dfile=$f -I./m4 in_unpack.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_pow_c} ; do
  ${M4} -Dfile=$f -I./m4 pow.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_powu_c} ; do
  ${M4} -Dfile=$f -I./m4 powu.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_pack_c} ; do
  ${M4} -Dfile=$f -I./m4 pack.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_unpack_c} ; do
  ${M4} -Dfile=$f -I./m4 unpack.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${i_spread_c} ; do
  ${M4} -Dfile=$f -I./m4 spread.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${gfor_built_specific_src} ; do
  ${M4} -Dfile=$f -I./m4 specific.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${gfor_built_specific2_src} ; do
  ${M4} -Dfile=$f -I./m4 specific2.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

for f in ${gfor_misc_specifics} ; do
  ${M4} -Dfile=$f -I./m4 misc_specifics.m4 > $f.tmp
  ../move-if-change $f.tmp $f
done

# Check that all generated files are listed in Makefile.am

for i in generated/*.c generated/*.F90 ; do
  if ! grep -q "$i" Makefile.am ; then
    echo "File $i is not present in Makefile.am"
  fi
done

# Check that all generated files listed in Makefile.am actually exist

genlist=`tr ' ' '\n' < Makefile.am | grep 'generated/'`
for i in $genlist ; do
  if ! test -e "$i" ; then
    echo "File $i listed in Makefile.am does not exist"
  fi
done

