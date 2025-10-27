/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <fcntl.h>
#include <unistd.h>

#include <cctype>
#include <cerrno>
#include <cmath>
#include <cfenv>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <algorithm>
#include <vector>

#include "config.h"
#include "libgcobol-fp.h"

#include "ec.h"
#include "common-defs.h"
#include "io.h"
#include "gcobolio.h"
#include "libgcobol.h"
#include "gmath.h"
#include "gcobolio.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#define MAX_INTERMEDIATE_BITS 126
#define MAX_INTERMEDIATE_DECIMALS 16

static int
conditional_stash(  cblc_field_t *destination,
                    size_t        destination_o,
                    size_t        destination_s,
                    bool          on_error_flag,
                    __int128      value,
                    int           rdigits,
                    cbl_round_t   rounded)
  {
  int retval = compute_error_none;
  if( !on_error_flag )
    {
    // It's an uncomplicated assignment, because there was no
    // ON SIZE ERROR phrase
    __gg__int128_to_qualified_field(destination,
                                    destination_o,
                                    destination_s,
                                    value,
                                    rdigits,
                                    rounded,
                                    &retval);
    }
  else
    {
    // This is slightly more complex, because in the event of a
    // SIZE ERROR. we need to leave the original value untouched

    unsigned char *stash = static_cast<unsigned char *>(malloc(destination_s));
    massert(stash);
    memcpy(stash, destination->data+destination_o, destination_s);

    __gg__int128_to_qualified_field(destination,
                                    destination_o,
                                    destination_s,
                                    value,
                                    rdigits,
                                    rounded,
                                    &retval);
    if( retval )
      {
      // Because there was a size error, we will report that
      // upon return, and we need to put back the original value:
      memcpy(destination->data+destination_o, stash, destination_s);
      }
    free(stash);
    }
  return retval;
  }

static int
conditional_stash(  cblc_field_t *destination,
                    size_t        destination_o,
                    size_t        destination_s,
                    bool          on_error_flag,
                    GCOB_FP128    value,
                    cbl_round_t     rounded)
  {
  int retval = compute_error_none;
  if( !on_error_flag )
    {
    // It's an uncomplicated assignment, because there was no
    // ON SIZE ERROR phrase
    __gg__float128_to_qualified_field(destination,
                                      destination_o,
                                      value,
                                      rounded,
                                      &retval);
    }
  else
    {
    // This is slightly more complex, because in the event of a
    // SIZE ERROR. we need to leave the original value untouched
    assert(destination_s);
    unsigned char *stash = static_cast<unsigned char *>(malloc(destination_s));
    massert(stash);
    memcpy(stash, destination->data+destination_o, destination_s);
    __gg__float128_to_qualified_field(destination,
                                      destination_o,
                                      value,
                                      rounded,
                                      &retval);
    if( retval )
      {
      // Because there was a size error, we will report that
      // upon return, and we need to put back the original value:
      memcpy(destination->data+destination_o, stash, destination_s);
      }
    free(stash);
    }
  return retval;
  }

static
GCOB_FP128
divide_helper_float(GCOB_FP128 a_value,
                    GCOB_FP128 b_value,
                    int      *compute_error)
  {
  if( b_value == 0 )
    {
    // Can't divide by zero
    *compute_error |= compute_error_divide_by_zero;
    return a_value;
    }

  // Do the actual division, giving us 0.399999999999999999999999999999999971
  a_value /= b_value;

  if( __builtin_isinf(a_value) )
    {
    *compute_error |= compute_error_overflow;
    return 0;
    }

  if( __builtin_isnan(a_value) )
    {
    *compute_error |= compute_error_underflow;
    return 0;
    }

  return a_value;
  }

static
GCOB_FP128
multiply_helper_float(GCOB_FP128 a_value,
                      GCOB_FP128 b_value,
                      int      *compute_error)
  {
  a_value *= b_value;

  if( __builtin_isinf(a_value) )
    {
    *compute_error |= compute_error_overflow;
    return 0;
    }

  if( __builtin_isnan(a_value) )
    {
    *compute_error |= compute_error_underflow;
    return 0;
    }

  return a_value;
  }

static
GCOB_FP128
addition_helper_float(GCOB_FP128 a_value,
                      GCOB_FP128 b_value,
                      int      *compute_error)
  {
  a_value += b_value;

  if( __builtin_isinf(a_value) )
    {
    *compute_error |= compute_error_overflow;
    return 0;
    }

  if( __builtin_isnan(a_value) )
    {
    *compute_error |= compute_error_underflow;
    return 0;
    }

  return a_value;
  }

static
GCOB_FP128
subtraction_helper_float(GCOB_FP128 a_value,
                         GCOB_FP128 b_value,
                         int      *compute_error)
  {
  a_value -= b_value;

  if( __builtin_isinf(a_value) )
    {
    *compute_error |= compute_error_overflow;
    return 0;
    }

  if( __builtin_isnan(a_value) )
    {
    *compute_error |= compute_error_underflow;
    return 0;
    }

  return a_value;
  }

extern "C"
void
__gg__pow(  cbl_arith_format_t,
            size_t,
            size_t,
            size_t,
      const cbl_round_t  *rounded,
            int           on_error_flag,
            int          *compute_error
            )
  {
        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;
        cblc_field_t **B  = __gg__treeplet_2f;
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  GCOB_FP128 avalue = __gg__float128_from_qualified_field(A[0], A_o[0], A_s[0]);
  GCOB_FP128 bvalue = __gg__float128_from_qualified_field(B[0], B_o[0], B_s[0]);
  GCOB_FP128 tgt_value;

  if( avalue == 0 && bvalue == 0 )
    {
    *compute_error |= compute_error_exp_zero_by_zero;
    tgt_value = 1;
    }
  else if(avalue == 0 && bvalue < 0 )
    {
    *compute_error |= compute_error_exp_zero_by_minus;
    tgt_value = 0;
    }
  else
    {
    // Calculate our answer, in floating point:
    errno = 0;
    feclearexcept(FE_ALL_EXCEPT);
    tgt_value = FP128_FUNC(pow)(avalue, bvalue);
    if( errno || fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW) )
      {
      // One of a large number of errors took place. See math_error(7) and
      // pow(3).  Let's just use this last error as a grab-bag; I didn't
      // care to go down the rabbit hole of figuring out if a floating point
      // number did or did not have a fractional part.  That way lies
      // madness.
      *compute_error |= compute_error_exp_minus_by_frac;
      // This kind of error doesn't overwrite the target, so the returned
      // value is not relevant.  Make it zero to avoid overheating the
      // converstion routine
      tgt_value = 0;
      }
    }
  if( !(*compute_error & compute_error_exp_minus_by_frac) )
    {
    *compute_error |= conditional_stash(C[0],
                                        C_o[0],
                                        C_s[0],
                                        (on_error_flag & ON_SIZE_ERROR),
                                        tgt_value,
                                        *rounded);
    }
  }

extern "C"
void
__gg__process_compute_error(int compute_error)
  {
  // This routine gets called after a series of parser_op operations is
  // complete (see parser_assign()) when the source code didn't specify
  // an ON SIZE ERROR clause.
  if( compute_error & compute_error_divide_by_zero)
    {
    exception_raise(ec_size_zero_divide_e);
    }
  else if( compute_error & compute_error_truncate )
    {
    exception_raise(ec_size_truncation_e);
    }
  else if( compute_error & (    compute_error_exp_zero_by_zero
                              | compute_error_exp_zero_by_minus
                              | compute_error_exp_minus_by_frac ) )
    {
    exception_raise(ec_size_exponentiation_e);
    }
  else if( compute_error & compute_error_overflow )
    {
    exception_raise(ec_size_overflow_e);
    }
  else if( compute_error & compute_error_underflow )
    {
    exception_raise(ec_size_underflow_e);
    }
  }

typedef unsigned __int128 uint128;
typedef struct int256
  {
  union
    {
    unsigned char  data[32];
    uint64_t       i64 [4];
    uint128        i128[2];
    };
  }int256;

static int
multiply_int256_by_int64(int256 &product, const uint64_t multiplier)
  {
  // Typical use of this routine is multiplying a temporary value by
  // a factor of ten.  This is effectively left-shifting by decimal
  // digits.  See scale_int256_by_digits
  uint64_t overflows[5] = {};
  for(int i=0; i<4; i++)
    {
    uint128 temp = (uint128)product.i64[i] * multiplier;
    product.i64[i] = *PTRCAST(uint64_t, &temp);
    overflows[i+1] = *PTRCAST(uint64_t, PTRCAST(uint8_t, &temp) + 8);
    }

  for(int i=1; i<4; i++)
    {
    product.i64[i] += overflows[i];
    if(product.i64[i] < overflows[i])
      {
      overflows[i+1] += 1;
      }
    }
  // Indicate that an overflow took place.  This is not useful unless the int256
  // is known to be positive.
  return overflows[4];
  }

static int
add_int256_to_int256(int256 &sum, const int256 &addend)
  {
  uint128 overflows[3] = {};
  for(int i=0; i<2; i++)
    {
    sum.i128[i] += addend.i128[i];
    if( sum.i128[i] < addend.i128[i] )
      {
      overflows[i+1] = 1;
      }
    }
  if( overflows[1] )
    {
    sum.i128[1] += overflows[1];
    if( sum.i128[1] == 0 )
      {
      overflows[2] = 1;
      }
    }
  // Indicate that an overflow took place.  This is not useful unless the two
  // values are known to be positive.
  return (int)overflows[2];
  }

static void
negate_int256(int256 &val)
  {
  val.i128[0] = ~val.i128[0];
  val.i128[1] = ~val.i128[1];
  val.i128[0] += 1;
  if( !val.i128[0] )
    {
    val.i128[1] += 1;
    }
  }

static int
subtract_int256_from_int256(int256 &difference, int256 subtrahend)
  {
  negate_int256(subtrahend);
  return add_int256_to_int256(difference, subtrahend);
  }

static void
scale_int256_by_digits(int256 &val, int digits)
  {
  uint64_t pot;
  while(digits > 17)
    {
    pot = (uint64_t)__gg__power_of_ten(17);
    multiply_int256_by_int64(val, pot);
    digits -= 17;
    }
  pot = (uint64_t)__gg__power_of_ten(digits);
  multiply_int256_by_int64(val, pot);
  }

static void
divide_int256_by_int64(int256 &val, uint64_t divisor)
  {
  // val needs to be a positive number
  uint128 temp = 0;
  for( int i=3; i>=0; i-- )
    {
    // Left shift temp 64 bits:
    *PTRCAST(uint64_t, ((PTRCAST(uint8_t, &temp))+8))
                          = *PTRCAST(uint64_t, ((PTRCAST(uint8_t, &temp))+0));

    // Put the high digit of val into the bottom of temp
    *PTRCAST(uint64_t, ((PTRCAST(uint8_t, &temp))+0)) = val.i64[i];

    // Divide that combinary by divisor to get the new digits
    val.i64[i] = temp / divisor;

    // And the new temp is that combination modulo divisor
    temp = temp % divisor;
    }
  }

static int
squeeze_int256(int256 &val, int &rdigits)
  {
  int overflow = 0;
  // It has been decreed that at this juncture the result must fit into
  // MAX_FIXED_POINT_DIGITS.  If the result does not, we have an OVERFLOW
  // error.

  int is_negative = val.data[31] & 0x80;
  if( is_negative )
    {
    negate_int256(val);
    }

  // As long as there are some decimal places left, we hold our nose and
  // right-shift a too-large value rightward by decimal digits.  In other
  // words, we truncate the fractional part to make room for the integer part:
  while(rdigits > 0 && val.i128[1] )
    {
    divide_int256_by_int64(val, 10UL);
    rdigits -= 1;
    }

  // At this point, to be useful, val has to have fewer than 128 bits:
  if( val.i128[1] )
    {
    overflow = compute_error_overflow;
    }
  else
    {
    // We know that it has fewer than 128 bits.  But the remaining 128 bits need
    // to be less than 10^MAX_FIXED_POINT_DIGITS.  This gets a bit nasty here,
    // since at this writing the gcc compiler doesn't understand 128-bit
    // constants.  So, we are forced into some annoying compiler gymnastics.
#if MAX_FIXED_POINT_DIGITS != 37
#error MAX_FIXED_POINT_DIGITS needs to be 37
#endif

    // These sixteen bytes comprise the binary value of 10^38
    static const uint8_t C1038[] = {0x00, 0x00, 0x00, 0x00, 0x40, 0x22, 0x8a, 0x09,
                                0x7a, 0xc4, 0x86, 0x5a, 0xa8, 0x4c, 0x3b, 0x4b};
    static const uint128 biggest = *reinterpret_cast<const uint128 *>(C1038);

    // If we still have some rdigits to throw away, we can keep shrinking
    // the value:

    while(rdigits > 0 && val.i128[0] >= biggest  )
      {
      divide_int256_by_int64(val, 10UL);
      rdigits -= 1;
      }

    // And we have to make sure that rdigits isn't too big

    while(rdigits > MAX_FIXED_POINT_DIGITS)
      {
      divide_int256_by_int64(val, 10UL);
      rdigits -= 1;
      }

    if( val.i128[0] >= biggest )
      {
      overflow = compute_error_overflow;
      }
    }

  if( is_negative )
    {
    negate_int256(val);
    }

  return overflow;
  }

static void
get_int256_from_qualified_field(int256 &var,
                                int &rdigits,
                          const cblc_field_t *field,
                                size_t field_o,
                                size_t field_s)
  {
  var.i128[0] = (uint128)__gg__binary_value_from_qualified_field( &rdigits,
                                                                  field,
                                                                  field_o,
                                                                  field_s);
  if( var.data[15] & 0x80 )
    {
    // This value is negative, so extend the sign bit:
    memset(var.data + 16, 0xFF, 16);
    }
  else
    {
    // This value is positive
    memset(var.data + 16, 0x00, 16);
    }
  }

static int256 phase1_result;
static int    phase1_rdigits;

static GCOB_FP128 phase1_result_float;

extern "C"
void
__gg__add_fixed_phase1( cbl_arith_format_t ,
                        size_t nA,
                        size_t ,
                        size_t ,
                  const cbl_round_t  *,
                        int           ,
                        int          *compute_error
                        )
  {
  // Our job is to add together the nA fixed-point values in the A[] array

  // The result goes into the temporary phase1_result.

        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;

  // Let us prime the pump with the first value of A[]
  get_int256_from_qualified_field(phase1_result, phase1_rdigits, A[0], A_o[0], A_s[0]);

  // We now go into a loop adding each of the A[] values to phase1_result:

  for( size_t i=1; i<nA; i++ )
    {
    int temp_rdigits;
    int256 temp = {};
    get_int256_from_qualified_field(temp, temp_rdigits, A[i], A_o[i], A_s[i]);

    // We have to scale the one with fewer rdigits to match the one with greater
    // rdigits:
    if( phase1_rdigits > temp_rdigits )
      {
      scale_int256_by_digits(temp, phase1_rdigits - temp_rdigits);
      }
    else if( phase1_rdigits < temp_rdigits )
      {
      scale_int256_by_digits(phase1_result, temp_rdigits - phase1_rdigits);
      phase1_rdigits = temp_rdigits;
      }

    // The two numbers have the same number of rdigits.  It's now safe to add
    // them.
    add_int256_to_int256(phase1_result, temp);
    }

  // phase1_result/phase1_rdigits now reflect the sum of all A[]

  int overflow = squeeze_int256(phase1_result, phase1_rdigits);
  if( overflow )
    {
    *compute_error |= compute_error_overflow;
    }
  }

extern "C"
void
__gg__addf1_fixed_phase2( cbl_arith_format_t ,
                          size_t ,
                          size_t ,
                          size_t ,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  // This is the assignment phase of an ADD Format 1

  // We take phase1_result and accumulate it into C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( C[0]->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)phase1_result.i128[0];
    value_a /= __gg__power_of_ten(phase1_rdigits);

    // Pick up the target
    GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);

    value_a += value_b;

    // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_a,
                                        *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating intoi a
    // fixed point target.
    int256 value_a   = phase1_result;
    int    rdigits_a = phase1_rdigits;

    int256 value_b = {};
    int rdigits_b;

    get_int256_from_qualified_field(value_b, rdigits_b, C[0], C_o[0], C_s[0]);

    // We have to scale the one with fewer rdigits to match the one with greater
    // rdigits:
    if( rdigits_a > rdigits_b )
      {
      scale_int256_by_digits(value_b, rdigits_a - rdigits_b);
      }
    else if( rdigits_a < rdigits_b )
      {
      scale_int256_by_digits(value_a, rdigits_b - rdigits_a);
      rdigits_a = rdigits_b;
      }

    // The two numbers have the same number of rdigits.  It's now safe to add
    // them.
    add_int256_to_int256(value_a, value_b);

    int overflow = squeeze_int256(value_a, rdigits_a);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

      // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_a.i128[0],
                                        rdigits_a,
                                        *rounded++);
    }
  }

extern "C"
void
__gg__fixed_phase2_assign_to_c( cbl_arith_format_t ,
                                size_t ,
                                size_t ,
                                size_t ,
                          const cbl_round_t  *rounded,
                                int           on_error_flag,
                                int          *compute_error
                                )
  {
  // This is the assignment phase of an ADD Format 2

        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;


  // We take phase1_result and put it into C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( C[0]->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)phase1_result.i128[0];
    value_a /= __gg__power_of_ten(phase1_rdigits);

    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_a,
                                       *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating intoi a
    // fixed point target.
    int256 value_a   = phase1_result;
    int    rdigits_a = phase1_rdigits;

    int overflow = squeeze_int256(value_a, rdigits_a);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

      // At this point, we assign that value to *C.
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_a.i128[0],
                                        rdigits_a,
                                       *rounded++);
    }
  }

extern "C"
void
__gg__add_float_phase1( cbl_arith_format_t ,
                        size_t nA,
                        size_t ,
                        size_t ,
                  const cbl_round_t  *,
                        int           ,
                        int          *compute_error
                        )
  {
  // Our job is to add together the nA floating-point values in the A[] array

  // The result goes into the temporary phase1_result_ffloat.

        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;

  // Let us prime the pump with the first value of A[]
  phase1_result_float = __gg__float128_from_qualified_field(A[0], A_o[0], A_s[0]);

  // We now go into a loop adding each of the A[] values to phase1_result_flt:

  for( size_t i=1; i<nA; i++ )
    {
    GCOB_FP128 temp = __gg__float128_from_qualified_field(A[i], A_o[i], A_s[i]);
    phase1_result_float = addition_helper_float(phase1_result_float,
                                                temp,
                                                compute_error);
    }
  }

extern "C"
void
__gg__addf1_float_phase2( cbl_arith_format_t ,
                          size_t ,
                          size_t ,
                          size_t ,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an ADD Format 2
  // We take phase1_result and accumulate it into C

  GCOB_FP128 temp = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
  temp = addition_helper_float(temp, phase1_result_float, compute_error);
  *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                      on_size_error,
                                      temp,
                                     *rounded++);
  }

extern "C"
void
__gg__float_phase2_assign_to_c( cbl_arith_format_t ,
                          size_t ,
                          size_t ,
                          size_t ,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an ADD Format 2
    // We take phase1_result and put it into C

  *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                      on_size_error,
                                      phase1_result_float,
                                     *rounded++);
  }

extern "C"
void
__gg__addf3(cbl_arith_format_t ,
            size_t nA,
            size_t ,
            size_t ,
      const cbl_round_t  *rounded,
            int           on_error_flag,
            int          *compute_error
            )
  {
  // This is an ADD Format 3.  Each A[i] gets accumulated into each C[i].  When
  // both are fixed, we do fixed arithmetic.  When either is a FldFloat, we
  // do floating-point arithmetic.
        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;

        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  for(size_t i=0; i<nA; i++)
    {
    if( A[i]->type == FldFloat || C[i]->type == FldFloat )
      {
      GCOB_FP128 value_a = __gg__float128_from_qualified_field(A[i], A_o[i], A_s[i]);
      GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[i], C_o[i], C_s[i]);

      value_a = addition_helper_float(value_a, value_b, compute_error);

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          value_a,
                                          *rounded++);
      }
    else
      {
      // We have are doing fixed-point arithmetic.
      int256 value_a;
      int    rdigits_a;

      int256 value_b;
      int rdigits_b;

      get_int256_from_qualified_field(value_a, rdigits_a, A[i], A_o[i], A_s[i]);
      get_int256_from_qualified_field(value_b, rdigits_b, C[i], C_o[i], C_s[i]);

      // We have to scale the one with fewer rdigits to match the one with greater
      // rdigits:
      if( rdigits_a > rdigits_b )
        {
        scale_int256_by_digits(value_b, rdigits_a - rdigits_b);
        }
      else if( rdigits_a < rdigits_b )
        {
        scale_int256_by_digits(value_a, rdigits_b - rdigits_a);
        rdigits_a = rdigits_b;
        }

      // The two numbers have the same number of rdigits.  It's now safe to add
      // them.
      add_int256_to_int256(value_a, value_b);

      int overflow = squeeze_int256(value_a, rdigits_a);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          value_a.i128[0],
                                          rdigits_a,
                                          *rounded++);
      }
    }
  }

extern "C"
void
__gg__subtractf1_fixed_phase2(cbl_arith_format_t ,
                              size_t ,
                              size_t ,
                              size_t ,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  // This is the assignment phase of an ADD Format 1

  // We take phase1_result and subtrace it from C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( C[0]->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)phase1_result.i128[0];
    value_a /= __gg__power_of_ten(phase1_rdigits);

    // Pick up the target
    GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);

    value_b -= value_a;

    // At this point, we assign the difference to *C.
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_b,
                                        *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating intoi a
    // fixed point target.
    int256 value_a   = phase1_result;
    int    rdigits_a = phase1_rdigits;

    int256 value_b = {};
    int rdigits_b;

    get_int256_from_qualified_field(value_b, rdigits_b, C[0], C_o[0], C_s[0]);

    // We have to scale the one with fewer rdigits to match the one with greater
    // rdigits:
    if( rdigits_a > rdigits_b )
      {
      scale_int256_by_digits(value_b, rdigits_a - rdigits_b);
      rdigits_b = rdigits_a;
      }
    else if( rdigits_a < rdigits_b )
      {
      scale_int256_by_digits(value_a, rdigits_b - rdigits_a);
      }

    // The two numbers have the same number of rdigits.  It's now safe to add
    // them.
    subtract_int256_from_int256(value_b, value_a);

    int overflow = squeeze_int256(value_b, rdigits_b);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

      // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        value_b.i128[0],
                                        rdigits_b,
                                        *rounded++);
    }
  }

extern "C"
void
__gg__subtractf2_fixed_phase1(cbl_arith_format_t ,
                              size_t nA,
                              size_t ,
                              size_t ,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  // This is the calculation phase of a fixed-point SUBTRACT Format 2

        cblc_field_t **B  = __gg__treeplet_2f;
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;

  // Add up all the A values
  __gg__add_fixed_phase1( not_expected_e ,
                          nA,
                          0,
                          0,
                          rounded,
                          on_error_flag,
                          compute_error);

  // Subtract that from the B value:

  int256 value_a   = phase1_result;
  int    rdigits_a = phase1_rdigits;

  int256 value_b = {};
  int rdigits_b;

  get_int256_from_qualified_field(value_b, rdigits_b, B[0], B_o[0], B_s[0]);

  // We have to scale the one with fewer rdigits to match the one with greater
  // rdigits:
  if( rdigits_a > rdigits_b )
    {
    scale_int256_by_digits(value_b, rdigits_a - rdigits_b);
    rdigits_b = rdigits_a;
    }
  else if( rdigits_a < rdigits_b )
    {
    scale_int256_by_digits(value_a, rdigits_b - rdigits_a);
    }

  // The two numbers have the same number of rdigits.  It's now safe to add
  // them.
  subtract_int256_from_int256(value_b, value_a);

  int overflow = squeeze_int256(value_b, rdigits_b);
  if( overflow )
    {
    *compute_error |= compute_error_overflow;
    }
  phase1_result  = value_b;
  phase1_rdigits = rdigits_b;
  }

extern "C"
void
__gg__subtractf1_float_phase2(cbl_arith_format_t ,
                              size_t ,
                              size_t ,
                              size_t ,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an ADD Format 2
  // We take phase1_result and subtract it from C

  GCOB_FP128 temp = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
  temp = subtraction_helper_float(temp, phase1_result_float, compute_error);
  *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                      on_size_error,
                                      temp,
                                     *rounded++);
  }

extern "C"
void
__gg__subtractf2_float_phase1(cbl_arith_format_t ,
                              size_t nA,
                              size_t ,
                              size_t ,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  // This is the calculation phase of a fixed-point SUBTRACT Format 2

        cblc_field_t **B  = __gg__treeplet_2f;
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;

  // Add up all the A values
  __gg__add_float_phase1( not_expected_e ,
                          nA,
                          0,
                          0,
                          rounded,
                          on_error_flag,
                          compute_error
                          );

  // Subtract that from the B value:
  GCOB_FP128 value_b = __gg__float128_from_qualified_field(B[0], B_o[0], B_s[0]);

  // The two numbers have the same number of rdigits.  It's now safe to add
  // them.
  phase1_result_float = subtraction_helper_float(value_b, phase1_result_float, compute_error);
  }

extern "C"
void
__gg__subtractf3( cbl_arith_format_t ,
                  size_t nA,
                  size_t ,
                  size_t ,
            const cbl_round_t  *rounded,
                  int           on_error_flag,
                  int          *compute_error
                  )
  {
  // This is an ADD Format 3.  Each A[i] gets accumulated into each C[i].  Each
  // SUBTRACTION is treated separately.

        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  for(size_t i=0; i<nA; i++)
    {
    if( A[i]->type == FldFloat || C[i]->type == FldFloat)
      {
      GCOB_FP128 value_a = __gg__float128_from_qualified_field(A[i], A_o[i], A_s[i]);
      GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[i], C_o[i], C_s[i]);

      value_b = subtraction_helper_float(value_b, value_a, compute_error);

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          value_b,
                                          *rounded++);
      }
    else
      {
      // We are doing fixed-point subtraction.
      int256 value_a;
      int    rdigits_a;

      int256 value_b;
      int rdigits_b;

      get_int256_from_qualified_field(value_a, rdigits_a, A[i], A_o[i], A_s[i]);
      get_int256_from_qualified_field(value_b, rdigits_b, C[i], C_o[i], C_s[i]);

      // We have to scale the one with fewer rdigits to match the one with greater
      // rdigits:
      if( rdigits_a > rdigits_b )
        {
        scale_int256_by_digits(value_b, rdigits_a - rdigits_b);
        rdigits_b = rdigits_a;
        }
      else if( rdigits_a < rdigits_b )
        {
        scale_int256_by_digits(value_a, rdigits_b - rdigits_a);
        }

      // The two numbers have the same number of rdigits.  It's now safe to add
      // them.
      subtract_int256_from_int256(value_b, value_a);

      int overflow = squeeze_int256(value_b, rdigits_b);

      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          value_b.i128[0],
                                          rdigits_b,
                                          *rounded++);
      }
    }
  }

static bool multiply_intermediate_is_float;
static GCOB_FP128 multiply_intermediate_float;
static __int128  multiply_intermediate_int128;
static int       multiply_intermediate_rdigits;

extern "C"
void
__gg__multiplyf1_phase1(cbl_arith_format_t ,
                        size_t ,
                        size_t ,
                        size_t ,
                  const cbl_round_t  *,
                        int           ,
                        int          *)
  {
  // We are getting just the one value, which we are converting to the necessary
  // intermediate form

        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;

  if( A[0]->type == FldFloat )
    {
    multiply_intermediate_is_float = true;
    multiply_intermediate_float = __gg__float128_from_qualified_field(A[0],
                                                                      A_o[0],
                                                                      A_s[0]);
    }
  else
    {
    multiply_intermediate_is_float = false;
    multiply_intermediate_int128 =
         __gg__binary_value_from_qualified_field(&multiply_intermediate_rdigits,
                                                 A[0],
                                                 A_o[0],
                                                 A_s[0]);
    }
  }

static
void multiply_int128_by_int128(int256 &ABCD,
                               __int128 ab_value,
                               __int128 cd_value)
  {
  int is_negative = ( (PTRCAST(uint8_t, (&ab_value)))[15]
                                ^(PTRCAST(uint8_t, (&cd_value)))[15]) & 0x80;
  if( ab_value < 0 )
    {
    ab_value = -ab_value;
    }
  if( cd_value < 0 )
    {
    cd_value = -cd_value;
    }

  uint128 AC00;
  uint128 AD0;
  uint128 BC0;
  uint128 BD;

  // Let's extract the digits.
  uint64_t a = *PTRCAST(uint64_t, (PTRCAST(unsigned char, (&ab_value))+8));
  uint64_t b = *PTRCAST(uint64_t, (PTRCAST(unsigned char, (&ab_value))+0));
  uint64_t c = *PTRCAST(uint64_t, (PTRCAST(unsigned char, (&cd_value))+8));
  uint64_t d = *PTRCAST(uint64_t, (PTRCAST(unsigned char, (&cd_value))+0));

  // multiply (a0 + b) * (c0 + d)

  AC00 = (uint128)a * c;
  AD0  = (uint128)a * d;
  BC0  = (uint128)b * c;
  BD   = (uint128)b * d;

  // ABCD is the sum of those four pieces
  int256 temp;

  ABCD.i128[1] = 0;
  ABCD.i128[0] = BD;

  temp.i64[3] = 0;
  memcpy(&temp.i64[1], &BC0, 16);
  temp.i64[0] = 0;
  add_int256_to_int256(ABCD, temp);

  memcpy(&temp.i64[1], &AD0, 16);
  add_int256_to_int256(ABCD, temp);

  temp.i64[1] = 0;
  memcpy(&temp.i64[2], &AC00, 16);
  add_int256_to_int256(ABCD, temp);

  // ABCD is now a 256-bit integer with rdigits decimal places
  if(is_negative)
    {
    negate_int256(ABCD);
    }
  }


extern "C"
void
__gg__multiplyf1_phase2(cbl_arith_format_t ,
                        size_t ,
                        size_t ,
                        size_t ,
                  const cbl_round_t  *rounded,
                        int           on_error_flag,
                        int          *compute_error
                        )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  GCOB_FP128 a_value;
  GCOB_FP128 b_value;

  if( multiply_intermediate_is_float )
    {
    a_value = multiply_intermediate_float;
    if( C[0]->type == FldFloat )
      {
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    else
      {
      // float times fixed
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    }
  else
    {
    if( C[0]->type == FldFloat )
      {
      // gixed * float
      a_value = (GCOB_FP128) multiply_intermediate_int128;
      if( multiply_intermediate_rdigits )
        {
        a_value /= (GCOB_FP128)__gg__power_of_ten(multiply_intermediate_rdigits);
        }
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    else
      {
      // fixed times fixed

      // We have two 128-bit numbers.  Call them AB and CD, where A, B, C, D are
      // 64-bit "digits".  We need to multiply them to create a 256-bit result

      int cd_rdigits;
      __int128 ab_value = multiply_intermediate_int128;
      __int128 cd_value = __gg__binary_value_from_qualified_field(&cd_rdigits, C[0], C_o[0], C_s[0]);

      int256 ABCD;
      int rdigits = multiply_intermediate_rdigits + cd_rdigits;

      multiply_int128_by_int128(ABCD, ab_value, cd_value);

      int overflow = squeeze_int256(ABCD, rdigits);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }
        // At this point, we assign running_sum to *C.
      *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                          on_size_error,
                                          ABCD.i128[0],
                                          rdigits,
                                          *rounded++);

      goto done;
      }
    }
  float_float:

  a_value = multiply_helper_float(a_value, b_value, &error_this_time);

  if( error_this_time && on_size_error)
    {
    *compute_error |= error_this_time;
    }
  else
    {
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        a_value,
                                        *rounded);
    }
  done:
  return;
  }

extern "C"
void
__gg__multiplyf2( cbl_arith_format_t ,
                  size_t ,
                  size_t ,
                  size_t nC,
            const cbl_round_t  *rounded,
                  int           on_error_flag,
                  int          *compute_error
                  )
  {
        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;
        cblc_field_t **B  = __gg__treeplet_2f;
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  bool      got_float = false;
  GCOB_FP128 product_float;
  int256    product_fix;
  int       product_fix_digits;

  if( A[0]->type == FldFloat || B[0]->type == FldFloat )
    {
    GCOB_FP128 a_value = __gg__float128_from_qualified_field(A[0], A_o[0], A_s[0]);
    GCOB_FP128 b_value = __gg__float128_from_qualified_field(B[0], B_o[0], B_s[0]);
    product_float = multiply_helper_float(a_value, b_value, compute_error);
    got_float = true;
    }
  else
    {
    int a_rdigits;
    int b_rdigits;
    __int128 a_value = __gg__binary_value_from_qualified_field(&a_rdigits, A[0], A_o[0], A_s[0]);
    __int128 b_value = __gg__binary_value_from_qualified_field(&b_rdigits, B[0], B_o[0], B_s[0]);
    product_fix_digits = a_rdigits + b_rdigits;
    multiply_int128_by_int128(product_fix, a_value, b_value);
    int overflow = squeeze_int256(product_fix, product_fix_digits);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }
    }

  for(size_t i=0; i<nC; i++)
    {
    if( got_float )
      {
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          product_float,
                                          *rounded++);
      }
    else
      {
      *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                          on_size_error,
                                          product_fix.i128[0],
                                          product_fix_digits,
                                          *rounded++);
      }
    }
  }

static void
shift_in_place128(uint8_t *buf, int size, int bits)
  {
  // Assume that size in bytes is some multiple of sixteen.  That is, the
  // buffer we are shifting is made up of either two 128-bit bit values (for
  // an int256) or three 128-bit values (an int384)

  // "bits" is a value from zero to 127

  if( !bits )
    {
    return;
    }

  size_t places = size/16;  // This is either two or three

  uint128 temp;
  uint128 overflow = 0;

  uint128 *as128 = PTRCAST(uint128, buf);

  for( size_t i=0; i<places; i++ )
    {
    temp = as128[i] >> (128 - bits);
    as128[i] <<= bits;
    as128[i] += overflow;
    overflow = temp;
    }
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

static char *
int256_as_decimal(int256 val)
  {
  char ach[120];
  memset(ach, 0, sizeof(ach));
  strcpy(ach, "0");
  int index = 0;
  while(val.i128[0] || val.i128[1])
    {
    int256 before;
    int256 after;
    before = val;
    after  = val;
    divide_int256_by_int64(after, 10);
    multiply_int256_by_int64(after, 10);
    uint64_t digit = before.i64[0] - after.i64[0];
    ach[index++] = digit + '0';
    divide_int256_by_int64(val, 10);
    }
  if( !index )
    {
    index = 1;
    }
  index -= 1;
  int r = 0;
  static char retval[120];
  while(index >= 0)
    {
    retval[r++] = ach[index--];
    }
  retval[r++] = '\0';
  return retval;
  }
#pragma GCC diagnostic pop


static void
divide_int128_by_int128(int256   &quotient,
                        int      &quotient_rdigits,
                        __int128  dividend,
                        int       dividend_rdigits,
                        __int128  divisor,
                        int       divisor_rdigits,
                        int      *compute_error)
  {
  if( divisor == 0 )
    {
    *compute_error |= compute_error_divide_by_zero;
    memset(&quotient, 0, sizeof(quotient));
    memcpy(&quotient, &dividend, 8);
    quotient_rdigits = dividend_rdigits;
    return;
    }

  bool is_negative = false;
  if(dividend < 0)
    {
    is_negative = !is_negative;
    dividend = -dividend;
    }
  if(divisor < 0)
    {
    is_negative = !is_negative;
    divisor = -divisor;
    }

  // We are going to be referencing the 64-bit pices of the 128-bit divisor:
  uint64_t *divisor64 = PTRCAST(uint64_t, &divisor);

  quotient.i128[1] = 0;
  quotient.i128[0] = dividend;

  // In order to get 0.3333333.... from 1 / 3, we are going to scale up the
  // numerator so that it has 37 rdigits:

  int scale = MAX_FIXED_POINT_DIGITS;
  scale_int256_by_digits(quotient, scale);
  quotient_rdigits = scale + dividend_rdigits - divisor_rdigits;

  // Now, let's see if we can do a simple divide-by-single-place calculation:

  if( divisor64[1] == 0 )
    {
    // Yes!  The divisor fits into 64 bits:
    divide_int256_by_int64(quotient, (uint64_t)divisor);
    }
  else
    {
    // We have to do long division, and that means Knuth's Algorithm D.  Let's
    // review where we are.
    //
    // We are using 64-bit places to divide one 128-bit number by another.  We
    // know that both are positive.  So, we are dividing
    //
    //      AB by CD, where we know C is non-zero.
    //
    // Because we want fractional digits to the right, we multiplied by 10**37,
    // which is smaller than 2**64, so we have one additional place:
    //
    //      ABx by CD
    //
    // Algorithm D requires that we left-shift ABX and CD by enough bits to make
    // turn on high-order by of CD.  This will be a value between 1 and 63
    // shifts, resulting in
    //
    //      ABxy by CD.
    //
    // We can know that the quotient will have at most two places.  We can see
    // this in the decimal analogy.  The worst case scenario is dividing
    //
    //      99 by 10
    //
    // We multiply the top by 10 to give us one fractional decimal place in the
    // result:
    //
    //      990 by 10
    //
    // To satisfy Algorithm D's requirement that C be >= b/2, we multiply both
    // dividend and divisor by 5:
    //
    //      4950 by 50
    //
    //  And then we do the work that gives us the two-place answer of 99.
    //
    //
    // Here is our four-place 256-bit "numerator"
    int256 numerator;

    // Copy the three-place ABx value into the numerator area
    memcpy(&numerator, &quotient, sizeof(quotient));

    // Calculate how many bits we need to shift CD to make the high-order bit
    // turn on:

    int bits_to_shift = 0;
    int i=15;
    while( (PTRCAST(uint8_t, &divisor))[i] == 0 )
      {
      i -= 1;
      bits_to_shift += 8;
      }    uint8_t tail = (  PTRCAST(uint8_t, &divisor)  )[i];
    while( !(tail & 0x80) )
      {
      bits_to_shift += 1;
      tail <<= 1;
      }

    // Shift both the numerator and the divisor that number of bits

    shift_in_place128( PTRCAST(uint8_t, &numerator), sizeof(numerator), bits_to_shift);
    shift_in_place128( PTRCAST(uint8_t, &divisor),   sizeof(divisor),   bits_to_shift);

    // We are now ready to do the guess-multiply-subtract loop.  We know that
    // the result will have two places, so we know we are going to go through
    // that loop two times.  We will build the quotient from the high-order
    // place down:

    // Let's prime the pump by loading remnant with the A value of ABxyz
    int q_place = 1;

    memset(&quotient, 0, sizeof(quotient));

    while( q_place >= 0 )
      {
      // We develop our guess for a quotient by dividing the top two places of
      // the numerator area by C
      uint128 temp;
      uint64_t *temp64 = PTRCAST(uint64_t, &temp);

      temp64[1] = numerator.i64[q_place+2];
      temp64[0] = numerator.i64[q_place+1];

      quotient.i64[q_place] = temp / divisor64[1];

      // Now we multiply our 64-bit guess by the 128-bit CD to get the
      // three-place value we are going to subtract from the numerator area.

      uint64_t subber[3];
      subber[2] = 0;

      // Start with the bottom 128 bits of the "subber"
      *PTRCAST(uint128, subber) = (uint128) divisor64[0] * quotient.i64[q_place];

      // Get the next 128 bits of subber
      temp = (uint128) divisor64[1] * quotient.i64[q_place];

      // Add the top of the first product to the bottom of the second:
      subber[1] += temp64[0];

      // See if there was an overflow:
      if( subber[1] < temp64[0] )
        {
        // There was an overflow
        subber[2] = 1;
        }
      // And now put the top of the second product into place:
      subber[2] += temp64[1];

      // "subber" is now the three-place product of the two-place divisor times
      // the one-place guess

      // We now subtract the three-place subber from the appropriate place of
      // the numerator:

      uint64_t borrow = 0;
      for(size_t j=0; j<3; j++)
        {
        if( numerator.i64[q_place + j] == 0 && borrow )
          {
          // We are subtracting from zero and we have a borrow.  Leave the
          // borrow on and just do the subtraction:
          numerator.i64[q_place + j] -= subber[j];
          }
        else
          {
          uint64_t stash = numerator.i64[q_place + j];
          numerator.i64[q_place + j] -= borrow;
          numerator.i64[q_place + j] -= subber[j];
          if( numerator.i64[q_place + j] > stash )
            {
            // After subtracting, the value got bigger, which means we have
            // to borrow from the next value to the left
            borrow = 1;
            }
          else
            {
            borrow = 0;
            }
          }
        }
      // The three-place subber has been removed from the numerator.

      // Now Algorithm D comes back into play.  Knuth has proved that the guess
      // is usually correct, but sometimes can be one too large, which means
      // that the numerator area goes negative.  On rare occasions, the guess can
      // be two too large.  So, we have to make sure that the numerator area is
      // actually positive by adding subber back in.

      while( numerator.i64[q_place+2] & 0x8000000000000000UL )
        {
        // We need to add subber back into the numerator area
        uint64_t carry = 0;
        for(size_t ii=0; ii<3; ii++)
          {
          if( numerator.i64[q_place + ii] == 0xFFFFFFFFFFFFFFFFUL && carry )
            {
            // We are at the top and have a carry.  Just leave the carry on
            // and do the addition:
            numerator.i64[q_place + ii] += subber[ii];
            }
          else
            {
            // We are not at the top.
            uint64_t stash = numerator.i64[q_place + ii];
            numerator.i64[q_place + ii] += carry;
            numerator.i64[q_place + ii] += subber[ii];
            if( numerator.i64[q_place + ii] < stash )
              {
              // The addition caused the result to get smaller, meaning that
              // we wrapped around:
              carry = 1;
              }
            else
              {
              carry = 0;
              }
            }
          }
        }
      q_place -= 1;
      }
    }
  if( is_negative )
    {
    negate_int256(quotient);
    }
  }

extern "C"
void
__gg__dividef1_phase2(cbl_arith_format_t ,
                      size_t ,
                      size_t ,
                      size_t ,
                const cbl_round_t  *rounded,
                      int           on_error_flag,
                      int          *compute_error
                      )
  {
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  GCOB_FP128 a_value;
  GCOB_FP128 b_value;

  if( multiply_intermediate_is_float )
    {
    a_value = multiply_intermediate_float;
    if( C[0]->type == FldFloat )
      {
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    else
      {
      // float times fixed
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    }
  else
    {
    if( C[0]->type == FldFloat )
      {
      // gixed * float
      a_value = (GCOB_FP128) multiply_intermediate_int128;
      if( multiply_intermediate_rdigits )
        {
        a_value /= (GCOB_FP128)__gg__power_of_ten(multiply_intermediate_rdigits);
        }
      b_value = __gg__float128_from_qualified_field(C[0], C_o[0], C_s[0]);
      goto float_float;
      }
    else
      {
      // fixed times fixed

      // We have two 128-bit numbers.  Call them AB and CD, where A, B, C, D are
      // 64-bit "digits".  We need to multiply them to create a 256-bit result

      int dividend_rdigits;
      __int128 dividend = __gg__binary_value_from_qualified_field(&dividend_rdigits, C[0], C_o[0], C_s[0]);

      int quotient_rdigits;
      int256 quotient;

      divide_int128_by_int128(quotient,
                              quotient_rdigits,
                              dividend,
                              dividend_rdigits,
                              multiply_intermediate_int128,
                              multiply_intermediate_rdigits,
                              compute_error);

      int overflow = squeeze_int256(quotient, quotient_rdigits);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }
        // At this point, we assign the quotient to *C.
      *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                          on_size_error,
                                          quotient.i128[0],
                                          quotient_rdigits,
                                          *rounded++);

      goto done;
      }
    }
  float_float:

  b_value = divide_helper_float(b_value, a_value, &error_this_time);

  *compute_error |= error_this_time;

  if( error_this_time && on_size_error)
    {
    }
  else
    {
    *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                        on_size_error,
                                        b_value,
                                        *rounded);
    }
  done:
  return;
  }

extern "C"
void
__gg__dividef23(cbl_arith_format_t ,
                size_t ,
                size_t ,
                size_t nC,
          const cbl_round_t  *rounded,
                int           on_error_flag,
                int          *compute_error
                )
  {
        cblc_field_t **A  = __gg__treeplet_1f;
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;
        cblc_field_t **B  = __gg__treeplet_2f;
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;
        cblc_field_t **C  = __gg__treeplet_3f;
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  if( A[0]->type == FldFloat ||  B[0]->type == FldFloat  )
    {
    GCOB_FP128 a_value;
    GCOB_FP128 b_value;
    GCOB_FP128 c_value;
    a_value = __gg__float128_from_qualified_field(A[0], A_o[0], A_s[0]);
    b_value = __gg__float128_from_qualified_field(B[0], B_o[0], B_s[0]);
    c_value = divide_helper_float(a_value, b_value, &error_this_time);

    *compute_error |= error_this_time;
    if( !error_this_time )
      {
      for(size_t i=0; i<nC; i++)
        {
        *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                            on_size_error,
                                            c_value,
                                            *rounded++);
        }
      }
    }
  else
    {
    // fixed divided by fixed
    int dividend_rdigits;
    __int128 dividend = __gg__binary_value_from_qualified_field(&dividend_rdigits, A[0], A_o[0], A_s[0]);

    int divisor_rdigits;
    __int128 divisor = __gg__binary_value_from_qualified_field(&divisor_rdigits, B[0], B_o[0], B_s[0]);

    int quotient_rdigits;
    int256 quotient;

    divide_int128_by_int128(quotient,
                            quotient_rdigits,
                            dividend,
                            dividend_rdigits,
                            divisor,
                            divisor_rdigits,
                            compute_error);

    *compute_error |= squeeze_int256(quotient, quotient_rdigits);
    if( !*compute_error )
      {
        // At this point, we assign the quotient to *C.
      for(size_t i=0; i<nC; i++)
        {
        *compute_error |= conditional_stash(C[i], C_o[i], C_s[i],
                                            on_size_error,
                                            quotient.i128[0],
                                            quotient_rdigits,
                                            *rounded++);
        }
      }
    }
  }

extern "C"
void
__gg__dividef45(cbl_arith_format_t ,
                size_t ,
                size_t ,
                size_t ,
                cbl_round_t  *rounded_p,
                int           on_error_flag,
                int          *compute_error
                )
  {
        cblc_field_t **A  = __gg__treeplet_1f;  // Numerator
  const size_t       *A_o = __gg__treeplet_1o;
  const size_t       *A_s = __gg__treeplet_1s;
        cblc_field_t **B  = __gg__treeplet_2f;  // Denominator
  const size_t       *B_o = __gg__treeplet_2o;
  const size_t       *B_s = __gg__treeplet_2s;
        cblc_field_t **C  = __gg__treeplet_3f;  // Has remainder, then quotient
  const size_t       *C_o = __gg__treeplet_3o;
  const size_t       *C_s = __gg__treeplet_3s;

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  if( A[0]->type == FldFloat ||  B[0]->type == FldFloat  )
    {
    GCOB_FP128 a_value;
    GCOB_FP128 b_value;
    GCOB_FP128 c_value;
    a_value = __gg__float128_from_qualified_field(A[0], A_o[0], A_s[0]);
    b_value = __gg__float128_from_qualified_field(B[0], B_o[0], B_s[0]);
    c_value = divide_helper_float(a_value, b_value, &error_this_time);

    *compute_error |= error_this_time;

    if( !error_this_time )
      {
      *compute_error |= conditional_stash(C[1], C_o[1], C_s[1],
                                          on_size_error,
                                          c_value,
                                          *rounded_p++);
      // This is floating point, and there is a remainder, and we don't know
      // what that means.  Set the remainder to zero
      if( !*compute_error )
        {
        c_value = 0;
        *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                            on_size_error,
                                            c_value,
                                            *rounded_p++);
        }
      }
    }
  else
    {
    // fixed divided by fixed
    int dividend_rdigits;
    __int128 dividend = __gg__binary_value_from_qualified_field(&dividend_rdigits, A[0], A_o[0], A_s[0]);

    int divisor_rdigits;
    __int128 divisor = __gg__binary_value_from_qualified_field(&divisor_rdigits, B[0], B_o[0], B_s[0]);

    int quotient_rdigits;
    int256 quotient;

    divide_int128_by_int128(quotient,
                            quotient_rdigits,
                            dividend,
                            dividend_rdigits,
                            divisor,
                            divisor_rdigits,
                            compute_error);

    *compute_error |= squeeze_int256(quotient, quotient_rdigits);
    if( !*compute_error )
      {
      // We are going to need the unrounded quotient to calculate the remainder
      __int128 unrounded_quotient = 0;
      int      unrounded_quotient_digits;
      rounded_p += 1;// Skip the rounded value for the remainder
      cbl_round_t rounded = *rounded_p;
      switch(rounded)
        {
        case truncation_e:
          {
          *compute_error |= conditional_stash(C[1], C_o[1], C_s[1],
                                              on_size_error,
                                              quotient.i128[0],
                                              quotient_rdigits,
                                              *rounded_p++);
          unrounded_quotient = __gg__binary_value_from_qualified_field(
                                                        &unrounded_quotient_digits,
                                                        C[1], C_o[1], C_s[1]);
          break;
          }
        default:
          {
          conditional_stash(C[1], C_o[1], C_s[1],
                            false,
                            quotient.i128[0],
                            quotient_rdigits,
                            truncation_e);
          unrounded_quotient = __gg__binary_value_from_qualified_field(
                                                        &unrounded_quotient_digits,
                                                        C[1], C_o[1], C_s[1]);
          // At this point, we assign the rounded quotient to *C.
          *compute_error |= conditional_stash(C[1], C_o[1], C_s[1],
                                              on_size_error,
                                              quotient.i128[0],
                                              quotient_rdigits,
                                              *rounded_p++);
          break;
          }
        }
      if( !*compute_error )
        {
        // We need to calculate the remainder

        // Remainders in COBOL are seriously weird.  The NIST suite
        // has an example where 174 is divided by 16.  The quotient
        // is a 999.9, and the remainder is a 9999

        // So, here goes:  174 by 16 is 10.875.  The unrounded
        // assignment to Q is thus 10.8
        // You then multiply 10.8 by 16, giving 172.8
        // That gets subtracted from 174, giving 1.2
        // That gets assigned to the 9999 remainder, which is
        // thus 1

        // Any mathematician would walk away, slowly, shaking their head.

        // We need to multiply the unrounded quotient by the divisor.
        int256 temp;
        int    temp_rdigits;
        // Step 1: Multiply the unrounded quotient by the divisor
        multiply_int128_by_int128(temp, unrounded_quotient, divisor);
        temp_rdigits = unrounded_quotient_digits + divisor_rdigits;

        int256 odividend = {};
        memcpy(&odividend, &dividend, sizeof(dividend));

        // We need to line up the rdigits so that we can subtract temp from
        // odividend:

        if( temp_rdigits < dividend_rdigits )
          {
          scale_int256_by_digits(temp, dividend_rdigits-temp_rdigits);
          temp_rdigits = dividend_rdigits;
          }
        else if(temp_rdigits > dividend_rdigits)
          {
          scale_int256_by_digits(odividend, temp_rdigits-dividend_rdigits);
          }

        subtract_int256_from_int256(odividend, temp);

        *compute_error |= squeeze_int256(odividend, temp_rdigits);

        if( !*compute_error )
          {
          *compute_error |= conditional_stash(C[0], C_o[0], C_s[0],
                                              on_size_error,
                                              odividend.i128[0],
                                              temp_rdigits,
                                              truncation_e);
          }
        }
      }
    }
  }

