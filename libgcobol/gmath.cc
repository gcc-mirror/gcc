/*
 * Copyright (c) 2021-2026 Symas Corporation
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
    retval = __gg__int128_to_qualified_field(destination,
                                             destination_o,
                                             destination_s,
                                             value,
                                             rdigits,
                                             rounded);
    }
  else
    {
    // This is slightly more complex, because in the event of a
    // SIZE ERROR. we need to leave the original value untouched

    unsigned char *stash = static_cast<unsigned char *>(malloc(destination_s));
    massert(stash);
    memcpy(stash, destination->data+destination_o, destination_s);

    retval = __gg__int128_to_qualified_field(destination,
                                             destination_o,
                                             destination_s,
                                             value,
                                             rdigits,
                                             rounded);
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

static GCOB_FP128
exponentiation_helper(GCOB_FP128 avalue, GCOB_FP128 bvalue, int *compute_error)
  {
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
    if(  errno
      || fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW) )
      {
      // One of a large number of errors took place. See math_error(7) and
      // pow(3).  Let's just use this last error as a grab-bag; I didn't
      // care to go down the rabbit hole of figuring out if a floating point
      // number did or did not have a fractional part.  That way lies
      // madness.
      *compute_error |= compute_error_exp_minus_by_frac;
      // This kind of error doesn't overwrite the target, so the returned
      // value is not relevant.  Make it zero to avoid overheating the
      // conversion routine
      tgt_value = 0;
      }
    }
  return tgt_value;
  }

extern "C"
void
__gg__pow(  cbl_arith_format_t,
            size_t,
      const cblc_referlet_t *A,
            size_t,
      const cblc_referlet_t *B,
            size_t,
      const cblc_referlet_t *C,
      const cbl_round_t  *rounded,
            int           on_error_flag,
            int          *compute_error
            )
  {
  GCOB_FP128 avalue =
      __gg__float128_from_qualified_field(A[0].field, A[0].offset, A[0].size);
  GCOB_FP128 bvalue =
      __gg__float128_from_qualified_field(B[0].field, B[0].offset, B[0].size);

  GCOB_FP128 tgt_value = exponentiation_helper(avalue, bvalue, compute_error);

  if( !(*compute_error & compute_error_exp_minus_by_frac) )
    {
    *compute_error |= conditional_stash(C[0].field,
                                        C[0].offset,
                                        C[0].size,
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

/*
 * int256 is deliberately represented as logical 64-bit limbs, not as a
 * byte-level overlay.  i64[0] is always the least-significant limb and i64[3]
 * is always the most-significant limb, independent of the host byte order.
 */
typedef struct int256
  {
  uint64_t i64[4];
  int rdigits;
  } int256;

static inline uint64_t
uint128_lo64(uint128 value)
  {
  return static_cast<uint64_t>(value);
  }

static inline uint64_t
uint128_hi64(uint128 value)
  {
  return static_cast<uint64_t>(value >> 64);
  }

static inline uint128
uint128_from_limbs(uint64_t hi, uint64_t lo)
  {
  return (static_cast<uint128>(hi) << 64) | lo;
  }

static inline uint128
int256_get_u128(const int256 &value, int half)
  {
  const int base = half * 2;
  return uint128_from_limbs(value.i64[base + 1], value.i64[base]);
  }

static inline void
int256_set_u128(int256 &value, int half, uint128 replacement)
  {
  const int base = half * 2;
  value.i64[base + 0] = uint128_lo64(replacement);
  value.i64[base + 1] = uint128_hi64(replacement);
  }

static inline bool
int256_is_negative(const int256 &value)
  {
  return !!(value.i64[3] & 0x8000000000000000ULL);
  }

static inline uint128
int128_abs_as_uint128(__int128 value)
  {
  return value < 0
       ? static_cast<uint128>(-(value + 1)) + 1
       : static_cast<uint128>(value);
  }


static int
multiply_int256_by_int64(int256 &product, const uint64_t multiplier)
  {
  // Typical use of this routine is multiplying a temporary value by
  // a power of ten.  This is effectively left-shifting by decimal
  // digits.  See scale_int256_by_digits
  uint128 carry = 0;
  for(int i=0; i<4; i++)
    {
    uint128 temp = static_cast<uint128>(product.i64[i]) * multiplier + carry;
    product.i64[i] = uint128_lo64(temp);
    carry = temp >> 64;
    }
  // Indicate that an overflow took place.  This is not useful unless the
  // int256 is known to be positive.
  return carry != 0;
  }

static void
scale_int256_by_digits(int256 &val, int digits)
  {
  if( digits )
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
  }

static void
add_int256_to_int256(int256 &sum, const int256 &addend)
  {
  // We are accumulating addend into sum.

  // We have to scale the one with fewer rdigits to match the one with
  // greater rdigits.
  if( addend.rdigits >= sum.rdigits )
    {
    // This is the easier case.  We are accumulating into sum, so we can scale
    // it in place:
    scale_int256_by_digits(sum, addend.rdigits - sum.rdigits);
    sum.rdigits += addend.rdigits - sum.rdigits;
    uint128 carry = 0;
    for(int i=0; i<4; i++)
      {
      uint128 temp = static_cast<uint128>(sum.i64[i]) + addend.i64[i] + carry;
      sum.i64[i] = uint128_lo64(temp);
      carry = temp >> 64;
      }
    }
  else
    {
    // We aren't rude enough to change addend.  Besides, we declared it const,
    // just in case we lost our heads.
    int256 addend2 = addend;
    scale_int256_by_digits(addend2, sum.rdigits - addend2.rdigits);
    uint128 carry = 0;
    for(int i=0; i<4; i++)
      {
      uint128 temp = static_cast<uint128>(sum.i64[i]) + addend2.i64[i] + carry;
      sum.i64[i] = uint128_lo64(temp);
      carry = temp >> 64;
      }
    }
  }

static void
negate_int256(int256 &val)
  {
  for(int i=0; i<4; i++)
    {
    val.i64[i] = ~val.i64[i];
    }
  for(int i=0; i<4; i++)
    {
    val.i64[i] += 1;
    if( val.i64[i] )
      {
      break;
      }
    }
  }

static void
subtract_int256_from_int256(int256 &difference, int256 subtrahend)
  {
  negate_int256(subtrahend);
  add_int256_to_int256(difference, subtrahend);
  }

static void
divide_int256_by_int64(int256 &val, uint64_t divisor)
  {
  // val needs to be a positive number
  uint128 remainder = 0;
  for( int i=3; i>=0; i-- )
    {
    uint128 combined = (remainder << 64) | val.i64[i];
    val.i64[i] = static_cast<uint64_t>(combined / divisor);
    remainder = combined % divisor;
    }
  }

static int
squeeze_int256(int256 &val)
  {
  int overflow = 0;
  // It has been decreed that at this juncture the result must fit into
  // MAX_FIXED_POINT_DIGITS.  If the result does not, we have an OVERFLOW
  // error.

  int is_negative = int256_is_negative(val);
  if( is_negative )
    {
    negate_int256(val);
    }

  // As long as there are some decimal places left, we hold our nose and
  // right-shift a too-large value rightward by decimal digits.  In other
  // words, we truncate the fractional part to make room for the integer part:
  while(val.rdigits > 0 && int256_get_u128(val, 1) )
    {
    divide_int256_by_int64(val, 10UL);
    val.rdigits -= 1;
    }

  // At this point, to be useful, val has to have fewer than 128 bits:
  if( int256_get_u128(val, 1) )
    {
    overflow = compute_error_overflow;
    }
  else
    {
    // We know that it has fewer than 128 bits.  But the remaining 128 bits
    // need to be less than 10^MAX_FIXED_POINT_DIGITS.  This gets a bit nasty
    // here, since at this writing the gcc compiler doesn't understand 128-bit
    // constants.  So, we are forced into some annoying compiler gymnastics.
#if MAX_FIXED_POINT_DIGITS != 37
#error MAX_FIXED_POINT_DIGITS needs to be 37
#endif

    // Binary value of 10^38, written as two 64-bit limbs so that the value is
    // independent of the host byte order and does not require type punning.
    static const uint128 biggest =
        (static_cast<uint128>(0x4b3b4ca85a86c47aULL) << 64)
    // cppcheck-suppress badBitmaskCheck
      |  static_cast<uint128>(0x098a224000000000ULL);

    // If we still have some val.rdigits to throw away, we can keep shrinking
    // the value:

    while(val.rdigits > 0 && int256_get_u128(val, 0) >= biggest  )
      {
      divide_int256_by_int64(val, 10UL);
      val.rdigits -= 1;
      }

    // And we have to make sure that val.rdigits isn't too big

    while(val.rdigits > MAX_FIXED_POINT_DIGITS)
      {
      divide_int256_by_int64(val, 10UL);
      val.rdigits -= 1;
      }

    if( int256_get_u128(val, 0) >= biggest )
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
                          const cblc_field_t *field,
                                size_t field_o,
                                size_t field_s)
  {
  int128 incoming;
  __gg__int128_from_qualified_field(incoming,
                                    field,
                                    field_o,
                                    field_s);
  var.rdigits = incoming.rdigits;
  int256_set_u128(var, 0, static_cast<uint128>(incoming.i128));
  if( incoming.i128 < 0 )
    {
    // This value is negative, so extend the sign bit:
    var.i64[2] = UINT64_MAX;
    var.i64[3] = UINT64_MAX;
    }
  else
    {
    // This value is positive
    var.i64[2] = 0;
    var.i64[3] = 0;
    }
  }

static int256 phase1_result;

static GCOB_FP128 phase1_result_float;

extern "C"
void
__gg__add_fixed_phase1( cbl_arith_format_t ,
                        size_t nA,
                  const cblc_referlet_t *AA,
                        size_t ,
                        cblc_referlet_t *,
                        size_t ,
                        cblc_referlet_t *,
                  const cbl_round_t  *,
                        int           ,
                        int          *compute_error
                        )
  {
  // Our job is to add together the nA fixed-point values in the A[] array

  // The result goes into the temporary phase1_result.

  // Let us prime the pump with the first value of A[]
  get_int256_from_qualified_field(phase1_result,
                                  AA[0].field,
                                  AA[0].offset,
                                  AA[0].size);

  // We now go into a loop adding each of the A[] values to phase1_result:

  for( size_t i=1; i<nA; i++ )
    {
    int256 temp = {};
    get_int256_from_qualified_field(temp,
                                    AA[i].field,
                                    AA[i].offset,
                                    AA[i].size);

    add_int256_to_int256(phase1_result, temp);
    }

  // phase1_result/phase1_result.rdigits now reflect the sum of all A[]

  int overflow = squeeze_int256(phase1_result);
  if( overflow )
    {
    *compute_error |= compute_error_overflow;
    }
  }

extern "C"
void
__gg__addf1_fixed_phase2( cbl_arith_format_t ,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                    const cblc_referlet_t *C,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
  // This is the assignment phase of an ADD Format 1

  // We take phase1_result and accumulate it into C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( C[0].field->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)int256_get_u128(phase1_result, 0);
    value_a /= __gg__power_of_ten(phase1_result.rdigits);

    // Pick up the target
    GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[0].field,
                                                             C[0].offset,
                                                             C[0].size);

    value_a += value_b;

    // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0].field,
                                        C[0].offset,
                                        C[0].size,
                                        on_size_error,
                                        value_a,
                                        *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating into a
    // fixed point target.
    int256 value_a   = phase1_result;
    int256 value_b = {};

    value_a.rdigits = phase1_result.rdigits;

    get_int256_from_qualified_field(value_b,
                                    C[0].field,
                                    C[0].offset,
                                    C[0].size);
    add_int256_to_int256(value_a, value_b);

    int overflow = squeeze_int256(value_a);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

      // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0].field,
                                        C[0].offset,
                                        C[0].size,
                                        on_size_error,
                                        int256_get_u128(value_a, 0),
                                        value_a.rdigits,
                                        *rounded++);
    }
  }

extern "C"
void
__gg__fixed_phase2_assign_to_c( cbl_arith_format_t ,
                                size_t ,
                                cblc_referlet_t *,
                                size_t ,
                                cblc_referlet_t *,
                                size_t ,
                          const cblc_referlet_t *CC,
                          const cbl_round_t  *rounded,
                                int           on_error_flag,
                                int          *compute_error
                                )
  {
  // This is the assignment phase of an ADD or SUBTRACT Format 2

  // We take phase1_result and put it into C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( CC[0].field->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)int256_get_u128(phase1_result, 0);
    value_a /= __gg__power_of_ten(phase1_result.rdigits);

    *compute_error |= conditional_stash(CC[0].field, CC[0].offset, CC[0].size,
                                        on_size_error,
                                        value_a,
                                       *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating intoi a
    // fixed point target.
    int256 value_a   = phase1_result;
    value_a.rdigits = phase1_result.rdigits;

    int overflow = squeeze_int256(value_a);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

    if( CC[0].field->type == FldPointer )
      {
      // In case somebody does pointer arithmetic that goes negative, we need
      // to make the top 64 bits positive.  Otherwise, the conditional stash
      // will see that FldPointer is not signable, and force the value
      // positive with a two's complement.
      int256_set_u128(value_a,
                      0,
                      int256_get_u128(value_a, 0) & 0xFFFFFFFFFFFFFFFFULL);
      }

      // At this point, we assign that value to *C.
    *compute_error |= conditional_stash(CC[0].field, CC[0].offset, CC[0].size,
                                        on_size_error,
                                        int256_get_u128(value_a, 0),
                                        value_a.rdigits,
                                       *rounded++);
    }
  }

extern "C"
void
__gg__add_float_phase1( cbl_arith_format_t ,
                        size_t nA,
                  const cblc_referlet_t *A,
                        size_t ,
                        cblc_referlet_t *,
                        size_t ,
                        cblc_referlet_t *,
                  const cbl_round_t  *,
                        int           ,
                        int          *compute_error
                        )
  {
  // Our job is to add together the nA floating-point values in the A[] array

  // The result goes into the temporary phase1_result_ffloat.

  // Let us prime the pump with the first value of A[]
  phase1_result_float = __gg__float128_from_qualified_field(A[0].field,
                                                            A[0].offset,
                                                            A[0].size);

  // We now go into a loop adding each of the A[] values to phase1_result_flt:

  for( size_t i=1; i<nA; i++ )
    {
    GCOB_FP128 temp = __gg__float128_from_qualified_field(A[i].field,
                                                          A[i].offset,
                                                          A[i].size);
    phase1_result_float = addition_helper_float(phase1_result_float,
                                                temp,
                                                compute_error);
    }
  }

extern "C"
void
__gg__addf1_float_phase2( cbl_arith_format_t ,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                    const cblc_referlet_t *C,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an ADD Format 2
  // We take phase1_result and accumulate it into C

  GCOB_FP128 temp = __gg__float128_from_qualified_field(C[0].field,
                                                        C[0].offset,
                                                        C[0].size);
  temp = addition_helper_float(temp, phase1_result_float, compute_error);
  *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                      on_size_error,
                                      temp,
                                     *rounded++);
  }

extern "C"
void
__gg__float_phase2_assign_to_c( cbl_arith_format_t ,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                          cblc_referlet_t *,
                          size_t ,
                    const cblc_referlet_t *C,
                    const cbl_round_t  *rounded,
                          int           on_error_flag,
                          int          *compute_error
                          )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an ADD Format 2
    // We take phase1_result and put it into C

  *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                      on_size_error,
                                      phase1_result_float,
                                     *rounded++);
  }

extern "C"
void
__gg__addf3(cbl_arith_format_t ,
            size_t nA,
       const cblc_referlet_t *A,
            size_t ,
            cblc_referlet_t *,
            size_t ,
       const cblc_referlet_t *C,
      const cbl_round_t  *rounded,
            int           on_error_flag,
            int          *compute_error
            )
  {
  // This is an ADD Format 3.  Each A[i] gets accumulated into each C[i].  When
  // both are fixed, we do fixed arithmetic.  When either is a FldFloat, we
  // do floating-point arithmetic.
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  for(size_t i=0; i<nA; i++)
    {
    if( A[i].field->type == FldFloat || C[i].field->type == FldFloat )
      {
      GCOB_FP128 value_a = __gg__float128_from_qualified_field(A[i].field,
                                                               A[i].offset,
                                                               A[i].size);
      GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[i].field,
                                                               C[i].offset,
                                                               C[i].size);

      value_a = addition_helper_float(value_a, value_b, compute_error);

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          value_a,
                                          *rounded++);
      }
    else
      {
      // We have are doing fixed-point arithmetic.
      int256 value_a;
      int256 value_b;

      get_int256_from_qualified_field(value_a,
                                      A[i].field,
                                      A[i].offset,
                                      A[i].size);
      get_int256_from_qualified_field(value_b,
                                      C[i].field,
                                      C[i].offset,
                                      C[i].size);
      add_int256_to_int256(value_a, value_b);

      int overflow = squeeze_int256(value_a);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          int256_get_u128(value_a, 0),
                                          value_a.rdigits,
                                          *rounded++);
      }
    }
  }

extern "C"
void
__gg__subtractf1_fixed_phase2(cbl_arith_format_t ,
                              size_t ,
                              cblc_referlet_t *,
                              size_t ,
                              cblc_referlet_t *,
                              size_t ,
                        const cblc_referlet_t *C,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  // This is the assignment phase of an ADD Format 1

  // We take phase1_result and subtrace it from C
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  if( C[0].field->type == FldFloat)
    {
    // The target we need to accumulate into is a floating-point number, so we
    // need to convert our fixed-point intermediate into floating point and
    // proceed accordingly.

    // Convert the intermediate
    GCOB_FP128 value_a = (GCOB_FP128)int256_get_u128(phase1_result, 0);
    value_a /= __gg__power_of_ten(phase1_result.rdigits);

    // Pick up the target
    GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[0].field,
                                                             C[0].offset,
                                                             C[0].size);

    value_b -= value_a;

    // At this point, we assign the difference to *C.
    *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                        on_size_error,
                                        value_b,
                                        *rounded++);
    }
  else
    {
    // We have a fixed-point intermediate, and we are accumulating intoi a
    // fixed point target.
    int256 value_a   = phase1_result;
    value_a.rdigits = phase1_result.rdigits;

    int256 value_b = {};

    get_int256_from_qualified_field(value_b,
                                    C[0].field,
                                    C[0].offset,
                                    C[0].size);

    subtract_int256_from_int256(value_b, value_a);

    int overflow = squeeze_int256(value_b);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }

      // At this point, we assign running_sum to *C.
    *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                        on_size_error,
                                        int256_get_u128(value_b, 0),
                                        value_b.rdigits,
                                        *rounded++);
    }
  }

extern "C"
void
__gg__subtractf2_fixed_phase1(cbl_arith_format_t ,
                              size_t nA,
                        const cblc_referlet_t *AA,
                              size_t ,
                        const cblc_referlet_t *BB,
                              size_t ,
                              cblc_referlet_t *,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  // This is the calculation phase of a fixed-point SUBTRACT Format 2

  // Add up all the A values
  __gg__add_fixed_phase1( not_expected_e ,
                          nA,
                          AA,
                          0,
                          NULL,
                          0,
                          NULL,
                          rounded,
                          on_error_flag,
                          compute_error);

  // Subtract the phase1_result from the B value:

  int256 value_a   = phase1_result;
  value_a.rdigits = phase1_result.rdigits;

  int256 value_b = {};

  get_int256_from_qualified_field(value_b,
                                  BB[0].field,
                                  BB[0].offset,
                                  BB[0].size);

  subtract_int256_from_int256(value_b, value_a);

  int overflow = squeeze_int256(value_b);
  if( overflow )
    {
    *compute_error |= compute_error_overflow;
    }
  phase1_result  = value_b;
  phase1_result.rdigits = value_b.rdigits;
  }

extern "C"
void
__gg__subtractf1_float_phase2(cbl_arith_format_t ,
                              size_t ,
                              cblc_referlet_t *,
                              size_t ,
                              cblc_referlet_t *,
                              size_t ,
                        const cblc_referlet_t *C,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  // This is the assignment phase of an SUBTRACT Format 2
  // We take phase1_result and subtract it from C

  GCOB_FP128 temp = __gg__float128_from_qualified_field(C[0].field,
                                                        C[0].offset,
                                                        C[0].size);
  temp = subtraction_helper_float(temp, phase1_result_float, compute_error);
  *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                      on_size_error,
                                      temp,
                                     *rounded++);
  }

extern "C"
void
__gg__subtractf2_float_phase1(cbl_arith_format_t ,
                              size_t nA,
                        const cblc_referlet_t *A,
                              size_t ,
                        const cblc_referlet_t *B,
                              size_t ,
                              cblc_referlet_t *,
                        const cbl_round_t  *rounded,
                              int           on_error_flag,
                              int          *compute_error
                              )
  {
  // This is the calculation phase of a fixed-point SUBTRACT Format 2

  // Add up all the A values
  __gg__add_float_phase1( not_expected_e ,
                          nA,
                          A,
                          0,
                          NULL,
                          0,
                          NULL,
                          rounded,
                          on_error_flag,
                          compute_error
                          );

  // Subtract that subtotal from the B value:
  GCOB_FP128 value_b = __gg__float128_from_qualified_field(B[0].field,
                                                           B[0].offset,
                                                           B[0].size);
  phase1_result_float = subtraction_helper_float(value_b,
                                                 phase1_result_float,
                                                 compute_error);
  }

extern "C"
void
__gg__subtractf3( cbl_arith_format_t ,
                  size_t nA,
            const cblc_referlet_t *A,
                  size_t ,
                  cblc_referlet_t *,
                  size_t ,
            const cblc_referlet_t *C,
            const cbl_round_t  *rounded,
                  int           on_error_flag,
                  int          *compute_error
                  )
  {
  // This is an ADD Format 3.  Each A[i] gets accumulated into each C[i].  Each
  // SUBTRACTION is treated separately.

  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  for(size_t i=0; i<nA; i++)
    {
    if( A[i].field->type == FldFloat || C[i].field->type == FldFloat)
      {
      GCOB_FP128 value_a = __gg__float128_from_qualified_field(A[i].field,
                                                               A[i].offset,
                                                               A[i].size);
      GCOB_FP128 value_b = __gg__float128_from_qualified_field(C[i].field,
                                                               C[i].offset,
                                                               C[i].size);

      value_b = subtraction_helper_float(value_b, value_a, compute_error);

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          value_b,
                                          *rounded++);
      }
    else
      {
      // We are doing fixed-point subtraction.
      int256 value_a;
      int256 value_b;

      get_int256_from_qualified_field(value_a,
                                      A[i].field,
                                      A[i].offset,
                                      A[i].size);
      get_int256_from_qualified_field(value_b,
                                      C[i].field,
                                      C[i].offset,
                                      C[i].size);

      subtract_int256_from_int256(value_b, value_a);

      int overflow = squeeze_int256(value_b);

      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }

        // At this point, we assign the sum to *C.
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          int256_get_u128(value_b, 0),
                                          value_b.rdigits,
                                          *rounded++);
      }
    }
  }

static bool multiply_intermediate_is_float;
static GCOB_FP128 multiply_intermediate_float;
static int128 multiply_intermediate_int128;

extern "C"
void
__gg__multiplyf1_phase1(cbl_arith_format_t ,
                        size_t ,
                  const cblc_referlet_t *A,
                        size_t ,
                        cblc_referlet_t *,
                        size_t ,
                        cblc_referlet_t *,
                  const cbl_round_t  *,
                        int           ,
                        int          *)
  {
  // We are getting just the one value, which we are converting to the
  // necessary intermediate form

  if( A[0].field->type == FldFloat )
    {
    multiply_intermediate_is_float = true;
    multiply_intermediate_float =
                               __gg__float128_from_qualified_field(A[0].field,
                                                                   A[0].offset,
                                                                   A[0].size);
    }
  else
    {
    multiply_intermediate_is_float = false;
    __gg__int128_from_qualified_field(multiply_intermediate_int128,
                                      A[0].field,
                                      A[0].offset,
                                      A[0].size);
    }
  }

static
void multiply_int128_by_int128(int256 &ABCD,
                               const int128 &ab_value,
                               const int128 &cd_value)
  {
  bool is_negative = (ab_value.i128 < 0) != (cd_value.i128 < 0);

  uint128 abs_ab = int128_abs_as_uint128(ab_value.i128);
  uint128 abs_cd = int128_abs_as_uint128(cd_value.i128);

  uint128 AC00;
  uint128 AD0;
  uint128 BC0;
  uint128 BD;

  // Let's extract the digits.
  uint64_t a = uint128_hi64(abs_ab);
  uint64_t b = uint128_lo64(abs_ab);
  uint64_t c = uint128_hi64(abs_cd);
  uint64_t d = uint128_lo64(abs_cd);

  // multiply (a0 + b) * (c0 + d)

  AC00 = (uint128)a * c;
  AD0  = (uint128)a * d;
  BC0  = (uint128)b * c;
  BD   = (uint128)b * d;

  // ABCD is the sum of those four pieces
  int256 temp;

  ABCD = int256{};
  int256_set_u128(ABCD, 0, BD);

  temp = int256{};
  temp.i64[1] = uint128_lo64(BC0);
  temp.i64[2] = uint128_hi64(BC0);
  add_int256_to_int256(ABCD, temp);

  temp = int256{};
  temp.i64[1] = uint128_lo64(AD0);
  temp.i64[2] = uint128_hi64(AD0);
  add_int256_to_int256(ABCD, temp);

  temp = int256{};
  temp.i64[2] = uint128_lo64(AC00);
  temp.i64[3] = uint128_hi64(AC00);
  add_int256_to_int256(ABCD, temp);

  // ABCD is now a 256-bit integer
  if( is_negative )
    {
    negate_int256(ABCD);
    }
  ABCD.rdigits = ab_value.rdigits + cd_value.rdigits;
  }

extern "C"
void
__gg__multiplyf1_phase2(cbl_arith_format_t ,
                        size_t ,
                        cblc_referlet_t *,
                        size_t ,
                        cblc_referlet_t *,
                        size_t ,
                  const cblc_referlet_t *C,
                  const cbl_round_t  *rounded,
                        int           on_error_flag,
                        int          *compute_error
                        )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  GCOB_FP128 a_value;
  GCOB_FP128 b_value;

  if( multiply_intermediate_is_float )
    {
    a_value = multiply_intermediate_float;
    if( C[0].field->type == FldFloat )
      {
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    else
      {
      // float times fixed
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    }
  else
    {
    if( C[0].field->type == FldFloat )
      {
      // fixed * float
      a_value = (GCOB_FP128) multiply_intermediate_int128.i128;
      if( multiply_intermediate_int128.rdigits )
        {
        a_value /= 
          (GCOB_FP128)__gg__power_of_ten(multiply_intermediate_int128.rdigits);
        }
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    else
      {
      // fixed times fixed

      // We have two 128-bit numbers.  Call them AB and CD, where A, B, C, D are
      // 64-bit "digits".  We need to multiply them to create a 256-bit result

      int128 ab_value = multiply_intermediate_int128;

      int128 cd_value;

      __gg__int128_from_qualified_field(cd_value,
                                        C[0].field,
                                        C[0].offset,
                                        C[0].size);
      int256 ABCD;
      multiply_int128_by_int128(ABCD, ab_value, cd_value);

      int overflow = squeeze_int256(ABCD);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }
        // At this point, we assign running_sum to *C.
      *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                          on_size_error,
                                          int256_get_u128(ABCD, 0),
                                          ABCD.rdigits,
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
    *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
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
            const cblc_referlet_t *A,
                  size_t ,
            const cblc_referlet_t *B,
                  size_t nC,
            const cblc_referlet_t *C,
            const cbl_round_t  *rounded,
                  int           on_error_flag,
                  int          *compute_error
                  )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);

  bool      got_float = false;
  GCOB_FP128 product_float;
  int256    product_fix;

  if( A[0].field->type == FldFloat || B[0].field->type == FldFloat )
    {
    GCOB_FP128 a_value = __gg__float128_from_qualified_field(A[0].field,
                                                             A[0].offset,
                                                             A[0].size);
    GCOB_FP128 b_value = __gg__float128_from_qualified_field(B[0].field,
                                                             B[0].offset,
                                                             B[0].size);
    product_float = multiply_helper_float(a_value, b_value, compute_error);
    got_float = true;
    }
  else
    {
    int128 a_value;
    int128 b_value;
    __gg__int128_from_qualified_field(a_value,
                                      A[0].field,
                                      A[0].offset,
                                      A[0].size);
    __gg__int128_from_qualified_field(b_value,
                                      B[0].field,
                                      B[0].offset,
                                      B[0].size);
    multiply_int128_by_int128(product_fix, a_value, b_value);
    int overflow = squeeze_int256(product_fix);
    if( overflow )
      {
      *compute_error |= compute_error_overflow;
      }
    }

  for(size_t i=0; i<nC; i++)
    {
    if( got_float )
      {
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          product_float,
                                          *rounded++);
      }
    else
      {
      *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                          on_size_error,
                                          int256_get_u128(product_fix, 0),
                                          product_fix.rdigits,
                                          *rounded++);
      }
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
  bool is_negative = false;

  if( int256_is_negative(val) )
    {
    negate_int256(val);
    is_negative = true;
    }

  while(val.i64[0] || val.i64[1] || val.i64[2] || val.i64[3])
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
  if( is_negative )
    {
    ach[index++] = '-';
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


static int
clz_uint64(uint64_t value)
  {
#if defined(__GNUC__) || defined(__clang__)
  return value ? __builtin_clzll(value) : 64;
#else
  int retval = 0;
  if( !value )
    {
    return 64;
    }
  while( !(value & 0x8000000000000000ULL) )
    {
    retval += 1;
    value <<= 1;
    }
  return retval;
#endif
  }

static void
shift_left_int256(int256 &value, int bits)
  {
  if( !bits )
    {
    return;
    }

  const int whole_limbs = bits / 64;
  const int inner_bits  = bits % 64;
  int256 shifted = {};

  for( int i=3; i>=0; i-- )
    {
    const int source = i - whole_limbs;
    if( source < 0 )
      {
      continue;
      }

    shifted.i64[i] = value.i64[source] << inner_bits;
    if( inner_bits && source > 0 )
      {
      shifted.i64[i] |= value.i64[source - 1] >> (64 - inner_bits);
      }
    }

  value = shifted;
  }

static void
divide_int128_by_int128(int256   &quotient,
                        __int128  dividend,
                        int       dividend_rdigits,
                        __int128  divisor,
                        int       divisor_rdigits,
                        int      *compute_error)
  {
  if( divisor == 0 )
    {
    *compute_error |= compute_error_divide_by_zero;
    quotient = int256{};
    quotient.i64[0] = static_cast<uint64_t>(dividend);
    quotient.rdigits = dividend_rdigits;
    return;
    }

  bool is_negative = (dividend < 0) != (divisor < 0);

  uint128 abs_dividend = int128_abs_as_uint128(dividend);
  uint128 abs_divisor  = int128_abs_as_uint128(divisor);

  quotient = int256{};
  int256_set_u128(quotient, 0, abs_dividend);

  // In order to get 0.3333333.... from 1 / 3, we are going to scale up the
  // numerator so that it has 37 rdigits:

  int scale = MAX_FIXED_POINT_DIGITS;
  scale_int256_by_digits(quotient, scale);
  quotient.rdigits = scale + dividend_rdigits - divisor_rdigits;

  // Now, let's see if we can do a simple divide-by-single-place calculation:

  if( uint128_hi64(abs_divisor) == 0 )
    {
    // Yes!  The divisor fits into 64 bits:
    divide_int256_by_int64(quotient, uint128_lo64(abs_divisor));
    }
  else
    {
    // We have to do long division, and that means Knuth's Algorithm D.  The
    // arithmetic below treats i64[0] as the least-significant limb and i64[3]
    // as the most-significant limb on every host, so it does not depend on the
    // in-memory byte order of __int128.

    int256 numerator = quotient;

    // Algorithm D requires the high-order divisor limb to have its top bit
    // set.
    int bits_to_shift = clz_uint64(uint128_hi64(abs_divisor));
    shift_left_int256(numerator, bits_to_shift);
    uint128 normalized_divisor = abs_divisor << bits_to_shift;

    uint64_t divisor_low  = uint128_lo64(normalized_divisor);
    uint64_t divisor_high = uint128_hi64(normalized_divisor);

    quotient = int256{};
    quotient.rdigits = scale + dividend_rdigits - divisor_rdigits;

    for( int q_place = 1; q_place >= 0; q_place-- )
      {
      uint64_t qhat;
      uint64_t rhat;

      if( numerator.i64[q_place + 2] == divisor_high )
        {
        qhat = UINT64_MAX;
        rhat = numerator.i64[q_place + 1];
        }
      else
        {
        uint128 temp = uint128_from_limbs(numerator.i64[q_place + 2],
                                          numerator.i64[q_place + 1]);
        qhat = static_cast<uint64_t>(temp / divisor_high);
        rhat = static_cast<uint64_t>(temp % divisor_high);
        }

      // Correct the guess if qhat * divisor_low is too large.
      while( true )
        {
        uint128 left  = static_cast<uint128>(qhat) * divisor_low;
        uint128 right = (static_cast<uint128>(rhat) << 64)
                      | numerator.i64[q_place];

        if( left <= right )
          {
          break;
          }

        qhat -= 1;
        uint128 expanded_rhat = static_cast<uint128>(rhat) + divisor_high;
        rhat = static_cast<uint64_t>(expanded_rhat);
        if( expanded_rhat >> 64 )
          {
          break;
          }
        }

      // Multiply the two-limb divisor by the one-limb quotient guess.  The
      // product occupies at most three limbs.
      uint128 product0 = static_cast<uint128>(qhat) * divisor_low;
      uint128 product1 = static_cast<uint128>(qhat) * divisor_high;

      uint64_t subber[3];
      subber[0] = uint128_lo64(product0);
      product1 += uint128_hi64(product0);
      subber[1] = uint128_lo64(product1);
      subber[2] = uint128_hi64(product1);

      bool borrow = false;
      for( int j=0; j<3; j++ )
        {
        uint128 subtrahend = static_cast<uint128>(subber[j])
                           + static_cast<uint128>(borrow ? 1 : 0);
        uint64_t old_value = numerator.i64[q_place + j];
        numerator.i64[q_place + j] = old_value - uint128_lo64(subtrahend);
        borrow = (subtrahend >> 64) || old_value < uint128_lo64(subtrahend);
        }

      // If the subtraction went negative, the guess was one too high.  Add the
      // divisor back and decrement the quotient limb.
      if( borrow )
        {
        qhat -= 1;

        uint128 sum = static_cast<uint128>(numerator.i64[q_place])
                    + divisor_low;
        numerator.i64[q_place] = uint128_lo64(sum);

        sum = static_cast<uint128>(numerator.i64[q_place + 1])
            + divisor_high
            + uint128_hi64(sum);
        numerator.i64[q_place + 1] = uint128_lo64(sum);

        numerator.i64[q_place + 2] += uint128_hi64(sum);
        }

      quotient.i64[q_place] = qhat;
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
                      cblc_referlet_t *,
                      size_t ,
                      cblc_referlet_t *,
                      size_t ,
                const cblc_referlet_t *C,
                const cbl_round_t  *rounded,
                      int           on_error_flag,
                      int          *compute_error
                      )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  GCOB_FP128 a_value;
  GCOB_FP128 b_value;

  if( multiply_intermediate_is_float )
    {
    a_value = multiply_intermediate_float;
    if( C[0].field->type == FldFloat )
      {
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    else
      {
      // float times fixed
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    }
  else
    {
    if( C[0].field->type == FldFloat )
      {
      // fixed by float
      a_value = (GCOB_FP128) multiply_intermediate_int128.i128;
      if( multiply_intermediate_int128.rdigits )
        {
        a_value /=
          (GCOB_FP128)__gg__power_of_ten(multiply_intermediate_int128.rdigits);
        }
      b_value = __gg__float128_from_qualified_field(C[0].field,
                                                    C[0].offset,
                                                    C[0].size);
      goto float_float;
      }
    else
      {
      // fixed by fixed
      int128 dividend;
      __gg__int128_from_qualified_field(dividend,
                                        C[0].field,
                                        C[0].offset,
                                        C[0].size);

      int256 quotient;

      divide_int128_by_int128(quotient,
                              dividend.i128,
                              dividend.rdigits,
                              multiply_intermediate_int128.i128,
                              multiply_intermediate_int128.rdigits,
                              compute_error);

      int overflow = squeeze_int256(quotient);
      if( overflow )
        {
        *compute_error |= compute_error_overflow;
        }
        // At this point, we assign the quotient to *C.
      *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
                                          on_size_error,
                                          int256_get_u128(quotient, 0),
                                          quotient.rdigits,
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
    *compute_error |= conditional_stash(C[0].field, C[0].offset, C[0].size,
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
          const cblc_referlet_t *A,
                size_t ,
          const cblc_referlet_t *B,
                size_t nC,
          const cblc_referlet_t *C,
          const cbl_round_t  *rounded,
                int           on_error_flag,
                int          *compute_error
                )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  if( A[0].field->type == FldFloat ||  B[0].field->type == FldFloat  )
    {
    GCOB_FP128 a_value;
    GCOB_FP128 b_value;
    GCOB_FP128 c_value;
    a_value = __gg__float128_from_qualified_field(A[0].field,
                                                  A[0].offset,
                                                  A[0].size);
    b_value = __gg__float128_from_qualified_field(B[0].field,
                                                  B[0].offset,
                                                  B[0].size);
    c_value = divide_helper_float(a_value, b_value, &error_this_time);

    *compute_error |= error_this_time;
    if( !error_this_time )
      {
      for(size_t i=0; i<nC; i++)
        {
        *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                            on_size_error,
                                            c_value,
                                            *rounded++);
        }
      }
    }
  else
    {
    // fixed divided by fixed
    int128 dividend;
    __gg__int128_from_qualified_field(dividend,
                                      A[0].field,
                                      A[0].offset,
                                      A[0].size);

    int128 divisor;
    __gg__int128_from_qualified_field(divisor,
                                      B[0].field,
                                      B[0].offset,
                                      B[0].size);
    int256 quotient;

    divide_int128_by_int128(quotient,
                            dividend.i128,
                            dividend.rdigits,
                            divisor.i128,
                            divisor.rdigits,
                            compute_error);


    *compute_error |= squeeze_int256(quotient);
    if( !*compute_error )
      {
        // At this point, we assign the quotient to *C.
      for(size_t i=0; i<nC; i++)
        {
        *compute_error |= conditional_stash(C[i].field, C[i].offset, C[i].size,
                                            on_size_error,
                                            int256_get_u128(quotient, 0),
                                            quotient.rdigits,
                                            *rounded++);
        }
      }
    }
  }

extern "C"
void
__gg__dividef45(cbl_arith_format_t ,
                size_t ,
          const cblc_referlet_t *A,
                size_t ,
          const cblc_referlet_t *B,
                size_t ,
          const cblc_referlet_t *C,
                cbl_round_t  *rounded_p,
                int           on_error_flag,
                int          *compute_error
                )
  {
  bool on_size_error = !!(on_error_flag & ON_SIZE_ERROR);
  int error_this_time=0;

  if( A[0].field->type == FldFloat ||  B[0].field->type == FldFloat  )
    {
    GCOB_FP128 a_value;
    GCOB_FP128 b_value;
    GCOB_FP128 c_value;
    a_value = __gg__float128_from_qualified_field(A[0].field,
                                                  A[0].offset,
                                                  A[0].size);
    b_value = __gg__float128_from_qualified_field(B[0].field,
                                                  B[0].offset,
                                                  B[0].size);
    c_value = divide_helper_float(a_value, b_value, &error_this_time);

    *compute_error |= error_this_time;

    if( !error_this_time )
      {
      // C[0] is the temporary remainder and C[1] is the quotient.
      // The quotient's rounding mode is therefore rounded_p[1].
      *compute_error |= conditional_stash(C[1].field,
                                          C[1].offset,
                                          C[1].size,
                                          on_size_error,
                                          c_value,
                                          rounded_p[1]);

      // This is floating point, and there is a remainder, and we don't know
      // what that means.  Set the remainder to zero.  A remainder has no
      // ROUNDED phrase, so use truncation explicitly.
      if( !*compute_error )
        {
        c_value = 0;
        *compute_error |= conditional_stash(C[0].field,
                                            C[0].offset,
                                            C[0].size,
                                            on_size_error,
                                            c_value,
                                            truncation_e);
        }
      }
    }
  else
    {
    // fixed divided by fixed
    int128 dividend;
    __gg__int128_from_qualified_field(dividend,
                                      A[0].field,
                                      A[0].offset,
                                      A[0].size);
    int128 divisor;                   
    __gg__int128_from_qualified_field(divisor,
                                      B[0].field,
                                      B[0].offset,
                                      B[0].size);

    int256 quotient;

    divide_int128_by_int128(quotient,
                            dividend.i128,
                            dividend.rdigits,
                            divisor.i128,
                            divisor.rdigits,
                            compute_error);

    *compute_error |= squeeze_int256(quotient);

    if( !*compute_error )
      {
      // We are going to need the unrounded quotient to calculate the remainder
      int128 unrounded_quotient;
      rounded_p += 1;// Skip the rounded value for the remainder
      cbl_round_t rounded = *rounded_p;
      switch(rounded)
        {
        case truncation_e:
          {
          *compute_error |= conditional_stash(C[1].field,
                                              C[1].offset,
                                              C[1].size,
                                              on_size_error,
                                              int256_get_u128(quotient, 0),
                                              quotient.rdigits,
                                              *rounded_p++);
          __gg__int128_from_qualified_field(unrounded_quotient,
                                            C[1].field,
                                            C[1].offset,
                                            C[1].size);
          break;
          }
        default:
          {
          conditional_stash(C[1].field, C[1].offset, C[1].size,
                            false,
                            int256_get_u128(quotient, 0),
                            quotient.rdigits,
                            truncation_e);
          __gg__int128_from_qualified_field(unrounded_quotient,
                                            C[1].field,
                                            C[1].offset,
                                            C[1].size);
          // At this point, we assign the rounded quotient to *C.
          *compute_error |= conditional_stash(C[1].field,
                                              C[1].offset,
                                              C[1].size,
                                              on_size_error,
                                              int256_get_u128(quotient, 0),
                                              quotient.rdigits,
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
        // Step 1: Multiply the unrounded quotient by the divisor
        multiply_int128_by_int128(temp, unrounded_quotient, divisor);

        int256 odividend = {};
        int256_set_u128(odividend, 0, static_cast<uint128>(dividend.i128));
        odividend.rdigits = dividend.rdigits;
        if( dividend.i128 < 0 )
          {
          odividend.i64[2] = UINT64_MAX;
          odividend.i64[3] = UINT64_MAX;
          }

        subtract_int256_from_int256(odividend, temp);

        *compute_error |= squeeze_int256(odividend);

        if( !*compute_error )
          {
          *compute_error |= conditional_stash(C[0].field,
                                              C[0].offset,
                                              C[0].size,
                                              on_size_error,
                                              int256_get_u128(odividend, 0),
                                              odividend.rdigits,
                                              truncation_e);
          }
        }
      }
    }
  }

static int
multiply_int256_by_int256(int256 &left, const int256 &right)
  {
  int retval = 0;
  // The inputs have both been squeezed into 128 bits.
  int128 l128;
  int128 r128;

  l128.i128    = int256_get_u128(left, 0);
  l128.rdigits = left.rdigits;

  r128.i128   = int256_get_u128(right, 0);
  r128.rdigits = right.rdigits;

  // We need to make both 128-bit operands positive:
  bool negative = false;
  if( left.i64[3] & 0x8000000000000000ULL )
    {
    negative = !negative;
    l128.i128 = ~l128.i128 + 1;
    }
  if( right.i64[3] & 0x8000000000000000ULL )
    {
    negative = !negative;
    r128.i128 = ~r128.i128 + 1;
    }

  multiply_int128_by_int128(left, l128, r128);

  if( negative )
    {
    // This code takes the two's complement of the 256-bit value:
    left.i64[0] = ~left.i64[0];
    left.i64[1] = ~left.i64[1];
    left.i64[2] = ~left.i64[2];
    left.i64[3] = ~left.i64[3];
    // I usually eschew code like this.  But it's just too precious.
    if(++left.i64[0] == 0)
    if(++left.i64[1] == 0)
    if(++left.i64[2] == 0)
       ++left.i64[3];
    }

  return retval;
  }

static int
divide_int256_by_int256(int256 &left, const int256 &right)
  {
  int retval = 0;
  // The inputs have both been squeezed into 128 bits.
  int128 l128;
  int128 r128;

  l128.i128    = int256_get_u128(left, 0);
  l128.rdigits = left.rdigits;

  r128.i128   = int256_get_u128(right, 0);
  r128.rdigits = right.rdigits;

  // We need to make both 128-bit operands positive:
  bool negative = false;
  if( left.i64[3] & 0x8000000000000000ULL )
    {
    negative = !negative;
    l128.i128 = ~l128.i128 + 1;
    }
  if( right.i64[3] & 0x8000000000000000ULL )
    {
    negative = !negative;
    r128.i128 = ~r128.i128 + 1;
    }

  divide_int128_by_int128(left,
                          l128.i128,
                          l128.rdigits,
                          r128.i128,
                          r128.rdigits,
                          &retval);

  if( negative )
    {
    // This code takes the two's complement of the 256-bit value:
    left.i64[0] = ~left.i64[0];
    left.i64[1] = ~left.i64[1];
    left.i64[2] = ~left.i64[2];
    left.i64[3] = ~left.i64[3];
    // I usually eschew code like this.  But it's just too precious.
    if(++left.i64[0] == 0)
    if(++left.i64[1] == 0)
    if(++left.i64[2] == 0)
       ++left.i64[3];
    }

  return retval;
  }

static std::vector<GCOB_FP128> compute_float_stack;
static std::vector<int256> compute_fixed_stack;

static int
compute_fixed_add()
  {
  // This is RPN at work, so stack[N-2] += stack[N-1] 
  int retval = 0;

  size_t level = compute_fixed_stack.size();
  assert( level >= 2 );
  int256 left  = compute_fixed_stack[level-2];
  int256 right = compute_fixed_stack[level-1];

  add_int256_to_int256(left, right);

  compute_fixed_stack[level-2] = left;
  compute_fixed_stack.pop_back();
  return retval;
  }

static int
compute_fixed_subtract()
  {
  int retval = 0;

  size_t level = compute_fixed_stack.size();
  assert( level >= 2 );
  int256 left  = compute_fixed_stack[level-2];
  int256 right = compute_fixed_stack[level-1];
  subtract_int256_from_int256(left, right);
  compute_fixed_stack[level-2] = left;
  compute_fixed_stack.pop_back();
  return retval;
  }

static int
compute_fixed_multiply()
  {
  int retval = 0;

  size_t level = compute_fixed_stack.size();
  assert( level >= 2 );
  int256 left  = compute_fixed_stack[level-2];
  int256 right = compute_fixed_stack[level-1];

  retval |= squeeze_int256(left);
  retval |= squeeze_int256(right);
  retval |= multiply_int256_by_int256(left, right);

  compute_fixed_stack[level-2] = left;
  compute_fixed_stack.pop_back();
  return retval;
  }

static int
compute_fixed_divide()
  {
  int retval = 0;

  size_t level = compute_fixed_stack.size();
  assert( level >= 2 );
  int256 left  = compute_fixed_stack[level-2];
  int256 right = compute_fixed_stack[level-1];

  retval |= squeeze_int256(left);
  retval |= squeeze_int256(right);
  retval |= divide_int256_by_int256(left, right);

  compute_fixed_stack[level-2] = left;
  compute_fixed_stack.pop_back();
  return retval;
  }

static int
compute_fixed_negate()
  {
  int retval = 0;

  assert( !compute_fixed_stack.empty() );
  int256 &left = compute_fixed_stack.back();

  left.i64[0] = ~left.i64[0];
  left.i64[1] = ~left.i64[1];
  left.i64[2] = ~left.i64[2];
  left.i64[3] = ~left.i64[3];
  // I usually eschew code like this.  But it's just too precious.
  if(++left.i64[0] == 0)
  if(++left.i64[1] == 0)
  if(++left.i64[2] == 0)
     ++left.i64[3];

  return retval;
  }

static int
compute_float_add()
  {
  // This is RPN at work, so stack[N-2] += stack[N-1] 
  int retval = 0;

  size_t level = compute_float_stack.size();
  assert( level >= 2 );
  GCOB_FP128 left  = compute_float_stack[level-2];
  GCOB_FP128 right = compute_float_stack[level-1];

  left = addition_helper_float(left, right, &retval);;

  compute_float_stack[level-2] = left;
  compute_float_stack.pop_back();
  return retval;
  }

static int
compute_float_subtract()
  {
  int retval = 0;

  size_t level = compute_float_stack.size();
  assert( level >= 2 );
  GCOB_FP128 left  = compute_float_stack[level-2];
  GCOB_FP128 right = compute_float_stack[level-1];

  left = subtraction_helper_float(left, right, &retval);;

  compute_float_stack[level-2] = left;
  compute_float_stack.pop_back();
  return retval;
  }

static int
compute_float_multiply()
  {
  int retval = 0;

  size_t level = compute_float_stack.size();
  assert( level >= 2 );
  GCOB_FP128 left  = compute_float_stack[level-2];
  GCOB_FP128 right = compute_float_stack[level-1];

  left = multiply_helper_float(left, right, &retval);

  compute_float_stack[level-2] = left;
  compute_float_stack.pop_back();
  return retval;
  }

static int
compute_float_divide()
  {
  int retval = 0;

  size_t level = compute_float_stack.size();
  assert( level >= 2 );
  GCOB_FP128 left  = compute_float_stack[level-2];
  GCOB_FP128 right = compute_float_stack[level-1];

  left = divide_helper_float(left, right, &retval);

  compute_float_stack[level-2] = left;
  compute_float_stack.pop_back();
  return retval;
  }

static int
compute_float_pow()
  {
  int retval = 0;

  size_t level = compute_float_stack.size();
  assert( level >= 2 );
  GCOB_FP128 left  = compute_float_stack[level-2];
  GCOB_FP128 right = compute_float_stack[level-1];
  
  left = exponentiation_helper(left, right, &retval);

  compute_float_stack[level-2] = left;
  compute_float_stack.pop_back();
  return retval;
  }

static int
compute_float_negate()
  {
  int retval = 0;
  assert( !compute_float_stack.empty() );
  compute_float_stack.back() = -compute_float_stack.back();
  return retval;
  }

extern "C"
int
__gg__compute_fixed(const char          opstring[],
                    const cblc_field_t *fields[],
                    const size_t        offsets[])
  {
  int retval = 0;
  size_t noperations = strlen(opstring);

  // We will compute in fixed-point
  compute_fixed_stack.clear();

  for(size_t i=0; i<noperations; i++)
    {
    char ch = opstring[i];
    switch(ch)
      {
      case 'P':
        {
        // We push a value onto the stack:
        int256 value;
        get_int256_from_qualified_field(value,
                                        fields[i],
                                        offsets[i],
                                        fields[i]->capacity);
        compute_fixed_stack.push_back(value);
        }
      break;

      case '+':
        retval |= compute_fixed_add();
        break;

      case '-':
        retval |= compute_fixed_subtract();
        break;

      case '*':
        retval |= compute_fixed_multiply();
        break;

      case '/':
        retval |= compute_fixed_divide();
        break;

      case '!':
        retval |= compute_fixed_negate();
        break;

      case '^':
        fprintf(stderr, "We shouldn't see an integer a^b compute\n");
        abort();
        break;
      }
    }

  // The destination was added to the end of the lists
  cblc_field_t *target = const_cast<cblc_field_t *>(fields[noperations]);
  size_t target_offset = offsets[noperations];

  assert(compute_fixed_stack.size() == 1);
  int256 v256 = compute_fixed_stack.back();
  int overflow = squeeze_int256(v256);
  if( overflow )
    {
    retval |= compute_error_overflow;
    }

  __int128 value;
  value   =  v256.i64[1];
  value <<= 64;
  value  +=  v256.i64[0];

  retval |= __gg__int128_to_qualified_field(target,
                                            target_offset,
                                            target->capacity,
                                            value,
                                            v256.rdigits,
                                            truncation_e);
  return retval;
  }

extern "C"
int
__gg__compute_float(const char          opstring[],
                    const cblc_field_t *fields[],
                    const size_t        offsets[])
  {
  int retval = 0;
  size_t noperations = strlen(opstring);

  // We will compute in fixed-point
  compute_float_stack.clear();

  for(size_t i=0; i<noperations; i++)
    {
    char ch = opstring[i];
    switch(ch)
      {
      case 'P':
        {
        // We push a value onto the stack:
        GCOB_FP128 value = 
                      __gg__float128_from_qualified_field(fields[i],
                                                          offsets[i],
                                                          fields[i]->capacity);
        compute_float_stack.push_back(value);
        }
      break;

      case '+':
        retval |= compute_float_add();
        break;

      case '-':
        retval |= compute_float_subtract();
        break;

      case '*':
        retval |= compute_float_multiply();
        break;

      case '/':
        retval |= compute_float_divide();
        break;

      case '^':
        retval |= compute_float_pow();
        break;

      case '!':
        retval |= compute_float_negate();
        break;

      }
    }

  // The destination was added to the end of the lists
  cblc_field_t *target = const_cast<cblc_field_t *>(fields[noperations]);
  size_t target_offset = offsets[noperations];

  assert(compute_float_stack.size() == 1);
  GCOB_FP128 value = compute_float_stack.back();

  __gg__float128_to_qualified_field(target,
                                    target_offset,
                                    value,
                                    truncation_e,
                                    &retval);
  return retval;
  }
