/* This is a software decimal floating point library.
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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

/* This implements IEEE 754 decimal floating point arithmetic, but
   does not provide a mechanism for setting the rounding mode, or for
   generating or handling exceptions.  Conversions between decimal
   floating point types and other types depend on C library functions.

   Contributed by Ben Elliston  <bje@au.ibm.com>.  */

#include <stdio.h>
#include <stdlib.h>
/* FIXME: compile with -std=gnu99 to get these from stdlib.h */
extern float strtof (const char *, char **);
extern long double strtold (const char *, char **);
#include <string.h>
#include <limits.h>

#include "dfp-bit.h"

/* Forward declarations.  */
#if WIDTH == 32 || WIDTH_TO == 32
void __host_to_ieee_32 (_Decimal32 in, decimal32 *out);
void __ieee_to_host_32 (decimal32 in, _Decimal32 *out);
#endif
#if WIDTH == 64 || WIDTH_TO == 64
void __host_to_ieee_64 (_Decimal64 in, decimal64 *out);
void __ieee_to_host_64 (decimal64 in, _Decimal64 *out);
#endif
#if WIDTH == 128 || WIDTH_TO == 128
void __host_to_ieee_128 (_Decimal128 in, decimal128 *out);
void __ieee_to_host_128 (decimal128 in, _Decimal128 *out);
#endif

/* A pointer to a binary decFloat operation.  */
typedef decFloat* (*dfp_binary_func)
     (decFloat *, const decFloat *, const decFloat *, decContext *);

/* Binary operations.  */

/* Use a decFloat (decDouble or decQuad) function to perform a DFP
   binary operation.  */
static inline decFloat
dfp_binary_op (dfp_binary_func op, decFloat arg_a, decFloat arg_b)
{
  decFloat result;
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);

  /* Perform the operation.  */
  op (&result, &arg_a, &arg_b, &context);

  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    {
      /* decNumber exception flags we care about here.  */
      int ieee_flags;
      int dec_flags = DEC_IEEE_854_Division_by_zero | DEC_IEEE_854_Inexact
		      | DEC_IEEE_854_Invalid_operation | DEC_IEEE_854_Overflow
		      | DEC_IEEE_854_Underflow;
      dec_flags &= context.status;
      ieee_flags = DFP_IEEE_FLAGS (dec_flags);
      if (ieee_flags != 0)
        DFP_HANDLE_EXCEPTIONS (ieee_flags);
    }

  return result;
}

#if WIDTH == 32
/* The decNumber package doesn't provide arithmetic for decSingle (32 bits);
   convert to decDouble, use the operation for that, and convert back.  */
static inline _Decimal32
d32_binary_op (dfp_binary_func op, _Decimal32 arg_a, _Decimal32 arg_b)
{
  union { _Decimal32 c; decSingle f; } a32, b32, res32;
  decDouble a, b, res;
  decContext context;

  /* Widen the operands and perform the operation.  */
  a32.c = arg_a;
  b32.c = arg_b;
  decSingleToWider (&a32.f, &a);
  decSingleToWider (&b32.f, &b);
  res = dfp_binary_op (op, a, b);

  /* Narrow the result, which might result in an underflow or overflow.  */
  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);
  decSingleFromWider (&res32.f, &res, &context);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    {
      /* decNumber exception flags we care about here.  */
      int ieee_flags;
      int dec_flags = DEC_IEEE_854_Inexact | DEC_IEEE_854_Overflow
		      | DEC_IEEE_854_Underflow;
      dec_flags &= context.status;
      ieee_flags = DFP_IEEE_FLAGS (dec_flags);
      if (ieee_flags != 0)
        DFP_HANDLE_EXCEPTIONS (ieee_flags);
    }

  return res32.c;
}
#else
/* decFloat operations are supported for decDouble (64 bits) and
   decQuad (128 bits).  The bit patterns for the types are the same.  */
static inline DFP_C_TYPE
dnn_binary_op (dfp_binary_func op, DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  union { DFP_C_TYPE c; decFloat f; } a, b, result;

  a.c = arg_a;
  b.c = arg_b;
  result.f = dfp_binary_op (op, a.f, b.f);
  return result.c;
}
#endif

/* Comparison operations.  */

/* Use a decFloat (decDouble or decQuad) function to perform a DFP
   comparison.  */
static inline CMPtype
dfp_compare_op (dfp_binary_func op, decFloat arg_a, decFloat arg_b)
{
  decContext context;
  decFloat res;
  int result;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);

  /* Perform the comparison.  */
  op (&res, &arg_a, &arg_b, &context);

  if (DEC_FLOAT_IS_SIGNED (&res))
    result = -1;
  else if (DEC_FLOAT_IS_ZERO (&res))
    result = 0;
  else if (DEC_FLOAT_IS_NAN (&res))
    result = -2;
  else
    result = 1;

  return (CMPtype) result;
}

#if WIDTH == 32
/* The decNumber package doesn't provide comparisons for decSingle (32 bits);
   convert to decDouble, use the operation for that, and convert back.  */
static inline CMPtype
d32_compare_op (dfp_binary_func op, _Decimal32 arg_a, _Decimal32 arg_b)
{
  union { _Decimal32 c; decSingle f; } a32, b32;
  decDouble a, b;

  a32.c = arg_a;
  b32.c = arg_b;
  decSingleToWider (&a32.f, &a);
  decSingleToWider (&b32.f, &b);
  return dfp_compare_op (op, a, b);  
}
#else
/* decFloat comparisons are supported for decDouble (64 bits) and
   decQuad (128 bits).  The bit patterns for the types are the same.  */
static inline CMPtype
dnn_compare_op (dfp_binary_func op, DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  union { DFP_C_TYPE c; decFloat f; } a, b;

  a.c = arg_a;
  b.c = arg_b;
  return dfp_compare_op (op, a.f, b.f);  
}
#endif

#if defined(L_conv_sd)
void
__host_to_ieee_32 (_Decimal32 in, decimal32 *out)
{
  memcpy (out, &in, 4);
}

void
__ieee_to_host_32 (decimal32 in, _Decimal32 *out)
{
  memcpy (out, &in, 4);
}
#endif /* L_conv_sd */

#if defined(L_conv_dd)
void
__host_to_ieee_64 (_Decimal64 in, decimal64 *out)
{
  memcpy (out, &in, 8);
}

void
__ieee_to_host_64 (decimal64 in, _Decimal64 *out)
{
  memcpy (out, &in, 8);
}
#endif /* L_conv_dd */

#if defined(L_conv_td)
void
__host_to_ieee_128 (_Decimal128 in, decimal128 *out)
{
  memcpy (out, &in, 16);
}

void
__ieee_to_host_128 (decimal128 in, _Decimal128 *out)
{
  memcpy (out, &in, 16);
}
#endif /* L_conv_td */

#if defined(L_addsub_sd) || defined(L_addsub_dd) || defined(L_addsub_td)
DFP_C_TYPE
DFP_ADD (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  return DFP_BINARY_OP (DEC_FLOAT_ADD, arg_a, arg_b);
}

DFP_C_TYPE
DFP_SUB (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  return DFP_BINARY_OP (DEC_FLOAT_SUBTRACT, arg_a, arg_b);
}
#endif /* L_addsub */

#if defined(L_mul_sd) || defined(L_mul_dd) || defined(L_mul_td)
DFP_C_TYPE
DFP_MULTIPLY (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  return DFP_BINARY_OP (DEC_FLOAT_MULTIPLY, arg_a, arg_b);
}
#endif /* L_mul */

#if defined(L_div_sd) || defined(L_div_dd) || defined(L_div_td)
DFP_C_TYPE
DFP_DIVIDE (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  return DFP_BINARY_OP (DEC_FLOAT_DIVIDE, arg_a, arg_b);
}
#endif /* L_div */

#if defined (L_eq_sd) || defined (L_eq_dd) || defined (L_eq_td)
CMPtype
DFP_EQ (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  CMPtype stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For EQ return zero for true, nonzero for false.  */
  return stat != 0;
}
#endif /* L_eq */

#if defined (L_ne_sd) || defined (L_ne_dd) || defined (L_ne_td)
CMPtype
DFP_NE (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  int stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For NE return zero for true, nonzero for false.  */
  if (__builtin_expect (stat == -2, 0))  /* An operand is NaN.  */
    return 1;
  return stat != 0;
}
#endif /* L_ne */

#if defined (L_lt_sd) || defined (L_lt_dd) || defined (L_lt_td)
CMPtype
DFP_LT (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  int stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For LT return -1 (<0) for true, 1 for false.  */
  return (stat == -1) ? -1 : 1;
}
#endif /* L_lt */

#if defined (L_gt_sd) || defined (L_gt_dd) || defined (L_gt_td)
CMPtype
DFP_GT (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  int stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For GT return 1 (>0) for true, -1 for false.  */
  return (stat == 1) ? 1 : -1;
}
#endif

#if defined (L_le_sd) || defined (L_le_dd) || defined (L_le_td)
CMPtype
DFP_LE (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  int stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For LE return 0 (<= 0) for true, 1 for false.  */
  if (__builtin_expect (stat == -2, 0))  /* An operand is NaN.  */
    return 1;
  return stat == 1;
}
#endif /* L_le */

#if defined (L_ge_sd) || defined (L_ge_dd) || defined (L_ge_td)
CMPtype
DFP_GE (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  int stat;
  stat = DFP_COMPARE_OP (DEC_FLOAT_COMPARE, arg_a, arg_b);
  /* For GE return 1 (>=0) for true, -1 for false.  */
  if (__builtin_expect (stat == -2, 0))  /* An operand is NaN.  */
    return -1;
  return (stat != -1) ? 1 : -1;
}
#endif /* L_ge */

#define BUFMAX 128

/* Check for floating point exceptions that are relevant for conversions
   between decimal float values and handle them.  */
static inline void
dfp_conversion_exceptions (const int status)
{
  /* decNumber exception flags we care about here.  */
  int ieee_flags;
  int dec_flags = DEC_IEEE_854_Inexact | DEC_IEEE_854_Invalid_operation
		  | DEC_IEEE_854_Overflow;
  dec_flags &= status;
  ieee_flags = DFP_IEEE_FLAGS (dec_flags);
  if (ieee_flags != 0)
    DFP_HANDLE_EXCEPTIONS (ieee_flags);
}

#if defined (L_sd_to_dd)
/* Use decNumber to convert directly from _Decimal32 to _Decimal64.  */
_Decimal64
DFP_TO_DFP (_Decimal32 f_from)
{
  union { _Decimal32 c; decSingle f; } from;
  union { _Decimal64 c; decDouble f; } to;

  from.c = f_from;
  to.f = *decSingleToWider (&from.f, &to.f);
  return to.c;
}
#endif

#if defined (L_sd_to_td)
/* Use decNumber to convert directly from _Decimal32 to _Decimal128.  */
_Decimal128
DFP_TO_DFP (_Decimal32 f_from)
{
  union { _Decimal32 c; decSingle f; } from;
  union { _Decimal128 c; decQuad f; } to;
  decDouble temp;

  from.c = f_from;
  temp = *decSingleToWider (&from.f, &temp);
  to.f = *decDoubleToWider (&temp, &to.f);
  return to.c;
}
#endif

#if defined (L_dd_to_td)
/* Use decNumber to convert directly from _Decimal64 to _Decimal128.  */
_Decimal128
DFP_TO_DFP (_Decimal64 f_from)
{
  union { _Decimal64 c; decDouble f; } from;
  union { _Decimal128 c; decQuad f; } to;

  from.c = f_from;
  to.f = *decDoubleToWider (&from.f, &to.f);
  return to.c;
}
#endif

#if defined (L_dd_to_sd)
/* Use decNumber to convert directly from _Decimal64 to _Decimal32.  */
_Decimal32
DFP_TO_DFP (_Decimal64 f_from)
{
  union { _Decimal32 c; decSingle f; } to;
  union { _Decimal64 c; decDouble f; } from;
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);
  from.c = f_from;
  to.f = *decSingleFromWider (&to.f, &from.f, &context);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return to.c;
}
#endif

#if defined (L_td_to_sd)
/* Use decNumber to convert directly from _Decimal128 to _Decimal32.  */
_Decimal32
DFP_TO_DFP (_Decimal128 f_from)
{
  union { _Decimal32 c; decSingle f; } to;
  union { _Decimal128 c; decQuad f; } from;
  decDouble temp;
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);
  from.c = f_from;
  temp = *decDoubleFromWider (&temp, &from.f, &context);
  to.f = *decSingleFromWider (&to.f, &temp, &context);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return to.c;
}
#endif

#if defined (L_td_to_dd)
/* Use decNumber to convert directly from _Decimal128 to _Decimal64.  */
_Decimal64
DFP_TO_DFP (_Decimal128 f_from)
{
  union { _Decimal64 c; decDouble f; } to;
  union { _Decimal128 c; decQuad f; } from;
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);
  from.c = f_from;
  to.f = *decDoubleFromWider (&to.f, &from.f, &context);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return to.c;
}
#endif

#if defined (L_dd_to_si) || defined (L_td_to_si) \
  || defined (L_dd_to_usi) || defined (L_td_to_usi)
/* Use decNumber to convert directly from decimal float to integer types.  */
INT_TYPE
DFP_TO_INT (DFP_C_TYPE x)
{
  union { DFP_C_TYPE c; decFloat f; } u;
  decContext context;
  INT_TYPE i;

  decContextDefault (&context, DEC_INIT_DECIMAL128);
  context.round = DEC_ROUND_DOWN;
  u.c = x;
  i = DEC_FLOAT_TO_INT (&u.f, &context, context.round);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return i;
}
#endif

#if defined (L_sd_to_si) || (L_sd_to_usi)
/* Use decNumber to convert directly from decimal float to integer types.  */
INT_TYPE
DFP_TO_INT (_Decimal32 x)
{
  union { _Decimal32 c; decSingle f; } u32;
  decDouble f64;
  decContext context;
  INT_TYPE i;

  decContextDefault (&context, DEC_INIT_DECIMAL128);
  context.round = DEC_ROUND_DOWN;
  u32.c = x;
  f64 = *decSingleToWider (&u32.f, &f64);
  i = DEC_FLOAT_TO_INT (&f64, &context, context.round);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return i;
}
#endif

#if defined (L_sd_to_di) || defined (L_dd_to_di) || defined (L_td_to_di) \
  || defined (L_sd_to_udi) || defined (L_dd_to_udi) || defined (L_td_to_udi)
/* decNumber doesn't provide support for conversions to 64-bit integer
   types, so do it the hard way.  */
INT_TYPE
DFP_TO_INT (DFP_C_TYPE x)
{
  /* decNumber's decimal* types have the same format as C's _Decimal*
     types, but they have different calling conventions.  */

  /* TODO: Decimal float to integer conversions should raise FE_INVALID
     if the result value does not fit into the result type.  */

  IEEE_TYPE s;
  char buf[BUFMAX];
  char *pos;
  decNumber qval, n1, n2;
  decContext context;

  /* Use a large context to avoid losing precision.  */
  decContextDefault (&context, DEC_INIT_DECIMAL128);
  /* Need non-default rounding mode here.  */
  context.round = DEC_ROUND_DOWN;

  HOST_TO_IEEE (x, &s);
  TO_INTERNAL (&s, &n1);
  /* Rescale if the exponent is less than zero.  */
  decNumberToIntegralValue (&n2, &n1, &context);
  /* Get a value to use for the quantize call.  */
  decNumberFromString (&qval, "1.", &context);
  /* Force the exponent to zero.  */
  decNumberQuantize (&n1, &n2, &qval, &context);
  /* Get a string, which at this point will not include an exponent.  */
  decNumberToString (&n1, buf);
  /* Ignore the fractional part.  */
  pos = strchr (buf, '.');
  if (pos)
    *pos = 0;
  /* Use a C library function to convert to the integral type.  */
  return STR_TO_INT (buf, NULL, 10);
}
#endif

#if defined (L_si_to_dd) || defined (L_si_to_td) \
  || defined (L_usi_to_dd) || defined (L_usi_to_td)
/* Use decNumber to convert directly from integer to decimal float types.  */
DFP_C_TYPE
INT_TO_DFP (INT_TYPE i)
{
  union { DFP_C_TYPE c; decFloat f; } u;

  u.f = *DEC_FLOAT_FROM_INT (&u.f, i);
  return u.c;
}
#endif

#if defined (L_si_to_sd) || defined (L_usi_to_sd)
_Decimal32
/* Use decNumber to convert directly from integer to decimal float types.  */
INT_TO_DFP (INT_TYPE i)
{
  union { _Decimal32 c; decSingle f; } u32;
  decDouble f64;
  decContext context;

  decContextDefault (&context, DEC_INIT_DECIMAL128);
  f64 = *DEC_FLOAT_FROM_INT (&f64, i);
  u32.f = *decSingleFromWider (&u32.f, &f64, &context);
  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);
  return u32.c;
}
#endif

#if defined (L_di_to_sd) || defined (L_di_to_dd) || defined (L_di_to_td) \
  || defined (L_udi_to_sd) || defined (L_udi_to_dd) || defined (L_udi_to_td)
/* decNumber doesn't provide support for conversions from 64-bit integer
   types, so do it the hard way.  */
DFP_C_TYPE
INT_TO_DFP (INT_TYPE i)
{
  DFP_C_TYPE f;
  IEEE_TYPE s;
  char buf[BUFMAX];
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);

  /* Use a C library function to get a floating point string.  */
  sprintf (buf, INT_FMT ".", CAST_FOR_FMT(i));
  /* Convert from the floating point string to a decimal* type.  */
  FROM_STRING (&s, buf, &context);
  IEEE_TO_HOST (s, &f);

  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    dfp_conversion_exceptions (context.status);

  return f;
}
#endif

#if defined (L_sd_to_sf) || defined (L_dd_to_sf) || defined (L_td_to_sf) \
 || defined (L_sd_to_df) || defined (L_dd_to_df) || defined (L_td_to_df) \
 || ((defined (L_sd_to_xf) || defined (L_dd_to_xf) || defined (L_td_to_xf)) \
     && LONG_DOUBLE_HAS_XF_MODE) \
 || ((defined (L_sd_to_tf) || defined (L_dd_to_tf) || defined (L_td_to_tf)) \
     && LONG_DOUBLE_HAS_TF_MODE)
BFP_TYPE
DFP_TO_BFP (DFP_C_TYPE f)
{
  IEEE_TYPE s;
  char buf[BUFMAX];

  HOST_TO_IEEE (f, &s);
  /* Write the value to a string.  */
  TO_STRING (&s, buf);
  /* Read it as the binary floating point type and return that.  */
  return STR_TO_BFP (buf, NULL);
}
#endif
                                                                                
#if defined (L_sf_to_sd) || defined (L_sf_to_dd) || defined (L_sf_to_td) \
 || defined (L_df_to_sd) || defined (L_df_to_dd) || defined (L_df_to_td) \
 || ((defined (L_xf_to_sd) || defined (L_xf_to_dd) || defined (L_xf_to_td)) \
     && LONG_DOUBLE_HAS_XF_MODE) \
 || ((defined (L_tf_to_sd) || defined (L_tf_to_dd) || defined (L_tf_to_td)) \
     && LONG_DOUBLE_HAS_TF_MODE)
DFP_C_TYPE
BFP_TO_DFP (BFP_TYPE x)
{
  DFP_C_TYPE f;
  IEEE_TYPE s;
  char buf[BUFMAX];
  decContext context;

  decContextDefault (&context, CONTEXT_INIT);
  DFP_INIT_ROUNDMODE (context.round);

  /* Use a C library function to write the floating point value to a string.  */
  sprintf (buf, BFP_FMT, (BFP_VIA_TYPE) x);

  /* Convert from the floating point string to a decimal* type.  */
  FROM_STRING (&s, buf, &context);
  IEEE_TO_HOST (s, &f);

  if (DFP_EXCEPTIONS_ENABLED && context.status != 0)
    {
      /* decNumber exception flags we care about here.  */
      int ieee_flags;
      int dec_flags = DEC_IEEE_854_Inexact | DEC_IEEE_854_Invalid_operation
		      | DEC_IEEE_854_Overflow | DEC_IEEE_854_Underflow;
      dec_flags &= context.status;
      ieee_flags = DFP_IEEE_FLAGS (dec_flags);
      if (ieee_flags != 0)
        DFP_HANDLE_EXCEPTIONS (ieee_flags);
    }

  return f;
}
#endif

#if defined (L_unord_sd) || defined (L_unord_dd) || defined (L_unord_td)
CMPtype
DFP_UNORD (DFP_C_TYPE arg_a, DFP_C_TYPE arg_b)
{
  decNumber arg1, arg2;
  IEEE_TYPE a, b;

  HOST_TO_IEEE (arg_a, &a);
  HOST_TO_IEEE (arg_b, &b);
  TO_INTERNAL (&a, &arg1);
  TO_INTERNAL (&b, &arg2);
  return (decNumberIsNaN (&arg1) || decNumberIsNaN (&arg2));
}
#endif /* L_unord_sd || L_unord_dd || L_unord_td */
