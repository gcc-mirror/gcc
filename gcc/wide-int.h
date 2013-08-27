/* Operations with very long integers.  -*- C++ -*-
   Copyright (C) 2012-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef WIDE_INT_H
#define WIDE_INT_H

/* Wide-int.[cc|h] implements a class that efficiently performs
   mathematical operations on finite precision integers.  Wide-ints
   are designed to be transient - they are not for long term storage
   of values.  There is tight integration between wide-ints and the
   other longer storage GCC representations (rtl and tree).

   The actual precision of a wide-int depends on the flavor.  There
   are three predefined flavors:

     1) wide_int (the default).  This flavor does the math in the
     precision of its input arguments.  It is assumed (and checked)
     that the precisions of the operands and results are consistent.
     This is the most efficient flavor.  It is not possible to examine
     bits above the precision that has been specified.  Because of
     this, the default flavor has semantics that are simple to
     understand and in general model the underlying hardware that the
     compiler is targetted for.

     This flavor must be used at the RTL level of gcc because there
     is, in general, not enough information in the RTL representation
     to extend a value beyond the precision specified in the mode.

     This flavor should also be used at the TREE and GIMPLE levels of
     the compiler except for the circumstances described in the
     descriptions of the other two flavors.

     The default wide_int representation does not contain any
     information inherent about signedness of the represented value,
     so it can be used to represent both signed and unsigned numbers.
     For operations where the results depend on signedness (full width
     multiply, division, shifts, comparisons, and operations that need
     overflow detected), the signedness must be specified separately.

     2) addr_wide_int.  This is a fixed size representation that is
     guaranteed to be large enough to compute any bit or byte sized
     address calculation on the target.  Currently the value is 64 + 4
     bits rounded up to the next number even multiple of
     HOST_BITS_PER_WIDE_INT (but this can be changed when the first
     port needs more than 64 bits for the size of a pointer).

     This flavor can be used for all address math on the target.  In
     this representation, the values are sign or zero extended based
     on their input types to the internal precision.  All math is done
     in this precision and then the values are truncated to fit in the
     result type.  Unlike most gimple or rtl intermediate code, it is
     not useful to perform the address arithmetic at the same
     precision in which the operands are represented because there has
     been no effort by the front ends to convert most addressing
     arithmetic to canonical types.

     In the addr_wide_int, all numbers are represented as signed
     numbers.  There are enough bits in the internal representation so
     that no infomation is lost by representing them this way.

     3) max_wide_int.  This representation is an approximation of
     infinite precision math.  However, it is not really infinite
     precision math as in the GMP library.  It is really finite
     precision math where the precision is 4 times the size of the
     largest integer that the target port can represent.

     Like, the addr_wide_ints, all numbers are inherently signed.

     There are several places in the GCC where this should/must be used:

     * Code that does widening conversions.  The canonical way that
       this is performed is to sign or zero extend the input value to
       the max width based on the sign of the type of the source and
       then to truncate that value to the target type.  This is in
       preference to using the sign of the target type to extend the
       value directly (which gets the wrong value for the conversion
       of large unsigned numbers to larger signed types).

     * Code that does induction variable optimizations.  This code
       works with induction variables of many different types at the
       same time.  Because of this, it ends up doing many different
       calculations where the operands are not compatible types.  The
       max_wide_int makes this easy, because it provides a field where
       nothing is lost when converting from any variable,

     * There are a small number of passes that currently use the
       max_wide_int that should use the default.  These should be
       changed.

   There are surprising features of addr_wide_int and max_wide_int
   that the users should be careful about:

     1) Shifts and rotations are just weird.  You have to specify a
     precision in which the shift or rotate is to happen in.  The bits
     above this precision remain unchanged.  While this is what you
     want, it is clearly is non obvious.

     2) Larger precision math sometimes does not produce the same
     answer as would be expected for doing the math at the proper
     precision.  In particular, a multiply followed by a divide will
     produce a different answer if the first product is larger than
     what can be represented in the input precision.

   The addr_wide_int and the max_wide_int flavors are more expensive
   than the default wide int, so in addition to the caveats with these
   two, the default is the prefered representation.

   All three flavors of wide_int are represented as a vector of
   HOST_WIDE_INTs.  The vector contains enough elements to hold a
   value of MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_WIDE_INT which is
   a derived for each host/target combination.  The values are stored
   in the vector with the least significant HOST_BITS_PER_WIDE_INT
   bits of the value stored in element 0.

   A wide_int contains three fields: the vector (VAL), precision and a
   length (LEN).  The length is the number of HWIs needed to
   represent the value.  For the max_wide_int and the addr_wide_int,
   the precision is a constant that cannot be changed.  For the
   default wide_int, the precision is set from the constructor.

   Since most integers used in a compiler are small values, it is
   generally profitable to use a representation of the value that is
   as small as possible.  LEN is used to indicate the number of
   elements of the vector that are in use.  The numbers are stored as
   sign extended numbers as a means of compression.  Leading
   HOST_WIDE_INTs that contain strings of either -1 or 0 are removed
   as long as they can be reconstructed from the top bit that is being
   represented.

   There are constructors to create the various forms of wide-int from
   trees, rtl and constants.  For trees and constants, you can simply say:

             tree t = ...;
	     wide_int x = t;
	     wide_int y = 6;

   However, a little more syntax is required for rtl constants since
   they do have an explicit precision.  To make an rtl into a
   wide_int, you have to pair it with a mode.  The canonical way to do
   this is with std::make_pair as in:

             rtx r = ...
	     wide_int x = std::make_pair (r, mode);

   Wide ints sometimes have a value with the precision of 0.  These
   come from two separate sources:

   * The front ends do sometimes produce values that really have a
     precision of 0.  The only place where these seem to come in are
     the MIN and MAX value for types with a precision of 0.  Asside
     from the computation of these MIN and MAX values, there appears
     to be no other use of true precision 0 numbers so the overloading
     of precision 0 does not appear to be an issue.  These appear to
     be associated with 0 width bit fields.  They are harmless, but
     there are several paths through the wide int code to support this
     without having to special case the front ends.

   * When a constant that has an integer type is converted to a
     wide-int it comes in with precision 0.  For these constants the
     top bit does accurately reflect the sign of that constant; this
     is an exception to the normal rule that the signedness is not
     represented.  When used in a binary operation, the wide-int
     implementation properly extends these constants so that they
     properly match the other operand of the computation.  This allows
     you write:

                tree t = ...
                wide_int x = t + 6;

     assuming t is a int_cst.

   Note that the bits above the precision are not defined and the
   algorithms used here are careful not to depend on their value.  In
   particular, values that come in from rtx constants may have random
   bits.  When the precision is 0, all the bits in the LEN elements of
   VEC are significant with no undefined bits.  Precisionless
   constants are limited to being one or two HOST_WIDE_INTs.  When two
   are used the upper value is 0, and the high order bit of the first
   value is set.  (Note that this may need to be generalized if it is
   ever necessary to support 32bit HWIs again).

   Many binary operations require that the precisions of the two
   operands be the same.  However, the API tries to keep this relaxed
   as much as possible.  In particular:

   * shifts do not care about the precision of the second operand.

   * values that come in from gcc source constants or variables are
     not checked as long one of the two operands has a precision.
     This is allowed because it is always known whether to sign or zero
     extend these values.

   * The comparisons do not require that the operands be the same
     length.  This allows wide ints to be used in hash tables where
     all of the values may not be the same precision.  */


#ifndef GENERATOR_FILE
#include <utility>
#include "tree.h"
#include "system.h"
#include "hwint.h"
#include "options.h"
#include "tm.h"
#include "insn-modes.h"
#include "machmode.h"
#include "double-int.h"
#include <gmp.h>
#include "dumpfile.h"
#include "real.h"
#include "signop.h"

#if 0
#define DEBUG_WIDE_INT
#endif

/* The MAX_BITSIZE_MODE_ANY_INT is automatically generated by a very
   early examination of the target's mode file.  Thus it is safe that
   some small multiple of this number is easily larger than any number
   that that target could compute.  The place in the compiler that
   currently needs the widest ints is the code that determines the
   range of a multiply.  This code needs 2n + 2 bits.  */

#define WIDE_INT_MAX_ELTS \
  ((4 * MAX_BITSIZE_MODE_ANY_INT + HOST_BITS_PER_WIDE_INT - 1) \
   / HOST_BITS_PER_WIDE_INT)

/* This is the max size of any pointer on any machine.  It does not
   seem to be as easy to sniff this out of the machine description as
   it is for MAX_BITSIZE_MODE_ANY_INT since targets may support
   multiple address sizes and may have different address sizes for
   different address spaces.  However, currently the largest pointer
   on any platform is 64 bits.  When that changes, then it is likely
   that a target hook should be defined so that targets can make this
   value larger for those targets.  */
#define ADDR_MAX_BITSIZE 64

/* This is the internal precision used when doing any address
   arithmetic.  The '4' is really 3 + 1.  Three of the bits are for
   the number of extra bits needed to do bit addresses and single bit is
   allow everything to be signed without loosing any precision.  Then
   everything is rounded up to the next HWI for efficiency.  */
#define ADDR_MAX_PRECISION \
  ((ADDR_MAX_BITSIZE + 4 + HOST_BITS_PER_WIDE_INT - 1) & ~(HOST_BITS_PER_WIDE_INT - 1))

/* This is used to bundle an rtx and a mode together so that the pair
   can be used as the second operand of a wide int expression.  If we
   ever put modes into rtx integer constants, this should go away and
   then just pass an rtx in.  */
typedef std::pair <rtx, enum machine_mode> rtx_mode_t;

template <typename T>
inline bool
signedp (T)
{
  return ~(T) 0 < (T) 0;
}

template <>
inline bool
signedp <unsigned int> (unsigned int)
{
  return false;
}

template <>
inline bool
signedp <unsigned long> (unsigned long)
{
  return false;
}

class wide_int;

class GTY(()) wide_int_ro
{
  template <int bitsize>
  friend class fixed_wide_int;
  friend class wide_int;
  /* Internal representation.  */

protected:
  HOST_WIDE_INT val[WIDE_INT_MAX_ELTS];
  unsigned short len;
  unsigned int precision;

  const HOST_WIDE_INT *get_val () const;
  wide_int_ro &operator = (const wide_int_ro &);

public:
  wide_int_ro ();
  wide_int_ro (const_tree);
  wide_int_ro (HOST_WIDE_INT);
  wide_int_ro (int);
  wide_int_ro (unsigned HOST_WIDE_INT);
  wide_int_ro (unsigned int);
  wide_int_ro (const rtx_mode_t &);

  /* Conversions.  */
  static wide_int_ro from_shwi (HOST_WIDE_INT, unsigned int = 0);
  static wide_int_ro from_uhwi (unsigned HOST_WIDE_INT, unsigned int = 0);
  static wide_int_ro from_hwi (HOST_WIDE_INT, const_tree);
  static wide_int_ro from_shwi (HOST_WIDE_INT, enum machine_mode);
  static wide_int_ro from_uhwi (unsigned HOST_WIDE_INT, enum machine_mode);
  static wide_int_ro from_array (const HOST_WIDE_INT *, unsigned int,
				 unsigned int, bool = true);
  static wide_int_ro from_double_int (double_int, unsigned int);
  static wide_int_ro from_buffer (const unsigned char *, int);
  void to_mpz (mpz_t, signop) const;
  static wide_int_ro from_mpz (const_tree, mpz_t, bool);
  HOST_WIDE_INT to_shwi (unsigned int = 0) const;

  unsigned HOST_WIDE_INT to_uhwi (unsigned int = 0) const;
  HOST_WIDE_INT to_short_addr () const;

  static wide_int_ro max_value (unsigned int, signop, unsigned int = 0);
  static wide_int_ro max_value (const_tree);
  static wide_int_ro max_value (enum machine_mode, signop);
  static wide_int_ro min_value (unsigned int, signop, unsigned int = 0);
  static wide_int_ro min_value (const_tree);
  static wide_int_ro min_value (enum machine_mode, signop);

  /* Small constants.  These are generally only needed in the places
     where the precision must be provided.  For instance in binary
     operations where the other operand has a precision, or for use
     with max_wide_int or addr_wide_int, these are never needed.  */
  static wide_int_ro minus_one (unsigned int);
  static wide_int_ro zero (unsigned int);
  static wide_int_ro one (unsigned int);
  static wide_int_ro two (unsigned int);

  /* Public accessors for the interior of a wide int.  */
  unsigned short get_len () const;
  unsigned int get_precision () const;
  HOST_WIDE_INT elt (unsigned int) const;

  /* Comparative functions.  */
  bool minus_one_p () const;
  bool zero_p () const;
  bool one_p () const;
  bool neg_p (signop sgn = SIGNED) const;
  bool multiple_of_p (const wide_int_ro &, signop, wide_int_ro *) const;

  /* Comparisons, note that only equality is an operator.  The other
     comparisons cannot be operators since they are inherently signed or
     unsigned and C++ has no such operators.  */
  template <typename T>
  bool operator == (const T &) const;

  template <typename T1, typename T2>
  static bool eq_p (const T1 &, const T2 &);

  template <typename T>
  bool operator != (const T &) const;

  template <typename T>
  bool lt_p (const T &, signop) const;

  template <typename T1, typename T2>
  static bool lt_p (const T1 &, const T2 &, signop);

  template <typename T>
  bool lts_p (const T &) const;

  template <typename T1, typename T2>
  static bool lts_p (const T1 &, const T2 &);

  template <typename T>
  bool ltu_p (const T &) const;

  template <typename T1, typename T2>
  static bool ltu_p (const T1 &, const T2 &);

  template <typename T>
  bool le_p (const T &, signop) const;

  template <typename T1, typename T2>
  static bool le_p (const T1 &, const T2 &, signop);

  template <typename T>
  bool les_p (const T &) const;

  template <typename T1, typename T2>
  static bool les_p (const T1 &, const T2 &);

  template <typename T>
  bool leu_p (const T &) const;

  template <typename T1, typename T2>
  static bool leu_p (const T1 &, const T2 &);

  template <typename T>
  bool gt_p (const T &, signop) const;

  template <typename T1, typename T2>
  static bool gt_p (const T1 &, const T2 &, signop);

  template <typename T>
  bool gts_p (const T &) const;

  template <typename T1, typename T2>
  static bool gts_p (const T1 &, const T2 &);

  template <typename T>
  bool gtu_p (const T &) const;

  template <typename T1, typename T2>
  static bool gtu_p (const T1 &, const T2 &);

  template <typename T>
  bool ge_p (const T &, signop) const;

  template <typename T1, typename T2>
  static bool ge_p (const T1 &, const T2 &, signop);

  template <typename T>
  bool ges_p (const T &) const;

  template <typename T1, typename T2>
  static bool ges_p (const T1 &, const T2 &);

  template <typename T>
  bool geu_p (const T &) const;

  template <typename T1, typename T2>
  static bool geu_p (const T1 &, const T2 &);

  template <typename T>
  int cmp (const T &, signop) const;

  template <typename T>
  int cmps (const T &) const;

  template <typename T>
  int cmpu (const T &) const;

  bool only_sign_bit_p (unsigned int) const;
  bool only_sign_bit_p () const;
  bool fits_shwi_p () const;
  bool fits_uhwi_p () const;
  bool fits_to_tree_p (const_tree) const;

  /* Min and max.  */

  template <typename T>
  wide_int_ro min (const T &, signop) const;
  wide_int_ro min (const wide_int_ro &, signop) const;

  template <typename T>
  wide_int_ro max (const T &, signop) const;
  wide_int_ro max (const wide_int_ro &, signop) const;

  template <typename T>
  wide_int_ro smin (const T &) const;
  wide_int_ro smin (const wide_int_ro &) const;

  template <typename T>
  wide_int_ro smax (const T &) const;
  wide_int_ro smax (const wide_int_ro &) const;

  template <typename T>
  wide_int_ro umin (const T &) const;
  wide_int_ro umin (const wide_int_ro &) const;

  template <typename T>
  wide_int_ro umax (const T &) const;
  wide_int_ro umax (const wide_int_ro &) const;

  /* Extensions.  These do not change the precision.  */

  wide_int_ro ext (unsigned int, signop) const;
  wide_int_ro sext (unsigned int) const;
  wide_int_ro zext (unsigned int) const;

  /* Size changing.  These change the underlying precision and are not
     available for max_wide_int or addr_wide_int.  */

  wide_int_ro force_to_size (unsigned int, signop) const;
  wide_int_ro sforce_to_size (unsigned int) const;
  wide_int_ro zforce_to_size (unsigned int) const;

  /* Masking, and Insertion.  */

  wide_int_ro set_bit (unsigned int) const;
  static wide_int_ro set_bit_in_zero (unsigned int, unsigned int);
  wide_int_ro insert (const wide_int_ro &, unsigned int, unsigned int) const;

  wide_int_ro bswap () const;

  static wide_int_ro mask (unsigned int, bool, unsigned int);
  static wide_int_ro shifted_mask (unsigned int, unsigned int, bool,
				   unsigned int);

  HOST_WIDE_INT sign_mask () const;

  void clear_undef (signop);

  /* Logicals.  */

  template <typename T>
  wide_int_ro operator & (const T &) const;

  template <typename T>
  wide_int_ro and_not (const T &) const;

  wide_int_ro operator ~ () const;

  template <typename T>
  wide_int_ro operator | (const T &) const;

  template <typename T>
  wide_int_ro or_not (const T &) const;

  template <typename T>
  wide_int_ro operator ^ (const T &) const;

  /* Arithmetic operation functions, alpha sorted (except divmod)..  */

  wide_int_ro abs () const;

  template <typename T>
  wide_int_ro operator + (const T &) const;

  template <typename T>
  wide_int_ro add (const T &, signop, bool *) const;

  wide_int_ro clz () const;
  wide_int_ro clrsb () const;
  wide_int_ro ctz () const;
  wide_int_ro exact_log2 () const;
  wide_int_ro floor_log2 () const;
  wide_int_ro ffs () const;

  template <typename T>
  wide_int_ro operator * (const T &) const;

  template <typename T>
  wide_int_ro mul (const T &, signop, bool *) const;

  template <typename T>
  wide_int_ro smul (const T &, bool *) const;

  template <typename T>
  wide_int_ro umul (const T &, bool *) const;

  template <typename T>
  wide_int_ro mul_full (const T &, signop) const;

  template <typename T>
  wide_int_ro smul_full (const T &) const;

  template <typename T>
  wide_int_ro umul_full (const T &) const;

  template <typename T>
  wide_int_ro mul_high (const T &, signop) const;

  wide_int_ro operator - () const;
  wide_int_ro neg (bool *) const;
  wide_int_ro parity () const;
  wide_int_ro popcount () const;

  template <typename T>
  wide_int_ro operator - (const T &) const;

  template <typename T>
  wide_int_ro sub (const T &, signop, bool *) const;

  /* Division and modulus.  These are the ones that are actually used in
     the compiler.  More can be added where they are needed.  */

  template <typename T>
  wide_int_ro div_trunc (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro sdiv_trunc (const T &) const;

  template <typename T>
  wide_int_ro udiv_trunc (const T &) const;

  template <typename T>
  wide_int_ro div_floor (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro udiv_floor (const T &) const;

  template <typename T>
  wide_int_ro sdiv_floor (const T &) const;

  template <typename T>
  wide_int_ro div_ceil (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro div_round (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro divmod_trunc (const T &, wide_int_ro *, signop) const;

  template <typename T>
  wide_int_ro sdivmod_trunc (const T &, wide_int_ro *) const;

  template <typename T>
  wide_int_ro udivmod_trunc (const T &, wide_int_ro *) const;

  template <typename T>
  wide_int_ro divmod_floor (const T &, wide_int_ro *, signop) const;

  template <typename T>
  wide_int_ro sdivmod_floor (const T &, wide_int_ro *) const;

  template <typename T>
  wide_int_ro mod_trunc (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro smod_trunc (const T &) const;

  template <typename T>
  wide_int_ro umod_trunc (const T &) const;

  template <typename T>
  wide_int_ro mod_floor (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro umod_floor (const T &) const;

  template <typename T>
  wide_int_ro mod_ceil (const T &, signop, bool * = 0) const;

  template <typename T>
  wide_int_ro mod_round (const T &, signop, bool * = 0) const;

  HOST_WIDE_INT extract_to_hwi (int, int) const;

  template <typename T>
  wide_int_ro lshift (const T &, unsigned int = 0) const;

  template <typename T>
  wide_int_ro lshift_widen (const T &, unsigned int) const;

  template <typename T>
  wide_int_ro lrotate (const T &, unsigned int = 0) const;

  wide_int_ro lrotate (unsigned HOST_WIDE_INT, unsigned int = 0) const;

  template <typename T>
  wide_int_ro rshift (const T &, signop, unsigned int = 0) const;

  template <typename T>
  wide_int_ro rshiftu (const T &, unsigned int = 0) const;

  template <typename T>
  wide_int_ro rshifts (const T &, unsigned int = 0) const;

  template <typename T>
  wide_int_ro rrotate (const T &, unsigned int = 0) const;

  wide_int_ro rrotate (unsigned HOST_WIDE_INT, unsigned int = 0) const;

  char *dump (char *) const;

private:
  /* Internal versions that do the work if the values do not fit in a HWI.  */

  /* Comparisons */
  static bool eq_p_large (const HOST_WIDE_INT *, unsigned int, unsigned int,
			  const HOST_WIDE_INT *, unsigned int);
  static bool lts_p_large (const HOST_WIDE_INT *, unsigned int, unsigned int,
			   const HOST_WIDE_INT *, unsigned int, unsigned int);
  static int cmps_large (const HOST_WIDE_INT *, unsigned int, unsigned int,
			 const HOST_WIDE_INT *, unsigned int, unsigned int);
  static bool ltu_p_large (const HOST_WIDE_INT *, unsigned int, unsigned int,
			   const HOST_WIDE_INT *, unsigned int, unsigned int);
  static int cmpu_large (const HOST_WIDE_INT *, unsigned int, unsigned int,
			 const HOST_WIDE_INT *, unsigned int, unsigned int);
  static void check_precision (unsigned int *, unsigned int *, bool, bool);


  /* Logicals.  */
  static wide_int_ro and_large (const HOST_WIDE_INT *, unsigned int,
				unsigned int, const HOST_WIDE_INT *,
				unsigned int);
  static wide_int_ro and_not_large (const HOST_WIDE_INT *, unsigned int,
				    unsigned int, const HOST_WIDE_INT *,
				    unsigned int);
  static wide_int_ro or_large (const HOST_WIDE_INT *, unsigned int,
			       unsigned int, const HOST_WIDE_INT *,
			       unsigned int);
  static wide_int_ro or_not_large (const HOST_WIDE_INT *, unsigned int,
				   unsigned int, const HOST_WIDE_INT *,
				   unsigned int);
  static wide_int_ro xor_large (const HOST_WIDE_INT *, unsigned int,
				unsigned int, const HOST_WIDE_INT *,
				unsigned int);

  /* Arithmetic */
  static wide_int_ro add_large (const HOST_WIDE_INT *, unsigned int,
				unsigned int, const HOST_WIDE_INT *,
				unsigned int, signop, bool * = 0);
  static wide_int_ro sub_large (const HOST_WIDE_INT *, unsigned int,
				unsigned int, const HOST_WIDE_INT *,
				unsigned int, signop, bool * = 0);

  wide_int_ro lshift_large (unsigned int, unsigned int) const;
  wide_int_ro rshiftu_large (unsigned int) const;
  wide_int_ro rshifts_large (unsigned int) const;

  static wide_int_ro mul_internal (bool, bool, const HOST_WIDE_INT *,
				   unsigned int, unsigned int,
				   const HOST_WIDE_INT *, unsigned int,
				   signop, bool *, bool);
  static void divmod_internal_2 (unsigned HOST_HALF_WIDE_INT *,
				 unsigned HOST_HALF_WIDE_INT *,
				 unsigned HOST_HALF_WIDE_INT *,
				 unsigned HOST_HALF_WIDE_INT *, int, int);
  static wide_int_ro divmod_internal (bool, const HOST_WIDE_INT *,
				      unsigned int, unsigned int,
				      const HOST_WIDE_INT *,
				      unsigned int, unsigned int,
				      signop, wide_int_ro *, bool, bool *);

  /* Private utility routines.  */
  int trunc_shift (const HOST_WIDE_INT *cnt, unsigned int bitsize) const;
  wide_int_ro decompress (unsigned int, unsigned int) const;
  void canonize ();
  static wide_int_ro from_rtx (const rtx_mode_t);

  template <typename T>
  static bool top_bit_set (T);

  template <typename T>
  static const HOST_WIDE_INT *to_shwi1 (HOST_WIDE_INT *, unsigned int *,
					unsigned int *, const T &);

  template <typename T>
  static const HOST_WIDE_INT *to_shwi2 (HOST_WIDE_INT *, unsigned int *,
					const T &);

#ifdef DEBUG_WIDE_INT
  /* Debugging routines.  */
  static void debug_wa (const char *, const wide_int_ro &,
			const HOST_WIDE_INT *, unsigned int, unsigned int);
  static void debug_waa (const char *, const wide_int_ro &,
			 const HOST_WIDE_INT *, unsigned int,
			 unsigned int, const HOST_WIDE_INT *,
			 unsigned int, unsigned int);
  static void debug_waav (const char *, const wide_int_ro &,
			  const HOST_WIDE_INT *, unsigned int,
			  unsigned int, const HOST_WIDE_INT *,
			  unsigned int, unsigned int, int);
  static void debug_vw  (const char *, int, const wide_int_ro &);
  static void debug_vwh (const char *, int, const wide_int_ro &,
			 HOST_WIDE_INT);
  static void debug_vaa (const char *, int, const HOST_WIDE_INT *,
			 unsigned int, unsigned int, const HOST_WIDE_INT *,
			 unsigned int, unsigned int);
  static void debug_vwa (const char *, int, const wide_int_ro &,
			 const HOST_WIDE_INT *, unsigned int, unsigned int);
  static void debug_vww (const char *, int, const wide_int_ro &,
			 const wide_int_ro &);
  static void debug_wh (const char *, const wide_int_ro &, HOST_WIDE_INT);
  static void debug_whh (const char *, const wide_int_ro &,
			 HOST_WIDE_INT, HOST_WIDE_INT);
  static void debug_wv (const char *, const wide_int_ro &, int);
  static void debug_wvv (const char *, const wide_int_ro &, int, int);
  static void debug_wvvv (const char *, const wide_int_ro &, int, int, int);
  static void debug_wvwa (const char *, const wide_int_ro &, int,
			  const wide_int_ro &, const HOST_WIDE_INT *,
			  unsigned int, unsigned int);
  static void debug_wvasa (const char *, const wide_int_ro &, int,
			   const HOST_WIDE_INT *, unsigned int, unsigned int,
			   const char *, const HOST_WIDE_INT *,
			   unsigned int, unsigned int);
  static void debug_wvww (const char *, const wide_int_ro &, int,
			  const wide_int_ro &, const wide_int_ro &);
  static void debug_wwa (const char *, const wide_int_ro &,
			 const wide_int_ro &, const HOST_WIDE_INT *,
			 unsigned int, unsigned int);
  static void debug_wwv (const char *, const wide_int_ro &,
			 const wide_int_ro &, int);
  static void debug_wwvs (const char *, const wide_int_ro &,
			  const wide_int_ro &, int, const char *);
  static void debug_wwvvs (const char *, const wide_int_ro &,
			   const wide_int_ro &, int, int, const char *);
  static void debug_wwwvv (const char *, const wide_int_ro &,
			   const wide_int_ro &, const wide_int_ro &, int, int);
  static void debug_ww (const char *, const wide_int_ro &,
			const wide_int_ro &);
  static void debug_www (const char *, const wide_int_ro &,
			 const wide_int_ro &, const wide_int_ro &);
  static void debug_wwasa (const char *, const wide_int_ro &,
			   const wide_int_ro &, const HOST_WIDE_INT *,
			   unsigned int, unsigned int, const char *,
			   const HOST_WIDE_INT *, unsigned int, unsigned int);
  static void debug_wwwa (const char *, const wide_int_ro &,
			  const wide_int_ro &, const wide_int_ro &,
			  const HOST_WIDE_INT *, unsigned int, unsigned int);
  static void debug_wwww (const char *, const wide_int_ro &,
			  const wide_int_ro &, const wide_int_ro &,
			  const wide_int_ro &);
#endif
};

class GTY(()) wide_int : public wide_int_ro {
 public:
  wide_int ();
  wide_int (const wide_int_ro &);
  wide_int (const_tree);
  wide_int (HOST_WIDE_INT);
  wide_int (int);
  wide_int (unsigned HOST_WIDE_INT);
  wide_int (unsigned int);
  wide_int (const rtx_mode_t &);

  wide_int &operator = (const wide_int_ro &);
  wide_int &operator = (const_tree);
  wide_int &operator = (HOST_WIDE_INT);
  wide_int &operator = (int);
  wide_int &operator = (unsigned HOST_WIDE_INT);
  wide_int &operator = (unsigned int);
  wide_int &operator = (const rtx_mode_t &);

  wide_int &operator ++ ();
  wide_int& operator -- ();

  template <typename T>
  wide_int &operator &= (const T &);

  template <typename T>
   wide_int &operator |= (const T &);

  template <typename T>
  wide_int &operator ^= (const T &);

  template <typename T>
  wide_int &operator += (const T &);

  template <typename T>
  wide_int &operator -= (const T &);

  template <typename T>
  wide_int &operator *= (const T &);
};

inline const HOST_WIDE_INT *
wide_int_ro::get_val () const
{
  return val;
}

inline wide_int_ro &
wide_int_ro::operator = (const wide_int_ro &r)
{
  for (unsigned int i = 0; i < r.get_len (); ++i)
    val[i] = r.get_val () [i];
  len = r.get_len ();
  precision = r.get_precision ();
  return *this;
}

inline wide_int_ro::wide_int_ro () : len (0) {}

/* Convert an INTEGER_CST into a wide int.  */
inline wide_int_ro::wide_int_ro (const_tree tcst)
{
  *this = from_array (&TREE_INT_CST_ELT (tcst, 0),
		      TREE_INT_CST_NUNITS (tcst),
		      TYPE_PRECISION (TREE_TYPE (tcst)), false);
}

inline wide_int_ro::wide_int_ro (HOST_WIDE_INT op0)
{
  precision = 0;
  val[0] = op0;
  len = 1;
}

inline wide_int_ro::wide_int_ro (int op0)
{
  precision = 0;
  val[0] = op0;
  len = 1;
}

inline wide_int_ro::wide_int_ro (unsigned HOST_WIDE_INT op0)
{
  *this = from_uhwi (op0);
}

inline wide_int_ro::wide_int_ro (unsigned int op0)
{
  *this = from_uhwi (op0);
}

inline wide_int_ro::wide_int_ro (const rtx_mode_t &op0)
{
  *this = from_rtx (op0);
}

/* Convert signed OP0 into a wide_int_ro with the precision taken from
   MODE.  */
inline wide_int_ro
wide_int_ro::from_shwi (HOST_WIDE_INT op0, enum machine_mode mode)
{
  unsigned int prec = GET_MODE_PRECISION (mode);
  return from_shwi (op0, prec);
}

/* Convert unsigned OP0 into a wide_int_ro with the precision taken from
   MODE.  */
inline wide_int_ro
wide_int_ro::from_uhwi (unsigned HOST_WIDE_INT op0, enum machine_mode mode)
{
  unsigned int prec = GET_MODE_PRECISION (mode);
  return from_uhwi (op0, prec);
}

/* Convert OP0 into a wide_int with parameters taken from TYPE.  */
inline wide_int_ro
wide_int_ro::from_hwi (HOST_WIDE_INT op0, const_tree type)
{
  unsigned int prec = TYPE_PRECISION (type);

  if (TYPE_UNSIGNED (type))
    return wide_int_ro::from_uhwi (op0, prec);
  else
    return wide_int_ro::from_shwi (op0, prec);
}

/* Return THIS as a signed HOST_WIDE_INT.  If THIS does not fit in PREC,
   the information is lost.  */
inline HOST_WIDE_INT
wide_int_ro::to_shwi (unsigned int prec) const
{
  HOST_WIDE_INT result;

  if (prec == 0)
    prec = precision;

  if (prec < HOST_BITS_PER_WIDE_INT)
    result = sext_hwi (val[0], prec);
  else
    result = val[0];

  return result;
}

/* Return THIS as an unsigned HOST_WIDE_INT.  If THIS does not fit in PREC,
   the information is lost.  */
inline unsigned HOST_WIDE_INT
wide_int_ro::to_uhwi (unsigned int prec) const
{
  HOST_WIDE_INT result;

  if (prec == 0)
    prec = precision;

  if (prec < HOST_BITS_PER_WIDE_INT)
    result = zext_hwi (val[0], prec);
  else
    result = val[0];

  return result;
}

/* TODO: The compiler is half converted from using HOST_WIDE_INT to
   represent addresses to using wide_int_ro to represent addresses.
   We use to_short_addr at the interface from new code to old,
   unconverted code.  */
inline HOST_WIDE_INT
wide_int_ro::to_short_addr () const
{
  return val[0];
}

/* Produce the largest number that is represented in TYPE.  The precision
   and sign are taken from TYPE.  */
inline wide_int_ro
wide_int_ro::max_value (const_tree type)
{
  unsigned int prec = TYPE_PRECISION (type);
  return max_value (prec, TYPE_SIGN (type), prec);
}

/* Produce the largest number that is represented in MODE.  The precision
   is taken from MODE and the sign from SGN.  */
inline wide_int_ro
wide_int_ro::max_value (enum machine_mode mode, signop sgn)
{
  unsigned int prec = GET_MODE_PRECISION (mode);
  return max_value (prec, sgn, prec);
}

/* Produce the smallest number that is represented in TYPE.  The precision
   and sign are taken from TYPE.  */
inline wide_int_ro
wide_int_ro::min_value (const_tree type)
{
  unsigned int prec = TYPE_PRECISION (type);
  return min_value (prec, TYPE_SIGN (type), prec);
}

/* Produce the smallest number that is represented in MODE.  The precision
   is taken from MODE and the sign from SGN.  */
inline wide_int_ro
wide_int_ro::min_value (enum machine_mode mode, signop sgn)
{
  unsigned int prec = GET_MODE_PRECISION (mode);
  return min_value (prec, sgn, prec);
}

/* Return a wide int of -1 with precision PREC.  */
inline wide_int_ro
wide_int_ro::minus_one (unsigned int prec)
{
  return from_shwi (-1, prec);
}

/* Return a wide int of 0 with precision PREC.  */
inline wide_int_ro
wide_int_ro::zero (unsigned int prec)
{
  return from_shwi (0, prec);
}

/* Return a wide int of 1 with precision PREC.  */
inline wide_int_ro
wide_int_ro::one (unsigned int prec)
{
  return from_shwi (1, prec);
}

/* Return a wide int of 2 with precision PREC.  */
inline wide_int_ro
wide_int_ro::two (unsigned int prec)
{
  return wide_int_ro::from_shwi (2, prec);
}

/* Get the number of HOST_WIDE_INTs actually represented within the
   wide int.  */
inline unsigned short
wide_int_ro::get_len () const
{
  return len;
}

/* Get the precision of the value represented within the wide int.  */
inline unsigned int
wide_int_ro::get_precision () const
{
  return precision;
}

/* Get a particular element of the wide int.  */
inline HOST_WIDE_INT
wide_int_ro::elt (unsigned int i) const
{
  return i >= len ? sign_mask () : val[i];
}

/* Return true if THIS is -1.  This is correct even if precision is 0.  */
inline bool
wide_int_ro::minus_one_p () const
{
  HOST_WIDE_INT x;

  if (precision && precision < HOST_BITS_PER_WIDE_INT)
    x = sext_hwi (val[0], precision);
  else
    x = val[0];

  return len == 1 && x == (HOST_WIDE_INT)-1;
}

/* Return true if THIS is 0.  This is correct even if precision is 0.  */
inline bool
wide_int_ro::zero_p () const
{
  HOST_WIDE_INT x;

  if (precision && precision < HOST_BITS_PER_WIDE_INT)
    x = sext_hwi (val[0], precision);
  else
    x = val[0];

  return len == 1 && x == 0;
}

/* Return true if THIS is 1.  This is correct even if precision is 0.  */
inline bool
wide_int_ro::one_p () const
{
  HOST_WIDE_INT x;

  if (precision && precision < HOST_BITS_PER_WIDE_INT)
    x = zext_hwi (val[0], precision);
  else
    x = val[0];

  return len == 1 && x == 1;
}

/* Return true if THIS is negative based on the interpretation of SGN.
   For UNSIGNED, this is always false.  This is correct even if
   precision is 0.  */
inline bool
wide_int_ro::neg_p (signop sgn) const
{
  if (sgn == UNSIGNED)
    return false;

  if (precision == 0)
    return (len == 1 && val[0] < 0);

  return sign_mask () != 0;
}

/* Return true if THIS == C.  If both operands have nonzero precisions,
   the precisions must be the same.  */
template <typename T>
inline bool
wide_int_ro::operator == (const T &c) const
{
  bool result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, false);

  if (p1 == 0)
    /* There are prec 0 types and we need to do this to check their
       min and max values.  */
    result = (len == cl) && (val[0] == s[0]);
  else if (p1 < HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << p1) - 1;
      result = (val[0] & mask) == (s[0] & mask);
    }
  else if (p1 == HOST_BITS_PER_WIDE_INT)
    result = val[0] == s[0];
  else
    result = eq_p_large (val, len, p1, s, cl);

  if (result)
    gcc_assert (len == cl);

#ifdef DEBUG_WIDE_INT
  debug_vwa ("wide_int_ro:: %d = (%s == %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return true if C1 == C2.  If both parameters have nonzero precisions,
   then those precisions must be equal.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::eq_p (const T1 &c1, const T2 &c2)
{
  bool result;
  HOST_WIDE_INT ws1[WIDE_INT_MAX_ELTS];
  HOST_WIDE_INT ws2[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s1, *s2;  /* Returned data */
  unsigned int cl1, cl2;         /* array lengths  */
  unsigned int p1, p2;           /* precisions */

  s1 = to_shwi1 (ws1, &cl1, &p1, c1);
  s2 = to_shwi1 (ws2, &cl2, &p2, c2);
  check_precision (&p1, &p2, true, false);

  if (p1 == 0)
    /* There are prec 0 types and we need to do this to check their
       min and max values.  */
    result = (cl1 == cl2) && (s1[0] == s2[0]);
  else if (p1 < HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << p1) - 1;
      result = (s1[0] & mask) == (s2[0] & mask);
    }
  else if (p1 == HOST_BITS_PER_WIDE_INT)
    result = s1[0] == s2[0];
  else
    result = eq_p_large (s1, cl1, p1, s2, cl2);

  return result;
}

/* Return true if THIS != C.  If both parameters have nonzero precisions,
   then those precisions must be equal.  */
template <typename T>
inline bool
wide_int_ro::operator != (const T &c) const
{
  return !(*this == c);
}

/* Return true if THIS < C using signed comparisons.  */
template <typename T>
inline bool
wide_int_ro::lts_p (const T &c) const
{
  bool result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT
      && p2 <= HOST_BITS_PER_WIDE_INT)
    {
      gcc_assert (cl != 0);
      HOST_WIDE_INT x0 = sext_hwi (val[0], p1);
      HOST_WIDE_INT x1 = sext_hwi (s[0], p2);
      result = x0 < x1;
    }
  else
    result = lts_p_large (val, len, p1, s, cl, p2);

#ifdef DEBUG_WIDE_INT
  debug_vwa ("wide_int_ro:: %d = (%s lts_p %s\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return true if C1 < C2 using signed comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::lts_p (const T1 &c1, const T2 &c2)
{
  bool result;
  HOST_WIDE_INT ws1[WIDE_INT_MAX_ELTS];
  HOST_WIDE_INT ws2[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s1, *s2;  /* Returned data */
  unsigned int cl1, cl2;         /* array lengths  */
  unsigned int p1, p2;           /* precisions */

  s1 = to_shwi1 (ws1, &cl1, &p1, c1);
  s2 = to_shwi1 (ws2, &cl2, &p2, c2);
  check_precision (&p1, &p2, false, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT
      && p2 <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT x0 = sext_hwi (s1[0], p1);
      HOST_WIDE_INT x1 = sext_hwi (s2[0], p2);
      result = x0 < x1;
    }
  else
    result = lts_p_large (s1, cl1, p1, s2, cl2, p2);

#ifdef DEBUG_WIDE_INT
  debug_vaa ("wide_int_ro:: %d = (%s lts_p %s\n", result, s1, cl1, p1, s2, cl2, p2);
#endif
  return result;
}

/* Return true if THIS < C using unsigned comparisons.  */
template <typename T>
inline bool
wide_int_ro::ltu_p (const T &c) const
{
  bool result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT
      && p2 <= HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT x0 = zext_hwi (val[0], p1);
      unsigned HOST_WIDE_INT x1 = zext_hwi (s[0], p2);
      result = x0 < x1;
    }
  else
    result = ltu_p_large (val, len, p1, s, cl, p2);

#ifdef DEBUG_WIDE_INT
  debug_vwa ("wide_int_ro:: %d = (%s ltu_p %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return true if C1 < C2 using unsigned comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::ltu_p (const T1 &c1, const T2 &c2)
{
  bool result;
  HOST_WIDE_INT ws1[WIDE_INT_MAX_ELTS];
  HOST_WIDE_INT ws2[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s1, *s2;  /* Returned data */
  unsigned int cl1, cl2;         /* array lengths  */
  unsigned int p1, p2;           /* precisions */

  s1 = to_shwi1 (ws1, &cl1, &p1, c1);
  s2 = to_shwi1 (ws2, &cl2, &p2, c2);
  check_precision (&p1, &p2, false, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT
      && p2 <= HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT x0 = zext_hwi (s1[0], p1);
      unsigned HOST_WIDE_INT x1 = zext_hwi (s2[0], p2);
      result = x0 < x1;
    }
  else
    result = ltu_p_large (s1, cl1, p1, s2, cl2, p2);
#ifdef DEBUG_WIDE_INT
  debug_vaa ("wide_int_ro:: %d = (%s ltu_p %s)\n", result, s1, cl1, p1, s2, cl2, p2);
#endif
  return result;
}

/* Return true if THIS < C.  Signedness is indicated by SGN.  */
template <typename T>
inline bool
wide_int_ro::lt_p (const T &c, signop sgn) const
{
  if (sgn == SIGNED)
    return lts_p (c);
  else
    return ltu_p (c);
}

/* Return true if C1 < C2.  Signedness is indicated by SGN.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::lt_p (const T1 &c1, const T2 &c2, signop sgn)
{
  if (sgn == SIGNED)
    return lts_p (c1, c2);
  else
    return ltu_p (c1, c2);
}

/* Return true if THIS <= C using signed comparisons.  */
template <typename T>
inline bool
wide_int_ro::les_p (const T &c) const
{
  return !gts_p (c);
}

/* Return true if C1 <= C2 using signed comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::les_p (const T1 &c1, const T2 &c2)
{
  return !gts_p (c1, c2);
}

/* Return true if THIS <= C using unsigned comparisons.  */
template <typename T>
inline bool
wide_int_ro::leu_p (const T &c) const
{
  return !gtu_p (c);
}

/* Return true if C1 <= C2 using unsigned comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::leu_p (const T1 &c1, const T2 &c2)
{
  return !gtu_p (c1, c2);
}

/* Return true if THIS <= C.  Signedness is indicated by SGN.  */
template <typename T>
inline bool
wide_int_ro::le_p (const T &c, signop sgn) const
{
  if (sgn == SIGNED)
    return les_p (c);
  else
    return leu_p (c);
}

/* Return true if C1 <= C2.  Signedness is indicated by SGN.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::le_p (const T1 &c1, const T2 &c2, signop sgn)
{
  if (sgn == SIGNED)
    return les_p (c1, c2);
  else
    return leu_p (c1, c2);
}

/* Return true if THIS > C using signed comparisons.  */
template <typename T>
inline bool
wide_int_ro::gts_p (const T &c) const
{
  return lts_p (c, *this);
}

/* Return true if C1 > C2 using signed comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::gts_p (const T1 &c1, const T2 &c2)
{
  return lts_p (c2, c1);
}

/* Return true if THIS > C using unsigned comparisons.  */
template <typename T>
inline bool
wide_int_ro::gtu_p (const T &c) const
{
  return ltu_p (c, *this);
}

/* Return true if C1 > C2 using unsigned comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::gtu_p (const T1 &c1, const T2 &c2)
{
  return ltu_p (c2, c1);
}

/* Return true if THIS > C.  Signedness is indicated by SGN.  */
template <typename T>
inline bool
wide_int_ro::gt_p (const T &c, signop sgn) const
{
  if (sgn == SIGNED)
    return gts_p (c);
  else
    return gtu_p (c);
}

/* Return true if C1 > C2.  Signedness is indicated by SGN.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::gt_p (const T1 &c1, const T2 &c2, signop sgn)
{
  if (sgn == SIGNED)
    return gts_p (c1, c2);
  else
    return gtu_p (c1, c2);
}

/* Return true if THIS >= C using signed comparisons.  */
template <typename T>
inline bool
wide_int_ro::ges_p (const T &c) const
{
  return !lts_p (c);
}

/* Return true if C1 >= C2 using signed comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::ges_p (const T1 &c1, const T2 &c2)
{
  return !lts_p (c1, c2);
}

/* Return true if THIS >= C using unsigned comparisons.  */
template <typename T>
inline bool
wide_int_ro::geu_p (const T &c) const
{
  return !ltu_p (c);
}

/* Return true if C1 >= C2 using unsigned comparisons.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::geu_p (const T1 &c1, const T2 &c2)
{
  return !ltu_p (c1, c2);
}

/* Return true if THIS >= C.  Signedness is indicated by SGN.  */
template <typename T>
inline bool
wide_int_ro::ge_p (const T &c, signop sgn) const
{
  if (sgn == SIGNED)
    return ges_p (c);
  else
    return geu_p (c);
}

/* Return true if C1 >= C2.  Signedness is indicated by SGN.  */
template <typename T1, typename T2>
inline bool
wide_int_ro::ge_p (const T1 &c1, const T2 &c2, signop sgn)
{
  if (sgn == SIGNED)
    return ges_p (c1, c2);
  else
    return geu_p (c1, c2);
}

/* Returns -1 if THIS < C, 0 if THIS == C and 1 if A > C using
   signed compares.  */
template <typename T>
inline int
wide_int_ro::cmps (const T &c) const
{
  int result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int prec;

  s = to_shwi1 (ws, &cl, &prec, c);
  if (prec == 0)
    prec = precision;

  if (precision <= HOST_BITS_PER_WIDE_INT
      && prec <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT x0 = sext_hwi (val[0], precision);
      HOST_WIDE_INT x1 = sext_hwi (s[0], prec);

      if (x0 < x1)
	result = -1;
      else if (x0 > x1)
	result = 1;
      else
	result = 0;
    }
  else
    result = cmps_large (val, len, precision, s, cl, prec);

#ifdef DEBUG_WIDE_INT
  debug_vwa ("wide_int_ro:: %d = (%s cmps %s)\n", result, *this, s, cl, prec);
#endif
  return result;
}

/* Returns -1 if THIS < C, 0 if THIS == C and 1 if A > C using
   unsigned compares.  */
template <typename T>
int
wide_int_ro::cmpu (const T &c) const
{
  int result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int prec;

  s = to_shwi1 (ws, &cl, &prec, c);
  if (prec == 0)
    prec = precision;

  if (precision <= HOST_BITS_PER_WIDE_INT
      && prec <= HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT x0 = zext_hwi (val[0], precision);
      unsigned HOST_WIDE_INT x1 = zext_hwi (s[0], prec);

      if (x0 < x1)
	result = -1;
      else if (x0 == x1)
	result = 0;
      else
	result = 1;
    }
  else
    result = cmpu_large (val, len, precision, s, cl, prec);

#ifdef DEBUG_WIDE_INT
  debug_vwa ("wide_int_ro:: %d = (%s cmpu %s)\n", result, *this, s, cl, prec);
#endif

  return result;
}

/* Return -1, 0 or 1 depending on how THIS compares with C.
   Signedness is indicated by SGN.  */
template <typename T>
inline int
wide_int_ro::cmp (const T &c, signop sgn) const
{
  if (sgn == SIGNED)
    return cmps (c);
  else
    return cmpu (c);
}

/* Return true if THIS has the sign bit set to 1 and all other bits
   are zero.  */
inline bool
wide_int_ro::only_sign_bit_p () const
{
  return only_sign_bit_p (precision);
}

/* Return true if THIS fits in a HOST_WIDE_INT with no loss of
   precision.  */
inline bool
wide_int_ro::fits_shwi_p () const
{
  return len == 1;
}

/* Return true if THIS fits in an unsigned HOST_WIDE_INT with no
   loss of precision.  */
inline bool
wide_int_ro::fits_uhwi_p () const
{
  return (precision <= HOST_BITS_PER_WIDE_INT)
    || (len == 1 && val[0] >= 0) 
    || (len == 2 && (precision >= 2 * HOST_BITS_PER_WIDE_INT) && (val[1] == 0))
    || (len == 2 && (sext_hwi (val[1], precision & (HOST_BITS_PER_WIDE_INT - 1)) == 0));
}

/* Return the signed or unsigned min of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::min (const T &c, signop sgn) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (sgn == SIGNED)
    return lts_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
  else
    return ltu_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the signed or unsigned min of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::min (const wide_int_ro &op1, signop sgn) const
{
  if (sgn == SIGNED)
    return lts_p (op1) ? (*this) : op1;
  else
    return ltu_p (op1) ? (*this) : op1;
}

/* Return the signed or unsigned max of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::max (const T &c, signop sgn) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);
  if (sgn == SIGNED)
    return gts_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
  else
    return gtu_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the signed or unsigned max of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::max (const wide_int_ro &op1, signop sgn) const
{
  if (sgn == SIGNED)
    return gts_p (op1) ? (*this) : op1;
  else
    return gtu_p (op1) ? (*this) : op1;
}

/* Return the signed min of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::smin (const T &c) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return lts_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the signed min of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::smin (const wide_int_ro &op1) const
{
  return lts_p (op1) ? (*this) : op1;
}

/* Return the signed max of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::smax (const T &c) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return gts_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the signed max of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::smax (const wide_int_ro &op1) const
{
  return gts_p (op1) ? (*this) : op1;
}

/* Return the unsigned min of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umin (const T &c) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  return ltu_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the unsigned min of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::umin (const wide_int_ro &op1) const
{
  return ltu_p (op1) ? (*this) : op1;
}

/* Return the unsigned max of THIS and C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umax (const T &c) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;

  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return gtu_p (c) ? (*this) : wide_int_ro::from_array (s, cl, p1, false);
}

/* Return the unsigned max of THIS and OP1.  */
inline wide_int_ro
wide_int_ro::umax (const wide_int_ro &op1) const
{
  return gtu_p (op1) ? (*this) : op1;
}

/* Return THIS extended to PREC.  The signedness of the extension is
   specified by SGN.  */
inline wide_int_ro
wide_int_ro::ext (unsigned int prec, signop sgn) const
{
  if (sgn == UNSIGNED)
    return zext (prec);
  else
    return sext (prec);
}

/* Return THIS forced to the size PREC.  This is sign extended if
   needed.  */
inline wide_int_ro
wide_int_ro::sforce_to_size (unsigned int prec) const
{
  return force_to_size (prec, SIGNED);
}

/* Return THIS forced to the size PREC.  This is zero extended if
   needed.  */
inline wide_int_ro
wide_int_ro::zforce_to_size (unsigned int prec) const
{
  return force_to_size (prec, UNSIGNED);
}

/* Produce 0 or -1 that is the smear of the sign bit.  */
inline HOST_WIDE_INT
wide_int_ro::sign_mask () const
{
  if (precision < HOST_BITS_PER_WIDE_INT)
    {
      /* We don't allow a int:0 inside a struct to get this far,
	 nor a value of indefinite precision.  */
      gcc_assert (precision != 0);
      return ((val[0] << (HOST_BITS_PER_WIDE_INT - precision))
	      >> (HOST_BITS_PER_WIDE_INT - 1));
    }

  /* TREE_VRP is not able to see that it is not possible for len to be
     0.  So without this test, it warns about this which causes
     bootstrap failures.  */
  if (len < 1)
    gcc_unreachable ();
  else
    return val[len - 1] >> (HOST_BITS_PER_WIDE_INT - 1);
}

/* Return THIS & C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator & (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] & s[0];
    }
  else
    result = and_large (val, len, p1, s, cl);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s & %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS & ~C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::and_not (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] & ~s[0];
    }
  else
    result = and_not_large (val, len, p1, s, cl);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s &~ %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return the logical negation (bitwise complement) of THIS.  */
inline wide_int_ro
wide_int_ro::operator ~ () const
{
  wide_int_ro result;
  int l0 = len - 1;

  result.len = len;
  result.precision = precision;

  while (l0 >= 0)
    {
      result.val[l0] = ~val[l0];
      l0--;
    }

#ifdef DEBUG_WIDE_INT
  debug_ww ("wide_int_ro:: %s = (~ %s)\n", result, *this);
#endif
  return result;
}

/* Return THIS | C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator | (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] | s[0];
    }
  else
    result = or_large (val, len, p1, s, cl);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s | %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS | ~C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::or_not (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] | ~s[0];
    }
  else
    result = or_not_large (val, len, p1, s, cl);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s |~ %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS ^ C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator ^ (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] ^ s[0];
    }
  else
    result = xor_large (val, len, p1, s, cl);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s ^ %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS + C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator + (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] + s[0];
      if (precision < HOST_BITS_PER_WIDE_INT)
	result.val[0] = sext_hwi (result.val[0], p1);
    }
  else
    result = add_large (val, len, p1, s, cl, UNSIGNED, 0);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s + %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS + C.  OVERFLOW is set based on the sign of the
   operation that is specified in SGN.  */
template <typename T>
inline wide_int_ro
wide_int_ro::add (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] + s[0];
      if (p1 < HOST_BITS_PER_WIDE_INT)
	result.val[0] = sext_hwi (result.val[0], p1);
      if (sgn == SIGNED)
	{
	  HOST_WIDE_INT x
	    = (((result.val[0] ^ val[0]) & (result.val[0] ^ s[0]))
	       >> (p1 - 1)) & 1;
	  *overflow = (x != 0);
	}
      else
	*overflow = ((unsigned HOST_WIDE_INT) result.val[0]
		     < (unsigned HOST_WIDE_INT) val[0]);
      }
  else
    result = add_large (val, len, p1, s, cl, sgn, overflow);

#ifdef DEBUG_WIDE_INT
  debug_waav ("wide_int_ro:: %s = (%s + %s) O=%d\n",
	      result, val, len, p1, s, cl, p1, *overflow);
#endif
  return result;
}

/* Multiply THIS and C.  The result is the same precision as the operands,
   so there is no reason for signed or unsigned versions.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator * (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  bool overflow = false;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] * s[0];
      if (precision < HOST_BITS_PER_WIDE_INT)
	result.val[0] = sext_hwi (result.val[0], precision);
    }
  else
    result = mul_internal (false, false,
			   val, len, p1,
			   s, cl, UNSIGNED, &overflow, false);
#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s * %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Multiply THIS and C.  The signedness is specified with SGN.
   OVERFLOW is set true if the result overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mul (const T &c, signop sgn, bool *overflow) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  if (overflow)
    *overflow = false;
  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return mul_internal (false, false,
		       val, len, p1,
		       s, cl, sgn, overflow, true);
}

/* Signed multiply THIS and C.  The result is the same precision
   as the operands.  OVERFLOW is set true if the result overflows,
   false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::smul (const T &c, bool *overflow) const
{
  return mul (c, SIGNED, overflow);
}

/* Unsigned multiply THIS and C.  The result is the same precision
   as the operands.  OVERFLOW is set true if the result overflows,
   false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umul (const T &c, bool *overflow) const
{
  return mul (c, UNSIGNED, overflow);
}

/* Multiply THIS and C.  The signedness is specified with SGN.
   The result is twice the precision of the operands.  The signedness
   is specified with SGN.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mul_full (const T &c, signop sgn) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return mul_internal (false, true,
		       val, len, p1,
		       s, cl, sgn, 0, false);
}

/* Signed multiply THIS and C.  The result is twice the precision
   of the operands.  */
template <typename T>
inline wide_int_ro
wide_int_ro::smul_full (const T &c) const
{
  return mul_full (c, SIGNED);
}

/* Unsigned multiply THIS and C.  The result is twice the precision
   of the operands.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umul_full (const T &c) const
{
  return mul_full (c, UNSIGNED);
}

/* Multiply THIS and C and return the high part of that result.
   The signedness is specified with SGN.  The result is the same
   precision as the operands.  The mode is the same mode as the
   operands.  The signedness is specified with SGN.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mul_high (const T &c, signop sgn) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  return mul_internal (true, false,
		       val, len, p1,
		       s, cl, sgn, 0, false);
}

/* Negate THIS.  */
inline wide_int_ro
wide_int_ro::operator - () const
{
  wide_int_ro r;
  r = wide_int_ro (0) - *this;
  return r;
}

/* Negate THIS.  OVERFLOW is set true if the value cannot be negated,
   false otherwise.  */
inline wide_int_ro
wide_int_ro::neg (bool *overflow) const
{
  gcc_checking_assert (precision);

  *overflow = only_sign_bit_p ();

  return wide_int_ro (0) - *this;
}

/* Return THIS - C.  */
template <typename T>
inline wide_int_ro
wide_int_ro::operator - (const T &c) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] - s[0];
      if (p1 < HOST_BITS_PER_WIDE_INT)
	result.val[0] = sext_hwi (result.val[0], p1);
    }
  else
    result = sub_large (val, len, p1, s, cl, UNSIGNED, 0);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s - %s)\n", result, *this, s, cl, p2);
#endif
  return result;
}

/* Return THIS - C.  OVERFLOW is set based on the sign of the
   operation that is specified in SGN.  */
template <typename T>
inline wide_int_ro
wide_int_ro::sub (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, true, true);

  if (p1 <= HOST_BITS_PER_WIDE_INT)
    {
      result.len = 1;
      result.precision = p1;
      result.val[0] = val[0] - s[0];
      if (p1 < HOST_BITS_PER_WIDE_INT)
	result.val[0] = sext_hwi (result.val[0], p1);
      if (sgn == SIGNED)
	{
	  HOST_WIDE_INT x
	    = (((val[0] ^ s[0]) & (result.val[0] ^ val[0]))
	       >> (p1 - 1)) & 1;
	  *overflow = (x != 0);
	}
      else
	*overflow = ((unsigned HOST_WIDE_INT) result.val[0]
		     > (unsigned HOST_WIDE_INT) val[0]);
    }
  else
    result = sub_large (val, len, p1, s, cl, sgn, overflow);

#ifdef DEBUG_WIDE_INT
  debug_waav ("wide_int_ro:: %s = (%s - %s) O=%d\n",
	      result, val, len, p1, s, cl, p1, *overflow);
#endif
  return result;
}

/* Divide DIVISOR into THIS.  The result is the same size as the
   operands.  The sign is specified in SGN.  The output is truncated.
   If the pointer to OVERFLOW is not 0, OVERFLOW is set to true if
   the result overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::div_trunc (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  return divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			  &remainder, false, overflow);
}

/* Signed divide with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::sdiv_trunc (const T &c) const
{
  return div_trunc (c, SIGNED);
}

/* Unsigned divide with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::udiv_trunc (const T &c) const
{
  return div_trunc (c, UNSIGNED);
}

/* Divide DIVISOR into THIS.  The result is the same size as the operands.
   The sign is specified in SGN.  The output is floor truncated.  If the
   pointer to OVERFLOW is not 0, OVERFLOW is set to true if the result
   overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::div_floor (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  return divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			  &remainder, false, overflow);
}

/* Unsigned divide with floor truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::udiv_floor (const T &c) const
{
  return div_floor (c, UNSIGNED);
}

/* Signed divide with floor truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::sdiv_floor (const T &c) const
{
  return div_floor (c, SIGNED);
}

/* Divide DIVISOR into THIS.  The result is the same size as the operands.
   The sign is specified in SGN.  The output is ceil truncated.  If the
   pointer to OVERFLOW is not 0, OVERFLOW is set to true if the result
   overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::div_ceil (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      &remainder, true, overflow);

  if (!quotient.neg_p (sgn) && !remainder.zero_p ())
    return quotient + 1;
  return quotient;
}

/* Divide DIVISOR into THIS.  The result is the same size as the operands.
   The sign is specified in SGN.  The output is round truncated.  If the
   pointer to OVERFLOW is not 0, OVERFLOW is set to true if the result
   overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::div_round (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      &remainder, true, overflow);
  if (!remainder.zero_p ())
    {
      wide_int_ro divisor = wide_int_ro::from_array (s, cl, precision);
      if (sgn == SIGNED)
	{
	  wide_int_ro p_remainder
	    = remainder.neg_p () ? -remainder : remainder;
	  wide_int_ro p_divisor = divisor.neg_p () ? -divisor : divisor;
	  p_divisor = p_divisor.rshiftu_large (1);

	  if (p_divisor.gts_p (p_remainder))
	    {
	      if (quotient.neg_p ())
		return quotient - 1;
	      else
		return quotient + 1;
	    }
	}
      else
	{
	  wide_int_ro p_divisor = divisor.rshiftu_large (1);
	  if (p_divisor.gtu_p (remainder))
	    return quotient + 1;
	}
    }
  return quotient;
}

/* Divide DIVISOR into THIS producing both the quotient and remainder.
   The result is the same size as the operands.  The sign is specified
   in SGN.  The output is truncated.  */
template <typename T>
inline wide_int_ro
wide_int_ro::divmod_trunc (const T &c, wide_int_ro *remainder,
			   signop sgn) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  return divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			  remainder, true, 0);
}

/* Signed divide/mod with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::sdivmod_trunc (const T &c, wide_int_ro *mod) const
{
  return divmod_trunc (c, mod, SIGNED);
}

/* Unsigned divide/mod with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::udivmod_trunc (const T &c, wide_int_ro *mod) const
{
  return divmod_trunc (c, mod, UNSIGNED);
}

/* Divide DIVISOR into THIS.  The remainder is also produced in
   REMAINDER.  The result is the same size as the operands.
   The sign is specified in SGN.  The outputs is floor truncated.  */
template <typename T>
inline wide_int_ro
wide_int_ro::divmod_floor (const T &c, wide_int_ro *remainder,
			   signop sgn) const
{
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      remainder, true, 0);
  if (quotient.neg_p (sgn) && !(*remainder).zero_p ())
    {
      *remainder = *remainder + wide_int_ro::from_array (s, cl, precision);
      return quotient - 1;
    }
  return quotient;
}

/* Signed divide/mod with floor truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::sdivmod_floor (const T &c, wide_int_ro *mod) const
{
  return divmod_floor (c, mod, SIGNED);
}

/* Divide DIVISOR into THIS producing the remainder.  The result is
   the same size as the operands.  The sign is specified in SGN.  The
   output is adjusted to be compatible with truncating divide.  If the
   pointer to OVERFLOW is not 0, OVERFLOW is set to true if the result
   overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mod_trunc (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  divmod_internal (false, val, len, p1, s, cl, p2, sgn,
		   &remainder, true, overflow);
  return remainder;
}

/* Signed mod with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::smod_trunc (const T &c) const
{
  return mod_trunc (c, SIGNED);
}

/* Unsigned mod with truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umod_trunc (const T &c) const
{
  return mod_trunc (c, UNSIGNED);
}

/* Divide DIVISOR into THIS producing the remainder.  The result is
   the same size as the operands.  The sign is specified in SGN.  The
   output is adjusted to be compatible with floor divide.  OVERFLOW is
   set to true if the result overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mod_floor (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      &remainder, true, overflow);

  if (quotient.neg_p (sgn) && !remainder.zero_p ())
    return remainder + wide_int_ro::from_array (s, cl, precision);
  return remainder;
}

/* Unsigned mod with floor truncation of result.  */
template <typename T>
inline wide_int_ro
wide_int_ro::umod_floor (const T &c) const
{
  return mod_floor (c, UNSIGNED);
}

/* Divide DIVISOR into THIS producing the remainder.  The result is
   the same size as the operands.  The sign is specified in SGN.  The
   output is adjusted to be compatible with ceil divide.  If the
   pointer to OVERFLOW is not 0, OVERFLOW is set to true if the result
   overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mod_ceil (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      &remainder, true, overflow);

  if (!quotient.neg_p (sgn) && !remainder.zero_p ())
    return  remainder - wide_int_ro::from_array (s, cl, precision);
  return remainder;
}

/* Divide DIVISOR into THIS producing the remainder.  The result is
   the same size as the operands.  The sign is specified in SGN.  The
   output is adjusted to be compatible with rounding divide.  OVERFLOW
   is set to true if the result overflows, false otherwise.  */
template <typename T>
inline wide_int_ro
wide_int_ro::mod_round (const T &c, signop sgn, bool *overflow) const
{
  wide_int_ro remainder;
  wide_int_ro quotient;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  unsigned int p1, p2;

  p1 = precision;
  s = to_shwi1 (ws, &cl, &p2, c);
  check_precision (&p1, &p2, false, true);

  quotient = divmod_internal (true, val, len, p1, s, cl, p2, sgn,
			      &remainder, true, overflow);

  if (!remainder.zero_p ())
    {
      wide_int_ro divisor = wide_int_ro::from_array (s, cl, precision);
      if (sgn == SIGNED)
	{
	  wide_int_ro p_remainder = (remainder.neg_p ()
				     ? -remainder : remainder);
	  wide_int_ro p_divisor = divisor.neg_p () ? -divisor : divisor;
	  p_divisor = p_divisor.rshiftu_large (1);

	  if (p_divisor.gts_p (p_remainder))
	    {
	      if (quotient.neg_p ())
		return remainder + divisor;
	      else
		return remainder - divisor;
	    }
	}
      else
	{
	  wide_int_ro p_divisor = divisor.rshiftu_large (1);
	  if (p_divisor.gtu_p (remainder))
	    return remainder - divisor;
	}
    }
  return remainder;
}

/* Left shift THIS by C.  C must be non-negative.  BITSIZE is the
   width of *THIS used for truncating the shift amount.   */
template <typename T>
inline wide_int_ro
wide_int_ro::lshift (const T &c, unsigned int bitsize) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  HOST_WIDE_INT shift;

  s = to_shwi2 (ws, &cl, c);

  gcc_checking_assert (precision);

  shift = trunc_shift (s, bitsize);
  if (shift == -1)
    result = wide_int_ro::zero (precision);
  else if (shift == 0)
    result = *this;
  /* Handle the simple case quickly.   */
  else if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      result.precision = precision;
      result.len = 1;
      result.val[0] = val[0] << shift;
    }
  else
    result = lshift_large (shift, precision);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s << %s)\n", result, *this, s, cl, 0);
#endif
  return result;
}

/* Left shift THIS by C into an expanded value with RES_PREC precision.
   C must be non-negative.  This function is only available for the default
   wide-int form.  */
template <typename T>
inline wide_int_ro
wide_int_ro::lshift_widen (const T &c, unsigned int res_prec) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  HOST_WIDE_INT shift;

  s = to_shwi2 (ws, &cl, c);

  gcc_checking_assert (precision);
  gcc_checking_assert (res_prec);

  shift = s[0];

  gcc_checking_assert (shift >= 0);

  if (shift == 0 && res_prec == precision)
    result = *this;
  /* Handle the simple case quickly.   */
  else if (res_prec <= HOST_BITS_PER_WIDE_INT)
    {
      result.precision = res_prec;
      result.len = 1;
      result.val[0] = val[0] << shift;
    }
  else
    result = lshift_large (shift, res_prec);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s <<W %s)\n", result, *this, s, cl, 0);
#endif
  return result;
}

/* Rotate THIS left by C within PREC.  If PREC is 0, the precsion of
   THIS is used for PREC.  The result is the precision of THIS.  */
template <typename T>
inline wide_int_ro
wide_int_ro::lrotate (const T &c, unsigned int prec) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;

  s = to_shwi2 (ws, &cl, c);

  return lrotate ((unsigned HOST_WIDE_INT) s[0], prec);
}

/* Rotate THIS left by CNT within PREC.  If PREC is 0, the precsion
   of THIS is used for PREC.  CNT must be non-negative.  The result
   is the precision of the THIS.  */
inline wide_int_ro
wide_int_ro::lrotate (unsigned HOST_WIDE_INT cnt, unsigned int prec) const
{
  wide_int_ro left, right, result;

  gcc_checking_assert (precision);

  if (prec == 0)
    prec = precision;

  left = lshift (cnt);
  right = rshiftu (prec - cnt);

  if (prec != precision)
    {
      left = left.zforce_to_size (precision);
      right = right.zforce_to_size (precision);
    }
  result = left | right;

  return result;
}

/* Right shift THIS by C.  BITSIZE is the width of *THIS used for
   truncating the shift amount.  SGN indicates the sign.  C must be
   non-negative.  */
template <typename T>
inline wide_int_ro
wide_int_ro::rshift (const T &c, signop sgn, unsigned int bitsize) const
{
  if (sgn == UNSIGNED)
    return rshiftu (c, bitsize);
  else
    return rshifts (c, bitsize);
}

/* Unsigned right shift THIS by C.  C must be non-negative.  BITSIZE
   is width of *THIS used for truncating the shift amount. */
template <typename T>
inline wide_int_ro
wide_int_ro::rshiftu (const T &c, unsigned int bitsize) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  HOST_WIDE_INT shift;

  s = to_shwi2 (ws, &cl, c);
  gcc_checking_assert (precision);
  shift = trunc_shift (s, bitsize);

  if (shift == 0)
    result = *this;
  else if (shift == -1)
    result = wide_int_ro::zero (precision);
  else if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      /* Handle the simple case quickly.   */
      unsigned HOST_WIDE_INT x = val[0];

      result.precision = precision;
      result.len = 1;

      if (precision < HOST_BITS_PER_WIDE_INT)
	x = zext_hwi (x, precision);

      result.val[0] = x >> shift;
    }
  else
    result = rshiftu_large (shift);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s >>U %s)\n", result, *this, s, cl, 0);
#endif
  return result;
}

/* Signed right shift THIS by C.  C must be non-negative, BITSIZE is
   the width of *THIS used for truncating the shift amount.   */
template <typename T>
inline wide_int_ro
wide_int_ro::rshifts (const T &c, unsigned int bitsize) const
{
  wide_int_ro result;
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;
  HOST_WIDE_INT shift;

  s = to_shwi2 (ws, &cl, c);
  gcc_checking_assert (precision);
  shift = trunc_shift (s, bitsize);

  if (shift == 0)
    result = *this;
  else if (shift == -1)
    result = wide_int_ro::zero (precision);
  else if (precision < HOST_BITS_PER_WIDE_INT)
    {
      /* Handle the simple case quickly.   */
      HOST_WIDE_INT x = val[0];

      result.precision = precision;
      result.len = 1;
      x = x << (HOST_BITS_PER_WIDE_INT - precision);
      result.val[0] = x >> (shift + HOST_BITS_PER_WIDE_INT - precision);
    }
  else if (precision == HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT x = val[0];

      result.precision = precision;
      result.len = 1;
      result.val[0] = x >> shift;
    }
  else
    result = rshifts_large (shift);

#ifdef DEBUG_WIDE_INT
  debug_wwa ("wide_int_ro:: %s = (%s >>S %s)\n", result, *this, s, cl, 0);
#endif
  return result;
}

/* Rotate THIS right by C within PREC.  If PREC is 0, the precsion
   of THIS is used for PREC.  The result has the precision of THIS.  */
template <typename T>
inline wide_int_ro
wide_int_ro::rrotate (const T &c, unsigned int prec) const
{
  HOST_WIDE_INT ws[WIDE_INT_MAX_ELTS];
  const HOST_WIDE_INT *s;
  unsigned int cl;

  s = to_shwi2 (ws, &cl, c);
  return rrotate ((unsigned HOST_WIDE_INT) s[0], prec);
}

/* Rotate THIS left by CNT within PREC.  If PREC is 0, the precsion
   of THIS is used for PREC.  The result has the precision of THIS.
   CNT must be non-negative.  */
inline wide_int_ro
wide_int_ro::rrotate (unsigned HOST_WIDE_INT cnt, unsigned int prec) const
{
  wide_int_ro left, right, result;

  gcc_checking_assert (precision);

  if (prec == 0)
    prec = precision;

  left = lshift (prec - cnt);
  right = rshiftu (cnt);

  if (prec != precision)
    {
      left = left.zforce_to_size (precision);
      right = right.zforce_to_size (precision);
    }
  result = left | right;

  return result;
}

/* Truncate the value of the shift so that the value is within the
   BITSIZE. */
inline int
wide_int_ro::trunc_shift (const HOST_WIDE_INT *cnt, unsigned int bitsize) const
{
  gcc_checking_assert (cnt[0] >= 0);

  if (bitsize == 0)
    return cnt[0];
  else
    return cnt[0] & (bitsize - 1);
}

template <typename T>
inline bool
wide_int_ro::top_bit_set (T x)
{
  return (x >> (sizeof (x)*8 - 1)) != 0;
}

/* The following template and its overrides are used for the first
   and second operand of static binary comparison functions.
   These have been implemented so that pointer copying is done
   from the rep of the operands rather than actual data copying.
   This is safe even for garbage collected objects since the value
   is immediately throw away.

   This template matches all integers.  */
template <typename T>
inline const HOST_WIDE_INT *
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s, unsigned int *l, unsigned int *p,
		       const T &x)
{
  s[0] = x;
  if (signedp (x)
      || sizeof (T) < sizeof (HOST_WIDE_INT)
      || ! top_bit_set (x))
    *l = 1;
  else
    {
      s[1] = 0;
      *l = 2;
    }
  *p = 0;
  return s;
}

/* The following template and its overrides are used for the second
   operand of binary functions.  These have been implemented so that
   pointer copying is done from the rep of the second operand rather
   than actual data copying.  This is safe even for garbage collected
   objects since the value is immediately throw away.

   The next template matches all integers.  */
template <typename T>
inline const HOST_WIDE_INT *
wide_int_ro::to_shwi2 (HOST_WIDE_INT *s, unsigned int *l, const T &x)
{
  s[0] = x;
  if (signedp (x)
      || sizeof (T) < sizeof (HOST_WIDE_INT)
      || ! top_bit_set (x))
    *l = 1;
  else
    {
      s[1] = 0;
      *l = 2;
    }
  return s;
}

inline wide_int::wide_int () {}

inline wide_int::wide_int (const wide_int_ro &r)
{
  static_cast <wide_int_ro &> (*this) = r;
}

/* Convert an INTEGER_CST into a wide int.  */
inline wide_int::wide_int (const_tree tcst)
{
  *this = from_array (&TREE_INT_CST_ELT (tcst, 0),
		      TREE_INT_CST_NUNITS (tcst),
		      TYPE_PRECISION (TREE_TYPE (tcst)), false);
}

inline wide_int::wide_int (HOST_WIDE_INT op0)
{
  precision = 0;
  val[0] = op0;
  len = 1;
}

inline wide_int::wide_int (int op0)
{
  precision = 0;
  val[0] = op0;
  len = 1;
}

inline wide_int::wide_int (unsigned HOST_WIDE_INT op0)
{
  *this = wide_int_ro::from_uhwi (op0);
}

inline wide_int::wide_int (unsigned int op0)
{
  *this = wide_int_ro::from_uhwi (op0);
}

inline wide_int::wide_int (const rtx_mode_t &op0)
{
  *this = wide_int_ro::from_rtx (op0);
}

inline wide_int &
wide_int::operator = (const wide_int_ro &r)
{
  static_cast <wide_int_ro &> (*this) = r;
  return *this;
}

inline wide_int &
wide_int::operator = (const_tree tcst)
{
  *this = from_array (&TREE_INT_CST_ELT (tcst, 0),
		      TREE_INT_CST_NUNITS (tcst),
		      TYPE_PRECISION (TREE_TYPE (tcst)), false);
  return *this;
}

inline wide_int &
wide_int::operator = (HOST_WIDE_INT op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  return *this;
}

inline wide_int &
wide_int::operator = (int op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  return *this;
}

inline wide_int &
wide_int::operator = (unsigned HOST_WIDE_INT op0)
{
  static_cast <wide_int_ro &> (*this) = wide_int_ro (op0);
  return *this;
}

inline wide_int &
wide_int::operator = (unsigned int op0)
{
  static_cast <wide_int_ro &> (*this) = wide_int_ro (op0);
  return *this;
}

inline wide_int &
wide_int::operator = (const rtx_mode_t &op0)
{
  *this = wide_int_ro::from_rtx (op0);
  return *this;
}

inline wide_int &
wide_int::operator ++ ()
{
  *this += 1;
  return *this;
}

inline wide_int &
wide_int::operator -- ()
{
  *this -= 1;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator &= (const T &c)
{
  *this = *this & c;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator |= (const T &c)
{
  *this = *this | c;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator ^= (const T &c)
{
  *this = *this ^ c;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator += (const T &c)
{
  *this = *this + c;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator -= (const T &c)
{
  *this = *this - c;
  return *this;
}

template <typename T>
inline wide_int &
wide_int::operator *= (const T &c)
{
  *this = *this * c;
  return *this;
}

template <int bitsize>
class GTY(()) fixed_wide_int : public wide_int_ro
{
  friend class wide_int_ro;

protected:
  fixed_wide_int &operator = (const wide_int &);
  fixed_wide_int (const wide_int_ro);
  const HOST_WIDE_INT *get_val () const;

  using wide_int_ro::val;

public:
  using wide_int_ro::get_precision;
  using wide_int_ro::get_len;
  using wide_int_ro::to_short_addr;
  using wide_int_ro::fits_uhwi_p;
  using wide_int_ro::fits_shwi_p;
  using wide_int_ro::gtu_p;
  using wide_int_ro::gts_p;
  using wide_int_ro::geu_p;
  using wide_int_ro::ges_p;
  using wide_int_ro::to_shwi;
  using wide_int_ro::operator ==;
  using wide_int_ro::ltu_p;
  using wide_int_ro::lts_p;
  using wide_int_ro::leu_p;
  using wide_int_ro::les_p;
  using wide_int_ro::to_uhwi;
  using wide_int_ro::cmps;
  using wide_int_ro::neg_p;
  using wide_int_ro::cmpu;
  using wide_int_ro::umod_floor;
  using wide_int_ro::one_p;
  using wide_int_ro::zero_p;
  using wide_int_ro::multiple_of_p;
  using wide_int_ro::minus_one_p;
  using wide_int_ro::operator !=;
  using wide_int_ro::elt;
  using wide_int_ro::fits_to_tree_p;
  using wide_int_ro::from_uhwi;
  using wide_int_ro::ctz;
  using wide_int_ro::cmp;
  using wide_int_ro::minus_one;

  static fixed_wide_int from_wide_int (const wide_int &);
  static fixed_wide_int from_array (const HOST_WIDE_INT *, unsigned int,
				    bool = true);

  fixed_wide_int ();
  fixed_wide_int (const_tree);
  fixed_wide_int (HOST_WIDE_INT);
  fixed_wide_int (int);
  fixed_wide_int (unsigned HOST_WIDE_INT);
  fixed_wide_int (unsigned int);

  fixed_wide_int &operator ++ ();
  fixed_wide_int &operator -- ();

  bool multiple_of_p (const wide_int_ro &, signop, fixed_wide_int *) const;

  /* Conversion to and from GMP integer representations.  */
  void to_mpz (mpz_t, signop) const;
  static fixed_wide_int from_mpz (const_tree, mpz_t, bool);
  fixed_wide_int &operator = (const_tree);
  fixed_wide_int &operator = (HOST_WIDE_INT);
  fixed_wide_int &operator = (int);
  fixed_wide_int &operator = (unsigned HOST_WIDE_INT);
  fixed_wide_int &operator = (unsigned int);

  /* Extension, these do not change the precision.  */
  fixed_wide_int ext (unsigned int, signop) const;
  fixed_wide_int sext (unsigned int) const;
  fixed_wide_int zext (unsigned int) const;

  /* Masking and Insertion  */
  fixed_wide_int set_bit (unsigned int) const;
  static fixed_wide_int set_bit_in_zero (unsigned int);
  fixed_wide_int insert (const wide_int_ro &, unsigned int,
			 unsigned int) const;

  static fixed_wide_int mask (unsigned int, bool);
  static fixed_wide_int shifted_mask (unsigned int, unsigned int, bool);

  /* Logicals */

  template <typename T>
  fixed_wide_int operator & (const T &) const;
  fixed_wide_int operator & (const fixed_wide_int &) const;

  template <typename T>
  fixed_wide_int &operator &= (const T &);
  fixed_wide_int &operator &= (const fixed_wide_int &);

  template <typename T>
  fixed_wide_int and_not (const T &) const;

  fixed_wide_int operator ~ () const;

  template <typename T>
  fixed_wide_int operator | (const T &) const;
  fixed_wide_int operator | (const fixed_wide_int &) const;

  template <typename T>
  fixed_wide_int &operator |= (const T &);
  fixed_wide_int &operator |= (const fixed_wide_int &);

  template <typename T>
  fixed_wide_int or_not (const T &) const;

  template <typename T>
  fixed_wide_int operator ^ (const T &) const;
  fixed_wide_int operator ^ (const fixed_wide_int &) const;

  template <typename T>
  fixed_wide_int &operator ^= (const T &);
  fixed_wide_int &operator ^= (const fixed_wide_int &);

  /* Arithmetic operation functions, alpha sorted.  */

  template <typename T>
  fixed_wide_int operator + (const T &) const;
  fixed_wide_int operator + (const fixed_wide_int &c) const;

  template <typename T>
  fixed_wide_int &operator += (const T &);
  fixed_wide_int &operator += (const fixed_wide_int &c);

  template <typename T>
  fixed_wide_int add (const T &, signop, bool *) const;

  template <typename T>
  fixed_wide_int operator * (const T &) const;

  template <typename T>
  fixed_wide_int &operator *= (const T &);

  template <typename T>
  fixed_wide_int mul (const T &, signop, bool *) const;

  template <typename T>
  fixed_wide_int smul (const T &, bool *) const;

  template <typename T>
  fixed_wide_int umul (const T &, bool *) const;

  template <typename T>
  fixed_wide_int operator - (const T &) const;

  fixed_wide_int operator - () const;

  fixed_wide_int operator - (const fixed_wide_int &) const;

  template <typename T>
  fixed_wide_int &operator -= (const T &);
  fixed_wide_int &operator -= (const fixed_wide_int &);

  template <typename T>
  fixed_wide_int sub (const T &, signop, bool *) const;

  /* Division and mod.  These are the ones that are actually used, but
     there are a lot of them.  */

  template <typename T>
  fixed_wide_int div_floor (const T &, signop, bool * = 0) const;

  template <typename T>
  fixed_wide_int udiv_floor (const T &) const;

  template <typename T>
  fixed_wide_int sdiv_floor (const T &) const;

  template <typename T>
  fixed_wide_int div_ceil (const T &, signop, bool * = 0) const;

  template <typename T>
  fixed_wide_int div_round (const T &, signop, bool * = 0) const;

  template <typename T>
  fixed_wide_int div_trunc (const T &, signop, bool * = 0) const;

  template <typename T>
  fixed_wide_int sdiv_trunc (const T &) const;

  template <typename T>
  fixed_wide_int udiv_trunc (const T &) const;

  template <typename T>
  fixed_wide_int divmod_floor (const T &, fixed_wide_int *, signop) const;

  template <typename T>
  fixed_wide_int sdivmod_floor (const T &, fixed_wide_int *) const;

  /* Shifting rotating and extracting.  */

  template <typename T>
  fixed_wide_int lrotate (const T &, unsigned int) const;
  fixed_wide_int lrotate (unsigned HOST_WIDE_INT, unsigned int) const;

  template <typename T>
  fixed_wide_int lshift (const T &, unsigned int = 0) const;

  template <typename T>
  fixed_wide_int lshift_widen (const T &, unsigned int) const;

  template <typename T>
  fixed_wide_int rshift (const T &, signop, unsigned int = 0) const;

  template <typename T>
  fixed_wide_int rshiftu (const T &, unsigned int = 0) const;

  template <typename T>
  fixed_wide_int rshifts (const T &, unsigned int = 0) const;

  template <typename T>
  fixed_wide_int rrotate (const T &, unsigned int) const;
  fixed_wide_int rrotate (unsigned HOST_WIDE_INT, unsigned int) const;
};

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (const wide_int &w)
{
  static_cast <wide_int_ro &> (*this) = w;

  /* We only allow the same size in, as otherwise
     we would not know how to extend it.  */
  gcc_assert (precision == bitsize);

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (const wide_int_ro w)
  : wide_int_ro (w)
{
  /* We only allow the same size in, as otherwise
     we would not know how to extend it.  */
  gcc_assert (precision == bitsize);
}

template <int bitsize>
inline const HOST_WIDE_INT *
fixed_wide_int <bitsize>::get_val () const
{
  return val;
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::from_wide_int (const wide_int &w)
{
  if (w.neg_p ())
    return w.sforce_to_size (bitsize);
  return w.zforce_to_size (bitsize);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::from_array (const HOST_WIDE_INT* op0,
				      unsigned int len,
				      bool need_canon)
{
  return wide_int_ro::from_array (op0, len, bitsize, need_canon);
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int () {}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (const_tree t)
{
  *this = t;
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (HOST_WIDE_INT op0)
  : wide_int_ro (op0)
{
  precision = bitsize;
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (int op0) : wide_int_ro (op0)
{
  precision = bitsize;
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (unsigned HOST_WIDE_INT op0)
  : wide_int_ro (op0)
{
  precision = bitsize;
  if (neg_p ())
    static_cast <wide_int_ro &> (*this) = zext (HOST_BITS_PER_WIDE_INT);
}

template <int bitsize>
inline fixed_wide_int <bitsize>::fixed_wide_int (unsigned int op0)
  : wide_int_ro (op0)
{
  precision = bitsize;
  if (sizeof (int) == sizeof (HOST_WIDE_INT)
      && neg_p ())
    *this = zext (HOST_BITS_PER_WIDE_INT);
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator ++ ()
{
  *this += 1;
  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator -- ()
{
  *this -= 1;
  return *this;
}

template <int bitsize>
inline bool
fixed_wide_int <bitsize>::multiple_of_p (const wide_int_ro &factor,
					 signop sgn,
					 fixed_wide_int *multiple) const
{
  return wide_int_ro::multiple_of_p (factor, sgn, multiple);
}

template <int bitsize>
inline void
fixed_wide_int <bitsize>::to_mpz (mpz_t m, signop sgn) const
{
  wide_int_ro::to_mpz (m, sgn);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::from_mpz (const_tree t, mpz_t m, bool e)
{
  return wide_int_ro::from_mpz (t, m, e).force_to_size (bitsize,
							TYPE_SIGN (t));
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (const_tree t)
{
  tree type = TREE_TYPE (t);

  static_cast <wide_int_ro &> (*this)
    = wide_int_ro::from_array (&TREE_INT_CST_ELT (t, 0),
			       TREE_INT_CST_NUNITS (t),
			       TYPE_PRECISION (TREE_TYPE (t)), false);

  precision = bitsize;

  /* This is logically top_bit_set_p.  */
  if (TYPE_SIGN (type) == UNSIGNED && neg_p ())
    static_cast <wide_int_ro &> (*this) = zext (TYPE_PRECISION (type));

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (HOST_WIDE_INT op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  precision = bitsize;

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (int op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  precision = bitsize;

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (unsigned HOST_WIDE_INT op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  precision = bitsize;

  /* This is logically top_bit_set_p.  */
  if (neg_p ())
    static_cast <wide_int_ro &> (*this) = zext (HOST_BITS_PER_WIDE_INT);

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator = (unsigned int op0)
{
  static_cast <wide_int_ro &> (*this) = op0;
  precision = bitsize;

  if (sizeof (int) == sizeof (HOST_WIDE_INT)
      && neg_p ())
    *this = zext (HOST_BITS_PER_WIDE_INT);

  return *this;
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::ext (unsigned int offset, signop sgn) const
{
  return wide_int_ro::ext (offset, sgn);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::sext (unsigned int offset) const
{
  return wide_int_ro::sext (offset);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::zext (unsigned int offset) const
{
  return wide_int_ro::zext (offset);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::set_bit (unsigned int bitpos) const
{
  return wide_int_ro::set_bit (bitpos);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::set_bit_in_zero (unsigned int bitpos)
{
  return wide_int_ro::set_bit_in_zero (bitpos, bitsize);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::insert (const wide_int_ro &op0, unsigned int offset,
				  unsigned int width) const
{
  return wide_int_ro::insert (op0, offset, width);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::mask (unsigned int width, bool negate)
{
  return wide_int_ro::mask (width, negate, bitsize);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::shifted_mask (unsigned int start, unsigned int width,
					bool negate)
{
  return wide_int_ro::shifted_mask (start, width, negate, bitsize);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator & (const T &c) const
{
  return *this & fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator &= (const T &c)
{
  *this &= fixed_wide_int (c);
  return *this;
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::and_not (const T &c) const
{
  return wide_int_ro::and_not (fixed_wide_int (c));
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator ~ () const
{
  return ~static_cast <const wide_int_ro &> (*this);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator | (const T &c) const
{
  return *this | fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator |= (const T &c)
{
  *this |= fixed_wide_int (c);
  return *this;
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::or_not (const T &c) const
{
  return wide_int_ro::or_not (fixed_wide_int (c));
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator ^ (const T &c) const
{
  return *this ^ fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator ^= (const T &c)
{
  *this ^= fixed_wide_int (c);
  return *this;
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator + (const T &c) const
{
  return *this + fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator += (const T &c)
{
  *this += fixed_wide_int (c);
  return *this;
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::add (const T &c, signop sgn, bool *overflow) const
{
  return wide_int_ro::add (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator * (const T &c) const
{
  return static_cast <const wide_int_ro &> (*this) * fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator *= (const T &c)
{
  reinterpret_cast <wide_int &> (*this) *= c;
  return *this;
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::mul (const T &c, signop sgn, bool *overflow) const
{
  return wide_int_ro::mul (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::smul (const T &c, bool *overflow) const
{
  return wide_int_ro::smul (c, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::umul (const T &c, bool *overflow) const
{
  return wide_int_ro::umul (c, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator - (const T &c) const
{
  return *this - fixed_wide_int (c);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator - () const
{
  return - static_cast <const wide_int_ro &> (*this);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator -= (const T &c)
{
  return *this -= fixed_wide_int (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::sub (const T &c, signop sgn, bool *overflow) const
{
  return wide_int_ro::sub (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::div_floor (const T &c, signop sgn,
				     bool *overflow) const
{
  return wide_int_ro::div_floor (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::udiv_floor (const T &c) const
{
  return wide_int_ro::udiv_floor (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::sdiv_floor (const T &c) const
{
  return wide_int_ro::sdiv_floor (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::div_ceil (const T &c, signop sgn,
				    bool *overflow) const
{
  return wide_int_ro::div_ceil (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::div_round (const T &c, signop sgn,
				     bool *overflow) const
{
  return wide_int_ro::div_round (c, sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::div_trunc (const T &c, signop sgn,
				     bool *overflow) const
{
  return wide_int_ro::div_trunc (c,sgn, overflow);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::sdiv_trunc (const T &c) const
{
  return wide_int_ro::sdiv_trunc (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::udiv_trunc (const T &c) const
{
  return wide_int_ro::udiv_trunc (c);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::divmod_floor (const T &c, fixed_wide_int *mod,
					signop sgn) const
{
  return wide_int_ro::divmod_floor (c, mod, sgn);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::sdivmod_floor (const T &c, fixed_wide_int *mod) const
{
  return wide_int_ro::sdivmod_floor (c, mod);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::lrotate (const T &c, unsigned int prec) const
{
  return wide_int_ro::lrotate (c, prec);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::lrotate (unsigned HOST_WIDE_INT y,
				   unsigned int prec) const
{
  return wide_int_ro::lrotate (y, prec);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::lshift (const T &c, unsigned int bit_size) const
{
  return wide_int_ro::lshift (c, bit_size);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::lshift_widen (const T &c,
					unsigned int new_prec) const
{
  return wide_int_ro::lshift_widen (c, new_prec);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::rshift (const T &c, signop sgn, 
				  unsigned int bit_size) const
{
  return wide_int_ro::rshift (c, sgn, bit_size);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::rshiftu (const T &c, 
				   unsigned int bit_size) const
{
  return wide_int_ro::rshiftu (c, bit_size);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::rshifts (const T &c,
				   unsigned int bit_size) const
{
  return wide_int_ro::rshifts (c, bit_size);
}

template <int bitsize>
template <typename T>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::rrotate (const T &c, unsigned int prec) const
{
  return wide_int_ro::rrotate (c, prec);
}

template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::rrotate (unsigned HOST_WIDE_INT y,
				   unsigned int prec) const
{
  return wide_int_ro::lrotate (y, prec);
}

/* Logicals */
template <>
template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator & (const fixed_wide_int <bitsize> &c) const
{
  return static_cast <const wide_int_ro &> (*this) & c;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator &= (const fixed_wide_int <bitsize> &c)
{
  reinterpret_cast <wide_int &> (*this) &= (const wide_int_ro &) c;
  return *this;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator | (const fixed_wide_int <bitsize> &c) const
{
  return static_cast <const wide_int_ro &> (*this) | c;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator |= (const fixed_wide_int <bitsize> &c)
{
  reinterpret_cast <wide_int &> (*this) |= (const wide_int_ro &) c;
  return *this;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator ^ (const fixed_wide_int <bitsize> &c) const
{
  return static_cast <const wide_int_ro &> (*this) ^ c;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator ^= (const fixed_wide_int <bitsize> &c)
{
  reinterpret_cast <wide_int &> (*this) ^= (const wide_int_ro &) c;
  return *this;
}

/* Math operators */
template <>
template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator + (const fixed_wide_int <bitsize> &c) const
{
  return static_cast <const wide_int_ro &> (*this) + c;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator += (const fixed_wide_int <bitsize> &c)
{
  reinterpret_cast <wide_int &> (*this) += (const wide_int_ro &) c;
  return *this;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize>
fixed_wide_int <bitsize>::operator - (const fixed_wide_int <bitsize> &c) const
{
  return static_cast <const wide_int_ro &> (*this) - c;
}

template <>
template <int bitsize>
inline fixed_wide_int <bitsize> &
fixed_wide_int <bitsize>::operator -= (const fixed_wide_int <bitsize> &c)
{
  reinterpret_cast <wide_int &> (*this) -= (const wide_int_ro &) c;
  return *this;
}

/* A wide_int_ro that has a large enough precision to do any address math
   on the target.  */
typedef fixed_wide_int <ADDR_MAX_PRECISION> addr_wide_int;
/* A wide_int_ro that has a large enough precision to do any math on the
   target.  */
typedef fixed_wide_int <MAX_BITSIZE_MODE_ANY_INT> max_wide_int;

extern void gt_ggc_mx(max_wide_int*);
extern void gt_pch_nx(max_wide_int*,void (*)(void*, void*), void*);
extern void gt_pch_nx(max_wide_int*);

extern addr_wide_int mem_ref_offset (const_tree);

/* The wide-int overload templates.  */

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const wide_int_ro &y)
{
  *p = y.precision;
  *l = y.len;
  return y.val;
}

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const wide_int &y)
{
  *p = y.precision;
  *l = y.len;
  return y.val;
}


template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const fixed_wide_int <ADDR_MAX_PRECISION> &y)
{
  *p = y.get_precision ();
  *l = y.get_len ();
  return y.get_val ();
}

#if ADDR_MAX_PRECISION != MAX_BITSIZE_MODE_ANY_INT
template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const fixed_wide_int <MAX_BITSIZE_MODE_ANY_INT> &y)
{
  *p = y.get_precision ();
  *l = y.get_len ();
  return y.get_val ();
}
#endif

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi2 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, const wide_int &y)
{
  *l = y.len;
  return y.val;
}


/* The tree and const_tree overload templates.   */
template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const tree &tcst)
{
  tree type = TREE_TYPE (tcst);

  *p = TYPE_PRECISION (type);
  *l = TREE_INT_CST_NUNITS (tcst);
  return (const HOST_WIDE_INT*)&TREE_INT_CST_ELT (tcst, 0);
}

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi1 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, unsigned int *p,
		       const const_tree &tcst)
{
  tree type = TREE_TYPE (tcst);

  *p = TYPE_PRECISION (type);
  *l = TREE_INT_CST_NUNITS (tcst);
  return (const HOST_WIDE_INT*)&TREE_INT_CST_ELT (tcst, 0);
}

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi2 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, const tree &tcst)
{
  *l = TREE_INT_CST_NUNITS (tcst);
  return (const HOST_WIDE_INT*)&TREE_INT_CST_ELT (tcst, 0);
}

template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi2 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, const const_tree &tcst)
{
  *l = TREE_INT_CST_NUNITS (tcst);
  return (const HOST_WIDE_INT*)&TREE_INT_CST_ELT (tcst, 0);
}

/* Checking for the functions that require that at least one of the
   operands have a nonzero precision.  If both of them have a precision,
   then if CHECK_EQUAL is true, require that the precision be the same.  */

inline void
wide_int_ro::check_precision (unsigned int *p1, unsigned int *p2,
			      bool check_equal ATTRIBUTE_UNUSED,
			      bool check_zero ATTRIBUTE_UNUSED)
{
  gcc_checking_assert ((!check_zero) || *p1 != 0 || *p2 != 0);

  if (*p1 == 0)
    *p1 = *p2;

  if (*p2 == 0)
    *p2 = *p1;

  gcc_checking_assert ((!check_equal) || *p1 == *p2);
}

/* This is used to bundle an rtx and a mode together so that the pair
   can be used as the second operand of a wide int expression.  If we
   ever put modes into rtx integer constants, this should go away and
   then just pass an rtx in.  */
typedef std::pair <rtx, enum machine_mode> rtx_mode_t;

/* There should logically be an overload for rtl here, but it cannot
   be here because of circular include issues.  It is in rtl.h.  */
template <>
inline const HOST_WIDE_INT*
wide_int_ro::to_shwi2 (HOST_WIDE_INT *s ATTRIBUTE_UNUSED,
		       unsigned int *l, const rtx_mode_t &rp);

/* tree related routines.  */

extern tree wide_int_to_tree (tree type, const wide_int_ro &cst);
extern tree wide_int_to_infinite_tree (tree type, const wide_int_ro &cst,
				       unsigned int prec);
extern tree force_fit_type_wide (tree, const wide_int_ro &, int, bool);

/* real related routines.  */
extern wide_int real_to_integer (const REAL_VALUE_TYPE *, bool *, int);
extern void real_from_integer (REAL_VALUE_TYPE *, enum machine_mode,
			       wide_int, signop);
extern wide_int decimal_real_to_integer (const REAL_VALUE_TYPE *, bool *, int);


#endif /* GENERATOR FILE */

#endif /* WIDE_INT_H */
