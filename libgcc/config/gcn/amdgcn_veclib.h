/* Macro library used to help during conversion of scalar math functions to
   vectorized SIMD equivalents on AMD GCN.

   Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by Siemens.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef union {
  v2sf t_v2sf;
  v4sf t_v4sf;
  v8sf t_v8sf;
  v16sf t_v16sf;
  v32sf t_v32sf;
  v64sf t_v64sf;

  v2df t_v2df;
  v4df t_v4df;
  v8df t_v8df;
  v16df t_v16df;
  v32df t_v32df;
  v64df t_v64df;

  v64qi t_v64qi;
  v64hi t_v64hi;

  v2si t_v2si;
  v4si t_v4si;
  v8si t_v8si;
  v16si t_v16si;
  v32si t_v32si;
  v64si t_v64si;

  v64usi t_v64usi;

  v2di t_v2di;
  v4di t_v4di;
  v8di t_v8di;
  v16di t_v16di;
  v32di t_v32di;
  v64di t_v64di;
} vector_union;

/* Cast between vectors with a different number of elements, or type.  */

#define VGPR_CAST(to_t, from) \
({ \
  to_t __res; \
  __asm__ ("" : "=v"(__res) : "0"(from)); \
  __res; \
})

#define PACK_SI_PAIR(low, high) \
({ \
  v64udi __res; \
  asm ("v_mov_b32\t%L0, %1\n\t" \
       "v_mov_b32\t%H0, %2" \
       : "=&v"(__res) : "v0"(low), "v"(high), "e"(-1L)); \
  __res; \
 })

#define UNPACK_SI_LOW(to_t, pair) VGPR_CAST(to_t, pair)
#define UNPACK_SI_HIGH(to_t, pair) \
({ \
  to_t __res; \
  asm ("v_mov_b32\t%0, %H1" : "=v"(__res) : "v"(pair), "e"(-1L)); \
  __res; \
 })

#define PACK_DI_PAIR(low, high) \
({ \
  v64uti __res; \
  asm ("v_mov_b32\t%L0, %L1\n\t" \
       "v_mov_b32\t%H0, %H1\n\t" \
       "v_mov_b32\t%J0, %L2\n\t" \
       "v_mov_b32\t%K0, %H2" \
       : "=&v"(__res) : "v0"(low), "v"(high), "e"(-1L)); \
  __res; \
 })

#define UNPACK_DI_LOW(to_t, pair) VGPR_CAST(to_t, pair)
#define UNPACK_DI_HIGH(to_t, pair) \
({ \
  to_t __res; \
  asm ("v_mov_b32\t%L0, %J1\n\t" \
       "v_mov_b32\t%H0, %K1" : "=v"(__res) : "v"(pair), "e"(-1L)); \
  __res; \
 })

#define NO_COND __mask

/* Note - __mask is _not_ accounted for in VECTOR_MERGE!  */
#define VECTOR_MERGE(vec1, vec2, cond) \
({ \
  _Static_assert (__builtin_types_compatible_p (typeof (vec1), typeof (vec2))); \
  union { \
    typeof (vec1) val; \
    v64qi t_v64qi; \
    v64hi t_v64hi; \
    v64si t_v64si; \
    v64di t_v64di; \
  } __vec1, __vec2, __res; \
  __vec1.val = (vec1); \
  __vec2.val = (vec2); \
  __builtin_choose_expr ( \
        sizeof (vec1) == sizeof (v64si), \
        ({ \
          v64si __bitmask = __builtin_convertvector ((cond), v64si); \
          __res.t_v64si = (__vec1.t_v64si & __bitmask) \
                          | (__vec2.t_v64si & ~__bitmask); \
        }), \
	__builtin_choose_expr ( \
	  sizeof (vec1) == sizeof (v64hi), \
	  ({ \
	    v64hi __bitmask = __builtin_convertvector ((cond), v64hi); \
	    __res.t_v64hi = (__vec1.t_v64hi & __bitmask) \
			    | (__vec2.t_v64hi & ~__bitmask); \
	   }), \
	   __builtin_choose_expr ( \
	     sizeof (vec1) == sizeof (v64qi), \
	     ({ \
	     v64qi __bitmask = __builtin_convertvector ((cond), v64qi); \
	     __res.t_v64qi = (__vec1.t_v64qi & __bitmask) \
			      | (__vec2.t_v64qi & ~__bitmask); \
	     }), \
	     ({ \
	      v64di __bitmask = __builtin_convertvector ((cond), v64di); \
	      __res.t_v64di = (__vec1.t_v64di & __bitmask) \
			      | (__vec2.t_v64di & ~__bitmask); \
	      })))); \
  __res.val; \
})

#define VECTOR_COND_MOVE(var, val, cond) \
do { \
  _Static_assert (__builtin_types_compatible_p (typeof (var), typeof (val))); \
  __auto_type __cond = __builtin_convertvector ((cond), typeof (__mask)); \
  var = VECTOR_MERGE ((val), var, __cond & __mask); \
} while (0)

#define VECTOR_IF(cond, cond_var) \
{ \
  __auto_type cond_var = (cond); \
  __auto_type __inv_cond __attribute__((unused)) = ~cond_var; \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSEIF(cond, cond_var) \
  } \
  cond_var = __inv_cond & (cond); \
  __inv_cond &= ~(cond); \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSE(cond_var) \
  } \
  cond_var = __inv_cond; \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_IF2(cond, cond_var, prev_cond_var) \
{ \
  __auto_type cond_var = (cond) & __builtin_convertvector (prev_cond_var, typeof (cond)); \
  __auto_type __inv_cond __attribute__((unused)) = ~cond_var; \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSEIF2(cond, cond_var, prev_cond_var) \
  } \
  cond_var = (cond) & __inv_cond & __builtin_convertvector (prev_cond_var, typeof (cond)); \
  __inv_cond &= ~(cond); \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSE2(cond_var, prev_cond_var) \
  } \
  cond_var = __inv_cond & __builtin_convertvector (prev_cond_var, typeof (__inv_cond)); \
  if (!ALL_ZEROES_P (cond_var)) \
  {


#define VECTOR_ENDIF \
  } \
}

#define VECTOR_INIT_AUX(x, type) \
({ \
  typeof (x) __e = (x); \
  type __tmp = { \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e }; \
  __tmp; \
})

#define VECTOR_INIT(x) \
  (_Generic ((x), int: VECTOR_INIT_AUX ((x), v64si), \
                  unsigned: VECTOR_INIT_AUX ((x), v64usi), \
                  char: VECTOR_INIT_AUX ((x), v64qi), \
                  unsigned char: VECTOR_INIT_AUX ((x), v64uqi), \
                  short: VECTOR_INIT_AUX ((x), v64hi), \
                  unsigned short: VECTOR_INIT_AUX ((x), v64uhi), \
                  long: VECTOR_INIT_AUX ((x), v64di), \
                  unsigned long: VECTOR_INIT_AUX ((x), v64udi), \
                  float: VECTOR_INIT_AUX ((x), v64sf), \
                  double: VECTOR_INIT_AUX ((x), v64df)))


#if defined (__GCN3__) || defined (__GCN5__) \
    || defined (__CDNA1__) || defined (__CDNA2__) \
    || defined (__RDNA2__) || defined (__RDNA3__)
#define CDNA3_PLUS 0
#else
#define CDNA3_PLUS 1
#endif

#define VECTOR_INIT_MASK(COUNT) \
({ \
  MASKMODE __mask; \
  int count = (COUNT); \
  if (count == 64) \
    { \
      if (sizeof (MASKMODE) < 512 || CDNA3_PLUS) \
	asm ("v_mov%B0\t%0, -1" : "=v"(__mask) : "e"(-1L)); \
      else \
	asm ("v_mov_b32\t%L0, -1\n\t" \
	     "v_mov_b32\t%H0, -1" : "=v"(__mask) : "e"(-1L)); \
    } \
  else \
    { \
      long bitmask = (count == 64 ? -1 : (1<<count)-1); \
      if (sizeof (MASKMODE) < 512 || CDNA3_PLUS) \
        { \
	  asm ("v_mov%B0\t%0, 0" : "=v"(__mask) : "e"(-1L)); \
	  asm ("v_mov%B0\t%0, -1" : "+v"(__mask) : "e"(bitmask)); \
	} \
      else \
        { \
	  asm ("v_mov_b32\t%L0, 0\n\t" \
	       "v_mov_b32\t%H0, 0" : "=v"(__mask) : "e"(-1L)); \
	  asm ("v_mov_b32\t%L0, -1\n\t" \
	       "v_mov_b32\t%H0, -1" : "+v"(__mask) : "e"(bitmask)); \
	} \
    } \
  __mask; \
})

#define ALL_ZEROES_P(x) (COND_TO_BITMASK(x) == 0)

#define COND_TO_BITMASK(x) \
({ \
  long __tmp = 0; \
  __auto_type __x = __builtin_convertvector((x), typeof (__mask)) & __mask; \
  __builtin_choose_expr (sizeof (__mask) != 512, \
                         ({ asm ("v_cmp_ne_u32_e64 %0, %1, 0" \
                                 : "=Sg" (__tmp) \
                                 : "v" (__x)); }), \
                         ({ asm ("v_cmp_ne_u64_e64 %0, %1, 0" \
                                 : "=Sg" (__tmp) \
                                 : "v" (__x)); })); \
  __tmp; \
})

#define VECTOR_WHILE(cond, cond_var, prev_cond_var) \
{ \
  __auto_type cond_var = prev_cond_var; \
  for (;;) { \
    cond_var &= (cond); \
    if (ALL_ZEROES_P (cond_var)) \
      break;

#define VECTOR_ENDWHILE \
  } \
}

#define DEF_VARIANT(FUN, SUFFIX, OTYPE, TYPE, COUNT) \
v##COUNT##OTYPE \
FUN##v##COUNT##SUFFIX (v##COUNT##TYPE __arg1, v##COUNT##TYPE __arg2) \
{ \
  __auto_type __upsized_arg1 = VGPR_CAST (v64##TYPE, __arg1); \
  __auto_type __upsized_arg2 = VGPR_CAST (v64##TYPE, __arg2); \
  __auto_type __mask = VECTOR_INIT_MASK (COUNT); \
  __auto_type __result = FUN##v64##SUFFIX##_aux (__upsized_arg1, __upsized_arg2, __mask); \
  return VGPR_CAST (v##COUNT##OTYPE, __result); \
}

#define DEF_VARIANTS(FUN, SUFFIX, TYPE) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 2) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 4) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 8) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 16) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 32) \
  DEF_VARIANT (FUN, SUFFIX, TYPE, TYPE, 64)

#define DEF_VARIANTS_B(FUN, SUFFIX, OTYPE, TYPE) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 2) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 4) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 8) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 16) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 32) \
  DEF_VARIANT (FUN, SUFFIX, OTYPE, TYPE, 64)
