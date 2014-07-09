/* CPP Library - lexical analysis.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Broken out to separate file, Zack Weinberg, Mar 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"

enum spell_type
{
  SPELL_OPERATOR = 0,
  SPELL_IDENT,
  SPELL_LITERAL,
  SPELL_NONE
};

struct token_spelling
{
  enum spell_type category;
  const unsigned char *name;
};

static const unsigned char *const digraph_spellings[] =
{ UC"%:", UC"%:%:", UC"<:", UC":>", UC"<%", UC"%>" };

#define OP(e, s) { SPELL_OPERATOR, UC s  },
#define TK(e, s) { SPELL_ ## s,    UC #e },
static const struct token_spelling token_spellings[N_TTYPES] = { TTYPE_TABLE };
#undef OP
#undef TK

#define TOKEN_SPELL(token) (token_spellings[(token)->type].category)
#define TOKEN_NAME(token) (token_spellings[(token)->type].name)

static void add_line_note (cpp_buffer *, const uchar *, unsigned int);
static int skip_line_comment (cpp_reader *);
static void skip_whitespace (cpp_reader *, cppchar_t);
static void lex_string (cpp_reader *, cpp_token *, const uchar *);
static void save_comment (cpp_reader *, cpp_token *, const uchar *, cppchar_t);
static void store_comment (cpp_reader *, cpp_token *);
static void create_literal (cpp_reader *, cpp_token *, const uchar *,
			    unsigned int, enum cpp_ttype);
static bool warn_in_comment (cpp_reader *, _cpp_line_note *);
static int name_p (cpp_reader *, const cpp_string *);
static tokenrun *next_tokenrun (tokenrun *);

static _cpp_buff *new_buff (size_t);


/* Utility routine:

   Compares, the token TOKEN to the NUL-terminated string STRING.
   TOKEN must be a CPP_NAME.  Returns 1 for equal, 0 for unequal.  */
int
cpp_ideq (const cpp_token *token, const char *string)
{
  if (token->type != CPP_NAME)
    return 0;

  return !ustrcmp (NODE_NAME (token->val.node.node), (const uchar *) string);
}

/* Record a note TYPE at byte POS into the current cleaned logical
   line.  */
static void
add_line_note (cpp_buffer *buffer, const uchar *pos, unsigned int type)
{
  if (buffer->notes_used == buffer->notes_cap)
    {
      buffer->notes_cap = buffer->notes_cap * 2 + 200;
      buffer->notes = XRESIZEVEC (_cpp_line_note, buffer->notes,
                                  buffer->notes_cap);
    }

  buffer->notes[buffer->notes_used].pos = pos;
  buffer->notes[buffer->notes_used].type = type;
  buffer->notes_used++;
}


/* Fast path to find line special characters using optimized character
   scanning algorithms.  Anything complicated falls back to the slow
   path below.  Since this loop is very hot it's worth doing these kinds
   of optimizations.

   One of the paths through the ifdefs should provide 

     const uchar *search_line_fast (const uchar *s, const uchar *end);

   Between S and END, search for \n, \r, \\, ?.  Return a pointer to
   the found character.

   Note that the last character of the buffer is *always* a newline,
   as forced by _cpp_convert_input.  This fact can be used to avoid
   explicitly looking for the end of the buffer.  */

/* Configure gives us an ifdef test.  */
#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif

/* We'd like the largest integer that fits into a register.  There's nothing
   in <stdint.h> that gives us that.  For most hosts this is unsigned long,
   but MS decided on an LLP64 model.  Thankfully when building with GCC we
   can get the "real" word size.  */
#ifdef __GNUC__
typedef unsigned int word_type __attribute__((__mode__(__word__)));
#else
typedef unsigned long word_type;
#endif

/* The code below is only expecting sizes 4 or 8.
   Die at compile-time if this expectation is violated.  */
typedef char check_word_type_size
  [(sizeof(word_type) == 8 || sizeof(word_type) == 4) * 2 - 1];

/* Return X with the first N bytes forced to values that won't match one
   of the interesting characters.  Note that NUL is not interesting.  */

static inline word_type
acc_char_mask_misalign (word_type val, unsigned int n)
{
  word_type mask = -1;
  if (WORDS_BIGENDIAN)
    mask >>= n * 8;
  else
    mask <<= n * 8;
  return val & mask;
}

/* Return X replicated to all byte positions within WORD_TYPE.  */

static inline word_type
acc_char_replicate (uchar x)
{
  word_type ret;

  ret = (x << 24) | (x << 16) | (x << 8) | x;
  if (sizeof(word_type) == 8)
    ret = (ret << 16 << 16) | ret;
  return ret;
}

/* Return non-zero if some byte of VAL is (probably) C.  */

static inline word_type
acc_char_cmp (word_type val, word_type c)
{
#if defined(__GNUC__) && defined(__alpha__)
  /* We can get exact results using a compare-bytes instruction.  
     Get (val == c) via (0 >= (val ^ c)).  */
  return __builtin_alpha_cmpbge (0, val ^ c);
#else
  word_type magic = 0x7efefefeU;
  if (sizeof(word_type) == 8)
    magic = (magic << 16 << 16) | 0xfefefefeU;
  magic |= 1;

  val ^= c;
  return ((val + magic) ^ ~val) & ~magic;
#endif
}

/* Given the result of acc_char_cmp is non-zero, return the index of
   the found character.  If this was a false positive, return -1.  */

static inline int
acc_char_index (word_type cmp ATTRIBUTE_UNUSED,
		word_type val ATTRIBUTE_UNUSED)
{
#if defined(__GNUC__) && defined(__alpha__) && !WORDS_BIGENDIAN
  /* The cmpbge instruction sets *bits* of the result corresponding to
     matches in the bytes with no false positives.  */
  return __builtin_ctzl (cmp);
#else
  unsigned int i;

  /* ??? It would be nice to force unrolling here,
     and have all of these constants folded.  */
  for (i = 0; i < sizeof(word_type); ++i)
    {
      uchar c;
      if (WORDS_BIGENDIAN)
	c = (val >> (sizeof(word_type) - i - 1) * 8) & 0xff;
      else
	c = (val >> i * 8) & 0xff;

      if (c == '\n' || c == '\r' || c == '\\' || c == '?')
	return i;
    }

  return -1;
#endif
}

/* A version of the fast scanner using bit fiddling techniques.
 
   For 32-bit words, one would normally perform 16 comparisons and
   16 branches.  With this algorithm one performs 24 arithmetic
   operations and one branch.  Whether this is faster with a 32-bit
   word size is going to be somewhat system dependent.

   For 64-bit words, we eliminate twice the number of comparisons
   and branches without increasing the number of arithmetic operations.
   It's almost certainly going to be a win with 64-bit word size.  */

static const uchar * search_line_acc_char (const uchar *, const uchar *)
  ATTRIBUTE_UNUSED;

static const uchar *
search_line_acc_char (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  const word_type repl_nl = acc_char_replicate ('\n');
  const word_type repl_cr = acc_char_replicate ('\r');
  const word_type repl_bs = acc_char_replicate ('\\');
  const word_type repl_qm = acc_char_replicate ('?');

  unsigned int misalign;
  const word_type *p;
  word_type val, t;
  
  /* Align the buffer.  Mask out any bytes from before the beginning.  */
  p = (word_type *)((uintptr_t)s & -sizeof(word_type));
  val = *p;
  misalign = (uintptr_t)s & (sizeof(word_type) - 1);
  if (misalign)
    val = acc_char_mask_misalign (val, misalign);

  /* Main loop.  */
  while (1)
    {
      t  = acc_char_cmp (val, repl_nl);
      t |= acc_char_cmp (val, repl_cr);
      t |= acc_char_cmp (val, repl_bs);
      t |= acc_char_cmp (val, repl_qm);

      if (__builtin_expect (t != 0, 0))
	{
	  int i = acc_char_index (t, val);
	  if (i >= 0)
	    return (const uchar *)p + i;
	}

      val = *++p;
    }
}

/* Disable on Solaris 2/x86 until the following problems can be properly
   autoconfed:

   The Solaris 9 assembler cannot assemble SSE4.2 insns.
   Before Solaris 9 Update 6, SSE insns cannot be executed.
   The Solaris 10+ assembler tags objects with the instruction set
   extensions used, so SSE4.2 executables cannot run on machines that
   don't support that extension.  */

#if (GCC_VERSION >= 4005) && (defined(__i386__) || defined(__x86_64__)) && !(defined(__sun__) && defined(__svr4__))

/* Replicated character data to be shared between implementations.
   Recall that outside of a context with vector support we can't
   define compatible vector types, therefore these are all defined
   in terms of raw characters.  */
static const char repl_chars[4][16] __attribute__((aligned(16))) = {
  { '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n',
    '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n' },
  { '\r', '\r', '\r', '\r', '\r', '\r', '\r', '\r',
    '\r', '\r', '\r', '\r', '\r', '\r', '\r', '\r' },
  { '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\',
    '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\' },
  { '?', '?', '?', '?', '?', '?', '?', '?',
    '?', '?', '?', '?', '?', '?', '?', '?' },
};

/* A version of the fast scanner using MMX vectorized byte compare insns.

   This uses the PMOVMSKB instruction which was introduced with "MMX2",
   which was packaged into SSE1; it is also present in the AMD MMX
   extension.  Mark the function as using "sse" so that we emit a real
   "emms" instruction, rather than the 3dNOW "femms" instruction.  */

static const uchar *
#ifndef __SSE__
__attribute__((__target__("sse")))
#endif
search_line_mmx (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  typedef char v8qi __attribute__ ((__vector_size__ (8)));
  typedef int __m64 __attribute__ ((__vector_size__ (8), __may_alias__));

  const v8qi repl_nl = *(const v8qi *)repl_chars[0];
  const v8qi repl_cr = *(const v8qi *)repl_chars[1];
  const v8qi repl_bs = *(const v8qi *)repl_chars[2];
  const v8qi repl_qm = *(const v8qi *)repl_chars[3];

  unsigned int misalign, found, mask;
  const v8qi *p;
  v8qi data, t, c;

  /* Align the source pointer.  While MMX doesn't generate unaligned data
     faults, this allows us to safely scan to the end of the buffer without
     reading beyond the end of the last page.  */
  misalign = (uintptr_t)s & 7;
  p = (const v8qi *)((uintptr_t)s & -8);
  data = *p;

  /* Create a mask for the bytes that are valid within the first
     16-byte block.  The Idea here is that the AND with the mask
     within the loop is "free", since we need some AND or TEST
     insn in order to set the flags for the branch anyway.  */
  mask = -1u << misalign;

  /* Main loop processing 8 bytes at a time.  */
  goto start;
  do
    {
      data = *++p;
      mask = -1;

    start:
      t = __builtin_ia32_pcmpeqb(data, repl_nl);
      c = __builtin_ia32_pcmpeqb(data, repl_cr);
      t = (v8qi) __builtin_ia32_por ((__m64)t, (__m64)c);
      c = __builtin_ia32_pcmpeqb(data, repl_bs);
      t = (v8qi) __builtin_ia32_por ((__m64)t, (__m64)c);
      c = __builtin_ia32_pcmpeqb(data, repl_qm);
      t = (v8qi) __builtin_ia32_por ((__m64)t, (__m64)c);
      found = __builtin_ia32_pmovmskb (t);
      found &= mask;
    }
  while (!found);

  __builtin_ia32_emms ();

  /* FOUND contains 1 in bits for which we matched a relevant
     character.  Conversion to the byte index is trivial.  */
  found = __builtin_ctz(found);
  return (const uchar *)p + found;
}

/* A version of the fast scanner using SSE2 vectorized byte compare insns.  */

static const uchar *
#ifndef __SSE2__
__attribute__((__target__("sse2")))
#endif
search_line_sse2 (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  typedef char v16qi __attribute__ ((__vector_size__ (16)));

  const v16qi repl_nl = *(const v16qi *)repl_chars[0];
  const v16qi repl_cr = *(const v16qi *)repl_chars[1];
  const v16qi repl_bs = *(const v16qi *)repl_chars[2];
  const v16qi repl_qm = *(const v16qi *)repl_chars[3];

  unsigned int misalign, found, mask;
  const v16qi *p;
  v16qi data, t;

  /* Align the source pointer.  */
  misalign = (uintptr_t)s & 15;
  p = (const v16qi *)((uintptr_t)s & -16);
  data = *p;

  /* Create a mask for the bytes that are valid within the first
     16-byte block.  The Idea here is that the AND with the mask
     within the loop is "free", since we need some AND or TEST
     insn in order to set the flags for the branch anyway.  */
  mask = -1u << misalign;

  /* Main loop processing 16 bytes at a time.  */
  goto start;
  do
    {
      data = *++p;
      mask = -1;

    start:
      t  = __builtin_ia32_pcmpeqb128(data, repl_nl);
      t |= __builtin_ia32_pcmpeqb128(data, repl_cr);
      t |= __builtin_ia32_pcmpeqb128(data, repl_bs);
      t |= __builtin_ia32_pcmpeqb128(data, repl_qm);
      found = __builtin_ia32_pmovmskb128 (t);
      found &= mask;
    }
  while (!found);

  /* FOUND contains 1 in bits for which we matched a relevant
     character.  Conversion to the byte index is trivial.  */
  found = __builtin_ctz(found);
  return (const uchar *)p + found;
}

#ifdef HAVE_SSE4
/* A version of the fast scanner using SSE 4.2 vectorized string insns.  */

static const uchar *
#ifndef __SSE4_2__
__attribute__((__target__("sse4.2")))
#endif
search_line_sse42 (const uchar *s, const uchar *end)
{
  typedef char v16qi __attribute__ ((__vector_size__ (16)));
  static const v16qi search = { '\n', '\r', '?', '\\' };

  uintptr_t si = (uintptr_t)s;
  uintptr_t index;

  /* Check for unaligned input.  */
  if (si & 15)
    {
      v16qi sv;

      if (__builtin_expect (end - s < 16, 0)
	  && __builtin_expect ((si & 0xfff) > 0xff0, 0))
	{
	  /* There are less than 16 bytes left in the buffer, and less
	     than 16 bytes left on the page.  Reading 16 bytes at this
	     point might generate a spurious page fault.  Defer to the
	     SSE2 implementation, which already handles alignment.  */
	  return search_line_sse2 (s, end);
	}

      /* ??? The builtin doesn't understand that the PCMPESTRI read from
	 memory need not be aligned.  */
      sv = __builtin_ia32_loaddqu ((const char *) s);
      index = __builtin_ia32_pcmpestri128 (search, 4, sv, 16, 0);

      if (__builtin_expect (index < 16, 0))
	goto found;

      /* Advance the pointer to an aligned address.  We will re-scan a
	 few bytes, but we no longer need care for reading past the
	 end of a page, since we're guaranteed a match.  */
      s = (const uchar *)((si + 16) & -16);
    }

  /* Main loop, processing 16 bytes at a time.  By doing the whole loop
     in inline assembly, we can make proper use of the flags set.  */
  __asm (      "sub $16, %1\n"
	"	.balign 16\n"
	"0:	add $16, %1\n"
	"	%vpcmpestri $0, (%1), %2\n"
	"	jnc 0b"
	: "=&c"(index), "+r"(s)
	: "x"(search), "a"(4), "d"(16));

 found:
  return s + index;
}

#else
/* Work around out-dated assemblers without sse4 support.  */
#define search_line_sse42 search_line_sse2
#endif

/* Check the CPU capabilities.  */

#include "../gcc/config/i386/cpuid.h"

typedef const uchar * (*search_line_fast_type) (const uchar *, const uchar *);
static search_line_fast_type search_line_fast;

#define HAVE_init_vectorized_lexer 1
static inline void
init_vectorized_lexer (void)
{
  unsigned dummy, ecx = 0, edx = 0;
  search_line_fast_type impl = search_line_acc_char;
  int minimum = 0;

#if defined(__SSE4_2__)
  minimum = 3;
#elif defined(__SSE2__)
  minimum = 2;
#elif defined(__SSE__)
  minimum = 1;
#endif

  if (minimum == 3)
    impl = search_line_sse42;
  else if (__get_cpuid (1, &dummy, &dummy, &ecx, &edx) || minimum == 2)
    {
      if (minimum == 3 || (ecx & bit_SSE4_2))
        impl = search_line_sse42;
      else if (minimum == 2 || (edx & bit_SSE2))
	impl = search_line_sse2;
      else if (minimum == 1 || (edx & bit_SSE))
	impl = search_line_mmx;
    }
  else if (__get_cpuid (0x80000001, &dummy, &dummy, &dummy, &edx))
    {
      if (minimum == 1
	  || (edx & (bit_MMXEXT | bit_CMOV)) == (bit_MMXEXT | bit_CMOV))
	impl = search_line_mmx;
    }

  search_line_fast = impl;
}

#elif (GCC_VERSION >= 4005) && defined(__ALTIVEC__)

/* A vection of the fast scanner using AltiVec vectorized byte compares.  */
/* ??? Unfortunately, attribute(target("altivec")) is not yet supported,
   so we can't compile this function without -maltivec on the command line
   (or implied by some other switch).  */

static const uchar *
search_line_fast (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  typedef __attribute__((altivec(vector))) unsigned char vc;

  const vc repl_nl = {
    '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n', 
    '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n'
  };
  const vc repl_cr = {
    '\r', '\r', '\r', '\r', '\r', '\r', '\r', '\r', 
    '\r', '\r', '\r', '\r', '\r', '\r', '\r', '\r'
  };
  const vc repl_bs = {
    '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\', 
    '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\'
  };
  const vc repl_qm = {
    '?', '?', '?', '?', '?', '?', '?', '?', 
    '?', '?', '?', '?', '?', '?', '?', '?', 
  };
  const vc ones = {
    -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1,
  };
  const vc zero = { 0 };

  vc data, mask, t;

  /* Altivec loads automatically mask addresses with -16.  This lets us
     issue the first load as early as possible.  */
  data = __builtin_vec_ld(0, (const vc *)s);

  /* Discard bytes before the beginning of the buffer.  Do this by
     beginning with all ones and shifting in zeros according to the
     mis-alignment.  The LVSR instruction pulls the exact shift we
     want from the address.  */
#ifdef __BIG_ENDIAN__
  mask = __builtin_vec_lvsr(0, s);
  mask = __builtin_vec_perm(zero, ones, mask);
#else
  mask = __builtin_vec_lvsl(0, s);
  mask = __builtin_vec_perm(ones, zero, mask);
#endif
  data &= mask;

  /* While altivec loads mask addresses, we still need to align S so
     that the offset we compute at the end is correct.  */
  s = (const uchar *)((uintptr_t)s & -16);

  /* Main loop processing 16 bytes at a time.  */
  goto start;
  do
    {
      vc m_nl, m_cr, m_bs, m_qm;

      s += 16;
      data = __builtin_vec_ld(0, (const vc *)s);

    start:
      m_nl = (vc) __builtin_vec_cmpeq(data, repl_nl);
      m_cr = (vc) __builtin_vec_cmpeq(data, repl_cr);
      m_bs = (vc) __builtin_vec_cmpeq(data, repl_bs);
      m_qm = (vc) __builtin_vec_cmpeq(data, repl_qm);
      t = (m_nl | m_cr) | (m_bs | m_qm);

      /* T now contains 0xff in bytes for which we matched one of the relevant
	 characters.  We want to exit the loop if any byte in T is non-zero.
	 Below is the expansion of vec_any_ne(t, zero).  */
    }
  while (!__builtin_vec_vcmpeq_p(/*__CR6_LT_REV*/3, t, zero));

  {
#define N  (sizeof(vc) / sizeof(long))

    union {
      vc v;
      /* Statically assert that N is 2 or 4.  */
      unsigned long l[(N == 2 || N == 4) ? N : -1];
    } u;
    unsigned long l, i = 0;

    u.v = t;

    /* Find the first word of T that is non-zero.  */
    switch (N)
      {
      case 4:
	l = u.l[i++];
	if (l != 0)
	  break;
	s += sizeof(unsigned long);
	l = u.l[i++];
	if (l != 0)
	  break;
	s += sizeof(unsigned long);
      case 2:
	l = u.l[i++];
	if (l != 0)
	  break;
	s += sizeof(unsigned long);
	l = u.l[i];
      }

    /* L now contains 0xff in bytes for which we matched one of the
       relevant characters.  We can find the byte index by finding
       its bit index and dividing by 8.  */
#ifdef __BIG_ENDIAN__
    l = __builtin_clzl(l) >> 3;
#else
    l = __builtin_ctzl(l) >> 3;
#endif
    return s + l;

#undef N
  }
}

#elif defined (__ARM_NEON__)
#include "arm_neon.h"

static const uchar *
search_line_fast (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  const uint8x16_t repl_nl = vdupq_n_u8 ('\n');
  const uint8x16_t repl_cr = vdupq_n_u8 ('\r');
  const uint8x16_t repl_bs = vdupq_n_u8 ('\\');
  const uint8x16_t repl_qm = vdupq_n_u8 ('?');
  const uint8x16_t xmask = (uint8x16_t) vdupq_n_u64 (0x8040201008040201ULL);

  unsigned int misalign, found, mask;
  const uint8_t *p;
  uint8x16_t data;

  /* Align the source pointer.  */
  misalign = (uintptr_t)s & 15;
  p = (const uint8_t *)((uintptr_t)s & -16);
  data = vld1q_u8 (p);

  /* Create a mask for the bytes that are valid within the first
     16-byte block.  The Idea here is that the AND with the mask
     within the loop is "free", since we need some AND or TEST
     insn in order to set the flags for the branch anyway.  */
  mask = (-1u << misalign) & 0xffff;

  /* Main loop, processing 16 bytes at a time.  */
  goto start;

  do
    {
      uint8x8_t l;
      uint16x4_t m;
      uint32x2_t n;
      uint8x16_t t, u, v, w;

      p += 16;
      data = vld1q_u8 (p);
      mask = 0xffff;

    start:
      t = vceqq_u8 (data, repl_nl);
      u = vceqq_u8 (data, repl_cr);
      v = vorrq_u8 (t, vceqq_u8 (data, repl_bs));
      w = vorrq_u8 (u, vceqq_u8 (data, repl_qm));
      t = vandq_u8 (vorrq_u8 (v, w), xmask);
      l = vpadd_u8 (vget_low_u8 (t), vget_high_u8 (t));
      m = vpaddl_u8 (l);
      n = vpaddl_u16 (m);
      
      found = vget_lane_u32 ((uint32x2_t) vorr_u64 ((uint64x1_t) n, 
	      vshr_n_u64 ((uint64x1_t) n, 24)), 0);
      found &= mask;
    }
  while (!found);

  /* FOUND contains 1 in bits for which we matched a relevant
     character.  Conversion to the byte index is trivial.  */
  found = __builtin_ctz (found);
  return (const uchar *)p + found;
}

#else

/* We only have one accellerated alternative.  Use a direct call so that
   we encourage inlining.  */

#define search_line_fast  search_line_acc_char

#endif

/* Initialize the lexer if needed.  */

void
_cpp_init_lexer (void)
{
#ifdef HAVE_init_vectorized_lexer
  init_vectorized_lexer ();
#endif
}

/* Returns with a logical line that contains no escaped newlines or
   trigraphs.  This is a time-critical inner loop.  */
void
_cpp_clean_line (cpp_reader *pfile)
{
  cpp_buffer *buffer;
  const uchar *s;
  uchar c, *d, *p;

  buffer = pfile->buffer;
  buffer->cur_note = buffer->notes_used = 0;
  buffer->cur = buffer->line_base = buffer->next_line;
  buffer->need_line = false;
  s = buffer->next_line;

  if (!buffer->from_stage3)
    {
      const uchar *pbackslash = NULL;

      /* Fast path.  This is the common case of an un-escaped line with
	 no trigraphs.  The primary win here is by not writing any
	 data back to memory until we have to.  */
      while (1)
	{
	  /* Perform an optimized search for \n, \r, \\, ?.  */
	  s = search_line_fast (s, buffer->rlimit);

	  c = *s;
	  if (c == '\\')
	    {
	      /* Record the location of the backslash and continue.  */
	      pbackslash = s++;
	    }
	  else if (__builtin_expect (c == '?', 0))
	    {
	      if (__builtin_expect (s[1] == '?', false)
		   && _cpp_trigraph_map[s[2]])
		{
		  /* Have a trigraph.  We may or may not have to convert
		     it.  Add a line note regardless, for -Wtrigraphs.  */
		  add_line_note (buffer, s, s[2]);
		  if (CPP_OPTION (pfile, trigraphs))
		    {
		      /* We do, and that means we have to switch to the
		         slow path.  */
		      d = (uchar *) s;
		      *d = _cpp_trigraph_map[s[2]];
		      s += 2;
		      goto slow_path;
		    }
		}
	      /* Not a trigraph.  Continue on fast-path.  */
	      s++;
	    }
	  else
	    break;
	}

      /* This must be \r or \n.  We're either done, or we'll be forced
	 to write back to the buffer and continue on the slow path.  */
      d = (uchar *) s;

      if (__builtin_expect (s == buffer->rlimit, false))
	goto done;

      /* DOS line ending? */
      if (__builtin_expect (c == '\r', false) && s[1] == '\n')
	{
	  s++;
	  if (s == buffer->rlimit)
	    goto done;
	}

      if (__builtin_expect (pbackslash == NULL, true))
	goto done;

      /* Check for escaped newline.  */
      p = d;
      while (is_nvspace (p[-1]))
	p--;
      if (p - 1 != pbackslash)
	goto done;

      /* Have an escaped newline; process it and proceed to
	 the slow path.  */
      add_line_note (buffer, p - 1, p != d ? ' ' : '\\');
      d = p - 2;
      buffer->next_line = p - 1;

    slow_path:
      while (1)
	{
	  c = *++s;
	  *++d = c;

	  if (c == '\n' || c == '\r')
	    {
	      /* Handle DOS line endings.  */
	      if (c == '\r' && s != buffer->rlimit && s[1] == '\n')
		s++;
	      if (s == buffer->rlimit)
		break;

	      /* Escaped?  */
	      p = d;
	      while (p != buffer->next_line && is_nvspace (p[-1]))
		p--;
	      if (p == buffer->next_line || p[-1] != '\\')
		break;

	      add_line_note (buffer, p - 1, p != d ? ' ': '\\');
	      d = p - 2;
	      buffer->next_line = p - 1;
	    }
	  else if (c == '?' && s[1] == '?' && _cpp_trigraph_map[s[2]])
	    {
	      /* Add a note regardless, for the benefit of -Wtrigraphs.  */
	      add_line_note (buffer, d, s[2]);
	      if (CPP_OPTION (pfile, trigraphs))
		{
		  *d = _cpp_trigraph_map[s[2]];
		  s += 2;
		}
	    }
	}
    }
  else
    {
      while (*s != '\n' && *s != '\r')
	s++;
      d = (uchar *) s;

      /* Handle DOS line endings.  */
      if (*s == '\r' && s != buffer->rlimit && s[1] == '\n')
	s++;
    }

 done:
  *d = '\n';
  /* A sentinel note that should never be processed.  */
  add_line_note (buffer, d + 1, '\n');
  buffer->next_line = s + 1;
}

/* Return true if the trigraph indicated by NOTE should be warned
   about in a comment.  */
static bool
warn_in_comment (cpp_reader *pfile, _cpp_line_note *note)
{
  const uchar *p;

  /* Within comments we don't warn about trigraphs, unless the
     trigraph forms an escaped newline, as that may change
     behavior.  */
  if (note->type != '/')
    return false;

  /* If -trigraphs, then this was an escaped newline iff the next note
     is coincident.  */
  if (CPP_OPTION (pfile, trigraphs))
    return note[1].pos == note->pos;

  /* Otherwise, see if this forms an escaped newline.  */
  p = note->pos + 3;
  while (is_nvspace (*p))
    p++;

  /* There might have been escaped newlines between the trigraph and the
     newline we found.  Hence the position test.  */
  return (*p == '\n' && p < note[1].pos);
}

/* Process the notes created by add_line_note as far as the current
   location.  */
void
_cpp_process_line_notes (cpp_reader *pfile, int in_comment)
{
  cpp_buffer *buffer = pfile->buffer;

  for (;;)
    {
      _cpp_line_note *note = &buffer->notes[buffer->cur_note];
      unsigned int col;

      if (note->pos > buffer->cur)
	break;

      buffer->cur_note++;
      col = CPP_BUF_COLUMN (buffer, note->pos + 1);

      if (note->type == '\\' || note->type == ' ')
	{
	  if (note->type == ' ' && !in_comment)
	    cpp_error_with_line (pfile, CPP_DL_WARNING, pfile->line_table->highest_line, col,
				 "backslash and newline separated by space");

	  if (buffer->next_line > buffer->rlimit)
	    {
	      cpp_error_with_line (pfile, CPP_DL_PEDWARN, pfile->line_table->highest_line, col,
				   "backslash-newline at end of file");
	      /* Prevent "no newline at end of file" warning.  */
	      buffer->next_line = buffer->rlimit;
	    }

	  buffer->line_base = note->pos;
	  CPP_INCREMENT_LINE (pfile, 0);
	}
      else if (_cpp_trigraph_map[note->type])
	{
	  if (CPP_OPTION (pfile, warn_trigraphs)
	      && (!in_comment || warn_in_comment (pfile, note)))
	    {
	      if (CPP_OPTION (pfile, trigraphs))
		cpp_warning_with_line (pfile, CPP_W_TRIGRAPHS,
                                       pfile->line_table->highest_line, col,
				       "trigraph ??%c converted to %c",
				       note->type,
				       (int) _cpp_trigraph_map[note->type]);
	      else
		{
		  cpp_warning_with_line 
		    (pfile, CPP_W_TRIGRAPHS,
                     pfile->line_table->highest_line, col,
		     "trigraph ??%c ignored, use -trigraphs to enable",
		     note->type);
		}
	    }
	}
      else if (note->type == 0)
	/* Already processed in lex_raw_string.  */;
      else
	abort ();
    }
}

/* Skip a C-style block comment.  We find the end of the comment by
   seeing if an asterisk is before every '/' we encounter.  Returns
   nonzero if comment terminated by EOF, zero otherwise.

   Buffer->cur points to the initial asterisk of the comment.  */
bool
_cpp_skip_block_comment (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  const uchar *cur = buffer->cur;
  uchar c;

  cur++;
  if (*cur == '/')
    cur++;

  for (;;)
    {
      /* People like decorating comments with '*', so check for '/'
	 instead for efficiency.  */
      c = *cur++;

      if (c == '/')
	{
	  if (cur[-2] == '*')
	    break;

	  /* Warn about potential nested comments, but not if the '/'
	     comes immediately before the true comment delimiter.
	     Don't bother to get it right across escaped newlines.  */
	  if (CPP_OPTION (pfile, warn_comments)
	      && cur[0] == '*' && cur[1] != '/')
	    {
	      buffer->cur = cur;
	      cpp_warning_with_line (pfile, CPP_W_COMMENTS,
				     pfile->line_table->highest_line,
				     CPP_BUF_COL (buffer),
				     "\"/*\" within comment");
	    }
	}
      else if (c == '\n')
	{
	  unsigned int cols;
	  buffer->cur = cur - 1;
	  _cpp_process_line_notes (pfile, true);
	  if (buffer->next_line >= buffer->rlimit)
	    return true;
	  _cpp_clean_line (pfile);

	  cols = buffer->next_line - buffer->line_base;
	  CPP_INCREMENT_LINE (pfile, cols);

	  cur = buffer->cur;
	}
    }

  buffer->cur = cur;
  _cpp_process_line_notes (pfile, true);
  return false;
}

/* Skip a C++ line comment, leaving buffer->cur pointing to the
   terminating newline.  Handles escaped newlines.  Returns nonzero
   if a multiline comment.  */
static int
skip_line_comment (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  source_location orig_line = pfile->line_table->highest_line;

  while (*buffer->cur != '\n')
    buffer->cur++;

  _cpp_process_line_notes (pfile, true);
  return orig_line != pfile->line_table->highest_line;
}

/* Skips whitespace, saving the next non-whitespace character.  */
static void
skip_whitespace (cpp_reader *pfile, cppchar_t c)
{
  cpp_buffer *buffer = pfile->buffer;
  bool saw_NUL = false;

  do
    {
      /* Horizontal space always OK.  */
      if (c == ' ' || c == '\t')
	;
      /* Just \f \v or \0 left.  */
      else if (c == '\0')
	saw_NUL = true;
      else if (pfile->state.in_directive && CPP_PEDANTIC (pfile))
	cpp_error_with_line (pfile, CPP_DL_PEDWARN, pfile->line_table->highest_line,
			     CPP_BUF_COL (buffer),
			     "%s in preprocessing directive",
			     c == '\f' ? "form feed" : "vertical tab");

      c = *buffer->cur++;
    }
  /* We only want non-vertical space, i.e. ' ' \t \f \v \0.  */
  while (is_nvspace (c));

  if (saw_NUL)
    cpp_error (pfile, CPP_DL_WARNING, "null character(s) ignored");

  buffer->cur--;
}

/* See if the characters of a number token are valid in a name (no
   '.', '+' or '-').  */
static int
name_p (cpp_reader *pfile, const cpp_string *string)
{
  unsigned int i;

  for (i = 0; i < string->len; i++)
    if (!is_idchar (string->text[i]))
      return 0;

  return 1;
}

/* After parsing an identifier or other sequence, produce a warning about
   sequences not in NFC/NFKC.  */
static void
warn_about_normalization (cpp_reader *pfile, 
			  const cpp_token *token,
			  const struct normalize_state *s)
{
  if (CPP_OPTION (pfile, warn_normalize) < NORMALIZE_STATE_RESULT (s)
      && !pfile->state.skipping)
    {
      /* Make sure that the token is printed using UCNs, even
	 if we'd otherwise happily print UTF-8.  */
      unsigned char *buf = XNEWVEC (unsigned char, cpp_token_len (token));
      size_t sz;

      sz = cpp_spell_token (pfile, token, buf, false) - buf;
      if (NORMALIZE_STATE_RESULT (s) == normalized_C)
	cpp_warning_with_line (pfile, CPP_W_NORMALIZE, token->src_loc, 0,
			       "`%.*s' is not in NFKC", (int) sz, buf);
      else
	cpp_warning_with_line (pfile, CPP_W_NORMALIZE, token->src_loc, 0,
			       "`%.*s' is not in NFC", (int) sz, buf);
      free (buf);
    }
}

/* Returns TRUE if the sequence starting at buffer->cur is invalid in
   an identifier.  FIRST is TRUE if this starts an identifier.  */
static bool
forms_identifier_p (cpp_reader *pfile, int first,
		    struct normalize_state *state)
{
  cpp_buffer *buffer = pfile->buffer;

  if (*buffer->cur == '$')
    {
      if (!CPP_OPTION (pfile, dollars_in_ident))
	return false;

      buffer->cur++;
      if (CPP_OPTION (pfile, warn_dollars) && !pfile->state.skipping)
	{
	  CPP_OPTION (pfile, warn_dollars) = 0;
	  cpp_error (pfile, CPP_DL_PEDWARN, "'$' in identifier or number");
	}

      return true;
    }

  /* Is this a syntactically valid UCN?  */
  if (CPP_OPTION (pfile, extended_identifiers)
      && *buffer->cur == '\\'
      && (buffer->cur[1] == 'u' || buffer->cur[1] == 'U'))
    {
      buffer->cur += 2;
      if (_cpp_valid_ucn (pfile, &buffer->cur, buffer->rlimit, 1 + !first,
			  state))
	return true;
      buffer->cur -= 2;
    }

  return false;
}

/* Helper function to get the cpp_hashnode of the identifier BASE.  */
static cpp_hashnode *
lex_identifier_intern (cpp_reader *pfile, const uchar *base)
{
  cpp_hashnode *result;
  const uchar *cur;
  unsigned int len;
  unsigned int hash = HT_HASHSTEP (0, *base);

  cur = base + 1;
  while (ISIDNUM (*cur))
    {
      hash = HT_HASHSTEP (hash, *cur);
      cur++;
    }
  len = cur - base;
  hash = HT_HASHFINISH (hash, len);
  result = CPP_HASHNODE (ht_lookup_with_hash (pfile->hash_table,
					      base, len, hash, HT_ALLOC));

  /* Rarely, identifiers require diagnostics when lexed.  */
  if (__builtin_expect ((result->flags & NODE_DIAGNOSTIC)
			&& !pfile->state.skipping, 0))
    {
      /* It is allowed to poison the same identifier twice.  */
      if ((result->flags & NODE_POISONED) && !pfile->state.poisoned_ok)
	cpp_error (pfile, CPP_DL_ERROR, "attempt to use poisoned \"%s\"",
		   NODE_NAME (result));

      /* Constraint 6.10.3.5: __VA_ARGS__ should only appear in the
	 replacement list of a variadic macro.  */
      if (result == pfile->spec_nodes.n__VA_ARGS__
	  && !pfile->state.va_args_ok)
	cpp_error (pfile, CPP_DL_PEDWARN,
		   "__VA_ARGS__ can only appear in the expansion"
		   " of a C99 variadic macro");

      /* For -Wc++-compat, warn about use of C++ named operators.  */
      if (result->flags & NODE_WARN_OPERATOR)
	cpp_warning (pfile, CPP_W_CXX_OPERATOR_NAMES,
		     "identifier \"%s\" is a special operator name in C++",
		     NODE_NAME (result));
    }

  return result;
}

/* Get the cpp_hashnode of an identifier specified by NAME in
   the current cpp_reader object.  If none is found, NULL is returned.  */
cpp_hashnode *
_cpp_lex_identifier (cpp_reader *pfile, const char *name)
{
  cpp_hashnode *result;
  result = lex_identifier_intern (pfile, (uchar *) name);
  return result;
}

/* Lex an identifier starting at BUFFER->CUR - 1.  */
static cpp_hashnode *
lex_identifier (cpp_reader *pfile, const uchar *base, bool starts_ucn,
		struct normalize_state *nst)
{
  cpp_hashnode *result;
  const uchar *cur;
  unsigned int len;
  unsigned int hash = HT_HASHSTEP (0, *base);

  cur = pfile->buffer->cur;
  if (! starts_ucn)
    {
      while (ISIDNUM (*cur))
	{
	  hash = HT_HASHSTEP (hash, *cur);
	  cur++;
	}
      NORMALIZE_STATE_UPDATE_IDNUM (nst, *(cur - 1));
    }
  pfile->buffer->cur = cur;
  if (starts_ucn || forms_identifier_p (pfile, false, nst))
    {
      /* Slower version for identifiers containing UCNs (or $).  */
      do {
	while (ISIDNUM (*pfile->buffer->cur))
	  {
	    NORMALIZE_STATE_UPDATE_IDNUM (nst, *pfile->buffer->cur);
	    pfile->buffer->cur++;
	  }
      } while (forms_identifier_p (pfile, false, nst));
      result = _cpp_interpret_identifier (pfile, base,
					  pfile->buffer->cur - base);
    }
  else
    {
      len = cur - base;
      hash = HT_HASHFINISH (hash, len);

      result = CPP_HASHNODE (ht_lookup_with_hash (pfile->hash_table,
						  base, len, hash, HT_ALLOC));
    }

  /* Rarely, identifiers require diagnostics when lexed.  */
  if (__builtin_expect ((result->flags & NODE_DIAGNOSTIC)
			&& !pfile->state.skipping, 0))
    {
      /* It is allowed to poison the same identifier twice.  */
      if ((result->flags & NODE_POISONED) && !pfile->state.poisoned_ok)
	cpp_error (pfile, CPP_DL_ERROR, "attempt to use poisoned \"%s\"",
		   NODE_NAME (result));

      /* Constraint 6.10.3.5: __VA_ARGS__ should only appear in the
	 replacement list of a variadic macro.  */
      if (result == pfile->spec_nodes.n__VA_ARGS__
	  && !pfile->state.va_args_ok)
	cpp_error (pfile, CPP_DL_PEDWARN,
		   "__VA_ARGS__ can only appear in the expansion"
		   " of a C99 variadic macro");

      /* For -Wc++-compat, warn about use of C++ named operators.  */
      if (result->flags & NODE_WARN_OPERATOR)
	cpp_warning (pfile, CPP_W_CXX_OPERATOR_NAMES,
		     "identifier \"%s\" is a special operator name in C++",
		     NODE_NAME (result));
    }

  return result;
}

/* Lex a number to NUMBER starting at BUFFER->CUR - 1.  */
static void
lex_number (cpp_reader *pfile, cpp_string *number,
	    struct normalize_state *nst)
{
  const uchar *cur;
  const uchar *base;
  uchar *dest;

  base = pfile->buffer->cur - 1;
  do
    {
      cur = pfile->buffer->cur;

      /* N.B. ISIDNUM does not include $.  */
      while (ISIDNUM (*cur) || *cur == '.' || DIGIT_SEP (*cur)
	     || VALID_SIGN (*cur, cur[-1]))
	{
	  NORMALIZE_STATE_UPDATE_IDNUM (nst, *cur);
	  cur++;
	}

      pfile->buffer->cur = cur;
    }
  while (forms_identifier_p (pfile, false, nst));

  number->len = cur - base;
  dest = _cpp_unaligned_alloc (pfile, number->len + 1);
  memcpy (dest, base, number->len);
  dest[number->len] = '\0';
  number->text = dest;
}

/* Create a token of type TYPE with a literal spelling.  */
static void
create_literal (cpp_reader *pfile, cpp_token *token, const uchar *base,
		unsigned int len, enum cpp_ttype type)
{
  uchar *dest = _cpp_unaligned_alloc (pfile, len + 1);

  memcpy (dest, base, len);
  dest[len] = '\0';
  token->type = type;
  token->val.str.len = len;
  token->val.str.text = dest;
}

/* Subroutine of lex_raw_string: Append LEN chars from BASE to the buffer
   sequence from *FIRST_BUFF_P to LAST_BUFF_P.  */

static void
bufring_append (cpp_reader *pfile, const uchar *base, size_t len,
		_cpp_buff **first_buff_p, _cpp_buff **last_buff_p)
{
  _cpp_buff *first_buff = *first_buff_p;
  _cpp_buff *last_buff = *last_buff_p;

  if (first_buff == NULL)
    first_buff = last_buff = _cpp_get_buff (pfile, len);
  else if (len > BUFF_ROOM (last_buff))
    {
      size_t room = BUFF_ROOM (last_buff);
      memcpy (BUFF_FRONT (last_buff), base, room);
      BUFF_FRONT (last_buff) += room;
      base += room;
      len -= room;
      last_buff = _cpp_append_extend_buff (pfile, last_buff, len);
    }

  memcpy (BUFF_FRONT (last_buff), base, len);
  BUFF_FRONT (last_buff) += len;

  *first_buff_p = first_buff;
  *last_buff_p = last_buff;
}


/* Returns true if a macro has been defined.
   This might not work if compile with -save-temps,
   or preprocess separately from compilation.  */

static bool
is_macro(cpp_reader *pfile, const uchar *base)
{
  const uchar *cur = base;
  if (! ISIDST (*cur))
    return false;
  unsigned int hash = HT_HASHSTEP (0, *cur);
  ++cur;
  while (ISIDNUM (*cur))
    {
      hash = HT_HASHSTEP (hash, *cur);
      ++cur;
    }
  hash = HT_HASHFINISH (hash, cur - base);

  cpp_hashnode *result = CPP_HASHNODE (ht_lookup_with_hash (pfile->hash_table,
					base, cur - base, hash, HT_NO_INSERT));

  return !result ? false : (result->type == NT_MACRO);
}


/* Lexes a raw string.  The stored string contains the spelling, including
   double quotes, delimiter string, '(' and ')', any leading
   'L', 'u', 'U' or 'u8' and 'R' modifier.  It returns the type of the
   literal, or CPP_OTHER if it was not properly terminated.

   The spelling is NUL-terminated, but it is not guaranteed that this
   is the first NUL since embedded NULs are preserved.  */

static void
lex_raw_string (cpp_reader *pfile, cpp_token *token, const uchar *base,
		const uchar *cur)
{
  uchar raw_prefix[17];
  uchar temp_buffer[18];
  const uchar *orig_base;
  unsigned int raw_prefix_len = 0, raw_suffix_len = 0;
  enum raw_str_phase { RAW_STR_PREFIX, RAW_STR, RAW_STR_SUFFIX };
  raw_str_phase phase = RAW_STR_PREFIX;
  enum cpp_ttype type;
  size_t total_len = 0;
  /* Index into temp_buffer during phases other than RAW_STR,
     during RAW_STR phase 17 to tell BUF_APPEND that nothing should
     be appended to temp_buffer.  */
  size_t temp_buffer_len = 0;
  _cpp_buff *first_buff = NULL, *last_buff = NULL;
  size_t raw_prefix_start;
  _cpp_line_note *note = &pfile->buffer->notes[pfile->buffer->cur_note];

  type = (*base == 'L' ? CPP_WSTRING :
	  *base == 'U' ? CPP_STRING32 :
	  *base == 'u' ? (base[1] == '8' ? CPP_UTF8STRING : CPP_STRING16)
	  : CPP_STRING);

#define BUF_APPEND(STR,LEN)					\
      do {							\
	bufring_append (pfile, (const uchar *)(STR), (LEN),	\
			&first_buff, &last_buff);		\
	total_len += (LEN);					\
	if (__builtin_expect (temp_buffer_len < 17, 0)		\
	    && (const uchar *)(STR) != base			\
	    && (LEN) <= 2)					\
	  {							\
	    memcpy (temp_buffer + temp_buffer_len,		\
		    (const uchar *)(STR), (LEN));		\
	    temp_buffer_len += (LEN);				\
	  }							\
      } while (0);

  orig_base = base;
  ++cur;
  raw_prefix_start = cur - base;
  for (;;)
    {
      cppchar_t c;

      /* If we previously performed any trigraph or line splicing
	 transformations, undo them in between the opening and closing
	 double quote.  */
      while (note->pos < cur)
	++note;
      for (; note->pos == cur; ++note)
	{
	  switch (note->type)
	    {
	    case '\\':
	    case ' ':
	      /* Restore backslash followed by newline.  */
	      BUF_APPEND (base, cur - base);
	      base = cur;
	      BUF_APPEND ("\\", 1);
	    after_backslash:
	      if (note->type == ' ')
		{
		  /* GNU backslash whitespace newline extension.  FIXME
		     could be any sequence of non-vertical space.  When we
		     can properly restore any such sequence, we should mark
		     this note as handled so _cpp_process_line_notes
		     doesn't warn.  */
		  BUF_APPEND (" ", 1);
		}

	      BUF_APPEND ("\n", 1);
	      break;

	    case 0:
	      /* Already handled.  */
	      break;

	    default:
	      if (_cpp_trigraph_map[note->type])
		{
		  /* Don't warn about this trigraph in
		     _cpp_process_line_notes, since trigraphs show up as
		     trigraphs in raw strings.  */
		  uchar type = note->type;
		  note->type = 0;

		  if (!CPP_OPTION (pfile, trigraphs))
		    /* If we didn't convert the trigraph in the first
		       place, don't do anything now either.  */
		    break;

		  BUF_APPEND (base, cur - base);
		  base = cur;
		  BUF_APPEND ("??", 2);

		  /* ??/ followed by newline gets two line notes, one for
		     the trigraph and one for the backslash/newline.  */
		  if (type == '/' && note[1].pos == cur)
		    {
		      if (note[1].type != '\\'
			  && note[1].type != ' ')
			abort ();
		      BUF_APPEND ("/", 1);
		      ++note;
		      goto after_backslash;
		    }
		  else
		    {
		      /* Skip the replacement character.  */
		      base = ++cur;
		      BUF_APPEND (&type, 1);
		      c = type;
		      goto check_c;
		    }
		}
	      else
		abort ();
	      break;
	    }
	}
      c = *cur++;
      if (__builtin_expect (temp_buffer_len < 17, 0))
	temp_buffer[temp_buffer_len++] = c;

     check_c:
      if (phase == RAW_STR_PREFIX)
	{
	  while (raw_prefix_len < temp_buffer_len)
	    {
	      raw_prefix[raw_prefix_len] = temp_buffer[raw_prefix_len];
	      switch (raw_prefix[raw_prefix_len])
		{
		case ' ': case '(': case ')': case '\\': case '\t':
		case '\v': case '\f': case '\n': default:
		  break;
		/* Basic source charset except the above chars.  */
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
		case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
		case 's': case 't': case 'u': case 'v': case 'w': case 'x':
		case 'y': case 'z':
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
		case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
		case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
		case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
		case 'Y': case 'Z':
		case '0': case '1': case '2': case '3': case '4': case '5':
		case '6': case '7': case '8': case '9':
		case '_': case '{': case '}': case '#': case '[': case ']':
		case '<': case '>': case '%': case ':': case ';': case '.':
		case '?': case '*': case '+': case '-': case '/': case '^':
		case '&': case '|': case '~': case '!': case '=': case ',':
		case '"': case '\'':
		  if (raw_prefix_len < 16)
		    {
		      raw_prefix_len++;
		      continue;
		    }
		  break;
		}

	      if (raw_prefix[raw_prefix_len] != '(')
		{
		  int col = CPP_BUF_COLUMN (pfile->buffer, cur) + 1;
		  if (raw_prefix_len == 16)
		    cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
					 col, "raw string delimiter longer "
					      "than 16 characters");
		  else if (raw_prefix[raw_prefix_len] == '\n')
		    cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
					 col, "invalid new-line in raw "
					      "string delimiter");
		  else
		    cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
					 col, "invalid character '%c' in "
					      "raw string delimiter",
					 (int) raw_prefix[raw_prefix_len]);
		  pfile->buffer->cur = orig_base + raw_prefix_start - 1;
		  create_literal (pfile, token, orig_base,
				  raw_prefix_start - 1, CPP_OTHER);
		  if (first_buff)
		    _cpp_release_buff (pfile, first_buff);
		  return;
		}
	      raw_prefix[raw_prefix_len] = '"';
	      phase = RAW_STR;
	      /* Nothing should be appended to temp_buffer during
		 RAW_STR phase.  */
	      temp_buffer_len = 17;
	      break;
	    }
	  continue;
	}
      else if (phase == RAW_STR_SUFFIX)
	{
	  while (raw_suffix_len <= raw_prefix_len
		 && raw_suffix_len < temp_buffer_len
		 && temp_buffer[raw_suffix_len] == raw_prefix[raw_suffix_len])
	    raw_suffix_len++;
	  if (raw_suffix_len > raw_prefix_len)
	    break;
	  if (raw_suffix_len == temp_buffer_len)
	    continue;
	  phase = RAW_STR;
	  /* Nothing should be appended to temp_buffer during
	     RAW_STR phase.  */
	  temp_buffer_len = 17;
	}
      if (c == ')')
	{
	  phase = RAW_STR_SUFFIX;
	  raw_suffix_len = 0;
	  temp_buffer_len = 0;
	}
      else if (c == '\n')
	{
	  if (pfile->state.in_directive
	      || (pfile->state.parsing_args
		  && pfile->buffer->next_line >= pfile->buffer->rlimit))
	    {
	      cur--;
	      type = CPP_OTHER;
	      cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc, 0,
				   "unterminated raw string");
	      break;
	    }

	  BUF_APPEND (base, cur - base);

	  if (pfile->buffer->cur < pfile->buffer->rlimit)
	    CPP_INCREMENT_LINE (pfile, 0);
	  pfile->buffer->need_line = true;

	  pfile->buffer->cur = cur-1;
	  _cpp_process_line_notes (pfile, false);
	  if (!_cpp_get_fresh_line (pfile))
	    {
	      source_location src_loc = token->src_loc;
	      token->type = CPP_EOF;
	      /* Tell the compiler the line number of the EOF token.  */
	      token->src_loc = pfile->line_table->highest_line;
	      token->flags = BOL;
	      if (first_buff != NULL)
		_cpp_release_buff (pfile, first_buff);
	      cpp_error_with_line (pfile, CPP_DL_ERROR, src_loc, 0,
				   "unterminated raw string");
	      return;
	    }

	  cur = base = pfile->buffer->cur;
	  note = &pfile->buffer->notes[pfile->buffer->cur_note];
	}
    }

  if (CPP_OPTION (pfile, user_literals))
    {
      /* If a string format macro, say from inttypes.h, is placed touching
	 a string literal it could be parsed as a C++11 user-defined string
	 literal thus breaking the program.
	 Try to identify macros with is_macro. A warning is issued. */
      if (is_macro (pfile, cur))
	{
	  /* Raise a warning, but do not consume subsequent tokens.  */
	  if (CPP_OPTION (pfile, warn_literal_suffix) && !pfile->state.skipping)
	    cpp_warning_with_line (pfile, CPP_W_LITERAL_SUFFIX,
				   token->src_loc, 0,
				   "invalid suffix on literal; C++11 requires "
				   "a space between literal and string macro");
	}
      /* Grab user defined literal suffix.  */
      else if (ISIDST (*cur))
	{
	  type = cpp_userdef_string_add_type (type);
	  ++cur;

	  while (ISIDNUM (*cur))
	    ++cur;
	}
    }

  pfile->buffer->cur = cur;
  if (first_buff == NULL)
    create_literal (pfile, token, base, cur - base, type);
  else
    {
      uchar *dest = _cpp_unaligned_alloc (pfile, total_len + (cur - base) + 1);

      token->type = type;
      token->val.str.len = total_len + (cur - base);
      token->val.str.text = dest;
      last_buff = first_buff;
      while (last_buff != NULL)
	{
	  memcpy (dest, last_buff->base,
		  BUFF_FRONT (last_buff) - last_buff->base);
	  dest += BUFF_FRONT (last_buff) - last_buff->base;
	  last_buff = last_buff->next;
	}
      _cpp_release_buff (pfile, first_buff);
      memcpy (dest, base, cur - base);
      dest[cur - base] = '\0';
    }
}

/* Lexes a string, character constant, or angle-bracketed header file
   name.  The stored string contains the spelling, including opening
   quote and any leading 'L', 'u', 'U' or 'u8' and optional
   'R' modifier.  It returns the type of the literal, or CPP_OTHER
   if it was not properly terminated, or CPP_LESS for an unterminated
   header name which must be relexed as normal tokens.

   The spelling is NUL-terminated, but it is not guaranteed that this
   is the first NUL since embedded NULs are preserved.  */
static void
lex_string (cpp_reader *pfile, cpp_token *token, const uchar *base)
{
  bool saw_NUL = false;
  const uchar *cur;
  cppchar_t terminator;
  enum cpp_ttype type;

  cur = base;
  terminator = *cur++;
  if (terminator == 'L' || terminator == 'U')
    terminator = *cur++;
  else if (terminator == 'u')
    {
      terminator = *cur++;
      if (terminator == '8')
	terminator = *cur++;
    }
  if (terminator == 'R')
    {
      lex_raw_string (pfile, token, base, cur);
      return;
    }
  if (terminator == '"')
    type = (*base == 'L' ? CPP_WSTRING :
	    *base == 'U' ? CPP_STRING32 :
	    *base == 'u' ? (base[1] == '8' ? CPP_UTF8STRING : CPP_STRING16)
			 : CPP_STRING);
  else if (terminator == '\'')
    type = (*base == 'L' ? CPP_WCHAR :
	    *base == 'U' ? CPP_CHAR32 :
	    *base == 'u' ? CPP_CHAR16 : CPP_CHAR);
  else
    terminator = '>', type = CPP_HEADER_NAME;

  for (;;)
    {
      cppchar_t c = *cur++;

      /* In #include-style directives, terminators are not escapable.  */
      if (c == '\\' && !pfile->state.angled_headers && *cur != '\n')
	cur++;
      else if (c == terminator)
	break;
      else if (c == '\n')
	{
	  cur--;
	  /* Unmatched quotes always yield undefined behavior, but
	     greedy lexing means that what appears to be an unterminated
	     header name may actually be a legitimate sequence of tokens.  */
	  if (terminator == '>')
	    {
	      token->type = CPP_LESS;
	      return;
	    }
	  type = CPP_OTHER;
	  break;
	}
      else if (c == '\0')
	saw_NUL = true;
    }

  if (saw_NUL && !pfile->state.skipping)
    cpp_error (pfile, CPP_DL_WARNING,
	       "null character(s) preserved in literal");

  if (type == CPP_OTHER && CPP_OPTION (pfile, lang) != CLK_ASM)
    cpp_error (pfile, CPP_DL_PEDWARN, "missing terminating %c character",
	       (int) terminator);

  if (CPP_OPTION (pfile, user_literals))
    {
      /* If a string format macro, say from inttypes.h, is placed touching
	 a string literal it could be parsed as a C++11 user-defined string
	 literal thus breaking the program.
	 Try to identify macros with is_macro. A warning is issued. */
      if (is_macro (pfile, cur))
	{
	  /* Raise a warning, but do not consume subsequent tokens.  */
	  if (CPP_OPTION (pfile, warn_literal_suffix) && !pfile->state.skipping)
	    cpp_warning_with_line (pfile, CPP_W_LITERAL_SUFFIX,
				   token->src_loc, 0,
				   "invalid suffix on literal; C++11 requires "
				   "a space between literal and string macro");
	}
      /* Grab user defined literal suffix.  */
      else if (ISIDST (*cur))
	{
	  type = cpp_userdef_char_add_type (type);
	  type = cpp_userdef_string_add_type (type);
          ++cur;

	  while (ISIDNUM (*cur))
	    ++cur;
	}
    }

  pfile->buffer->cur = cur;
  create_literal (pfile, token, base, cur - base, type);
}

/* Return the comment table. The client may not make any assumption
   about the ordering of the table.  */
cpp_comment_table *
cpp_get_comments (cpp_reader *pfile)
{
  return &pfile->comments;
}

/* Append a comment to the end of the comment table. */
static void 
store_comment (cpp_reader *pfile, cpp_token *token) 
{
  int len;

  if (pfile->comments.allocated == 0)
    {
      pfile->comments.allocated = 256; 
      pfile->comments.entries = (cpp_comment *) xmalloc
	(pfile->comments.allocated * sizeof (cpp_comment));
    }

  if (pfile->comments.count == pfile->comments.allocated)
    {
      pfile->comments.allocated *= 2;
      pfile->comments.entries = (cpp_comment *) xrealloc
	(pfile->comments.entries,
	 pfile->comments.allocated * sizeof (cpp_comment));
    }

  len = token->val.str.len;

  /* Copy comment. Note, token may not be NULL terminated. */
  pfile->comments.entries[pfile->comments.count].comment = 
    (char *) xmalloc (sizeof (char) * (len + 1));
  memcpy (pfile->comments.entries[pfile->comments.count].comment,
	  token->val.str.text, len);
  pfile->comments.entries[pfile->comments.count].comment[len] = '\0';

  /* Set source location. */
  pfile->comments.entries[pfile->comments.count].sloc = token->src_loc;

  /* Increment the count of entries in the comment table. */
  pfile->comments.count++;
}

/* The stored comment includes the comment start and any terminator.  */
static void
save_comment (cpp_reader *pfile, cpp_token *token, const unsigned char *from,
	      cppchar_t type)
{
  unsigned char *buffer;
  unsigned int len, clen, i;

  len = pfile->buffer->cur - from + 1; /* + 1 for the initial '/'.  */

  /* C++ comments probably (not definitely) have moved past a new
     line, which we don't want to save in the comment.  */
  if (is_vspace (pfile->buffer->cur[-1]))
    len--;

  /* If we are currently in a directive or in argument parsing, then
     we need to store all C++ comments as C comments internally, and
     so we need to allocate a little extra space in that case.

     Note that the only time we encounter a directive here is
     when we are saving comments in a "#define".  */
  clen = ((pfile->state.in_directive || pfile->state.parsing_args)
	  && type == '/') ? len + 2 : len;

  buffer = _cpp_unaligned_alloc (pfile, clen);

  token->type = CPP_COMMENT;
  token->val.str.len = clen;
  token->val.str.text = buffer;

  buffer[0] = '/';
  memcpy (buffer + 1, from, len - 1);

  /* Finish conversion to a C comment, if necessary.  */
  if ((pfile->state.in_directive || pfile->state.parsing_args) && type == '/')
    {
      buffer[1] = '*';
      buffer[clen - 2] = '*';
      buffer[clen - 1] = '/';
      /* As there can be in a C++ comments illegal sequences for C comments
         we need to filter them out.  */
      for (i = 2; i < (clen - 2); i++)
        if (buffer[i] == '/' && (buffer[i - 1] == '*' || buffer[i + 1] == '*'))
          buffer[i] = '|';
    }

  /* Finally store this comment for use by clients of libcpp. */
  store_comment (pfile, token);
}

/* Allocate COUNT tokens for RUN.  */
void
_cpp_init_tokenrun (tokenrun *run, unsigned int count)
{
  run->base = XNEWVEC (cpp_token, count);
  run->limit = run->base + count;
  run->next = NULL;
}

/* Returns the next tokenrun, or creates one if there is none.  */
static tokenrun *
next_tokenrun (tokenrun *run)
{
  if (run->next == NULL)
    {
      run->next = XNEW (tokenrun);
      run->next->prev = run;
      _cpp_init_tokenrun (run->next, 250);
    }

  return run->next;
}

/* Return the number of not yet processed token in a given
   context.  */
int
_cpp_remaining_tokens_num_in_context (cpp_context *context)
{
  if (context->tokens_kind == TOKENS_KIND_DIRECT)
    return (LAST (context).token - FIRST (context).token);
  else if (context->tokens_kind == TOKENS_KIND_INDIRECT
	   || context->tokens_kind == TOKENS_KIND_EXTENDED)
    return (LAST (context).ptoken - FIRST (context).ptoken);
  else
      abort ();
}

/* Returns the token present at index INDEX in a given context.  If
   INDEX is zero, the next token to be processed is returned.  */
static const cpp_token*
_cpp_token_from_context_at (cpp_context *context, int index)
{
  if (context->tokens_kind == TOKENS_KIND_DIRECT)
    return &(FIRST (context).token[index]);
  else if (context->tokens_kind == TOKENS_KIND_INDIRECT
	   || context->tokens_kind == TOKENS_KIND_EXTENDED)
    return FIRST (context).ptoken[index];
 else
   abort ();
}

/* Look ahead in the input stream.  */
const cpp_token *
cpp_peek_token (cpp_reader *pfile, int index)
{
  cpp_context *context = pfile->context;
  const cpp_token *peektok;
  int count;

  /* First, scan through any pending cpp_context objects.  */
  while (context->prev)
    {
      ptrdiff_t sz = _cpp_remaining_tokens_num_in_context (context);

      if (index < (int) sz)
        return _cpp_token_from_context_at (context, index);
      index -= (int) sz;
      context = context->prev;
    }

  /* We will have to read some new tokens after all (and do so
     without invalidating preceding tokens).  */
  count = index;
  pfile->keep_tokens++;

  do
    {
      peektok = _cpp_lex_token (pfile);
      if (peektok->type == CPP_EOF)
	return peektok;
    }
  while (index--);

  _cpp_backup_tokens_direct (pfile, count + 1);
  pfile->keep_tokens--;

  return peektok;
}

/* Allocate a single token that is invalidated at the same time as the
   rest of the tokens on the line.  Has its line and col set to the
   same as the last lexed token, so that diagnostics appear in the
   right place.  */
cpp_token *
_cpp_temp_token (cpp_reader *pfile)
{
  cpp_token *old, *result;
  ptrdiff_t sz = pfile->cur_run->limit - pfile->cur_token;
  ptrdiff_t la = (ptrdiff_t) pfile->lookaheads;

  old = pfile->cur_token - 1;
  /* Any pre-existing lookaheads must not be clobbered.  */
  if (la)
    {
      if (sz <= la)
        {
          tokenrun *next = next_tokenrun (pfile->cur_run);

          if (sz < la)
            memmove (next->base + 1, next->base,
                     (la - sz) * sizeof (cpp_token));

          next->base[0] = pfile->cur_run->limit[-1];
        }

      if (sz > 1)
        memmove (pfile->cur_token + 1, pfile->cur_token,
                 MIN (la, sz - 1) * sizeof (cpp_token));
    }

  if (!sz && pfile->cur_token == pfile->cur_run->limit)
    {
      pfile->cur_run = next_tokenrun (pfile->cur_run);
      pfile->cur_token = pfile->cur_run->base;
    }

  result = pfile->cur_token++;
  result->src_loc = old->src_loc;
  return result;
}

/* Lex a token into RESULT (external interface).  Takes care of issues
   like directive handling, token lookahead, multiple include
   optimization and skipping.  */
const cpp_token *
_cpp_lex_token (cpp_reader *pfile)
{
  cpp_token *result;

  for (;;)
    {
      if (pfile->cur_token == pfile->cur_run->limit)
	{
	  pfile->cur_run = next_tokenrun (pfile->cur_run);
	  pfile->cur_token = pfile->cur_run->base;
	}
      /* We assume that the current token is somewhere in the current
	 run.  */
      if (pfile->cur_token < pfile->cur_run->base
	  || pfile->cur_token >= pfile->cur_run->limit)
	abort ();

      if (pfile->lookaheads)
	{
	  pfile->lookaheads--;
	  result = pfile->cur_token++;
	}
      else
	result = _cpp_lex_direct (pfile);

      if (result->flags & BOL)
	{
	  /* Is this a directive.  If _cpp_handle_directive returns
	     false, it is an assembler #.  */
	  if (result->type == CPP_HASH
	      /* 6.10.3 p 11: Directives in a list of macro arguments
		 gives undefined behavior.  This implementation
		 handles the directive as normal.  */
	      && pfile->state.parsing_args != 1)
	    {
	      if (_cpp_handle_directive (pfile, result->flags & PREV_WHITE))
		{
		  if (pfile->directive_result.type == CPP_PADDING)
		    continue;
		  result = &pfile->directive_result;
		}
	    }
	  else if (pfile->state.in_deferred_pragma)
	    result = &pfile->directive_result;

	  if (pfile->cb.line_change && !pfile->state.skipping)
	    pfile->cb.line_change (pfile, result, pfile->state.parsing_args);
	}

      /* We don't skip tokens in directives.  */
      if (pfile->state.in_directive || pfile->state.in_deferred_pragma)
	break;

      /* Outside a directive, invalidate controlling macros.  At file
	 EOF, _cpp_lex_direct takes care of popping the buffer, so we never
	 get here and MI optimization works.  */
      pfile->mi_valid = false;

      if (!pfile->state.skipping || result->type == CPP_EOF)
	break;
    }

  return result;
}

/* Returns true if a fresh line has been loaded.  */
bool
_cpp_get_fresh_line (cpp_reader *pfile)
{
  int return_at_eof;

  /* We can't get a new line until we leave the current directive.  */
  if (pfile->state.in_directive)
    return false;

  for (;;)
    {
      cpp_buffer *buffer = pfile->buffer;

      if (!buffer->need_line)
	return true;

      if (buffer->next_line < buffer->rlimit)
	{
	  _cpp_clean_line (pfile);
	  return true;
	}

      /* First, get out of parsing arguments state.  */
      if (pfile->state.parsing_args)
	return false;

      /* End of buffer.  Non-empty files should end in a newline.  */
      if (buffer->buf != buffer->rlimit
	  && buffer->next_line > buffer->rlimit
	  && !buffer->from_stage3)
	{
	  /* Clip to buffer size.  */
	  buffer->next_line = buffer->rlimit;
	}

      return_at_eof = buffer->return_at_eof;
      _cpp_pop_buffer (pfile);
      if (pfile->buffer == NULL || return_at_eof)
	return false;
    }
}

#define IF_NEXT_IS(CHAR, THEN_TYPE, ELSE_TYPE)		\
  do							\
    {							\
      result->type = ELSE_TYPE;				\
      if (*buffer->cur == CHAR)				\
	buffer->cur++, result->type = THEN_TYPE;	\
    }							\
  while (0)

/* Lex a token into pfile->cur_token, which is also incremented, to
   get diagnostics pointing to the correct location.

   Does not handle issues such as token lookahead, multiple-include
   optimization, directives, skipping etc.  This function is only
   suitable for use by _cpp_lex_token, and in special cases like
   lex_expansion_token which doesn't care for any of these issues.

   When meeting a newline, returns CPP_EOF if parsing a directive,
   otherwise returns to the start of the token buffer if permissible.
   Returns the location of the lexed token.  */
cpp_token *
_cpp_lex_direct (cpp_reader *pfile)
{
  cppchar_t c;
  cpp_buffer *buffer;
  const unsigned char *comment_start;
  cpp_token *result = pfile->cur_token++;

 fresh_line:
  result->flags = 0;
  buffer = pfile->buffer;
  if (buffer->need_line)
    {
      if (pfile->state.in_deferred_pragma)
	{
	  result->type = CPP_PRAGMA_EOL;
	  pfile->state.in_deferred_pragma = false;
	  if (!pfile->state.pragma_allow_expansion)
	    pfile->state.prevent_expansion--;
	  return result;
	}
      if (!_cpp_get_fresh_line (pfile))
	{
	  result->type = CPP_EOF;
	  if (!pfile->state.in_directive)
	    {
	      /* Tell the compiler the line number of the EOF token.  */
	      result->src_loc = pfile->line_table->highest_line;
	      result->flags = BOL;
	    }
	  return result;
	}
      if (!pfile->keep_tokens)
	{
	  pfile->cur_run = &pfile->base_run;
	  result = pfile->base_run.base;
	  pfile->cur_token = result + 1;
	}
      result->flags = BOL;
      if (pfile->state.parsing_args == 2)
	result->flags |= PREV_WHITE;
    }
  buffer = pfile->buffer;
 update_tokens_line:
  result->src_loc = pfile->line_table->highest_line;

 skipped_white:
  if (buffer->cur >= buffer->notes[buffer->cur_note].pos
      && !pfile->overlaid_buffer)
    {
      _cpp_process_line_notes (pfile, false);
      result->src_loc = pfile->line_table->highest_line;
    }
  c = *buffer->cur++;

  if (pfile->forced_token_location_p)
    result->src_loc = *pfile->forced_token_location_p;
  else
    result->src_loc = linemap_position_for_column (pfile->line_table,
					  CPP_BUF_COLUMN (buffer, buffer->cur));

  switch (c)
    {
    case ' ': case '\t': case '\f': case '\v': case '\0':
      result->flags |= PREV_WHITE;
      skip_whitespace (pfile, c);
      goto skipped_white;

    case '\n':
      if (buffer->cur < buffer->rlimit)
	CPP_INCREMENT_LINE (pfile, 0);
      buffer->need_line = true;
      goto fresh_line;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      {
	struct normalize_state nst = INITIAL_NORMALIZE_STATE;
	result->type = CPP_NUMBER;
	lex_number (pfile, &result->val.str, &nst);
	warn_about_normalization (pfile, result, &nst);
	break;
      }

    case 'L':
    case 'u':
    case 'U':
    case 'R':
      /* 'L', 'u', 'U', 'u8' or 'R' may introduce wide characters,
	 wide strings or raw strings.  */
      if (c == 'L' || CPP_OPTION (pfile, rliterals)
	  || (c != 'R' && CPP_OPTION (pfile, uliterals)))
	{
	  if ((*buffer->cur == '\'' && c != 'R')
	      || *buffer->cur == '"'
	      || (*buffer->cur == 'R'
		  && c != 'R'
		  && buffer->cur[1] == '"'
		  && CPP_OPTION (pfile, rliterals))
	      || (*buffer->cur == '8'
		  && c == 'u'
		  && (buffer->cur[1] == '"'
		      || (buffer->cur[1] == 'R' && buffer->cur[2] == '"'
			  && CPP_OPTION (pfile, rliterals)))))
	    {
	      lex_string (pfile, result, buffer->cur - 1);
	      break;
	    }
	}
      /* Fall through.  */

    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't':           case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P': case 'Q':
    case 'S': case 'T':           case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
      result->type = CPP_NAME;
      {
	struct normalize_state nst = INITIAL_NORMALIZE_STATE;
	result->val.node.node = lex_identifier (pfile, buffer->cur - 1, false,
						&nst);
	warn_about_normalization (pfile, result, &nst);
      }

      /* Convert named operators to their proper types.  */
      if (result->val.node.node->flags & NODE_OPERATOR)
	{
	  result->flags |= NAMED_OP;
	  result->type = (enum cpp_ttype) result->val.node.node->directive_index;
	}
      break;

    case '\'':
    case '"':
      lex_string (pfile, result, buffer->cur - 1);
      break;

    case '/':
      /* A potential block or line comment.  */
      comment_start = buffer->cur;
      c = *buffer->cur;
      
      if (c == '*')
	{
	  if (_cpp_skip_block_comment (pfile))
	    cpp_error (pfile, CPP_DL_ERROR, "unterminated comment");
	}
      else if (c == '/' && (CPP_OPTION (pfile, cplusplus_comments)
			    || cpp_in_system_header (pfile)))
	{
	  /* Warn about comments only if pedantically GNUC89, and not
	     in system headers.  */
	  if (CPP_OPTION (pfile, lang) == CLK_GNUC89 && CPP_PEDANTIC (pfile)
	      && ! buffer->warned_cplusplus_comments)
	    {
	      cpp_error (pfile, CPP_DL_PEDWARN,
			 "C++ style comments are not allowed in ISO C90");
	      cpp_error (pfile, CPP_DL_PEDWARN,
			 "(this will be reported only once per input file)");
	      buffer->warned_cplusplus_comments = 1;
	    }

	  if (skip_line_comment (pfile) && CPP_OPTION (pfile, warn_comments))
	    cpp_warning (pfile, CPP_W_COMMENTS, "multi-line comment");
	}
      else if (c == '=')
	{
	  buffer->cur++;
	  result->type = CPP_DIV_EQ;
	  break;
	}
      else
	{
	  result->type = CPP_DIV;
	  break;
	}

      if (!pfile->state.save_comments)
	{
	  result->flags |= PREV_WHITE;
	  goto update_tokens_line;
	}

      /* Save the comment as a token in its own right.  */
      save_comment (pfile, result, comment_start, c);
      break;

    case '<':
      if (pfile->state.angled_headers)
	{
	  lex_string (pfile, result, buffer->cur - 1);
	  if (result->type != CPP_LESS)
	    break;
	}

      result->type = CPP_LESS;
      if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_LESS_EQ;
      else if (*buffer->cur == '<')
	{
	  buffer->cur++;
	  IF_NEXT_IS ('=', CPP_LSHIFT_EQ, CPP_LSHIFT);
	}
      else if (CPP_OPTION (pfile, digraphs))
	{
	  if (*buffer->cur == ':')
	    {
	      /* C++11 [2.5/3 lex.pptoken], "Otherwise, if the next
		 three characters are <:: and the subsequent character
		 is neither : nor >, the < is treated as a preprocessor
		 token by itself".  */
	      if (CPP_OPTION (pfile, cplusplus)
		  && CPP_OPTION (pfile, lang) != CLK_CXX98
		  && CPP_OPTION (pfile, lang) != CLK_GNUCXX
		  && buffer->cur[1] == ':'
		  && buffer->cur[2] != ':' && buffer->cur[2] != '>')
		break;

	      buffer->cur++;
	      result->flags |= DIGRAPH;
	      result->type = CPP_OPEN_SQUARE;
	    }
	  else if (*buffer->cur == '%')
	    {
	      buffer->cur++;
	      result->flags |= DIGRAPH;
	      result->type = CPP_OPEN_BRACE;
	    }
	}
      break;

    case '>':
      result->type = CPP_GREATER;
      if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_GREATER_EQ;
      else if (*buffer->cur == '>')
	{
	  buffer->cur++;
	  IF_NEXT_IS ('=', CPP_RSHIFT_EQ, CPP_RSHIFT);
	}
      break;

    case '%':
      result->type = CPP_MOD;
      if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_MOD_EQ;
      else if (CPP_OPTION (pfile, digraphs))
	{
	  if (*buffer->cur == ':')
	    {
	      buffer->cur++;
	      result->flags |= DIGRAPH;
	      result->type = CPP_HASH;
	      if (*buffer->cur == '%' && buffer->cur[1] == ':')
		buffer->cur += 2, result->type = CPP_PASTE, result->val.token_no = 0;
	    }
	  else if (*buffer->cur == '>')
	    {
	      buffer->cur++;
	      result->flags |= DIGRAPH;
	      result->type = CPP_CLOSE_BRACE;
	    }
	}
      break;

    case '.':
      result->type = CPP_DOT;
      if (ISDIGIT (*buffer->cur))
	{
	  struct normalize_state nst = INITIAL_NORMALIZE_STATE;
	  result->type = CPP_NUMBER;
	  lex_number (pfile, &result->val.str, &nst);
	  warn_about_normalization (pfile, result, &nst);
	}
      else if (*buffer->cur == '.' && buffer->cur[1] == '.')
	buffer->cur += 2, result->type = CPP_ELLIPSIS;
      else if (*buffer->cur == '*' && CPP_OPTION (pfile, cplusplus))
	buffer->cur++, result->type = CPP_DOT_STAR;
      break;

    case '+':
      result->type = CPP_PLUS;
      if (*buffer->cur == '+')
	buffer->cur++, result->type = CPP_PLUS_PLUS;
      else if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_PLUS_EQ;
      break;

    case '-':
      result->type = CPP_MINUS;
      if (*buffer->cur == '>')
	{
	  buffer->cur++;
	  result->type = CPP_DEREF;
	  if (*buffer->cur == '*' && CPP_OPTION (pfile, cplusplus))
	    buffer->cur++, result->type = CPP_DEREF_STAR;
	}
      else if (*buffer->cur == '-')
	buffer->cur++, result->type = CPP_MINUS_MINUS;
      else if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_MINUS_EQ;
      break;

    case '&':
      result->type = CPP_AND;
      if (*buffer->cur == '&')
	buffer->cur++, result->type = CPP_AND_AND;
      else if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_AND_EQ;
      break;

    case '|':
      result->type = CPP_OR;
      if (*buffer->cur == '|')
	buffer->cur++, result->type = CPP_OR_OR;
      else if (*buffer->cur == '=')
	buffer->cur++, result->type = CPP_OR_EQ;
      break;

    case ':':
      result->type = CPP_COLON;
      if (*buffer->cur == ':' && CPP_OPTION (pfile, cplusplus))
	buffer->cur++, result->type = CPP_SCOPE;
      else if (*buffer->cur == '>' && CPP_OPTION (pfile, digraphs))
	{
	  buffer->cur++;
	  result->flags |= DIGRAPH;
	  result->type = CPP_CLOSE_SQUARE;
	}
      break;

    case '*': IF_NEXT_IS ('=', CPP_MULT_EQ, CPP_MULT); break;
    case '=': IF_NEXT_IS ('=', CPP_EQ_EQ, CPP_EQ); break;
    case '!': IF_NEXT_IS ('=', CPP_NOT_EQ, CPP_NOT); break;
    case '^': IF_NEXT_IS ('=', CPP_XOR_EQ, CPP_XOR); break;
    case '#': IF_NEXT_IS ('#', CPP_PASTE, CPP_HASH); result->val.token_no = 0; break;

    case '?': result->type = CPP_QUERY; break;
    case '~': result->type = CPP_COMPL; break;
    case ',': result->type = CPP_COMMA; break;
    case '(': result->type = CPP_OPEN_PAREN; break;
    case ')': result->type = CPP_CLOSE_PAREN; break;
    case '[': result->type = CPP_OPEN_SQUARE; break;
    case ']': result->type = CPP_CLOSE_SQUARE; break;
    case '{': result->type = CPP_OPEN_BRACE; break;
    case '}': result->type = CPP_CLOSE_BRACE; break;
    case ';': result->type = CPP_SEMICOLON; break;

      /* @ is a punctuator in Objective-C.  */
    case '@': result->type = CPP_ATSIGN; break;

    case '$':
    case '\\':
      {
	const uchar *base = --buffer->cur;
	struct normalize_state nst = INITIAL_NORMALIZE_STATE;

	if (forms_identifier_p (pfile, true, &nst))
	  {
	    result->type = CPP_NAME;
	    result->val.node.node = lex_identifier (pfile, base, true, &nst);
	    warn_about_normalization (pfile, result, &nst);
	    break;
	  }
	buffer->cur++;
      }

    default:
      create_literal (pfile, result, buffer->cur - 1, 1, CPP_OTHER);
      break;
    }

  return result;
}

/* An upper bound on the number of bytes needed to spell TOKEN.
   Does not include preceding whitespace.  */
unsigned int
cpp_token_len (const cpp_token *token)
{
  unsigned int len;

  switch (TOKEN_SPELL (token))
    {
    default:		len = 6;				break;
    case SPELL_LITERAL:	len = token->val.str.len;		break;
    case SPELL_IDENT:	len = NODE_LEN (token->val.node.node) * 10;	break;
    }

  return len;
}

/* Parse UTF-8 out of NAMEP and place a \U escape in BUFFER.
   Return the number of bytes read out of NAME.  (There are always
   10 bytes written to BUFFER.)  */

static size_t
utf8_to_ucn (unsigned char *buffer, const unsigned char *name)
{
  int j;
  int ucn_len = 0;
  int ucn_len_c;
  unsigned t;
  unsigned long utf32;
  
  /* Compute the length of the UTF-8 sequence.  */
  for (t = *name; t & 0x80; t <<= 1)
    ucn_len++;
  
  utf32 = *name & (0x7F >> ucn_len);
  for (ucn_len_c = 1; ucn_len_c < ucn_len; ucn_len_c++)
    {
      utf32 = (utf32 << 6) | (*++name & 0x3F);
      
      /* Ill-formed UTF-8.  */
      if ((*name & ~0x3F) != 0x80)
	abort ();
    }
  
  *buffer++ = '\\';
  *buffer++ = 'U';
  for (j = 7; j >= 0; j--)
    *buffer++ = "0123456789abcdef"[(utf32 >> (4 * j)) & 0xF];
  return ucn_len;
}

/* Given a token TYPE corresponding to a digraph, return a pointer to
   the spelling of the digraph.  */
static const unsigned char *
cpp_digraph2name (enum cpp_ttype type)
{
  return digraph_spellings[(int) type - (int) CPP_FIRST_DIGRAPH];
}

/* Write the spelling of a token TOKEN to BUFFER.  The buffer must
   already contain the enough space to hold the token's spelling.
   Returns a pointer to the character after the last character written.
   FORSTRING is true if this is to be the spelling after translation
   phase 1 (this is different for UCNs).
   FIXME: Would be nice if we didn't need the PFILE argument.  */
unsigned char *
cpp_spell_token (cpp_reader *pfile, const cpp_token *token,
		 unsigned char *buffer, bool forstring)
{
  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;
	unsigned char c;

	if (token->flags & DIGRAPH)
	  spelling = cpp_digraph2name (token->type);
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);

	while ((c = *spelling++) != '\0')
	  *buffer++ = c;
      }
      break;

    spell_ident:
    case SPELL_IDENT:
      if (forstring)
	{
	  memcpy (buffer, NODE_NAME (token->val.node.node),
		  NODE_LEN (token->val.node.node));
	  buffer += NODE_LEN (token->val.node.node);
	}
      else
	{
	  size_t i;
	  const unsigned char * name = NODE_NAME (token->val.node.node);
	  
	  for (i = 0; i < NODE_LEN (token->val.node.node); i++)
	    if (name[i] & ~0x7F)
	      {
		i += utf8_to_ucn (buffer, name + i) - 1;
		buffer += 10;
	      }
	    else
	      *buffer++ = NODE_NAME (token->val.node.node)[i];
	}
      break;

    case SPELL_LITERAL:
      memcpy (buffer, token->val.str.text, token->val.str.len);
      buffer += token->val.str.len;
      break;

    case SPELL_NONE:
      cpp_error (pfile, CPP_DL_ICE,
		 "unspellable token %s", TOKEN_NAME (token));
      break;
    }

  return buffer;
}

/* Returns TOKEN spelt as a null-terminated string.  The string is
   freed when the reader is destroyed.  Useful for diagnostics.  */
unsigned char *
cpp_token_as_text (cpp_reader *pfile, const cpp_token *token)
{ 
  unsigned int len = cpp_token_len (token) + 1;
  unsigned char *start = _cpp_unaligned_alloc (pfile, len), *end;

  end = cpp_spell_token (pfile, token, start, false);
  end[0] = '\0';

  return start;
}

/* Returns a pointer to a string which spells the token defined by
   TYPE and FLAGS.  Used by C front ends, which really should move to
   using cpp_token_as_text.  */
const char *
cpp_type2name (enum cpp_ttype type, unsigned char flags)
{
  if (flags & DIGRAPH)
    return (const char *) cpp_digraph2name (type);
  else if (flags & NAMED_OP)
    return cpp_named_operator2name (type);

  return (const char *) token_spellings[type].name;
}

/* Writes the spelling of token to FP, without any preceding space.
   Separated from cpp_spell_token for efficiency - to avoid stdio
   double-buffering.  */
void
cpp_output_token (const cpp_token *token, FILE *fp)
{
  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;
	int c;

	if (token->flags & DIGRAPH)
	  spelling = cpp_digraph2name (token->type);
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);

	c = *spelling;
	do
	  putc (c, fp);
	while ((c = *++spelling) != '\0');
      }
      break;

    spell_ident:
    case SPELL_IDENT:
      {
	size_t i;
	const unsigned char * name = NODE_NAME (token->val.node.node);
	
	for (i = 0; i < NODE_LEN (token->val.node.node); i++)
	  if (name[i] & ~0x7F)
	    {
	      unsigned char buffer[10];
	      i += utf8_to_ucn (buffer, name + i) - 1;
	      fwrite (buffer, 1, 10, fp);
	    }
	  else
	    fputc (NODE_NAME (token->val.node.node)[i], fp);
      }
      break;

    case SPELL_LITERAL:
      fwrite (token->val.str.text, 1, token->val.str.len, fp);
      break;

    case SPELL_NONE:
      /* An error, most probably.  */
      break;
    }
}

/* Compare two tokens.  */
int
_cpp_equiv_tokens (const cpp_token *a, const cpp_token *b)
{
  if (a->type == b->type && a->flags == b->flags)
    switch (TOKEN_SPELL (a))
      {
      default:			/* Keep compiler happy.  */
      case SPELL_OPERATOR:
	/* token_no is used to track where multiple consecutive ##
	   tokens were originally located.  */
	return (a->type != CPP_PASTE || a->val.token_no == b->val.token_no);
      case SPELL_NONE:
	return (a->type != CPP_MACRO_ARG
		|| a->val.macro_arg.arg_no == b->val.macro_arg.arg_no);
      case SPELL_IDENT:
	return a->val.node.node == b->val.node.node;
      case SPELL_LITERAL:
	return (a->val.str.len == b->val.str.len
		&& !memcmp (a->val.str.text, b->val.str.text,
			    a->val.str.len));
      }

  return 0;
}

/* Returns nonzero if a space should be inserted to avoid an
   accidental token paste for output.  For simplicity, it is
   conservative, and occasionally advises a space where one is not
   needed, e.g. "." and ".2".  */
int
cpp_avoid_paste (cpp_reader *pfile, const cpp_token *token1,
		 const cpp_token *token2)
{
  enum cpp_ttype a = token1->type, b = token2->type;
  cppchar_t c;

  if (token1->flags & NAMED_OP)
    a = CPP_NAME;
  if (token2->flags & NAMED_OP)
    b = CPP_NAME;

  c = EOF;
  if (token2->flags & DIGRAPH)
    c = digraph_spellings[(int) b - (int) CPP_FIRST_DIGRAPH][0];
  else if (token_spellings[b].category == SPELL_OPERATOR)
    c = token_spellings[b].name[0];

  /* Quickly get everything that can paste with an '='.  */
  if ((int) a <= (int) CPP_LAST_EQ && c == '=')
    return 1;

  switch (a)
    {
    case CPP_GREATER:	return c == '>';
    case CPP_LESS:	return c == '<' || c == '%' || c == ':';
    case CPP_PLUS:	return c == '+';
    case CPP_MINUS:	return c == '-' || c == '>';
    case CPP_DIV:	return c == '/' || c == '*'; /* Comments.  */
    case CPP_MOD:	return c == ':' || c == '>';
    case CPP_AND:	return c == '&';
    case CPP_OR:	return c == '|';
    case CPP_COLON:	return c == ':' || c == '>';
    case CPP_DEREF:	return c == '*';
    case CPP_DOT:	return c == '.' || c == '%' || b == CPP_NUMBER;
    case CPP_HASH:	return c == '#' || c == '%'; /* Digraph form.  */
    case CPP_NAME:	return ((b == CPP_NUMBER
				 && name_p (pfile, &token2->val.str))
				|| b == CPP_NAME
				|| b == CPP_CHAR || b == CPP_STRING); /* L */
    case CPP_NUMBER:	return (b == CPP_NUMBER || b == CPP_NAME
				|| c == '.' || c == '+' || c == '-');
				      /* UCNs */
    case CPP_OTHER:	return ((token1->val.str.text[0] == '\\'
				 && b == CPP_NAME)
				|| (CPP_OPTION (pfile, objc)
				    && token1->val.str.text[0] == '@'
				    && (b == CPP_NAME || b == CPP_STRING)));
    case CPP_STRING:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
    case CPP_STRING16:
    case CPP_STRING32:	return (CPP_OPTION (pfile, user_literals)
				&& (b == CPP_NAME
				    || (TOKEN_SPELL (token2) == SPELL_LITERAL
					&& ISIDST (token2->val.str.text[0]))));

    default:		break;
    }

  return 0;
}

/* Output all the remaining tokens on the current line, and a newline
   character, to FP.  Leading whitespace is removed.  If there are
   macros, special token padding is not performed.  */
void
cpp_output_line (cpp_reader *pfile, FILE *fp)
{
  const cpp_token *token;

  token = cpp_get_token (pfile);
  while (token->type != CPP_EOF)
    {
      cpp_output_token (token, fp);
      token = cpp_get_token (pfile);
      if (token->flags & PREV_WHITE)
	putc (' ', fp);
    }

  putc ('\n', fp);
}

/* Return a string representation of all the remaining tokens on the
   current line.  The result is allocated using xmalloc and must be
   freed by the caller.  */
unsigned char *
cpp_output_line_to_string (cpp_reader *pfile, const unsigned char *dir_name)
{
  const cpp_token *token;
  unsigned int out = dir_name ? ustrlen (dir_name) : 0;
  unsigned int alloced = 120 + out;
  unsigned char *result = (unsigned char *) xmalloc (alloced);

  /* If DIR_NAME is empty, there are no initial contents.  */
  if (dir_name)
    {
      sprintf ((char *) result, "#%s ", dir_name);
      out += 2;
    }

  token = cpp_get_token (pfile);
  while (token->type != CPP_EOF)
    {
      unsigned char *last;
      /* Include room for a possible space and the terminating nul.  */
      unsigned int len = cpp_token_len (token) + 2;

      if (out + len > alloced)
	{
	  alloced *= 2;
	  if (out + len > alloced)
	    alloced = out + len;
	  result = (unsigned char *) xrealloc (result, alloced);
	}

      last = cpp_spell_token (pfile, token, &result[out], 0);
      out = last - result;

      token = cpp_get_token (pfile);
      if (token->flags & PREV_WHITE)
	result[out++] = ' ';
    }

  result[out] = '\0';
  return result;
}

/* Memory buffers.  Changing these three constants can have a dramatic
   effect on performance.  The values here are reasonable defaults,
   but might be tuned.  If you adjust them, be sure to test across a
   range of uses of cpplib, including heavy nested function-like macro
   expansion.  Also check the change in peak memory usage (NJAMD is a
   good tool for this).  */
#define MIN_BUFF_SIZE 8000
#define BUFF_SIZE_UPPER_BOUND(MIN_SIZE) (MIN_BUFF_SIZE + (MIN_SIZE) * 3 / 2)
#define EXTENDED_BUFF_SIZE(BUFF, MIN_EXTRA) \
	(MIN_EXTRA + ((BUFF)->limit - (BUFF)->cur) * 2)

#if MIN_BUFF_SIZE > BUFF_SIZE_UPPER_BOUND (0)
  #error BUFF_SIZE_UPPER_BOUND must be at least as large as MIN_BUFF_SIZE!
#endif

/* Create a new allocation buffer.  Place the control block at the end
   of the buffer, so that buffer overflows will cause immediate chaos.  */
static _cpp_buff *
new_buff (size_t len)
{
  _cpp_buff *result;
  unsigned char *base;

  if (len < MIN_BUFF_SIZE)
    len = MIN_BUFF_SIZE;
  len = CPP_ALIGN (len);

#ifdef ENABLE_VALGRIND_CHECKING
  /* Valgrind warns about uses of interior pointers, so put _cpp_buff
     struct first.  */
  size_t slen = CPP_ALIGN2 (sizeof (_cpp_buff), 2 * DEFAULT_ALIGNMENT);
  base = XNEWVEC (unsigned char, len + slen);
  result = (_cpp_buff *) base;
  base += slen;
#else
  base = XNEWVEC (unsigned char, len + sizeof (_cpp_buff));
  result = (_cpp_buff *) (base + len);
#endif
  result->base = base;
  result->cur = base;
  result->limit = base + len;
  result->next = NULL;
  return result;
}

/* Place a chain of unwanted allocation buffers on the free list.  */
void
_cpp_release_buff (cpp_reader *pfile, _cpp_buff *buff)
{
  _cpp_buff *end = buff;

  while (end->next)
    end = end->next;
  end->next = pfile->free_buffs;
  pfile->free_buffs = buff;
}

/* Return a free buffer of size at least MIN_SIZE.  */
_cpp_buff *
_cpp_get_buff (cpp_reader *pfile, size_t min_size)
{
  _cpp_buff *result, **p;

  for (p = &pfile->free_buffs;; p = &(*p)->next)
    {
      size_t size;

      if (*p == NULL)
	return new_buff (min_size);
      result = *p;
      size = result->limit - result->base;
      /* Return a buffer that's big enough, but don't waste one that's
         way too big.  */
      if (size >= min_size && size <= BUFF_SIZE_UPPER_BOUND (min_size))
	break;
    }

  *p = result->next;
  result->next = NULL;
  result->cur = result->base;
  return result;
}

/* Creates a new buffer with enough space to hold the uncommitted
   remaining bytes of BUFF, and at least MIN_EXTRA more bytes.  Copies
   the excess bytes to the new buffer.  Chains the new buffer after
   BUFF, and returns the new buffer.  */
_cpp_buff *
_cpp_append_extend_buff (cpp_reader *pfile, _cpp_buff *buff, size_t min_extra)
{
  size_t size = EXTENDED_BUFF_SIZE (buff, min_extra);
  _cpp_buff *new_buff = _cpp_get_buff (pfile, size);

  buff->next = new_buff;
  memcpy (new_buff->base, buff->cur, BUFF_ROOM (buff));
  return new_buff;
}

/* Creates a new buffer with enough space to hold the uncommitted
   remaining bytes of the buffer pointed to by BUFF, and at least
   MIN_EXTRA more bytes.  Copies the excess bytes to the new buffer.
   Chains the new buffer before the buffer pointed to by BUFF, and
   updates the pointer to point to the new buffer.  */
void
_cpp_extend_buff (cpp_reader *pfile, _cpp_buff **pbuff, size_t min_extra)
{
  _cpp_buff *new_buff, *old_buff = *pbuff;
  size_t size = EXTENDED_BUFF_SIZE (old_buff, min_extra);

  new_buff = _cpp_get_buff (pfile, size);
  memcpy (new_buff->base, old_buff->cur, BUFF_ROOM (old_buff));
  new_buff->next = old_buff;
  *pbuff = new_buff;
}

/* Free a chain of buffers starting at BUFF.  */
void
_cpp_free_buff (_cpp_buff *buff)
{
  _cpp_buff *next;

  for (; buff; buff = next)
    {
      next = buff->next;
#ifdef ENABLE_VALGRIND_CHECKING
      free (buff);
#else
      free (buff->base);
#endif
    }
}

/* Allocate permanent, unaligned storage of length LEN.  */
unsigned char *
_cpp_unaligned_alloc (cpp_reader *pfile, size_t len)
{
  _cpp_buff *buff = pfile->u_buff;
  unsigned char *result = buff->cur;

  if (len > (size_t) (buff->limit - result))
    {
      buff = _cpp_get_buff (pfile, len);
      buff->next = pfile->u_buff;
      pfile->u_buff = buff;
      result = buff->cur;
    }

  buff->cur = result + len;
  return result;
}

/* Allocate permanent, unaligned storage of length LEN from a_buff.
   That buffer is used for growing allocations when saving macro
   replacement lists in a #define, and when parsing an answer to an
   assertion in #assert, #unassert or #if (and therefore possibly
   whilst expanding macros).  It therefore must not be used by any
   code that they might call: specifically the lexer and the guts of
   the macro expander.

   All existing other uses clearly fit this restriction: storing
   registered pragmas during initialization.  */
unsigned char *
_cpp_aligned_alloc (cpp_reader *pfile, size_t len)
{
  _cpp_buff *buff = pfile->a_buff;
  unsigned char *result = buff->cur;

  if (len > (size_t) (buff->limit - result))
    {
      buff = _cpp_get_buff (pfile, len);
      buff->next = pfile->a_buff;
      pfile->a_buff = buff;
      result = buff->cur;
    }

  buff->cur = result + len;
  return result;
}

/* Say which field of TOK is in use.  */

enum cpp_token_fld_kind
cpp_token_val_index (const cpp_token *tok)
{
  switch (TOKEN_SPELL (tok))
    {
    case SPELL_IDENT:
      return CPP_TOKEN_FLD_NODE;
    case SPELL_LITERAL:
      return CPP_TOKEN_FLD_STR;
    case SPELL_OPERATOR:
      if (tok->type == CPP_PASTE)
	return CPP_TOKEN_FLD_TOKEN_NO;
      else
	return CPP_TOKEN_FLD_NONE;
    case SPELL_NONE:
      if (tok->type == CPP_MACRO_ARG)
	return CPP_TOKEN_FLD_ARG_NO;
      else if (tok->type == CPP_PADDING)
	return CPP_TOKEN_FLD_SOURCE;
      else if (tok->type == CPP_PRAGMA)
	return CPP_TOKEN_FLD_PRAGMA;
      /* else fall through */
    default:
      return CPP_TOKEN_FLD_NONE;
    }
}

/* All tokens lexed in R after calling this function will be forced to have
   their source_location the same as the location referenced by P, until
   cpp_stop_forcing_token_locations is called for R.  */

void
cpp_force_token_locations (cpp_reader *r, source_location *p)
{
  r->forced_token_location_p = p;
}

/* Go back to assigning locations naturally for lexed tokens.  */

void
cpp_stop_forcing_token_locations (cpp_reader *r)
{
  r->forced_token_location_p = NULL;
}
