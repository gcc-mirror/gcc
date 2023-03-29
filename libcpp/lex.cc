/* CPP Library - lexical analysis.
   Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

/* ISO 10646 defines the UCS codespace as the range 0-0x10FFFF inclusive.  */
#define UCS_LIMIT 0x10FFFF

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

/* Disable on Solaris 2/x86 until the following problem can be properly
   autoconfed:

   The Solaris 10+ assembler tags objects with the instruction set
   extensions used, so SSE4.2 executables cannot run on machines that
   don't support that extension.  */

#if (GCC_VERSION >= 4005) && (__GNUC__ >= 5 || !defined(__PIC__)) && (defined(__i386__) || defined(__x86_64__)) && !(defined(__sun__) && defined(__svr4__))

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
      t  = data == repl_nl;
      t |= data == repl_cr;
      t |= data == repl_bs;
      t |= data == repl_qm;
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
      s = (const uchar *)((si + 15) & -16);
    }

  /* Main loop, processing 16 bytes at a time.  */
#ifdef __GCC_ASM_FLAG_OUTPUTS__
  while (1)
    {
      char f;

      /* By using inline assembly instead of the builtin,
	 we can use the result, as well as the flags set.  */
      __asm ("%vpcmpestri\t$0, %2, %3"
	     : "=c"(index), "=@ccc"(f)
	     : "m"(*s), "x"(search), "a"(4), "d"(16));
      if (f)
	break;
      
      s += 16;
    }
#else
  s -= 16;
  /* By doing the whole loop in inline assembly,
     we can make proper use of the flags set.  */
  __asm (      ".balign 16\n"
	"0:	add $16, %1\n"
	"	%vpcmpestri\t$0, (%1), %2\n"
	"	jnc 0b"
	: "=&c"(index), "+r"(s)
	: "x"(search), "a"(4), "d"(16));
#endif

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

#elif (GCC_VERSION >= 4005) && defined(_ARCH_PWR8) && defined(__ALTIVEC__)

/* A vection of the fast scanner using AltiVec vectorized byte compares
   and VSX unaligned loads (when VSX is available).  This is otherwise
   the same as the AltiVec version.  */

ATTRIBUTE_NO_SANITIZE_UNDEFINED
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
  const vc zero = { 0 };

  vc data, t;

  /* Main loop processing 16 bytes at a time.  */
  do
    {
      vc m_nl, m_cr, m_bs, m_qm;

      data = __builtin_vec_vsx_ld (0, s);
      s += 16;

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

  /* Restore s to to point to the 16 bytes we just processed.  */
  s -= 16;

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
	/* FALLTHRU */
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

#elif (GCC_VERSION >= 4005) && defined(__ALTIVEC__) && defined (__BIG_ENDIAN__)

/* A vection of the fast scanner using AltiVec vectorized byte compares.
   This cannot be used for little endian because vec_lvsl/lvsr are
   deprecated for little endian and the code won't work properly.  */
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
  mask = __builtin_vec_lvsr(0, s);
  mask = __builtin_vec_perm(zero, ones, mask);
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
	/* FALLTHROUGH */
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
    l = __builtin_clzl(l) >> 3;
    return s + l;

#undef N
  }
}

#elif defined (__ARM_NEON) && defined (__ARM_64BIT_STATE)
#include "arm_neon.h"

/* This doesn't have to be the exact page size, but no system may use
   a size smaller than this.  ARMv8 requires a minimum page size of
   4k.  The impact of being conservative here is a small number of
   cases will take the slightly slower entry path into the main
   loop.  */

#define AARCH64_MIN_PAGE_SIZE 4096

static const uchar *
search_line_fast (const uchar *s, const uchar *end ATTRIBUTE_UNUSED)
{
  const uint8x16_t repl_nl = vdupq_n_u8 ('\n');
  const uint8x16_t repl_cr = vdupq_n_u8 ('\r');
  const uint8x16_t repl_bs = vdupq_n_u8 ('\\');
  const uint8x16_t repl_qm = vdupq_n_u8 ('?');
  const uint8x16_t xmask = (uint8x16_t) vdupq_n_u64 (0x8040201008040201ULL);

#ifdef __ARM_BIG_ENDIAN
  const int16x8_t shift = {8, 8, 8, 8, 0, 0, 0, 0};
#else
  const int16x8_t shift = {0, 0, 0, 0, 8, 8, 8, 8};
#endif

  unsigned int found;
  const uint8_t *p;
  uint8x16_t data;
  uint8x16_t t;
  uint16x8_t m;
  uint8x16_t u, v, w;

  /* Align the source pointer.  */
  p = (const uint8_t *)((uintptr_t)s & -16);

  /* Assuming random string start positions, with a 4k page size we'll take
     the slow path about 0.37% of the time.  */
  if (__builtin_expect ((AARCH64_MIN_PAGE_SIZE
			 - (((uintptr_t) s) & (AARCH64_MIN_PAGE_SIZE - 1)))
			< 16, 0))
    {
      /* Slow path: the string starts near a possible page boundary.  */
      uint32_t misalign, mask;

      misalign = (uintptr_t)s & 15;
      mask = (-1u << misalign) & 0xffff;
      data = vld1q_u8 (p);
      t = vceqq_u8 (data, repl_nl);
      u = vceqq_u8 (data, repl_cr);
      v = vorrq_u8 (t, vceqq_u8 (data, repl_bs));
      w = vorrq_u8 (u, vceqq_u8 (data, repl_qm));
      t = vorrq_u8 (v, w);
      t = vandq_u8 (t, xmask);
      m = vpaddlq_u8 (t);
      m = vshlq_u16 (m, shift);
      found = vaddvq_u16 (m);
      found &= mask;
      if (found)
	return (const uchar*)p + __builtin_ctz (found);
    }
  else
    {
      data = vld1q_u8 ((const uint8_t *) s);
      t = vceqq_u8 (data, repl_nl);
      u = vceqq_u8 (data, repl_cr);
      v = vorrq_u8 (t, vceqq_u8 (data, repl_bs));
      w = vorrq_u8 (u, vceqq_u8 (data, repl_qm));
      t = vorrq_u8 (v, w);
      if (__builtin_expect (vpaddd_u64 ((uint64x2_t)t) != 0, 0))
	goto done;
    }

  do
    {
      p += 16;
      data = vld1q_u8 (p);
      t = vceqq_u8 (data, repl_nl);
      u = vceqq_u8 (data, repl_cr);
      v = vorrq_u8 (t, vceqq_u8 (data, repl_bs));
      w = vorrq_u8 (u, vceqq_u8 (data, repl_qm));
      t = vorrq_u8 (v, w);
    } while (!vpaddd_u64 ((uint64x2_t)t));

done:
  /* Now that we've found the terminating substring, work out precisely where
     we need to stop.  */
  t = vandq_u8 (t, xmask);
  m = vpaddlq_u8 (t);
  m = vshlq_u16 (m, shift);
  found = vaddvq_u16 (m);
  return (((((uintptr_t) p) < (uintptr_t) s) ? s : (const uchar *)p)
	  + __builtin_ctz (found));
}

#elif defined (__ARM_NEON)
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

/* We only have one accelerated alternative.  Use a direct call so that
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
      if (*s == '\r' && s + 1 != buffer->rlimit && s[1] == '\n')
	s++;
    }

 done:
  *d = '\n';
  /* A sentinel note that should never be processed.  */
  add_line_note (buffer, d + 1, '\n');
  buffer->next_line = s + 1;
}

template <bool lexing_raw_string>
static bool get_fresh_line_impl (cpp_reader *pfile);

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

namespace bidi {
  enum class kind {
    NONE, LRE, RLE, LRO, RLO, LRI, RLI, FSI, PDF, PDI, LTR, RTL
  };

  /* All the UTF-8 encodings of bidi characters start with E2.  */
  constexpr uchar utf8_start = 0xe2;

  struct context
  {
    context () {}
    context (location_t loc, kind k, bool pdf, bool ucn)
    : m_loc (loc), m_kind (k), m_pdf (pdf), m_ucn (ucn)
    {
    }

    kind get_pop_kind () const
    {
      return m_pdf ? kind::PDF : kind::PDI;
    }
    bool ucn_p () const
    {
      return m_ucn;
    }

    location_t m_loc;
    kind m_kind;
    unsigned m_pdf : 1;
    unsigned m_ucn : 1;
  };

  /* A vector holding currently open bidi contexts.  We use a char for
     each context, its LSB is 1 if it represents a PDF context, 0 if it
     represents a PDI context.  The next bit is 1 if this context was open
     by a bidi character written as a UCN, and 0 when it was UTF-8.  */
  semi_embedded_vec <context, 16> vec;

  /* Close the whole comment/identifier/string literal/character constant
     context.  */
  void on_close ()
  {
    vec.truncate (0);
  }

  /* Pop the last element in the vector.  */
  void pop ()
  {
    unsigned int len = vec.count ();
    gcc_checking_assert (len > 0);
    vec.truncate (len - 1);
  }

  /* Return the pop kind of the context of the Ith element.  */
  kind pop_kind_at (unsigned int i)
  {
    return vec[i].get_pop_kind ();
  }

  /* Return the pop kind of the context that is currently opened.  */
  kind current_ctx ()
  {
    unsigned int len = vec.count ();
    if (len == 0)
      return kind::NONE;
    return vec[len - 1].get_pop_kind ();
  }

  /* Return true if the current context comes from a UCN origin, that is,
     the bidi char which started this bidi context was written as a UCN.  */
  bool current_ctx_ucn_p ()
  {
    unsigned int len = vec.count ();
    gcc_checking_assert (len > 0);
    return vec[len - 1].m_ucn;
  }

  location_t current_ctx_loc ()
  {
    unsigned int len = vec.count ();
    gcc_checking_assert (len > 0);
    return vec[len - 1].m_loc;
  }

  /* We've read a bidi char, update the current vector as necessary.
     LOC is only valid when K is not kind::NONE.  */
  void on_char (kind k, bool ucn_p, location_t loc)
  {
    switch (k)
      {
      case kind::LRE:
      case kind::RLE:
      case kind::LRO:
      case kind::RLO:
	vec.push (context (loc, k, true, ucn_p));
	break;
      case kind::LRI:
      case kind::RLI:
      case kind::FSI:
	vec.push (context (loc, k, false, ucn_p));
	break;
      /* PDF terminates the scope of the last LRE, RLE, LRO, or RLO
	 whose scope has not yet been terminated.  */
      case kind::PDF:
	if (current_ctx () == kind::PDF)
	  pop ();
	break;
      /* PDI terminates the scope of the last LRI, RLI, or FSI whose
	 scope has not yet been terminated, as well as the scopes of
	 any subsequent LREs, RLEs, LROs, or RLOs whose scopes have not
	 yet been terminated.  */
      case kind::PDI:
	for (int i = vec.count () - 1; i >= 0; --i)
	  if (pop_kind_at (i) == kind::PDI)
	    {
	      vec.truncate (i);
	      break;
	    }
	break;
      case kind::LTR:
      case kind::RTL:
	/* These aren't popped by a PDF/PDI.  */
	break;
      ATTR_LIKELY case kind::NONE:
	break;
      default:
	abort ();
      }
  }

  /* Return a descriptive string for K.  */
  const char *to_str (kind k)
  {
    switch (k)
      {
      case kind::LRE:
	return "U+202A (LEFT-TO-RIGHT EMBEDDING)";
      case kind::RLE:
	return "U+202B (RIGHT-TO-LEFT EMBEDDING)";
      case kind::LRO:
	return "U+202D (LEFT-TO-RIGHT OVERRIDE)";
      case kind::RLO:
	return "U+202E (RIGHT-TO-LEFT OVERRIDE)";
      case kind::LRI:
	return "U+2066 (LEFT-TO-RIGHT ISOLATE)";
      case kind::RLI:
	return "U+2067 (RIGHT-TO-LEFT ISOLATE)";
      case kind::FSI:
	return "U+2068 (FIRST STRONG ISOLATE)";
      case kind::PDF:
	return "U+202C (POP DIRECTIONAL FORMATTING)";
      case kind::PDI:
	return "U+2069 (POP DIRECTIONAL ISOLATE)";
      case kind::LTR:
	return "U+200E (LEFT-TO-RIGHT MARK)";
      case kind::RTL:
	return "U+200F (RIGHT-TO-LEFT MARK)";
      default:
	abort ();
      }
  }
}

/* Get location_t for the range of bytes [START, START + NUM_BYTES)
   within the current line in FILE, with the caret at START.  */

static location_t
get_location_for_byte_range_in_cur_line (cpp_reader *pfile,
					 const unsigned char *const start,
					 size_t num_bytes)
{
  gcc_checking_assert (num_bytes > 0);

  /* CPP_BUF_COLUMN and linemap_position_for_column both refer
     to offsets in bytes, but CPP_BUF_COLUMN is 0-based,
     whereas linemap_position_for_column is 1-based.  */

  /* Get 0-based offsets within the line.  */
  size_t start_offset = CPP_BUF_COLUMN (pfile->buffer, start);
  size_t end_offset = start_offset + num_bytes - 1;

  /* Now convert to location_t, where "columns" are 1-based byte offsets.  */
  location_t start_loc = linemap_position_for_column (pfile->line_table,
						      start_offset + 1);
  location_t end_loc = linemap_position_for_column (pfile->line_table,
						     end_offset + 1);

  if (start_loc == end_loc)
    return start_loc;

  source_range src_range;
  src_range.m_start = start_loc;
  src_range.m_finish = end_loc;
  location_t combined_loc = COMBINE_LOCATION_DATA (pfile->line_table,
						   start_loc,
						   src_range,
						   NULL,
						   0);
  return combined_loc;
}

/* Parse a sequence of 3 bytes starting with P and return its bidi code.  */

static bidi::kind
get_bidi_utf8_1 (const unsigned char *const p)
{
  gcc_checking_assert (p[0] == bidi::utf8_start);

  if (p[1] == 0x80)
    switch (p[2])
      {
      case 0xaa:
	return bidi::kind::LRE;
      case 0xab:
	return bidi::kind::RLE;
      case 0xac:
	return bidi::kind::PDF;
      case 0xad:
	return bidi::kind::LRO;
      case 0xae:
	return bidi::kind::RLO;
      case 0x8e:
	return bidi::kind::LTR;
      case 0x8f:
	return bidi::kind::RTL;
      default:
	break;
      }
  else if (p[1] == 0x81)
    switch (p[2])
      {
      case 0xa6:
	return bidi::kind::LRI;
      case 0xa7:
	return bidi::kind::RLI;
      case 0xa8:
	return bidi::kind::FSI;
      case 0xa9:
	return bidi::kind::PDI;
      default:
	break;
      }

  return bidi::kind::NONE;
}

/* Parse a sequence of 3 bytes starting with P and return its bidi code.
   If the kind is not NONE, write the location to *OUT.*/

static bidi::kind
get_bidi_utf8 (cpp_reader *pfile, const unsigned char *const p, location_t *out)
{
  bidi::kind result = get_bidi_utf8_1 (p);
  if (result != bidi::kind::NONE)
    {
      /* We have a sequence of 3 bytes starting at P.  */
      *out = get_location_for_byte_range_in_cur_line (pfile, p, 3);
    }
  return result;
}

/* Parse a UCN where P points just past \u or \U and return its bidi code.  */

static bidi::kind
get_bidi_ucn_1 (const unsigned char *p, bool is_U, const unsigned char **end)
{
  /* 6.4.3 Universal Character Names
      \u hex-quad
      \U hex-quad hex-quad
      \u { simple-hexadecimal-digit-sequence }
     where \unnnn means \U0000nnnn.  */

  *end = p + 4;
  if (is_U)
    {
      if (p[0] != '0' || p[1] != '0' || p[2] != '0' || p[3] != '0')
	return bidi::kind::NONE;
      /* Skip 4B so we can treat \u and \U the same below.  */
      p += 4;
      *end += 4;
    }
  else if (p[0] == '{')
    {
      p++;
      while (*p == '0')
	p++;
      if (p[0] != '2'
	  || p[1] != '0'
	  || !ISXDIGIT (p[2])
	  || !ISXDIGIT (p[3])
	  || p[4] != '}')
	return bidi::kind::NONE;
      *end = p + 5;
    }

  /* All code points we are looking for start with 20xx.  */
  if (p[0] != '2' || p[1] != '0')
    return bidi::kind::NONE;
  else if (p[2] == '2')
    switch (p[3])
      {
      case 'a':
      case 'A':
	return bidi::kind::LRE;
      case 'b':
      case 'B':
	return bidi::kind::RLE;
      case 'c':
      case 'C':
	return bidi::kind::PDF;
      case 'd':
      case 'D':
	return bidi::kind::LRO;
      case 'e':
      case 'E':
	return bidi::kind::RLO;
      default:
	break;
      }
  else if (p[2] == '6')
    switch (p[3])
      {
      case '6':
	return bidi::kind::LRI;
      case '7':
	return bidi::kind::RLI;
      case '8':
	return bidi::kind::FSI;
      case '9':
	return bidi::kind::PDI;
      default:
	break;
      }
  else if (p[2] == '0')
    switch (p[3])
      {
      case 'e':
      case 'E':
	return bidi::kind::LTR;
      case 'f':
      case 'F':
	return bidi::kind::RTL;
      default:
	break;
      }

  return bidi::kind::NONE;
}

/* Parse a UCN where P points just past \u or \U and return its bidi code.
   If the kind is not NONE, write the location to *OUT.  */

static bidi::kind
get_bidi_ucn (cpp_reader *pfile, const unsigned char *p, bool is_U,
	      location_t *out)
{
  const unsigned char *end;
  bidi::kind result = get_bidi_ucn_1 (p, is_U, &end);
  if (result != bidi::kind::NONE)
    {
      const unsigned char *start = p - 2;
      size_t num_bytes = end - start;
      *out = get_location_for_byte_range_in_cur_line (pfile, start, num_bytes);
    }
  return result;
}

/* Parse a named universal character escape where P points just past \N and
   return its bidi code.  If the kind is not NONE, write the location to
   *OUT.  */

static bidi::kind
get_bidi_named (cpp_reader *pfile, const unsigned char *p, location_t *out)
{
  bidi::kind result = bidi::kind::NONE;
  if (*p != '{')
    return bidi::kind::NONE;
  if (strncmp ((const char *) (p + 1), "LEFT-TO-RIGHT ", 14) == 0)
    {
      if (strncmp ((const char *) (p + 15), "MARK}", 5) == 0)
	result = bidi::kind::LTR;
      else if (strncmp ((const char *) (p + 15), "EMBEDDING}", 10) == 0)
	result = bidi::kind::LRE;
      else if (strncmp ((const char *) (p + 15), "OVERRIDE}", 9) == 0)
	result = bidi::kind::LRO;
      else if (strncmp ((const char *) (p + 15), "ISOLATE}", 8) == 0)
	result = bidi::kind::LRI;
    }
  else if (strncmp ((const char *) (p + 1), "RIGHT-TO-LEFT ", 14) == 0)
    {
      if (strncmp ((const char *) (p + 15), "MARK}", 5) == 0)
	result = bidi::kind::RTL;
      else if (strncmp ((const char *) (p + 15), "EMBEDDING}", 10) == 0)
	result = bidi::kind::RLE;
      else if (strncmp ((const char *) (p + 15), "OVERRIDE}", 9) == 0)
	result = bidi::kind::RLO;
      else if (strncmp ((const char *) (p + 15), "ISOLATE}", 8) == 0)
	result = bidi::kind::RLI;
    }
  else if (strncmp ((const char *) (p + 1), "POP DIRECTIONAL ", 16) == 0)
    {
      if (strncmp ((const char *) (p + 16), "FORMATTING}", 11) == 0)
	result = bidi::kind::PDF;
      else if (strncmp ((const char *) (p + 16), "ISOLATE}", 8) == 0)
	result = bidi::kind::PDI;
    }
  else if (strncmp ((const char *) (p + 1), "FIRST STRONG ISOLATE}", 21) == 0)
    result = bidi::kind::FSI;
  if (result != bidi::kind::NONE)
    *out = get_location_for_byte_range_in_cur_line (pfile, p - 2,
						    (strchr ((const char *)
							     (p + 1), '}')
						     - (const char *) p)
						    + 3);
  return result;
}

/* Subclass of rich_location for reporting on unpaired UTF-8
   bidirectional control character(s).
   Escape the source lines on output, and show all unclosed
   bidi context, labelling everything.  */

class unpaired_bidi_rich_location : public rich_location
{
 public:
  class custom_range_label : public range_label
  {
   public:
     label_text get_text (unsigned range_idx) const final override
     {
       /* range 0 is the primary location; each subsequent range i + 1
	  is for bidi::vec[i].  */
       if (range_idx > 0)
	 {
	   const bidi::context &ctxt (bidi::vec[range_idx - 1]);
	   return label_text::borrow (bidi::to_str (ctxt.m_kind));
	 }
       else
	 return label_text::borrow (_("end of bidirectional context"));
     }
  };

  unpaired_bidi_rich_location (cpp_reader *pfile, location_t loc)
  : rich_location (pfile->line_table, loc, &m_custom_label)
  {
    set_escape_on_output (true);
    for (unsigned i = 0; i < bidi::vec.count (); i++)
      add_range (bidi::vec[i].m_loc,
		 SHOW_RANGE_WITHOUT_CARET,
		 &m_custom_label);
  }

 private:
   custom_range_label m_custom_label;
};

/* We're closing a bidi context, that is, we've encountered a newline,
   are closing a C-style comment, or are at the end of a string literal,
   character constant, or identifier.  Warn if this context was not
   properly terminated by a PDI or PDF.  P points to the last character
   in this context.  */

static void
maybe_warn_bidi_on_close (cpp_reader *pfile, const uchar *p)
{
  const auto warn_bidi = CPP_OPTION (pfile, cpp_warn_bidirectional);
  if (bidi::vec.count () > 0
      && (warn_bidi & bidirectional_unpaired
	  && (!bidi::current_ctx_ucn_p ()
	      || (warn_bidi & bidirectional_ucn))))
    {
      const location_t loc
	= linemap_position_for_column (pfile->line_table,
				       CPP_BUF_COLUMN (pfile->buffer, p));
      unpaired_bidi_rich_location rich_loc (pfile, loc);
      /* cpp_callbacks doesn't yet have a way to handle singular vs plural
	 forms of a diagnostic, so fake it for now.  */
      if (bidi::vec.count () > 1)
	cpp_warning_at (pfile, CPP_W_BIDIRECTIONAL, &rich_loc,
			"unpaired UTF-8 bidirectional control characters "
			"detected");
      else
	cpp_warning_at (pfile, CPP_W_BIDIRECTIONAL, &rich_loc,
			"unpaired UTF-8 bidirectional control character "
			"detected");
    }
  /* We're done with this context.  */
  bidi::on_close ();
}

/* We're at the beginning or in the middle of an identifier/comment/string
   literal/character constant.  Warn if we've encountered a bidi character.
   KIND says which bidi control character it was; UCN_P is true iff this bidi
   control character was written as a UCN.  LOC is the location of the
   character, but is only valid if KIND != bidi::kind::NONE.  */

static void
maybe_warn_bidi_on_char (cpp_reader *pfile, bidi::kind kind,
			 bool ucn_p, location_t loc)
{
  if (__builtin_expect (kind == bidi::kind::NONE, 1))
    return;

  const auto warn_bidi = CPP_OPTION (pfile, cpp_warn_bidirectional);

  if (warn_bidi & (bidirectional_unpaired|bidirectional_any))
    {
      rich_location rich_loc (pfile->line_table, loc);
      rich_loc.set_escape_on_output (true);

      /* It seems excessive to warn about a PDI/PDF that is closing
	 an opened context because we've already warned about the
	 opening character.  Except warn when we have a UCN x UTF-8
	 mismatch, if UCN checking is enabled.  */
      if (kind == bidi::current_ctx ())
	{
	  if (warn_bidi == (bidirectional_unpaired|bidirectional_ucn)
	      && bidi::current_ctx_ucn_p () != ucn_p)
	    {
	      rich_loc.add_range (bidi::current_ctx_loc ());
	      cpp_warning_at (pfile, CPP_W_BIDIRECTIONAL, &rich_loc,
			      "UTF-8 vs UCN mismatch when closing "
			      "a context by \"%s\"", bidi::to_str (kind));
	    }
	}
      else if (warn_bidi & bidirectional_any
	       && (!ucn_p || (warn_bidi & bidirectional_ucn)))
	{
	  if (kind == bidi::kind::PDF || kind == bidi::kind::PDI)
	    cpp_warning_at (pfile, CPP_W_BIDIRECTIONAL, &rich_loc,
			    "\"%s\" is closing an unopened context",
			    bidi::to_str (kind));
	  else
	    cpp_warning_at (pfile, CPP_W_BIDIRECTIONAL, &rich_loc,
			    "found problematic Unicode character \"%s\"",
			    bidi::to_str (kind));
	}
    }
  /* We're done with this context.  */
  bidi::on_char (kind, ucn_p, loc);
}

static const cppchar_t utf8_continuation = 0x80;
static const cppchar_t utf8_signifier = 0xC0;

/* Emit -Winvalid-utf8 warning on invalid UTF-8 character starting
   at PFILE->buffer->cur.  Return a pointer after the diagnosed
   invalid character.  */

static const uchar *
_cpp_warn_invalid_utf8 (cpp_reader *pfile)
{
  cpp_buffer *buffer = pfile->buffer;
  const uchar *cur = buffer->cur;
  bool pedantic = (CPP_PEDANTIC (pfile)
		   && CPP_OPTION (pfile, cpp_warn_invalid_utf8) == 2);

  if (cur[0] < utf8_signifier
      || cur[1] < utf8_continuation || cur[1] >= utf8_signifier)
    {
      if (pedantic)
	cpp_error_with_line (pfile, CPP_DL_PEDWARN,
			     pfile->line_table->highest_line,
			     CPP_BUF_COL (buffer),
			     "invalid UTF-8 character <%x>",
			     cur[0]);
      else
	cpp_warning_with_line (pfile, CPP_W_INVALID_UTF8,
			       pfile->line_table->highest_line,
			       CPP_BUF_COL (buffer),
			       "invalid UTF-8 character <%x>",
			       cur[0]);
      return cur + 1;
    }
  else if (cur[2] < utf8_continuation || cur[2] >= utf8_signifier)
    {
      if (pedantic)
	cpp_error_with_line (pfile, CPP_DL_PEDWARN,
			     pfile->line_table->highest_line,
			     CPP_BUF_COL (buffer),
			     "invalid UTF-8 character <%x><%x>",
			     cur[0], cur[1]);
      else
	cpp_warning_with_line (pfile, CPP_W_INVALID_UTF8,
			       pfile->line_table->highest_line,
			       CPP_BUF_COL (buffer),
			       "invalid UTF-8 character <%x><%x>",
			       cur[0], cur[1]);
      return cur + 2;
    }
  else if (cur[3] < utf8_continuation || cur[3] >= utf8_signifier)
    {
      if (pedantic)
	cpp_error_with_line (pfile, CPP_DL_PEDWARN,
			     pfile->line_table->highest_line,
			     CPP_BUF_COL (buffer),
			     "invalid UTF-8 character <%x><%x><%x>",
			     cur[0], cur[1], cur[2]);
      else
	cpp_warning_with_line (pfile, CPP_W_INVALID_UTF8,
			       pfile->line_table->highest_line,
			       CPP_BUF_COL (buffer),
			       "invalid UTF-8 character <%x><%x><%x>",
			       cur[0], cur[1], cur[2]);
      return cur + 3;
    }
  else
    {
      if (pedantic)
	cpp_error_with_line (pfile, CPP_DL_PEDWARN,
			     pfile->line_table->highest_line,
			     CPP_BUF_COL (buffer),
			     "invalid UTF-8 character <%x><%x><%x><%x>",
			     cur[0], cur[1], cur[2], cur[3]);
      else
	cpp_warning_with_line (pfile, CPP_W_INVALID_UTF8,
			       pfile->line_table->highest_line,
			       CPP_BUF_COL (buffer),
			       "invalid UTF-8 character <%x><%x><%x><%x>",
			       cur[0], cur[1], cur[2], cur[3]);
      return cur + 4;
    }
}

/* Helper function of *skip_*_comment and lex*_string.  For C,
   character at CUR[-1] with MSB set handle -Wbidi-chars* and
   -Winvalid-utf8 diagnostics and return pointer to first character
   that should be processed next.  */

static inline const uchar *
_cpp_handle_multibyte_utf8 (cpp_reader *pfile, uchar c,
			    const uchar *cur, bool warn_bidi_p,
			    bool warn_invalid_utf8_p)
{
  /* If this is a beginning of a UTF-8 encoding, it might be
     a bidirectional control character.  */
  if (c == bidi::utf8_start && warn_bidi_p)
    {
      location_t loc;
      bidi::kind kind = get_bidi_utf8 (pfile, cur - 1, &loc);
      maybe_warn_bidi_on_char (pfile, kind, /*ucn_p=*/false, loc);
    }
  if (!warn_invalid_utf8_p)
    return cur;
  if (c >= utf8_signifier)
    {
      cppchar_t s;
      const uchar *pstr = cur - 1;
      if (_cpp_valid_utf8 (pfile, &pstr, pfile->buffer->rlimit, 0, NULL, &s)
	  && s <= UCS_LIMIT)
	return pstr;
    }
  pfile->buffer->cur = cur - 1;
  return _cpp_warn_invalid_utf8 (pfile);
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
  const bool warn_bidi_p = pfile->warn_bidi_p ();
  const bool warn_invalid_utf8_p = CPP_OPTION (pfile, cpp_warn_invalid_utf8);
  const bool warn_bidi_or_invalid_utf8_p = warn_bidi_p | warn_invalid_utf8_p;

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
	    {
	      if (warn_bidi_p)
		maybe_warn_bidi_on_close (pfile, cur);
	      break;
	    }

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
	  if (warn_bidi_p)
	    maybe_warn_bidi_on_close (pfile, cur);
	  _cpp_process_line_notes (pfile, true);
	  if (buffer->next_line >= buffer->rlimit)
	    return true;
	  _cpp_clean_line (pfile);

	  cols = buffer->next_line - buffer->line_base;
	  CPP_INCREMENT_LINE (pfile, cols);

	  cur = buffer->cur;
	}
      else if (__builtin_expect (c >= utf8_continuation, 0)
	       && warn_bidi_or_invalid_utf8_p)
	cur = _cpp_handle_multibyte_utf8 (pfile, c, cur, warn_bidi_p,
					  warn_invalid_utf8_p);
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
  location_t orig_line = pfile->line_table->highest_line;
  const bool warn_bidi_p = pfile->warn_bidi_p ();
  const bool warn_invalid_utf8_p = CPP_OPTION (pfile, cpp_warn_invalid_utf8);
  const bool warn_bidi_or_invalid_utf8_p = warn_bidi_p | warn_invalid_utf8_p;

  if (!warn_bidi_or_invalid_utf8_p)
    while (*buffer->cur != '\n')
      buffer->cur++;
  else if (!warn_invalid_utf8_p)
    {
      while (*buffer->cur != '\n'
	     && *buffer->cur != bidi::utf8_start)
	buffer->cur++;
      if (__builtin_expect (*buffer->cur == bidi::utf8_start, 0))
	{
	  while (*buffer->cur != '\n')
	    {
	      if (__builtin_expect (*buffer->cur == bidi::utf8_start, 0))
		{
		  location_t loc;
		  bidi::kind kind = get_bidi_utf8 (pfile, buffer->cur, &loc);
		  maybe_warn_bidi_on_char (pfile, kind, /*ucn_p=*/false, loc);
		}
	      buffer->cur++;
	    }
	  maybe_warn_bidi_on_close (pfile, buffer->cur);
	}
    }
  else
    {
      while (*buffer->cur != '\n')
	{
	  if (*buffer->cur < utf8_continuation)
	    {
	      buffer->cur++;
	      continue;
	    }
	  buffer->cur
	    = _cpp_handle_multibyte_utf8 (pfile, *buffer->cur, buffer->cur + 1,
					  warn_bidi_p, warn_invalid_utf8_p);
	}
      if (warn_bidi_p)
	maybe_warn_bidi_on_close (pfile, buffer->cur);
    }

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
    {
      encoding_rich_location rich_loc (pfile);
      cpp_error_at (pfile, CPP_DL_WARNING, &rich_loc,
		    "null character(s) ignored");
    }

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
			  const struct normalize_state *s,
			  bool identifier)
{
  if (CPP_OPTION (pfile, warn_normalize) < NORMALIZE_STATE_RESULT (s)
      && !pfile->state.skipping)
    {
      location_t loc = token->src_loc;

      /* If possible, create a location range for the token.  */
      if (loc >= RESERVED_LOCATION_COUNT
	  && token->type != CPP_EOF
	  /* There must be no line notes to process.  */
	  && (!(pfile->buffer->cur
		>= pfile->buffer->notes[pfile->buffer->cur_note].pos
		&& !pfile->overlaid_buffer)))
	{
	  source_range tok_range;
	  tok_range.m_start = loc;
	  tok_range.m_finish
	    = linemap_position_for_column (pfile->line_table,
					   CPP_BUF_COLUMN (pfile->buffer,
							   pfile->buffer->cur));
	  loc = COMBINE_LOCATION_DATA (pfile->line_table,
				       loc, tok_range, NULL, 0);
	}

      encoding_rich_location rich_loc (pfile, loc);

      /* Make sure that the token is printed using UCNs, even
	 if we'd otherwise happily print UTF-8.  */
      unsigned char *buf = XNEWVEC (unsigned char, cpp_token_len (token));
      size_t sz;

      sz = cpp_spell_token (pfile, token, buf, false) - buf;
      if (NORMALIZE_STATE_RESULT (s) == normalized_C)
	cpp_warning_at (pfile, CPP_W_NORMALIZE, &rich_loc,
			"`%.*s' is not in NFKC", (int) sz, buf);
      else if (identifier && CPP_OPTION (pfile, xid_identifiers))
	cpp_pedwarning_at (pfile, CPP_W_NORMALIZE, &rich_loc,
				  "`%.*s' is not in NFC", (int) sz, buf);
      else
	cpp_warning_at (pfile, CPP_W_NORMALIZE, &rich_loc,
			"`%.*s' is not in NFC", (int) sz, buf);
      free (buf);
    }
}

/* Returns TRUE if the sequence starting at buffer->cur is valid in
   an identifier.  FIRST is TRUE if this starts an identifier.  */

static bool
forms_identifier_p (cpp_reader *pfile, int first,
		    struct normalize_state *state)
{
  cpp_buffer *buffer = pfile->buffer;
  const bool warn_bidi_p = pfile->warn_bidi_p ();

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

  /* Is this a syntactically valid UCN or a valid UTF-8 char?  */
  if (CPP_OPTION (pfile, extended_identifiers))
    {
      cppchar_t s;
      if (*buffer->cur >= utf8_signifier)
	{
	  if (__builtin_expect (*buffer->cur == bidi::utf8_start, 0)
	      && warn_bidi_p)
	    {
	      location_t loc;
	      bidi::kind kind = get_bidi_utf8 (pfile, buffer->cur, &loc);
	      maybe_warn_bidi_on_char (pfile, kind, /*ucn_p=*/false, loc);
	    }
	  if (_cpp_valid_utf8 (pfile, &buffer->cur, buffer->rlimit, 1 + !first,
			       state, &s))
	    return true;
	}
      else if (*buffer->cur == '\\'
	       && (buffer->cur[1] == 'u'
		   || buffer->cur[1] == 'U'
		   || buffer->cur[1] == 'N'))
	{
	  buffer->cur += 2;
	  if (warn_bidi_p)
	    {
	      location_t loc;
	      bidi::kind kind;
	      if (buffer->cur[-1] == 'N')
		kind = get_bidi_named (pfile, buffer->cur, &loc);
	      else
		kind = get_bidi_ucn (pfile, buffer->cur,
				     buffer->cur[-1] == 'U', &loc);
	      maybe_warn_bidi_on_char (pfile, kind, /*ucn_p=*/true, loc);
	    }
	  if (_cpp_valid_ucn (pfile, &buffer->cur, buffer->rlimit, 1 + !first,
			      state, &s, NULL, NULL))
	    return true;
	  buffer->cur -= 2;
	}
    }

  return false;
}

/* Helper function to issue error about improper __VA_OPT__ use.  */
static void
maybe_va_opt_error (cpp_reader *pfile)
{
  if (CPP_PEDANTIC (pfile) && !CPP_OPTION (pfile, va_opt))
    {
      /* __VA_OPT__ should not be accepted at all, but allow it in
	 system headers.  */
      if (!_cpp_in_system_header (pfile))
	{
	  if (CPP_OPTION (pfile, cplusplus))
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_OPT__ is not available until C++20");
	  else
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_OPT__ is not available until C2X");
	}
    }
  else if (!pfile->state.va_args_ok)
    {
      /* __VA_OPT__ should only appear in the replacement list of a
	 variadic macro.  */
      cpp_error (pfile, CPP_DL_PEDWARN,
		 "__VA_OPT__ can only appear in the expansion"
		 " of a C++20 variadic macro");
    }
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
	{
	  if (CPP_OPTION (pfile, cplusplus))
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_ARGS__ can only appear in the expansion"
		       " of a C++11 variadic macro");
	  else
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_ARGS__ can only appear in the expansion"
		       " of a C99 variadic macro");
	}

      if (result == pfile->spec_nodes.n__VA_OPT__)
	maybe_va_opt_error (pfile);

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
		struct normalize_state *nst, cpp_hashnode **spelling)
{
  cpp_hashnode *result;
  const uchar *cur;
  unsigned int len;
  unsigned int hash = HT_HASHSTEP (0, *base);
  const bool warn_bidi_p = pfile->warn_bidi_p ();

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
      /* Slower version for identifiers containing UCNs
	 or extended chars (including $).  */
      do {
	while (ISIDNUM (*pfile->buffer->cur))
	  {
	    NORMALIZE_STATE_UPDATE_IDNUM (nst, *pfile->buffer->cur);
	    pfile->buffer->cur++;
	  }
      } while (forms_identifier_p (pfile, false, nst));
      if (warn_bidi_p)
	maybe_warn_bidi_on_close (pfile, pfile->buffer->cur);
      result = _cpp_interpret_identifier (pfile, base,
					  pfile->buffer->cur - base);
      *spelling = cpp_lookup (pfile, base, pfile->buffer->cur - base);
    }
  else
    {
      len = cur - base;
      hash = HT_HASHFINISH (hash, len);

      result = CPP_HASHNODE (ht_lookup_with_hash (pfile->hash_table,
						  base, len, hash, HT_ALLOC));
      *spelling = result;
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
	{
	  if (CPP_OPTION (pfile, cplusplus))
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_ARGS__ can only appear in the expansion"
		       " of a C++11 variadic macro");
	  else
	    cpp_error (pfile, CPP_DL_PEDWARN,
		       "__VA_ARGS__ can only appear in the expansion"
		       " of a C99 variadic macro");
	}

      /* __VA_OPT__ should only appear in the replacement list of a
	 variadic macro.  */
      if (result == pfile->spec_nodes.n__VA_OPT__)
	maybe_va_opt_error (pfile);

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
      const uchar *adj_digit_sep = NULL;
      cur = pfile->buffer->cur;

      /* N.B. ISIDNUM does not include $.  */
      while (ISIDNUM (*cur)
	     || (*cur == '.' && !DIGIT_SEP (cur[-1]))
	     || DIGIT_SEP (*cur)
	     || (VALID_SIGN (*cur, cur[-1]) && !DIGIT_SEP (cur[-2])))
	{
	  NORMALIZE_STATE_UPDATE_IDNUM (nst, *cur);
	  /* Adjacent digit separators do not form part of the pp-number syntax.
	     However, they can safely be diagnosed here as an error, since '' is
	     not a valid preprocessing token.  */
	  if (DIGIT_SEP (*cur) && DIGIT_SEP (cur[-1]) && !adj_digit_sep)
	    adj_digit_sep = cur;
	  cur++;
	}
      /* A number can't end with a digit separator.  */
      while (cur > pfile->buffer->cur && DIGIT_SEP (cur[-1]))
	--cur;
      if (adj_digit_sep && adj_digit_sep < cur)
	cpp_error (pfile, CPP_DL_ERROR, "adjacent digit separators");

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
  token->type = type;
  token->val.str.len = len;
  token->val.str.text = cpp_alloc_token_string (pfile, base, len);
}

const uchar *
cpp_alloc_token_string (cpp_reader *pfile,
			const unsigned char *ptr, unsigned len)
{
  uchar *dest = _cpp_unaligned_alloc (pfile, len + 1);

  dest[len] = 0;
  memcpy (dest, ptr, len);
  return dest;
}

/* A pair of raw buffer pointers.  The currently open one is [1], the
   first one is [0].  Used for string literal lexing.  */
struct lit_accum {
  _cpp_buff *first;
  _cpp_buff *last;
  const uchar *rpos;
  size_t accum;

  lit_accum ()
    : first (NULL), last (NULL), rpos (0), accum (0)
  {
  }

  void append (cpp_reader *, const uchar *, size_t);

  void read_begin (cpp_reader *);
  bool reading_p () const
  {
    return rpos != NULL;
  }
  char read_char ()
  {
    char c = *rpos++;
    if (rpos == BUFF_FRONT (last))
      rpos = NULL;
    return c;
  }
};

/* Subroutine of lex_raw_string: Append LEN chars from BASE to the buffer
   sequence from *FIRST_BUFF_P to LAST_BUFF_P.  */

void
lit_accum::append (cpp_reader *pfile, const uchar *base, size_t len)
{
  if (!last)
    /* Starting.  */
    first = last = _cpp_get_buff (pfile, len);
  else if (len > BUFF_ROOM (last))
    {
      /* There is insufficient room in the buffer.  Copy what we can,
	 and then either extend or create a new one.  */
      size_t room = BUFF_ROOM (last);
      memcpy (BUFF_FRONT (last), base, room);
      BUFF_FRONT (last) += room;
      base += room;
      len -= room;
      accum += room;

      gcc_checking_assert (!rpos);

      last = _cpp_append_extend_buff (pfile, last, len);
    }

  memcpy (BUFF_FRONT (last), base, len);
  BUFF_FRONT (last) += len;
  accum += len;
}

void
lit_accum::read_begin (cpp_reader *pfile)
{
  /* We never accumulate more than 4 chars to read.  */
  if (BUFF_ROOM (last) < 4)

    last = _cpp_append_extend_buff (pfile, last, 4);
  rpos = BUFF_FRONT (last);
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

  return result && cpp_macro_p (result);
}

/* Returns true if a literal suffix does not have the expected form
   and is defined as a macro.  */

static bool
is_macro_not_literal_suffix(cpp_reader *pfile, const uchar *base)
{
  /* User-defined literals outside of namespace std must start with a single
     underscore, so assume anything of that form really is a UDL suffix.
     We don't need to worry about UDLs defined inside namespace std because
     their names are reserved, so cannot be used as macro names in valid
     programs.  */
  if (base[0] == '_' && base[1] != '_')
    return false;
  return is_macro (pfile, base);
}

/* Lexes a raw string.  The stored string contains the spelling,
   including double quotes, delimiter string, '(' and ')', any leading
   'L', 'u', 'U' or 'u8' and 'R' modifier.  The created token contains
   the type of the literal, or CPP_OTHER if it was not properly
   terminated.

   BASE is the start of the token.  Updates pfile->buffer->cur to just
   after the lexed string.

   The spelling is NUL-terminated, but it is not guaranteed that this
   is the first NUL since embedded NULs are preserved.  */

static void
lex_raw_string (cpp_reader *pfile, cpp_token *token, const uchar *base)
{
  const uchar *pos = base;
  const bool warn_bidi_p = pfile->warn_bidi_p ();
  const bool warn_invalid_utf8_p = CPP_OPTION (pfile, cpp_warn_invalid_utf8);
  const bool warn_bidi_or_invalid_utf8_p = warn_bidi_p | warn_invalid_utf8_p;

  /* 'tis a pity this information isn't passed down from the lexer's
     initial categorization of the token.  */
  enum cpp_ttype type = CPP_STRING;

  if (*pos == 'L')
    {
      type = CPP_WSTRING;
      pos++;
    }
  else if (*pos == 'U')
    {
      type = CPP_STRING32;
      pos++;
    }
  else if (*pos == 'u')
    {
      if (pos[1] == '8')
	{
	  type = CPP_UTF8STRING;
	  pos++;
	}
      else
	type = CPP_STRING16;
      pos++;
    }

  gcc_checking_assert (pos[0] == 'R' && pos[1] == '"');
  pos += 2;

  _cpp_line_note *note = &pfile->buffer->notes[pfile->buffer->cur_note];

  /* Skip notes before the ".  */
  while (note->pos < pos)
    ++note;

  lit_accum accum;
  
  uchar prefix[17];
  unsigned prefix_len = 0;
  enum Phase
  {
   PHASE_PREFIX = -2,
   PHASE_NONE = -1,
   PHASE_SUFFIX = 0
  } phase = PHASE_PREFIX;

  for (;;)
    {
      gcc_checking_assert (note->pos >= pos);

      /* Undo any escaped newlines and trigraphs.  */
      if (!accum.reading_p () && note->pos == pos)
	switch (note->type)
	  {
	  case '\\':
	  case ' ':
	    /* Restore backslash followed by newline.  */
	    accum.append (pfile, base, pos - base);
	    base = pos;
	    accum.read_begin (pfile);
	    accum.append (pfile, UC"\\", 1);

	  after_backslash:
	    if (note->type == ' ')
	      /* GNU backslash whitespace newline extension.  FIXME
		 could be any sequence of non-vertical space.  When we
		 can properly restore any such sequence, we should
		 mark this note as handled so _cpp_process_line_notes
		 doesn't warn.  */
	      accum.append (pfile, UC" ", 1);

	    accum.append (pfile, UC"\n", 1);
	    note++;
	    break;

	  case '\n':
	    /* This can happen for ??/<NEWLINE> when trigraphs are not
	       being interpretted.  */
	    gcc_checking_assert (!CPP_OPTION (pfile, trigraphs));
	    note->type = 0;
	    note++;
	    break;

	  default:
	    gcc_checking_assert (_cpp_trigraph_map[note->type]);

	    /* Don't warn about this trigraph in
	       _cpp_process_line_notes, since trigraphs show up as
	       trigraphs in raw strings.  */
	    uchar type = note->type;
	    note->type = 0;

	    if (CPP_OPTION (pfile, trigraphs))
	      {
		accum.append (pfile, base, pos - base);
		base = pos;
		accum.read_begin (pfile);
		accum.append (pfile, UC"??", 2);
		accum.append (pfile, &type, 1);

		/* ??/ followed by newline gets two line notes, one for
		   the trigraph and one for the backslash/newline.  */
		if (type == '/' && note[1].pos == pos)
		  {
		    note++;
		    gcc_assert (note->type == '\\' || note->type == ' ');
		    goto after_backslash;
		  }
		/* Skip the replacement character.  */
		base = ++pos;
	      }

	    note++;
	    break;
	  }

      /* Now get a char to process.  Either from an expanded note, or
	 from the line buffer.  */
      bool read_note = accum.reading_p ();
      char c = read_note ? accum.read_char () : *pos++;

      if (phase == PHASE_PREFIX)
	{
	  if (c == '(')
	    {
	      /* Done.  */
	      phase = PHASE_NONE;
	      prefix[prefix_len++] = '"';
	    }
	  else if (prefix_len < 16
		   /* Prefix chars are any of the basic character set,
		      [lex.charset] except for '
		      ()\\\t\v\f\n'. Optimized for a contiguous
		      alphabet.  */
		   /* Unlike a switch, this collapses down to one or
		      two shift and bitmask operations on an ASCII
		      system, with an outlier or two.   */
		   && (('Z' - 'A' == 25
			? ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
			: ISIDST (c))
		       || (c >= '0' && c <= '9')
		       || c == '_' || c == '{' || c == '}'
		       || c == '[' || c == ']' || c == '#'
		       || c == '<' || c == '>' || c == '%'
		       || c == ':' || c == ';' || c == '.' || c == '?'
		       || c == '*' || c == '+' || c == '-' || c == '/'
		       || c == '^' || c == '&' || c == '|' || c == '~'
		       || c == '!' || c == '=' || c == ','
		       || c == '"' || c == '\''))
	    prefix[prefix_len++] = c;
	  else
	    {
	      /* Something is wrong.  */
	      int col = CPP_BUF_COLUMN (pfile->buffer, pos) + read_note;
	      if (prefix_len == 16)
		cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
				     col, "raw string delimiter longer "
				     "than 16 characters");
	      else if (c == '\n')
		cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
				     col, "invalid new-line in raw "
				     "string delimiter");
	      else
		cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc,
				     col, "invalid character '%c' in "
				     "raw string delimiter", c);
	      type = CPP_OTHER;
	      phase = PHASE_NONE;
	      /* Continue until we get a close quote, that's probably
		 the best failure mode.  */
	      prefix_len = 0;
	    }
	  if (c != '\n')
	    continue;
	}

      if (phase != PHASE_NONE)
	{
	  if (prefix[phase] != c)
	    phase = PHASE_NONE;
	  else if (unsigned (phase + 1) == prefix_len)
	    break;
	  else
	    {
	      phase = Phase (phase + 1);
	      continue;
	    }
	}

      if (!prefix_len && c == '"')
	/* Failure mode lexing.  */
	goto out;
      else if (prefix_len && c == ')')
	phase = PHASE_SUFFIX;
      else if (!read_note && c == '\n')
	{
	  pos--;
	  pfile->buffer->cur = pos;
	  if ((pfile->state.in_directive || pfile->state.parsing_args)
	      && pfile->buffer->next_line >= pfile->buffer->rlimit)
	    {
	      cpp_error_with_line (pfile, CPP_DL_ERROR, token->src_loc, 0,
				   "unterminated raw string");
	      type = CPP_OTHER;
	      goto out;
	    }

	  accum.append (pfile, base, pos - base + 1);
	  _cpp_process_line_notes (pfile, false);

	  if (pfile->buffer->next_line < pfile->buffer->rlimit)
	    CPP_INCREMENT_LINE (pfile, 0);
	  pfile->buffer->need_line = true;

	  if (!get_fresh_line_impl<true> (pfile))
	    {
	      /* We ran out of file and failed to get a line.  */
	      location_t src_loc = token->src_loc;
	      token->type = CPP_EOF;
	      /* Tell the compiler the line number of the EOF token.  */
	      token->src_loc = pfile->line_table->highest_line;
	      token->flags = BOL;
	      if (accum.first)
		_cpp_release_buff (pfile, accum.first);
	      cpp_error_with_line (pfile, CPP_DL_ERROR, src_loc, 0,
				   "unterminated raw string");

	      /* Now pop the buffer that get_fresh_line_impl() did not.  Popping
		 is not safe if processing a directive, however this cannot
		 happen as we already checked above that a line would be
		 available, and get_fresh_line_impl() can't fail in this
		 case.  */
	      gcc_assert (!pfile->state.in_directive);
	      _cpp_pop_buffer (pfile);

	      return;
	    }

	  pos = base = pfile->buffer->cur;
	  note = &pfile->buffer->notes[pfile->buffer->cur_note];
	}
      else if (__builtin_expect ((unsigned char) c >= utf8_continuation, 0)
	       && warn_bidi_or_invalid_utf8_p)
	pos = _cpp_handle_multibyte_utf8 (pfile, c, pos, warn_bidi_p,
					  warn_invalid_utf8_p);
    }

  if (warn_bidi_p)
    maybe_warn_bidi_on_close (pfile, pos);

  if (CPP_OPTION (pfile, user_literals))
    {
      /* If a string format macro, say from inttypes.h, is placed touching
	 a string literal it could be parsed as a C++11 user-defined string
	 literal thus breaking the program.  */
      if (is_macro_not_literal_suffix (pfile, pos))
	{
	  /* Raise a warning, but do not consume subsequent tokens.  */
	  if (CPP_OPTION (pfile, warn_literal_suffix) && !pfile->state.skipping)
	    cpp_warning_with_line (pfile, CPP_W_LITERAL_SUFFIX,
				   token->src_loc, 0,
				   "invalid suffix on literal; C++11 requires "
				   "a space between literal and string macro");
	}
      /* Grab user defined literal suffix.  */
      else if (ISIDST (*pos))
	{
	  type = cpp_userdef_string_add_type (type);
	  ++pos;

	  while (ISIDNUM (*pos))
	    ++pos;
	}
    }

 out:
  pfile->buffer->cur = pos;
  if (!accum.accum)
    create_literal (pfile, token, base, pos - base, type);
  else
    {
      size_t extra_len = pos - base;
      uchar *dest = _cpp_unaligned_alloc (pfile, accum.accum + extra_len + 1);

      token->type = type;
      token->val.str.len = accum.accum + extra_len;
      token->val.str.text = dest;
      for (_cpp_buff *buf = accum.first; buf; buf = buf->next)
	{
	  size_t len = BUFF_FRONT (buf) - buf->base;
	  memcpy (dest, buf->base, len);
	  dest += len;
	}
      _cpp_release_buff (pfile, accum.first);
      memcpy (dest, base, extra_len);
      dest[extra_len] = '\0';
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
      lex_raw_string (pfile, token, base);
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
	    *base == 'u' ? (base[1] == '8' ? CPP_UTF8CHAR : CPP_CHAR16)
			 : CPP_CHAR);
  else
    terminator = '>', type = CPP_HEADER_NAME;

  const bool warn_bidi_p = pfile->warn_bidi_p ();
  const bool warn_invalid_utf8_p = CPP_OPTION (pfile, cpp_warn_invalid_utf8);
  const bool warn_bidi_or_invalid_utf8_p = warn_bidi_p | warn_invalid_utf8_p;
  for (;;)
    {
      cppchar_t c = *cur++;

      /* In #include-style directives, terminators are not escapable.  */
      if (c == '\\' && !pfile->state.angled_headers && *cur != '\n')
	{
	  if ((cur[0] == 'u' || cur[0] == 'U' || cur[0] == 'N') && warn_bidi_p)
	    {
	      location_t loc;
	      bidi::kind kind;
	      if (cur[0] == 'N')
		kind = get_bidi_named (pfile, cur + 1, &loc);
	      else
		kind = get_bidi_ucn (pfile, cur + 1, cur[0] == 'U', &loc);
	      maybe_warn_bidi_on_char (pfile, kind, /*ucn_p=*/true, loc);
	    }
	  cur++;
	}
      else if (c == terminator)
	{
	  if (warn_bidi_p)
	    maybe_warn_bidi_on_close (pfile, cur - 1);
	  break;
	}
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
      else if (__builtin_expect (c >= utf8_continuation, 0)
	       && warn_bidi_or_invalid_utf8_p)
	cur = _cpp_handle_multibyte_utf8 (pfile, c, cur, warn_bidi_p,
					  warn_invalid_utf8_p);
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
	 literal thus breaking the program.  */
      if (is_macro_not_literal_suffix (pfile, cur))
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
  else if (CPP_OPTION (pfile, cpp_warn_cxx11_compat)
	   && is_macro (pfile, cur)
	   && !pfile->state.skipping)
    cpp_warning_with_line (pfile, CPP_W_CXX11_COMPAT,
			   token->src_loc, 0, "C++11 requires a space "
			   "between string literal and macro");

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

/* Returns true if comment at COMMENT_START is a recognized FALLTHROUGH
   comment.  */

static bool
fallthrough_comment_p (cpp_reader *pfile, const unsigned char *comment_start)
{
  const unsigned char *from = comment_start + 1;

  switch (CPP_OPTION (pfile, cpp_warn_implicit_fallthrough))
    {
      /* For both -Wimplicit-fallthrough=0 and -Wimplicit-fallthrough=5 we
	 don't recognize any comments.  The latter only checks attributes,
	 the former doesn't warn.  */
    case 0:
    default:
      return false;
      /* -Wimplicit-fallthrough=1 considers any comment, no matter what
	 content it has.  */
    case 1:
      return true;
    case 2:
      /* -Wimplicit-fallthrough=2 looks for (case insensitive)
	 .*falls?[ \t-]*thr(u|ough).* regex.  */
      for (; (size_t) (pfile->buffer->cur - from) >= sizeof "fallthru" - 1;
	   from++)
	{
	  /* Is there anything like strpbrk with upper boundary, or
	     memchr looking for 2 characters rather than just one?  */
	  if (from[0] != 'f' && from[0] != 'F')
	    continue;
	  if (from[1] != 'a' && from[1] != 'A')
	    continue;
	  if (from[2] != 'l' && from[2] != 'L')
	    continue;
	  if (from[3] != 'l' && from[3] != 'L')
	    continue;
	  from += sizeof "fall" - 1;
	  if (from[0] == 's' || from[0] == 'S')
	    from++;
	  while (*from == ' ' || *from == '\t' || *from == '-')
	    from++;
	  if (from[0] != 't' && from[0] != 'T')
	    continue;
	  if (from[1] != 'h' && from[1] != 'H')
	    continue;
	  if (from[2] != 'r' && from[2] != 'R')
	    continue;
	  if (from[3] == 'u' || from[3] == 'U')
	    return true;
	  if (from[3] != 'o' && from[3] != 'O')
	    continue;
	  if (from[4] != 'u' && from[4] != 'U')
	    continue;
	  if (from[5] != 'g' && from[5] != 'G')
	    continue;
	  if (from[6] != 'h' && from[6] != 'H')
	    continue;
	  return true;
	}
      return false;
    case 3:
    case 4:
      break;
    }

  /* Whole comment contents:
     -fallthrough
     @fallthrough@
   */
  if (*from == '-' || *from == '@')
    {
      size_t len = sizeof "fallthrough" - 1;
      if ((size_t) (pfile->buffer->cur - from - 1) < len)
	return false;
      if (memcmp (from + 1, "fallthrough", len))
	return false;
      if (*from == '@')
	{
	  if (from[len + 1] != '@')
	    return false;
	  len++;
	}
      from += 1 + len;
    }
  /* Whole comment contents (regex):
     lint -fallthrough[ \t]*
   */
  else if (*from == 'l')
    {
      size_t len = sizeof "int -fallthrough" - 1;
      if ((size_t) (pfile->buffer->cur - from - 1) < len)
	return false;
      if (memcmp (from + 1, "int -fallthrough", len))
	return false;
      from += 1 + len;
      while (*from == ' ' || *from == '\t')
	from++;
    }
  /* Whole comment contents (regex):
     [ \t]*FALLTHR(U|OUGH)[ \t]*
   */
  else if (CPP_OPTION (pfile, cpp_warn_implicit_fallthrough) == 4)
    {
      while (*from == ' ' || *from == '\t')
	from++;
      if ((size_t) (pfile->buffer->cur - from)  < sizeof "FALLTHRU" - 1)
	return false;
      if (memcmp (from, "FALLTHR", sizeof "FALLTHR" - 1))
	return false;
      from += sizeof "FALLTHR" - 1;
      if (*from == 'U')
	from++;
      else if ((size_t) (pfile->buffer->cur - from)  < sizeof "OUGH" - 1)
	return false;
      else if (memcmp (from, "OUGH", sizeof "OUGH" - 1))
	return false;
      else
	from += sizeof "OUGH" - 1;
      while (*from == ' ' || *from == '\t')
	from++;
    }
  /* Whole comment contents (regex):
     [ \t.!]*(ELSE,? |INTENTIONAL(LY)? )?FALL(S | |-)?THR(OUGH|U)[ \t.!]*(-[^\n\r]*)?
     [ \t.!]*(Else,? |Intentional(ly)? )?Fall((s | |-)[Tt]|t)hr(ough|u)[ \t.!]*(-[^\n\r]*)?
     [ \t.!]*([Ee]lse,? |[Ii]ntentional(ly)? )?fall(s | |-)?thr(ough|u)[ \t.!]*(-[^\n\r]*)?
   */
  else
    {
      while (*from == ' ' || *from == '\t' || *from == '.' || *from == '!')
	from++;
      unsigned char f = *from;
      bool all_upper = false;
      if (f == 'E' || f == 'e')
	{
	  if ((size_t) (pfile->buffer->cur - from)
	      < sizeof "else fallthru" - 1)
	    return false;
	  if (f == 'E' && memcmp (from + 1, "LSE", sizeof "LSE" - 1) == 0)
	    all_upper = true;
	  else if (memcmp (from + 1, "lse", sizeof "lse" - 1))
	    return false;
	  from += sizeof "else" - 1;
	  if (*from == ',')
	    from++;
	  if (*from != ' ')
	    return false;
	  from++;
	  if (all_upper && *from == 'f')
	    return false;
	  if (f == 'e' && *from == 'F')
	    return false;
	  f = *from;
	}
      else if (f == 'I' || f == 'i')
	{
	  if ((size_t) (pfile->buffer->cur - from)
	      < sizeof "intentional fallthru" - 1)
	    return false;
	  if (f == 'I' && memcmp (from + 1, "NTENTIONAL",
				  sizeof "NTENTIONAL" - 1) == 0)
	    all_upper = true;
	  else if (memcmp (from + 1, "ntentional",
			   sizeof "ntentional" - 1))
	    return false;
	  from += sizeof "intentional" - 1;
	  if (*from == ' ')
	    {
	      from++;
	      if (all_upper && *from == 'f')
		return false;
	    }
	  else if (all_upper)
	    {
	      if (memcmp (from, "LY F", sizeof "LY F" - 1))
		return false;
	      from += sizeof "LY " - 1;
	    }
	  else
	    {
	      if (memcmp (from, "ly ", sizeof "ly " - 1))
		return false;
	      from += sizeof "ly " - 1;
	    }
	  if (f == 'i' && *from == 'F')
	    return false;
	  f = *from;
	}
      if (f != 'F' && f != 'f')
	return false;
      if ((size_t) (pfile->buffer->cur - from) < sizeof "fallthru" - 1)
	return false;
      if (f == 'F' && memcmp (from + 1, "ALL", sizeof "ALL" - 1) == 0)
	all_upper = true;
      else if (all_upper)
	return false;
      else if (memcmp (from + 1, "all", sizeof "all" - 1))
	return false;
      from += sizeof "fall" - 1;
      if (*from == (all_upper ? 'S' : 's') && from[1] == ' ')
	from += 2;
      else if (*from == ' ' || *from == '-')
	from++;
      else if (*from != (all_upper ? 'T' : 't'))
	return false;
      if ((f == 'f' || *from != 'T') && (all_upper || *from != 't'))
	return false;
      if ((size_t) (pfile->buffer->cur - from) < sizeof "thru" - 1)
	return false;
      if (memcmp (from + 1, all_upper ? "HRU" : "hru", sizeof "hru" - 1))
	{
	  if ((size_t) (pfile->buffer->cur - from) < sizeof "through" - 1)
	    return false;
	  if (memcmp (from + 1, all_upper ? "HROUGH" : "hrough",
		      sizeof "hrough" - 1))
	    return false;
	  from += sizeof "through" - 1;
	}
      else
	from += sizeof "thru" - 1;
      while (*from == ' ' || *from == '\t' || *from == '.' || *from == '!')
	from++;
      if (*from == '-')
	{
	  from++;
	  if (*comment_start == '*')
	    {
	      do
		{
		  while (*from && *from != '*'
			 && *from != '\n' && *from != '\r')
		    from++;
		  if (*from != '*' || from[1] == '/')
		    break;
		  from++;
		}
	      while (1);
	    }
	  else
	    while (*from && *from != '\n' && *from != '\r')
	      from++;
	}
    }
  /* C block comment.  */
  if (*comment_start == '*')
    {
      if (*from != '*' || from[1] != '/')
	return false;
    }
  /* C++ line comment.  */
  else if (*from != '\n')
    return false;

  return true;
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

  /* For peeked tokens temporarily disable line_change reporting,
     until the tokens are parsed for real.  */
  void (*line_change) (cpp_reader *, const cpp_token *, int)
    = pfile->cb.line_change;
  pfile->cb.line_change = NULL;

  do
    {
      peektok = _cpp_lex_token (pfile);
      if (peektok->type == CPP_EOF)
	{
	  index--;
	  break;
	}
      else if (peektok->type == CPP_PRAGMA)
	{
	  /* Don't peek past a pragma.  */
	  if (peektok == &pfile->directive_result)
	    /* Save the pragma in the buffer.  */
	    *pfile->cur_token++ = *peektok;
	  index--;
	  break;
	}
    }
  while (index--);

  _cpp_backup_tokens_direct (pfile, count - index);
  pfile->keep_tokens--;
  pfile->cb.line_change = line_change;

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

/* We're at the beginning of a logical line (so not in
  directives-mode) and RESULT is a CPP_NAME with NODE_MODULE set.  See
  if we should enter deferred_pragma mode to tokenize the rest of the
  line as a module control-line.  */

static void
cpp_maybe_module_directive (cpp_reader *pfile, cpp_token *result)
{
  unsigned backup = 0; /* Tokens we peeked.  */
  cpp_hashnode *node = result->val.node.node;
  cpp_token *peek = result;
  cpp_token *keyword = peek;
  cpp_hashnode *(&n_modules)[spec_nodes::M_HWM][2] = pfile->spec_nodes.n_modules;
  int header_count = 0;

  /* Make sure the incoming state is as we expect it.  This way we
     can restore it using constants.  */
  gcc_checking_assert (!pfile->state.in_deferred_pragma
		       && !pfile->state.skipping
		       && !pfile->state.parsing_args
		       && !pfile->state.angled_headers
		       && (pfile->state.save_comments
			   == !CPP_OPTION (pfile, discard_comments)));

  /* Enter directives mode sufficiently for peeking.  We don't have
     to actually set in_directive.  */
  pfile->state.in_deferred_pragma = true;

  /* These two fields are needed to process tokenization in deferred
     pragma mode.  They are not used outside deferred pragma mode or
     directives mode.  */
  pfile->state.pragma_allow_expansion = true;
  pfile->directive_line = result->src_loc;

  /* Saving comments is incompatible with directives mode.   */
  pfile->state.save_comments = 0;

  if (node == n_modules[spec_nodes::M_EXPORT][0])
    {
      peek = _cpp_lex_direct (pfile);
      keyword = peek;
      backup++;
      if (keyword->type != CPP_NAME)
	goto not_module;
      node = keyword->val.node.node;
      if (!(node->flags & NODE_MODULE))
	goto not_module;
    }

  if (node == n_modules[spec_nodes::M__IMPORT][0])
    /* __import  */
    header_count = backup + 2 + 16;
  else if (node == n_modules[spec_nodes::M_IMPORT][0])
    /* import  */
    header_count = backup + 2 + (CPP_OPTION (pfile, preprocessed) ? 16 : 0);
  else if (node == n_modules[spec_nodes::M_MODULE][0])
    ; /* module  */
  else
    goto not_module;

  /* We've seen [export] {module|import|__import}.  Check the next token.  */
  if (header_count)
    /* After '{,__}import' a header name may appear.  */
    pfile->state.angled_headers = true;
  peek = _cpp_lex_direct (pfile);
  backup++;

  /* ... import followed by identifier, ':', '<' or
     header-name preprocessing tokens, or module
     followed by cpp-identifier, ':' or ';' preprocessing
     tokens.  C++ keywords are not yet relevant.  */
  if (peek->type == CPP_NAME
      || peek->type == CPP_COLON
      ||  (header_count
	   ? (peek->type == CPP_LESS
	      || (peek->type == CPP_STRING && peek->val.str.text[0] != 'R')
	      || peek->type == CPP_HEADER_NAME)
	   : peek->type == CPP_SEMICOLON))
    {
      pfile->state.pragma_allow_expansion = !CPP_OPTION (pfile, preprocessed);
      if (!pfile->state.pragma_allow_expansion)
	pfile->state.prevent_expansion++;

      if (!header_count && linemap_included_from
	  (LINEMAPS_LAST_ORDINARY_MAP (pfile->line_table)))
	cpp_error_with_line (pfile, CPP_DL_ERROR, keyword->src_loc, 0,
			     "module control-line cannot be in included file");

      /* The first one or two tokens cannot be macro names.  */
      for (int ix = backup; ix--;)
	{
	  cpp_token *tok = ix ? keyword : result;
	  cpp_hashnode *node = tok->val.node.node;

	  /* Don't attempt to expand the token.  */
	  tok->flags |= NO_EXPAND;
	  if (_cpp_defined_macro_p (node)
	      && _cpp_maybe_notify_macro_use (pfile, node, tok->src_loc)
	      && !cpp_fun_like_macro_p (node))
	    cpp_error_with_line (pfile, CPP_DL_ERROR, tok->src_loc, 0, 
				 "module control-line \"%s\" cannot be"
				 " an object-like macro",
				 NODE_NAME (node));
	}

      /* Map to underbar variants.  */
      keyword->val.node.node = n_modules[header_count
					 ? spec_nodes::M_IMPORT
					 : spec_nodes::M_MODULE][1];
      if (backup != 1)
	result->val.node.node = n_modules[spec_nodes::M_EXPORT][1];

      /* Maybe tell the tokenizer we expect a header-name down the
	 road.  */
      pfile->state.directive_file_token = header_count;
    }
  else
    {
    not_module:
      /* Drop out of directive mode.  */
      /* We aaserted save_comments had this value upon entry.  */
      pfile->state.save_comments
	= !CPP_OPTION (pfile, discard_comments);
      pfile->state.in_deferred_pragma = false;
      /* Do not let this remain on.  */
      pfile->state.angled_headers = false;
    }

  /* In either case we want to backup the peeked tokens.  */
  if (backup)
    {
      /* If we saw EOL, we should drop it, because this isn't a module
	 control-line after all.  */
      bool eol = peek->type == CPP_PRAGMA_EOL;
      if (!eol || backup > 1)
	{
	  /* Put put the peeked tokens back  */
	  _cpp_backup_tokens_direct (pfile, backup);
	  /* But if the last one was an EOL, forget it.  */
	  if (eol)
	    pfile->lookaheads--;
	}
    }
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
	  else if (result->type == CPP_NAME
		   && (result->val.node.node->flags & NODE_MODULE)
		   && !pfile->state.skipping
		   /* Unlike regular directives, we do not deal with
		      tokenizing module directives as macro arguments.
		      That's not permitted.  */
		   && !pfile->state.parsing_args)
	    {
	      /* P1857.  Before macro expansion, At start of logical
		 line ... */
	      /* We don't have to consider lookaheads at this point.  */
	      gcc_checking_assert (!pfile->lookaheads);

	      cpp_maybe_module_directive (pfile, result);
	    }

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
template <bool lexing_raw_string>
static bool
get_fresh_line_impl (cpp_reader *pfile)
{
  /* We can't get a new line until we leave the current directive, unless we
     are lexing a raw string, in which case it will be OK as long as we don't
     pop the current buffer.  */
  if (!lexing_raw_string && pfile->state.in_directive)
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

      /* We can't change buffers until we leave the current directive.  */
      if (lexing_raw_string && pfile->state.in_directive)
	return false;

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

      if (buffer->prev && !buffer->return_at_eof)
	_cpp_pop_buffer (pfile);
      else
	{
	  /* End of translation.  Do not pop the buffer yet. Increment
	     line number so that the EOF token is on a line of its own
	     (_cpp_lex_direct doesn't increment in that case, because
	     it's hard for it to distinguish this special case). */
	  CPP_INCREMENT_LINE (pfile, 0);
	  return false;
	}
    }
}

bool
_cpp_get_fresh_line (cpp_reader *pfile)
{
  return get_fresh_line_impl<false> (pfile);
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
  bool fallthrough_comment = false;
  cpp_token *result = pfile->cur_token++;

 fresh_line:
  result->flags = 0;
  buffer = pfile->buffer;
  if (buffer->need_line)
    {
      if (pfile->state.in_deferred_pragma)
	{
	  /* This can happen in cases like:
	     #define loop(x) whatever
	     #pragma omp loop
	     where when trying to expand loop we need to peek
	     next token after loop, but aren't still in_deferred_pragma
	     mode but are in in_directive mode, so buffer->need_line
	     is set, a CPP_EOF is peeked.  */
	  result->type = CPP_PRAGMA_EOL;
	  pfile->state.in_deferred_pragma = false;
	  if (!pfile->state.pragma_allow_expansion)
	    pfile->state.prevent_expansion--;
	  return result;
	}
      if (!_cpp_get_fresh_line (pfile))
	{
	  result->type = CPP_EOF;
	  /* Not a real EOF in a directive or arg parsing -- we refuse
  	     to advance to the next file now, and will once we're out
  	     of those modes.  */
	  if (!pfile->state.in_directive && !pfile->state.parsing_args)
	    {
	      /* Tell the compiler the line number of the EOF token.  */
	      result->src_loc = pfile->line_table->highest_line;
	      result->flags = BOL;
	      /* Now pop the buffer that _cpp_get_fresh_line did not.  */
	      _cpp_pop_buffer (pfile);
	    }
	  return result;
	}
      if (buffer != pfile->buffer)
	fallthrough_comment = false;
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

  if (pfile->forced_token_location)
    result->src_loc = pfile->forced_token_location;
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
      /* Increment the line, unless this is the last line ...  */
      if (buffer->cur < buffer->rlimit
	  /* ... or this is a #include, (where _cpp_stack_file needs to
	     unwind by one line) ...  */
	  || (pfile->state.in_directive > 1
	      /* ... except traditional-cpp increments this elsewhere.  */
	      && !CPP_OPTION (pfile, traditional)))
	CPP_INCREMENT_LINE (pfile, 0);
      buffer->need_line = true;
      if (pfile->state.in_deferred_pragma)
	{
	  /* Produce the PRAGMA_EOL on this line.  File reading
	     ensures there is always a \n at end of the buffer, thus
	     in a deferred pragma we always see CPP_PRAGMA_EOL before
	     any CPP_EOF.  */
	  result->type = CPP_PRAGMA_EOL;
	  result->flags &= ~PREV_WHITE;
	  pfile->state.in_deferred_pragma = false;
	  if (!pfile->state.pragma_allow_expansion)
	    pfile->state.prevent_expansion--;
	  return result;
	}
      goto fresh_line;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      {
	struct normalize_state nst = INITIAL_NORMALIZE_STATE;
	result->type = CPP_NUMBER;
	lex_number (pfile, &result->val.str, &nst);
	warn_about_normalization (pfile, result, &nst, false);
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
		  && ((buffer->cur[1] == '"' || (buffer->cur[1] == '\''
				&& CPP_OPTION (pfile, utf8_char_literals)))
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
						&nst,
						&result->val.node.spelling);
	warn_about_normalization (pfile, result, &nst, true);
      }

      /* Convert named operators to their proper types.  */
      if (result->val.node.node->flags & NODE_OPERATOR)
	{
	  result->flags |= NAMED_OP;
	  result->type = (enum cpp_ttype) result->val.node.node->directive_index;
	}

      /* Signal FALLTHROUGH comment followed by another token.  */
      if (fallthrough_comment)
	result->flags |= PREV_FALLTHROUGH;
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
      else if (c == '/' && ! CPP_OPTION (pfile, traditional))
	{
	  /* Don't warn for system headers.  */
	  if (_cpp_in_system_header (pfile))
	    ;
	  /* Warn about comments if pedantically GNUC89, and not
	     in system headers.  */
	  else if (CPP_OPTION (pfile, lang) == CLK_GNUC89
		   && CPP_PEDANTIC (pfile)
		   && ! buffer->warned_cplusplus_comments)
	    {
	      if (cpp_error (pfile, CPP_DL_PEDWARN,
			     "C++ style comments are not allowed in ISO C90"))
		cpp_error (pfile, CPP_DL_NOTE,
			   "(this will be reported only once per input file)");
	      buffer->warned_cplusplus_comments = 1;
	    }
	  /* Or if specifically desired via -Wc90-c99-compat.  */
	  else if (CPP_OPTION (pfile, cpp_warn_c90_c99_compat) > 0
		   && ! CPP_OPTION (pfile, cplusplus)
		   && ! buffer->warned_cplusplus_comments)
	    {
	      if (cpp_error (pfile, CPP_DL_WARNING,
			     "C++ style comments are incompatible with C90"))
		cpp_error (pfile, CPP_DL_NOTE,
			   "(this will be reported only once per input file)");
	      buffer->warned_cplusplus_comments = 1;
	    }
	  /* In C89/C94, C++ style comments are forbidden.  */
	  else if ((CPP_OPTION (pfile, lang) == CLK_STDC89
		    || CPP_OPTION (pfile, lang) == CLK_STDC94))
	    {
	      /* But don't be confused about valid code such as
	         - // immediately followed by *,
		 - // in a preprocessing directive,
		 - // in an #if 0 block.  */
	      if (buffer->cur[1] == '*'
		  || pfile->state.in_directive
		  || pfile->state.skipping)
		{
		  result->type = CPP_DIV;
		  break;
		}
	      else if (! buffer->warned_cplusplus_comments)
		{
		  if (cpp_error (pfile, CPP_DL_ERROR,
				 "C++ style comments are not allowed in "
				 "ISO C90"))
		    cpp_error (pfile, CPP_DL_NOTE,
			       "(this will be reported only once per input "
			       "file)");
		  buffer->warned_cplusplus_comments = 1;
		}
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

      if (fallthrough_comment_p (pfile, comment_start))
	fallthrough_comment = true;

      if (pfile->cb.comment)
	{
	  size_t len = pfile->buffer->cur - comment_start;
	  pfile->cb.comment (pfile, result->src_loc, comment_start - 1,
			     len + 1);
	}

      if (!pfile->state.save_comments)
	{
	  result->flags |= PREV_WHITE;
	  goto update_tokens_line;
	}

      if (fallthrough_comment)
	result->flags |= PREV_FALLTHROUGH;

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
	{
	  buffer->cur++, result->type = CPP_LESS_EQ;
	  if (*buffer->cur == '>'
	      && CPP_OPTION (pfile, cplusplus)
	      && CPP_OPTION (pfile, lang) >= CLK_GNUCXX20)
	    buffer->cur++, result->type = CPP_SPACESHIP;
	}
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
	  warn_about_normalization (pfile, result, &nst, false);
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
      if (*buffer->cur == ':' && CPP_OPTION (pfile, scope))
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

    default:
      {
	const uchar *base = --buffer->cur;
	static int no_warn_cnt;

	/* Check for an extended identifier ($ or UCN or UTF-8).  */
	struct normalize_state nst = INITIAL_NORMALIZE_STATE;
	if (forms_identifier_p (pfile, true, &nst))
	  {
	    result->type = CPP_NAME;
	    result->val.node.node = lex_identifier (pfile, base, true, &nst,
						    &result->val.node.spelling);
	    warn_about_normalization (pfile, result, &nst, true);
	    break;
	  }

	/* Otherwise this will form a CPP_OTHER token.  Parse valid UTF-8 as a
	   single token.  */
	buffer->cur++;
	if (c >= utf8_signifier)
	  {
	    const uchar *pstr = base;
	    cppchar_t s;
	    if (_cpp_valid_utf8 (pfile, &pstr, buffer->rlimit, 0, NULL, &s))
	      {
		if (s > UCS_LIMIT && CPP_OPTION (pfile, cpp_warn_invalid_utf8))
		  {
		    buffer->cur = base;
		    _cpp_warn_invalid_utf8 (pfile);
		  }
		buffer->cur = pstr;
	      }
	    else if (CPP_OPTION (pfile, cpp_warn_invalid_utf8))
	      {
		buffer->cur = base;
		const uchar *end = _cpp_warn_invalid_utf8 (pfile);
		buffer->cur = base + 1;
		no_warn_cnt = end - buffer->cur;
	      }
	  }
	else if (c >= utf8_continuation
		 && CPP_OPTION (pfile, cpp_warn_invalid_utf8))
	  {
	    if (no_warn_cnt)
	      --no_warn_cnt;
	    else
	      {
		buffer->cur = base;
		_cpp_warn_invalid_utf8 (pfile);
		buffer->cur = base + 1;
	      }
	  }
	create_literal (pfile, result, base, buffer->cur - base, CPP_OTHER);
	break;
      }

    }

  /* Potentially convert the location of the token to a range.  */
  if (result->src_loc >= RESERVED_LOCATION_COUNT
      && result->type != CPP_EOF)
    {
      /* Ensure that any line notes are processed, so that we have the
	 correct physical line/column for the end-point of the token even
	 when a logical line is split via one or more backslashes.  */
      if (buffer->cur >= buffer->notes[buffer->cur_note].pos
	  && !pfile->overlaid_buffer)
	_cpp_process_line_notes (pfile, false);

      source_range tok_range;
      tok_range.m_start = result->src_loc;
      tok_range.m_finish
	= linemap_position_for_column (pfile->line_table,
				       CPP_BUF_COLUMN (buffer, buffer->cur));

      result->src_loc = COMBINE_LOCATION_DATA (pfile->line_table,
					       result->src_loc,
					       tok_range, NULL, 0);
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

/* Write the spelling of an identifier IDENT, using UCNs, to BUFFER.
   The buffer must already contain the enough space to hold the
   token's spelling.  Returns a pointer to the character after the
   last character written.  */
unsigned char *
_cpp_spell_ident_ucns (unsigned char *buffer, cpp_hashnode *ident)
{
  size_t i;
  const unsigned char *name = NODE_NAME (ident);
	  
  for (i = 0; i < NODE_LEN (ident); i++)
    if (name[i] & ~0x7F)
      {
	i += utf8_to_ucn (buffer, name + i) - 1;
	buffer += 10;
      }
    else
      *buffer++ = name[i];

  return buffer;
}

/* Write the spelling of a token TOKEN to BUFFER.  The buffer must
   already contain the enough space to hold the token's spelling.
   Returns a pointer to the character after the last character written.
   FORSTRING is true if this is to be the spelling after translation
   phase 1 (with the original spelling of extended identifiers), false
   if extended identifiers should always be written using UCNs (there is
   no option for always writing them in the internal UTF-8 form).
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
	  memcpy (buffer, NODE_NAME (token->val.node.spelling),
		  NODE_LEN (token->val.node.spelling));
	  buffer += NODE_LEN (token->val.node.spelling);
	}
      else
	buffer = _cpp_spell_ident_ucns (buffer, token->val.node.node);
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
      if (token->type == CPP_HEADER_NAME)
	fputc ('"', fp);
      fwrite (token->val.str.text, 1, token->val.str.len, fp);
      if (token->type == CPP_HEADER_NAME)
	fputc ('"', fp);
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
		|| (a->val.macro_arg.arg_no == b->val.macro_arg.arg_no
		    && a->val.macro_arg.spelling == b->val.macro_arg.spelling));
      case SPELL_IDENT:
	return (a->val.node.node == b->val.node.node
		&& a->val.node.spelling == b->val.node.spelling);
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
    case CPP_PRAGMA:
    case CPP_NAME:	return ((b == CPP_NUMBER
				 && name_p (pfile, &token2->val.str))
				|| b == CPP_NAME
				|| b == CPP_CHAR || b == CPP_STRING); /* L */
    case CPP_NUMBER:	return (b == CPP_NUMBER || b == CPP_NAME
				|| b == CPP_CHAR
				|| c == '.' || c == '+' || c == '-');
				      /* UCNs */
    case CPP_OTHER:	return ((token1->val.str.text[0] == '\\'
				 && b == CPP_NAME)
				|| (CPP_OPTION (pfile, objc)
				    && token1->val.str.text[0] == '@'
				    && (b == CPP_NAME || b == CPP_STRING)));
    case CPP_LESS_EQ:	return c == '>';
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

#ifdef ENABLE_VALGRIND_ANNOTATIONS
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
#ifdef ENABLE_VALGRIND_ANNOTATIONS
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

/* Commit or allocate storage from a buffer.  */

void *
_cpp_commit_buff (cpp_reader *pfile, size_t size)
{
  void *ptr = BUFF_FRONT (pfile->a_buff);

  if (pfile->hash_table->alloc_subobject)
    {
      void *copy = pfile->hash_table->alloc_subobject (size);
      memcpy (copy, ptr, size);
      ptr = copy;
    }
  else
    BUFF_FRONT (pfile->a_buff) += size;

  return ptr;
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
      /* Operands which were originally spelled as ident keep around
         the node for the exact spelling.  */
      if (tok->flags & NAMED_OP)
	return CPP_TOKEN_FLD_NODE;
      else if (tok->type == CPP_PASTE)
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
      /* fall through */
    default:
      return CPP_TOKEN_FLD_NONE;
    }
}

/* All tokens lexed in R after calling this function will be forced to
   have their location_t to be P, until
   cpp_stop_forcing_token_locations is called for R.  */

void
cpp_force_token_locations (cpp_reader *r, location_t loc)
{
  r->forced_token_location = loc;
}

/* Go back to assigning locations naturally for lexed tokens.  */

void
cpp_stop_forcing_token_locations (cpp_reader *r)
{
  r->forced_token_location = 0;
}

/* We're looking at \, if it's escaping EOL, look past it.  If at
   LIMIT, don't advance.  */

static const unsigned char *
do_peek_backslash (const unsigned char *peek, const unsigned char *limit)
{
  const unsigned char *probe = peek;

  if (__builtin_expect (peek[1] == '\n', true))
    {
    eol:
      probe += 2;
      if (__builtin_expect (probe < limit, true))
	{
	  peek = probe;
	  if (*peek == '\\')
	    /* The user might be perverse.  */
	    return do_peek_backslash (peek, limit);
	}
    }
  else if (__builtin_expect (peek[1] == '\r', false))
    {
      if (probe[2] == '\n')
	probe++;
      goto eol;
    }

  return peek;
}

static const unsigned char *
do_peek_next (const unsigned char *peek, const unsigned char *limit)
{
  if (__builtin_expect (*peek == '\\', false))
    peek = do_peek_backslash (peek, limit);
  return peek;
}

static const unsigned char *
do_peek_prev (const unsigned char *peek, const unsigned char *bound)
{
  if (peek == bound)
    return NULL;

  unsigned char c = *--peek;
  if (__builtin_expect (c == '\n', false)
      || __builtin_expect (c == 'r', false))
    {
      if (peek == bound)
	return peek;
      int ix = -1;
      if (c == '\n' && peek[ix] == '\r')
	{
	  if (peek + ix == bound)
	    return peek;
	  ix--;
	}

      if (peek[ix] == '\\')
	return do_peek_prev (peek + ix, bound);

      return peek;
    }
  else
    return peek;
}

/* If PEEK[-1] is identifier MATCH, scan past it and trailing white
   space.  Otherwise return NULL.  */

static const unsigned char *
do_peek_ident (const char *match, const unsigned char *peek,
	       const unsigned char *limit)
{
  for (; *++match; peek++)
    if (*peek != *match)
      {
	peek = do_peek_next (peek, limit);
	if (*peek != *match)
	  return NULL;
      }

  /* Must now not be looking at an identifier char.  */
  peek = do_peek_next (peek, limit);
  if (ISIDNUM (*peek))
    return NULL;

  /* Skip control-line whitespace.  */
 ws:
  while (*peek == ' ' || *peek == '\t')
    peek++;
  if (__builtin_expect (*peek == '\\', false))
    {
      peek = do_peek_backslash (peek, limit);
      if (*peek != '\\')
	goto ws;
    }

  return peek;
}

/* Are we looking at a module control line starting as PEEK - 1?  */

static bool
do_peek_module (cpp_reader *pfile, unsigned char c,
		const unsigned char *peek, const unsigned char *limit)
{
  bool import = false;

  if (__builtin_expect (c == 'e', false))
    {
      if (!((peek[0] == 'x' || peek[0] == '\\')
	    && (peek = do_peek_ident ("export", peek, limit))))
	return false;

      /* export, peek for import or module.  No need to peek __import
	 here.  */
      if (peek[0] == 'i')
	{
	  if (!((peek[1] == 'm' || peek[1] == '\\')
		&& (peek = do_peek_ident ("import", peek + 1, limit))))
	    return false;
	  import = true;
	}
      else if (peek[0] == 'm')
	{
	  if (!((peek[1] == 'o' || peek[1] == '\\')
		&& (peek = do_peek_ident ("module", peek + 1, limit))))
	    return false;
	}
      else
	return false;
    }
  else if (__builtin_expect (c == 'i', false))
    {
      if (!((peek[0] == 'm' || peek[0] == '\\')
	    && (peek = do_peek_ident ("import", peek, limit))))
	return false;
      import = true;
    }
  else if (__builtin_expect (c == '_', false))
    {
      /* Needed for translated includes.   */
      if (!((peek[0] == '_' || peek[0] == '\\')
	    && (peek = do_peek_ident ("__import", peek, limit))))
	return false;
      import = true;
    }
  else if (__builtin_expect (c == 'm', false))
    {
      if (!((peek[0] == 'o' || peek[0] == '\\')
	    && (peek = do_peek_ident ("module", peek, limit))))
	return false;
    }
  else
    return false;

  /* Peek the next character to see if it's good enough.  We'll be at
     the first non-whitespace char, including skipping an escaped
     newline.  */
  /* ... import followed by identifier, ':', '<' or header-name
     preprocessing tokens, or module followed by identifier, ':' or
     ';' preprocessing tokens.  */
  unsigned char p = *peek++;
      
  /* A character literal is ... single quotes, ... optionally preceded
     by u8, u, U, or L */
  /* A string-literal is a ... double quotes, optionally prefixed by
     R, u8, u8R, u, uR, U, UR, L, or LR */
  if (p == 'u')
    {
      peek = do_peek_next (peek, limit);
      if (*peek == '8')
	{
	  peek++;
	  goto peek_u8;
	}
      goto peek_u;
    }
  else if (p == 'U' || p == 'L')
    {
    peek_u8:
      peek = do_peek_next (peek, limit);
    peek_u:
      if (*peek == '\"' || *peek == '\'')
	return false;

      if (*peek == 'R')
	goto peek_R;
      /* Identifier. Ok.  */
    }
  else if (p == 'R')
    {
    peek_R:
      if (CPP_OPTION (pfile, rliterals))
	{
	  peek = do_peek_next (peek, limit);
	  if (*peek == '\"')
	    return false;
	}
      /* Identifier. Ok.  */
    }
  else if ('Z' - 'A' == 25
	   ? ((p >= 'A' && p <= 'Z') || (p >= 'a' && p <= 'z') || p == '_')
	   : ISIDST (p))
    {
      /* Identifier.  Ok. */
    }
  else if (p == '<')
    {
      /* Maybe angle header, ok for import.  Reject
	 '<=', '<<' digraph:'<:'.  */
      if (!import)
	return false;
      peek = do_peek_next (peek, limit);
      if (*peek == '=' || *peek == '<'
	  || (*peek == ':' && CPP_OPTION (pfile, digraphs)))
	return false;
    }
  else if (p == ';')
    {
      /* SEMICOLON, ok for module.  */
      if (import)
	return false;
    }
  else if (p == '"')
    {
      /* STRING, ok for import.  */
      if (!import)
	return false;
    }
  else if (p == ':')
    {
      /* Maybe COLON, ok.  Reject '::', digraph:':>'.  */
      peek = do_peek_next (peek, limit);
      if (*peek == ':' || (*peek == '>' && CPP_OPTION (pfile, digraphs)))
	return false;
    }
  else
    /* FIXME: Detect a unicode character, excluding those not
       permitted as the initial character. [lex.name]/1.  I presume
       we need to check the \[uU] spellings, and directly using
       Unicode in say UTF8 form?  Or perhaps we do the phase-1
       conversion of UTF8 to universal-character-names?  */
    return false;

  return true;
}

/* Directives-only scanning.  Somewhat more relaxed than correct
   parsing -- some ill-formed programs will not be rejected.  */

void
cpp_directive_only_process (cpp_reader *pfile,
			    void *data,
			    void (*cb) (cpp_reader *, CPP_DO_task, void *, ...))
{
  bool module_p = CPP_OPTION (pfile, module_directives);

  do
    {
    restart:
      /* Buffer initialization, but no line cleaning. */
      cpp_buffer *buffer = pfile->buffer;
      buffer->cur_note = buffer->notes_used = 0;
      buffer->cur = buffer->line_base = buffer->next_line;
      buffer->need_line = false;
      /* Files always end in a newline or carriage return.  We rely on this for
	 character peeking safety.  */
      gcc_assert (buffer->rlimit[0] == '\n' || buffer->rlimit[0] == '\r');

      const unsigned char *base = buffer->cur;
      unsigned line_count = 0;
      const unsigned char *line_start = base;

      bool bol = true;
      bool raw = false;

      const unsigned char *lwm = base;
      for (const unsigned char *pos = base, *limit = buffer->rlimit;
	   pos < limit;)
	{
	  unsigned char c = *pos++;
	  /* This matches the switch in _cpp_lex_direct.  */
	  switch (c)
	    {
	    case ' ': case '\t': case '\f': case '\v':
	      /* Whitespace, do nothing.  */
	      break;

	    case '\r': /* MAC line ending, or Windows \r\n  */
	      if (*pos == '\n')
		pos++;
	      /* FALLTHROUGH */

	    case '\n':
	      bol = true;

	    next_line:
	      CPP_INCREMENT_LINE (pfile, 0);
	      line_count++;
	      line_start = pos;
	      break;

	    case '\\':
	      /* <backslash><newline> is removed, and doesn't undo any
		 preceeding escape or whatnot.  */
	      if (*pos == '\n')
		{
		  pos++;
		  goto next_line;
		}
	      else if (*pos == '\r')
		{
		  if (pos[1] == '\n')
		    pos++;
		  pos++;
		  goto next_line;
		}
	      goto dflt;
	      
	    case '#':
	      if (bol)
		{
		  /* Line directive.  */
		  if (pos - 1 > base && !pfile->state.skipping)
		    cb (pfile, CPP_DO_print, data,
			line_count, base, pos - 1 - base);

		  /* Prep things for directive handling. */
		  buffer->next_line = pos;
		  buffer->need_line = true;
		  bool ok = _cpp_get_fresh_line (pfile);
		  gcc_checking_assert (ok);

		  /* Ensure proper column numbering for generated
		     error messages. */
		  buffer->line_base -= pos - line_start;

		  _cpp_handle_directive (pfile, line_start + 1 != pos);

		  /* Sanitize the line settings.  Duplicate #include's can
		     mess things up. */
		  // FIXME: Necessary?
		  pfile->line_table->highest_location
		    = pfile->line_table->highest_line;

		  if (!pfile->state.skipping
		      && pfile->buffer->next_line < pfile->buffer->rlimit)
		    cb (pfile, CPP_DO_location, data,
			pfile->line_table->highest_line);

		  goto restart;
		}
	      goto dflt;

	    case '/':
	      {
		const unsigned char *peek = do_peek_next (pos, limit);
		if (!(*peek == '/' || *peek == '*'))
		  goto dflt;

		/* Line or block comment  */
		bool is_block = *peek == '*';
		bool star = false;
		bool esc = false;
		location_t sloc
		  = linemap_position_for_column (pfile->line_table,
						 pos - line_start);

		while (pos < limit)
		  {
		    char c = *pos++;
		    switch (c)
		      {
		      case '\\':
			esc = true;
			break;

		      case '\r':
			if (*pos == '\n')
			  pos++;
			/* FALLTHROUGH  */

		      case '\n':
			{
			  CPP_INCREMENT_LINE (pfile, 0);
			  line_count++;
			  line_start = pos;
			  if (!esc && !is_block)
			    {
			      bol = true;
			      goto done_comment;
			    }
			}
			if (!esc)
			  star = false;
			esc = false;
			break;

		      case '*':
			if (pos > peek)
			  star = is_block;
			esc = false;
			break;

		      case '/':
			if (star)
			  goto done_comment;
			/* FALLTHROUGH  */

		      default:
			star = false;
			esc = false;
			break;
		      }
		  }
		if (pos < limit || is_block)
		  cpp_error_with_line (pfile, CPP_DL_ERROR, sloc, 0,
				       "unterminated comment");
	      done_comment:
		lwm = pos;
		break;
	      }

	    case '\'':
	      if (!CPP_OPTION (pfile, digit_separators))
		goto delimited_string;

	      /* Possibly a number punctuator.  */
	      if (!ISIDNUM (*do_peek_next (pos, limit)))
		goto delimited_string;

	      goto quote_peek;

	    case '\"':
	      if (!CPP_OPTION (pfile, rliterals))
		goto delimited_string;

	    quote_peek:
	      {
		/* For ' see if it's a number punctuator
		   \.?<digit>(<digit>|<identifier-nondigit>
		   |'<digit>|'<nondigit>|[eEpP]<sign>|\.)* */
		/* For " see if it's a raw string
		   {U,L,u,u8}R.  This includes CPP_NUMBER detection,
		   because that could be 0e+R.  */
		const unsigned char *peek = pos - 1;
		bool quote_first = c == '"';
		bool quote_eight = false;
		bool maybe_number_start = false;
		bool want_number = false;

		while ((peek = do_peek_prev (peek, lwm)))
		  {
		    unsigned char p = *peek;
		    if (quote_first)
		      {
			if (!raw)
			  {
			    if (p != 'R')
			      break;
			    raw = true;
			    continue;
			  }

			quote_first = false;
			if (p == 'L' || p == 'U' || p == 'u')
			  ;
			else if (p == '8')
			  quote_eight = true;
			else
			  goto second_raw;
		      }
		    else if (quote_eight)
		      {
			if (p != 'u')
			  {
			    raw = false;
			    break;
			  }
			quote_eight = false;
		      }
		    else if (c == '"')
		      {
		      second_raw:;
			if (!want_number && ISIDNUM (p))
			  {
			    raw = false;
			    break;
			  }
		      }

		    if (ISDIGIT (p))
		      maybe_number_start = true;
		    else if (p == '.')
		      want_number = true;
		    else if (ISIDNUM (p))
		      maybe_number_start = false;
		    else if (p == '+' || p == '-')
		      {
			if (const unsigned char *peek_prev
			    = do_peek_prev (peek, lwm))
			  {
			    p = *peek_prev;
			    if (p == 'e' || p == 'E'
				|| p == 'p' || p == 'P')
			      {
				want_number = true;
				maybe_number_start = false;
			      }
			    else
			      break;
			  }
			else
			  break;
		      }
		    else if (p == '\'' || p == '\"')
		      {
			/* If this is lwm, this must be the end of a
			   previous string.  So this is a trailing
			   literal type, (a) if those are allowed,
			     and (b) maybe_start is false.  Otherwise
			     this must be a CPP_NUMBER because we've
			     met another ', and we'd have checked that
			     in its own right.  */
			if (peek == lwm && CPP_OPTION (pfile, uliterals))
			  {
			    if  (!maybe_number_start && !want_number)
			      /* Must be a literal type.  */
			      raw = false;
			  }
			else if (p == '\''
				 && CPP_OPTION (pfile, digit_separators))
			  maybe_number_start = true;
			break;
		      }
		    else if (c == '\'')
		      break;
		    else if (!quote_first && !quote_eight)
		      break;
		  }

		if (maybe_number_start)
		  {
		    if (c == '\'')
		      /* A CPP NUMBER.  */
		      goto dflt;
		    raw = false;
		  }

		goto delimited_string;
	      }

	    delimited_string:
	      {
		/* (Possibly raw) string or char literal.  */
		unsigned char end = c;
		int delim_len = -1;
		const unsigned char *delim = NULL;
		location_t sloc = linemap_position_for_column (pfile->line_table,
							       pos - line_start);
		int esc = 0;

		if (raw)
		  {
		    /* There can be no line breaks in the delimiter.  */
		    delim = pos;
		    for (delim_len = 0; (c = *pos++) != '('; delim_len++)
		      {
			if (delim_len == 16)
			  {
			    cpp_error_with_line (pfile, CPP_DL_ERROR,
						 sloc, 0,
						 "raw string delimiter"
						 " longer than %d"
						 " characters",
						 delim_len);
			    raw = false;
			    pos = delim;
			    break;
			  }
			if (strchr (") \\\t\v\f\n", c))
			  {
			    cpp_error_with_line (pfile, CPP_DL_ERROR,
						 sloc, 0,
						 "invalid character '%c'"
						 " in raw string"
						 " delimiter", c);
			    raw = false;
			    pos = delim;
			    break;
			  }
			if (pos >= limit)
			  goto bad_string;
		      }
		  }

		while (pos < limit)
		  {
		    char c = *pos++;
		    switch (c)
		      {
		      case '\\':
			if (!raw)
			  esc++;
			break;

		      case '\r':
			if (*pos == '\n')
			  pos++;
			/* FALLTHROUGH  */

		      case '\n':
			{
			  CPP_INCREMENT_LINE (pfile, 0);
			  line_count++;
			  line_start = pos;
			}
			if (esc)
			  esc--;
			break;

		      case ')':
			if (raw
			    && pos + delim_len + 1 < limit
			    && pos[delim_len] == end
			    && !memcmp (delim, pos, delim_len))
			  {
			    pos += delim_len + 1;
			    raw = false;
			    goto done_string;
			  }
			break;

		      default:
			if (!raw && !(esc & 1) && c == end)
			  goto done_string;
			esc = 0;
			break;
		      }
		  }
	      bad_string:
		cpp_error_with_line (pfile, CPP_DL_ERROR, sloc, 0,
				     "unterminated literal");
		
	      done_string:
		raw = false;
		lwm = pos - 1;
	      }
	      goto dflt;

	    case '_':
	    case 'e':
	    case 'i':
	    case 'm':
	      if (bol && module_p && !pfile->state.skipping
		  && do_peek_module (pfile, c, pos, limit))
		{
		  /* We've seen the start of a module control line.
		     Start up the tokenizer.  */
		  pos--; /* Backup over the first character.  */

		  /* Backup over whitespace to start of line.  */
		  while (pos > line_start
			 && (pos[-1] == ' ' || pos[-1] == '\t'))
		    pos--;

		  if (pos > base)
		    cb (pfile, CPP_DO_print, data, line_count, base, pos - base);

		  /* Prep things for directive handling. */
		  buffer->next_line = pos;
		  buffer->need_line = true;

		  /* Now get tokens until the PRAGMA_EOL.  */
		  do
		    {
		      location_t spelling;
		      const cpp_token *tok
			= cpp_get_token_with_location (pfile, &spelling);

		      gcc_assert (pfile->state.in_deferred_pragma
				  || tok->type == CPP_PRAGMA_EOL);
		      cb (pfile, CPP_DO_token, data, tok, spelling);
		    }
		  while (pfile->state.in_deferred_pragma);

		  if (pfile->buffer->next_line < pfile->buffer->rlimit)
		    cb (pfile, CPP_DO_location, data,
			pfile->line_table->highest_line);

		  pfile->mi_valid = false;
		  goto restart;
		}
	      goto dflt;

	    default:
	    dflt:
	      bol = false;
	      pfile->mi_valid = false;
	      break;
	    }
	}

      if (buffer->rlimit > base && !pfile->state.skipping)
	{
	  const unsigned char *limit = buffer->rlimit;
	  /* If the file was not newline terminated, add rlimit, which is
	     guaranteed to point to a newline, to the end of our range.  */
	  if (limit[-1] != '\n')
	    {
	      limit++;
	      CPP_INCREMENT_LINE (pfile, 0);
	      line_count++;
	    }
	  cb (pfile, CPP_DO_print, data, line_count, base, limit - base);
	}

      _cpp_pop_buffer (pfile);
    }
  while (pfile->buffer);
}
