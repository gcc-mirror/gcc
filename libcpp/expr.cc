/* Parse C expressions for cpplib.
   Copyright (C) 1987-2024 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994.

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

#define PART_PRECISION (sizeof (cpp_num_part) * CHAR_BIT)
#define HALF_MASK (~(cpp_num_part) 0 >> (PART_PRECISION / 2))
#define LOW_PART(num_part) (num_part & HALF_MASK)
#define HIGH_PART(num_part) (num_part >> (PART_PRECISION / 2))

struct op
{
  const cpp_token *token;	/* The token forming op (for diagnostics).  */
  cpp_num value;		/* The value logically "right" of op.  */
  location_t loc;          /* The location of this value.         */
  enum cpp_ttype op;
};

/* Some simple utility routines on double integers.  */
#define num_zerop(num) ((num.low | num.high) == 0)
#define num_eq(num1, num2) (num1.low == num2.low && num1.high == num2.high)
static bool num_positive (cpp_num, size_t);
static bool num_greater_eq (cpp_num, cpp_num, size_t);
static cpp_num num_trim (cpp_num, size_t);
static cpp_num num_part_mul (cpp_num_part, cpp_num_part);

static cpp_num num_unary_op (cpp_reader *, cpp_num, enum cpp_ttype);
static cpp_num num_binary_op (cpp_reader *, cpp_num, cpp_num, enum cpp_ttype);
static cpp_num num_negate (cpp_num, size_t);
static cpp_num num_bitwise_op (cpp_reader *, cpp_num, cpp_num, enum cpp_ttype);
static cpp_num num_inequality_op (cpp_reader *, cpp_num, cpp_num,
				  enum cpp_ttype);
static cpp_num num_equality_op (cpp_reader *, cpp_num, cpp_num,
				enum cpp_ttype);
static cpp_num num_mul (cpp_reader *, cpp_num, cpp_num);
static cpp_num num_div_op (cpp_reader *, cpp_num, cpp_num, enum cpp_ttype,
			   location_t);
static cpp_num num_lshift (cpp_num, size_t, size_t);
static cpp_num num_rshift (cpp_num, size_t, size_t);

static cpp_num append_digit (cpp_num, int, int, size_t);
static cpp_num parse_defined (cpp_reader *);
static cpp_num eval_token (cpp_reader *, const cpp_token *, location_t);
static struct op *reduce (cpp_reader *, struct op *, enum cpp_ttype);
static unsigned int interpret_float_suffix (cpp_reader *, const uchar *, size_t);
static unsigned int interpret_int_suffix (cpp_reader *, const uchar *, size_t);
static void check_promotion (cpp_reader *, const struct op *);

/* Token type abuse to create unary plus and minus operators.  */
#define CPP_UPLUS ((enum cpp_ttype) (CPP_LAST_CPP_OP + 1))
#define CPP_UMINUS ((enum cpp_ttype) (CPP_LAST_CPP_OP + 2))

/* With -O2, gcc appears to produce nice code, moving the error
   message load and subsequent jump completely out of the main path.  */
#define SYNTAX_ERROR(msgid) \
  do { cpp_error (pfile, CPP_DL_ERROR, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR2(msgid, arg) \
  do { cpp_error (pfile, CPP_DL_ERROR, msgid, arg); goto syntax_error; } \
  while(0)
#define SYNTAX_ERROR_AT(loc, msgid) \
  do { cpp_error_with_line (pfile, CPP_DL_ERROR, (loc), 0, msgid); goto syntax_error; } \
  while(0)
#define SYNTAX_ERROR2_AT(loc, msgid, arg)					\
  do { cpp_error_with_line (pfile, CPP_DL_ERROR, (loc), 0, msgid, arg); goto syntax_error; } \
  while(0)

/* Subroutine of cpp_classify_number.  S points to a float suffix of
   length LEN, possibly zero.  Returns 0 for an invalid suffix, or a
   flag vector (of CPP_N_* bits) describing the suffix.  */
static unsigned int
interpret_float_suffix (cpp_reader *pfile, const uchar *s, size_t len)
{
  size_t orig_len = len;
  const uchar *orig_s = s;
  size_t flags;
  size_t f, d, l, w, q, i, fn, fnx, fn_bits, bf16;

  flags = 0;
  f = d = l = w = q = i = fn = fnx = fn_bits = bf16 = 0;

  /* The following decimal float suffixes, from TR 24732:2009, TS
     18661-2:2015 and C23, are supported:

     df, DF - _Decimal32.
     dd, DD - _Decimal64.
     dl, DL - _Decimal128.

     The dN and DN suffixes for _DecimalN, and dNx and DNx for
     _DecimalNx, defined in TS 18661-3:2015, are not supported.

     Fixed-point suffixes, from TR 18037:2008, are supported.  They
     consist of three parts, in order:

     (i) An optional u or U, for unsigned types.

     (ii) An optional h or H, for short types, or l or L, for long
     types, or ll or LL, for long long types.  Use of ll or LL is a
     GNU extension.

     (iii) r or R, for _Fract types, or k or K, for _Accum types.

     Otherwise the suffix is for a binary or standard floating-point
     type.  Such a suffix, or the absence of a suffix, may be preceded
     or followed by i, I, j or J, to indicate an imaginary number with
     the corresponding complex type.  The following suffixes for
     binary or standard floating-point types are supported:

     f, F - float (ISO C and C++).
     l, L - long double (ISO C and C++).
     d, D - double, even with the FLOAT_CONST_DECIMAL64 pragma in
	    operation (from TR 24732:2009; the pragma and the suffix
	    are not included in TS 18661-2:2015).
     w, W - machine-specific type such as __float80 (GNU extension).
     q, Q - machine-specific type such as __float128 (GNU extension).
     fN, FN - _FloatN (TS 18661-3:2015).
     fNx, FNx - _FloatNx (TS 18661-3:2015).
     bf16, BF16 - std::bfloat16_t (ISO C++23).  */

  /* Process decimal float suffixes, which are two letters starting
     with d or D.  Order and case are significant.  */
  if (len == 2 && (*s == 'd' || *s == 'D'))
    {
      bool uppercase = (*s == 'D');
      switch (s[1])
      {
      case 'f': return (!uppercase ? (CPP_N_DFLOAT | CPP_N_SMALL): 0); break;
      case 'F': return (uppercase ? (CPP_N_DFLOAT | CPP_N_SMALL) : 0); break;
      case 'd': return (!uppercase ? (CPP_N_DFLOAT | CPP_N_MEDIUM): 0); break;
      case 'D': return (uppercase ? (CPP_N_DFLOAT | CPP_N_MEDIUM) : 0); break;
      case 'l': return (!uppercase ? (CPP_N_DFLOAT | CPP_N_LARGE) : 0); break;
      case 'L': return (uppercase ? (CPP_N_DFLOAT | CPP_N_LARGE) : 0); break;
      default:
	/* Additional two-character suffixes beginning with D are not
	   for decimal float constants.  */
	break;
      }
    }

  if (CPP_OPTION (pfile, ext_numeric_literals))
    {
      /* Recognize a fixed-point suffix.  */
      if (len != 0)
	switch (s[len-1])
	  {
	  case 'k': case 'K': flags = CPP_N_ACCUM; break;
	  case 'r': case 'R': flags = CPP_N_FRACT; break;
	  default: break;
	  }

      /* Continue processing a fixed-point suffix.  The suffix is case
	 insensitive except for ll or LL.  Order is significant.  */
      if (flags)
	{
	  if (len == 1)
	    return flags;
	  len--;

	  if (*s == 'u' || *s == 'U')
	    {
	      flags |= CPP_N_UNSIGNED;
	      if (len == 1)
		return flags;
	      len--;
	      s++;
            }

	  switch (*s)
	  {
	  case 'h': case 'H':
	    if (len == 1)
	      return flags |= CPP_N_SMALL;
	    break;
	  case 'l':
	    if (len == 1)
	      return flags |= CPP_N_MEDIUM;
	    if (len == 2 && s[1] == 'l')
	      return flags |= CPP_N_LARGE;
	    break;
	  case 'L':
	    if (len == 1)
	      return flags |= CPP_N_MEDIUM;
	    if (len == 2 && s[1] == 'L')
	      return flags |= CPP_N_LARGE;
	    break;
	  default:
	    break;
	  }
	  /* Anything left at this point is invalid.  */
	  return 0;
	}
    }

  /* In any remaining valid suffix, the case and order don't matter.  */
  while (len--)
    {
      switch (s[0])
	{
	case 'f': case 'F':
	  f++;
	  if (len > 0
	      && s[1] >= '1'
	      && s[1] <= '9'
	      && fn_bits == 0)
	    {
	      f--;
	      while (len > 0
		     && s[1] >= '0'
		     && s[1] <= '9'
		     && fn_bits < CPP_FLOATN_MAX)
		{
		  fn_bits = fn_bits * 10 + (s[1] - '0');
		  len--;
		  s++;
		}
	      if (len > 0 && s[1] == 'x')
		{
		  fnx++;
		  len--;
		  s++;
		}
	      else
		fn++;
	    }
	  break;
	case 'b': case 'B':
	  if (len > 2
	      /* Except for bf16 / BF16 where case is significant.  */
	      && s[1] == (s[0] == 'b' ? 'f' : 'F')
	      && s[2] == '1'
	      && s[3] == '6')
	    {
	      bf16++;
	      len -= 3;
	      s += 3;
	      break;
	    }
	  return 0;
	case 'd': case 'D': d++; break;
	case 'l': case 'L': l++; break;
	case 'w': case 'W': w++; break;
	case 'q': case 'Q': q++; break;
	case 'i': case 'I':
	case 'j': case 'J': i++; break;
	default:
	  return 0;
	}
      s++;
    }

  /* Reject any case of multiple suffixes specifying types, multiple
     suffixes specifying an imaginary constant, _FloatN or _FloatNx
     suffixes for invalid values of N, and _FloatN suffixes for values
     of N larger than can be represented in the return value.  The
     caller is responsible for rejecting _FloatN suffixes where
     _FloatN is not supported on the chosen target.  */
  if (f + d + l + w + q + fn + fnx + bf16 > 1 || i > 1)
    return 0;
  if (fn_bits > CPP_FLOATN_MAX)
    return 0;
  if (fnx && fn_bits != 32 && fn_bits != 64 && fn_bits != 128)
    return 0;
  if (fn && fn_bits != 16 && fn_bits % 32 != 0)
    return 0;
  if (fn && fn_bits == 96)
    return 0;

  if (i)
    {
      if (!CPP_OPTION (pfile, ext_numeric_literals))
	return 0;

      /* In C++14 and up these suffixes are in the standard library, so treat
	 them as user-defined literals.  */
      if (CPP_OPTION (pfile, cplusplus)
	  && CPP_OPTION (pfile, lang) > CLK_CXX11
	  && orig_s[0] == 'i'
	  && (orig_len == 1
	      || (orig_len == 2
		  && (orig_s[1] == 'f' || orig_s[1] == 'l'))))
	return 0;
    }

  if ((w || q) && !CPP_OPTION (pfile, ext_numeric_literals))
    return 0;

  return ((i ? CPP_N_IMAGINARY : 0)
	  | (f ? CPP_N_SMALL :
	     d ? CPP_N_MEDIUM :
	     l ? CPP_N_LARGE :
	     w ? CPP_N_MD_W :
	     q ? CPP_N_MD_Q :
	     fn ? CPP_N_FLOATN | (fn_bits << CPP_FLOATN_SHIFT) :
	     fnx ? CPP_N_FLOATNX | (fn_bits << CPP_FLOATN_SHIFT) :
	     bf16 ? CPP_N_BFLOAT16 :
	     CPP_N_DEFAULT));
}

/* Return the classification flags for a float suffix.  */
unsigned int
cpp_interpret_float_suffix (cpp_reader *pfile, const char *s, size_t len)
{
  return interpret_float_suffix (pfile, (const unsigned char *)s, len);
}

/* Subroutine of cpp_classify_number.  S points to an integer suffix
   of length LEN, possibly zero. Returns 0 for an invalid suffix, or a
   flag vector describing the suffix.  */
static unsigned int
interpret_int_suffix (cpp_reader *pfile, const uchar *s, size_t len)
{
  size_t orig_len = len;
  size_t u, l, i, z, wb;

  u = l = i = z = wb = 0;

  while (len--)
    switch (s[len])
      {
      case 'z': case 'Z':	z++; break;
      case 'u': case 'U':	u++; break;
      case 'i': case 'I':
      case 'j': case 'J':	i++; break;
      case 'l': case 'L':	l++;
	/* If there are two Ls, they must be adjacent and the same case.  */
	if (l == 2 && s[len] != s[len + 1])
	  return 0;
	break;
      case 'b':
	if (len == 0 || s[len - 1] != 'w')
	  return 0;
	wb++;
	len--;
	break;
      case 'B':
	if (len == 0 || s[len - 1] != 'W')
	  return 0;
	wb++;
	len--;
	break;
      default:
	return 0;
      }

  if (l > 2 || u > 1 || i > 1 || z > 1 || wb > 1)
    return 0;

  if (z)
    {
      if (l > 0 || i > 0)
	return 0;
      if (!CPP_OPTION (pfile, cplusplus))
	return 0;
    }

  if (wb)
    {
      if (CPP_OPTION (pfile, cplusplus))
	return 0;
      if (l > 0 || i > 0 || z > 0)
	return 0;
    }

  if (i)
    {
      if (!CPP_OPTION (pfile, ext_numeric_literals))
	return 0;

      /* In C++14 and up these suffixes are in the standard library, so treat
	 them as user-defined literals.  */
      if (CPP_OPTION (pfile, cplusplus)
	  && CPP_OPTION (pfile, lang) > CLK_CXX11
	  && s[0] == 'i'
	  && (orig_len == 1 || (orig_len == 2 && s[1] == 'l')))
	return 0;
    }

  return ((i ? CPP_N_IMAGINARY : 0)
	  | (u ? CPP_N_UNSIGNED : 0)
	  | ((l == 0) ? CPP_N_SMALL
	     : (l == 1) ? CPP_N_MEDIUM : CPP_N_LARGE)
	  | (z ? CPP_N_SIZE_T : 0)
	  | (wb ? CPP_N_BITINT : 0));
}

/* Return the classification flags for an int suffix.  */
unsigned int
cpp_interpret_int_suffix (cpp_reader *pfile, const char *s, size_t len)
{
  return interpret_int_suffix (pfile, (const unsigned char *)s, len);
}

/* Return the string type corresponding to the the input user-defined string
   literal type.  If the input type is not a user-defined string literal
   type return the input type.  */
enum cpp_ttype
cpp_userdef_string_remove_type (enum cpp_ttype type)
{
  if (type == CPP_STRING_USERDEF)
    return CPP_STRING;
  else if (type == CPP_WSTRING_USERDEF)
    return CPP_WSTRING;
  else if (type == CPP_STRING16_USERDEF)
    return CPP_STRING16;
  else if (type == CPP_STRING32_USERDEF)
    return CPP_STRING32;
  else if (type == CPP_UTF8STRING_USERDEF)
    return CPP_UTF8STRING;
  else
    return type;
}

/* Return the user-defined string literal type corresponding to the input
   string type.  If the input type is not a string type return the input
   type.  */
enum cpp_ttype
cpp_userdef_string_add_type (enum cpp_ttype type)
{
  if (type == CPP_STRING)
    return CPP_STRING_USERDEF;
  else if (type == CPP_WSTRING)
    return CPP_WSTRING_USERDEF;
  else if (type == CPP_STRING16)
    return CPP_STRING16_USERDEF;
  else if (type == CPP_STRING32)
    return CPP_STRING32_USERDEF;
  else if (type == CPP_UTF8STRING)
    return CPP_UTF8STRING_USERDEF;
  else
    return type;
}

/* Return the char type corresponding to the the input user-defined char
   literal type.  If the input type is not a user-defined char literal
   type return the input type.  */
enum cpp_ttype
cpp_userdef_char_remove_type (enum cpp_ttype type)
{
  if (type == CPP_CHAR_USERDEF)
    return CPP_CHAR;
  else if (type == CPP_WCHAR_USERDEF)
    return CPP_WCHAR;
  else if (type == CPP_CHAR16_USERDEF)
    return CPP_CHAR16;
  else if (type == CPP_CHAR32_USERDEF)
    return CPP_CHAR32;
  else if (type == CPP_UTF8CHAR_USERDEF)
    return CPP_UTF8CHAR;
  else
    return type;
}

/* Return the user-defined char literal type corresponding to the input
   char type.  If the input type is not a char type return the input
   type.  */
enum cpp_ttype
cpp_userdef_char_add_type (enum cpp_ttype type)
{
  if (type == CPP_CHAR)
    return CPP_CHAR_USERDEF;
  else if (type == CPP_WCHAR)
    return CPP_WCHAR_USERDEF;
  else if (type == CPP_CHAR16)
    return CPP_CHAR16_USERDEF;
  else if (type == CPP_CHAR32)
    return CPP_CHAR32_USERDEF;
  else if (type == CPP_UTF8CHAR)
    return CPP_UTF8CHAR_USERDEF;
  else
    return type;
}

/* Return true if the token type is a user-defined string literal.  */
bool
cpp_userdef_string_p (enum cpp_ttype type)
{
  if (type == CPP_STRING_USERDEF
   || type == CPP_WSTRING_USERDEF
   || type == CPP_STRING16_USERDEF
   || type == CPP_STRING32_USERDEF
   || type == CPP_UTF8STRING_USERDEF)
    return true;
  else
    return false;
}

/* Return true if the token type is a user-defined char literal.  */
bool
cpp_userdef_char_p (enum cpp_ttype type)
{
  if (type == CPP_CHAR_USERDEF
   || type == CPP_WCHAR_USERDEF
   || type == CPP_CHAR16_USERDEF
   || type == CPP_CHAR32_USERDEF
   || type == CPP_UTF8CHAR_USERDEF)
    return true;
  else
    return false;
}

/* Extract the suffix from a user-defined literal string or char.  */
const char *
cpp_get_userdef_suffix (const cpp_token *tok)
{
  unsigned int len = tok->val.str.len;
  const char *text = (const char *)tok->val.str.text;
  char delim;
  unsigned int i;
  for (i = 0; i < len; ++i)
    if (text[i] == '\'' || text[i] == '"')
      break;
  if (i == len)
    return text + len;
  delim = text[i];
  for (i = len; i > 0; --i)
    if (text[i - 1] == delim)
      break;
  return text + i;
}

/* Categorize numeric constants according to their field (integer,
   floating point, or invalid), radix (decimal, octal, hexadecimal),
   and type suffixes.

   TOKEN is the token that represents the numeric constant to
   classify.

   In C++0X if UD_SUFFIX is non null it will be assigned
   any unrecognized suffix for a user-defined literal.

   VIRTUAL_LOCATION is the virtual location for TOKEN.  */
unsigned int
cpp_classify_number (cpp_reader *pfile, const cpp_token *token,
		     const char **ud_suffix, location_t virtual_location)
{
  const uchar *str = token->val.str.text;
  const uchar *limit;
  unsigned int max_digit, result, radix;
  enum {NOT_FLOAT = 0, AFTER_POINT, AFTER_EXPON} float_flag;
  bool seen_digit;
  bool seen_digit_sep;

  if (ud_suffix)
    *ud_suffix = NULL;

  /* If the lexer has done its job, length one can only be a single
     digit.  Fast-path this very common case.  */
  if (token->val.str.len == 1)
    return CPP_N_INTEGER | CPP_N_SMALL | CPP_N_DECIMAL;

  limit = str + token->val.str.len;
  float_flag = NOT_FLOAT;
  max_digit = 0;
  radix = 10;
  seen_digit = false;
  seen_digit_sep = false;

  /* First, interpret the radix.  */
  if (*str == '0')
    {
      radix = 8;
      str++;

      /* Require at least one hex digit to classify it as hex.  */
      if (*str == 'x' || *str == 'X')
	{
	  if (str[1] == '.' || ISXDIGIT (str[1]))
	    {
	      radix = 16;
	      str++;
	    }
	  else if (DIGIT_SEP (str[1]))
	    SYNTAX_ERROR_AT (virtual_location,
			     "digit separator after base indicator");
	}
      else if (*str == 'b' || *str == 'B')
	{
	  if (str[1] == '0' || str[1] == '1')
	    {
	      radix = 2;
	      str++;
	    }
	  else if (DIGIT_SEP (str[1]))
	    SYNTAX_ERROR_AT (virtual_location,
			     "digit separator after base indicator");
	}
    }

  /* Now scan for a well-formed integer or float.  */
  for (;;)
    {
      unsigned int c = *str++;

      if (ISDIGIT (c) || (ISXDIGIT (c) && radix == 16))
	{
	  seen_digit_sep = false;
	  seen_digit = true;
	  c = hex_value (c);
	  if (c > max_digit)
	    max_digit = c;
	}
      else if (DIGIT_SEP (c))
	seen_digit_sep = true;
      else if (c == '.')
	{
	  if (seen_digit_sep || DIGIT_SEP (*str))
	    SYNTAX_ERROR_AT (virtual_location,
			     "digit separator adjacent to decimal point");
	  seen_digit_sep = false;
	  if (float_flag == NOT_FLOAT)
	    float_flag = AFTER_POINT;
	  else
	    SYNTAX_ERROR_AT (virtual_location,
			     "too many decimal points in number");
	}
      else if ((radix <= 10 && (c == 'e' || c == 'E'))
	       || (radix == 16 && (c == 'p' || c == 'P')))
	{
	  if (seen_digit_sep || DIGIT_SEP (*str))
	    SYNTAX_ERROR_AT (virtual_location,
			     "digit separator adjacent to exponent");
	  float_flag = AFTER_EXPON;
	  break;
	}
      else
	{
	  /* Start of suffix.  */
	  str--;
	  break;
	}
    }

  if (seen_digit_sep && float_flag != AFTER_EXPON)
    SYNTAX_ERROR_AT (virtual_location,
		     "digit separator outside digit sequence");

  /* The suffix may be for decimal fixed-point constants without exponent.  */
  if (radix != 16 && float_flag == NOT_FLOAT)
    {
      result = interpret_float_suffix (pfile, str, limit - str);
      if ((result & CPP_N_FRACT) || (result & CPP_N_ACCUM))
	{
	  result |= CPP_N_FLOATING;
	  /* We need to restore the radix to 10, if the radix is 8.  */
	  if (radix == 8)
	    radix = 10;

	  if (CPP_PEDANTIC (pfile))
	    cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
				 "fixed-point constants are a GCC extension");
	  goto syntax_ok;
	}
      else
	result = 0;
    }

  if (float_flag != NOT_FLOAT && radix == 8)
    radix = 10;

  if (max_digit >= radix)
    {
      if (radix == 2)
	SYNTAX_ERROR2_AT (virtual_location,
			  "invalid digit \"%c\" in binary constant", '0' + max_digit);
      else
	SYNTAX_ERROR2_AT (virtual_location,
			  "invalid digit \"%c\" in octal constant", '0' + max_digit);
    }

  if (float_flag != NOT_FLOAT)
    {
      if (radix == 2)
	{
	  cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
			       "invalid prefix \"0b\" for floating constant");
	  return CPP_N_INVALID;
	}

      if (radix == 16 && !seen_digit)
	SYNTAX_ERROR_AT (virtual_location,
			 "no digits in hexadecimal floating constant");

      if (radix == 16 && CPP_PEDANTIC (pfile)
	  && !CPP_OPTION (pfile, extended_numbers))
	{
	  if (CPP_OPTION (pfile, cplusplus))
	    cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
				 "use of C++17 hexadecimal floating constant");
	  else
	    cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
				 "use of C99 hexadecimal floating constant");
	}

      if (float_flag == AFTER_EXPON)
	{
	  if (*str == '+' || *str == '-')
	    str++;

	  /* Exponent is decimal, even if string is a hex float.  */
	  if (!ISDIGIT (*str))
	    {
	      if (DIGIT_SEP (*str))
		SYNTAX_ERROR_AT (virtual_location,
				 "digit separator adjacent to exponent");
	      else
		SYNTAX_ERROR_AT (virtual_location, "exponent has no digits");
	    }
	  do
	    {
	      seen_digit_sep = DIGIT_SEP (*str);
	      str++;
	    }
	  while (ISDIGIT (*str) || DIGIT_SEP (*str));
	}
      else if (radix == 16)
	SYNTAX_ERROR_AT (virtual_location,
			 "hexadecimal floating constants require an exponent");

      if (seen_digit_sep)
	SYNTAX_ERROR_AT (virtual_location,
			 "digit separator outside digit sequence");

      result = interpret_float_suffix (pfile, str, limit - str);
      if (result == 0)
	{
	  if (CPP_OPTION (pfile, user_literals))
	    {
	      if (ud_suffix)
		*ud_suffix = (const char *) str;
	      result = CPP_N_LARGE | CPP_N_USERDEF;
	    }
	  else
	    {
	      cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
				   "invalid suffix \"%.*s\" on floating constant",
				   (int) (limit - str), str);
	      return CPP_N_INVALID;
	    }
	}

      /* Traditional C didn't accept any floating suffixes.  */
      if (limit != str
	  && CPP_WTRADITIONAL (pfile)
	  && ! cpp_sys_macro_p (pfile))
	cpp_warning_with_line (pfile, CPP_W_TRADITIONAL, virtual_location, 0,
			       "traditional C rejects the \"%.*s\" suffix",
			       (int) (limit - str), str);

      /* A suffix for double is a GCC extension via decimal float support.
	 If the suffix also specifies an imaginary value we'll catch that
	 later.  */
      if ((result == CPP_N_MEDIUM) && CPP_PEDANTIC (pfile))
	cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
			     "suffix for double constant is a GCC extension");

      /* Radix must be 10 for decimal floats.  */
      if ((result & CPP_N_DFLOAT) && radix != 10)
        {
          cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
			       "invalid suffix \"%.*s\" with hexadecimal floating constant",
			       (int) (limit - str), str);
          return CPP_N_INVALID;
        }

      if ((result & (CPP_N_FRACT | CPP_N_ACCUM)) && CPP_PEDANTIC (pfile))
	cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
			     "fixed-point constants are a GCC extension");

      if (result & CPP_N_DFLOAT)
	{
	  if (CPP_PEDANTIC (pfile) && !CPP_OPTION (pfile, dfp_constants))
	    cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
				 "decimal float constants are a C23 feature");
	  else if (CPP_OPTION (pfile, cpp_warn_c11_c23_compat) > 0)
	    cpp_warning_with_line (pfile, CPP_W_C11_C23_COMPAT,
				   virtual_location, 0,
				   "decimal float constants are a C23 feature");
	}

      result |= CPP_N_FLOATING;
    }
  else
    {
      result = interpret_int_suffix (pfile, str, limit - str);
      if (result == 0)
	{
	  if (CPP_OPTION (pfile, user_literals))
	    {
	      if (ud_suffix)
		*ud_suffix = (const char *) str;
	      result = CPP_N_UNSIGNED | CPP_N_LARGE | CPP_N_USERDEF;
	    }
	  else
	    {
	      cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
				   "invalid suffix \"%.*s\" on integer constant",
				   (int) (limit - str), str);
	      return CPP_N_INVALID;
	    }
	}

      /* Traditional C only accepted the 'L' suffix.
         Suppress warning about 'LL' with -Wno-long-long.  */
      if (CPP_WTRADITIONAL (pfile) && ! cpp_sys_macro_p (pfile))
	{
	  int u_or_i = (result & (CPP_N_UNSIGNED|CPP_N_IMAGINARY));
	  int large = (result & CPP_N_WIDTH) == CPP_N_LARGE
		       && CPP_OPTION (pfile, cpp_warn_long_long);

	  if (u_or_i || large)
	    cpp_warning_with_line (pfile, large ? CPP_W_LONG_LONG : CPP_W_TRADITIONAL,
				   virtual_location, 0,
				   "traditional C rejects the \"%.*s\" suffix",
				   (int) (limit - str), str);
	}

      if ((result & CPP_N_WIDTH) == CPP_N_LARGE
	  && CPP_OPTION (pfile, cpp_warn_long_long))
        {
          const char *message = CPP_OPTION (pfile, cplusplus) 
				? N_("use of C++11 long long integer constant")
		                : N_("use of C99 long long integer constant");

	  if (CPP_OPTION (pfile, c99))
            cpp_warning_with_line (pfile, CPP_W_LONG_LONG, virtual_location,
				   0, message);
          else
            cpp_pedwarning_with_line (pfile, CPP_W_LONG_LONG,
				      virtual_location, 0, message);
        }

      if ((result & CPP_N_SIZE_T) == CPP_N_SIZE_T
	  && !CPP_OPTION (pfile, size_t_literals))
       {
	  const char *message = (result & CPP_N_UNSIGNED) == CPP_N_UNSIGNED
				? N_("use of C++23 %<size_t%> integer constant")
				: N_("use of C++23 %<make_signed_t<size_t>%> integer constant");
	  cpp_warning_with_line (pfile, CPP_W_SIZE_T_LITERALS,
				 virtual_location, 0, message);
       }

      if ((result & CPP_N_BITINT) != 0
	  && CPP_OPTION (pfile, cpp_warn_c11_c23_compat) != 0)
	{
	  if (CPP_OPTION (pfile, cpp_warn_c11_c23_compat) > 0)
	    {
	      const char *message = N_("ISO C does not support literal "
				       "%<wb%> suffixes before C23");
	      if (CPP_PEDANTIC (pfile) && !CPP_OPTION (pfile, true_false))
		cpp_pedwarning_with_line (pfile, CPP_W_C11_C23_COMPAT,
					  virtual_location, 0, message);
	      else
		cpp_warning_with_line (pfile, CPP_W_C11_C23_COMPAT,
				       virtual_location, 0, message);
	    }
	  else if (CPP_PEDANTIC (pfile) && !CPP_OPTION (pfile, true_false))
	    {
	      const char *message = N_("ISO C does not support literal "
				       "%<wb%> suffixes before C23");
	      cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
				   message);
	    }
	}

      result |= CPP_N_INTEGER;
    }

 syntax_ok:
  if ((result & CPP_N_IMAGINARY) && CPP_PEDANTIC (pfile))
    cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
			 "imaginary constants are a GCC extension");
  if (radix == 2)
    {
      if (!CPP_OPTION (pfile, binary_constants)
	  && CPP_PEDANTIC (pfile))
	cpp_error_with_line (pfile, CPP_DL_PEDWARN, virtual_location, 0,
			     CPP_OPTION (pfile, cplusplus)
			     ? N_("binary constants are a C++14 feature "
				  "or GCC extension")
			     : N_("binary constants are a C23 feature "
				  "or GCC extension"));
      else if (CPP_OPTION (pfile, cpp_warn_c11_c23_compat) > 0)
	cpp_warning_with_line (pfile, CPP_W_C11_C23_COMPAT,
			       virtual_location, 0,
			       "binary constants are a C23 feature");
    }

  if (radix == 10)
    result |= CPP_N_DECIMAL;
  else if (radix == 16)
    result |= CPP_N_HEX;
  else if (radix == 2)
    result |= CPP_N_BINARY;
  else
    result |= CPP_N_OCTAL;

  return result;

 syntax_error:
  return CPP_N_INVALID;
}

/* cpp_interpret_integer converts an integer constant into a cpp_num,
   of precision options->precision.

   We do not provide any interface for decimal->float conversion,
   because the preprocessor doesn't need it and we don't want to
   drag in GCC's floating point emulator.  */
cpp_num
cpp_interpret_integer (cpp_reader *pfile, const cpp_token *token,
		       unsigned int type)
{
  const uchar *p, *end;
  cpp_num result;

  result.low = 0;
  result.high = 0;
  result.unsignedp = !!(type & CPP_N_UNSIGNED);
  result.overflow = false;

  p = token->val.str.text;
  end = p + token->val.str.len;

  /* Common case of a single digit.  */
  if (token->val.str.len == 1)
    result.low = p[0] - '0';
  else
    {
      cpp_num_part max;
      size_t precision = CPP_OPTION (pfile, precision);
      unsigned int base = 10, c = 0;
      bool overflow = false;

      if ((type & CPP_N_RADIX) == CPP_N_OCTAL)
	{
	  base = 8;
	  p++;
	}
      else if ((type & CPP_N_RADIX) == CPP_N_HEX)
	{
	  base = 16;
	  p += 2;
	}
      else if ((type & CPP_N_RADIX) == CPP_N_BINARY)
	{
	  base = 2;
	  p += 2;
	}

      /* We can add a digit to numbers strictly less than this without
	 needing the precision and slowness of double integers.  */
      max = ~(cpp_num_part) 0;
      if (precision < PART_PRECISION)
	max >>= PART_PRECISION - precision;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
	{
	  c = *p;

	  if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
	    c = hex_value (c);
	  else if (DIGIT_SEP (c))
	    continue;
	  else
	    break;

	  /* Strict inequality for when max is set to zero.  */
	  if (result.low < max)
	    result.low = result.low * base + c;
	  else
	    {
	      result = append_digit (result, c, base, precision);
	      overflow |= result.overflow;
	      max = 0;
	    }
	}

      if (overflow && !(type & CPP_N_USERDEF))
	cpp_error (pfile, CPP_DL_PEDWARN,
		   "integer constant is too large for its type");
      /* If too big to be signed, consider it unsigned.  Only warn for
	 decimal numbers.  Traditional numbers were always signed (but
	 we still honor an explicit U suffix); but we only have
	 traditional semantics in directives.  */
      else if (!result.unsignedp
	       && !(CPP_OPTION (pfile, traditional)
		    && pfile->state.in_directive)
	       && !num_positive (result, precision))
	{
	  /* This is for constants within the range of uintmax_t but
	     not that of intmax_t.  For such decimal constants, a
	     diagnostic is required for C99 as the selected type must
	     be signed and not having a type is a constraint violation
	     (DR#298, TC3), so this must be a pedwarn.  For C90,
	     unsigned long is specified to be used for a constant that
	     does not fit in signed long; if uintmax_t has the same
	     range as unsigned long this means only a warning is
	     appropriate here.  C90 permits the preprocessor to use a
	     wider range than unsigned long in the compiler, so if
	     uintmax_t is wider than unsigned long no diagnostic is
	     required for such constants in preprocessor #if
	     expressions and the compiler will pedwarn for such
	     constants outside the range of unsigned long that reach
	     the compiler so a diagnostic is not required there
	     either; thus, pedwarn for C99 but use a plain warning for
	     C90.  */
	  if (base == 10)
	    cpp_error (pfile, (CPP_OPTION (pfile, c99)
			       ? CPP_DL_PEDWARN
			       : CPP_DL_WARNING),
		       "integer constant is so large that it is unsigned");
	  result.unsignedp = true;
	}
    }

  return result;
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base BASE.  */
static cpp_num
append_digit (cpp_num num, int digit, int base, size_t precision)
{
  cpp_num result;
  unsigned int shift;
  bool overflow;
  cpp_num_part add_high, add_low;

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we don't
     need to worry about add_high overflowing.  */
  switch (base)
    {
    case 2:
      shift = 1;
      break;

    case 16:
      shift = 4;
      break;

    default:
      shift = 3;
    }
  overflow = !!(num.high >> (PART_PRECISION - shift));
  result.high = num.high << shift;
  result.low = num.low << shift;
  result.high |= num.low >> (PART_PRECISION - shift);
  result.unsignedp = num.unsignedp;

  if (base == 10)
    {
      add_low = num.low << 1;
      add_high = (num.high << 1) + (num.low >> (PART_PRECISION - 1));
    }
  else
    add_high = add_low = 0;

  if (add_low + digit < add_low)
    add_high++;
  add_low += digit;

  if (result.low + add_low < result.low)
    add_high++;
  if (result.high + add_high < result.high)
    overflow = true;

  result.low += add_low;
  result.high += add_high;
  result.overflow = overflow;

  /* The above code catches overflow of a cpp_num type.  This catches
     overflow of the (possibly shorter) target precision.  */
  num.low = result.low;
  num.high = result.high;
  result = num_trim (result, precision);
  if (!num_eq (result, num))
    result.overflow = true;

  return result;
}

/* Handle meeting "defined" in a preprocessor expression.  */
static cpp_num
parse_defined (cpp_reader *pfile)
{
  cpp_num result;
  int paren = 0;
  cpp_hashnode *node = 0;
  const cpp_token *token;
  cpp_context *initial_context = pfile->context;

  /* Don't expand macros.  */
  pfile->state.prevent_expansion++;

  token = cpp_get_token (pfile);
  if (token->type == CPP_OPEN_PAREN)
    {
      paren = 1;
      token = cpp_get_token (pfile);
    }

  if (token->type == CPP_NAME)
    {
      node = token->val.node.node;
      if (paren && cpp_get_token (pfile)->type != CPP_CLOSE_PAREN)
	{
	  cpp_error (pfile, CPP_DL_ERROR, "missing ')' after \"defined\"");
	  node = 0;
	}
    }
  else
    {
      cpp_error (pfile, CPP_DL_ERROR,
		 "operator \"defined\" requires an identifier");
      if (token->flags & NAMED_OP)
	{
	  cpp_token op;

	  op.flags = 0;
	  op.type = token->type;
	  cpp_error (pfile, CPP_DL_ERROR,
		     "(\"%s\" is an alternative token for \"%s\" in C++)",
		     cpp_token_as_text (pfile, token),
		     cpp_token_as_text (pfile, &op));
	}
    }

  bool is_defined = false;
  if (node)
    {
      if ((pfile->context != initial_context
	   || initial_context != &pfile->base_context)
	  && CPP_OPTION (pfile, warn_expansion_to_defined))
        cpp_pedwarning (pfile, CPP_W_EXPANSION_TO_DEFINED,
		        "this use of \"defined\" may not be portable");
      is_defined = _cpp_defined_macro_p (node);
      if (!_cpp_maybe_notify_macro_use (pfile, node, token->src_loc))
	/* It wasn't a macro after all.  */
	is_defined = false;
      _cpp_mark_macro_used (node);

      /* A possible controlling macro of the form #if !defined ().
	 _cpp_parse_expr checks there was no other junk on the line.  */
      pfile->mi_ind_cmacro = node;
    }

  pfile->state.prevent_expansion--;

  /* Do not treat conditional macros as being defined.  This is due to the
     powerpc port using conditional macros for 'vector', 'bool', and 'pixel'
     to act as conditional keywords.  This messes up tests like #ifndef
     bool.  */
  result.unsignedp = false;
  result.high = 0;
  result.overflow = false;
  result.low = is_defined;
  return result;
}

/* Convert a token into a CPP_NUMBER (an interpreted preprocessing
   number or character constant, or the result of the "defined" or "#"
   operators).  */
static cpp_num
eval_token (cpp_reader *pfile, const cpp_token *token,
	    location_t virtual_location)
{
  cpp_num result;
  unsigned int temp;
  int unsignedp = 0;

  result.unsignedp = false;
  result.overflow = false;

  switch (token->type)
    {
    case CPP_NUMBER:
      temp = cpp_classify_number (pfile, token, NULL, virtual_location);
      if (temp & CPP_N_USERDEF)
	cpp_error (pfile, CPP_DL_ERROR,
		   "user-defined literal in preprocessor expression");
      switch (temp & CPP_N_CATEGORY)
	{
	case CPP_N_FLOATING:
	  cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
			       "floating constant in preprocessor expression");
	  break;
	case CPP_N_INTEGER:
	  if (!(temp & CPP_N_IMAGINARY))
	    return cpp_interpret_integer (pfile, token, temp);
	  cpp_error_with_line (pfile, CPP_DL_ERROR, virtual_location, 0,
			       "imaginary number in preprocessor expression");
	  break;

	case CPP_N_INVALID:
	  /* Error already issued.  */
	  break;
	}
      result.high = result.low = 0;
      break;

    case CPP_WCHAR:
    case CPP_CHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_UTF8CHAR:
      {
	cppchar_t cc = cpp_interpret_charconst (pfile, token,
						&temp, &unsignedp);

	result.high = 0;
	result.low = cc;
	/* Sign-extend the result if necessary.  */
	if (!unsignedp && (cppchar_signed_t) cc < 0)
	  {
	    if (PART_PRECISION > BITS_PER_CPPCHAR_T)
	      result.low |= ~(~(cpp_num_part) 0
			      >> (PART_PRECISION - BITS_PER_CPPCHAR_T));
	    result.high = ~(cpp_num_part) 0;
	    result = num_trim (result, CPP_OPTION (pfile, precision));
	  }
      }
      break;

    case CPP_NAME:
      if (token->val.node.node == pfile->spec_nodes.n_defined)
	return parse_defined (pfile);
      else if (CPP_OPTION (pfile, true_false)
	       && (token->val.node.node == pfile->spec_nodes.n_true
		   || token->val.node.node == pfile->spec_nodes.n_false))
	{
	  result.high = 0;
	  result.low = (token->val.node.node == pfile->spec_nodes.n_true);
	}
      else
	{
	  result.high = 0;
	  result.low = 0;
	  if (CPP_OPTION (pfile, warn_undef) && !pfile->state.skip_eval)
	    cpp_warning_with_line (pfile, CPP_W_UNDEF, virtual_location, 0,
				   "\"%s\" is not defined, evaluates to 0",
				   NODE_NAME (token->val.node.node));
	}
      break;

    case CPP_HASH:
      if (!pfile->state.skipping)
	{
	  /* A pedantic warning takes precedence over a deprecated
	     warning here.  */
	  if (CPP_PEDANTIC (pfile))
	    cpp_error_with_line (pfile, CPP_DL_PEDWARN,
				 virtual_location, 0,
				 "assertions are a GCC extension");
	  else if (CPP_OPTION (pfile, cpp_warn_deprecated))
	    cpp_warning_with_line (pfile, CPP_W_DEPRECATED, virtual_location, 0,
				   "assertions are a deprecated extension");
	}
      _cpp_test_assertion (pfile, &temp);
      result.high = 0;
      result.low = temp;
      break;

    default:
      abort ();
    }

  result.unsignedp = !!unsignedp;
  return result;
}

/* Operator precedence and flags table.

After an operator is returned from the lexer, if it has priority less
than the operator on the top of the stack, we reduce the stack by one
operator and repeat the test.  Since equal priorities do not reduce,
this is naturally right-associative.

We handle left-associative operators by decrementing the priority of
just-lexed operators by one, but retaining the priority of operators
already on the stack.

The remaining cases are '(' and ')'.  We handle '(' by skipping the
reduction phase completely.  ')' is given lower priority than
everything else, including '(', effectively forcing a reduction of the
parenthesized expression.  If there is a matching '(', the routine
reduce() exits immediately.  If the normal exit route sees a ')', then
there cannot have been a matching '(' and an error message is output.

The parser assumes all shifted operators require a left operand unless
the flag NO_L_OPERAND is set.  These semantics are automatic; any
extra semantics need to be handled with operator-specific code.  */

/* Flags.  If CHECK_PROMOTION, we warn if the effective sign of an
   operand changes because of integer promotions.  */
#define NO_L_OPERAND	(1 << 0)
#define LEFT_ASSOC	(1 << 1)
#define CHECK_PROMOTION	(1 << 2)

/* Operator to priority map.  Must be in the same order as the first
   N entries of enum cpp_ttype.  */
static const struct cpp_operator
{
  uchar prio;
  uchar flags;
} optab[] =
{
  /* EQ */		{0, 0},	/* Shouldn't happen.  */
  /* NOT */		{16, NO_L_OPERAND},
  /* GREATER */		{12, LEFT_ASSOC | CHECK_PROMOTION},
  /* LESS */		{12, LEFT_ASSOC | CHECK_PROMOTION},
  /* PLUS */		{14, LEFT_ASSOC | CHECK_PROMOTION},
  /* MINUS */		{14, LEFT_ASSOC | CHECK_PROMOTION},
  /* MULT */		{15, LEFT_ASSOC | CHECK_PROMOTION},
  /* DIV */		{15, LEFT_ASSOC | CHECK_PROMOTION},
  /* MOD */		{15, LEFT_ASSOC | CHECK_PROMOTION},
  /* AND */		{9, LEFT_ASSOC | CHECK_PROMOTION},
  /* OR */		{7, LEFT_ASSOC | CHECK_PROMOTION},
  /* XOR */		{8, LEFT_ASSOC | CHECK_PROMOTION},
  /* RSHIFT */		{13, LEFT_ASSOC},
  /* LSHIFT */		{13, LEFT_ASSOC},

  /* COMPL */		{16, NO_L_OPERAND},
  /* AND_AND */		{6, LEFT_ASSOC},
  /* OR_OR */		{5, LEFT_ASSOC},
  /* Note that QUERY, COLON, and COMMA must have the same precedence.
     However, there are some special cases for these in reduce().  */
  /* QUERY */		{4, 0},
  /* COLON */		{4, LEFT_ASSOC | CHECK_PROMOTION},
  /* COMMA */		{4, LEFT_ASSOC},
  /* OPEN_PAREN */	{1, NO_L_OPERAND},
  /* CLOSE_PAREN */	{0, 0},
  /* EOF */		{0, 0},
  /* EQ_EQ */		{11, LEFT_ASSOC},
  /* NOT_EQ */		{11, LEFT_ASSOC},
  /* GREATER_EQ */	{12, LEFT_ASSOC | CHECK_PROMOTION},
  /* LESS_EQ */		{12, LEFT_ASSOC | CHECK_PROMOTION},
  /* UPLUS */		{16, NO_L_OPERAND},
  /* UMINUS */		{16, NO_L_OPERAND}
};

/* Parse and evaluate a C expression, reading from PFILE.
   Returns the truth value of the expression.

   The implementation is an operator precedence parser, i.e. a
   bottom-up parser, using a stack for not-yet-reduced tokens.

   The stack base is op_stack, and the current stack pointer is 'top'.
   There is a stack element for each operator (only), and the most
   recently pushed operator is 'top->op'.  An operand (value) is
   stored in the 'value' field of the stack element of the operator
   that precedes it.  */
bool
_cpp_parse_expr (cpp_reader *pfile, bool is_if)
{
  struct op *top = pfile->op_stack;
  unsigned int lex_count;
  bool saw_leading_not, want_value = true;
  location_t virtual_location = 0;

  pfile->state.skip_eval = 0;

  /* Set up detection of #if ! defined().  */
  pfile->mi_ind_cmacro = 0;
  saw_leading_not = false;
  lex_count = 0;

  /* Lowest priority operator prevents further reductions.  */
  top->op = CPP_EOF;

  for (;;)
    {
      struct op op;

      lex_count++;
      op.token = cpp_get_token_with_location (pfile, &virtual_location);
      op.op = op.token->type;
      op.loc = virtual_location;

      switch (op.op)
	{
	  /* These tokens convert into values.  */
	case CPP_NUMBER:
	case CPP_CHAR:
	case CPP_WCHAR:
	case CPP_CHAR16:
	case CPP_CHAR32:
	case CPP_UTF8CHAR:
	case CPP_NAME:
	case CPP_HASH:
	  if (!want_value)
	    SYNTAX_ERROR2_AT (op.loc,
			      "missing binary operator before token \"%s\"",
			      cpp_token_as_text (pfile, op.token));
	  want_value = false;
	  top->value = eval_token (pfile, op.token, op.loc);
	  continue;

	case CPP_NOT:
	  saw_leading_not = lex_count == 1;
	  break;
	case CPP_PLUS:
	  if (want_value)
	    op.op = CPP_UPLUS;
	  break;
	case CPP_MINUS:
	  if (want_value)
	    op.op = CPP_UMINUS;
	  break;

	case CPP_PADDING:
	  lex_count--;
	  continue;

	default:
	  if ((int) op.op <= (int) CPP_EQ || (int) op.op >= (int) CPP_PLUS_EQ)
	    SYNTAX_ERROR2_AT (op.loc,
			      "token \"%s\" is not valid in preprocessor expressions",
			      cpp_token_as_text (pfile, op.token));
	  break;
	}

      /* Check we have a value or operator as appropriate.  */
      if (optab[op.op].flags & NO_L_OPERAND)
	{
	  if (!want_value)
	    SYNTAX_ERROR2_AT (op.loc,
			      "missing binary operator before token \"%s\"",
			      cpp_token_as_text (pfile, op.token));
	}
      else if (want_value)
	{
	  /* We want a number (or expression) and haven't got one.
	     Try to emit a specific diagnostic.  */
	  if (op.op == CPP_CLOSE_PAREN && top->op == CPP_OPEN_PAREN)
	    SYNTAX_ERROR_AT (op.loc,
			     "missing expression between '(' and ')'");

	  if (op.op == CPP_EOF && top->op == CPP_EOF)
 	    SYNTAX_ERROR2_AT (op.loc,
			      "%s with no expression", is_if ? "#if" : "#elif");

 	  if (top->op != CPP_EOF && top->op != CPP_OPEN_PAREN)
 	    SYNTAX_ERROR2_AT (op.loc,
			      "operator '%s' has no right operand",
			      cpp_token_as_text (pfile, top->token));
	  else if (op.op == CPP_CLOSE_PAREN || op.op == CPP_EOF)
	    /* Complain about missing paren during reduction.  */;
	  else
	    SYNTAX_ERROR2_AT (op.loc,
			      "operator '%s' has no left operand",
			      cpp_token_as_text (pfile, op.token));
	}

      top = reduce (pfile, top, op.op);
      if (!top)
	goto syntax_error;

      if (op.op == CPP_EOF)
	break;

      switch (op.op)
	{
	case CPP_CLOSE_PAREN:
	  continue;
	case CPP_OR_OR:
	  if (!num_zerop (top->value))
	    pfile->state.skip_eval++;
	  break;
	case CPP_AND_AND:
	case CPP_QUERY:
	  if (num_zerop (top->value))
	    pfile->state.skip_eval++;
	  break;
	case CPP_COLON:
	  if (top->op != CPP_QUERY)
	    SYNTAX_ERROR_AT (op.loc,
			     " ':' without preceding '?'");
	  if (!num_zerop (top[-1].value)) /* Was '?' condition true?  */
	    pfile->state.skip_eval++;
	  else
	    pfile->state.skip_eval--;
	default:
	  break;
	}

      want_value = true;

      /* Check for and handle stack overflow.  */
      if (++top == pfile->op_limit)
	top = _cpp_expand_op_stack (pfile);

      top->op = op.op;
      top->token = op.token;
      top->loc = op.loc;
    }

  /* The controlling macro expression is only valid if we called lex 3
     times: <!> <defined expression> and <EOF>.  push_conditional ()
     checks that we are at top-of-file.  */
  if (pfile->mi_ind_cmacro && !(saw_leading_not && lex_count == 3))
    pfile->mi_ind_cmacro = 0;

  if (top != pfile->op_stack)
    {
      cpp_error_with_line (pfile, CPP_DL_ICE, top->loc, 0,
			   "unbalanced stack in %s",
			   is_if ? "#if" : "#elif");
    syntax_error:
      return false;  /* Return false on syntax error.  */
    }

  return !num_zerop (top->value);
}

/* Reduce the operator / value stack if possible, in preparation for
   pushing operator OP.  Returns NULL on error, otherwise the top of
   the stack.  */
static struct op *
reduce (cpp_reader *pfile, struct op *top, enum cpp_ttype op)
{
  unsigned int prio;

  if (top->op <= CPP_EQ || top->op > CPP_LAST_CPP_OP + 2)
    {
    bad_op:
      cpp_error (pfile, CPP_DL_ICE, "impossible operator '%u'", top->op);
      return 0;
    }

  if (op == CPP_OPEN_PAREN)
    return top;

  /* Decrement the priority of left-associative operators to force a
     reduction with operators of otherwise equal priority.  */
  prio = optab[op].prio - ((optab[op].flags & LEFT_ASSOC) != 0);
  while (prio < optab[top->op].prio)
    {
      if (CPP_OPTION (pfile, warn_num_sign_change)
	  && optab[top->op].flags & CHECK_PROMOTION)
	check_promotion (pfile, top);

      switch (top->op)
	{
	case CPP_UPLUS:
	case CPP_UMINUS:
	case CPP_NOT:
	case CPP_COMPL:
	  top[-1].value = num_unary_op (pfile, top->value, top->op);
	  top[-1].loc = top->loc;
	  break;

	case CPP_PLUS:
	case CPP_MINUS:
	case CPP_RSHIFT:
	case CPP_LSHIFT:
	case CPP_COMMA:
	  top[-1].value = num_binary_op (pfile, top[-1].value,
					 top->value, top->op);
	  top[-1].loc = top->loc;
	  break;

	case CPP_GREATER:
	case CPP_LESS:
	case CPP_GREATER_EQ:
	case CPP_LESS_EQ:
	  top[-1].value
	    = num_inequality_op (pfile, top[-1].value, top->value, top->op);
	  top[-1].loc = top->loc;
	  break;

	case CPP_EQ_EQ:
	case CPP_NOT_EQ:
	  top[-1].value
	    = num_equality_op (pfile, top[-1].value, top->value, top->op);
	  top[-1].loc = top->loc;
	  break;

	case CPP_AND:
	case CPP_OR:
	case CPP_XOR:
	  top[-1].value
	    = num_bitwise_op (pfile, top[-1].value, top->value, top->op);
	  top[-1].loc = top->loc;
	  break;

	case CPP_MULT:
	  top[-1].value = num_mul (pfile, top[-1].value, top->value);
	  top[-1].loc = top->loc;
	  break;

	case CPP_DIV:
	case CPP_MOD:
	  top[-1].value = num_div_op (pfile, top[-1].value,
				      top->value, top->op, top->loc);
	  top[-1].loc = top->loc;
	  break;

	case CPP_OR_OR:
	  top--;
	  if (!num_zerop (top->value))
	    pfile->state.skip_eval--;
	  top->value.low = (!num_zerop (top->value)
			    || !num_zerop (top[1].value));
	  top->value.high = 0;
	  top->value.unsignedp = false;
	  top->value.overflow = false;
	  top->loc = top[1].loc;
	  continue;

	case CPP_AND_AND:
	  top--;
	  if (num_zerop (top->value))
	    pfile->state.skip_eval--;
	  top->value.low = (!num_zerop (top->value)
			    && !num_zerop (top[1].value));
	  top->value.high = 0;
	  top->value.unsignedp = false;
	  top->value.overflow = false;
	  top->loc = top[1].loc;
	  continue;

	case CPP_OPEN_PAREN:
	  if (op != CPP_CLOSE_PAREN)
	    {
	      cpp_error_with_line (pfile, CPP_DL_ERROR, 
				   top->token->src_loc,
				   0, "missing ')' in expression");
	      return 0;
	    }
	  top--;
	  top->value = top[1].value;
	  top->loc = top[1].loc;
	  return top;

	case CPP_COLON:
	  top -= 2;
	  if (!num_zerop (top->value))
	    {
	      pfile->state.skip_eval--;
	      top->value = top[1].value;
	      top->loc = top[1].loc;
	    }
	  else
	    {
	      top->value = top[2].value;
	      top->loc = top[2].loc;
	    }
	  top->value.unsignedp = (top[1].value.unsignedp
				  || top[2].value.unsignedp);
	  continue;

	case CPP_QUERY:
	  /* COMMA and COLON should not reduce a QUERY operator.  */
	  if (op == CPP_COMMA || op == CPP_COLON)
	    return top;
	  cpp_error (pfile, CPP_DL_ERROR, "'?' without following ':'");
	  return 0;

	default:
	  goto bad_op;
	}

      top--;
      if (top->value.overflow && !pfile->state.skip_eval)
	cpp_error (pfile, CPP_DL_PEDWARN,
		   "integer overflow in preprocessor expression");
    }

  if (op == CPP_CLOSE_PAREN)
    {
      cpp_error (pfile, CPP_DL_ERROR, "missing '(' in expression");
      return 0;
    }

  return top;
}

/* Returns the position of the old top of stack after expansion.  */
struct op *
_cpp_expand_op_stack (cpp_reader *pfile)
{
  size_t old_size = (size_t) (pfile->op_limit - pfile->op_stack);
  size_t new_size = old_size * 2 + 20;

  pfile->op_stack = XRESIZEVEC (struct op, pfile->op_stack, new_size);
  pfile->op_limit = pfile->op_stack + new_size;

  return pfile->op_stack + old_size;
}

/* Emits a warning if the effective sign of either operand of OP
   changes because of integer promotions.  */
static void
check_promotion (cpp_reader *pfile, const struct op *op)
{
  if (op->value.unsignedp == op[-1].value.unsignedp)
    return;

  if (op->value.unsignedp)
    {
      if (!num_positive (op[-1].value, CPP_OPTION (pfile, precision)))
	cpp_error_with_line (pfile, CPP_DL_WARNING, op[-1].loc, 0,
			     "the left operand of \"%s\" changes sign when promoted",
			     cpp_token_as_text (pfile, op->token));
    }
  else if (!num_positive (op->value, CPP_OPTION (pfile, precision)))
    cpp_error_with_line (pfile, CPP_DL_WARNING, op->loc, 0,
	       "the right operand of \"%s\" changes sign when promoted",
	       cpp_token_as_text (pfile, op->token));
}

/* Clears the unused high order bits of the number pointed to by PNUM.  */
static cpp_num
num_trim (cpp_num num, size_t precision)
{
  if (precision > PART_PRECISION)
    {
      precision -= PART_PRECISION;
      if (precision < PART_PRECISION)
	num.high &= ((cpp_num_part) 1 << precision) - 1;
    }
  else
    {
      if (precision < PART_PRECISION)
	num.low &= ((cpp_num_part) 1 << precision) - 1;
      num.high = 0;
    }

  return num;
}

/* True iff A (presumed signed) >= 0.  */
static bool
num_positive (cpp_num num, size_t precision)
{
  if (precision > PART_PRECISION)
    {
      precision -= PART_PRECISION;
      return (num.high & (cpp_num_part) 1 << (precision - 1)) == 0;
    }

  return (num.low & (cpp_num_part) 1 << (precision - 1)) == 0;
}

/* Sign extend a number, with PRECISION significant bits and all
   others assumed clear, to fill out a cpp_num structure.  */
cpp_num
cpp_num_sign_extend (cpp_num num, size_t precision)
{
  if (!num.unsignedp)
    {
      if (precision > PART_PRECISION)
	{
	  precision -= PART_PRECISION;
	  if (precision < PART_PRECISION
	      && (num.high & (cpp_num_part) 1 << (precision - 1)))
	    num.high |= ~(~(cpp_num_part) 0 >> (PART_PRECISION - precision));
	}
      else if (num.low & (cpp_num_part) 1 << (precision - 1))
	{
	  if (precision < PART_PRECISION)
	    num.low |= ~(~(cpp_num_part) 0 >> (PART_PRECISION - precision));
	  num.high = ~(cpp_num_part) 0;
	}
    }

  return num;
}

/* Returns the negative of NUM.  */
static cpp_num
num_negate (cpp_num num, size_t precision)
{
  cpp_num copy;

  copy = num;
  num.high = ~num.high;
  num.low = ~num.low;
  if (++num.low == 0)
    num.high++;
  num = num_trim (num, precision);
  num.overflow = (!num.unsignedp && num_eq (num, copy) && !num_zerop (num));

  return num;
}

/* Returns true if A >= B.  */
static bool
num_greater_eq (cpp_num pa, cpp_num pb, size_t precision)
{
  bool unsignedp;

  unsignedp = pa.unsignedp || pb.unsignedp;

  if (!unsignedp)
    {
      /* Both numbers have signed type.  If they are of different
       sign, the answer is the sign of A.  */
      unsignedp = num_positive (pa, precision);

      if (unsignedp != num_positive (pb, precision))
	return unsignedp;

      /* Otherwise we can do an unsigned comparison.  */
    }

  return (pa.high > pb.high) || (pa.high == pb.high && pa.low >= pb.low);
}

/* Returns LHS OP RHS, where OP is a bit-wise operation.  */
static cpp_num
num_bitwise_op (cpp_reader *pfile ATTRIBUTE_UNUSED,
		cpp_num lhs, cpp_num rhs, enum cpp_ttype op)
{
  lhs.overflow = false;
  lhs.unsignedp = lhs.unsignedp || rhs.unsignedp;

  /* As excess precision is zeroed, there is no need to num_trim () as
     these operations cannot introduce a set bit there.  */
  if (op == CPP_AND)
    {
      lhs.low &= rhs.low;
      lhs.high &= rhs.high;
    }
  else if (op == CPP_OR)
    {
      lhs.low |= rhs.low;
      lhs.high |= rhs.high;
    }
  else
    {
      lhs.low ^= rhs.low;
      lhs.high ^= rhs.high;
    }

  return lhs;
}

/* Returns LHS OP RHS, where OP is an inequality.  */
static cpp_num
num_inequality_op (cpp_reader *pfile, cpp_num lhs, cpp_num rhs,
		   enum cpp_ttype op)
{
  bool gte = num_greater_eq (lhs, rhs, CPP_OPTION (pfile, precision));

  if (op == CPP_GREATER_EQ)
    lhs.low = gte;
  else if (op == CPP_LESS)
    lhs.low = !gte;
  else if (op == CPP_GREATER)
    lhs.low = gte && !num_eq (lhs, rhs);
  else /* CPP_LESS_EQ.  */
    lhs.low = !gte || num_eq (lhs, rhs);

  lhs.high = 0;
  lhs.overflow = false;
  lhs.unsignedp = false;
  return lhs;
}

/* Returns LHS OP RHS, where OP is == or !=.  */
static cpp_num
num_equality_op (cpp_reader *pfile ATTRIBUTE_UNUSED,
		 cpp_num lhs, cpp_num rhs, enum cpp_ttype op)
{
  /* Work around a 3.0.4 bug; see PR 6950.  */
  bool eq = num_eq (lhs, rhs);
  if (op == CPP_NOT_EQ)
    eq = !eq;
  lhs.low = eq;
  lhs.high = 0;
  lhs.overflow = false;
  lhs.unsignedp = false;
  return lhs;
}

/* Shift NUM, of width PRECISION, right by N bits.  */
static cpp_num
num_rshift (cpp_num num, size_t precision, size_t n)
{
  cpp_num_part sign_mask;
  bool x = num_positive (num, precision);

  if (num.unsignedp || x)
    sign_mask = 0;
  else
    sign_mask = ~(cpp_num_part) 0;

  if (n >= precision)
    num.high = num.low = sign_mask;
  else
    {
      /* Sign-extend.  */
      if (precision < PART_PRECISION)
	num.high = sign_mask, num.low |= sign_mask << precision;
      else if (precision < 2 * PART_PRECISION)
	num.high |= sign_mask << (precision - PART_PRECISION);

      if (n >= PART_PRECISION)
	{
	  n -= PART_PRECISION;
	  num.low = num.high;
	  num.high = sign_mask;
	}

      if (n)
	{
	  num.low = (num.low >> n) | (num.high << (PART_PRECISION - n));
	  num.high = (num.high >> n) | (sign_mask << (PART_PRECISION - n));
	}
    }

  num = num_trim (num, precision);
  num.overflow = false;
  return num;
}

/* Shift NUM, of width PRECISION, left by N bits.  */
static cpp_num
num_lshift (cpp_num num, size_t precision, size_t n)
{
  if (n >= precision)
    {
      num.overflow = !num.unsignedp && !num_zerop (num);
      num.high = num.low = 0;
    }
  else
    {
      cpp_num orig, maybe_orig;
      size_t m = n;

      orig = num;
      if (m >= PART_PRECISION)
	{
	  m -= PART_PRECISION;
	  num.high = num.low;
	  num.low = 0;
	}
      if (m)
	{
	  num.high = (num.high << m) | (num.low >> (PART_PRECISION - m));
	  num.low <<= m;
	}
      num = num_trim (num, precision);

      if (num.unsignedp)
	num.overflow = false;
      else
	{
	  maybe_orig = num_rshift (num, precision, n);
	  num.overflow = !num_eq (orig, maybe_orig);
	}
    }

  return num;
}

/* The four unary operators: +, -, ! and ~.  */
static cpp_num
num_unary_op (cpp_reader *pfile, cpp_num num, enum cpp_ttype op)
{
  switch (op)
    {
    case CPP_UPLUS:
      if (CPP_WTRADITIONAL (pfile) && !pfile->state.skip_eval)
	cpp_warning (pfile, CPP_W_TRADITIONAL,
		     "traditional C rejects the unary plus operator");
      num.overflow = false;
      break;

    case CPP_UMINUS:
      num = num_negate (num, CPP_OPTION (pfile, precision));
      break;

    case CPP_COMPL:
      num.high = ~num.high;
      num.low = ~num.low;
      num = num_trim (num, CPP_OPTION (pfile, precision));
      num.overflow = false;
      break;

    default: /* case CPP_NOT: */
      num.low = num_zerop (num);
      num.high = 0;
      num.overflow = false;
      num.unsignedp = false;
      break;
    }

  return num;
}

/* The various binary operators.  */
static cpp_num
num_binary_op (cpp_reader *pfile, cpp_num lhs, cpp_num rhs, enum cpp_ttype op)
{
  cpp_num result;
  size_t precision = CPP_OPTION (pfile, precision);
  size_t n;

  switch (op)
    {
      /* Shifts.  */
    case CPP_LSHIFT:
    case CPP_RSHIFT:
      if (!rhs.unsignedp && !num_positive (rhs, precision))
	{
	  /* A negative shift is a positive shift the other way.  */
	  if (op == CPP_LSHIFT)
	    op = CPP_RSHIFT;
	  else
	    op = CPP_LSHIFT;
	  rhs = num_negate (rhs, precision);
	}
      if (rhs.high)
	n = ~0;			/* Maximal.  */
      else
	n = rhs.low;
      if (op == CPP_LSHIFT)
	lhs = num_lshift (lhs, precision, n);
      else
	lhs = num_rshift (lhs, precision, n);
      break;

      /* Arithmetic.  */
    case CPP_MINUS:
      result.low = lhs.low - rhs.low;
      result.high = lhs.high - rhs.high;
      if (result.low > lhs.low)
	result.high--;
      result.unsignedp = lhs.unsignedp || rhs.unsignedp;
      result.overflow = false;

      result = num_trim (result, precision);
      if (!result.unsignedp)
	{
	  bool lhsp = num_positive (lhs, precision);
	  result.overflow = (lhsp != num_positive (rhs, precision)
			     && lhsp != num_positive (result, precision));
	}
      return result;

    case CPP_PLUS:
      result.low = lhs.low + rhs.low;
      result.high = lhs.high + rhs.high;
      if (result.low < lhs.low)
	result.high++;
      result.unsignedp = lhs.unsignedp || rhs.unsignedp;
      result.overflow = false;

      result = num_trim (result, precision);
      if (!result.unsignedp)
	{
	  bool lhsp = num_positive (lhs, precision);
	  result.overflow = (lhsp == num_positive (rhs, precision)
			     && lhsp != num_positive (result, precision));
	}
      return result;

      /* Comma.  */
    default: /* case CPP_COMMA: */
      if (CPP_PEDANTIC (pfile) && (!CPP_OPTION (pfile, c99)
				   || !pfile->state.skip_eval))
	cpp_pedwarning (pfile, CPP_W_PEDANTIC,
			"comma operator in operand of #if");
      lhs = rhs;
      break;
    }

  return lhs;
}

/* Multiplies two unsigned cpp_num_parts to give a cpp_num.  This
   cannot overflow.  */
static cpp_num
num_part_mul (cpp_num_part lhs, cpp_num_part rhs)
{
  cpp_num result;
  cpp_num_part middle[2], temp;

  result.low = LOW_PART (lhs) * LOW_PART (rhs);
  result.high = HIGH_PART (lhs) * HIGH_PART (rhs);

  middle[0] = LOW_PART (lhs) * HIGH_PART (rhs);
  middle[1] = HIGH_PART (lhs) * LOW_PART (rhs);

  temp = result.low;
  result.low += LOW_PART (middle[0]) << (PART_PRECISION / 2);
  if (result.low < temp)
    result.high++;

  temp = result.low;
  result.low += LOW_PART (middle[1]) << (PART_PRECISION / 2);
  if (result.low < temp)
    result.high++;

  result.high += HIGH_PART (middle[0]);
  result.high += HIGH_PART (middle[1]);
  result.unsignedp = true;
  result.overflow = false;

  return result;
}

/* Multiply two preprocessing numbers.  */
static cpp_num
num_mul (cpp_reader *pfile, cpp_num lhs, cpp_num rhs)
{
  cpp_num result, temp;
  bool unsignedp = lhs.unsignedp || rhs.unsignedp;
  bool overflow, negate = false;
  size_t precision = CPP_OPTION (pfile, precision);

  /* Prepare for unsigned multiplication.  */
  if (!unsignedp)
    {
      if (!num_positive (lhs, precision))
	negate = !negate, lhs = num_negate (lhs, precision);
      if (!num_positive (rhs, precision))
	negate = !negate, rhs = num_negate (rhs, precision);
    }

  overflow = lhs.high && rhs.high;
  result = num_part_mul (lhs.low, rhs.low);

  temp = num_part_mul (lhs.high, rhs.low);
  result.high += temp.low;
  if (temp.high)
    overflow = true;

  temp = num_part_mul (lhs.low, rhs.high);
  result.high += temp.low;
  if (temp.high)
    overflow = true;

  temp.low = result.low, temp.high = result.high;
  result = num_trim (result, precision);
  if (!num_eq (result, temp))
    overflow = true;

  if (negate)
    result = num_negate (result, precision);

  if (unsignedp)
    result.overflow = false;
  else
    result.overflow = overflow || (num_positive (result, precision) ^ !negate
				   && !num_zerop (result));
  result.unsignedp = unsignedp;

  return result;
}

/* Divide two preprocessing numbers, LHS and RHS, returning the answer
   or the remainder depending upon OP. LOCATION is the source location
   of this operator (for diagnostics).  */

static cpp_num
num_div_op (cpp_reader *pfile, cpp_num lhs, cpp_num rhs, enum cpp_ttype op,
	    location_t location)
{
  cpp_num result, sub;
  cpp_num_part mask;
  bool unsignedp = lhs.unsignedp || rhs.unsignedp;
  bool negate = false, lhs_neg = false;
  size_t i, precision = CPP_OPTION (pfile, precision);

  /* Prepare for unsigned division.  */
  if (!unsignedp)
    {
      if (!num_positive (lhs, precision))
	negate = !negate, lhs_neg = true, lhs = num_negate (lhs, precision);
      if (!num_positive (rhs, precision))
	negate = !negate, rhs = num_negate (rhs, precision);
    }

  /* Find the high bit.  */
  if (rhs.high)
    {
      i = precision - 1;
      mask = (cpp_num_part) 1 << (i - PART_PRECISION);
      for (; ; i--, mask >>= 1)
	if (rhs.high & mask)
	  break;
    }
  else if (rhs.low)
    {
      if (precision > PART_PRECISION)
	i = precision - PART_PRECISION - 1;
      else
	i = precision - 1;
      mask = (cpp_num_part) 1 << i;
      for (; ; i--, mask >>= 1)
	if (rhs.low & mask)
	  break;
    }
  else
    {
      if (!pfile->state.skip_eval)
	cpp_error_with_line (pfile, CPP_DL_ERROR, location, 0,
			     "division by zero in #if");
      lhs.unsignedp = unsignedp;
      return lhs;
    }

  /* First nonzero bit of RHS is bit I.  Do naive division by
     shifting the RHS fully left, and subtracting from LHS if LHS is
     at least as big, and then repeating but with one less shift.
     This is not very efficient, but is easy to understand.  */

  rhs.unsignedp = true;
  lhs.unsignedp = true;
  i = precision - i - 1;
  sub = num_lshift (rhs, precision, i);

  result.high = result.low = 0;
  for (;;)
    {
      if (num_greater_eq (lhs, sub, precision))
	{
	  lhs = num_binary_op (pfile, lhs, sub, CPP_MINUS);
	  if (i >= PART_PRECISION)
	    result.high |= (cpp_num_part) 1 << (i - PART_PRECISION);
	  else
	    result.low |= (cpp_num_part) 1 << i;
	}
      if (i-- == 0)
	break;
      sub.low = (sub.low >> 1) | (sub.high << (PART_PRECISION - 1));
      sub.high >>= 1;
    }

  /* We divide so that the remainder has the sign of the LHS.  */
  if (op == CPP_DIV)
    {
      result.unsignedp = unsignedp;
      result.overflow = false;
      if (!unsignedp)
	{
	  if (negate)
	    result = num_negate (result, precision);
	  result.overflow = (num_positive (result, precision) ^ !negate
			     && !num_zerop (result));
	}

      return result;
    }

  /* CPP_MOD.  */
  lhs.unsignedp = unsignedp;
  lhs.overflow = false;
  if (lhs_neg)
    lhs = num_negate (lhs, precision);

  return lhs;
}

