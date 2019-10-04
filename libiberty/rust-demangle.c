/* Demangler for the Rust programming language
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Written by David Tolnay (dtolnay@gmail.com).

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU Library General Public
License, the Free Software Foundation gives you unlimited permission
to link the compiled version of this file into combinations with other
programs, and to distribute those combinations without any restriction
coming from the use of this file.  (The Library Public License
restrictions do apply in other respects; for example, they cover
modification of the file, and distribution when not linked into a
combined executable.)

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.
If not, see <http://www.gnu.org/licenses/>.  */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "safe-ctype.h"

#include <sys/types.h>
#include <string.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern size_t strlen(const char *s);
extern int strncmp(const char *s1, const char *s2, size_t n);
extern void *memset(void *s, int c, size_t n);
#endif

#include <demangle.h>
#include "libiberty.h"
#include "rust-demangle.h"


/* Mangled (legacy) Rust symbols look like this:
     _$LT$std..sys..fd..FileDesc$u20$as$u20$core..ops..Drop$GT$::drop::hc68340e1baa4987a

   The original symbol is:
     <std::sys::fd::FileDesc as core::ops::Drop>::drop

   The last component of the path is a 64-bit hash in lowercase hex,
   prefixed with "h". Rust does not have a global namespace between
   crates, an illusion which Rust maintains by using the hash to
   distinguish things that would otherwise have the same symbol.

   Any path component not starting with a XID_Start character is
   prefixed with "_".

   The following escape sequences are used:

   ","  =>  $C$
   "@"  =>  $SP$
   "*"  =>  $BP$
   "&"  =>  $RF$
   "<"  =>  $LT$
   ">"  =>  $GT$
   "("  =>  $LP$
   ")"  =>  $RP$
   "\u{XY}"  =>  $uXY$

   A double ".." means "::" and a single "." means "-".

   The only characters allowed in the mangled symbol are a-zA-Z0-9 and _.:$  */

static const char *hash_prefix = "::h";
static const size_t hash_prefix_len = 3;
static const size_t hash_len = 16;

static int is_prefixed_hash (const char *start);
static int parse_lower_hex_nibble (char nibble);
static char parse_legacy_escape (const char **in);

/* INPUT: sym: symbol that has been through C++ (gnu v3) demangling

   This function looks for the following indicators:

   1. The hash must consist of "h" followed by 16 lowercase hex digits.

   2. As a sanity check, the hash must use between 5 and 15 of the 16
      possible hex digits. This is true of 99.9998% of hashes so once
      in your life you may see a false negative. The point is to
      notice path components that could be Rust hashes but are
      probably not, like "haaaaaaaaaaaaaaaa". In this case a false
      positive (non-Rust symbol has an important path component
      removed because it looks like a Rust hash) is worse than a false
      negative (the rare Rust symbol is not demangled) so this sets
      the balance in favor of false negatives.

   3. There must be no characters other than a-zA-Z0-9 and _.:$  */

int
rust_is_mangled (const char *sym)
{
  size_t len, len_without_hash;
  const char *end;

  if (!sym)
    return 0;

  len = strlen (sym);
  if (len <= hash_prefix_len + hash_len)
    /* Not long enough to contain "::h" + hash + something else */
    return 0;

  len_without_hash = len - (hash_prefix_len + hash_len);
  if (!is_prefixed_hash (sym + len_without_hash))
    return 0;

  end = sym + len_without_hash;

  while (sym < end)
    {
      if (*sym == '$' || *sym == '.' || *sym == '_' || *sym == ':'
          || ISALNUM (*sym))
        sym++;
      else
        return 0;
    }

  return 1;
}

/* A hash is the prefix "::h" followed by 16 lowercase hex digits. The
   hex digits must contain at least 5 distinct digits.  */

static int
is_prefixed_hash (const char *str)
{
  const char *end;
  char seen[16];
  size_t i;
  int count, nibble;

  if (strncmp (str, hash_prefix, hash_prefix_len))
    return 0;
  str += hash_prefix_len;

  memset (seen, 0, sizeof(seen));
  for (end = str + hash_len; str < end; str++)
    {
      nibble = parse_lower_hex_nibble (*str);
      if (nibble < 0)
        return 0;
      seen[nibble] = 1;
    }

  /* Count how many distinct digits seen */
  count = 0;
  for (i = 0; i < 16; i++)
    if (seen[i])
      count++;

  return count >= 5;
}

/*
  INPUT: sym: symbol for which rust_is_mangled(sym) returned 1.

  The input is demangled in-place because the mangled name is always
  longer than the demangled one.  */

void
rust_demangle_sym (char *sym)
{
  const char *in;
  char *out;
  const char *end;
  char unescaped;

  if (!sym)
    return;

  in = sym;
  out = sym;
  end = sym + strlen (sym) - (hash_prefix_len + hash_len);

  while (in < end)
    {
      if (*in == '$')
        {
          unescaped = parse_legacy_escape (&in);
          if (unescaped)
            *out++ = unescaped;
          else
            /* unexpected escape sequence, skip the rest of this segment. */
            while (in < end && *in != ':')
              *out++ = *in++;
        }
      else if (*in == '_')
        {
          /* If this is the start of a path component and the next
             character is an escape sequence, ignore the underscore. The
             mangler inserts an underscore to make sure the path
             component begins with a XID_Start character. */
          if ((in == sym || in[-1] == ':') && in[1] == '$')
            in++;
          else
            *out++ = *in++;
        }
      else if (*in == '.')
        {
          if (in[1] == '.')
            {
              /* ".." becomes "::" */
              *out++ = ':';
              *out++ = ':';
              in += 2;
            }
          else
            {
              /* "." becomes "-" */
              *out++ = '-';
              in++;
            }
        }
      else if (*in == ':' || ISALNUM (*in))
        *out++ = *in++;
      else
        {
          /* unexpected character in symbol, not rust_is_mangled.  */
          *out++ = '?'; /* This is pretty lame, but it's hard to do better. */
          *out = '\0';
          return;
        }
    }

  *out = '\0';
}

/* Return a 0x0-0xf value if the char is 0-9a-f, and -1 otherwise. */
static int
parse_lower_hex_nibble (char nibble)
{
  if ('0' <= nibble && nibble <= '9')
    return nibble - '0';
  if ('a' <= nibble && nibble <= 'f')
    return 0xa + (nibble - 'a');
  return -1;
}

/* Return the unescaped character for a "$...$" escape, or 0 if invalid. */
static char
parse_legacy_escape (const char **in)
{
  char c = 0;
  const char *e;
  size_t escape_len = 0;
  int lo_nibble = -1, hi_nibble = -1;

  if ((*in)[0] != '$')
    return 0;

  e = *in + 1;

  if (e[0] == 'C')
    {
      escape_len = 1;

      c = ',';
    }
  else
    {
      escape_len = 2;

      if (e[0] == 'S' && e[1] == 'P')
        c = '@';
      else if (e[0] == 'B' && e[1] == 'P')
        c = '*';
      else if (e[0] == 'R' && e[1] == 'F')
        c = '&';
      else if (e[0] == 'L' && e[1] == 'T')
        c = '<';
      else if (e[0] == 'G' && e[1] == 'T')
        c = '>';
      else if (e[0] == 'L' && e[1] == 'P')
        c = '(';
      else if (e[0] == 'R' && e[1] == 'P')
        c = ')';
      else if (e[0] == 'u')
        {
          escape_len = 3;

          hi_nibble = parse_lower_hex_nibble (e[1]);
          if (hi_nibble < 0)
            return 0;
          lo_nibble = parse_lower_hex_nibble (e[2]);
          if (lo_nibble < 0)
            return 0;

          /* Only allow non-control ASCII characters. */
          if (hi_nibble > 7)
            return 0;
          c = (hi_nibble << 4) | lo_nibble;
          if (c < 0x20)
            return 0;
        }
    }

  if (!c || e[escape_len] != '$')
    return 0;

  *in += 2 + escape_len;
  return c;
}
