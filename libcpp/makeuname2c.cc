/* Make uname2c.h from various sources.
   Copyright (C) 2005-2023 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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

/* Run this program as
   ./makeuname2c UnicodeData.txt NameAliases.txt > uname2c.h

   This program generates 2 big arrays and 2 small ones.
   The large ones are uname2c_dict, initialized by string literal
   representing dictionary, and uname2c_tree, which is a space optimized
   radix tree.
   The format of the radix tree is:
   byte 0	either 0x80 + (key[0] - ' ')	(if key_len == 1)
		or key_len			(otherwise)
		either of them ored with 0x40 if it has a codepoint
   byte 1	LSB of offset into uname2c_dict for key	(only if key_len > 1)
   byte 2	MSB of offset into uname2c_dict for key	(only if key_len > 1)
		if key_len == 1, the above 2 bytes are omitted
   byte 3	LSB of codepoint (only if it has a codepoint)
   byte 4	middle byte of codepoint (ditto)
   byte 5	MSB of codepoint (ditto), ored with 0x80 if node has children
				   ored with 0x40 if it doesn't have siblings
		if it doesn't have a codepoint, the above 3 bytes are omitted
		and we assume that the node has children
   byte 6, 7, 8	uleb128 encoded offset to first child relative to the end
		of the uleb128 (only if node has children)
   byte 9	0xff (only if node doesn't have a codepoint and doesn't
		      have siblings)

   For prefixes of Unicode NR1 or NR2 rule generated names, on a node
   representing end of the prefix codepoint is 0xd800 + index into
   uname2c_generated array with indexes into uname2c_pairs array of
   code points (low, high) of the ranges terminated by single 0.
   0xd800 is NR1 rule (Hangul syllables), rest are NR2 rules.
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))

#define NUM_CODE_POINTS 0x110000
#define MAX_CODE_POINT 0x10ffff
#define NO_VALUE 0xdc00
#define GENERATED 0xd800

struct entry { const char *name; unsigned long codepoint; };
static struct entry *entries;
static unsigned long num_allocated, num_entries;

/* Unicode 15 Table 4-8.  */
struct generated {
  const char *prefix;
  /* max_high is a workaround for UnicodeData.txt inconsistencies
     on a few CJK UNIFIED IDEOGRAPH- ranges where the "*, Last>"
     entry is a few code points above the end of the range.  */
  unsigned long low, high, max_high;
  int idx, ok;
};
static struct generated generated_ranges[] =
{ { "HANGUL SYLLABLE ", 0xac00, 0xd7a3, 0, 0, 0 }, /* NR1 rule */
  { "CJK UNIFIED IDEOGRAPH-", 0x3400, 0x4dbf, 0, 1, 0 }, /* NR2 rules */
  { "CJK UNIFIED IDEOGRAPH-", 0x4e00, 0x9fff, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x20000, 0x2a6df, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x2a700, 0x2b739, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x2b740, 0x2b81d, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x2b820, 0x2cea1, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x2ceb0, 0x2ebe0, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x30000, 0x3134a, 0, 1, 0 },
  { "CJK UNIFIED IDEOGRAPH-", 0x31350, 0x323af, 0, 1, 0 },
  { "TANGUT IDEOGRAPH-", 0x17000, 0x187f7, 0, 2, 0 },
  { "TANGUT IDEOGRAPH-", 0x18d00, 0x18d08, 0, 2, 0 },
  { "KHITAN SMALL SCRIPT CHARACTER-", 0x18b00, 0x18cd5, 0, 3, 0 },
  { "NUSHU CHARACTER-", 0x1b170, 0x1b2fb, 0, 4, 0 },
  { "CJK COMPATIBILITY IDEOGRAPH-", 0xf900, 0xfa6d, 0, 5, 0 },
  { "CJK COMPATIBILITY IDEOGRAPH-", 0xfa70, 0xfad9, 0, 5, 0 },
  { "CJK COMPATIBILITY IDEOGRAPH-", 0x2f800, 0x2fa1d, 0, 5, 0 }
};

struct node {
  struct node *sibling, *child;
  const char *key;
  size_t key_len, key_idx, node_size, size_sum, child_off;
  unsigned long codepoint;
  bool in_dict;
};
static struct node *root, **nodes;
static unsigned long num_nodes;
static size_t dict_size, tree_size, max_entry_len;
static char *dict;
static unsigned char *tree;

/* Die!  */

static void
fail (const char *s, ...)
{
  va_list ap;

  va_start (ap, s);
  vfprintf (stderr, s, ap);
  va_end (ap);
  fputc ('\n', stderr);
  exit (1);
}

static void *
xmalloc (size_t size)
{
  void *ret = malloc (size);

  if (ret == NULL)
    fail ("failed to allocate %ld bytes", (long) size);
  return ret;
}

static void *
xrealloc (void *p, size_t size)
{
  void *ret = p ? realloc (p, size) : malloc (size);

  if (ret == NULL)
    fail ("failed to allocate %ld bytes", (long) size);
  return ret;
}

static int
entrycmp (const void *p1, const void *p2)
{
  const struct entry *e1 = (const struct entry *) p1;
  const struct entry *e2 = (const struct entry *) p2;
  int ret = strcmp (e1->name, e2->name);

  if (ret != 0)
    return ret;
  if (e1->codepoint < e2->codepoint)
    return -1;
  if (e1->codepoint > e2->codepoint)
    return 1;
  return 0;
}

static int
nodecmp (const void *p1, const void *p2)
{
  const struct node *n1 = *(const struct node *const *) p1;
  const struct node *n2 = *(const struct node *const *) p2;
  if (n1->key_len > n2->key_len)
    return -1;
  if (n1->key_len < n2->key_len)
    return 1;
  return memcmp (n1->key, n2->key, n1->key_len);
}

/* Read UnicodeData.txt and fill in the 'decomp' table to be the
   decompositions of characters for which both the character
   decomposed and all the code points in the decomposition are valid
   for some supported language version, and the 'all_decomp' table to
   be the decompositions of all characters without those
   constraints.  */

static void
read_table (char *fname, bool aliases_p)
{
  FILE *f = fopen (fname, "r");
  const char *sname = aliases_p ? "NameAliases.txt" : "UnicodeData.txt";

  if (!f)
    fail ("opening %s", sname);
  for (;;)
    {
      char line[256];
      unsigned long codepoint;
      const char *name, *aname;
      char *l;
      size_t i;

      if (!fgets (line, sizeof (line), f))
	break;
      codepoint = strtoul (line, &l, 16);
      if (l == line && aliases_p)
	{
	  /* NameAliased.txt can contain comments and empty lines.  */
	  if (*line == '#' || *line == '\n')
	    continue;
	}
      if (l == line || *l != ';')
	fail ("parsing %s, reading code point", sname);
      if (codepoint > MAX_CODE_POINT)
	fail ("parsing %s, code point too large", sname);

      name = l + 1;
      do {
	++l;
      } while (*l != ';');

      aname = NULL;
      if (aliases_p)
	{
	  /* Ignore figment and abbreviation aliases.  */
	  if (strcmp (l + 1, "correction\n") != 0
	      && strcmp (l + 1, "control\n") != 0
	      && strcmp (l + 1, "alternate\n") != 0)
	    continue;
	  i = ARRAY_SIZE (generated_ranges);
	}
      else
	{
	  for (i = 0; i < ARRAY_SIZE (generated_ranges); ++i)
	    if (codepoint >= generated_ranges[i].low
		&& codepoint <= generated_ranges[i].max_high)
	      break;
	  if (i != ARRAY_SIZE (generated_ranges))
	    {
	      if (*name == '<' && l[-1] == '>')
		{
		  if (codepoint == generated_ranges[i].low
		      && l - name >= 9
		      && memcmp (l - 8, ", First>", 8) == 0
		      && generated_ranges[i].ok == 0)
		    {
		      generated_ranges[i].ok = INT_MAX - 1;
		      aname = generated_ranges[i].prefix;
		      codepoint = GENERATED + generated_ranges[i].idx;
		    }
		  /* Unfortunately, UnicodeData.txt isn't consistent
		     with the Table 4-8 range endpoints in 3 cases,
		     the ranges are longer there by a few codepoints.
		     So use the max_high hack to avoid verification
		     failures.  */
		  else if (codepoint == generated_ranges[i].max_high
			   && l - name >= 8
			   && memcmp (l - 7, ", Last>", 7) == 0
			   && generated_ranges[i].ok == INT_MAX - 1)
		    {
		      generated_ranges[i].ok = INT_MAX;
		      continue;
		    }
		  else
		    fail ("unexpected generated entry %lx %.*s",
			  codepoint, (int) (l - name), name);
		}
	      else if (codepoint
		       == generated_ranges[i].low + generated_ranges[i].ok
		       && l - name == (strlen (generated_ranges[i].prefix)
				       + (name - 1 - line))
		       && memcmp (name, generated_ranges[i].prefix,
				  strlen (generated_ranges[i].prefix)) == 0
		       && memcmp (name + strlen (generated_ranges[i].prefix),
				  line, name - 1 - line) == 0)
		{
		  ++generated_ranges[i].ok;
		  if (codepoint != generated_ranges[i].low)
		    continue;
		  aname = generated_ranges[i].prefix;
		  codepoint = GENERATED + generated_ranges[i].idx;
		}
	      else
		fail ("unexpected generated entry %lx %.*s",
		      codepoint, (int) (l - name), name);
	      if (aname == generated_ranges[i].prefix)
		{
		  size_t j;

		  /* Don't add an entry for a generated range where the
		     same prefix has been added already.  */
		  for (j = 0; j < i; ++j)
		    if (generated_ranges[j].idx == generated_ranges[i].idx
			&& generated_ranges[j].ok != 0)
		      break;
		  if (j < i)
		    continue;
		}
	    }
	  else if (*name == '<' && l[-1] == '>')
	    continue;
	}

      if (num_entries == num_allocated)
	{
	  num_allocated = num_allocated ? 2 * num_allocated : 65536;
	  entries = (struct entry *) xrealloc (entries, num_allocated
							* sizeof (entries[0]));
	}

      if (aname == NULL)
	{
	  char *a = (char *) xmalloc (l + 1 - name);
	  if (l - name > max_entry_len)
	    max_entry_len = l - name;
	  memcpy (a, name, l - name);
	  a[l - name] = '\0';
	  aname = a;
	}
      entries[num_entries].name = aname;
      entries[num_entries++].codepoint = codepoint;
    }
  if (ferror (f))
    fail ("reading %s", sname);
  fclose (f);
}

/* Assumes nodes are added from sorted array, so we never
   add any node before existing one, only after it.  */

static void
node_add (struct node **p, const char *key, size_t key_len,
	  unsigned long codepoint)
{
  struct node *n;
  size_t i;

  do
    {
      if (*p == NULL)
	{
	  *p = n = (struct node *) xmalloc (sizeof (struct node));
	  ++num_nodes;
	  assert (key_len);
	  n->sibling = NULL;
	  n->child = NULL;
	  n->key = key;
	  n->key_len = key_len;
	  n->codepoint = codepoint;
	  return;
	}
      n = *p;
      for (i = 0; i < n->key_len && i < key_len; ++i)
	if (n->key[i] != key[i])
	  break;
      if (i == 0)
	{
	  p = &n->sibling;
	  continue;
	}
      if (i == n->key_len)
	{
	  assert (key_len > n->key_len);
	  p = &n->child;
	  key += n->key_len;
	  key_len -= n->key_len;
	  continue;
	}
      /* Need to split the node.  */
      assert (i < key_len);
      n = (struct node *) xmalloc (sizeof (struct node));
      ++num_nodes;
      n->sibling = NULL;
      n->child = (*p)->child;
      n->key = (*p)->key + i;
      n->key_len = (*p)->key_len - i;
      n->codepoint = (*p)->codepoint;
      (*p)->child = n;
      (*p)->key_len = i;
      (*p)->codepoint = NO_VALUE;
      key += i;
      key_len -= i;
      p = &n->sibling;
    }
  while (1);
}

static void
append_nodes (struct node *n)
{
  for (; n; n = n->sibling)
    {
      nodes[num_nodes++] = n;
      append_nodes (n->child);
    }
}

static size_t
sizeof_uleb128 (size_t val)
{
  size_t sz = 0;
  do
    {
      val >>= 7;
      sz += 1;
    }
  while (val != 0);
  return sz;
}

static void
size_nodes (struct node *n)
{
  if (n->child)
    size_nodes (n->child);
  if (n->sibling)
    size_nodes (n->sibling);
  n->node_size = 1 + (n->key_len > 1) * 2;
  if (n->codepoint != NO_VALUE)
    n->node_size += 3;
  else if (n->sibling == NULL)
    ++n->node_size;
  n->size_sum = 0;
  n->child_off = 0;
  if (n->sibling)
    n->size_sum += n->sibling->size_sum;
  if (n->child)
    {
      n->child_off = n->size_sum + (n->codepoint == NO_VALUE
				    && n->sibling == NULL);
      n->node_size += sizeof_uleb128 (n->child_off);
    }
  n->size_sum += n->node_size;
  if (n->child)
    n->size_sum += n->child->size_sum;
  tree_size += n->node_size;
}

static void
write_uleb128 (unsigned char *p, size_t val)
{
  unsigned char c;
  do
    {
      c = val & 0x7f;
      val >>= 7;
      if (val)
	c |= 0x80;
      *p++ = c;
    }
  while (val);
}

static void
write_nodes (struct node *n, size_t off)
{
  for (; n; n = n->sibling)
    {
      assert (off < tree_size && tree[off] == 0);
      if (n->key_len > 1)
	{
	  assert (n->key_len < 64);
	  tree[off] = n->key_len;
	}
      else
	tree[off] = (n->key[0] - ' ') | 0x80;
      assert ((tree[off] & 0x40) == 0);
      if (n->codepoint != NO_VALUE)
	tree[off] |= 0x40;
      off++;
      if (n->key_len > 1)
	{
	  tree[off++] = n->key_idx & 0xff;
	  tree[off++] = (n->key_idx >> 8) & 0xff;
	}
      if (n->codepoint != NO_VALUE)
	{
	  assert (n->codepoint < (1L << 21));
	  tree[off++] = n->codepoint & 0xff;
	  tree[off++] = (n->codepoint >> 8) & 0xff;
	  tree[off] = (n->codepoint >> 16) & 0xff;
	  if (n->child)
	    tree[off] |= 0x80;
	  if (!n->sibling)
	    tree[off] |= 0x40;
	  off++;
	}
      if (n->child)
	{
	  write_uleb128 (&tree[off], n->child_off);
	  off += sizeof_uleb128 (n->child_off);
	  write_nodes (n->child, off + n->child_off);
	}
      if (n->codepoint == NO_VALUE
	  && n->sibling == NULL)
	tree[off++] = 0xff;
    }
  assert (off <= tree_size);
}

static void
build_radix_tree (void)
{
  size_t i, j, k, key_idx;

  for (i = 0; i < ARRAY_SIZE (generated_ranges); ++i)
    if (generated_ranges[i].ok == INT_MAX)
      {
	if (generated_ranges[i].max_high - generated_ranges[i].high > 15UL)
	  break;
      }
    else if (generated_ranges[i].ok == (generated_ranges[i].high
					- generated_ranges[i].low + 1))
      {
	if (generated_ranges[i].max_high != generated_ranges[i].high)
	  break;
      }
    else
      break;
  if (i < ARRAY_SIZE (generated_ranges))
    fail ("uncovered generated range %s %lx %lx",
	  generated_ranges[i].prefix, generated_ranges[i].low,
	  generated_ranges[i].high);
  /* Sort entries alphabetically, node_add relies on that.  */
  qsort (entries, num_entries, sizeof (struct entry), entrycmp);
  for (i = 1; i < num_entries; ++i)
    if (i && strcmp (entries[i].name, entries[i - 1].name) == 0)
      fail ("multiple entries for name %s", entries[i].name);

  for (i = 0; i < num_entries; ++i)
    node_add (&root, entries[i].name, strlen (entries[i].name),
	      entries[i].codepoint);

  nodes = (struct node **) xmalloc (num_nodes * sizeof (struct node *));
  i = num_nodes;
  num_nodes = 0;
  append_nodes (root);
  assert (num_nodes == i);
  /* Sort node pointers by decreasing string length to handle substrings
     right.  */
  qsort (nodes, num_nodes, sizeof (struct node *), nodecmp);
  if (nodes[0]->key_len >= 64)
    /* We could actually encode even 64 and 65, as key_len 0 and 1 will
       never appear in the multiple letter key encodings, so could subtract
       2.  */
    fail ("can't encode key length %d >= 64, so need to split some radix "
	  "tree nodes to ensure length fits", nodes[0]->key_len);

  /* Verify a property charset.cc UAX44-LM2 matching relies on:
     if - is at the end of key of some node, then all its siblings
     start with alphanumeric characters.
     Only 2 character names and 1 alias have - followed by space:
     U+0F0A TIBETAN MARK BKA- SHOG YIG MGO
     U+0FD0 TIBETAN MARK BKA- SHOG GI MGO RGYAN
     U+0FD0 TIBETAN MARK BSKA- SHOG GI MGO RGYAN
     so the KA- in there will always be followed at least by SHOG
     in the same node.
     If this changes, charset.cc needs to change.  */
  for (i = 0; i < num_nodes; ++i)
    if (nodes[i]->key[nodes[i]->key_len - 1] == '-'
	&& nodes[i]->child)
      {
	struct node *n;

	for (n = nodes[i]->child; n; n = n->sibling)
	  if (n->key[0] == ' ')
	    fail ("node with key %.*s followed by node with key %.*s",
		  (int) nodes[i]->key_len, nodes[i]->key,
		  (int) n->key_len, n->key);
      }

  /* This is expensive, O(num_nodes * num_nodes * nodes[0]->key_len), but
     fortunately num_nodes is < 64K and key_len < 64.  */
  key_idx = 0;
  for (i = 0; i < num_nodes; ++i)
    {
      nodes[i]->key_idx = SIZE_MAX;
      nodes[i]->in_dict = false;
      if (nodes[i]->key_len > 1)
	{
	  for (j = 0; j < i; ++j)
	    /* Can't rely on memmem unfortunately.  */
	    if (nodes[j]->in_dict)
	      {
		for (k = 0; k <= nodes[j]->key_len - nodes[i]->key_len; ++k)
		  if (nodes[j]->key[k] == nodes[i]->key[0]
		      && memcmp (nodes[j]->key + k + 1, nodes[i]->key + 1,
				 nodes[i]->key_len - 1) == 0)
		    {
		      nodes[i]->key_idx = nodes[j]->key_idx + k;
		      j = i;
		      break;
		    }
		if (j == i)
		  break;
		for (; k < nodes[j]->key_len; ++k)
		  if (nodes[j]->key[k] == nodes[i]->key[0]
		      && memcmp (nodes[j]->key + k + 1, nodes[i]->key + 1,
				 nodes[j]->key_len - 1 - k) == 0)
		    {
		      size_t l;

		      for (l = j + 1; l < i; ++l)
			if (nodes[l]->in_dict)
			  break;
		      if (l < i
			  && memcmp (nodes[l]->key,
				     nodes[i]->key + (nodes[j]->key_len - k),
				     nodes[i]->key_len
				     - (nodes[j]->key_len - k)) == 0)
			{
			  nodes[i]->key_idx = nodes[j]->key_idx + k;
			  j = i;
			}
		      else
			j = l - 1;
		      break;
		    }
	      }
	  if (nodes[i]->key_idx == SIZE_MAX)
	    {
	      nodes[i]->key_idx = key_idx;
	      nodes[i]->in_dict = true;
	      key_idx += nodes[i]->key_len;
	    }
	}
    }
  if (key_idx >= 65536)
    /* We only use 2 bytes for offsets into the dictionary.
       If it grows more, there is e.g. a possibility to replace
       most often seen words or substrings in the dictionary
       with characters other than [A-Z0-9 -] (say LETTER occurs
       in the dictionary almost 197 times and so by using a
       instead of LETTER we could save (6 - 1) * 197 bytes,
       with some on the side table mapping 'a' to "LETTER".  */
    fail ("too large dictionary %ld", (long) key_idx);
  dict_size = key_idx;

  size_nodes (root);

  dict = (char *) xmalloc (dict_size + 1);
  for (i = 0; i < num_nodes; ++i)
    if (nodes[i]->in_dict)
      memcpy (dict + nodes[i]->key_idx, nodes[i]->key, nodes[i]->key_len);
  dict[dict_size] = '\0';

  tree = (unsigned char *) xmalloc (tree_size);
  memset (tree, 0, tree_size);
  write_nodes (root, 0);
}

/* Print out the huge copyright notice.  */

static void
write_copyright (void)
{
  static const char copyright[] = "\
/* Unicode name to codepoint.\n\
   Copyright (C) 2005-2023 Free Software Foundation, Inc.\n\
\n\
   This program is free software; you can redistribute it and/or modify it\n\
   under the terms of the GNU General Public License as published by the\n\
   Free Software Foundation; either version 3, or (at your option) any\n\
   later version.\n\
\n\
   This program is distributed in the hope that it will be useful,\n\
   but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
   GNU General Public License for more details.\n\
\n\
   You should have received a copy of the GNU General Public License\n\
   along with this program; see the file COPYING3.  If not see\n\
   <http://www.gnu.org/licenses/>.\n\
\n\
\n\
   Copyright (C) 1991-2022 Unicode, Inc.  All rights reserved.\n\
   Distributed under the Terms of Use in\n\
   http://www.unicode.org/copyright.html.\n\
\n\
   Permission is hereby granted, free of charge, to any person\n\
   obtaining a copy of the Unicode data files and any associated\n\
   documentation (the \"Data Files\") or Unicode software and any\n\
   associated documentation (the \"Software\") to deal in the Data Files\n\
   or Software without restriction, including without limitation the\n\
   rights to use, copy, modify, merge, publish, distribute, and/or\n\
   sell copies of the Data Files or Software, and to permit persons to\n\
   whom the Data Files or Software are furnished to do so, provided\n\
   that (a) the above copyright notice(s) and this permission notice\n\
   appear with all copies of the Data Files or Software, (b) both the\n\
   above copyright notice(s) and this permission notice appear in\n\
   associated documentation, and (c) there is clear notice in each\n\
   modified Data File or in the Software as well as in the\n\
   documentation associated with the Data File(s) or Software that the\n\
   data or software has been modified.\n\
\n\
   THE DATA FILES AND SOFTWARE ARE PROVIDED \"AS IS\", WITHOUT WARRANTY\n\
   OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE\n\
   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n\
   NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE\n\
   COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR\n\
   ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY\n\
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,\n\
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS\n\
   ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE\n\
   OF THE DATA FILES OR SOFTWARE.\n\
\n\
   Except as contained in this notice, the name of a copyright holder\n\
   shall not be used in advertising or otherwise to promote the sale,\n\
   use or other dealings in these Data Files or Software without prior\n\
   written authorization of the copyright holder.  */\n";

   puts (copyright);
}

static void
write_dict (void)
{
  size_t i;

  printf ("static const char uname2c_dict[%ld] =\n", (long) (dict_size + 1));
  for (i = 0; i < dict_size; i += 77)
    printf ("\"%.77s\"%s\n", dict + i, i + 76 > dict_size ? ";" : "");
  puts ("");
}

static void
write_tree (void)
{
  size_t i, j;

  printf ("static const unsigned char uname2c_tree[%ld] = {\n",
	  (long) tree_size);
  for (i = 0, j = 0; i < tree_size; ++i)
    {
      printf ("%s0x%02x%s", j == 0 ? "  " : "", tree[i],
	      i == tree_size - 1 ? " };\n\n" : j == 11 ? ",\n" : ", ");
      if (j == 11)
	j = 0;
      else
	++j;
    }
}

static void
write_generated (void)
{
  size_t i, j;

  puts ("static const cppchar_t uname2c_pairs[] = {");
  for (i = 0; i < ARRAY_SIZE (generated_ranges); ++i)
    {
      if (i == 0)
	;
      else if (generated_ranges[i - 1].idx != generated_ranges[i].idx)
	puts (", 0,");
      else
	puts (",");
      printf ("  0x%lx, 0x%lx /* %s */",
	      generated_ranges[i].low,
	      generated_ranges[i].high,
	      generated_ranges[i].prefix);
    }
  puts (", 0 };\n");

  puts ("static const unsigned char uname2c_generated[] = {");
  for (i = 0, j = -1; i < ARRAY_SIZE (generated_ranges); ++i)
    {
      if (i == 0 || generated_ranges[i - 1].idx != generated_ranges[i].idx)
	printf ("%s  %d /* %s */", i ? ",\n" : "",
		++j, generated_ranges[i].prefix);
      j += 2;
    }
  puts (" };\n");
}

/* Main program.  */

int
main (int argc, char **argv)
{
  size_t i;

  if (argc != 3)
    fail ("too few arguments to makeradixtree");
  for (i = 0; i < ARRAY_SIZE (generated_ranges); ++i)
    if (!generated_ranges[i].max_high)
      generated_ranges[i].max_high = generated_ranges[i].high;
  read_table (argv[1], false);
  read_table (argv[2], true);
  build_radix_tree ();

  write_copyright ();
  write_dict ();
  write_tree ();
  write_generated ();
  printf ("static const unsigned int uname2c_max_name_len = %ld;\n\n", max_entry_len);
  return 0;
}
