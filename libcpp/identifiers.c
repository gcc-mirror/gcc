/* Hash tables for the CPP library.
   Copyright (C) 1986-2019 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

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
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"

static hashnode alloc_node (cpp_hash_table *);

/* Return an identifier node for hashtable.c.  Used by cpplib except
   when integrated with the C front ends.  */
static hashnode
alloc_node (cpp_hash_table *table)
{
  cpp_hashnode *node;

  node = XOBNEW (&table->pfile->hash_ob, cpp_hashnode);
  memset (node, 0, sizeof (cpp_hashnode));
  return HT_NODE (node);
}

/* Set up the identifier hash table.  Use TABLE if non-null, otherwise
   create our own.  */
void
_cpp_init_hashtable (cpp_reader *pfile, cpp_hash_table *table)
{
  struct spec_nodes *s;

  if (table == NULL)
    {
      pfile->our_hashtable = 1;
      table = ht_create (13);	/* 8K (=2^13) entries.  */
      table->alloc_node = alloc_node;

      obstack_specify_allocation (&pfile->hash_ob, 0, 0, xmalloc, free);
    }

  table->pfile = pfile;
  pfile->hash_table = table;

  /* Now we can initialize things that use the hash table.  */
  _cpp_init_directives (pfile);
  _cpp_init_internal_pragmas (pfile);

  s = &pfile->spec_nodes;
  s->n_defined		= cpp_lookup (pfile, DSC("defined"));
  s->n_true		= cpp_lookup (pfile, DSC("true"));
  s->n_false		= cpp_lookup (pfile, DSC("false"));
  s->n__VA_ARGS__       = cpp_lookup (pfile, DSC("__VA_ARGS__"));
  s->n__VA_ARGS__->flags |= NODE_DIAGNOSTIC;
  s->n__VA_OPT__        = cpp_lookup (pfile, DSC("__VA_OPT__"));
  s->n__VA_OPT__->flags |= NODE_DIAGNOSTIC;
  s->n__has_include__   = cpp_lookup (pfile, DSC("__has_include__"));
  s->n__has_include_next__ = cpp_lookup (pfile, DSC("__has_include_next__"));
}

/* Tear down the identifier hash table.  */
void
_cpp_destroy_hashtable (cpp_reader *pfile)
{
  if (pfile->our_hashtable)
    {
      ht_destroy (pfile->hash_table);
      obstack_free (&pfile->hash_ob, 0);
    }
}

/* Returns the hash entry for the STR of length LEN, creating one
   if necessary.  */
cpp_hashnode *
cpp_lookup (cpp_reader *pfile, const unsigned char *str, unsigned int len)
{
  /* ht_lookup cannot return NULL.  */
  return CPP_HASHNODE (ht_lookup (pfile->hash_table, str, len, HT_ALLOC));
}

/* Determine whether the str STR, of length LEN, is a defined macro.  */
int
cpp_defined (cpp_reader *pfile, const unsigned char *str, int len)
{
  cpp_hashnode *node;

  node = CPP_HASHNODE (ht_lookup (pfile->hash_table, str, len, HT_NO_INSERT));

  /* If it's a macro, it cannot have been poisoned.  */
  return node && cpp_macro_p (node);
}

/* We don't need a proxy since the hash table's identifier comes first
   in cpp_hashnode.  However, in case this is ever changed, we have a
   static assertion for it.  */
extern char proxy_assertion_broken[offsetof (struct cpp_hashnode, ident) == 0 ? 1 : -1];

/* For all nodes in the hashtable, callback CB with parameters PFILE,
   the node, and V.  */
void
cpp_forall_identifiers (cpp_reader *pfile, cpp_cb cb, void *v)
{
  ht_forall (pfile->hash_table, (ht_cb) cb, v);
}
