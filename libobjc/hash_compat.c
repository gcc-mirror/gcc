/* Binary compatibility hash implementations for Objective C.
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#define OBJC_IGNORE_DEPRECATED_API 1
#include "objc/hash.h"

cache_ptr
hash_new (unsigned int size,
	  hash_func_type hash_func,
	  compare_func_type compare_func)
{
  return objc_hash_new(size, hash_func, compare_func);
}

void
hash_delete(cache_ptr cache)
{
  objc_hash_delete(cache);
}

void
hash_add (cache_ptr *cachep, const void *key, void *value)
{
  objc_hash_add(cachep, key, value);
}

void
hash_remove (cache_ptr cache, const void *key)
{
  objc_hash_remove (cache, key);
}

node_ptr
hash_next (cache_ptr cache, node_ptr node)
{
  return objc_hash_next (cache, node);
}

void *
hash_value_for_key (cache_ptr cache, const void *key)
{
  return objc_hash_value_for_key (cache, key);
}

BOOL
hash_is_key_in_hash (cache_ptr cache, const void *key)
{
  return objc_hash_is_key_in_hash (cache, key);
}

unsigned int
hash_ptr (cache_ptr cache, const void *key)
{
  return objc_hash_ptr (cache, key);
}

unsigned int 
hash_string (cache_ptr cache, const void *key)
{
  return objc_hash_string (cache, key);
}

int 
compare_ptrs (const void *k1, const void *k2)
{
  return objc_compare_ptrs (k1, k2);
}

int 
compare_strings (const void *k1, const void *k2)
{
  return objc_compare_strings (k1, k2);
}

