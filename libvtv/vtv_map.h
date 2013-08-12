/* Copyright (C) 2012-2013
   Free Software Foundation

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _VTV_MAP_H
#define _VTV_MAP_H 1

#include <string.h>
#include <vtv_utils.h>

inline uint64_t
load8bytes (const void *p)
{
  uint64_t result;
  memcpy (&result, p, 8);
  return result;
}

/* Insert_only_hash_map maps keys to values.  The implementation is a
   basic hash table with open addressing.  The keys are not "owned" by
   the table; it only stores pointers to keys.  The key type is
   specified below (see insert_only_hash_map::key_type) and is,
   roughly speaking, a string of any length with the string length and
   a hash code stored at the front.  The code here does not compute
   any hash codes, but rather uses what's given.  */

template<typename T, typename Alloc>
class insert_only_hash_map
  {
    public:
      typedef size_t size_type;
      typedef T value_type;
      typedef Alloc alloc_type;
      enum { min_capacity = 4 };
#if HASHMAP_STATS
  enum { stats = true };
#else
  enum { stats = false };
#endif

  /* Keys are a byte string (up to 2^32 - 1 long) plus a uint32_t
     that's used as a hash code.  The latter can encode arbitrary
     information at the client's discretion, so, e.g., multiple keys
     that are the same string still "differ" if the hash codes differ.
     Keys are equal if the first 8 bytes are equal and the next n
     bytes are equal.  */
  struct key_type
  {
    uint32_t n;
    uint32_t hash;
    char bytes[0];

    bool
    equals (const key_type *k) const;
  };

  /* Create an empty map with a reasonable number of buckets for the
     expected size.  Returns NULL if the allocator fails.  */

  static insert_only_hash_map *
  create (size_type expected_size);

  /* The opposite of create().  Free the memory for the given map.  */

  static void
  destroy (insert_only_hash_map *m)
  { Alloc().dealloc (m, m->size_in_bytes_); }

  /* Return a map identical to this except that *k is mapped to v.
     Typcially it's done by modifying this in place, but if a resize
     is necessary then this is deallocated and a new map is returned.
     Requires k to be non-NULL.  Does nothing and returns NULL if the
     allocator fails.  */

  insert_only_hash_map*
  put (const key_type *k, const value_type &v)
  { return this->put_internal (k, v, false); }

  /* If *k is a key in this then set *v to point to the corresponding
     value.  Otherwise, do the equivalent of insert(k, value_type())
     and, if that succeeds, set *v to point to the inserted value.
     Requires k to be non-NULL.  Does nothing and returns NULL if the
     allocator fails.  Typically returns this, but will return a new
     insert_only_hash_map if a resize occurs.  If the return value is
     non-NULL, *v is set and it's valid until a resize of the map that
     is the return value.  */

  insert_only_hash_map *
  find_or_add_key (const key_type *k, value_type **v);

  /* Get the value corresponding to *k.  Returns NULL if there is
     none.  Requires k to be non-NULL.  The return value is valid
     until any resize.  */
  const value_type *get (const key_type *k) const;

  size_type
  size () const
  { return num_entries_; }

  bool
  empty () const
  { return this->size () == 0; }

  size_type
  bucket_count () const
  { return num_buckets_; }

 private:
  typedef std::pair <const key_type *, value_type> bucket_type;

  insert_only_hash_map *put_internal (const key_type *, const value_type &,
				      bool);

  /* This function determines when to resize the table.  */
  bool
  is_too_full (size_type entries) const
  { return entries > (this->bucket_count () * 0.7); }

  /* Return a copy with double the number of buckets.  Returns NULL if
     the allocator fails.  Otherwise, calls destroy (this).  */
  insert_only_hash_map *destructive_copy ();

 /* Must be a power of 2 not less than min_capacity. */
  size_type num_buckets_; 
  size_type num_entries_;
  size_type size_in_bytes_;
  bucket_type buckets[0];  /* Actual array size is num_buckets.  */
};

template <typename T, typename Alloc>
insert_only_hash_map <T, Alloc> *
insert_only_hash_map <T, Alloc>::create (size_type expected_size)
{
  size_t cap = min_capacity;
  while (expected_size >= cap)
    {
      cap *= 2;
    }
  size_t size_in_bytes = sizeof (insert_only_hash_map <T, Alloc>)
                                                  + cap * sizeof (bucket_type);
  insert_only_hash_map <T, Alloc>* result =
      static_cast <insert_only_hash_map <T, Alloc>*> (Alloc ()
                                                       .alloc (size_in_bytes));
  if (result != NULL)
    {
      result->size_in_bytes_ = size_in_bytes;
      result->num_buckets_ = cap;
      result->num_entries_ = 0;
      memset (result->buckets, 0, cap * sizeof (bucket_type));
    }
  return result;
}

template <typename T, typename Alloc>
insert_only_hash_map <T, Alloc>*
insert_only_hash_map <T, Alloc>::destructive_copy ()
{
  insert_only_hash_map* copy = create (this->bucket_count ());
  if (copy == NULL)
    return NULL;
  VTV_DEBUG_ASSERT (copy->bucket_count () == 2 * this->bucket_count ());
  for (size_type i = 0; i < this->bucket_count (); i++)
    if (this->buckets[i].first != NULL)
      copy->put_internal (this->buckets[i].first, this->buckets[i].second,
			  true);
  VTV_DEBUG_ASSERT (copy->size () == this->size ());
  destroy (this);
  return copy;
}

template <typename T, typename Alloc>
insert_only_hash_map <T, Alloc>*
insert_only_hash_map <T, Alloc>::find_or_add_key (const key_type *k,
						  value_type **v)
{
  /* Table size is always a power of 2.  */
  const size_type mask = this->bucket_count () - 1;
  size_type bucket_index = k->hash & mask;
  size_type step = 1;
  for (;;)
    {
      bucket_type &bucket = this->buckets[bucket_index];
      if (bucket.first == NULL)
        {
          /* Key was not present. */
          if (this->is_too_full (this->size () + 1))
            {
              insert_only_hash_map <T, Alloc>* result =
		                                     this->destructive_copy ();
              return result == NULL
                  ? NULL
                  : result->find_or_add_key (k, v);
            }
          else
            {
              bucket.first = k;
              bucket.second = T ();
              this->num_entries_++;
              *v = &bucket.second;
              return this;
            }
        }
      else if (bucket.first->equals (k))
        {
          /* Key was present. */
          *v = &bucket.second;
          return this;
        }
      else
        bucket_index = (bucket_index + step++) & mask;
    }
}

template <typename T, typename Alloc>
insert_only_hash_map <T, Alloc>*
insert_only_hash_map <T, Alloc>::put_internal (
				     const insert_only_hash_map::key_type *k,
				     const insert_only_hash_map::value_type &v,
				     bool unique_key_and_resize_not_needed)
{
  /* Table size is always a power of 2.  */
  const size_type mask = this->bucket_count () - 1;
  size_type bucket_index = k->hash & mask;
  size_type step = 1;
  for (;;)
    {
      bucket_type &bucket = this->buckets[bucket_index];
      if (bucket.first == NULL)
        {
          /* Key was not present.  */
          if (!unique_key_and_resize_not_needed
              && this->is_too_full (this->size () + 1))
            {
              insert_only_hash_map <T, Alloc>* result =
                                                     this->destructive_copy ();
              return result == NULL
                  ? NULL
                  : result->put_internal (k, v, true);
            }
          else
            {
              bucket.first = k;
              bucket.second = v;
              this->num_entries_++;
              return this;
            }
        }
      else if (!unique_key_and_resize_not_needed && bucket.first->equals (k))
        {
          /* Key was present.  Just change the value.  */
          bucket.second = v;
          return this;
        }
      else
        bucket_index = (bucket_index + step++) & mask;
    }
}

template <typename T, typename Alloc>
inline const typename insert_only_hash_map <T, Alloc>::value_type*
insert_only_hash_map <T, Alloc>::get (const insert_only_hash_map::key_type *k)
                                                                          const
{
  /* Table size is always a power of 2.  */
  const size_type mask = this->bucket_count () - 1;
  size_type bucket_index = k->hash & mask;
  size_type step = 1;
  for (;;)
    {
      const bucket_type &bucket = this->buckets[bucket_index];
      if (bucket.first == NULL)
        return NULL;
      else if (bucket.first->equals (k))
        return &bucket.second;
      else
        bucket_index = (bucket_index + step++) & mask;
    }
}

template <typename T, typename Alloc>
inline bool
insert_only_hash_map <T, Alloc>::key_type::equals (
             const typename insert_only_hash_map <T, Alloc>::key_type *k) const
{
  const char* x = reinterpret_cast <const char *> (k);
  const char* y = reinterpret_cast <const char *> (this);
  return (load8bytes (x) == load8bytes (y)
          && memcmp (x + 8, y + 8, this->n) == 0);
}

#endif  /* _VTV_MAP_H  */
