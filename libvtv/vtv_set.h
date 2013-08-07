/* Copyright (C) 2012-2013
   Free Software Foundation

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License
   and a copy of the GCC Runtime Library Exception along with this
   program; see the files COPYING3 and COPYING.RUNTIME respectively.
   If not, see <http://www.gnu.org/licenses/>.  */

#ifndef _VTV_SET_H
#define _VTV_SET_H 1

/* Code in this file manages a collection of insert-only sets.  We
   have only tested the case where Key is uintptr_t, though it
   theoretically should work for some other cases.  All odd keys are
   reserved, and must not be inserted into any of the sets.  This code
   is intended primarily for sets of pointers, and the code is
   optimized for small sets (including size 0 and 1), but regardless
   of the set size, insert() and contains() have close to O(1) speed
   in practice.

   TODO(gpike): fix this comment.

   Recommended multithreaded use of a set:

   For speed, we want to use a lock-free test for set membership.  The
   code handles simultaneous reads and inserts, as long as at most one
   insertion is in progress at a time.  After an insert, other threads
   may not immediately "see" the inserted key if they perform a
   lock-free read, so we recommend retrying, as explained below.

   Also, to make data corruption less likely, we recommend using a
   "normal" RW page as well as one or pages that are typically RO
   but that can be switched to RW and back as needed.  The latter
   pages should contain sets.  The former should contain a lock, L,
   and an int or similar, num_writers.  Then, to insert, something
   like this would be safe:
    o Acquire L.
    o Increment num_writers; if that made it 1, change pages to RW.
    o Release L.
    o while (there are insertions to do in some set, S) {
        acquire L;
        do some insertions in S;
        release L;
      }
    o Acquire L.
    o Decrement num_writers; if that made it 0, change pages to RO.
    o Release L.

   And to check if the set contains some key, one could use
     set.contains(key) ||
       ({ Acquire L; bool b = set.contains(key); Release L; b; })

   In this scheme, the number of threads with reads in progress isn't
   tracked, so old sets can never be deleted.  In addition, on some
   architectures the intentionally racy reads might cause contains()
   to return true when it should have returned false.  This should be
   no problem on x86, and most other machines, where reading or
   writing an aligned uintptr_t is atomic.  E.g., on those machines,
   if *p is 0 and one thread does *p = x while another reads *p, the
   read will see either 0 or x.

   To make the above easier, the insert_only_hash_sets class provides
   an interface to manipulate any number of hash sets.  One shouldn't
   create objects of that class, as it has no member data and its
   methods are static.

   So the recommended model is to have a single lock, a single
   num_writers variable, and some number of sets.  If lock contention
   becomes a problem then the sets can be divided into k groups, each
   of which has a lock and a num_writers variable; or each set can be
   represented as a set of values that equal 0 mod m, a set of values
   that equal 1 mod m, ..., plus a set of values that equal m-1 mod m.

   However, we expect most or all uses of this code to call contains()
   much more frequently than anything else, so lock contention is
   likely to be low.  */

#include <algorithm>

#ifndef HASHTABLE_STATS
#define HASHTABLE_STATS 0
#endif

#ifndef HASHTABLE_STATS_ATOMIC
#define HASHTABLE_STATS_ATOMIC 0
#endif

#if HASHTABLE_STATS
#if HASHTABLE_STATS_ATOMIC
/* Stat counters, with atomics. */
#include <bits/atomic_word.h>

typedef _Atomic_word _AtomicStatCounter;

void
inc_by (_AtomicStatCounter &stat, int amount)
{ 
  __atomic_add_fetch (&stat, amount,  __ATOMIC_ACQ_REL);
}

#else

/* Stat counters, but without atomics. */
typedef int _AtomicStatCounter;

void
inc_by (_AtomicStatCounter& stat, int amount)
{ 
  stat += amount;
}

#endif


/* Number of calls to contains(), insert(), etc. */
extern _AtomicStatCounter stat_insert;
extern _AtomicStatCounter stat_contains;
extern _AtomicStatCounter stat_resize;
extern _AtomicStatCounter stat_create;

/* Sum of set size over all calls to contains().  */
extern _AtomicStatCounter stat_contains_sizes;

/* contains() calls in a set whose capacity is more than 1. */
extern _AtomicStatCounter stat_contains_in_non_trivial_set;

/* Probes in a set whose capacity is more than 1.  Ideally, this will
   be pretty close to stat_contains_in_non_trivial_set.  That will
   happen if our hash function is good and/or important keys were
   inserted before unimportant keys.  */
extern _AtomicStatCounter stat_probes_in_non_trivial_set;

/* number of calls to contains() with size=0, 1, etc. */
extern _AtomicStatCounter stat_contains_size0;
extern _AtomicStatCounter stat_contains_size1;
extern _AtomicStatCounter stat_contains_size2;
extern _AtomicStatCounter stat_contains_size3;
extern _AtomicStatCounter stat_contains_size4;
extern _AtomicStatCounter stat_contains_size5;
extern _AtomicStatCounter stat_contains_size6;
extern _AtomicStatCounter stat_contains_size7;
extern _AtomicStatCounter stat_contains_size8;
extern _AtomicStatCounter stat_contains_size9;
extern _AtomicStatCounter stat_contains_size10;
extern _AtomicStatCounter stat_contains_size11;
extern _AtomicStatCounter stat_contains_size12;
extern _AtomicStatCounter stat_contains_size13_or_more;
extern _AtomicStatCounter stat_grow_from_size0_to_1;
extern _AtomicStatCounter stat_grow_from_size1_to_2;
extern _AtomicStatCounter stat_double_the_number_of_buckets;
extern _AtomicStatCounter stat_insert_key_that_was_already_present;

/* Hash collisions detected during insert_no_resize().  Only counts
   hasher(k) == hasher(k'); hasher(k) % tablesize == hasher(k') %
   tablesize is not sufficient.  Will count collisions that are
   detected during table resizes etc., so the same two keys may add to
   this stat multiple times.  */
extern _AtomicStatCounter stat_insert_found_hash_collision;

#include <string>

struct insert_only_hash_sets_logger
{
  static char *
  log (char c, char *buf)
  {
    *buf++ = c;
    return buf;
  }

  static char *
  log (const char *s, char *buf)
  { return strcpy (buf, s) + strlen (s); }

  static char *
  log (_AtomicStatCounter i, char *buf)
  {
    if (i < 10)
      return log ((char) ('0' + i), buf);
    else
      return log ((char) ('0' + i % 10), log (i / 10, buf));
  }

  static char *
  log (const char *label, _AtomicStatCounter i, char *buf)
  {
    buf = log (label, buf);
    buf = log (": ", buf);
    buf = log (i, buf);
    return log ('\n', buf);
  }
};

// Write stats to the given buffer, which should be at least 4000 bytes.
static inline void
insert_only_hash_tables_stats (char *buf)
{
  buf = insert_only_hash_sets_logger::log ("insert", stat_insert, buf);
  buf = insert_only_hash_sets_logger::log ("contains", stat_contains, buf);
  buf = insert_only_hash_sets_logger::log ("resize", stat_resize, buf);
  buf = insert_only_hash_sets_logger::log ("create", stat_create, buf);
  buf = insert_only_hash_sets_logger::log ("insert_key_that_was_already_"
				      "present",
				      stat_insert_key_that_was_already_present,
				      buf);
  buf = insert_only_hash_sets_logger::log ("contains_sizes",
					   stat_contains_sizes, buf);
  buf = insert_only_hash_sets_logger::log ("contains_in_non_trivial_set",
					   stat_contains_in_non_trivial_set,
					   buf);
  buf = insert_only_hash_sets_logger::log ("probes_in_non_trivial_set",
					   stat_probes_in_non_trivial_set,
					   buf);
  buf = insert_only_hash_sets_logger::log ("contains_size0",
					   stat_contains_size0, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size1",
					   stat_contains_size1, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size2",
					   stat_contains_size2, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size3",
					   stat_contains_size3, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size4",
					   stat_contains_size4, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size5",
					   stat_contains_size5, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size6",
					   stat_contains_size6, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size7",
					   stat_contains_size7, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size8",
					   stat_contains_size8, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size9",
					   stat_contains_size9, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size10",
					   stat_contains_size10, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size11",
					   stat_contains_size11, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size12",
					   stat_contains_size12, buf);
  buf = insert_only_hash_sets_logger::log ("contains_size13_or_more",
					   stat_contains_size13_or_more, buf);
  buf = insert_only_hash_sets_logger::log ("grow_from_size0_to_1",
					   stat_grow_from_size0_to_1, buf);
  buf = insert_only_hash_sets_logger::log ("grow_from_size1_to_2",
					   stat_grow_from_size1_to_2, buf);
  buf = insert_only_hash_sets_logger::log ("insert_found_hash_collision",
					   stat_insert_found_hash_collision,
					   buf);
  buf = insert_only_hash_sets_logger::log ("double_the_number_of_buckets",
					   stat_double_the_number_of_buckets,
					   buf);
  *buf = '\0';
}

#else

/* No stats. */
#define inc_by(statname, amount) do { } while (false && (amount))

#endif

#define inc(statname) inc_by (statname, 1)

template <typename Key, class HashFcn, class Alloc>
class insert_only_hash_sets
{
 public:
  typedef Key key_type;
  typedef size_t size_type;
  typedef Alloc alloc_type;
  enum { illegal_key = 1 };
  enum { min_capacity = 4 };
#if HASHTABLE_STATS
  enum { stats = true };
#else
  enum { stats = false };
#endif

  /* Do not directly use insert_only_hash_set.  Instead, use the
     static methods below to create and manipulate objects of the
     following class.
  
     Implementation details: each set is represented by a pointer
     plus, perhaps, out-of-line data, which would be an object of type
     insert_only_hash_set.  For a pointer, s, the interpretation is: s
     == NULL means empty set, lsb(s) == 1 means a set with one
     element, which is (uintptr_t)s - 1, and otherwise s is a pointer
     of type insert_only_hash_set*.  So, to increase the size of a set
     we have to change s and/or *s.  To check if a set contains some
     key we have to examine s and possibly *s.  */
  class insert_only_hash_set
  {
   public:
    /* Insert a key.  The key must not be a reserved key.  */
    static inline insert_only_hash_set *insert (key_type key,
						insert_only_hash_set *s);
    

    /* Create an empty set.  */
    static inline insert_only_hash_set *create (size_type capacity);

    /* Return whether the given key is present.  If key is illegal_key
       then either true or false may be returned, but for all other
       reserved keys false will be returned.  */
    static bool
    contains (key_type key, const insert_only_hash_set *s)
    {
      if (stats)
	{
	  inc (stat_contains);
	  switch (size (s))
	    {
	      case 0: inc (stat_contains_size0); break;
	      case 1: inc (stat_contains_size1); break;
	      case 2: inc (stat_contains_size2); break;
	      case 3: inc (stat_contains_size3); break;
	      case 4: inc (stat_contains_size4); break;
	      case 5: inc (stat_contains_size5); break;
	      case 6: inc (stat_contains_size6); break;
	      case 7: inc (stat_contains_size7); break;
	      case 8: inc (stat_contains_size8); break;
	      case 9: inc (stat_contains_size9); break;
	      case 10: inc (stat_contains_size10); break;
	      case 11: inc (stat_contains_size11); break;
	      case 12: inc (stat_contains_size12); break;
	      default: inc (stat_contains_size13_or_more); break;
	    }
          inc_by (stat_contains_sizes, size (s));
	}

      return (singleton (s) ?
              singleton_key (key) == s :
              ((s != NULL) && s->contains (key)));
    }

    /* Return a set's size.  */
    static size_type
    size (const insert_only_hash_set *s)
    { return (s == NULL) ? 0 : (singleton (s) ? 1 : s->num_entries); }

    static inline insert_only_hash_set *resize (size_type target_num_buckets,
						insert_only_hash_set *s);
    

   private:
    /* Return whether a set has size 1. */
    static bool
    singleton (const insert_only_hash_set *s)
    { return (uintptr_t) s & 1; }

    /* Return the representation of a singleton set containing the
       given key.  */
    static insert_only_hash_set *
    singleton_key (key_type key)
    { return (insert_only_hash_set *) ((uintptr_t) key + 1); }

    /* Given a singleton set, what key does it contain?  */
    static key_type
    extract_singleton_key (const insert_only_hash_set *s)
    {
      VTV_DEBUG_ASSERT (singleton (s));
      return (key_type) ((uintptr_t) s - 1);
    }

    volatile key_type &
    key_at_index (size_type index)
    { return buckets[index]; }

    key_type
    key_at_index (size_type index) const
    { return buckets[index]; }

    size_type
    next_index (size_type index, size_type indices_examined) const
    { return (index + indices_examined) & (num_buckets - 1); }

    inline void insert_no_resize (key_type key);
    
    inline bool contains (key_type key) const;
    
    inline insert_only_hash_set *resize_if_necessary (void);
    
    size_type num_buckets;  /* Must be a power of 2 not less than
			       min_capacity.  */
    volatile size_type num_entries;
    volatile key_type buckets[0];  /* Actual array size is num_buckets.  */
  };

  /* Create an empty set with the given capacity.  Requires that n be
     0 or a power of 2.  If 1 < n < min_capacity then treat n as
     min_capacity.  Sets *handle.  Returns true unless the allocator
     fails.  Subsequent operations on this set should use the same
     handle. */

  static inline bool create (size_type n, insert_only_hash_set **handle);

  /* Force the capacity of a set to be n, unless it was more than n
     already.  Requires that n be 0 or a power of 2.  Sets *handle
     unless the current capacity is n or more.  Returns true unless
     the allocator fails.  */

  static inline bool resize (size_type n, insert_only_hash_set **handle);

  /* Insert a key.  *handle is unmodified unless (1) a resize occurs,
     or (2) the set was initially empty. Returns true unless the
     allocator fails during a resize.  If the allocator fails during a
     resize then the set is reset to be the empty set.  The key must
     not be a reserved key.  */

  static inline bool insert (key_type key, insert_only_hash_set **handle);

  /* Check for the presence of a key.  If key is illegal_key then
     either true or false may be returned, but for all other reserved
     keys false will be returned.  */

  static inline bool
  contains (key_type key, /* const */ insert_only_hash_set **handle)
  { return insert_only_hash_set::contains (key, *handle); }

  /* Return the size of the given set.  */
  static size_type
  size (const insert_only_hash_set **handle)
  { return insert_only_hash_set::size (*handle); }

  static bool
  is_reserved_key (key_type key)
  { return ((uintptr_t) key % 2) == 1; }
};

template <typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set *
insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set::resize
                                         (size_type n, insert_only_hash_set *s)
{
  if (s == NULL)
    return create (n);

  size_type capacity = singleton (s) ? 1 : s->num_buckets;

  if (n <= capacity)
    return s;

  insert_only_hash_set *result =
                                create (std::max<size_type> (n, min_capacity));
  if (result != NULL)
    {
      if (singleton (s))
        {
          result->insert_no_resize (extract_singleton_key (s));
        }
      else
        {
          for (size_type i = 0; i < s->num_buckets; i++)
            if (s->buckets[i] != (key_type) illegal_key)
              result->insert_no_resize (s->buckets[i]);
        }
      VTV_DEBUG_ASSERT (size (result) == size (s));
    }
  return result;
}

template <typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set *
insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set::insert 
                                        (key_type key, insert_only_hash_set *s)
{
  VTV_DEBUG_ASSERT (!is_reserved_key (key));

  inc_by (stat_grow_from_size0_to_1, s == NULL);

  if (s == NULL)
    return singleton_key (key);

  if (singleton (s))
    {
      const key_type old_key = extract_singleton_key (s);
      if (old_key == key)
	return s;

      /* Grow from size 1 to size 2.  */
      inc (stat_grow_from_size1_to_2);
      s = create (2);
      if (s == NULL)
	return NULL;

      s->insert_no_resize (old_key);
      s->insert_no_resize (key);
      VTV_DEBUG_ASSERT (size (s) == 2);
      return s;
    }
  s = s->resize_if_necessary();
  if (s != NULL)
    s->insert_no_resize (key);
  return s;
}

template <typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set *
insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set::create
                                                           (size_type capacity)
{
  if (capacity <= 1)
    return NULL;

  VTV_DEBUG_ASSERT (capacity > 1 && (capacity & (capacity - 1)) == 0);
  VTV_DEBUG_ASSERT (sizeof (insert_only_hash_set) == 2 * sizeof (size_type));
  capacity = std::max <size_type> (capacity, min_capacity);
  const size_t num_bytes = sizeof (insert_only_hash_set) +
                                                  sizeof (key_type) * capacity;
  alloc_type alloc;
  insert_only_hash_set *result = (insert_only_hash_set *) alloc (num_bytes);
  result->num_buckets = capacity;
  result->num_entries = 0;
  for (size_type i = 0; i < capacity; i++)
    result->buckets[i] = (key_type) illegal_key;
  return result;
}

template <typename Key, class HashFcn, class Alloc>
void
insert_only_hash_sets<Key, HashFcn,
                                 Alloc>::insert_only_hash_set::insert_no_resize
                                                                 (key_type key)
{
  HashFcn hasher;
  const size_type capacity = num_buckets;
  VTV_DEBUG_ASSERT (capacity >= min_capacity);
  VTV_DEBUG_ASSERT (!is_reserved_key (key));
  size_type index = hasher (key) & (capacity - 1);
  key_type k = key_at_index (index);
  size_type indices_examined = 0;
  while (k != key)
    {
      ++indices_examined;
      if (k == (key_type) illegal_key)
        {
          key_at_index (index) = key;
          ++num_entries;
          return;
        }
      else
	{
	  inc_by (stat_insert_found_hash_collision,
		  hasher (k) == hasher (key));
	}
      VTV_DEBUG_ASSERT (indices_examined < capacity);
      index = next_index (index, indices_examined);
      k = key_at_index (index);
    }
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::contains
                                                           (key_type key) const
{
  inc (stat_contains_in_non_trivial_set);
  HashFcn hasher;
  const size_type capacity = num_buckets;
  size_type index = hasher (key) & (capacity - 1);
  key_type k = key_at_index (index);
  size_type indices_examined = 0;
  inc (stat_probes_in_non_trivial_set);
  while (k != key)
    {
      ++indices_examined;
      if (/*UNLIKELY*/(k == (key_type) illegal_key
		       || indices_examined == capacity))
	return false;

      index = next_index (index, indices_examined);
      k = key_at_index (index);
      inc (stat_probes_in_non_trivial_set);
    }
  return true;
}

template <typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets <Key, HashFcn, Alloc>::insert_only_hash_set *
   insert_only_hash_sets<Key, HashFcn,
                       Alloc>::insert_only_hash_set::resize_if_necessary (void)
{
  VTV_DEBUG_ASSERT (num_buckets >= min_capacity);
  size_type unused = num_buckets - num_entries;
  if (unused < (num_buckets >> 2))
    {
      inc (stat_double_the_number_of_buckets);
      size_type new_num_buckets = num_buckets * 2;
      insert_only_hash_set *s = create (new_num_buckets);
      for (size_type i = 0; i < num_buckets; i++)
        if (buckets[i] != (key_type) illegal_key)
          s->insert_no_resize (buckets[i]);
      VTV_DEBUG_ASSERT (size (this) == size (s));
      return s;
    }
  else
    return this;
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::create (size_type n,
						 insert_only_hash_set **handle)
  
{
  inc (stat_create);
  *handle = insert_only_hash_set::create (n);
  return (n <= 1) || (*handle != NULL);
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::resize (size_type n,
					         insert_only_hash_set **handle)
{
  inc (stat_resize);
  *handle = insert_only_hash_set::resize (n, *handle);
  return (n <= 1) || (*handle != NULL);
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::insert (key_type key,
                                                 insert_only_hash_set **handle)
{
  inc (stat_insert);
  const size_type old_size = insert_only_hash_set::size (*handle);
  *handle = insert_only_hash_set::insert (key, *handle);
  if (*handle != NULL)
    {
      const size_type delta = insert_only_hash_set::size (*handle) - old_size;
      inc_by (stat_insert_key_that_was_already_present, delta == 0);
    }
  return *handle != NULL;
}

#endif /* VTV_SET_H  */
