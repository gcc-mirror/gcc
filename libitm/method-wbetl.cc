/* Copyright (C) 2009, 2011 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libitm_i.h"

namespace {

using namespace GTM;

class wbetl_dispatch : public abi_dispatch
{
 private:
  static const size_t RW_SET_SIZE = 4096;

  struct r_entry
  {
    gtm_version version;
    gtm_stmlock *lock;
  };

  r_entry *m_rset_entries;
  size_t m_rset_nb_entries;
  size_t m_rset_size;

  struct w_entry
  {
    /* There's a hashtable where the locks are held, so multiple
       cachelines can hash to a given bucket.  This link points to the
       possible next cacheline that also hashes to this bucket.  */
    struct w_entry *next;

    /* Every entry in this bucket (accessed by NEXT) has the same LOCK
       address below.  */
    gtm_stmlock *lock;

    gtm_cacheline *addr;
    gtm_cacheline *value;
    gtm_version version;
  };

  w_entry *m_wset_entries;
  size_t m_wset_nb_entries;
  size_t m_wset_size;
  bool m_wset_reallocate;

  gtm_version m_start;
  gtm_version m_end;

  gtm_cacheline_page *m_cache_page;
  unsigned m_n_cache_page;

 private:
  bool local_w_entry_p (w_entry *w);
  bool has_read (gtm_stmlock *lock);
  bool validate();
  bool extend();

  gtm_cacheline *do_write_lock(gtm_cacheline *);
  gtm_cacheline *do_after_write_lock(gtm_cacheline *);
  const gtm_cacheline *do_read_lock(const gtm_cacheline *, bool);

 public:
  wbetl_dispatch();

  virtual const gtm_cacheline *read_lock(const gtm_cacheline *, ls_modifier);
  virtual mask_pair write_lock(gtm_cacheline *, ls_modifier);

  virtual bool trycommit();
  virtual void rollback();
  virtual void reinit();
  virtual void fini();
  virtual bool trydropreference (void *, size_t);
};

/* Check if W is one of our write locks.  */

inline bool
wbetl_dispatch::local_w_entry_p (w_entry *w)
{
  return (m_wset_entries <= w && w < m_wset_entries + m_wset_nb_entries);
}

/* Check if stripe has been read previously.  */

inline bool
wbetl_dispatch::has_read (gtm_stmlock *lock)
{
  // ??? Consider using an AA tree to lookup the r_set entries.
  size_t n = m_rset_nb_entries;
  for (size_t i = 0; i < n; ++i)
    if (m_rset_entries[i].lock == lock)
      return true;

  return false;
}

/* Validate read set, i.e. check if all read addresses are still valid now.  */

bool
wbetl_dispatch::validate ()
{
  __sync_synchronize ();

  size_t n = m_rset_nb_entries;
  for (size_t i = 0; i < n; ++i)
    {
      r_entry *r = &m_rset_entries[i];
      gtm_stmlock l = *r->lock;

      if (gtm_stmlock_owned_p (l))
	{
	  w_entry *w = (w_entry *) gtm_stmlock_get_addr (l);

	  // If someone has locked us, it better be by someone in the
	  // current thread.
	  if (!local_w_entry_p (w))
	    return false;
	}
      else if (gtm_stmlock_get_version (l) != r->version)
	return false;
    }

  return true;
}

/* Extend the snapshot range.  */

bool
wbetl_dispatch::extend ()
{
  gtm_version now = gtm_get_clock ();

  if (validate ())
    {
      m_end = now;
      return true;
    }
  return false;
}

/* Acquire a write lock on ADDR.  */

gtm_cacheline *
wbetl_dispatch::do_write_lock(gtm_cacheline *addr)
{
  gtm_stmlock *lock;
  gtm_stmlock l, l2;
  gtm_version version;
  w_entry *w, *prev = NULL;

  lock = gtm_get_stmlock (addr);
  l = *lock;

 restart_no_load:
  if (gtm_stmlock_owned_p (l))
    {
      w = (w_entry *) gtm_stmlock_get_addr (l);

      /* Did we previously write the same address?  */
      if (local_w_entry_p (w))
	{
	  prev = w;
	  while (1)
	    {
	      if (addr == prev->addr)
		return prev->value;
	      if (prev->next == NULL)
		break;
	      prev = prev->next;
	    }

	  /* Get version from previous entry write set.  */
	  version = prev->version;

	  /* If there's not enough entries, we must reallocate the array,
	     which invalidates all pointers to write set entries, which
	     means we have to restart the transaction.  */
	  if (m_wset_nb_entries == m_wset_size)
	    {
	      m_wset_size *= 2;
	      m_wset_reallocate = true;
	      gtm_tx()->restart (RESTART_REALLOCATE);
	    }

	  w = &m_wset_entries[m_wset_nb_entries];
	  goto do_write;
	}

      gtm_tx()->restart (RESTART_LOCKED_WRITE);
    }
  else
    {
      version = gtm_stmlock_get_version (l);

      /* We might have read an older version previously.  */
      if (version > m_end)
	{
	  if (has_read (lock))
	    gtm_tx()->restart (RESTART_VALIDATE_WRITE);
	}

      /* Extend write set, aborting to reallocate write set entries.  */
      if (m_wset_nb_entries == m_wset_size)
	{
	  m_wset_size *= 2;
	  m_wset_reallocate = true;
	  gtm_tx()->restart (RESTART_REALLOCATE);
	}

      /* Acquire the lock.  */
      w = &m_wset_entries[m_wset_nb_entries];
      l2 = gtm_stmlock_set_owned (w);
      l = __sync_val_compare_and_swap (lock, l, l2);
      if (l != l2)
	goto restart_no_load;
    }

 do_write:
  m_wset_nb_entries++;
  if (prev != NULL)
    prev->next = w;
  w->next = 0;
  w->lock = lock;
  w->addr = addr;
  w->version = version;

  gtm_cacheline_page *page = m_cache_page;
  unsigned index = m_n_cache_page;

  if (page == NULL || index == gtm_cacheline_page::LINES)
    {
      gtm_cacheline_page *npage = new gtm_cacheline_page;
      npage->prev = page;
      m_cache_page = page = npage;
      m_n_cache_page = 1;
      index = 0;
    }
  else
    m_n_cache_page = index + 1;

  gtm_cacheline *line = &page->lines[index];
  w->value = line;
  page->masks[index] = 0;
  *line = *addr;

  return line;
}

gtm_cacheline *
wbetl_dispatch::do_after_write_lock (gtm_cacheline *addr)
{
  gtm_stmlock *lock;
  gtm_stmlock l;
  w_entry *w;

  lock = gtm_get_stmlock (addr);
  l = *lock;
  assert (gtm_stmlock_owned_p (l));

  w = (w_entry *) gtm_stmlock_get_addr (l);
  assert (local_w_entry_p (w));

  while (1)
    {
      if (addr == w->addr)
	return w->value;
      w = w->next;
    }
}

/* Acquire a read lock on ADDR.  */

const gtm_cacheline *
wbetl_dispatch::do_read_lock (const gtm_cacheline *addr, bool after_read)
{
  gtm_stmlock *lock;
  gtm_stmlock l, l2;
  gtm_version version;
  w_entry *w;

  lock = gtm_get_stmlock (addr);
  l = *lock;

 restart_no_load:
  if (gtm_stmlock_owned_p (l))
    {
      w = (w_entry *) gtm_stmlock_get_addr (l);

      /* Did we previously write the same address?  */
      if (local_w_entry_p (w))
	{
	  while (1)
	    {
	      if (addr == w->addr)
		return w->value;
	      if (w->next == NULL)
		return addr;
	      w = w->next;
	    }
	}

      gtm_tx()->restart (RESTART_LOCKED_READ);
    }

  version = gtm_stmlock_get_version (l);

  /* If version is no longer valid, re-validate the read set.  */
  if (version > m_end)
    {
      if (!extend ())
	gtm_tx()->restart (RESTART_VALIDATE_READ);

      if (!after_read)
	{
	  // Verify that the version has not yet been overwritten.  The read
	  // value has not yet been added to read set and may not have been
	  // checked during the extend.
	  //
	  // ??? This only makes sense if we're actually reading the value
	  // and returning it now -- which I believe the original TinySTM
	  // did.  This doesn't make a whole lot of sense when we're
	  // manipulating cachelines as we are now.  Do we need some other
	  // form of lock verification here, or is the validate call in
	  // trycommit sufficient?

	  __sync_synchronize ();
	  l2 = *lock;
	  if (l != l2)
	    {
	      l = l2;
	      goto restart_no_load;
	    }
	}
    }

  if (!after_read)
    {
      r_entry *r;

      /* Add the address and version to the read set.  */
      if (m_rset_nb_entries == m_rset_size)
	{
	  m_rset_size *= 2;

	  m_rset_entries = (r_entry *)
	    xrealloc (m_rset_entries, m_rset_size * sizeof(r_entry));
	}
      r = &m_rset_entries[m_rset_nb_entries++];
      r->version = version;
      r->lock = lock;
    }

  return addr;
}

const gtm_cacheline *
wbetl_dispatch::read_lock (const gtm_cacheline *addr, ls_modifier ltype)
{
  switch (ltype)
    {
    case NONTXNAL:
      return addr;
    case R:
      return do_read_lock (addr, false);
    case RaR:
      return do_read_lock (addr, true);
    case RaW:
      return do_after_write_lock (const_cast<gtm_cacheline *>(addr));
    case RfW:
      return do_write_lock (const_cast<gtm_cacheline *>(addr));
    default:
      abort ();
    }
}

abi_dispatch::mask_pair
wbetl_dispatch::write_lock (gtm_cacheline *addr, ls_modifier ltype)
{
  gtm_cacheline *line;

  switch (ltype)
    {
    case NONTXNAL:
      return mask_pair (addr, &mask_sink);
    case W:
    case WaR:
      line = do_write_lock (addr);
      break;
    case WaW:
      line = do_after_write_lock (addr);
      break;
    default:
      abort ();
    }

  return mask_pair (line, gtm_cacheline_page::mask_for_page_line (line));
}

/* Commit the transaction.  */

bool
wbetl_dispatch::trycommit ()
{
  const size_t n = m_wset_nb_entries;
  if (n != 0)
    {
      /* Get commit timestamp.  */
      gtm_version t = gtm_inc_clock ();

      /* Validate only if a concurrent transaction has started since.  */
      if (m_start != t - 1 && !validate ())
	return false;

      /* Install new versions.  */
      for (size_t i = 0; i < n; ++i)
	{
	  w_entry *w = &m_wset_entries[i];
	  gtm_cacheline_mask mask
	    = *gtm_cacheline_page::mask_for_page_line (w->value);

	  /* Filter out any updates that overlap the libitm stack.  */
	  mask = gtm_mask_stack (w->addr, mask);

	  gtm_cacheline::copy_mask (w->addr, w->value, mask);
	}

      /* Only emit barrier after all cachelines are copied.  */
      gtm_cacheline::copy_mask_wb ();

      /* Drop locks.  */
      for (size_t i = 0; i < n; ++i)
	{
	  w_entry *w = &m_wset_entries[i];

	  /* Every link along the chain has the same lock, but only
	     bother dropping the lock once per bucket (at the end).  */
	  if (w->next == NULL)
	    *w->lock = gtm_stmlock_set_version (t);
	}
    }

  __sync_synchronize ();
  return true;
}

void
wbetl_dispatch::rollback ()
{
  /* Drop locks.  */
  const size_t n = m_wset_nb_entries;
  for (size_t i = 0; i < n; ++i)
    {
      w_entry *w = &m_wset_entries[i];

      /* Every link along the chain has the same lock, but only
	 bother dropping the lock once per bucket (at the end).  */
      if (w->next == NULL)
	*w->lock = gtm_stmlock_set_version (w->version);
    }

  __sync_synchronize ();
}

void
wbetl_dispatch::reinit ()
{
  gtm_cacheline_page *page;

  m_rset_nb_entries = 0;
  m_wset_nb_entries = 0;

  if (m_wset_reallocate)
    {
      m_wset_reallocate = 0;
      m_wset_entries = (w_entry *)
	xrealloc (m_wset_entries, m_wset_size * sizeof(w_entry));
    }

  page = m_cache_page;
  if (page)
    {
      /* Release all but one of the pages of cachelines.  */
      gtm_cacheline_page *prev = page->prev;
      if (prev)
	{
	  page->prev = 0;
	  delete prev;
	}

      /* Start the next cacheline allocation from the beginning.  */
      m_n_cache_page = 0;
    }

  m_start = m_end = gtm_get_clock ();
}

void
wbetl_dispatch::fini ()
{
  delete m_cache_page;
  free (m_rset_entries);
  free (m_wset_entries);
  delete this;
}

/* Attempt to drop any internal references to PTR.  Return TRUE if successful.

   This is an adaptation of the transactional memcpy function.

   What we do here is flush out the current transactional content of
   PTR to real memory, and remove the write mask bits associated with
   it so future commits will ignore this piece of memory.  */

bool
wbetl_dispatch::trydropreference (void *ptr, size_t size)
{
  if (size == 0)
    return true;

  if (!validate ())
    return false;

  uintptr_t isrc = (uintptr_t)ptr;
  // The position in the source cacheline where *PTR starts.
  uintptr_t sofs = isrc & (CACHELINE_SIZE - 1);
  gtm_cacheline *src
    = reinterpret_cast<gtm_cacheline *>(isrc & -CACHELINE_SIZE);
  unsigned char *dst = (unsigned char *)ptr;
  abi_dispatch::mask_pair pair;

  // If we're trying to drop a reference, we should already have a
  // write lock on it.  If we don't have one, there's no work to do.
  if (!gtm_stmlock_owned_p (*gtm_get_stmlock (src)))
    return true;

  // We copy the data in three stages:

  // (a) Copy stray bytes at the beginning that are smaller than a
  // cacheline.
  if (sofs != 0)
    {
      size_t sleft = CACHELINE_SIZE - sofs;
      size_t min = (size <= sleft ? size : sleft);

      // WaW will give us the current locked entry.
      pair = this->write_lock (src, WaW);

      // *jedi mind wave*...these aren't the droids you're looking for.
      *pair.mask &= ~((((gtm_cacheline_mask)1 << min) - 1) << sofs);

      memcpy (dst, &pair.line->b[sofs], min);
      dst += min;
      src++;
      size -= min;
    }

  // (b) Copy subsequent cacheline sized chunks.
  while (size >= CACHELINE_SIZE)
    {
      pair = this->write_lock(src, WaW);
      *pair.mask = 0;
      memcpy (dst, pair.line, CACHELINE_SIZE);
      dst += CACHELINE_SIZE;
      src++;
      size -= CACHELINE_SIZE;
    }

  // (c) Copy anything left over.
  if (size != 0)
    {
      pair = this->write_lock(src, WaW);
      *pair.mask &= ~(((gtm_cacheline_mask)1 << size) - 1);
      memcpy (dst, pair.line, size);
    }

  // No need to drop locks, since we're going to abort the transaction
  // anyhow.

  return true;
}


wbetl_dispatch::wbetl_dispatch ()
  : abi_dispatch (false, false)
{
  m_rset_entries = (r_entry *) xmalloc (RW_SET_SIZE * sizeof(r_entry));
  m_rset_nb_entries = 0;
  m_rset_size = RW_SET_SIZE;

  m_wset_entries = (w_entry *) xmalloc (RW_SET_SIZE * sizeof(w_entry));
  m_wset_nb_entries = 0;
  m_wset_size = RW_SET_SIZE;
  m_wset_reallocate = false;

  m_start = m_end = gtm_get_clock ();

  m_cache_page = 0;
  m_n_cache_page = 0;
}

} // anon namespace

abi_dispatch *
GTM::dispatch_wbetl ()
{
  return new wbetl_dispatch ();
}
