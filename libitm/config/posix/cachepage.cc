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
#include <pthread.h>

//
// We have three possibilities for alloction: mmap, memalign, posix_memalign
//

#if defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEV_ZERO)
#include <sys/mman.h>
#include <fcntl.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

namespace GTM HIDDEN {

#if defined(HAVE_MMAP_ANON)
# if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#  define MAP_ANONYMOUS MAP_ANON
# endif
# define dev_zero -1
#elif defined(HAVE_MMAP_DEV_ZERO)
# ifndef MAP_ANONYMOUS
#  define MAP_ANONYMOUS 0
# endif
static int dev_zero = -1;
#endif

#if defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEV_ZERO)
/* If we get here, we've already opened /dev/zero and verified that
   PAGE_SIZE is valid for the system.  */
static gtm_cacheline_page * alloc_mmap (void) UNUSED;
static gtm_cacheline_page *
alloc_mmap (void)
{
  gtm_cacheline_page *r;
  r = (gtm_cacheline_page *) mmap (NULL, PAGE_SIZE, PROT_READ | PROT_WRITE,
				   MAP_PRIVATE | MAP_ANONYMOUS, dev_zero, 0);
  if (r == (gtm_cacheline_page *) MAP_FAILED)
    abort ();
  return r;
}
#endif /* MMAP_ANON | MMAP_DEV_ZERO */

#ifdef HAVE_MEMALIGN
static gtm_cacheline_page * alloc_memalign (void) UNUSED;
static gtm_cacheline_page *
alloc_memalign (void)
{
  gtm_cacheline_page *r;
  r = (gtm_cacheline_page *) memalign (PAGE_SIZE, PAGE_SIZE);
  if (r == NULL)
    abort ();
  return r;
}
#endif /* MEMALIGN */

#ifdef HAVE_POSIX_MEMALIGN
static gtm_cacheline_page *alloc_posix_memalign (void) UNUSED;
static gtm_cacheline_page *
alloc_posix_memalign (void)
{
  void *r;
  if (posix_memalign (&r, PAGE_SIZE, PAGE_SIZE))
    abort ();
  return (gtm_cacheline_page *) r;
}
#endif /* POSIX_MEMALIGN */

#if defined(HAVE_MMAP_ANON) && defined(FIXED_PAGE_SIZE)
# define alloc_page  alloc_mmap
#elif defined(HAVE_MMAP_DEV_ZERO) && defined(FIXED_PAGE_SIZE)
static gtm_cacheline_page *
alloc_page (void)
{
  if (dev_zero < 0)
    {
      dev_zero = open ("/dev/zero", O_RDWR);
      assert (dev_zero >= 0);
    }
  return alloc_mmap ();
}
#elif defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEV_ZERO)
static gtm_cacheline_page * (*alloc_page) (void);
static void __attribute__((constructor))
init_alloc_page (void)
{
  size_t page_size = getpagesize ();
  if (page_size <= PAGE_SIZE && PAGE_SIZE % page_size == 0)
    {
# ifndef HAVE_MMAP_ANON
      dev_zero = open ("/dev/zero", O_RDWR);
      assert (dev_zero >= 0);
# endif
      alloc_page = alloc_mmap;
      return;
    }
# ifdef HAVE_MEMALIGN
  alloc_page = alloc_memalign;
# elif defined(HAVE_POSIX_MEMALIGN)
  alloc_page = alloc_posix_memalign;
# else
#  error "No fallback aligned memory allocation method"
# endif
}
#elif defined(HAVE_MEMALIGN)
# define alloc_page  alloc_memalign
#elif defined(HAVE_POSIX_MEMALIGN)
# define alloc_page  alloc_posix_memalign
#else
# error "No aligned memory allocation method"
#endif

static gtm_cacheline_page *free_pages;
static pthread_mutex_t free_page_lock = PTHREAD_MUTEX_INITIALIZER;

void *
gtm_cacheline_page::operator new (size_t size)
{
  assert (size == sizeof (gtm_cacheline_page));
  assert (size <= PAGE_SIZE);

  pthread_mutex_lock(&free_page_lock);

  gtm_cacheline_page *r = free_pages;
  free_pages = r ? r->prev : NULL;

  pthread_mutex_unlock(&free_page_lock);

  if (r == NULL)
    r = alloc_page ();

  return r;
}

void
gtm_cacheline_page::operator delete (void *xhead)
{
  gtm_cacheline_page *head = static_cast<gtm_cacheline_page *>(xhead);
  gtm_cacheline_page *tail;

  if (head == 0)
    return;

  /* ??? We should eventually really free some of these.  */

  for (tail = head; tail->prev != 0; tail = tail->prev)
    continue;

  pthread_mutex_lock(&free_page_lock);

  tail->prev = free_pages;
  free_pages = head;

  pthread_mutex_unlock(&free_page_lock);
}

} // namespace GTM
