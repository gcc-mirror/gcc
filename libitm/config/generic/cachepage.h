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

#ifndef LIBITM_CACHEPAGE_H
#define LIBITM_CACHEPAGE_H 1

namespace GTM HIDDEN {

// A "page" worth of saved cachelines plus modification masks.  This
// arrangement is intended to minimize the overhead of alignment.  The
// PAGE_SIZE defined by the target must be a constant for this to work,
// which means that this definition may not be the same as the real
// system page size.  An additional define of FIXED_PAGE_SIZE by the
// target indicates that PAGE_SIZE exactly matches the system page size.

#ifndef PAGE_SIZE
#define PAGE_SIZE 4096
#endif

struct gtm_cacheline_page
{
  static const size_t LINES
    = ((PAGE_SIZE - sizeof(gtm_cacheline_page *))
       / (CACHELINE_SIZE + sizeof(gtm_cacheline_mask)));

  gtm_cacheline lines[LINES] __attribute__((aligned(PAGE_SIZE)));
  gtm_cacheline_mask masks[LINES];
  gtm_cacheline_page *prev;

  static gtm_cacheline_page *
  page_for_line (gtm_cacheline *c)
  {
    return (gtm_cacheline_page *)((uintptr_t)c & -PAGE_SIZE);
  }

  gtm_cacheline_mask *
  mask_for_line (gtm_cacheline *c)
  {
    size_t index = c - &this->lines[0];
    return &this->masks[index];
  }

  static gtm_cacheline_mask *
  mask_for_page_line (gtm_cacheline *c)
  {
    gtm_cacheline_page *p = page_for_line (c);
    return p->mask_for_line (c);
  }

  static void *operator new (size_t);
  static void operator delete (void *);
};

} // namespace GTM

#endif // LIBITM_CACHEPAGE_H
