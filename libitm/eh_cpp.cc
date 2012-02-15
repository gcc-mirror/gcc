/* Copyright (C) 2009, 2011, 2012 Free Software Foundation, Inc.
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

using namespace GTM;

/* Everything from libstdc++ is weak, to avoid requiring that library
   to be linked into plain C applications using libitm.so.  */

#define WEAK  __attribute__((weak))

extern "C" {

extern void *__cxa_allocate_exception (size_t) WEAK;
extern void __cxa_throw (void *, void *, void *) WEAK;
extern void *__cxa_begin_catch (void *) WEAK;
extern void *__cxa_end_catch (void) WEAK;
extern void __cxa_tm_cleanup (void *, void *, unsigned int) WEAK;

#if !defined (HAVE_ELF_STYLE_WEAKREF) && !defined (__MACH__)
void *__cxa_allocate_exception (size_t) { return NULL; }
void __cxa_throw (void *, void *, void *) { return; }
void *__cxa_begin_catch (void *) { return NULL; }
void *__cxa_end_catch (void) { return NULL; }
void __cxa_tm_cleanup (void *, void *, unsigned int) { return; }
void _Unwind_DeleteException (_Unwind_Exception *) { return; }
#endif /* HAVE_ELF_STYLE_WEAKREF */

}


void *
_ITM_cxa_allocate_exception (size_t size)
{
  void *r = __cxa_allocate_exception (size);
  gtm_thr()->cxa_unthrown = r;
  return r;
}

void
_ITM_cxa_throw (void *obj, void *tinfo, void *dest)
{
  gtm_thr()->cxa_unthrown = NULL;
  __cxa_throw (obj, tinfo, dest);
}

void *
_ITM_cxa_begin_catch (void *exc_ptr)
{
  gtm_thr()->cxa_catch_count++;
  return __cxa_begin_catch (exc_ptr);
}

void
_ITM_cxa_end_catch (void)
{
  gtm_thr()->cxa_catch_count--;
  __cxa_end_catch ();
}

void
GTM::gtm_thread::revert_cpp_exceptions (gtm_transaction_cp *cp)
{
  if (cp)
    {
      // If rolling back a nested transaction, only clean up unthrown
      // exceptions since the last checkpoint. Always reset eh_in_flight
      // because it just contains the argument provided to
      // _ITM_commitTransactionEH
      void *unthrown =
	  (cxa_unthrown != cp->cxa_unthrown ? cxa_unthrown : NULL);
      assert (cxa_catch_count >= cp->cxa_catch_count);
      uint32_t catch_count = cxa_catch_count - cp->cxa_catch_count;
      if (unthrown || catch_count)
	{
	  __cxa_tm_cleanup (unthrown, this->eh_in_flight, catch_count);
	  cxa_catch_count = cp->cxa_catch_count;
	  cxa_unthrown = cp->cxa_unthrown;
	  this->eh_in_flight = NULL;
	}
    }
  else
    {
      // Both cxa_catch_count and cxa_unthrown are maximal because EH regions
      // and transactions are properly nested.
      if (this->cxa_unthrown || this->cxa_catch_count)
	{
	  __cxa_tm_cleanup (this->cxa_unthrown, this->eh_in_flight,
	      this->cxa_catch_count);
	  this->cxa_catch_count = 0;
	  this->cxa_unthrown = NULL;
	  this->eh_in_flight = NULL;
	}
    }
}
