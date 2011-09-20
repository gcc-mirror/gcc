/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_access.h"
#include "upc_sync.h"
#include "upc_sup.h"
#include "upc_mem.h"

//begin lib_inline_access

__attribute__((__always_inline__))
static inline
void *
__upc_access_sptr_to_addr (upc_shared_ptr_t p)
{
  if (GUPCR_PTS_IS_NULL (p))
    __upc_fatal ("Invalid access via null shared pointer");
  if ((int)GUPCR_PTS_THREAD(p) >= THREADS)
    __upc_fatal ("Thread number in shared address is out of range");
  return __upc_sptr_to_addr (p);
}

//inline
u_intQI_t
__getqi2 (upc_shared_ptr_t p)
{
  const u_intQI_t *addr = (u_intQI_t *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
u_intHI_t
__gethi2 (upc_shared_ptr_t p)
{
  const u_intHI_t *addr = (u_intHI_t *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
u_intSI_t
__getsi2 (upc_shared_ptr_t p)
{
  const u_intSI_t *addr = (u_intSI_t *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
u_intDI_t
__getdi2 (upc_shared_ptr_t p)
{
  const u_intDI_t *addr = (u_intDI_t *) __upc_access_sptr_to_addr (p);
  return *addr;
}

#if GUPCR_TARGET64
//inline
u_intTI_t
__getti2 (upc_shared_ptr_t p)
{
  const u_intTI_t *addr = (u_intTI_t *) __upc_access_sptr_to_addr (p);
  return *addr;
}
#endif /* GUPCR_TARGET64 */

//inline
float
__getsf2 (upc_shared_ptr_t p)
{
  const float *addr = (float *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
double
__getdf2 (upc_shared_ptr_t p)
{
  const double *addr = (double *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
long double
__gettf2 (upc_shared_ptr_t p)
{
  const long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
long double
__getxf2 (upc_shared_ptr_t p)
{
  const long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  return *addr;
}

//inline
void
__getblk3 (void *dest, upc_shared_ptr_t src, size_t n)
{
  __upc_memget (dest, src, n);
}

//inline
void
__putqi2 (upc_shared_ptr_t p, u_intQI_t v)
{
  u_intQI_t * const addr = (u_intQI_t *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__puthi2 (upc_shared_ptr_t p, u_intHI_t v)
{
  u_intHI_t * const addr = (u_intHI_t *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__putsi2 (upc_shared_ptr_t p, u_intSI_t v)
{
  u_intSI_t * const addr = (u_intSI_t *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__putdi2 (upc_shared_ptr_t p, u_intDI_t v)
{
  u_intDI_t * const addr = (u_intDI_t *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

#if GUPCR_TARGET64
//inline
void
__putti2 (upc_shared_ptr_t p, u_intTI_t v)
{
  u_intTI_t * const addr = (u_intTI_t *) __upc_access_sptr_to_addr (p);
  *addr = v;
}
#endif /* GUPCR_TARGET64 */

//inline
void
__putsf2 (upc_shared_ptr_t p, float v)
{
  float * const addr = (float *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__putdf2 (upc_shared_ptr_t p, double v)
{
  double * const addr = (double *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__puttf2 (upc_shared_ptr_t p, long double v)
{
  long double * const addr = (long double *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__putxf2 (upc_shared_ptr_t p, long double v)
{
  long double * const addr = (long double *) __upc_access_sptr_to_addr (p);
  *addr = v;
}

//inline
void
__putblk3 (upc_shared_ptr_t dest, void *src, size_t n)
{
  __upc_memput (dest, src, n);
}

//inline
void
__copyblk3 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n)
{
  __upc_memcpy (dest, src, n);
}

/* Strict memory accesses. */

//inline
u_intQI_t
__getsqi2 (upc_shared_ptr_t p)
{
  const u_intQI_t *addr = (u_intQI_t *) __upc_access_sptr_to_addr (p);
  u_intQI_t result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
u_intHI_t
__getshi2 (upc_shared_ptr_t p)
{
  const u_intHI_t *addr = (u_intHI_t *) __upc_access_sptr_to_addr (p);
  u_intHI_t result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
u_intSI_t
__getssi2 (upc_shared_ptr_t p)
{
  const u_intSI_t *addr = (u_intSI_t *) __upc_access_sptr_to_addr (p);
  u_intSI_t result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
u_intDI_t
__getsdi2 (upc_shared_ptr_t p)
{
  const u_intDI_t *addr = (u_intDI_t *) __upc_access_sptr_to_addr (p);
  u_intDI_t result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

#if GUPCR_TARGET64
//inline
u_intTI_t
__getsti2 (upc_shared_ptr_t p)
{
  const u_intTI_t *addr = (u_intTI_t *) __upc_access_sptr_to_addr (p);
  u_intTI_t result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}
#endif /* GUPCR_TARGET64 */

//inline
float
__getssf2 (upc_shared_ptr_t p)
{
  const float *addr = (float *) __upc_access_sptr_to_addr (p);
  float result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
double
__getsdf2 (upc_shared_ptr_t p)
{
  const double *addr = (double *) __upc_access_sptr_to_addr (p);
  double result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
long double
__getstf2 (upc_shared_ptr_t p)
{
  const long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  long double result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
long double
__getsxf2 (upc_shared_ptr_t p)
{
  const long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  long double result;
  GUPCR_FENCE ();
  result = *addr;
  GUPCR_READ_FENCE ();
  return result;
}

//inline
void
__getsblk3 (void *dest, upc_shared_ptr_t src, size_t len)
{
  GUPCR_FENCE ();
  __getblk3 (dest, src, len);
  GUPCR_READ_FENCE ();
}

//inline
void
__putsqi2 (upc_shared_ptr_t p, u_intQI_t v)
{
  u_intQI_t *addr = (u_intQI_t *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putshi2 (upc_shared_ptr_t p, u_intHI_t v)
{
  u_intHI_t *addr = (u_intHI_t *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putssi2 (upc_shared_ptr_t p, u_intSI_t v)
{
  u_intSI_t *addr = (u_intSI_t *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putsdi2 (upc_shared_ptr_t p, u_intDI_t v)
{
  u_intDI_t *addr = (u_intDI_t *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

#if GUPCR_TARGET64
//inline
void
__putsti2 (upc_shared_ptr_t p, u_intTI_t v)
{
  u_intTI_t *addr = (u_intTI_t *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}
#endif /* GUPCR_TARGET64 */

//inline
void
__putssf2 (upc_shared_ptr_t p, float v)
{
  float *addr = (float *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putsdf2 (upc_shared_ptr_t p, double v)
{
  double *addr = (double *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putstf2 (upc_shared_ptr_t p, long double v)
{
  long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putsxf2 (upc_shared_ptr_t p, long double v)
{
  long double *addr = (long double *) __upc_access_sptr_to_addr (p);
  GUPCR_WRITE_FENCE ();
  *addr = v;
  GUPCR_FENCE ();
}

//inline
void
__putsblk3 (upc_shared_ptr_t dest, void *src, size_t len)
{
  GUPCR_WRITE_FENCE ();
  __putblk3 (dest, src, len);
  GUPCR_FENCE ();
}

//inline
void
__copysblk3 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t len)
{
  GUPCR_WRITE_FENCE ();
  __copyblk3 (dest, src, len);
  GUPCR_FENCE ();
}
//end lib_inline_access
