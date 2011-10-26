/* Copyright (c) 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
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
#include "upc_lib.h"
#include "gasp_upc.h"
#include "upc_pupc.h"

/* relaxed accesses (profiled) */

u_intQI_t
__getgqi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intQI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getqi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intHI_t
__getghi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intHI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __gethi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intSI_t
__getgsi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intSI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intDI_t
__getgdi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intDI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getdi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

#if GUPCR_TARGET64
u_intTI_t
__getgti3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intTI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getti2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}
#endif

float
__getgsf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  float val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

double
__getgdf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getdf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

long double
__getgtf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  long double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __gettf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

long double
__getgxf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  long double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getxf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

void
__getgblk5 (void *dest, upc_shared_ptr_t src, size_t n, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, dest, &src, n);
  __getblk3 (dest, src, n);
  p_end (GASP_UPC_GET, 1, dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgqi4 (upc_shared_ptr_t p, u_intQI_t v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putqi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putghi4 (upc_shared_ptr_t p, u_intHI_t v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __puthi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgsi4 (upc_shared_ptr_t p, u_intSI_t v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgdi4 (upc_shared_ptr_t p, u_intDI_t v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putdi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

#if GUPCR_TARGET64
void
__putgti4 (upc_shared_ptr_t p, u_intTI_t v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putti2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}
#endif

void
__putgsf4 (upc_shared_ptr_t p, float v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgdf4 (upc_shared_ptr_t p, double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putdf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgtf4 (upc_shared_ptr_t p, long double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __puttf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgxf4 (upc_shared_ptr_t p, long double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putxf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putgblk5 (upc_shared_ptr_t dest, void *src, size_t n, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &dest, src, n);
  __putblk3 (dest, src, n);
  p_end (GASP_UPC_PUT, 1, &dest, src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
__copygblk5 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n,
	     const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMCPY, &dest, &src, n);
  __copyblk3 (dest, src, n);
  p_end (GASP_UPC_MEMCPY, &dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

/* strict accesses (profiled) */

u_intQI_t
__getsgqi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intQI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsqi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intHI_t
__getsghi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intHI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getshi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intSI_t
__getsgsi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intSI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getssi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

u_intDI_t
__getsgdi3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intDI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsdi2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

#if GUPCR_TARGET64
u_intTI_t
__getsgti3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  u_intTI_t val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsti2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}
#endif

float
__getsgsf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  float val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getssf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

double
__getsgdf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsdf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

long double
__getsgtf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  long double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getstf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

long double
__getsgxf3 (upc_shared_ptr_t p, const char *filename, int linenum)
{
  long double val;
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  val = __getsxf2 (p);
  p_end (GASP_UPC_GET, 1, &val, &p, sizeof (val));
  GUPCR_CLEAR_ERR_LOC();
  return val;
}

void
__getsgblk5 (void *dest, upc_shared_ptr_t src, size_t n, const char *filename,
	     int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_GET, 1, dest, &src, n);
  __getblk3 (dest, src, n);
  p_end (GASP_UPC_GET, 1, dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgqi4 (upc_shared_ptr_t p, u_intQI_t v, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsqi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsghi4 (upc_shared_ptr_t p, u_intHI_t v, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putshi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgsi4 (upc_shared_ptr_t p, u_intSI_t v, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putssi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgdi4 (upc_shared_ptr_t p, u_intDI_t v, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsdi2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

#if GUPCR_TARGET64
void
__putsgti4 (upc_shared_ptr_t p, u_intTI_t v, const char *filename,
	    int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsti2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}
#endif

void
__putsgsf4 (upc_shared_ptr_t p, float v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putssf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgdf4 (upc_shared_ptr_t p, double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsdf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgtf4 (upc_shared_ptr_t p, long double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putstf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgxf4 (upc_shared_ptr_t p, long double v, const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  __putsxf2 (p, v);
  p_end (GASP_UPC_PUT, 1, &p, &v, sizeof (v));
  GUPCR_CLEAR_ERR_LOC();
}

void
__putsgblk5 (upc_shared_ptr_t dest, void *src, size_t n, const char *filename,
	     int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_PUT, 0, &dest, src, n);
  __putsblk3 (dest, src, n);
  p_end (GASP_UPC_PUT, 0, &dest, src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
__copysgblk5 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n,
	      const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMCPY, &dest, &src, n);
  __copysblk3 (dest, src, n);
  p_end (GASP_UPC_MEMCPY, &dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
upc_memcpyg (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n,
	     const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMCPY, &dest, &src, n);
  upc_memcpy (dest, src, n);
  p_end (GASP_UPC_MEMCPY, &dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
upc_memgetg (void *dest, upc_shared_ptr_t src, size_t n, const char *filename,
	     int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMGET, &dest, &src, n);
  upc_memget (dest, src, n);
  p_end (GASP_UPC_MEMGET, &dest, &src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
upc_memputg (upc_shared_ptr_t dest, const void *src, size_t n,
	     const char *filename, int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMPUT, &dest, src, n);
  upc_memput (dest, src, n);
  p_end (GASP_UPC_MEMPUT, &dest, src, n);
  GUPCR_CLEAR_ERR_LOC();
}

void
upc_memsetg (upc_shared_ptr_t dest, int c, size_t n, const char *filename,
	     int linenum)
{
  GUPCR_SET_ERR_LOC();
  p_start (GASP_UPC_MEMSET, &dest, c, n);
  upc_memset (dest, c, n);
  p_end (GASP_UPC_MEMSET, &dest, c, n);
  GUPCR_CLEAR_ERR_LOC();
}
