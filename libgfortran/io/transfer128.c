/* Copyright (C) 2010-2024 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
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

/* Note: This file needs to be a separate translation unit (.o file)
   to make sure that for static linkage, the libquad dependence only
   occurs if needed.  */

#include "io.h"


#if defined(GFC_REAL_16_IS_FLOAT128) || defined(HAVE_GFC_REAL_17)

/* The prototypes for the called procedures in transfer.c.  */

extern void transfer_real (st_parameter_dt *, void *, int);
export_proto(transfer_real);

extern void transfer_real_write (st_parameter_dt *, void *, int);
export_proto(transfer_real_write);

extern void transfer_complex (st_parameter_dt *, void *, int);
export_proto(transfer_complex);

extern void transfer_complex_write (st_parameter_dt *, void *, int);
export_proto(transfer_complex_write);


/* The prototypes for the procedures in this file.  */

extern void transfer_real128 (st_parameter_dt *, void *, int);
export_proto(transfer_real128);

extern void transfer_real128_write (st_parameter_dt *, void *, int);
export_proto(transfer_real128_write);

extern void transfer_complex128 (st_parameter_dt *, void *, int);
export_proto(transfer_complex128);

extern void transfer_complex128_write (st_parameter_dt *, void *, int);
export_proto(transfer_complex128_write);


/* Make sure that libquadmath is pulled in. The functions strtoflt128
   and quadmath_snprintf are weakly referrenced in convert_real and
   write_float; the pointer assignment with USED attribute make sure
   that there is a non-weakref dependence if the quadmath functions
   are used. That avoids segfault when libquadmath is statically linked.  */
# if (defined(HAVE_GFC_REAL_17) && !defined(POWER_IEEE128) \
      && !defined(GFC_REAL_17_USE_IEC_60559)) \
     || (!defined(HAVE_GFC_REAL_17) && !defined(GFC_REAL_16_USE_IEC_60559))
static void __attribute__((used)) *tmp1 = strtoflt128;
static void __attribute__((used)) *tmp2 = quadmath_snprintf;
# endif

void
transfer_real128 (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_real (dtp, p, kind);
}


void
transfer_real128_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_real (dtp, p, kind);
}


void
transfer_complex128 (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_complex (dtp, p, kind);
}


void
transfer_complex128_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_complex_write (dtp, p, kind);
}
#endif
