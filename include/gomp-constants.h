/* Communication between GCC and libgomp.

   Copyright (C) 2014-2015 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

#ifndef GOMP_CONSTANTS_H
#define GOMP_CONSTANTS_H 1

/* Memory mapping types.  */

/* One byte.  */
#define GOMP_MAP_LAST			(1 << 8)

#define GOMP_MAP_FLAG_TO		(1 << 0)
#define GOMP_MAP_FLAG_FROM		(1 << 1)
/* Special map kinds, enumerated starting here.  */
#define GOMP_MAP_FLAG_SPECIAL_0		(1 << 2)
#define GOMP_MAP_FLAG_SPECIAL_1		(1 << 3)
#define GOMP_MAP_FLAG_SPECIAL		(GOMP_MAP_FLAG_SPECIAL_1 \
					 | GOMP_MAP_FLAG_SPECIAL_0)
/* Flag to force a specific behavior (or else, trigger a run-time error).  */
#define GOMP_MAP_FLAG_FORCE		(1 << 7)

enum gomp_map_kind
  {
    /* If not already present, allocate.  */
    GOMP_MAP_ALLOC =			0,
    /* ..., and copy to device.  */
    GOMP_MAP_TO =			(GOMP_MAP_ALLOC | GOMP_MAP_FLAG_TO),
    /* ..., and copy from device.  */
    GOMP_MAP_FROM =			(GOMP_MAP_ALLOC | GOMP_MAP_FLAG_FROM),
    /* ..., and copy to and from device.  */
    GOMP_MAP_TOFROM =			(GOMP_MAP_TO | GOMP_MAP_FROM),
    /* The following kind is an internal only map kind, used for pointer based
       array sections.  OMP_CLAUSE_SIZE for these is not the pointer size,
       which is implicitly POINTER_SIZE_UNITS, but the bias.  */
    GOMP_MAP_POINTER =			(GOMP_MAP_FLAG_SPECIAL_0 | 0),
    /* Also internal, behaves like GOMP_MAP_TO, but additionally any
       GOMP_MAP_POINTER records consecutive after it which have addresses
       falling into that range will not be ignored if GOMP_MAP_TO_PSET wasn't
       mapped already.  */
    GOMP_MAP_TO_PSET =			(GOMP_MAP_FLAG_SPECIAL_0 | 1),
    /* Must already be present.  */
    GOMP_MAP_FORCE_PRESENT =		(GOMP_MAP_FLAG_SPECIAL_0 | 2),
    /* Deallocate a mapping, without copying from device.  */
    GOMP_MAP_FORCE_DEALLOC =		(GOMP_MAP_FLAG_SPECIAL_0 | 3),
    /* Is a device pointer.  OMP_CLAUSE_SIZE for these is unused; is implicitly
       POINTER_SIZE_UNITS.  */
    GOMP_MAP_FORCE_DEVICEPTR =		(GOMP_MAP_FLAG_SPECIAL_1 | 0),
    /* Allocate.  */
    GOMP_MAP_FORCE_ALLOC =		(GOMP_MAP_FLAG_FORCE | GOMP_MAP_ALLOC),
    /* ..., and copy to device.  */
    GOMP_MAP_FORCE_TO =			(GOMP_MAP_FLAG_FORCE | GOMP_MAP_TO),
    /* ..., and copy from device.  */
    GOMP_MAP_FORCE_FROM =		(GOMP_MAP_FLAG_FORCE | GOMP_MAP_FROM),
    /* ..., and copy to and from device.  */
    GOMP_MAP_FORCE_TOFROM =		(GOMP_MAP_FLAG_FORCE | GOMP_MAP_TOFROM)
  };

#define GOMP_MAP_COPY_TO_P(X) \
  (!((X) & GOMP_MAP_FLAG_SPECIAL) \
   && ((X) & GOMP_MAP_FLAG_TO))

#define GOMP_MAP_COPY_FROM_P(X) \
  (!((X) & GOMP_MAP_FLAG_SPECIAL) \
   && ((X) & GOMP_MAP_FLAG_FROM))

#define GOMP_MAP_POINTER_P(X) \
  ((X) == GOMP_MAP_POINTER)


/* Asynchronous behavior.  Keep in sync with
   libgomp/{openacc.h,openacc.f90,openacc_lib.h}:acc_async_t.  */

#define GOMP_ASYNC_NOVAL		-1
#define GOMP_ASYNC_SYNC			-2


/* Device codes.  Keep in sync with
   libgomp/{openacc.h,openacc.f90,openacc_lib.h}:acc_device_t as well as
   libgomp/libgomp_target.h.  */
#define GOMP_DEVICE_NONE		0
#define GOMP_DEVICE_DEFAULT		1
#define GOMP_DEVICE_HOST		2
#define GOMP_DEVICE_HOST_NONSHM		3
#define GOMP_DEVICE_NOT_HOST		4
#define GOMP_DEVICE_NVIDIA_PTX		5
#define GOMP_DEVICE_INTEL_MIC		6

#define GOMP_DEVICE_ICV			-1
#define GOMP_DEVICE_HOST_FALLBACK	-2

#endif
