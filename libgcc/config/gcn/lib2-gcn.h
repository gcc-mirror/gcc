/* Integer arithmetic support for gcn.

   Copyright (C) 2012-2025 Free Software Foundation, Inc.
   Contributed by Altera and Mentor Graphics, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef LIB2_GCN_H
#define LIB2_GCN_H

/* Types.  */

typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
typedef int word_type __attribute__ ((mode (__word__)));

typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef float v16sf __attribute__ ((vector_size (64)));
typedef float v32sf __attribute__ ((vector_size (128)));
typedef float v64sf __attribute__ ((vector_size (256)));

typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));
typedef double v8df __attribute__ ((vector_size (64)));
typedef double v16df __attribute__ ((vector_size (128)));
typedef double v32df __attribute__ ((vector_size (256)));
typedef double v64df __attribute__ ((vector_size (512)));

typedef signed char v2qi __attribute__ ((vector_size (2)));
typedef signed char v4qi __attribute__ ((vector_size (4)));
typedef signed char v8qi __attribute__ ((vector_size (8)));
typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef signed char v32qi __attribute__ ((vector_size (32)));
typedef signed char v64qi __attribute__ ((vector_size (64)));

typedef unsigned char v2uqi __attribute__ ((vector_size (2)));
typedef unsigned char v4uqi __attribute__ ((vector_size (4)));
typedef unsigned char v8uqi __attribute__ ((vector_size (8)));
typedef unsigned char v16uqi __attribute__ ((vector_size (16)));
typedef unsigned char v32uqi __attribute__ ((vector_size (32)));
typedef unsigned char v64uqi __attribute__ ((vector_size (64)));

typedef short v2hi __attribute__ ((vector_size (4)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef short v64hi __attribute__ ((vector_size (128)));

typedef unsigned short v2uhi __attribute__ ((vector_size (4)));
typedef unsigned short v4uhi __attribute__ ((vector_size (8)));
typedef unsigned short v8uhi __attribute__ ((vector_size (16)));
typedef unsigned short v16uhi __attribute__ ((vector_size (32)));
typedef unsigned short v32uhi __attribute__ ((vector_size (64)));
typedef unsigned short v64uhi __attribute__ ((vector_size (128)));

typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef int v32si __attribute__ ((vector_size (128)));
typedef int v64si __attribute__ ((vector_size (256)));

typedef unsigned int v2usi __attribute__ ((vector_size (8)));
typedef unsigned int v4usi __attribute__ ((vector_size (16)));
typedef unsigned int v8usi __attribute__ ((vector_size (32)));
typedef unsigned int v16usi __attribute__ ((vector_size (64)));
typedef unsigned int v32usi __attribute__ ((vector_size (128)));
typedef unsigned int v64usi __attribute__ ((vector_size (256)));

typedef long v2di __attribute__ ((vector_size (16)));
typedef long v4di __attribute__ ((vector_size (32)));
typedef long v8di __attribute__ ((vector_size (64)));
typedef long v16di __attribute__ ((vector_size (128)));
typedef long v32di __attribute__ ((vector_size (256)));
typedef long v64di __attribute__ ((vector_size (512)));

typedef unsigned long v2udi __attribute__ ((vector_size (16)));
typedef unsigned long v4udi __attribute__ ((vector_size (32)));
typedef unsigned long v8udi __attribute__ ((vector_size (64)));
typedef unsigned long v16udi __attribute__ ((vector_size (128)));
typedef unsigned long v32udi __attribute__ ((vector_size (256)));
typedef unsigned long v64udi __attribute__ ((vector_size (512)));

typedef UTItype v2uti __attribute__ ((vector_size (32)));
typedef UTItype v4uti __attribute__ ((vector_size (64)));
typedef UTItype v8uti __attribute__ ((vector_size (128)));
typedef UTItype v16uti __attribute__ ((vector_size (256)));
typedef UTItype v32uti __attribute__ ((vector_size (512)));
typedef UTItype v64uti __attribute__ ((vector_size (1024)));

/* Exported functions.  */
extern DItype __divdi3 (DItype, DItype);
extern DItype __moddi3 (DItype, DItype);
extern UTItype __divmoddi4 (DItype, DItype);
extern UDItype __udivdi3 (UDItype, UDItype);
extern UDItype __umoddi3 (UDItype, UDItype);
extern UTItype __udivmoddi4 (UDItype, UDItype);
extern SItype __divsi3 (SItype, SItype);
extern SItype __modsi3 (SItype, SItype);
extern UDItype __divmodsi4 (SItype, SItype);
extern USItype __udivsi3 (USItype, USItype);
extern USItype __umodsi3 (USItype, USItype);
extern UDItype __udivmodsi4 (USItype, USItype);
extern SItype __mulsi3 (SItype, SItype);

#define VECTOR_PROTOTYPES(SIZE) \
  extern v##SIZE##qi  __divv##SIZE##qi3     (v##SIZE##qi,  v##SIZE##qi);  \
  extern v##SIZE##qi  __modv##SIZE##qi3     (v##SIZE##qi,  v##SIZE##qi);  \
  extern v##SIZE##udi __divmodv##SIZE##qi4  (v##SIZE##qi,  v##SIZE##qi);  \
  extern v##SIZE##uqi __udivv##SIZE##qi3    (v##SIZE##uqi, v##SIZE##uqi); \
  extern v##SIZE##uqi __umodv##SIZE##qi3    (v##SIZE##uqi, v##SIZE##uqi); \
  extern v##SIZE##udi __udivmodv##SIZE##qi4 (v##SIZE##uqi, v##SIZE##uqi);  \
  extern v##SIZE##hi  __divv##SIZE##hi3     (v##SIZE##hi,  v##SIZE##hi);  \
  extern v##SIZE##hi  __modv##SIZE##hi3     (v##SIZE##hi,  v##SIZE##hi);  \
  extern v##SIZE##udi __divmodv##SIZE##hi4  (v##SIZE##hi,  v##SIZE##hi);  \
  extern v##SIZE##uhi __udivv##SIZE##hi3    (v##SIZE##uhi, v##SIZE##uhi); \
  extern v##SIZE##uhi __umodv##SIZE##hi3    (v##SIZE##uhi, v##SIZE##uhi); \
  extern v##SIZE##udi __udivmodv##SIZE##hi4 (v##SIZE##uhi, v##SIZE##uhi); \
  extern v##SIZE##si  __divv##SIZE##si3     (v##SIZE##si,  v##SIZE##si);  \
  extern v##SIZE##si  __modv##SIZE##si3     (v##SIZE##si,  v##SIZE##si);  \
  extern v##SIZE##udi __divmodv##SIZE##si4  (v##SIZE##si,  v##SIZE##si);  \
  extern v##SIZE##usi __udivv##SIZE##si3    (v##SIZE##usi, v##SIZE##usi); \
  extern v##SIZE##usi __umodv##SIZE##si3    (v##SIZE##usi, v##SIZE##usi); \
  extern v##SIZE##udi __udivmodv##SIZE##si4 (v##SIZE##usi, v##SIZE##usi); \
  extern v##SIZE##di  __divv##SIZE##di3     (v##SIZE##di,  v##SIZE##di);  \
  extern v##SIZE##di  __modv##SIZE##di3     (v##SIZE##di,  v##SIZE##di);  \
  extern v##SIZE##uti __divmodv##SIZE##di4  (v##SIZE##di,  v##SIZE##di);  \
  extern v##SIZE##udi __udivv##SIZE##di3    (v##SIZE##udi, v##SIZE##udi); \
  extern v##SIZE##udi __umodv##SIZE##di3    (v##SIZE##udi, v##SIZE##udi); \
  extern v##SIZE##uti __udivmodv##SIZE##di4 (v##SIZE##udi, v##SIZE##udi);
VECTOR_PROTOTYPES (2)
VECTOR_PROTOTYPES (4)
VECTOR_PROTOTYPES (8)
VECTOR_PROTOTYPES (16)
VECTOR_PROTOTYPES (32)
VECTOR_PROTOTYPES (64)
#undef VECTOR_PROTOTYPES

#endif /* LIB2_GCN_H */
