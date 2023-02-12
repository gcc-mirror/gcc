/* RX C ABI functions
   Copyright (C) 2009-2023 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* The RX C ABI includes the specification of a set of compiler support
   functions.  Libgcc2 includes some of them, although the names have to
   be changed (see rx-abi.h), and the rest are defined here.

   FIXME: Given that FINE_GRAINED_LIBRARIES is defined we ought to consider
   compiling this file multiple times with one function per iteration being
   compiled.  */

#ifdef __RX_64BIT_DOUBLES__

int _COM_CMPLTd (double a, double b) { return __ltdf2 (a, b) == -1; }
int _COM_CMPGTd (double a, double b) { return __gtdf2 (a, b) == 1; }
int _COM_CMPLEd (double a, double b) { return __ledf2 (a, b) != 1; }
int _COM_CMPGEd (double a, double b) { return __gedf2 (a, b) != -1; }
int _COM_CMPEQd (double a, double b) { return __eqdf2 (a, b) == 0; }
int _COM_CMPNEd (double a, double b) { return __nedf2 (a, b) != 0; }

int _COM_CMPLTf (double, double) __attribute__ ((weak, alias ("_COM_CMPLTd")));
int _COM_CMPGTf (double, double) __attribute__ ((weak, alias ("_COM_CMPGTd")));
int _COM_CMPLEf (double, double) __attribute__ ((weak, alias ("_COM_CMPLEd")));
int _COM_CMPGEf (double, double) __attribute__ ((weak, alias ("_COM_CMPGEd")));
int _COM_CMPEQf (double, double) __attribute__ ((weak, alias ("_COM_CMPEQd")));
int _COM_CMPNEf (double, double) __attribute__ ((weak, alias ("_COM_CMPNEd")));

#else /* 32-bit doubles.  */

double _COM_CONVfd (float a) { return a; }
float  _COM_CONVdf (double a) { return a; }

int _COM_CMPLTd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPLTf")));
int _COM_CMPGTd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPGTf")));
int _COM_CMPLEd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPLEf")));
int _COM_CMPGEd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPGEf")));
int _COM_CMPEQd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPEQf")));
int _COM_CMPNEd (double a, double b) __attribute__ ((weak, alias ("_COM_CMPNEf")));

signed long long   _COM_CONVd64s (double a) { return (signed long long) a; }
unsigned long long _COM_CONVd64u (double a)  { return (unsigned long long) a; }

int _COM_CMPLTf (float a, float b) { return __ltsf2 (a, b) == -1; }
int _COM_CMPGTf (float a, float b) { return __gtsf2 (a, b) == 1; }
int _COM_CMPLEf (float a, float b) { return __lesf2 (a, b) != 1; }
int _COM_CMPGEf (float a, float b) { return __gesf2 (a, b) != -1; }
int _COM_CMPEQf (float a, float b) { return __eqsf2 (a, b) == 0; }
int _COM_CMPNEf (float a, float b) { return __nesf2 (a, b) != 0; }

#endif /* 64-bit vs 32-bit doubles.  */

double _COM_CONV64sd (signed long long a)   { return (double) a; }
double _COM_CONV64ud (unsigned long long a) { return (double) a; }

extern int __cmpdi2 (long long, long long);
extern int __ucmpdi2 (long long, long long);

int _COM_CMPLT64s (long long a, long long b) { return __cmpdi2 (a, b)  == 0; }
int _COM_CMPLT64u (long long a, long long b) { return __ucmpdi2 (a, b) == 0; }
int _COM_CMPGT64s (long long a, long long b) { return __cmpdi2 (a, b)  == 2; }
int _COM_CMPGT64u (long long a, long long b) { return __ucmpdi2 (a, b) == 2; }
int _COM_CMPLE64s (long long a, long long b) { return __cmpdi2 (a, b)  != 2; }
int _COM_CMPLE64u (long long a, long long b) { return __ucmpdi2 (a, b) != 2; }
int _COM_CMPGE64s (long long a, long long b) { return __cmpdi2 (a, b)  != 0; }
int _COM_CMPGE64u (long long a, long long b) { return __ucmpdi2 (a, b) != 0; }
int _COM_CMPEQ64  (long long a, long long b) { return __cmpdi2 (a, b)  == 1; }
int _COM_CMPNE64  (long long a, long long b) { return __cmpdi2 (a, b)  != 1; }

