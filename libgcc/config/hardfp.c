/* Dummy floating-point routines for hard-float code.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define sf float
#define df double

#if defined (OP_add3)
TYPE FUNC (TYPE x, TYPE y) { return x + y; }
#elif defined (OP_sub3)
TYPE FUNC (TYPE x, TYPE y) { return x - y; }
#elif defined (OP_neg2)
TYPE FUNC (TYPE x) { return -x; }
#elif defined (OP_mul3)
TYPE FUNC (TYPE x, TYPE y) { return x * y; }
#elif defined (OP_div3)
TYPE FUNC (TYPE x, TYPE y) { return x / y; }
#elif defined (OP_eq2) || defined (OP_ne2)
int FUNC (TYPE x, TYPE y) { return x == y ? 0 : 1; }
#elif defined (OP_ge2)
int FUNC (TYPE x, TYPE y) { return x >= y ? 0 : -1; }
#elif defined (OP_gt2)
int FUNC (TYPE x, TYPE y) { return x > y ? 1 : 0; }
#elif defined (OP_le2)
int FUNC (TYPE x, TYPE y) { return x <= y ? 0 : 1; }
#elif defined (OP_lt2)
int FUNC (TYPE x, TYPE y) { return x < y ? -1 : 0; }
#elif defined (OP_unord2)
int FUNC (TYPE x, TYPE y) { return __builtin_isunordered (x, y); }
#elif defined (OP_fixsi)
int FUNC (TYPE x) { return (int) x; }
#elif defined (OP_floatsi)
TYPE FUNC (int x) { return (TYPE) x; }
#elif defined (OP_floatunsi)
TYPE FUNC (unsigned int x) { return (TYPE) x; }
#elif defined (OP_extendsf2)
TYPE FUNC (float x) { return (TYPE) x; }
#elif defined (OP_truncdf2)
TYPE FUNC (double x) { return (TYPE) x; }
#else
#error Unknown operation
#endif
