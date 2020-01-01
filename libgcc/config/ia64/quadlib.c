/* Subroutines for long double support.
   Copyright (C) 2000-2020 Free Software Foundation, Inc.

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

extern int _U_Qfcmp (long double a, long double b, int);

int _U_Qfeq (long double, long double);
int _U_Qfne (long double, long double);
int _U_Qfgt (long double, long double);
int _U_Qfge (long double, long double);
int _U_Qflt (long double, long double);
int _U_Qfle (long double, long double);
int _U_Qfcomp (long double, long double);

int
_U_Qfeq (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 4) != 0);
}

int
_U_Qfne (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 4) == 0);
}
	
int
_U_Qfgt (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 17) != 0);
}

int
_U_Qfge (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 21) != 0);
}

int
_U_Qflt (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 9) != 0);
}

int
_U_Qfle (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 13) != 0);
}

int
_U_Qfcomp (long double a, long double b)
{
  if (_U_Qfcmp (a, b, 4) == 0)
    return 0;

  return (_U_Qfcmp (a, b, 22) != 0 ? 1 : -1);
}
