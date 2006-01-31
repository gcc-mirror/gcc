/*
 * IBM Accurate Mathematical Library
 * Written by International Business Machines Corp.
 * Copyright (C) 2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/********************************************************************/
/* Ultimate math functions. Each function computes the exact        */
/* theoretical value of its argument rounded to nearest or even.    */
/*                                                                  */
/* Assumption: Machine arithmetic operations are performed in       */
/* round nearest mode of IEEE 754 standard.                         */
/********************************************************************/

#ifndef UMATH_LIB
#define UMATH_LIB
/********************************************************************/
/* Function changes the precision mode to IEEE 754 double precision */
/* and the rounding mode to nearest or even.                        */
/* It returns the original status of these modes.                   */
/* See further explanations of usage in DPChange.h                  */
/********************************************************************/
unsigned short Init_Lib(void);

/********************************************************************/
/* Function that changes the precision and rounding modes to the    */
/* specified by the argument received. See further explanations in  */
/* DPChange.h                                                       */
/********************************************************************/
void Exit_Lib(unsigned short);


/* The  asin() function calculates the arc sine of its argument.    */
/* The  function returns the arc sine in radians                    */
/* (between -PI/2 and PI/2).                                        */
/* If the argument is greater than 1 or less than -1 it returns     */
/* a NaN.                                                           */
double uasin(double );


/* The  acos() function calculates the arc cosine of its argument.  */
/* The  function returns the arc cosine in radians                  */
/* (between -PI/2 and PI/2).                                        */
/* If the argument is greater than 1 or less than -1 it returns     */
/* a NaN.                                                           */
double uacos(double );

/* The  atan() function calculates the arctanget of its argument.   */
/* The  function returns the arc tangent in radians                 */
/* (between -PI/2 and PI/2).                                        */
double uatan(double );


/* The uatan2() function calculates the arc tangent of the two arguments x   */
/* and y (x is the right argument and y is the left one).The signs of both   */
/* arguments are used to determine the quadrant of the result.               */
/* The function returns the result in radians, which is between -PI and PI   */
double uatan2(double ,double );

/* Compute log(x). The base of log is e (natural logarithm)         */
double ulog(double );

/* Compute e raised to the power of argument x.                     */
double uexp(double );

/* Compute sin(x). The argument x is assumed to be given in radians.*/
double usin(double );

/* Compute cos(x). The argument x is assumed to be given in radians.*/
double ucos(double );

/* Compute tan(x). The argument x is assumed to be given in radians.*/
double utan(double );

/* Compute the square root of non-negative argument x.              */
/* If x is negative the returned value is NaN.                      */
double usqrt(double );

/* Compute x raised to the power of y, where x is the left argument */
/* and y is the right argument. The function returns a NaN if x<0.  */
/* If x equals zero it returns -inf                                 */
double upow(double , double );

/* Computing x mod y, where x is the left argument and y is the     */
/* right one.                                                       */
double uremainder(double , double );


#endif
