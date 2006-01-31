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

/***********************************************************************/
/*MODULE_NAME: dla.h                                                   */
/*                                                                     */
/* This file holds C language macros for 'Double Length Floating Point */
/* Arithmetic'. The macros are based on the paper:                     */
/* T.J.Dekker, "A floating-point Technique for extending the           */
/* Available Precision", Number. Math. 18, 224-242 (1971).              */
/* A Double-Length number is defined by a pair (r,s), of IEEE double    */
/* precision floating point numbers that satisfy,                      */
/*                                                                     */
/*              abs(s) <= abs(r+s)*2**(-53)/(1+2**(-53)).              */
/*                                                                     */
/* The computer arithmetic assumed is IEEE double precision in         */
/* round to nearest mode. All variables in the macros must be of type  */
/* IEEE double.                                                        */
/***********************************************************************/

/* CN = 1+2**27 = '41a0000002000000' IEEE double format */
#define  CN   134217729.0


/* Exact addition of two single-length floating point numbers, Dekker. */
/* The macro produces a double-length number (z,zz) that satisfies     */
/* z+zz = x+y exactly.                                                 */

#define  EADD(x,y,z,zz)  \
           z=(x)+(y);  zz=(ABS(x)>ABS(y)) ? (((x)-(z))+(y)) : (((y)-(z))+(x));


/* Exact subtraction of two single-length floating point numbers, Dekker. */
/* The macro produces a double-length number (z,zz) that satisfies        */
/* z+zz = x-y exactly.                                                    */

#define  ESUB(x,y,z,zz)  \
           z=(x)-(y);  zz=(ABS(x)>ABS(y)) ? (((x)-(z))-(y)) : ((x)-((y)+(z)));


/* Exact multiplication of two single-length floating point numbers,   */
/* Veltkamp. The macro produces a double-length number (z,zz) that     */
/* satisfies z+zz = x*y exactly. p,hx,tx,hy,ty are temporary           */
/* storage variables of type double.                                   */

#define  EMULV(x,y,z,zz,p,hx,tx,hy,ty)          \
           p=CN*(x);  hx=((x)-p)+p;  tx=(x)-hx; \
           p=CN*(y);  hy=((y)-p)+p;  ty=(y)-hy; \
           z=(x)*(y); zz=(((hx*hy-z)+hx*ty)+tx*hy)+tx*ty;


/* Exact multiplication of two single-length floating point numbers, Dekker. */
/* The macro produces a nearly double-length number (z,zz) (see Dekker)      */
/* that satisfies z+zz = x*y exactly. p,hx,tx,hy,ty,q are temporary          */
/* storage variables of type double.                                         */

#define  MUL12(x,y,z,zz,p,hx,tx,hy,ty,q)        \
           p=CN*(x);  hx=((x)-p)+p;  tx=(x)-hx; \
           p=CN*(y);  hy=((y)-p)+p;  ty=(y)-hy; \
           p=hx*hy;  q=hx*ty+tx*hy; z=p+q;  zz=((p-z)+q)+tx*ty;


/* Double-length addition, Dekker. The macro produces a double-length   */
/* number (z,zz) which satisfies approximately   z+zz = x+xx + y+yy.    */
/* An error bound: (abs(x+xx)+abs(y+yy))*4.94e-32. (x,xx), (y,yy)       */
/* are assumed to be double-length numbers. r,s are temporary           */
/* storage variables of type double.                                    */

#define  ADD2(x,xx,y,yy,z,zz,r,s)                    \
           r=(x)+(y);  s=(ABS(x)>ABS(y)) ?           \
                       (((((x)-r)+(y))+(yy))+(xx)) : \
                       (((((y)-r)+(x))+(xx))+(yy));  \
           z=r+s;  zz=(r-z)+s;


/* Double-length subtraction, Dekker. The macro produces a double-length  */
/* number (z,zz) which satisfies approximately   z+zz = x+xx - (y+yy).    */
/* An error bound: (abs(x+xx)+abs(y+yy))*4.94e-32. (x,xx), (y,yy)         */
/* are assumed to be double-length numbers. r,s are temporary             */
/* storage variables of type double.                                      */

#define  SUB2(x,xx,y,yy,z,zz,r,s)                    \
           r=(x)-(y);  s=(ABS(x)>ABS(y)) ?           \
                       (((((x)-r)-(y))-(yy))+(xx)) : \
                       ((((x)-((y)+r))+(xx))-(yy));  \
           z=r+s;  zz=(r-z)+s;


/* Double-length multiplication, Dekker. The macro produces a double-length  */
/* number (z,zz) which satisfies approximately   z+zz = (x+xx)*(y+yy).       */
/* An error bound: abs((x+xx)*(y+yy))*1.24e-31. (x,xx), (y,yy)               */
/* are assumed to be double-length numbers. p,hx,tx,hy,ty,q,c,cc are         */
/* temporary storage variables of type double.                               */

#define  MUL2(x,xx,y,yy,z,zz,p,hx,tx,hy,ty,q,c,cc)  \
           MUL12(x,y,c,cc,p,hx,tx,hy,ty,q)          \
           cc=((x)*(yy)+(xx)*(y))+cc;   z=c+cc;   zz=(c-z)+cc;


/* Double-length division, Dekker. The macro produces a double-length        */
/* number (z,zz) which satisfies approximately   z+zz = (x+xx)/(y+yy).       */
/* An error bound: abs((x+xx)/(y+yy))*1.50e-31. (x,xx), (y,yy)               */
/* are assumed to be double-length numbers. p,hx,tx,hy,ty,q,c,cc,u,uu        */
/* are temporary storage variables of type double.                           */

#define  DIV2(x,xx,y,yy,z,zz,p,hx,tx,hy,ty,q,c,cc,u,uu)  \
           c=(x)/(y);   MUL12(c,y,u,uu,p,hx,tx,hy,ty,q)  \
           cc=(((((x)-u)-uu)+(xx))-c*(yy))/(y);   z=c+cc;   zz=(c-z)+cc;


/* Double-length addition, slower but more accurate than ADD2.               */
/* The macro produces a double-length                                        */
/* number (z,zz) which satisfies approximately   z+zz = (x+xx)+(y+yy).       */
/* An error bound: abs(x+xx + y+yy)*1.50e-31. (x,xx), (y,yy)                 */
/* are assumed to be double-length numbers. r,rr,s,ss,u,uu,w                 */
/* are temporary storage variables of type double.                           */

#define  ADD2A(x,xx,y,yy,z,zz,r,rr,s,ss,u,uu,w)                        \
           r=(x)+(y);                                                  \
           if (ABS(x)>ABS(y)) { rr=((x)-r)+(y);  s=(rr+(yy))+(xx); }   \
           else               { rr=((y)-r)+(x);  s=(rr+(xx))+(yy); }   \
           if (rr!=0.0) {                                              \
             z=r+s;  zz=(r-z)+s; }                                     \
           else {                                                      \
             ss=(ABS(xx)>ABS(yy)) ? (((xx)-s)+(yy)) : (((yy)-s)+(xx)); \
             u=r+s;                                                    \
             uu=(ABS(r)>ABS(s))   ? ((r-u)+s)   : ((s-u)+r)  ;         \
             w=uu+ss;  z=u+w;                                          \
             zz=(ABS(u)>ABS(w))   ? ((u-z)+w)   : ((w-z)+u)  ; }


/* Double-length subtraction, slower but more accurate than SUB2.            */
/* The macro produces a double-length                                        */
/* number (z,zz) which satisfies approximately   z+zz = (x+xx)-(y+yy).       */
/* An error bound: abs(x+xx - (y+yy))*1.50e-31. (x,xx), (y,yy)               */
/* are assumed to be double-length numbers. r,rr,s,ss,u,uu,w                 */
/* are temporary storage variables of type double.                           */

#define  SUB2A(x,xx,y,yy,z,zz,r,rr,s,ss,u,uu,w)                        \
           r=(x)-(y);                                                  \
           if (ABS(x)>ABS(y)) { rr=((x)-r)-(y);  s=(rr-(yy))+(xx); }   \
           else               { rr=(x)-((y)+r);  s=(rr+(xx))-(yy); }   \
           if (rr!=0.0) {                                              \
             z=r+s;  zz=(r-z)+s; }                                     \
           else {                                                      \
             ss=(ABS(xx)>ABS(yy)) ? (((xx)-s)-(yy)) : ((xx)-((yy)+s)); \
             u=r+s;                                                    \
             uu=(ABS(r)>ABS(s))   ? ((r-u)+s)   : ((s-u)+r)  ;         \
             w=uu+ss;  z=u+w;                                          \
             zz=(ABS(u)>ABS(w))   ? ((u-z)+w)   : ((w-z)+u)  ; }







