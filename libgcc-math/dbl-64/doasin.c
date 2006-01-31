/*
 * IBM Accurate Mathematical Library
 * written by International Business Machines Corp.
 * Copyright (C) 2001 Free Software Foundation
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
/**********************************************************************/
/* MODULE_NAME: doasin.c                                              */
/*                                                                    */
/* FUNCTION: doasin                                                   */
/*                                                                    */
/* FILES NEEDED:endian.h mydefs.h dla.h doasin.h                      */
/*              mpa.c                                                 */
/*                                                                    */
/* Compute arcsin(x,dx,v) of double-length number (x+dx) the result   */
/* stored in v where v= v[0]+v[1] =arcsin(x+dx)                       */
/**********************************************************************/

#include "endian.h"
#include "mydefs.h"
#include "dla.h"
#include "math_private.h"

/********************************************************************/
/* Compute arcsin(x,dx,v) of double-length number (x+dx) the result */
/* stored in v where v= v[0]+v[1] =arcsin(x+dx)                     */
/********************************************************************/
void __doasin(double x, double dx, double v[]) {

#include "doasin.h"

  static const double
    d5 =  0.22372159090911789889975459505194491E-01,
    d6 =  0.17352764422456822913014975683014622E-01,
    d7 =  0.13964843843786693521653681033981614E-01,
    d8 =  0.11551791438485242609036067259086589E-01,
    d9 =  0.97622386568166960207425666787248914E-02,
    d10 = 0.83638737193775788576092749009744976E-02,
    d11 = 0.79470250400727425881446981833568758E-02;

  double xx,p,pp,u,uu,r,s;
  double hx,tx,hy,ty,tp,tq,tc,tcc;


/* Taylor series for arcsin for Double-Length numbers         */
  xx = x*x+2.0*x*dx;
  p = ((((((d11*xx+d10)*xx+d9)*xx+d8)*xx+d7)*xx+d6)*xx+d5)*xx;
  pp = 0;

  MUL2(x,dx,x,dx,u,uu,tp,hx,tx,hy,ty,tq,tc,tcc);
  ADD2(p,pp,c4.x,cc4.x,p,pp,r,s);
  MUL2(p,pp,u,uu,p,pp,tp,hx,tx,hy,ty,tq,tc,tcc);
  ADD2(p,pp,c3.x,cc3.x,p,pp,r,s);
  MUL2(p,pp,u,uu,p,pp,tp,hx,tx,hy,ty,tq,tc,tcc);
  ADD2(p,pp,c2.x,cc2.x,p,pp,r,s);
  MUL2(p,pp,u,uu,p,pp,tp,hx,tx,hy,ty,tq,tc,tcc);
  ADD2(p,pp,c1.x,cc1.x,p,pp,r,s);
  MUL2(p,pp,u,uu,p,pp,tp,hx,tx,hy,ty,tq,tc,tcc);
  MUL2(p,pp,x,dx,p,pp,tp,hx,tx,hy,ty,tq,tc,tcc);
  ADD2(p,pp,x,dx,p,pp,r,s);
  v[0]=p;
  v[1]=pp; /* arcsin(x+dx)=v[0]+v[1] */
}
