/* Copyright (C) 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#define decimal128FromString __dpd128FromString
#define decimal128ToString __dpd128ToString
#define decimal128ToEngString __dpd128ToEngString
#define decimal128FromNumber __dpd128FromNumber
#define decimal128ToNumber __dpd128ToNumber

#include "dpd/decimal128.c"

#undef decimal128FromString
#undef decimal128ToString
#undef decimal128ToEngString
#undef decimal128FromNumber
#undef decimal128ToNumber

#include "bid-dpd.h"

#ifdef IN_LIBGCC2
#define decimal128FromString __decimal128FromString
#define decimal128ToString __decimal128ToString
#define decimal128ToEngString __decimal128ToEngString
#define decimal128FromNumber __decimal128FromNumber
#define decimal128ToNumber __decimal128ToNumber
#endif

decimal128 *decimal128FromString (decimal128 *, const char *, decContext *);
char *decimal128ToString (const decimal128 *, char *);
char *decimal128ToEngString (const decimal128 *, char *);
decimal128 *decimal128FromNumber (decimal128 *, const decNumber *, decContext *);
decNumber *decimal128ToNumber (const decimal128 *, decNumber *);

void __host_to_ieee_128 (_Decimal128 in, decimal128 *out);
void __ieee_to_host_128 (decimal128 in, _Decimal128 *out);

decimal128 *
decimal128FromNumber (decimal128 *d128, const decNumber *dn,
		      decContext *set)
{
  /* decimal128 and _Decimal128 are different types.  */
  union
    {
      _Decimal128 _Dec;
      decimal128 dec;
    } u;

  __dpd128FromNumber (d128, dn, set);

  /* __dpd128FromNumber returns in big endian. But _dpd_to_bid128 takes
     host endian. */
  __ieee_to_host_128 (*d128, &u._Dec);

  /* Convert DPD to BID.  */
  _dpd_to_bid128 (&u._Dec, &u._Dec);

  /* dfp.c is in bid endian. */
  __host_to_ieee_128 (u._Dec, &u.dec);

  /* d128 is returned as a pointer to _Decimal128 here.  */
  *d128 = u.dec;

  return d128;
}

decNumber *
decimal128ToNumber (const decimal128 *bid128, decNumber *dn)
{
  /* decimal128 and _Decimal128 are different types.  */
  union
    {
      _Decimal128 _Dec;
      decimal128 dec;
    } u;

  /* bid128 is a pointer to _Decimal128 in bid endian. But _bid_to_dpd128
     takes host endian.  */
  __ieee_to_host_128 (*bid128, &u._Dec);

  /* Convert BID to DPD.  */
  _bid_to_dpd128 (&u._Dec, &u._Dec);

  /* __dpd128ToNumber is in bid endian.  */
  __host_to_ieee_128 (u._Dec, &u.dec);

  return __dpd128ToNumber (&u.dec, dn);
}

char *
decimal128ToString (const decimal128 *d128, char *string)
{
  decNumber dn;			/* work */
  decimal128ToNumber (d128, &dn);
  decNumberToString (&dn, string);
  return string;
}

char *
decimal128ToEngString (const decimal128 *d128, char *string)
{
  decNumber dn;			/* work */
  decimal128ToNumber (d128, &dn);
  decNumberToEngString (&dn, string);
  return string;
}

decimal128 *
decimal128FromString (decimal128 *result, const char *string,
		      decContext *set)
{
  decContext dc;		/* work */
  decNumber dn;			/* .. */

  decContextDefault (&dc, DEC_INIT_DECIMAL128);	/* no traps, please */
  dc.round = set->round;	/* use supplied rounding */

  decNumberFromString (&dn, string, &dc);	/* will round if needed */
  decimal128FromNumber (result, &dn, &dc);
  if (dc.status != 0)
    {				/* something happened */
      decContextSetStatus (set, dc.status);	/* .. pass it on */
    }
  return result;
}
