/* Copyright (C) 2007-2023 Free Software Foundation, Inc.

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

#define decimal32FromString __dpd32FromString
#define decimal32ToString __dpd32ToString
#define decimal32ToEngString __dpd32ToEngString
#define decimal32FromNumber __dpd32FromNumber
#define decimal32ToNumber __dpd32ToNumber

#include "dpd/decimal32.c"

#undef decimal32FromString
#undef decimal32ToString
#undef decimal32ToEngString
#undef decimal32FromNumber
#undef decimal32ToNumber

#include "bid-dpd.h"

#ifdef IN_LIBGCC2
#define decimal32FromString __decimal32FromString
#define decimal32ToString __decimal32ToString
#define decimal32ToEngString __decimal32ToEngString
#define decimal32FromNumber __decimal32FromNumber
#define decimal32ToNumber __decimal32ToNumber
#endif

decimal32 *decimal32FromString (decimal32 *, const char *, decContext *);
char *decimal32ToString (const decimal32 *, char *);
char *decimal32ToEngString (const decimal32 *, char *);
decimal32 *decimal32FromNumber (decimal32 *, const decNumber *, decContext *);
decNumber *decimal32ToNumber (const decimal32 *, decNumber *);

void __host_to_ieee_32 (_Decimal32 in, decimal32 *out);
void __ieee_to_host_32 (decimal32 in, _Decimal32 *out);

decimal32 *
decimal32FromNumber (decimal32 *d32, const decNumber *dn,
		      decContext *set)
{
  /* decimal32 and _Decimal32 are different types.  */
  union
    {
      _Decimal32 _Dec;
      decimal32 dec;
    } u;

  __dpd32FromNumber (d32, dn, set);

  /* __dpd32FromNumber returns in big endian. But _dpd_to_bid32 takes
     host endian. */
  __ieee_to_host_32 (*d32, &u._Dec);

  /* Convert DPD to BID.  */
  _dpd_to_bid32 (&u._Dec, &u._Dec);

  /* dfp.c is in bid endian. */
  __host_to_ieee_32 (u._Dec, &u.dec);

  /* d32 is returned as a pointer to _Decimal32 here.  */
  *d32 = u.dec;

  return d32;
}

decNumber *
decimal32ToNumber (const decimal32 *bid32, decNumber *dn)
{
  /* decimal32 and _Decimal32 are different types.  */
  union
    {
      _Decimal32 _Dec;
      decimal32 dec;
    } u;

  /* bid32 is a pointer to _Decimal32 in bid endian. But _bid_to_dpd32
     takes host endian.  */
  __ieee_to_host_32 (*bid32, &u._Dec);

  /* Convert BID to DPD.  */
  _bid_to_dpd32 (&u._Dec, &u._Dec);

  /* __dpd32ToNumber is in bid endian.  */
  __host_to_ieee_32 (u._Dec, &u.dec);

  return __dpd32ToNumber (&u.dec, dn);
}

char *
decimal32ToString (const decimal32 *d32, char *string)
{
  decNumber dn;			/* work */
  decimal32ToNumber (d32, &dn);
  decNumberToString (&dn, string);
  return string;
}

char *
decimal32ToEngString (const decimal32 *d32, char *string)
{
  decNumber dn;			/* work */
  decimal32ToNumber (d32, &dn);
  decNumberToEngString (&dn, string);
  return string;
}

decimal32 *
decimal32FromString (decimal32 *result, const char *string,
		      decContext *set)
{
  decContext dc;		/* work */
  decNumber dn;			/* .. */

  decContextDefault (&dc, DEC_INIT_DECIMAL32);	/* no traps, please */
  dc.round = set->round;	/* use supplied rounding */

  decNumberFromString (&dn, string, &dc);	/* will round if needed */
  decimal32FromNumber (result, &dn, &dc);
  if (dc.status != 0)
    {				/* something happened */
      decContextSetStatus (set, dc.status);	/* .. pass it on */
    }
  return result;
}
