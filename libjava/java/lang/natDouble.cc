// natDouble.cc - Implementation of java.lang.Double native methods.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdlib.h>

#include <gcj/cni.h>
#include <java/lang/String.h>
#include <java/lang/Double.h>
#include <java/lang/NumberFormatException.h>
#include <jvm.h>

#include <stdio.h>
#include <string.h>

#include "mprec.h"

union u
{
  jlong l;
  jdouble d;
};

jlong 
java::lang::Double::doubleToLongBits(jdouble value)
{
  union u u;
  u.d = value;
  
  jlong e = u.l & 0x7ff0000000000000LL;
  jlong f = u.l & 0x000fffffffffffffLL;

  if (e == 0x7ff0000000000000LL && f != 0L)
    u.l = 0x7ff8000000000000LL;

  return u.l;
}

jdouble 
java::lang::Double::longBitsToDouble(jlong bits)
{
  union u u;
  u.l = bits;
  return u.d;
}

jstring 
java::lang::Double::toString(jdouble value, jboolean isFloat)
{
  if (isNaN (value))
    return JvNewStringLatin1 ("NaN", sizeof ("NaN") - 1);
    
  if (value == POSITIVE_INFINITY)
    return JvNewStringLatin1 ("Infinity", sizeof ("Infinity") - 1);
    
  if (value == NEGATIVE_INFINITY)
    return JvNewStringLatin1 ("-Infinity", sizeof ("-Infinity") - 1);
    
  char buffer[50], result[50];
  int decpt, sign;

  _dtoa (value, 0, 20, &decpt, &sign, NULL, buffer, (int)isFloat);

  value = fabs (value);

  char *s = buffer;
  char *d = result;

  if (sign)
    *d++ = '-';

  if (value >= 1e-3 && value < 1e7 || value == 0)
    {
      if (decpt <= 0)
	*d++ = '0';
      else
	{
	  for (int i = 0; i < decpt; i++)
	    if (*s)
	      *d++ = *s++;
	    else
	      *d++ = '0';
	}

      *d++ = '.';

      if (*s == 0)
	{
	  *d++ = '0';
	  decpt++;
	}
	  
      while (decpt++ < 0)
	*d++ = '0';      
      
      while (*s)
	*d++ = *s++;

      *d = 0;

      return JvNewStringLatin1 (result, strlen (result));
    }

  *d++ = *s++;
  decpt--;
  *d++ = '.';
  
  if (*s == 0)
    *d++ = '0';

  while (*s)
    *d++ = *s++;

  *d++ = 'E';
  
  if (decpt < 0)
    {
      *d++ = '-';
      decpt = -decpt;
    }

  {
    char exp[4];
    char *e = exp + sizeof exp;
    
    *--e = 0;
    do
      {
	*--e = '0' + decpt % 10;
	decpt /= 10;
      }
    while (decpt > 0);

    while (*e)
      *d++ = *e++;
  }
  
  *d = 0;

  return JvNewStringLatin1 (result, strlen (result));
}

jdouble 
java::lang::Double::doubleValueOf(jstring str)
{
  int length = str->length();
  // Note that UTF can expand 3x.

#ifdef HAVE_ALLOCA
  char *data = (char *) alloca (3 * length + 1);
#else
#error --- need an alternate implementation here ---
#endif

  data[_Jv_GetStringUTFRegion (str, 0, length, data)] = 0; 

  struct _Jv_reent reent;  
  memset (&reent, 0, sizeof reent);

  double val = _strtod_r (&reent, data, NULL);

  if (reent._errno)
    _Jv_Throw (new NumberFormatException);

  return val;
}
