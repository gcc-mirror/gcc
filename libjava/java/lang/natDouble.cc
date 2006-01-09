// natDouble.cc - Implementation of java.lang.Double native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <java/lang/String.h>
#include <java/lang/Double.h>
#include <java/lang/Character.h>
#include <java/lang/NumberFormatException.h>
#include <jvm.h>

#include <stdio.h>
#include <string.h>

#include "fdlibm.h"

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

jlong 
java::lang::Double::doubleToRawLongBits(jdouble value)
{
  union u u;
  u.d = value;
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
java::lang::Double::parseDouble(jstring str)
{
  int length = str->length();

  while (length > 0
	 && Character::isWhitespace(str->charAt(length - 1)))
    length--;

  // The String could end with a f/F/d/D which is valid but we don't need.
  bool saw_trailer = false;
  if (length > 0)
    {
      jchar last = str->charAt(length-1);
      if (last == 'f' || last == 'F' || last == 'd' || last == 'D')
	{
	  length--;
	  saw_trailer = true;
	}
    }

  jsize start = 0;
  while (length > 0
	 && Character::isWhitespace(str->charAt(start)))
    start++, length--;

  if (length > 0)
    {
      // Note that UTF can expand 3x.
      char *data = (char *) __builtin_alloca (3 * length + 1);
      jsize blength = _Jv_GetStringUTFRegion (str, start, length, data);
      data[blength] = 0; 

      if (! saw_trailer)
	{
	  if (! strcmp (data, "NaN") || ! strcmp (data, "+NaN")
	      || ! strcmp (data, "-NaN"))
	    return NaN;
	  else if (! strcmp (data, "Infinity") || ! strcmp (data, "+Infinity"))
	    return POSITIVE_INFINITY;
	  else if (! strcmp (data, "-Infinity"))
	    return NEGATIVE_INFINITY;
	}

      struct _Jv_reent reent;  
      memset (&reent, 0, sizeof reent);

      char *endptr;
      double val = _strtod_r (&reent, data, &endptr);
      if (endptr == data + blength)
	return val;
    }
  throw new NumberFormatException(str);
}
