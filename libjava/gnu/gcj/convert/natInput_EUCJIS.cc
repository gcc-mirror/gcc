/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gnu/gcj/convert/Input_EUCJIS.h>

#define ERROR_CHAR 0xFFFD

extern unsigned short JIS0208_to_Unicode[84][94];
extern unsigned short JIS0212_to_Unicode[76][94];

jint
gnu::gcj::convert::Input_EUCJIS::read(jcharArray outbuffer, jint outpos,
				      jint count)
{
  jint start_outpos = outpos;
  for (;;)
    {
      if (outpos - start_outpos >= count)
	break;
      if (inpos >= inlength)
	break;
      int b = ((unsigned char*) elements(inbuffer))[inpos++];
      if (codeset == 0)  // ASCII or JIS-Roman
	{
	  if (b < 128)
	    {
#if 1
	      // Technically, we should translate 0x5c to Yen symbol;
	      // in practice, it is not clear.
	      if (b == 0x5c)
		b = 0x00A5;  // Yen sign.
#endif
	      elements(outbuffer)[outpos++] = (char) b;
	    }
	  else
	    {
	      if (b == 0x8E) // SS2
		codeset = 2;
	      else if (b == 0x8F) // SS3
		codeset = 3;
	      else
		{
		  codeset = 1;
		  first_byte = b;
		}
	    }
	}
      else if (codeset == 1) // JIS X 0208:1997
	{
	  first_byte -= 0x80 + 33;
	  b -= 0x80 + 33;
	  if ((unsigned) first_byte >= 84 || (unsigned) b >= 94)
	    b = ERROR_CHAR;
	  else
	    {
	      b = JIS0208_to_Unicode[first_byte][b];
	      if (b == 0)
		b = ERROR_CHAR;
	    }
	  elements(outbuffer)[outpos++] = b;
	  codeset = 0;
	}
      else if (codeset == 2) // Half-width katakana
	{
	  if (b >= 0xA1 && b <= 0xDF)
	    b += 0xFF61 - 0xA1;
	  else
	    b = ERROR_CHAR;
	  elements(outbuffer)[outpos++] = b;
	  codeset = 0;
	}
      else if (codeset == 3) // second byte of JIS X 0212-1990
	{
	  first_byte = b;
	  codeset = 4;
	}
      else // codeset == 4 // third byte of JIS X 0212-1990
	{
	  first_byte -= 0x80 + 34;
	  b -= 0x80 + 33;
	  if ((unsigned) first_byte >= 76 || (unsigned) b >= 94)
	    b = ERROR_CHAR;
	  else
	    {
	      b = JIS0208_to_Unicode[first_byte][b];
	      if (b == 0)
		b = ERROR_CHAR;
	    }
	  elements(outbuffer)[outpos++] = b;
	  codeset = 0;
	}
    }
  return outpos - start_outpos;
}
