/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gnu/gcj/convert/Input_SJIS.h>

#define ERROR_CHAR 0xFFFD

extern unsigned short JIS0208_to_Unicode[84][94];
extern unsigned short JIS0212_to_Unicode[76][94];

jint
gnu::gcj::convert::Input_SJIS::read(jcharArray outbuffer, jint outpos,
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
      if (first_byte == 0)
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
	  else if (b >= 0xA1 && b <= 0xDF)
	    {
	      b += 0xFF61 - 0xA1;
	      elements(outbuffer)[outpos++] = b;
	    }
	  else
	    first_byte = b;
	}
      else
	{
	  // From Lunde: "CJKV Informatio Processing", O'Reilly, 1999, p 420:
	  bool adjust = b < 159;
	  int rowOffset = first_byte < 160 ? 112 : 176;
	  int cellOffset = adjust ? (b > 127 ? 32 : 31) : 126;
	  first_byte = ((first_byte - rowOffset) << 1) - adjust;
	  b -= cellOffset;

	  first_byte -= 33;
	  b -= 33;

	  if ((unsigned) first_byte >= 84 || (unsigned) b >= 94)
	    b = ERROR_CHAR;
	  else
	    {
	      b = JIS0208_to_Unicode[first_byte][b];
	      if (b == 0)
		b = ERROR_CHAR;
	    }
	  elements(outbuffer)[outpos++] = b;

	  first_byte = 0;
	}
    }
  return outpos - start_outpos;
}
