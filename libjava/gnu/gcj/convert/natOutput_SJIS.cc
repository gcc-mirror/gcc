/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gnu/gcj/convert/Output_SJIS.h>

extern unsigned short Unicode_to_JIS[];

extern int trie_lookup (unsigned short *trie, unsigned short key);

static jint
convert_TO_SJIS (gnu::gcj::convert::Output_SJIS *encoder,
			  jchar *ptr, jint inlength)
{
  int orig_inlength = inlength;
  jint outbuf_length = encoder->buf->length;
  for (;;)
    {
      if (encoder->count >= outbuf_length)
	break;
      if (encoder->pending >= 0)
	{
	  elements(encoder->buf)[encoder->count++] = encoder->pending;
	  encoder->pending = -1;
	  continue;
	}
      if (inlength == 0)
	break;
      jchar ch = *ptr++;
      inlength--;
      unsigned short val = trie_lookup(Unicode_to_JIS, ch);
      if (val < 0xFF)
	{
	  if (val == 0xffff)
	    val = '?';
	}
      else
	{
	  int b1 = val >> 8;
	  int b2 = val & 0xff;
	  // From Lunde: "CJKV Informatio Processing", O'Reilly, 1999:
	  int rowOffset = b1 < 95 ? 112 : 176;
	  int cellOffset = (b1 & 1) != 0 ? (b2 > 95 ? 32 : 31) : 126;
	  b1 = ((b1 + 1) >> 1) + rowOffset;
	  b2 += cellOffset;
	  val = b1;
	  encoder->pending = b2;
	}
      elements(encoder->buf)[encoder->count++] = val;
    }
  return orig_inlength - inlength;
}

jint
gnu::gcj::convert::Output_SJIS::write (jcharArray inbuffer,
					 jint inpos, jint inlength)
{
  return convert_TO_SJIS(this, &elements(inbuffer)[inpos], inlength);
}

jint
gnu::gcj::convert::Output_SJIS::write (jstring str, jint inpos,
					 jint inlength, jcharArray)
{
  return convert_TO_SJIS(this, _Jv_GetStringChars(str)+inpos, inlength);
}
