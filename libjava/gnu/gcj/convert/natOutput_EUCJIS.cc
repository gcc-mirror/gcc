/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gnu/gcj/convert/Output_EUCJIS.h>

/* A trie structure to map unicode values to JIS codes.
 * code == -1: the character is undefined.
 * code >= 0 && code < 128:  JIS-Roman - mostly Ascii.
 * code >= 128 && code < 256:  Half-width Katakana.
 * code >= 256 && code < 0x8000:  JIS X 0208:1997.
 * code >= 0x8000 && code < 0xFFFF:  JIX X 0212-1990.
 */

extern unsigned short Unicode_to_JIS[];

int
trie_lookup (unsigned short *trie, unsigned short key)
{
  unsigned short branch = trie[(key >> 12) & 0xf];
  if (branch == 0)
    return -1;
  branch = trie[branch + ((key >> 8) & 0xf)];
  if (branch == 0)
    return -1;
  branch = trie[branch + ((key >> 4) & 0xf)];
  if (branch == 0)
    return -1;
  return trie[branch + (key & 0xf)];
}

static jint
convert_TO_EUCJIS (gnu::gcj::convert::Output_EUCJIS *encoder,
			  jchar *ptr, jint inlength)
{
  int orig_inlength = inlength;
  jint outbuf_length = encoder->buf->length;
  for (;;)
    {
      if (encoder->count >= outbuf_length)
	break;
      if (encoder->pending1 >= 0)
	{
	  elements(encoder->buf)[encoder->count++] = encoder->pending1;
	  encoder->pending1 = encoder->pending2;
	  encoder->pending2 = -1;
	  continue;
	}
      if (inlength == 0)
	break;
      jchar ch = *ptr++;
      inlength--;
      unsigned short val = trie_lookup(Unicode_to_JIS, ch);
      if (val < 0x80)
	{
	  if (val == 0xffff)
	    val = '?';
	}
      else if (val <= 0xFF)
	{
	  encoder->pending1 = val;
	  encoder->pending2 = -1;
	  val = 0x8e;
	}
      else if (val < 0x8000)
	{
	  val |= 0x8080;
	  encoder->pending1 = val & 0xff;
	  val = val >> 8;
	  encoder->pending2 = -1;
	}
      else
	{
	  val |= 0x8080;
	  encoder->pending1 = val >> 8;
	  encoder->pending2 = val & 0xff;
	  val = 0x8f;
	}
      elements(encoder->buf)[encoder->count++] = val;
    }
  return orig_inlength - inlength;
}

jint
gnu::gcj::convert::Output_EUCJIS::write (jcharArray inbuffer,
					 jint inpos, jint inlength)
{
  return convert_TO_EUCJIS(this, &elements(inbuffer)[inpos], inlength);
}

jint
gnu::gcj::convert::Output_EUCJIS::write (jstring str, jint inpos,
					 jint inlength, jcharArray)
{
  return convert_TO_EUCJIS(this, _Jv_GetStringChars(str)+inpos, inlength);
}
