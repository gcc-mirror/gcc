// Input_iconv.java -- Java side of iconv() reader.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Tom Tromey <tromey@redhat.com>.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/gcj/convert/Input_iconv.h>
#include <gnu/gcj/convert/Output_iconv.h>
#include <java/io/UnsupportedEncodingException.h>

#ifdef HAVE_ICONV
#include <iconv.h>
#endif

void
gnu::gcj::convert::Input_iconv::init (jstring encoding)
{
#ifdef HAVE_ICONV
  jsize len = _Jv_GetStringUTFLength (encoding);
  char buffer[len];
  _Jv_GetStringUTFRegion (encoding, 0, len, buffer);

  iconv_t h = iconv_open ("UCS-2", buffer);
  if (h == (iconv_t) -1)
    JvThrow (new java::io::UnsupportedEncodingException);

  JvAssert (h != NULL);
  handle = reinterpret_cast<gnu::gcj::RawData *> (h);
#else /* HAVE_ICONV */
  // If no iconv, just throw an exception.
  JvThrow (new java::io::UnsupportedEncodingException);
#endif /* HAVE_ICONV */
}

void
gnu::gcj::convert::Input_iconv::finalize (void)
{
#ifdef HAVE_ICONV
  if (handle != NULL)
    {
      iconv_close ((iconv_t) handle);
      handle = NULL;
    }
#endif /* HAVE_ICONV */
}

jint
gnu::gcj::convert::Input_iconv::read (jcharArray outbuffer,
				      jint outpos, jint count)
{
#ifdef HAVE_ICONV
  jint origpos = outpos;

  jbyte *bytes = elements (inbuffer);
  jchar *out = elements (outbuffer);
  size_t inavail = inlength - inpos;
  size_t old_in = inavail;
  size_t outavail = count;
  size_t old_out = outavail;

  const char *inbuf = (const char *) &bytes[inpos];
  char *outbuf = (char *) &out[outpos];

  size_t r = iconv ((iconv_t) handle,
		    &inbuf, &inavail,
		    &outbuf, &outavail);
  // FIXME: what if R==-1?

  inpos += old_in - inavail;
  return old_out - outavail;
#else /* HAVE_ICONV */
  return -1;
#endif /* HAVE_ICONV */
}

void
gnu::gcj::convert::Output_iconv::init (jstring encoding)
{
#ifdef HAVE_ICONV
  jsize len = _Jv_GetStringUTFLength (encoding);
  char buffer[len];
  _Jv_GetStringUTFRegion (encoding, 0, len, buffer);

  iconv_t h = iconv_open (buffer, "UCS-2");
  if (h == (iconv_t) -1)
    JvThrow (new java::io::UnsupportedEncodingException);

  JvAssert (h != NULL);
  handle = reinterpret_cast<gnu::gcj::RawData *> (h);
#else /* HAVE_ICONV */
  // If no iconv, just throw an exception.
  JvThrow (new java::io::UnsupportedEncodingException);
#endif /* HAVE_ICONV */
}

void
gnu::gcj::convert::Output_iconv::finalize (void)
{
#ifdef HAVE_ICONV
  if (handle != NULL)
    {
      iconv_close ((iconv_t) handle);
      handle = NULL;
    }
#endif /* HAVE_ICONV */
}

jint
gnu::gcj::convert::Output_iconv::write (jcharArray inbuffer,
					jint inpos, jint count)
{
#ifdef HAVE_ICONV
  jint origpos = inpos;

  jchar *chars = elements (inbuffer);
  jbyte *out = elements (buf);

  size_t inavail = count;
  size_t old_in = count;

  size_t outavail = buf->length - count;
  size_t old_out = outavail;

  const char *inbuf = (const char *) &chars[inpos];
  char *outbuf = (char *) &out[count];

  size_t r = iconv ((iconv_t) handle,
		    &inbuf, &inavail,
		    &outbuf, &outavail);
  // FIXME: what if R==-1?

  count += old_out - outavail;
  return old_in - inavail;
#else /* HAVE_ICONV */
  return -1;
#endif /* HAVE_ICONV */
}
