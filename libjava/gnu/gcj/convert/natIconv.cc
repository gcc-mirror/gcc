// Input_iconv.java -- Java side of iconv() reader.

/* Copyright (C) 2000, 2001  Free Software Foundation

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
#include <java/io/CharConversionException.h>
#include <java/io/UnsupportedEncodingException.h>

#include <errno.h>

#ifdef HAVE_ICONV
#include <iconv.h>

template<typename T>
static inline size_t
iconv_adapter (size_t (*iconv_f) (iconv_t, T, size_t *, char **, size_t *),
	       iconv_t handle, char **inbuf, size_t *inavail,
	       char **outbuf, size_t *outavail)
{
  return (*iconv_f) (handle, (T) inbuf, inavail, outbuf, outavail);
}

#endif

void
gnu::gcj::convert::Input_iconv::init (jstring encoding)
{
#ifdef HAVE_ICONV
  jsize len = _Jv_GetStringUTFLength (encoding);
  char buffer[len + 1];
  _Jv_GetStringUTFRegion (encoding, 0, len, buffer);
  buffer[len] = '\0';

  iconv_t h = iconv_open ("UCS-2", buffer);
  if (h == (iconv_t) -1)
    throw new java::io::UnsupportedEncodingException (encoding);

  JvAssert (h != NULL);
  handle = reinterpret_cast<gnu::gcj::RawData *> (h);
#else /* HAVE_ICONV */
  // If no iconv, just throw an exception.
  throw new java::io::UnsupportedEncodingException (encoding);
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
  jbyte *bytes = elements (inbuffer);
  jchar *out = elements (outbuffer);
  size_t inavail = inlength - inpos;
  size_t old_in = inavail;
  size_t outavail = count * sizeof (jchar);
  size_t old_out = outavail;

  char *inbuf = (char *) &bytes[inpos];
  char *outbuf = (char *) &out[outpos];

  size_t r = iconv_adapter (iconv, (iconv_t) handle,
			    &inbuf, &inavail,
			    &outbuf, &outavail);

  if (r == (size_t) -1)
    {
      // If we see EINVAL then there is an incomplete sequence at the
      // end of the input buffer.  If we see E2BIG then we ran out of
      // space in the output buffer.  However, in both these cases
      // some conversion might have taken place.  So we fall through
      // to the normal case.
      if (errno != EINVAL && errno != E2BIG)
	throw new java::io::CharConversionException ();
    }

  if (iconv_byte_swap)
    {
      size_t max = (old_out - outavail) / sizeof (jchar);
      for (size_t i = 0; i < max; ++i)
	{
	  // Byte swap.
	  jchar c = (((out[outpos + i] & 0xff) << 8)
		     | ((out[outpos + i] >> 8) & 0xff));
	  outbuf[i] = c;
	}
    }

  inpos += old_in - inavail;
  return (old_out - outavail) / sizeof (jchar);
#else /* HAVE_ICONV */
  return -1;
#endif /* HAVE_ICONV */
}

void
gnu::gcj::convert::Input_iconv::done ()
{
#ifdef HAVE_ICONV
  // 50 bytes should be enough for any reset sequence.
  size_t avail = 50;
  char tmp[avail];
  char *p = tmp;
  // Calling iconv() with a NULL INBUF pointer will cause iconv() to
  // switch to its initial state.  We don't care about the output that
  // might be generated in that situation.
  iconv_adapter (iconv, (iconv_t) handle, NULL, NULL, &p, &avail);
  BytesToUnicode::done ();
#else /* HAVE_ICONV */
  // If no iconv, do nothing
#endif /* HAVE_ICONV */
}

void
gnu::gcj::convert::Output_iconv::init (jstring encoding)
{
#ifdef HAVE_ICONV
  jsize len = _Jv_GetStringUTFLength (encoding);
  char buffer[len + 1];
  _Jv_GetStringUTFRegion (encoding, 0, len, buffer);
  buffer[len] = '\0';

  iconv_t h = iconv_open (buffer, "UCS-2");
  if (h == (iconv_t) -1)
    throw new java::io::UnsupportedEncodingException (encoding);

  JvAssert (h != NULL);
  handle = reinterpret_cast<gnu::gcj::RawData *> (h);
#else /* HAVE_ICONV */
  // If no iconv, just throw an exception.
  throw new java::io::UnsupportedEncodingException (encoding);
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
					jint inpos, jint inlength)
{
#ifdef HAVE_ICONV
  jchar *chars = elements (inbuffer);
  jbyte *out = elements (buf);
  jchar *temp_buffer = NULL;

  size_t inavail = inlength * sizeof (jchar);
  size_t old_in = inavail;

  size_t outavail = buf->length - count;
  size_t old_out = outavail;

  char *inbuf = (char *) &chars[inpos];
  char *outbuf = (char *) &out[count];

  if (iconv_byte_swap)
    {
      // Ugly performance penalty -- don't use losing systems!
      temp_buffer = (jchar *) _Jv_Malloc (inlength * sizeof (jchar));
      for (int i = 0; i < inlength; ++i)
	{
	  // Byte swap.
	  jchar c = (((chars[inpos + i] & 0xff) << 8)
		     | ((chars[inpos + i] >> 8) & 0xff));
	  temp_buffer[i] = c;
	}
      inbuf = (char *) temp_buffer;
    }

  // If the conversion fails on the very first character, then we
  // assume that the character can't be represented in the output
  // encoding.  There's nothing useful we can do here, so we simply
  // omit that character.  Note that we can't check `errno' because
  // glibc 2.1.3 doesn't set it correctly.  We could check it if we
  // really needed to, but we'd have to disable support for 2.1.3.
  size_t loop_old_in = old_in;
  while (1)
    {
      size_t r = iconv_adapter (iconv, (iconv_t) handle,
				&inbuf, &inavail,
				&outbuf, &outavail);
      if (r == (size_t) -1 && inavail == loop_old_in)
	{
	  inavail -= 2;
	  if (inavail == 0)
	    break;
	  loop_old_in -= 2;
	  inbuf += 2;
	}
      else
	break;
    }

  if (temp_buffer != NULL)
    _Jv_Free (temp_buffer);

  count += old_out - outavail;
  return (old_in - inavail) / sizeof (jchar);
#else /* HAVE_ICONV */
  return -1;
#endif /* HAVE_ICONV */
}

jboolean
gnu::gcj::convert::IOConverter::iconv_init (void)
{
  // Some versions of iconv() always return their UCS-2 results in
  // big-endian order, and they also require UCS-2 inputs to be in
  // big-endian order.  For instance, glibc 2.1.3 does this.  If the
  // UTF-8=>UCS-2 iconv converter has this feature, then we assume
  // that all UCS-2 converters do.  (This might not be the best
  // heuristic, but is is all we've got.)
  jboolean result = false;
#ifdef HAVE_ICONV
  iconv_t handle = iconv_open ("UCS-2", "UTF-8");
  if (handle != (iconv_t) -1)
    {
      jchar c;
      unsigned char in[3];
      char *inp, *outp;
      size_t inc, outc, r;

      // This is the UTF-8 encoding of \ufeff.
      in[0] = 0xef;
      in[1] = 0xbb;
      in[2] = 0xbf;

      inp = (char *) in;
      inc = 3;
      outp = (char *) &c;
      outc = 2;

      r = iconv_adapter (iconv, handle, &inp, &inc, &outp, &outc);
      // Conversion must be complete for us to use the result.
      if (r != (size_t) -1 && inc == 0 && outc == 0)
	result = (c != 0xfeff);

      // Release iconv handle.
      iconv_close (handle);
    }
#endif /* HAVE_ICONV */
  return result;
}

void
gnu::gcj::convert::Output_iconv::done ()
{
#ifdef HAVE_ICONV
  // 50 bytes should be enough for any reset sequence.
  size_t avail = 50;
  char tmp[avail];
  char *p = tmp;
  // Calling iconv() with a NULL INBUF pointer will cause iconv() to
  // switch to its initial state.  We don't care about the output that
  // might be generated in that situation.
  iconv_adapter (iconv, (iconv_t) handle, NULL, NULL, &p, &avail);
  UnicodeToBytes::done ();
#else /* HAVE_ICONV */
  // If no iconv, do nothing
#endif /* HAVE_ICONV */
}
