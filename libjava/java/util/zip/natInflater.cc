// natInflater.cc - Implementation of Inflater native methods.

/* Copyright (C) 1999, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@cygnus.com>

#include <config.h>

#include <zlib.h>
#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/util/zip/Inflater.h>
#include <java/util/zip/DataFormatException.h>

#include <java/lang/InternalError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/OutOfMemoryError.h>



// A couple of helper functions used to interface with zlib's
// allocation.

void *
_Jv_ZMalloc (void *, uInt nitems, uInt size)
{
  return _Jv_Malloc (nitems * size);
}

void
_Jv_ZFree (void *, void *addr)
{
  _Jv_Free (addr);
}



void
java::util::zip::Inflater::end ()
{
  JvSynchronize sync (this);
  // Just ignore errors.
  inflateEnd ((z_streamp) zstream);
  _Jv_Free (zstream);
  zstream = NULL;
}

jint
java::util::zip::Inflater::getAdler ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->adler;
}

jint
java::util::zip::Inflater::getRemaining ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->avail_in;
}

jint
java::util::zip::Inflater::getTotalIn ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->total_in;
}

jint
java::util::zip::Inflater::getTotalOut ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->total_out;
}

jint
java::util::zip::Inflater::inflate (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    throw new java::lang::NullPointerException;
  if (off < 0 || len < 0 || off + len > buf->length)
    throw new java::lang::ArrayIndexOutOfBoundsException;

  if (len == 0)
    return 0;

  s->next_out = (Bytef *) (elements (buf) + off);
  s->avail_out = len;

  switch (::inflate (s, Z_SYNC_FLUSH))
    {
    case Z_BUF_ERROR:
      /* Using the no_header option, zlib requires an extra padding byte at the
      end of the stream in order to successfully complete decompression (see
      zlib/contrib/minizip/unzip.c). We don't do this, so can end up with a 
      Z_BUF_ERROR at the end of a stream when zlib has completed inflation
      and there's no more input. Thats not a problem. */
      if (s->avail_in != 0)
        throw new java::lang::InternalError;
      // Fall through.
      
    case Z_STREAM_END:
      is_finished = true;
      if (s->avail_out == (unsigned int) len)
	return -1;
      break;

    case Z_NEED_DICT:
      dict_needed = true;
      break;

    case Z_DATA_ERROR:
      throw new java::util::zip::DataFormatException 
	(s->msg == NULL ? NULL : JvNewStringLatin1 (s->msg));
      break;

    case Z_MEM_ERROR:
      throw new java::lang::OutOfMemoryError;
      break;

    case Z_OK:
      break;
    }

  return len - s->avail_out;
}

void
java::util::zip::Inflater::reset ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  // Just ignore errors.
  inflateReset (s);
  s->avail_in = 0;
  is_finished = false;
  dict_needed = false;
}

void
java::util::zip::Inflater::setDictionary (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    throw new java::lang::NullPointerException;
  if (off < 0 || len < 0 || off + len > buf->length)
    throw new java::lang::ArrayIndexOutOfBoundsException;

  // Ignore errors.
  inflateSetDictionary (s, (Bytef *) (elements (buf) + off), len);
  dict_needed = false;
}

void
java::util::zip::Inflater::setInput (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    throw new java::lang::NullPointerException;
  if (off < 0 || len < 0 || off + len > buf->length)
    throw new java::lang::ArrayIndexOutOfBoundsException;

  s->next_in = (Bytef *) (elements (buf) + off);
  s->avail_in = len;
}

void
java::util::zip::Inflater::init (jboolean no_header)
{
  z_stream_s *stream = (z_stream_s *) _Jv_Malloc (sizeof (z_stream_s));
  stream->next_in = Z_NULL;
  stream->avail_in = 0;
  stream->zalloc = _Jv_ZMalloc;
  stream->zfree = _Jv_ZFree;
  stream->opaque = NULL;

  // Handle NO_HEADER using undocumented zlib feature.
  int wbits = MAX_WBITS;
  if (no_header)
    wbits = - wbits;

  if (inflateInit2 (stream, wbits) != Z_OK)
    {
      jstring msg = NULL;
      if (stream->msg != NULL)
	msg = JvNewStringLatin1 (stream->msg);
      throw new java::lang::InternalError (msg);
    }

  zstream = reinterpret_cast<gnu::gcj::RawData *> (stream);
  is_finished = false;
  dict_needed = false;
}
