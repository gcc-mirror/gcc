// natDeflater.cc - Implementation of Deflater native methods.

/* Copyright (C) 1999  Free Software Foundation

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

#include <java/util/zip/Deflater.h>
#include <java/util/zip/DataFormatException.h>

#include <java/lang/InternalError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>

extern void *_Jv_ZMalloc (void *, uInt nitems, uInt size);
extern void _Jv_ZFree (void *, void *addr);



jint
java::util::zip::Deflater::deflate (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    _Jv_Throw (new java::lang::NullPointerException);
  if (off < 0 || len < 0 || off + len > buf->length)
    _Jv_Throw (new java::lang::ArrayIndexOutOfBoundsException);

  if (len == 0)
    return 0;

  s->next_out = (Bytef *) (elements (buf) + off);
  s->avail_out = len;

  switch (::deflate (s, flush_flag))
    {
    case Z_STREAM_END:
      is_finished = true;
      if (s->avail_out == (unsigned int) len)
	return -1;
      break;

    case Z_STREAM_ERROR:
    case Z_BUF_ERROR:
      // FIXME?
      _Jv_Throw (new java::lang::InternalError);
      break;

    case Z_OK:
      break;
    }

  return len - s->avail_out;
}

void
java::util::zip::Deflater::end ()
{
  JvSynchronize sync (this);
  // Just ignore errors.
  deflateEnd ((z_streamp) zstream);
  _Jv_Free (zstream);
  zstream = NULL;
}

void
java::util::zip::Deflater::finish ()
{
  JvSynchronize sync (this);
  flush_flag = Z_FINISH;
}

jint
java::util::zip::Deflater::getAdler ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->adler;
}

jint
java::util::zip::Deflater::getTotalIn ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->total_in;
}

jint
java::util::zip::Deflater::getTotalOut ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->total_out;
}

jboolean
java::util::zip::Deflater::needsInput ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  return s->avail_in == 0;
}

void
java::util::zip::Deflater::reset ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;
  // Just ignore errors.
  deflateReset (s);
  flush_flag = 0;
}

void
java::util::zip::Deflater::setDictionary (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    _Jv_Throw (new java::lang::NullPointerException);
  if (off < 0 || len < 0 || off + len > buf->length)
    _Jv_Throw (new java::lang::ArrayIndexOutOfBoundsException);

  // Ignore errors.
  deflateSetDictionary (s, (Bytef *) (elements (buf) + off), len);
}

void
java::util::zip::Deflater::setInput (jbyteArray buf, jint off, jint len)
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  if (! buf)
    _Jv_Throw (new java::lang::NullPointerException);
  if (off < 0 || len < 0 || off + len > buf->length)
    _Jv_Throw (new java::lang::ArrayIndexOutOfBoundsException);

  s->next_in = (Bytef *) (elements (buf) + off);
  s->avail_in = len;
}

void
java::util::zip::Deflater::update ()
{
  JvSynchronize sync (this);
  z_streamp s = (z_streamp) zstream;

  int strat = Z_DEFAULT_STRATEGY;
  switch (strategy)
    {
    case DEFAULT_STRATEGY:
      strat = Z_DEFAULT_STRATEGY;
      break;
    case FILTERED:
      strat = Z_FILTERED;
      break;
    case HUFFMAN_ONLY:
      strat = Z_HUFFMAN_ONLY;
      break;
    default:
      JvFail ("unexpected strategy");
    }

  // Ignore errors.
  deflateParams (s, level, strat);
}

void
java::util::zip::Deflater::init (jint level, jboolean no_header)
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

#define DEFAULT_MEM_LEVEL 8
  if (deflateInit2 (stream, level, Z_DEFLATED, wbits,
		    DEFAULT_MEM_LEVEL, Z_DEFAULT_STRATEGY) != Z_OK)
    {
      jstring msg = NULL;
      if (stream->msg != NULL)
	msg = JvNewStringLatin1 (stream->msg);
      _Jv_Throw (new java::lang::InternalError (msg));
    }

  zstream = reinterpret_cast<gnu::gcj::RawData *> (stream);
  is_finished = false;
  flush_flag = 0;
}
