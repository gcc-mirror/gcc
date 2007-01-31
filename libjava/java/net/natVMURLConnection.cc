/* Copyright (C) 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <java/net/VMURLConnection.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>
#include <stdio.h>

#if defined (HAVE_MAGIC_T) && defined (HAVE_MAGIC_H) && defined (USE_LTDL)

#include <magic.h>
#include <ltdl.h>

static magic_t cookie;

static magic_t (*p_magic_open)(int flags);
static int (*p_magic_load)(magic_t cookie, const char *filename);
static void (*p_magic_close)(magic_t cookie);
static const char * (*p_magic_buffer) (magic_t cookie, const void *buffer,
				       size_t length);

#endif /* HAVE_MAGIC_T && HAVE_MAGIC_H && defined (USE_LTDL) */

void
java::net::VMURLConnection::init ()
{
#if defined (HAVE_MAGIC_T) && defined (HAVE_MAGIC_H) && defined (USE_LTDL)
  lt_dlhandle handle = lt_dlopenext ("libmagic.so");
  if (!handle)
    return;

  p_magic_open = (typeof (p_magic_open))lt_dlsym(handle, "magic_open");
  if (p_magic_open == NULL)
    return;
  p_magic_buffer = (typeof (p_magic_buffer))lt_dlsym(handle, "magic_buffer");
  if (p_magic_buffer == NULL)
    return;
  p_magic_close = (typeof (p_magic_close))lt_dlsym(handle, "magic_close");
  if (p_magic_close == NULL)
    return;
  p_magic_load = (typeof (p_magic_load))lt_dlsym(handle, "magic_load");
  if (p_magic_load == NULL)
    return;

  cookie = p_magic_open (MAGIC_MIME);
  if (cookie == (magic_t) NULL)
    return;
  if (p_magic_load (cookie, NULL) == -1)
    {
      p_magic_close (cookie);
      cookie = (magic_t) NULL;
    }
#endif /* HAVE_MAGIC_T && HAVE_MAGIC_H  && defined (USE_LTDL) */
}

::java::lang::String *
java::net::VMURLConnection::guessContentTypeFromBuffer (jbyteArray bytes,
							jint valid)
{
#if defined (HAVE_MAGIC_T) && defined (HAVE_MAGIC_H) && defined (USE_LTDL)
  const char *result;

  if (cookie == (magic_t) NULL)
    return NULL;

  result = p_magic_buffer (cookie, elements(bytes), valid);

  if (result == NULL)
    return NULL;
  return _Jv_NewStringUTF (result);
#else
  return NULL;
#endif /* HAVE_MAGIC_T && HAVE_MAGIC_H  && defined (USE_LTDL) */
}
