/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <java/net/VMURLConnection.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

#if defined (HAVE_MAGIC_H) && defined (HAVE_MAGIC_OPEN)

#include <magic.h>

static magic_t cookie;

#endif /* HAVE_MAGIC_H && HAVE_MAGIC_OPEN */

void
java::net::VMURLConnection::init ()
{
#if defined (HAVE_MAGIC_H) && defined (HAVE_MAGIC_OPEN)
  cookie = magic_open (MAGIC_MIME);
  if (cookie == (magic_t) NULL)
    return;
  if (magic_load (cookie, NULL) == -1)
    {
      magic_close (cookie);
      cookie = (magic_t) NULL;
    }
#endif /* HAVE_MAGIC_H && HAVE_MAGIC_OPEN */
}

::java::lang::String *
java::net::VMURLConnection::guessContentTypeFromBuffer (jbyteArray bytes,
							jint valid)
{
#if defined (HAVE_MAGIC_H) && defined (HAVE_MAGIC_OPEN)
  const char *result;

  if (cookie == (magic_t) NULL)
    return NULL;

  result = magic_buffer (cookie, elements(bytes), valid);

  if (result == NULL)
    return NULL;
  return _Jv_NewStringUTF (result);
#else
  return NULL;
#endif /* HAVE_MAGIC_H && HAVE_MAGIC_OPEN */
}
