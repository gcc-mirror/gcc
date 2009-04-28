// natConfiguration.cc - native code for configuration.

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/classpath/Configuration.h>

jstring
gnu::classpath::Configuration::classpath_home()
{
  return JvNewStringLatin1(LIBGCJ_PREFIX);
}

jboolean
gnu::classpath::Configuration::debug()
{
#ifdef __GCJ_DEBUG
  return true;
#else
  return false;
#endif
}

jstring
gnu::classpath::Configuration::toolkit()
{
  return JvNewStringLatin1(AWT_TOOLKIT);
}

jstring
gnu::classpath::Configuration::ecj()
{
  return JvNewStringLatin1(ECJ_JAR_FILE);
}
