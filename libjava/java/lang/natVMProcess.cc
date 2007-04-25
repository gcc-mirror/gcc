// natVMProcess.cc - native code for ProcessBuilder

/* Copyright (C) 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <platform.h>

#include <jvm.h>

#include <java/lang/VMProcess.h>
#include <java/lang/Process.h>
#include <java/io/File.h>

// It is convenient and safe to simply include all of these.
#include <java/lang/Win32Process.h>
#include <java/lang/EcosProcess.h>
#include <java/lang/PosixProcess.h>

::java::lang::Process *
java::lang::VMProcess::nativeExec (jstringArray cmd,
				   jstringArray env,
				   ::java::io::File *dir,
				   jboolean redirect)
{
  return new _Jv_platform_process (cmd, env, dir, redirect);
}
