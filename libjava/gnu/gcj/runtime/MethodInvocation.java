// MethodInvocation.java - wrapper used by the interpreter.
// (the native method is implemented in interpret.cc)

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author:  Kresten Krab Thorup <krab@gnu.org>  */

package gnu.gcj.runtime;

import gnu.gcj.RawData;

final class MethodInvocation {

  private static Throwable continue0 (RawData meth, RawData inv)
  {
    try {
      continue1 (meth, inv);
    } catch (Throwable ex) {
      return ex;
    }
    return null;
  }

  private static native void continue1 (RawData meth, RawData inv);

}
