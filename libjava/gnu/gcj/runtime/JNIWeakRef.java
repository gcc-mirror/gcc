// JNIWeakRef.java - Wrapper class for JNI-specific weak references.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.lang.ref.*;

// This wrapper is used by the JNI implementation to keep track of
// global weak references.  Each such reference is wrapped in an
// instance of this class, and automatically unwrapped when used.
public final class JNIWeakRef extends WeakReference
{
  public JNIWeakRef (Object referent)
  {
    super (referent);
  }
}
