// gnu.gcj.runtime.MethodRef -- used by StackTrace.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import gnu.gcj.RawData;

class MethodRef
{
  MethodRef(RawData /* Actually _Jv_Method */ m, Class k)
  {
    klass = k;
    method = m;
  }

  public RawData method; // Actually a raw pointer to _Jv_Method
  public Class klass;
}
