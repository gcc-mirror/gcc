/* Copyright (C) 2001, 2003, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj;

public class Core
{
  public native static Core create (String name) throws java.io.IOException;

  // Same as create, except returns null if not found.
  public native static Core find (String name);

  public RawData ptr;
  public int length;

  Core ()
  {
  }
}
