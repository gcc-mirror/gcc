/* GCInfo.java -- Support for creating heap dumps.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.util;

import java.security.BasicPermission;

public class UtilPermission extends BasicPermission
{
  public UtilPermission(String name)
  {
    super(name);
  }
}
