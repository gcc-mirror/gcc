// AccessibleObject.java - Base for reflection objects.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 */
/* Written using JDK 1.2 beta docs.
 * Status:  Believed complete and correct.
 */

public class AccessibleObject
{
  protected AccessibleObject ()
    {
      flag = false;
    }

  boolean isAccessible ()
    {
      return flag;
    }

  static void setAccessible (AccessibleObject[] array, boolean flag)
    {
      checkPermission ();
      for (int i = 0; i < array.length; ++i)
	array[i].flag = flag;
    }

  void setAccessible (boolean flag)
    {
      checkPermission ();
      this.flag = flag;
    }

  private static final void checkPermission ()
    {
      SecurityManager sm = System.getSecurityManager();
      // FIXME: sm.checkPermission(ReflectPermission ("suppressAccessChecks"))
    }

  private boolean flag;
}
