/* java.lang.reflect.AccessibleObject
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

 
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

  public boolean isAccessible ()
  {
    return flag;
  }

  public static void setAccessible (AccessibleObject[] array, boolean flag)
  {
    checkPermission ();
    // FIXME: check for invalid changes in the loop.
    // For instance, can't set this flag to true for a Constructor for
    // Class (example from the manual).
    for (int i = 0; i < array.length; ++i)
      array[i].flag = flag;
  }

  public void setAccessible (boolean flag)
  {
    checkPermission ();
    // FIXME: check for invalid changes.
    // For instance, can't set this flag to true for a Constructor for
    // Class (example from the manual).
    this.flag = flag;
  }

  private static final void checkPermission ()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission (new ReflectPermission ("suppressAccessChecks"));
  }

  private boolean flag;
}
