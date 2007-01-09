/* ReflectUtil.java - JSR 166 reflection hooks
   Copyright (C) 2006 Free Software Foundation

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package sun.reflect.misc;

import java.lang.reflect.Modifier;

public class ReflectUtil
{
  // We use this inaccessible inner class as an argument type
  // in verifyMemberAccess.  All current users of this method
  // in the JSR 166 RI pass 'null' for this argument, and
  // consequently we don't know what it means.  Using a funny
  // type like this for the argument means that if the RI changes,
  // we will see a compilation error.
  private static class MustBeNull
  {
  }

  /**
   * Check if the current thread is allowed to access the package of
   * the declaringClass. 
   *
   * @param declaringClass class name to check access to
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if declaringClass is null
   */
  public static void checkPackageAccess(Class declaringClass)
  {
    SecurityManager sm;
    if ((sm = System.getSecurityManager()) != null)
      {
	while (declaringClass.isArray())
	  declaringClass = declaringClass.getComponentType();
	String name = declaringClass.getName();
	int i = name.lastIndexOf('.');
	if (i != -1) // if declaringClass is a member of a package
	  {
	    name = name.substring(0, i);
	    sm.checkPackageAccess(name);
	  }
      }
  }

  /**
   * Perform access checks on a member of a class.  This API is
   * derived from the public domain code in the JSR 166 reference
   * implementation.
   * @param caller the class requesting access to the member
   * @param declarer the declaring class of the member
   * @param ignored unknown parameter; always null
   * @param modifiers the modifiers on the member
   * @return true if access is granted, false otherwise
   */
  public static void ensureMemberAccess(Class caller,
                                        Class declarer,
                                        MustBeNull ignored,
                                        int modifiers)
  {
    // Same class, always ok.
    if (caller == declarer)
      return;
    // Public access is ok.
    if ((modifiers & Modifier.PUBLIC) != 0)
      return;
    // Protected access and request comes from
    // a subclass of the declarer -- ok.
    if ((modifiers & Modifier.PROTECTED) != 0
        && declarer.isAssignableFrom(caller))
      return;
    // Package-private access, or protected access,
    // and the packages are the same --ok.
    if ((modifiers & Modifier.PRIVATE) == 0
        && caller.getPackage() == declarer.getPackage())
      return;
    // Otherwise, no.
    throw new IllegalAccessError();
  }
}
