/* DelegationPermission.java -- kerberos delegation permission
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.security.auth.kerberos;

import java.security.BasicPermission;
import java.security.Permission;
import java.security.PermissionCollection;
import java.util.Enumeration;
import java.util.Vector;

/**
 * @since 1.4
 */
public final class DelegationPermission
    extends BasicPermission
{
  // FIXME: Enable this when serialization works.
  // private static final long serialVersionUID = 883133252142523922L;

  /**
   * Create a new instance with the given name.
   */
  public DelegationPermission(String name)
  {
    super(name);
    checkSyntax(name);
  }

  /**
   * Create a new instance with the given name and actions.
   *
   * The name consists of two parts: first the subordinate
   * service principal, then the target service principal.
   * Each principal is surrounded by quotes; the two are separated
   * by a space.
   *
   * @param name the name
   * @param actions the actions; this is ignored
   */
  public DelegationPermission(String name, String actions)
  {
    super(name, actions);
    checkSyntax(name);
  }

  private static void checkSyntax(String name)
  {
    int index = name.indexOf('"', 1);
    int len = name.length();
    if (name.charAt(0) != '"' || name.charAt(len - 1) != '"'
        || index == -1 || index + 3 >= len
        || name.charAt(index + 1) != ' '
        || name.charAt(index + 2) != '"')
      // FIXME: better message here.
      throw new IllegalArgumentException("invalid syntax for principals");
  }

  public boolean implies(Permission perm)
  {
    return equals(perm);
  }

  public PermissionCollection newPermissionCollection()
  {
    // FIXME: don't know how to serialize here.  I suspect this
    // class has to have a particular name, etc ...
    return new PermissionCollection()
    {
      private Vector permissions = new Vector();

      public void add(Permission perm)
      {
        if (isReadOnly())
          throw new SecurityException("readonly");
        if (! (perm instanceof DelegationPermission))
          throw new IllegalArgumentException("can only add DelegationPermissions");
        permissions.add(perm);
      }

      public boolean implies(Permission perm)
      {
        if (! (perm instanceof DelegationPermission))
          return false;
        Enumeration e = elements();
        while (e.hasMoreElements())
          {
            DelegationPermission dp = (DelegationPermission) e.nextElement();
            if (dp.implies(perm))
              return true;
          }
        return false;
      }

      public Enumeration elements()
      {
        return permissions.elements();
      }
    };
  }
}
