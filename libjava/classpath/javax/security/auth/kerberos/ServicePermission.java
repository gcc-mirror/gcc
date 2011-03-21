/* ServicePermission.java -- kerberos service permission
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

import java.security.Permission;
import java.security.PermissionCollection;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * This represents permission to access to a Kerberos service principal.
 * See the Kerberos authentication RFC for more information:
 * <a href="http://www.ietf.org/rfc/rfc1510.txt">RFC 1510</a>.
 *
 * @since 1.4
 */
public final class ServicePermission
    extends Permission
{
  // FIXME: Enable this when serialization works.
  // private static final long serialVersionUID = -1227585031618624935L;

  private static final int INITIATE = 1;
  private static final int ACCEPT = 2;

  private int flags;

  /**
   * Create a new service permission with the indicated name and actions.
   *
   * The name is the name of the kerberos principal for the service.
   *
   * The actions are a comma-separated list of strings.  The recognized
   * actions are "initiate" and "accept".  The "initiate" action means
   * that the holder of the permission can access the service.  The
   * "accept" action means that the holder of the permission can operate
   * as this service.
   *
   * @param name the prinicpal's name
   * @param action the allowed actions
   */
  public ServicePermission(String name, String action)
  {
    super(name);
    parseActions(action);
  }

  public boolean implies(Permission perm)
  {
    if (! (perm instanceof ServicePermission))
      return false;
    ServicePermission sp = (ServicePermission) perm;
    if ((flags & sp.flags) != sp.flags)
      return false;
    return getName().equals(sp.getName());
  }

  public boolean equals(Object obj)
  {
    if (! (obj instanceof ServicePermission))
      return false;
    ServicePermission sp = (ServicePermission) obj;
    return flags == sp.flags && getName().equals(sp.getName());
  }

  public int hashCode()
  {
    return getName().hashCode() + flags;
  }

  /**
   * Return a string representing the actions.
   */
  public String getActions()
  {
    if (flags == (INITIATE | ACCEPT))
      return "initiate,accept";
    if (flags == INITIATE)
      return "initiate";
    if (flags == ACCEPT)
      return "accept";
    return "";
  }

  public PermissionCollection newPermissionCollection()
  {
    return new PermissionCollection()
    {
      private Vector permissions = new Vector();

      public void add(Permission perm)
      {
        if (isReadOnly())
          throw new SecurityException("readonly");
        if (! (perm instanceof ServicePermission))
          throw new IllegalArgumentException("can only add DelegationPermissions");
        permissions.add(perm);
      }

      public boolean implies(Permission perm)
      {
        if (! (perm instanceof ServicePermission))
          return false;
        Enumeration e = elements();
        while (e.hasMoreElements())
          {
            ServicePermission sp = (ServicePermission) e.nextElement();
            if (sp.implies(perm))
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

  private void parseActions(String actions)
  {
    StringTokenizer tok = new StringTokenizer(actions, ",");
    while (tok.hasMoreTokens())
      {
        String token = tok.nextToken();
        if ("accept".equals(token))
          flags |= ACCEPT;
        else if ("initiate".equals(token))
          flags |= INITIATE;
        else
          throw new IllegalArgumentException("unrecognized token: " + token);
      }
  }
}
