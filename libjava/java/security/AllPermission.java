/* AllPermission.java -- Permission to do anything
   Copyright (C) 1998, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.security;

import gnu.java.util.EmptyEnumeration;

import java.util.Collections;
import java.util.Enumeration;

/**
 * This class is a permission that implies all other permissions.  Granting
 * this permission effectively grants all others.  Extreme caution should
 * be exercised in granting this permission.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see AccessController
 * @see Permissions
 * @see SecurityManager
 * @since 1.1
 * @status updated to 1.4
 */
public final class AllPermission extends Permission
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -2916474571451318075L;

  /**
   * Create a new AllPermission object.
   */
  public AllPermission()
  {
    super("*");
  }

  /**
   * Create a new AllPermission object. The parameters are ignored, as all
   * permission implies ALL PERMISSION.
   *
   * @param name ignored
   * @param actions ignored
   */
  public AllPermission(String name, String actions)
  {
    super("*");
  }

  /**
   * This method always returns <code>true</code> to indicate that this
   * permission always implies that any other permission is also granted.
   *
   * @param perm ignored
   * @return true, the permission is implied
   */
  public boolean implies(Permission perm)
  {
    return true;
  }

  /**
   * Checks an object for equality. All AllPermissions are equal.
   *
   * @param obj the <code>Object</code> to test for equality
   */
  public boolean equals(Object obj)
  {
    return obj instanceof AllPermission;
  }

  /**
   * This method returns a hash code for this object. This returns 1.
   *
   * @return a hash value for this object
   */
  public int hashCode()
  {
    return 1;
  }

  /**
   * This method returns the list of actions associated with this object.
   * This will always be the empty string ("") for this class.
   *
   * @return the action list
   */
  public String getActions()
  {
    return "";
  }

  /**
   * Returns a PermissionCollection which can hold AllPermission.
   *
   * @return a permission collection
   */
  public PermissionCollection newPermissionCollection()
  {
    return new AllPermissionCollection();
  }

  /**
   * Implements AllPermission.newPermissionCollection, and obeys serialization
   * of JDK.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class AllPermissionCollection extends PermissionCollection
  {
    /**
     * Compatible with JDK 1.1+.
     */
    private static final long serialVersionUID = -4023755556366636806L;

    /**
     * Whether an AllPermission has been added to the collection.
     *
     * @serial if all permission is in the collection yet
     */
    private boolean all_allowed;

    /**
     * Add an AllPermission.
     *
     * @param perm the permission to add
     * @throws IllegalArgumentException if perm is not an AllPermission
     * @throws SecurityException if the collection is read-only
     */
    public void add(Permission perm)
    {
      if (isReadOnly())
        throw new SecurityException();
      if (! (perm instanceof AllPermission))
        throw new IllegalArgumentException();
      all_allowed = true;
    }

    /**
     * Returns true if this collection implies a permission.
     *
     * @param perm the permission to check
     * @return true if this collection contains an AllPermission
     */
    public boolean implies(Permission perm)
    {
      return all_allowed;
    }

    /**
     * Returns an enumeration of the elements in the collection.
     *
     * @return the elements in the collection
     */
    public Enumeration elements()
    {
      return all_allowed
        ? Collections.enumeration(Collections.singleton(new AllPermission()))
        : EmptyEnumeration.getInstance();
    }
  } // class AllPermissionCollection
} // class AllPermission
