/* Permissions.java -- a collection of permission collections
   Copyright (C) 1998, 2001, 2002 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * This class is a heterogeneous collection of permissions.  It is
 * organized as a collection of <code>PermissionCollection</code>'s stored
 * in a hashtable.  Each individual <code>PermissionCollection</code>
 * contains permissions of a single type.  If a specific type of
 * <code>Permission</code> does not provide a collection type to use
 * via its <code>newPermissionCollection</code> method, then a default
 * collection type which stores its permissions in a hash table will be
 * used.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.1
 */
public final class Permissions extends PermissionCollection
  implements Serializable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 4858622370623524688L;

  /**
   * Holds instances of <code>AllPermission</code>.
   *
   * @serial the permission collection for AllPermission
   */
  private PermissionCollection allPermission;

  /**
   * This is the <code>Hashtable</code> that contains our collections.
   *
   * @serial maps Class to PermissionCollection
   */
  private final Hashtable perms = new Hashtable();

  /**
   * This method initializes a new instance of <code>Permissions</code>.
   */
  public Permissions()
  {
  }

  /**
   * This method adds a new <code>Permission</code> to this collection.  It
   * will be stored in a <code>PermissionCollection</code> of the appropriate
   * type, as determined by calling <code>newPermissionCollection</code> on
   * the specified permission (if an appropriate collection does not already
   * exist). If this object does not specify a particular type of collection,
   * a default collection, which stores in permissions in a hash table, will
   * be used.
   *
   * @param perm the <code>Permission</code> to add
   * @throws SecurityException if this collection is marked as read only
   */
  public void add(Permission perm)
  {
    if (isReadOnly())
      throw new SecurityException("PermissionCollection is read only");
    if (perm instanceof AllPermission)
      {
        if (allPermission == null)
          {
            allPermission = perm.newPermissionCollection();
            allPermission.add(perm);
            perms.put(perm.getClass(), allPermission);
          }
      }
    else
      {
        PermissionCollection pc
          = (PermissionCollection) perms.get(perm.getClass());
        if (pc == null)
          {
            pc = perm.newPermissionCollection();
            if (pc == null)
              pc = new PermissionsHash();
            perms.put(perm.getClass(), pc);
          }
        pc.add(perm);
      }
  }

  /**
   * This method tests whether or not the specified <code>Permission</code>
   * is implied by this <code>PermissionCollection</code>.
   *
   * @param perm the <code>Permission</code> to test
   * @return true if the specified permission is implied by this
   */
  public boolean implies(Permission perm)
  {
    if (allPermission != null)
      return true;
    PermissionCollection pc
      = (PermissionCollection) perms.get(perm.getClass());
    return pc == null ? false : pc.implies(perm);
  }

  /**
   * This method returns an <code>Enumeration</code> which contains a
   * list of all <code>Permission</code> objects contained in this
   * collection.
   *
   * @return an <code>Enumeration</code> of this collection's elements
   */
  public Enumeration elements()
  {
    return new Enumeration()
    {
      Enumeration main_enum = perms.elements();
      Enumeration sub_enum;

      public boolean hasMoreElements()
      {
        if (sub_enum == null)
          {
            if (main_enum == null)
              return false;
            if (! main_enum.hasMoreElements())
              {
                main_enum = null;
                return false;
              }
            PermissionCollection pc =
              (PermissionCollection) main_enum.nextElement();
            sub_enum = pc.elements();
          }
        if (! sub_enum.hasMoreElements())
          {
            sub_enum = null;
            return hasMoreElements();
          }
        return true;
      }

      public Object nextElement()
      {
        if (! hasMoreElements())
          throw new NoSuchElementException();
        return sub_enum.nextElement();
      }
    };
  }
} // class Permissions

/**
 * Implements the permission collection for all permissions without one of
 * their own, and obeys serialization of JDK.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 */
class PermissionsHash extends PermissionCollection
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -8491988220802933440L;

  /**
   * Hashtable where we store permissions.
   *
   * @serial the stored permissions, both as key and value
   */
  private final Hashtable perms = new Hashtable();

  /**
   * Add a permission. We don't need to check for read-only, as this
   * collection is never exposed outside of Permissions, which has already
   * done that check.
   *
   * @param perm the permission to add
   */
  public void add(Permission perm)
  {
    perms.put(perm, perm);
  }

  /**
   * Returns true if perm is in the collection.
   *
   * @param perm the permission to check
   * @return true if it is implied
   */
  public boolean implies(Permission perm)
  {
    return perms.get(perm) != null;
  }

  /**
   * Return the elements.
   *
   * @return the elements
   */
  public Enumeration elements()
  {
    return perms.elements();
  }
} // class Permissions
