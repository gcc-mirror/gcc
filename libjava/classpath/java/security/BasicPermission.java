/* BasicPermission.java -- implements a simple named permission
   Copyright (C) 1998, 1999, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.security;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * This class implements a simple model for named permissions without an
 * associated action list.  That is, either the named permission is granted
 * or it is not.
 *
 * <p>It also supports trailing wildcards to allow the easy granting of
 * permissions in a hierarchical fashion.  (For example, the name "org.gnu.*"
 * might grant all permissions under the "org.gnu" permissions hierarchy).
 * The only valid wildcard character is a '*' which matches anything. It
 * must be the rightmost element in the permission name and must follow a
 * '.' or else the Permission name must consist of only a '*'. Any other
 * occurrence of a '*' is not valid.
 *
 * <p>This class ignores the action list.  Subclasses can choose to implement
 * actions on top of this class if desired.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Permission
 * @see Permissions
 * @see PermissionCollection
 * @see RuntimePermission
 * @see SecurityPermission
 * @see PropertyPermission
 * @see AWTPermission
 * @see NetPermission
 * @see SecurityManager
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class BasicPermission extends java.security.Permission
  implements Serializable
  // FIXME extends with fully qualified classname as workaround for gcj 3.3.
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 6279438298436773498L;

  /**
   * Create a new instance with the specified permission name. If the
   * name is empty an exception is thrown.
   *
   * @param name the name of this permission
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException if name is invalid
   */
  public BasicPermission(String name)
  {
    super(name);

    // This routine used to check for illegal wildcards, but no such
    // requirement exists in the specification and Sun's runtime
    // doesn't appear to do it.

    if ("".equals(name))
      throw new IllegalArgumentException("Empty name");
  }

  /**
   * Create a new instance with the specified permission name. If the name
   * is empty, or contains an illegal wildcard character, an exception is
   * thrown. The actions parameter is ignored.
   *
   * @param name the name of this permission
   * @param actions ignored
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException if name is invalid
   */
  public BasicPermission(String name, String actions)
  {
    this(name);
  }

  /**
   * This method tests to see if the specified permission is implied by this
   * permission.  This will be true if the following conditions are met:<ul>
   * <li>The specified object is an instance of the same class as this
   * object.</li>
   * <li>The name of the specified permission is implied by this permission's
   * name based on wildcard matching. For example, "a.*" implies "a.b".</li>
   * </ul>
   *
   * @param perm the <code>Permission</code> object to test against
   * @return true if the specified permission is implied
   */
  public boolean implies(Permission perm)
  {
    if (! getClass().isInstance(perm))
      return false;

    String otherName = perm.getName();
    String name = getName();

    if (name.equals(otherName))
      return true;

    int last = name.length() - 1;
    return name.charAt(last) == '*'
      && otherName.startsWith(name.substring(0, last));
  }

  /**
   * This method tests to see if this object is equal to the specified
   * <code>Object</code>.  This will be true if and only if the specified
   * object meets the following conditions:<ul>
   * <li>It is an instance of the same class as this.</li>
   * <li>It has the same name as this permission.</li>
   * </ul>
   *
   * @param obj the <code>Object</code> to test for equality
   * @return true if obj is semantically equal to this
   */
  public boolean equals(Object obj)
  {
    return getClass().isInstance(obj)
      && getName().equals(((BasicPermission) obj).getName());
  }

  /**
   * This method returns a hash code for this permission object.  The hash
   * code returned is the value returned by calling the <code>hashCode</code>
   * method on the <code>String</code> that is the name of this permission.
   *
   * @return a hash value for this object
   */
  public int hashCode()
  {
    return getName().hashCode();
  }

  /**
   * This method returns a list of the actions associated with this
   * permission.  This method always returns the empty string ("") since
   * this class ignores actions.
   *
   * @return the action list
   */
  public String getActions()
  {
    return "";
  }

  /**
   * This method returns an instance of <code>PermissionCollection</code>
   * suitable for storing <code>BasicPermission</code> objects.  The
   * collection returned can only store objects of the same type as this.
   * Subclasses which use actions must override this method; but a class with
   * no actions will work fine with this.
   *
   * @return a new empty <code>PermissionCollection</code> object
   */
  public PermissionCollection newPermissionCollection()
  {
    return new BasicPermissionCollection(getClass());
  }

  /**
   * Implements AllPermission.newPermissionCollection, and obeys serialization
   * of JDK.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class BasicPermissionCollection extends PermissionCollection
  {
    /**
     * Compatible with JDK 1.1+.
     */
    private static final long serialVersionUID = 739301742472979399L;

    /**
     * The permissions in the collection.
     *
     * @serial a hash mapping name to permissions, all of type permClass
     */
    private final Hashtable permissions = new Hashtable();

    /**
     * If "*" is in the collection.
     *
     * @serial true if a permission named "*" is in the collection
     */
    private boolean all_allowed;

    /**
     * The runtime class which all entries in the table must belong to.
     *
     * @serial the limiting subclass of this collection
     */
    private final Class permClass;

    /**
     * Construct a collection over the given runtime class.
     *
     * @param c the class
     */
    BasicPermissionCollection(Class c)
    {
      permClass = c;
    }

    /**
     * Add a Permission. It must be of the same type as the permission which
     * created this collection.
     *
     * @param perm the permission to add
     * @throws IllegalArgumentException if perm is not the correct type
     * @throws SecurityException if the collection is read-only
     */
    public void add(Permission perm)
    {
      if (isReadOnly())
        throw new SecurityException("readonly");
      if (! permClass.isInstance(perm))
        throw new IllegalArgumentException("Expecting instance of " + permClass);
      BasicPermission bp = (BasicPermission) perm;
      String name = bp.getName();
      if (name.equals("*"))
        all_allowed = true;
      permissions.put(name, bp);
    }

    /**
     * Returns true if this collection implies the given permission.
     *
     * @param permission the permission to check
     * @return true if it is implied by this
     */
    public boolean implies(Permission permission)
    {
      if (! permClass.isInstance(permission))
        return false;
      if (all_allowed)
        return true;
      BasicPermission toImply = (BasicPermission) permission;
      String name = toImply.getName();
      if (name.equals("*"))
        return false;
      int prefixLength = name.length();
      if (name.endsWith("*"))
        prefixLength -= 2;

      while (true)
        {
          if (permissions.get(name) != null)
            return true;
          prefixLength = name.lastIndexOf('.', prefixLength);
          if (prefixLength < 0)
            return false;
          name = name.substring(0, prefixLength + 1) + '*';
        }
    }

    /**
     * Enumerate over the collection.
     *
     * @return an enumeration of the collection contents
     */
    public Enumeration elements()
    {
      return permissions.elements();
    }
  } // class BasicPermissionCollection
} // class BasicPermission
