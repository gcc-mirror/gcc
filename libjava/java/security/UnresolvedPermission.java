/* UnresolvedPermission.java -- Placeholder for unresolved permissions
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

// All uses of Certificate in this file refer to the one in the listed
// package, not this one.
import java.security.cert.Certificate;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.NoSuchElementException;
import java.util.Vector;

/**
 * This class is used to hold instances of all permissions that cannot
 * be resolved to available permission classes when the security
 * <code>Policy</code> object is instantiated.  This may happen when the
 * necessary security class has not yet been downloaded from the network.
 *
 * <p>Instances of this class are re-resolved when
 * <code>AccessController</code> check is done.  At that time, a scan is
 * made of all existing <code>UnresolvedPermission</code> objects and they
 * are converted to objects of the appropriate permission type if the class
 * for that type is then available.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see Permission
 * @see Permissions
 * @see PermissionCollection
 * @see Policy
 * @since 1.1
 * @status updated to 1.4
 */
public final class UnresolvedPermission extends Permission
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -4821973115467008846L;

  /**
   * The list of actions associated with this permission object.
   *
   * @serial the permission actions
   */
  private final String actions;

  /**
   * The list of <code>Certificates</code> associated with this object.
   */
  private final transient Certificate[] certs;

  /**
   * The name of the class this object should be resolved to.
   *
   * @serial the fully-qualified classname of the resolved type
   */
  // Package visible for use by UnresolvedPermissionCollection.
  final String type;

  /**
   * The name of the permission.
   *
   * @serial the permission name
   */
  private final String name;

  /**
   * Create a new instance with all the information necessary to resolve it
   * to an instance of the proper class at a future time.
   *
   * @param type the fully-qualified name of the class of this permission
   * @param name the name of this permission
   * @param actions the action list for this permission
   * @param certs the list of certificates that sign this permission
   */
  public UnresolvedPermission(String type, String name, String actions,
                              Certificate[] certs)
  {
    super(name);
    this.name = name;
    this.type = type;
    this.actions = actions;
    this.certs = certs;
  }

  /**
   * This method returns <code>false</code> always to indicate that this
   * permission does not imply the specified permission.  An
   * <code>UnresolvedPermission</code> never grants any permissions.
   *
   * @param perm the <code>Permission</code> object to test
   * @return false; until a permission is resolved, it implies nothing
   */
  public boolean implies(Permission perm)
  {
    return false;
  }

  /**
   * This method tests this permission for equality against the specified
   * <code>Object</code>. This will be true if and only if the following
   * conditions are met:<ul>
   * <li>The specified <code>Object</code> is an UnresolvedPermission</li>
   * <li>The specified permission has the same type (i.e., desired class name)
   *     as this permission.</li>
   * <li>The specified permission has the same name as this one.</li>
   * <li>The specified permissoin has the same action list as this one.</li>
   * <li>The specified permission has the same certificate list as this
   *     one.</li>
   * </ul>
   *
   * @param obj the <code>Object</code> to test for equality
   * @return true if the specified object is equal to this one
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof UnresolvedPermission))
      return (false);
    UnresolvedPermission up = (UnresolvedPermission) obj;
    return up.name.equals(name) && up.actions.equals(actions)
      && up.type.equals(type) && Arrays.equals(up.certs, certs);
  }

  /**
   * Returns a hash code value for this object. Following the lead of
   * Permission, this returns the hashcode of the permission name.
   *
   * @return A hash value
   */
  public int hashCode()
  {
    return name.hashCode();
  }

  /**
   * This method returns the list of actions associated with this
   * permission.
   *
   * @return the action list
   */
  public String getActions()
  {
    return actions;
  }

  /**
   * This method returns a <code>String</code> representation of this
   * class.  The format is: '(unresolved "ClassName "name" "actions")'
   *
   * @return  <code>String</code> representation of this object
   */
  public String toString()
  {
    return "(unresolved " + type + ' ' + name + ' ' + actions + ')';
  }

  /**
   * This class returns a <code>PermissionCollection</code> object that can
   * be used to store instances of <code>UnresolvedPermission</code>.
   *
   * @return a new <code>PermissionCollection</code>
   */
  public PermissionCollection newPermissionCollection()
  {
    return new UnresolvedPermissionCollection();
  }
} // class UnresolvedPermission

/**
 * Implements the permission collection for unresolved permissions, and
 * obeys serialization of JDK.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 */
class UnresolvedPermissionCollection extends PermissionCollection
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -7176153071733132400L;

  // Package-private to avoid a trampoline.
  /**
   * Hashtable where we store permissions.
   *
   * @serial map of typename to a Vector of permissions (you'd think Sun
   *         would document this better!)
   */
  final Hashtable permissions = new Hashtable();

  /**
   * Add a permission.
   *
   * @param perm the permission to add
   * @throws IllegalArgumentException if perm is not an UnresolvedPermission
   * @throws SecurityException if the collection is read-only
   */
  public void add(Permission perm)
  {
    if (isReadOnly())
      throw new SecurityException();
    if (! (perm instanceof UnresolvedPermission))
      throw new IllegalArgumentException();
    UnresolvedPermission up = (UnresolvedPermission) perm;
    Vector v = (Vector) permissions.get(up.type);
    if (v == null)
      {
        v = new Vector();
        permissions.put(up.type, v);
      }
    v.add(up);
  }

  /**
   * Returns true if perm is implied by the collection.
   *
   * @param perm the permission to check
   * @return false; unresolved permissions imply nothing
   */
  public boolean implies(Permission perm)
  {
    return false;
  }

  /**
   * Return the elements.
   *
   * @return the elements
   */
  public Enumeration elements()
  {
    return new Enumeration()
    {
      Enumeration main_enum = permissions.elements();
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
            Vector v = (Vector) main_enum.nextElement();
            sub_enum = v.elements();
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
} // class UnresolvedPermissionCollection
