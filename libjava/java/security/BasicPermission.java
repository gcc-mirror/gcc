/* BasicPermission.java -- Implements a simple named permission.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/**
 * This class implements a simple model for named permissions without an
 * associated action list.  That is, either the named permission is granted
 * or it is not.  
 * <p>
 * It also supports trailing wildcards to allow the
 * easy granting of permissions in a hierarchical fashion.  (For example,
 * the name "org.gnu.*" might grant all permissions under the "org.gnu"
 * permissions hierarchy).  The only valid wildcard character is a '*'
 * which matches anything.  It must be the rightmost element in the
 * permission name and must follow a '.' or else the Permission name must
 * consist of only a '*'.  Any other occurrence of a '*' is not valid.
 * <p>
 * This class ignores the action list.  Subclasses can choose to implement
 * actions on top of this class if desired.
 *
 * @version 0.1
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public abstract class BasicPermission extends java.security.Permission
  implements Serializable
  // FIXME extends with fully qualified classname as workaround for gcj 3.0.4
{
  /**
   * This method initializes a new instance of <code>BasicPermission</code>
   * with the specified name.  If the name contains an illegal wildcard
   * character, an exception is thrown.
   *
   * @param name The name of this permission.
   *
   * @exception IllegalArgumentException If the name contains an invalid wildcard character
   * @exception NullPointerException If the name is null
   */
  public BasicPermission(String name) 
    throws IllegalArgumentException, NullPointerException
  {
    super(name);

    if (name.indexOf("*") != -1)
      {
	if (!name.endsWith(".*") && !name.equals("*"))
	  throw new IllegalArgumentException("Bad wildcard: " + name);

	if (name.indexOf("*") != name.lastIndexOf("*"))
	  throw new IllegalArgumentException("Bad wildcard: " + name);
      }
  }

  /**
   * This method initializes a new instance of <code>BasicPermission</code>
   * with the specified name.  If the name contains an illegal wildcard
   * character, an exception is thrown.  The action list passed to this
   * form of the constructor is ignored.
   *
   * @param name The name of this permission.
   * @param actions The list of actions for this permission - ignored in this class.
   *
   * @exception IllegalArgumentException If the name contains an invalid wildcard character
   * @exception NullPointerException If the name is null
   */
  public BasicPermission(String name, String actions) 
    throws IllegalArgumentException, NullPointerException
  {
    // ignore actions
    this(name);
  }

  /**
   * This method tests to see if the specified permission is implied by 
   * this permission.  This will be true if the following conditions are met:
   * <p>
   * <ul>
   * <li>The specified object is an instance of <code>BasicPermission</code>, 
   * or a subclass.
   * <li>The name of the specified permission is identical to this permission's
   * name or the name of the specified permission satisfies a wildcard match 
   * on this permission.
   * </ul>
   *
   * @param perm The <code>Permission</code> object to test against.
   *
   * @return <code>true</code> if the specified permission is implied by this one or <code>false</code> otherwise.
   */
  public boolean implies(Permission perm)
  {
    if (!(perm instanceof BasicPermission))
      return false;

    String otherName = perm.getName();
    String name = getName();

    if (name.equals(otherName))
      return true;

    int last = name.length() - 1;
    if (name.charAt(last) == '*'
	&& otherName.startsWith(name.substring(0, last)))
      return true;

    return false;
  }

  /**
   * This method tests to see if this object is equal to the specified
   * <code>Object</code>.  This will be true if and only if the specified
   * object meets the following conditions:
   * <p>
   * <ul>
   * <li>It is an instance of <code>BasicPermission</code>, or a subclass.
   * <li>It has the same name as this permission.
   * </ul>
   *
   * @param obj The <code>Object</code> to test for equality against this object
   *
   * @return <code>true</code> if the specified <code>Object</code> is equal to this object or <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof BasicPermission))
      return (false);

    if (!getName().equals(((BasicPermission) obj).getName()))
      return (false);

    return (true);
  }

  /**
   * This method returns a hash code for this permission object.  The hash
   * code returned is the value returned by calling the <code>hashCode</code>
   * method on the <code>String</code> that is the name of this permission.
   *
   * @return A hash value for this object
   */
  public int hashCode()
  {
    return (getName().hashCode());
  }

  /**
   * This method returns a list of the actions associated with this 
   * permission.  This method always returns the empty string ("") since
   * this class ignores actions.
   *
   * @return The action list.
   */
  public String getActions()
  {
    return ("");
  }

  /**
   * This method returns an instance of <code>PermissionCollection</code>
   * suitable for storing <code>BasicPermission</code> objects.  This returns
   * be a sub class of <code>PermissionCollection</code>
   * that allows for an efficient and consistent implementation of
   * the <code>implies</code> method.  The collection doesn't handle subclasses
   * of BasicPermission correctly; they must override this method. 
   *
   * @return A new empty <code>PermissionCollection</code> object.
   */
  public PermissionCollection newPermissionCollection()
  {
    return new PermissionCollection()
    {
      Hashtable permissions = new Hashtable();
      boolean allAllowed = false;

      public void add(Permission permission)
      {
	if (isReadOnly())
	  throw new IllegalStateException("readonly");

	BasicPermission bp = (BasicPermission) permission;
	String name = bp.getName();
	if (name.equals("*"))
	  allAllowed = true;
	permissions.put(name, bp);
      }

      public boolean implies(Permission permission)
      {
	if (!(permission instanceof BasicPermission))
	  return false;

	if (allAllowed)
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

      public Enumeration elements()
      {
	return permissions.elements();
      }
    };
  }
}
