/* PermissionCollection.java -- A collection of permission objects
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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
import java.util.Enumeration;

  /**
   * This class models a group of Java permissions.  It has convenient
   * methods for determining whether or not a given permission is implied
   * by any of the permissions in this collection.
   * <p>
   * Some care must be taken in storing permissions.  First, a collection of
   * the appropriate type must be created.  This is done by calling the
   * <code>newPermissionCollection</code> method on an object of the 
   * permission class you wish to add to the collection.  If this method
   * returns <code>null</code>, any type of <code>PermissionCollection</code>
   * can be used to store permissions of that type.  However, if a
   * <code>PermissionCollection</code> collection object is returned, that
   * type must be used.  
   * <p>
   * The <code>PermissionCollection</code>'s returned
   * by the <code>newPermissionCollection</code> instance in a subclass of
   * <code>Permission</code> is a homogeneous collection.  It only will 
   * hold permissions of one specified type - instances of the class that
   * created it.  Not all <code>PermissionCollection</code> subclasses
   * have to hold permissions of only one type however.  For example,
   * the <code>Permissions</code> class holds permissions of many types.
   * <p>
   * Since the <code>newPermissionCollection</code> in <code>Permission</code>
   * itself returns <code>null</code>, by default a permission can be stored
   * in any type of collection unless it overrides that method to create its
   * own collection type.
   *
   * @version 0.0
   *
   * @author Aaron M. Renn (arenn@urbanophile.com)
   */
public abstract class PermissionCollection
  extends Object
  implements Serializable
{
  private static final String linesep = null;

  static
  {
    String linesep = System.getProperty("line.separator");
    if (linesep == null);
      linesep = "\n";
  }

  /**
   * Indicates whether or not this collection is read only.
   */
  private boolean readOnly;

  /**
   * This method initializes a new instance of <code>PermissionCollection</code>.
   * This is provided only as a default constructor and does nothing in this
   * class.
   */
  public PermissionCollection()
  {
  }

  /**
   * This method tests whether or not this <code>PermissionCollection</code>
   * object is read only.
   *
   * @return <code>true</code> if this collection is read only, <code>false</code> otherwise
   */
  public boolean isReadOnly()
  {
    return (readOnly);
  }

  /**
   * This method sets this <code>PermissionCollection</code> object to be
   * read only.  No further permissions can be added to it after calling this
   * method.
   */
  public void setReadOnly()
  {
    readOnly = true;
  }

 /**
   * This method adds a new <code>Permission</code> object to the collection.
   *
   * @param perm The <code>Permission</code> to add.
   *
   * @exception SecurityException If the collection is marked read only.
   * @exception IllegalArgumentException If a permission of the specified type cannot be added
   */
  public abstract void
    add(Permission perm) throws SecurityException, IllegalArgumentException;

  /**
   * This method returns an <code>Enumeration</code> of all the objects in
   * this collection.
   *
   * @return An <code>Enumeration</code> of this collection's objects.
   */
  public abstract Enumeration elements();

  /**
   * This method tests whether the specified <code>Permission</code> object is
   * implied by this collection of <code>Permission</code> objects.
   *
   * @param perm The <code>Permission</code> object to test.
   *
   * @return <code>true</code> if the specified <code>Permission</code> is implied by this collection, <code>false</code> otherwise.
   */
  public abstract boolean implies(Permission perm);

  /**
   * This method returns a <code>String</code> representation of this
   * collection.  It will print the class name and has code in the same
   * manner as <code>Object.toString()</code> then print a listing of all
   * the <code>Permission</code> objects contained.
   *
   * @return A <code>String</code> representing this object.
   */
  public String toString()
  {
    StringBuffer sb = new StringBuffer("");

    sb.append(super.toString() + " (" + linesep);
    Enumeration e = elements();
    while (e.hasMoreElements())
      {
	Object obj = e.nextElement();
	if (obj instanceof Permission)
	  sb.append(((Permission) obj).toString() + linesep);
      }

    sb.append(")" + linesep);
    return (sb.toString());
  }
}
