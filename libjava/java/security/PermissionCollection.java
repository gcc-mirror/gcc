/* PermissionCollection.java -- A collection of permission objects
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
import java.util.Enumeration;

/**
 * This class models a group of Java permissions.  It has convenient
 * methods for determining whether or not a given permission is implied
 * by any of the permissions in this collection.
 *
 * <p>Some care must be taken in storing permissions.  First, a collection of
 * the appropriate type must be created.  This is done by calling the
 * <code>newPermissionCollection</code> method on an object of the
 * permission class you wish to add to the collection.  If this method
 * returns <code>null</code>, any type of <code>PermissionCollection</code>
 * can be used to store permissions of that type.  However, if a
 * <code>PermissionCollection</code> collection object is returned, that
 * type must be used.
 *
 * <p>A <code>PermissionCollection</code> returned by the
 * <code>newPermissionCollection</code> method in a subclass of
 * <code>Permission</code> is a homogeneous collection.  It only will
 * hold permissions of one specified type - instances of the class that
 * created it.  Not all <code>PermissionCollection</code> subclasses
 * have to hold permissions of only one type however.  For example,
 * the <code>Permissions</code> class holds permissions of many types.
 *
 * <p>Since the <code>newPermissionCollection</code> in <code>Permission</code>
 * itself returns <code>null</code>, by default a permission can be stored
 * in any type of collection unless it overrides that method to create its
 * own collection type.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Permission
 * @see Permissions
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class PermissionCollection implements Serializable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -6727011328946861783L;

  /**
   * Indicates whether or not this collection is read only.
   *
   * @serial if the collection is read-only
   */
  private boolean readOnly;

  /**
   * Create a new collection.
   */
  public PermissionCollection()
  {
  }

  /**
   * This method adds a new <code>Permission</code> object to the collection.
   *
   * @param perm the <code>Permission</code> to add
   *
   * @throws SecurityException if the collection is marked read only
   * @throws IllegalArgumentException if perm is of the wrong type
   */
  public abstract void add(Permission perm);

  /**
   * This method tests whether the specified <code>Permission</code> object is
   * implied by this collection of <code>Permission</code> objects.
   *
   * @param perm the <code>Permission</code> object to test
   * @return true if the collection implies perm
   */
  public abstract boolean implies(Permission perm);

  /**
   * This method returns an <code>Enumeration</code> of all the objects in
   * this collection.
   *
   * @return an <code>Enumeration</code> of this collection's objects
   */
  public abstract Enumeration elements();

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
   * This method tests whether or not this <code>PermissionCollection</code>
   * object is read only.
   *
   * @return true if this collection is read only
   */
  public boolean isReadOnly()
  {
    return readOnly;
  }

  /**
   * This method returns a <code>String</code> representation of this
   * collection.  It is formed by:
   * <pre>
   * super.toString()" (\n"
   *   // enumerate all permissions, one per line
   * ")\n"
   * </pre>
   *
   * @return a <code>String</code> representing this object
   */
  public String toString()
  {
    StringBuffer sb = new StringBuffer(super.toString());

    sb.append(" (\n");
    Enumeration e = elements();
    while (e.hasMoreElements())
      sb.append(' ').append(e.nextElement()).append('\n');
    return sb.append(")\n").toString();
  }
} // class PermissionCollection
