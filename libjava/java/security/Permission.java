/* Permission.java -- The superclass for all permission objects
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

/**
 * This class is the abstract superclass of all classes that implement
 * the concept of a permission.  A permission consists of a permission name
 * and optionally a list of actions that relate to the permission.  The
 * actual meaning of the name of the permission is defined only in the
 * context of a subclass.  It may name a resource to which access permissions
 * are granted (for example, the name of a file) or it might represent
 * something else entirely.  Similarly, the action list only has meaning
 * within the context of a subclass.  Some permission names may have no
 * actions associated with them.  That is, you either have the permission
 * or you don't.
 *
 * <p>The most important method in this class is <code>implies</code>.  This
 * checks whether if one has this permission, then the specified
 * permission is also implied.  As a conceptual example, consider the
 * permissions "Read All Files" and "Read File foo".  The permission
 * "Read All Files" implies that the caller has permission to read the
 * file foo.
 *
 * <p><code>Permission</code>'s must be immutable - do not change their
 * state after creation.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @see Permissions
 * @see PermissionCollection
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class Permission implements Guard, Serializable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -5636570222231596674L;

  /**
   * This is the name assigned to this permission object.
   *
   * @serial the name of the permission
   */
  private String name;

  /**
   * Create an instance with the specified name.
   *
   * @param name the permission name
   */
  public Permission(String name)
  {
    this.name = name;
  }

  /**
   * This method implements the <code>Guard</code> interface for this class.
   * It calls the <code>checkPermission</code> method in
   * <code>SecurityManager</code> with this <code>Permission</code> as its
   * argument.  This method returns silently if the security check succeeds
   * or throws an exception if it fails.
   *
   * @param obj the <code>Object</code> being guarded - ignored by this class
   * @throws SecurityException if the security check fails
   * @see GuardedObject
   * @see SecurityManager#checkPermission(Permission)
   */
  public void checkGuard(Object obj)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(this);
  }

  /**
   * This method tests whether this <code>Permission</code> implies that the
   * specified <code>Permission</code> is also granted.
   *
   * @param perm the <code>Permission</code> to test against
   * @return true if perm is implied by this
   */
  public abstract boolean implies(Permission perm);

  /**
   * Check to see if this object equals obj. Use <code>implies</code>, rather
   * than <code>equals</code>, when making access control decisions.
   *
   * @param obj the object to compare to
   */
  public abstract boolean equals(Object obj);

  /**
   * This method returns a hash code for this <code>Permission</code>. It
   * must satisfy the contract of <code>Object.hashCode</code>: it must be
   * the same for all objects that equals considers to be the same.
   *
   * @return a hash value
   */
  public abstract int hashCode();

  /**
   * Get the name of this <code>Permission</code>.
   *
   * @return the name
   */
  public final String getName()
  {
    return name;
  }

  /**
   * This method returns the list of actions for this <code>Permission</code>
   * as a <code>String</code>. The string should be in canonical order, for
   * example, both <code>new FilePermission(f, "write,read")</code> and
   * <code>new FilePermission(f, "read,write")</code> have the action list
   * "read,write".
   *
   * @return the action list for this <code>Permission</code>
   */
  public abstract String getActions();

  /**
   * This method returns an empty <code>PermissionCollection</code> object
   * that can store permissions of this type, or <code>null</code> if no
   * such collection is defined. Subclasses must override this to provide
   * an appropriate collection when one is needed to accurately calculate
   * <code>implies</code>.
   *
   * @return a new <code>PermissionCollection</code>
   */
  public PermissionCollection newPermissionCollection()
  {
    return null;
  }

  /**
   * This method returns a <code>String</code> representation of this
   * <code>Permission</code> object. This is in the format:
   * <code>'(' + getClass().getName() + ' ' + getName() + ' ' + getActions
   * + ')'</code>.
   *
   * @return this object as a <code>String</code>
   */
  public String toString()
  {
    return '(' + getClass().getName() + ' ' + getName() + ' '
      + getActions() + ')';
  }
} // class Permission
