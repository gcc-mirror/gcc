/* AclEntry.java -- An entry in an ACL list.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.security.acl;

import java.security.Principal;
import java.util.Enumeration;

/**
 * This interface models an entry in an access control list (ACL).  Java
 * ACL's consist of a list of entries, where each consists of a 
 * <code>Principal</code> and a list of <code>Permission</code>'s which
 * have been granted to that <code>Principal</code>.  An ACL can also
 * be <em>negative</em>, which indicates that the list of 
 * <code>Permission</code>'s is a list of permissions that are <em>not</em>
 * granted to the <code>Principal</code>.  A <code>Principal</code> can
 * have at most one regular (or positive) ACL entry and one negative
 * ACL entry.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface AclEntry extends Cloneable
{
 /**
   * This method returns the <code>Principal</code> associated with this
   * ACL entry.
   *
   * @return The <code>Principal</code> for this ACL entry
   */
  public abstract Principal getPrincipal();

  /**
   * This method sets ths <code>Principal</code> associated with this
   * ACL entry.  This operation will only succeed if there is not already
   * a <code>Principal</code> assigned.
   *
   * @param user The <code>Principal</code> for this ACL entry
   *
   * @return <code>true</code> if the <code>Principal</code> was successfully set or <code>false</code> if this entry already has a <code>Principal</code>.
   */
  public abstract boolean setPrincipal(Principal user);

  /**
   * This method sets this ACL entry to be a <em>negative</em> entry, indicating
   * that it contains a list of permissions that are <em>not</em> granted
   * to the entry's <code>Principal</code>.  Note that there is no way to
   * undo this operation.
   */
  public abstract void setNegativePermissions();

  /**
   * This method tests whether or not this ACL entry is a negative entry or not.
   *
   * @return <code>true</code> if this ACL entry is negative, <code>false</code> otherwise
   */
  public abstract boolean isNegative();

  /**
   * This method adds the specified permission to this ACL entry.
   *
   * @param perm The <code>Permission</code> to add
   *
   * @return <code>true</code> if the permission was added or <code>false</code> if it was already set for this entry
   */
  public abstract boolean addPermission(Permission permission);

  /**
   * This method deletes the specified permission to this ACL entry.
   *
   * @param perm The <code>Permission</code> to delete from this ACL entry.
   *
   * @return <code>true</code> if the permission was successfully deleted or <code>false</code> if the permission was not part of this ACL to begin with
   */
  public abstract boolean removePermission(Permission perm);

  /**
   * This method tests whether or not the specified permission is associated
   * with this ACL entry.
   *
   * @param perm The <code>Permission</code> to test
   *
   * @return <code>true</code> if this permission is associated with this entry or <code>false</code> otherwise
   */
  public abstract boolean checkPermission(Permission permission);

  /**
   * This method returns a list of all <code>Permission</code> objects
   * associated with this ACL entry as an <code>Enumeration</code>.
   *
   * @return A list of permissions for this ACL entry
   */
  public abstract Enumeration permissions();

  /**
   * This method returns this object as a <code>String</code>.
   *
   * @return A <code>String</code> representation of this object
   */
  public abstract String toString();

  /**
   * This method returns a clone of this ACL entry
   *
   * @return A clone of this ACL entry
   */
  public abstract Object clone();
}
