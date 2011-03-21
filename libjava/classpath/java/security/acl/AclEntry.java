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
  Principal getPrincipal();

  /**
   * This method sets ths <code>Principal</code> associated with this
   * ACL entry.  This operation will only succeed if there is not already
   * a <code>Principal</code> assigned.
   *
   * @param user The <code>Principal</code> for this ACL entry
   *
   * @return <code>true</code> if the <code>Principal</code> was successfully set or <code>false</code> if this entry already has a <code>Principal</code>.
   */
  boolean setPrincipal(Principal user);

  /**
   * This method sets this ACL entry to be a <em>negative</em> entry, indicating
   * that it contains a list of permissions that are <em>not</em> granted
   * to the entry's <code>Principal</code>.  Note that there is no way to
   * undo this operation.
   */
  void setNegativePermissions();

  /**
   * This method tests whether or not this ACL entry is a negative entry or not.
   *
   * @return <code>true</code> if this ACL entry is negative, <code>false</code> otherwise
   */
  boolean isNegative();

  /**
   * This method adds the specified permission to this ACL entry.
   *
   * @param permission The <code>Permission</code> to add
   *
   * @return <code>true</code> if the permission was added or <code>false</code> if it was already set for this entry
   */
  boolean addPermission(Permission permission);

  /**
   * This method deletes the specified permission to this ACL entry.
   *
   * @param perm The <code>Permission</code> to delete from this ACL entry.
   *
   * @return <code>true</code> if the permission was successfully deleted or <code>false</code> if the permission was not part of this ACL to begin with
   */
  boolean removePermission(Permission perm);

  /**
   * This method tests whether or not the specified permission is associated
   * with this ACL entry.
   *
   * @param permission The <code>Permission</code> to test
   *
   * @return <code>true</code> if this permission is associated with this entry or <code>false</code> otherwise
   */
  boolean checkPermission(Permission permission);

  /**
   * This method returns a list of all <code>Permission</code> objects
   * associated with this ACL entry as an <code>Enumeration</code>.
   *
   * @return A list of permissions for this ACL entry
   */
  Enumeration<Permission> permissions();

  /**
   * This method returns this object as a <code>String</code>.
   *
   * @return A <code>String</code> representation of this object
   */
  String toString();

  /**
   * This method returns a clone of this ACL entry
   *
   * @return A clone of this ACL entry
   */
  Object clone();
}
