/* Acl.java -- An access control list
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
 * A Java access control list (ACL) is a group of individual ACL entries.
 * These entries consist of a <code>Principal</code> and a list of
 * permissions this <code>Principal</code> is either granted or denied.
 * A given <code>Principal</code> can have at most one positive ACL entry
 * (i.e., one that grants permissions) and one negative ACL entry (i.e., one
 * that denies permissions).  If a given permission is both granted and
 * denied, the ACL treats it as if it were never granted or denied.  If
 * both a <code>Principal</code> and a <code>Group</code> to which the
 * <code>Principal</code> belongs have an ACL entry, the permissions for
 * the individual <code>Principal</code> take precedence over the 
 * permissions of the <code>Group</code> if there is a conflict.
 * <p
 * Additionally, the ACL interface extends the <code>Owner</code> interface
 * and so an ACL has owners.  Actions which modify the ACL are restricted
 * to owners.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Acl extends Owner
{

  /**
   * This method returns the name of this ACL.
   *
   * @return The name of this ACL
   */
  public abstract String getName();

  /**
   * This method sets the name of the ACL
   *
   * @param caller The <code>Principal</code> requesting the action.
   * @param name The new name for this ACL.
   *
   * @exception NotOwnerException If the caller is not an owner of this ACL.
   */
  public abstract void setName(Principal caller, String name)
    throws NotOwnerException;

  /**
   * This method adds the specified entry to the ACL
   *
   * @param caller The <code>Principal</code> requesting the addition
   * @param entry The ACL entry to add
   *
   * @return <code>true</code> if the entry was added, <code>false</code> if there is already an entry of the same type for the <code>Principal</code>.
   *
   * @exception NotOwnerException If the caller is not an owner of this ACL.
   */
  public abstract boolean addEntry(Principal caller, AclEntry entry) 
    throws NotOwnerException;

  /**
   * This method delets the specified entry from the ACL
   *
   * @param caller The <code>Principal</code> requesting the deletion.
   * @param entry The ACL entry to delete
   *
   * @return <code>true</code> if the entry was deleted, or <code>false</code> if this entry was not part of the ACL to begin with
   *
   * @exception NotOwnerException If the caller is not an owner of this ACL.
   */
  public abstract boolean removeEntry(Principal caller, AclEntry entry)
    throws NotOwnerException;

  /**
   * This method returns a list of all the entries in the ACL as an
   * <code>Enumeration</code>.
   *
   * @return An enumeration of the ACL entries
   */
  public abstract Enumeration entries();

  /**
   * This method tests whether or not the specified <code>Principal</code>
   * has the specified <code>Permission</code>
   *
   * @param user The <code>Principal</code> to test
   * @param perm The <code>Permission</code> to test for
   *
   * @return <code>true</code> if the user has been granted the permission, <code>false</code> otherwise
   */
  public abstract boolean checkPermission(Principal user, Permission perm);

  /**
   * This method returns a list of <code>Permission</code>'s that are granted
   * to a particular <code>Principal</code>.  This includes any permissions
   * that are granted to <code>Group</code>'s to which the <code>Principal</code>
   * belongs unless they are overridden by a negative ACL.  This permission
   * list is returned as an <code>Enumeration</code>.
   *
   * @param user The <code>Principal</code> to retrieve permissions for.
   *
   * @return A list of permissions for the <code>Principal</code>.
   */
  public abstract Enumeration getPermissions(Principal user);

  /**
   * This method returns the ACL as a <code>String</code>
   *
   * @return A <code>String</code> representation of this ACL
   */
  public abstract String toString();
}
