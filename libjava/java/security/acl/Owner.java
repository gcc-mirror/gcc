/* Owner.java -- ACL owner
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

/**
 * This interface provides a mechanism for maintaining a list of owners
 * of an access control list (ACL).  Since a <code>Principal</code> must
 * be an owner in order to modify the owner list, a mechanism must be
 * provided to specify the initial owner of the ACL.  The proper way to do
 * this is for the implementing class to specify the initial owner in
 * the contructor for that class.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Owner
{
  /**
   * This method adds an owner to the access control list (ACL).  Only a
   * <code>Principal</code> who is already an owner can perform this operation.
   *
   * @param caller The <code>Principal</code> who is requesting that an owner be added
   * @param owner The <code>Principal</code> to add as a new owner
   *
   * @param <code>true</code> if the new owner was successfully added or <code>false</code> if the specified new owner is already an owner
   *
   * @exception NotOwnerException If the caller is not already an owner of this ACL
   */
  public abstract boolean addOwner(Principal caller, Principal owner) 
    throws NotOwnerException;

  /**
   * This method delets an owner from the access control list (ACL).  Only a
   * <code>Principal</code> who is an owner can perform this operation.  An
   * owner can delete itself from the list.  If there is only one
   * owner remaining on this list, any attempt to delete it will throw an
   * exception.
   *
   * @param caller The <code>Principal</code> who is requesting that an owner be deleted
   * @param owner The <code>Principal</code> to delete as an owner
   *
   * @param <code>true</code> if the new owner was successfully deleted or <code>false</code> if the specified owner is not currently an owner
   *
   * @exception NotOwnerException If the caller is not already an owner of this ACL
   * @exception LastOwnerException If completing the operation would delete the last ACL owner
   */
  public abstract boolean deleteOwner(Principal caller, Principal owner) 
    throws NotOwnerException, LastOwnerException;

  /**
   * This method tests whether or not a given <code>Principal</code> is an
   * owner of this access control list (ACL).
   *
   * @return <code>true</code> if the <code>Principal</code> is an owner, <code>false</code> otherwise
   */
  public abstract boolean isOwner(Principal owner);
}
