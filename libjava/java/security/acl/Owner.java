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
  boolean addOwner(Principal caller, Principal owner) 
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
  boolean deleteOwner(Principal caller, Principal owner) 
    throws NotOwnerException, LastOwnerException;

  /**
   * This method tests whether or not a given <code>Principal</code> is an
   * owner of this access control list (ACL).
   *
   * @return <code>true</code> if the <code>Principal</code> is an owner, <code>false</code> otherwise
   */
  boolean isOwner(Principal owner);
}
