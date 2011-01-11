/* Group.java -- Represents a group of Principals
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
 * This interface represents a group of <code>Principals</code>.  Note that
 * since this interface extends <code>Principal</code>, a <code>Group</code>
 * can be used where ever a <code>Principal</code> is requested.  This
 * includes arguments to the methods in this interface.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Group extends Principal
{
  /**
   * This method adds a new <code>Principal</code> to this group.
   *
   * @param user The new <code>Principal</code> to add
   *
   * @return <code>true</code> if the user was successfully added or <code>false</code> if the user is already a member
   */
  boolean addMember(Principal user);

  /**
   * This method deletes a member from the group.
   *
   * @param user The <code>Principal</code> to delete
   *
   * @return <code>true</code> if the user was successfully deleted or <code>false</code> if the user is not a member of the group
   */
  boolean removeMember(Principal user);

  /**
   * This method tests whether or not a given <code>Principal</code> is a
   * member of this group.
   *
   * @param member The <code>Principal</code> to test for membership
   *
   * @return <code>true</code> if the user is member, <code>false</code> otherwise
   */
  boolean isMember(Principal member);

  /**
   * This method returns a list of all members of the group as an
   * <code>Enumeration</code>.
   *
   * @return The list of all members of the group
   */
  Enumeration<? extends Principal> members();
}
