/* Permission.java -- Information about an ACL permission
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

/**
 * This interface provides information about a permission that can be
 * granted.  Note that this is <em>not</em> the same as the class
 * <code>java.security.Permission</code>.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Permission
{
  /**
   * This method tests whether or not a specified <code>Permission</code>
   * (passed as an <code>Object</code>) is the same as this permission.
   *
   * @param perm The permission to check for equality
   *
   * @return <code>true</code> if the specified permission is the same as this one, <code>false</code> otherwise
   */
  public abstract boolean equals(Object perm);

  /**
   * This method returns this <code>Permission</code> as a <code>String</code>.
   *
   * @return A <code>String</code> representing this permission.
   */
  public String toString();
}
