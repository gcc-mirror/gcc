/* AccessControlException.java -- Permission is denied
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

package java.security;

/**
 * This exception is thrown when the <code>AccessController</code> denies
 * an attempt to perform an operation.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class AccessControlException extends SecurityException
{
  /**
   * The <code>Permission</code> associated with this exception
   */
  private Permission perm;

  /**
   * This method initializes a new instance of <code>AccessControlException</code>
   * with a descriptive error message.  There will be no <code>Permission</code>
   * object associated with this exception.
   *
   * @param msg The descriptive error message
   */
  public AccessControlException(String msg)
  {
    super(msg);
  }

  /**
   * This method initializes a new instance of <code>AccessControlException</code>
   * with a descriptive error message and an instance of <code>Permission</code>
   * that is the permission that caused the exception to be thrown.
   *
   * @param msg The descriptive error message
   * @param perm The <code>Permission</code> object that caused this exception.
   */
  public AccessControlException(String msg, Permission perm)
  {
    super(msg);
    this.perm = perm;
  }

  /**
   * This method returns the <code>Permission</code> object that caused
   * this exception to be thrown.
   *
   * @return The requested <code>Permission</code> object, or <code>null</code> if none is available.
   */
  public Permission getPermission()
  {
    return (perm);
  }
}
