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
