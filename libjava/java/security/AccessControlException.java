/* AccessControlException.java -- Permission is denied
   Copyright (C) 1998, 2002, 2005  Free Software Foundation, Inc.

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
 * an attempt to perform an operation. This often keeps track of the
 * permission that was not granted.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see AccessController
 * @status updated to 1.4
 */
public class AccessControlException extends SecurityException
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 5138225684096988535L;

  /**
   * The <code>Permission</code> associated with this exception.
   *
   * @serial the permission
   */
  private final Permission perm;

  /**
   * Create a new instance with a descriptive error message, and a null
   * <code>Permission</code> object.
   *
   * @param msg the descriptive error message
   */
  public AccessControlException(String msg)
  {
    this(msg, null);
  }

  /**
   * Create a new instance with a descriptive error message and an associated
   * <code>Permission</code> object.
   *
   * @param msg the descriptive error message
   * @param perm the permission that caused this
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
   * @return the denied permission, or null
   */
  public Permission getPermission()
  {
    return perm;
  }
}
