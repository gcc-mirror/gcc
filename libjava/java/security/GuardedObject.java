/* GuardedObject.java -- An object protected by a Guard
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

import java.io.Serializable;

/**
 * This class is an object that is guarded by a <code>Guard</code> object.
 * The object that is being guarded is retrieved by a call to the only 
 * method in this class - <code>getObject</code>.  That method returns the
 * guarded <code>Object</code> after first checking with the 
 * <code>Guard</code>.  If the <code>Guard</code> disallows access, an
 * exception will be thrown.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class GuardedObject implements Serializable
{
  /**
   * This is the Guard that is protecting the object.
   */
  private Guard guard;

  /**
   * This is the object that is being guarded.
   */
  private Object object;

  /**
   * This method initializes a new instance of <code>GuardedObject</code>
   * that protects the specified <code>Object</code> using the specified
   * <code>Guard</code>
   *
   * @param object The <code>Object</code> to guard
   * @param guard The <code>Guard</code> that is protecting the object.
   */
  public GuardedObject(Object object, Guard guard)
  {
    this.object = object;
    this.guard = guard;
  }

  /**
   * This method first call the <code>checkGuard</code> method on the 
   * <code>Guard</code> object protecting the guarded object.  If the 
   * <code>Guard</code> disallows access, an exception is thrown, otherwise
   * the <code>Object</code> is returned.
   *
   * @return The object being guarded
   *
   * @exception SecurityException If the <code>Guard</code> disallows access to the object.
   */
  public Object getObject() throws SecurityException
  {
    guard.checkGuard(object);
    return (object);
  }
}
