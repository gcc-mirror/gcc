/* LockInfo.java - Information on a lock.
   Copyright (C) 2006 Free Software Foundation

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

package java.lang.management;

import java.beans.ConstructorProperties;

/**
 * Provides information on a lock held by a thread.
 * A lock can be either a built-in monitor, an
 * <emph>ownable synchronizer</emph> (i.e. a subclass
 * of {@link java.util.concurrent.locks.AbstractOwnableSynchronizer}),
 * or a {@link java.util.concurrent.locks.Condition}
 * object.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public class LockInfo
{

  /**
   * The class name of the lock object.
   */
  private String className;

  /**
   * The identity hash code of the lock object.
   */
  private int identityHashCode;

  /**
   * Constructs a new {@link LockInfo} object with the
   * specified class name and identity hash code.
   *
   * @param className the name of the class of the lock object.
   * @param identityHashCode the identity hash code of the
   *                         lock object.
   */
  @ConstructorProperties({"className","identityHashCode"})
    public LockInfo(String className, int identityHashCode)
  {
    this.className = className;
    this.identityHashCode = identityHashCode;
  }

  /**
   * Returns the class name of the lock object.
   *
   * @return the class name of the lock object.
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * Returns the identity hash code of the lock object.
   *
   * @return the identity hash code of the lock object.
   */
  public int getIdentityHashCode()
  {
    return identityHashCode;
  }

  /**
   * Returns a textual representation of the lock,
   * constructed by concatenating the class name,
   * <code>'@'</code> and the identity hash code
   * in unsigned hexadecimal form.
   *
   * @return a textual representation of the lock.
   */
  public String toString()
  {
    return className + '@' + Integer.toHexString(identityHashCode);
  }

}
