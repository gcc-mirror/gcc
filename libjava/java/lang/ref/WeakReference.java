/* java.lang.ref.WeakReference
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.lang.ref;

/**
 * A weak reference will be cleared, if the object is only weakly
 * reachable.  It is useful for lookup tables, where you aren't
 * interested in an entry, if the key isn't reachable anymore.
 * <code>WeakHashtable</code> is a complete implementation of such a
 * table. <br>
 *
 * It is also useful to make objects unique: You create a set of weak
 * references to those objects, and when you create a new object you
 * look in this set, if the object already exists and return it.  If
 * an object is not referenced anymore, the reference will
 * automatically cleared, and you may remove it from the set. <br>
 *
 * @author Jochen Hoenicke 
 * @see java.util.WeakHashtable 
 */
public class WeakReference 
  extends Reference
{
  /**
   * Create a new weak reference, that is not registered to any queue.
   * @param referent the object we refer to.
   */
  public WeakReference(Object referent)
  {
    super(referent);
  }

  /**
   * Create a new weak reference.
   * @param referent the object we refer to.
   * @param q the reference queue to register on.
   * @exception NullPointerException if q is null.
   */
  public WeakReference(Object referent, ReferenceQueue q)
  {
    super(referent, q);
  }
}
