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
