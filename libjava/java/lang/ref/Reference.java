/* java.lang.ref.Reference
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
 * This is the base class of all references.  A reference allows
 * refering to an object without preventing the garbage collection to
 * collect it.  The only way to get the referred object is via the
 * <code>get()</code>-method.  This method will return
 * <code>null</code> if the object was collected. <br>
 *
 * A reference may be registered with a queue.  When a referred
 * element gets collected the reference will be put on the queue, so
 * that you will be notified. <br>
 *
 * There are currently three types of references:  soft reference,
 * weak reference and phantom reference. <br>
 *
 * Soft references will be cleared if the garbage collection is told
 * to free some memory and there are no unreferenced or weakly referenced
 * objects.  It is useful for caches. <br>
 *
 * Weak references will be cleared as soon as the garbage collection
 * determines that the refered object is only weakly reachable.  They
 * are useful as keys in hashtables (see <code>WeakHashtable</code>) as
 * you get notified when nobody has the key anymore.
 *
 * Phantom references don't prevent finalization.  If an object is only
 * phantom reachable, it will be finalized, and the reference will be
 * enqueued, but not cleared.  Since you mustn't access an finalized
 * object, the <code>get</code> method of a phantom reference will never
 * work.  It is useful to keep track, when an object is finalized.
 *
 * @author Jochen Hoenicke
 * @see java.util.WeakHashtable
 */
public abstract class Reference
{
  /**
   * The underlying object.  This field is handled in a special way by
   * the garbage collection.
   */
  Object referent;

  /**
   * The queue this reference is registered on. This is null, if this
   * wasn't registered to any queue or reference was already enqueued.
   */
  ReferenceQueue queue;

  /**
   * Link to the next entry on the queue.  If this is null, this
   * reference is not enqueued.  Otherwise it points to the next
   * reference.  The last reference on a queue will point to itself
   * (not to null, that value is used to mark a not enqueued
   * reference).  
   */
  Reference nextOnQueue;

  /**
   * This lock should be taken by the garbage collection, before
   * determining reachability.  It will prevent the get()-method to
   * return the reference so that reachability doesn't change.
   */
  static Object lock = new Object();

  /**
   * Creates a new reference that is not registered to any queue.
   * Since it is package private, it is not possible to overload this
   * class in a different package.  
   * @param referent the object we refer to.
   */
  Reference(Object ref)
  {
    referent = ref;
  }

  /**
   * Creates a reference that is registered to a queue.  Since this is
   * package private, it is not possible to overload this class in a
   * different package.  
   * @param referent the object we refer to.
   * @param q the reference queue to register on.
   * @exception NullPointerException if q is null.
   */
  Reference(Object ref, ReferenceQueue q)
  {
    if (q == null)
      throw new NullPointerException();
    referent = ref;
    queue = q;
  }

  /**
   * Returns the object, this reference refers to.
   * @return the object, this reference refers to, or null if the 
   * reference was cleared.
   */
  public Object get()
  {
    synchronized(lock)
      {
	return referent;
      }
  }

  /**
   * Clears the reference, so that it doesn't refer to its object
   * anymore.  For soft and weak references this is called by the
   * garbage collection.  For phantom references you should call 
   * this when enqueuing the reference.
   */
  public void clear()
  {
    referent = null;
  }

  /**
   * Tells if the object is enqueued on a reference queue.
   * @return true if it is enqueued, false otherwise.
   */
  public boolean isEnqueued()
  {
    return nextOnQueue != null;
  }

  /**
   * Enqueue an object on a reference queue.  This is normally executed
   * by the garbage collection.
   */
  public boolean enqueue() 
  {
    if (queue != null)
      {
	queue.enqueue(this);
	queue = null;
	return true;
      }
    return false;
  }
}
