/* java.lang.ref.ReferenceQueue
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
 * This is the queue, where references can enqueue themselve on.  Each
 * reference may be registered to a queue at initialization time and
 * will be appended to the queue, when the enqueue method is called.
 *
 * The enqueue method may be automatically called by the garbage
 * collector if it detects, that the object is only reachable through
 * the Reference objects.
 *
 * @author Jochen Hoenicke
 * @see Reference#enqueue()
 */
public class ReferenceQueue
{
  /**
   * This is a linked list of references.  If this is null, the list is
   * empty.  Otherwise this points to the first reference on the queue.
   * The first reference will point to the next reference via the 
   * <code>nextOnQueue</code> field.  The last reference will point to
   * itself (not to null, since <code>nextOnQueue</code> is used to 
   * determine if a reference is enqueued).
   */
  private Reference first;

  /**
   * Creates a new empty reference queue.
   */
  public ReferenceQueue()
  {
  }

  /**
   * Checks if there is a reference on the queue, returning it
   * immediately.  The reference will be dequeued.
   *
   * @return a reference on the queue, if there is one,
   * <code>null</code> otherwise.  
   */
  public synchronized Reference poll()
  { 
    return dequeue();
  }

  /**
   * This is called by reference to enqueue itself on this queue.  
   * @param ref the reference that should be enqueued.
   */
  synchronized void enqueue(Reference ref)
  {
    /* last reference will point to itself */
    ref.nextOnQueue = first == null ? ref : first;
    first = ref;
    /* this wakes only one remove thread. */
    notify();
  }

  /**
   * Remove a reference from the queue, if there is one.
   * @return the first element of the queue, or null if there isn't any.
   */
  private Reference dequeue()
  {
    if (first == null)
      return null;

    Reference result = first;
    first = (first == first.nextOnQueue) ? null : first.nextOnQueue;
    result.nextOnQueue = null;
    return result;
  }

  /**
   * Removes a reference from the queue, blocking for <code>timeout</code>
   * until a reference is enqueued.
   * @param timeout the timeout period in milliseconds, <code>0</code> means
   * wait forever.
   * @return the reference removed from the queue, or 
   * <code>null</code> if timeout period expired.  
   * @exception InterruptedException if the wait was interrupted.
   */
  public synchronized Reference remove(long timeout)
    throws InterruptedException
  {
    if (first == null)
      {
	wait(timeout);
      }

    return dequeue();
  }
    

  /**
   * Removes a reference from the queue, blocking until a reference is
   * enqueued.
   *
   * @return the reference removed from the queue.  
   * @exception InterruptedException if the wait was interrupted.
   */
  public Reference remove()
    throws InterruptedException
  {
    return remove(0L);
  }
}
