/* InheritableThreadLocal -- a ThreadLocal which inherits values across threads
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

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

package java.lang;

import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.WeakHashMap;

/**
 * A ThreadLocal whose value is inherited by child Threads. The value of the
 * InheritableThreadLocal associated with the (parent) Thread is copied to
 * the new (child) Thread at the moment of creation.
 *
 * <p>It is possible to make the value associated with the child Thread a
 * function of the value that is associated with the parent Thread by
 * overriding the <code>childValue()</code> method. The utility of this class
 * is in transferring items like User ID or Transaction ID across threads
 * automatically.
 *
 * @author Mark Wielaard <mark@klomp.org>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see ThreadLocal
 * @since 1.2
 * @status updated to 1.4
 */
public class InheritableThreadLocal extends ThreadLocal
{
  /**
   * Maps Threads to a List of InheritableThreadLocals (the heritage of that
   * Thread). Uses a WeakHashMap so if the Thread is garbage collected the
   * List can be collected, too. Maps to a list in case the user overrides
   * equals.
   */
  private static final WeakHashMap threadMap = new WeakHashMap();

  /**
   * Creates a new InheritableThreadLocal that has no values associated
   * with it yet.
   */
  public InheritableThreadLocal()
  {
    Thread currentThread = Thread.currentThread();
    // Note that we don't have to synchronize, as only this thread will
    // ever modify the returned heritage.
    List heritage = (List) threadMap.get(currentThread);
    if (heritage == null)
      {
        heritage = new ArrayList();
        threadMap.put(currentThread, heritage);
      }
    heritage.add(this);
  }

  /**
   * Determines the value associated with a newly created child Thread as a
   * function of the value associated with the currently executing (parent)
   * Thread. The default implementation just returns the parentValue.
   *
   * @param parentValue the value of this object in the parent thread at
   *        the moment of creation of the child
   * @return the initial value for the child thread
   */
  protected Object childValue(Object parentValue)
  {
    return parentValue;
  }

  /**
   * Generates the childValues of all <code>InheritableThreadLocal</code>s
   * that are in the heritage of the current Thread for the newly created
   * childThread. Should be called from the contructor Thread.
   *
   * @param childThread the newly created thread, to inherit from this thread
   * @see Thread#Thread(ThreadGroup, Runnable, String)
   */
  static void newChildThread(Thread childThread)
  {
    // The currentThread is the parent of the new thread.
    Thread parentThread = Thread.currentThread();
    // Note that we don't have to synchronize, as only this thread will
    // ever modify the returned heritage.
    ArrayList heritage = (ArrayList) threadMap.get(parentThread);
    if (heritage != null)
      {
        threadMap.put(childThread, heritage.clone());
        // Perform the inheritance.
        Iterator it = heritage.iterator();
        int i = heritage.size();
        while (--i >= 0)
          {
            InheritableThreadLocal local = (InheritableThreadLocal) it.next();
            Object parentValue = local.valueMap.get(parentThread);
            if (parentValue != null)
              {
                Object childValue = local.childValue(parentValue == NULL
                                                     ? null : parentValue);
                local.valueMap.put(childThread, (childValue == null
                                                 ? NULL : parentValue));
              }
          }
      }
  }
}
