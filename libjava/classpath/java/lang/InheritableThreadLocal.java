/* InheritableThreadLocal -- a ThreadLocal which inherits values across threads
   Copyright (C) 2000, 2001, 2002, 2003, 2005  Free Software Foundation, Inc.

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

package java.lang;

import java.util.Iterator;
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
 * @author Mark Wielaard (mark@klomp.org)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see ThreadLocal
 * @since 1.2
 * @status updated to 1.4
 */
public class InheritableThreadLocal extends ThreadLocal
{
  /**
   * Creates a new InheritableThreadLocal that has no values associated
   * with it yet.
   */
  public InheritableThreadLocal()
  {
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
    if (parentThread.locals != null)
      {
        Iterator keys = parentThread.locals.keySet().iterator();
        while (keys.hasNext())
          {
            Key key = (Key)keys.next();
            if (key.get() instanceof InheritableThreadLocal)
              {
                InheritableThreadLocal local = (InheritableThreadLocal)key.get();
                Object parentValue = parentThread.locals.get(key);
                Object childValue = local.childValue(parentValue == NULL
                                                     ? null : parentValue);
                if (childThread.locals == null)
                    childThread.locals = new WeakHashMap();
                childThread.locals.put(key, (childValue == null
                                             ? NULL : childValue));
              }
          }
      }
  }
}
