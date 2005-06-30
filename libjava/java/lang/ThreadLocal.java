/* ThreadLocal -- a variable with a unique value per thread
   Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.

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

import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;


/**
 * ThreadLocal objects have a different state associated with every
 * Thread that accesses them. Every access to the ThreadLocal object
 * (through the <code>get()</code> and <code>set()</code> methods)
 * only affects the state of the object as seen by the currently
 * executing Thread.
 *
 * <p>The first time a ThreadLocal object is accessed on a particular
 * Thread, the state for that Thread's copy of the local variable is set by
 * executing the method <code>initialValue()</code>.
 * </p>
 *
 * <p>An example how you can use this:
 * </p>
 *
 * <pre>
 * class Connection
 * {
 *   private static ThreadLocal owner = new ThreadLocal()
 *     {
 *       public Object initialValue()
 *       {
 *         return("nobody");
 *       }
 *     };
 * ...
 * }
 * </pre>
 *
 * <p>Now all instances of connection can see who the owner of the currently
 * executing Thread is by calling <code>owner.get()</code>. By default any
 * Thread would be associated with 'nobody'. But the Connection object could
 * offer a method that changes the owner associated with the Thread on
 * which the method was called by calling <code>owner.put("somebody")</code>.
 * (Such an owner changing method should then be guarded by security checks.)
 * </p>
 *
 * <p>When a Thread is garbage collected all references to values of
 * the ThreadLocal objects associated with that Thread are removed.
 * </p>
 *
 * @author Mark Wielaard (mark@klomp.org)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public class ThreadLocal
{
  /**
   * Placeholder to distinguish between uninitialized and null set by the
   * user. Do not expose this to the public. Package visible for use by
   * InheritableThreadLocal
   */
  static final Object NULL = new Object();

  /**
   * The stored value. Package visible for use by InheritableThreadLocal. */
  Object value;
	
  /**
   * Maps Threads to values. Uses a WeakHashMap so if a Thread is garbage
   * collected the reference to the Value will disappear. A null value means
   * uninitialized, while NULL means a user-specified null. Only the
   * <code>set(Thread, Object)</code> and <code>get(Thread)</code> methods
   * access it. Package visible for use by InheritableThreadLocal.
   */
  final Map valueMap = Collections.synchronizedMap(new WeakHashMap());
	
  /**
   * Creates a ThreadLocal object without associating any value to it yet.
   */
  public ThreadLocal()
  {
  }

  /**
   * Called once per thread on the first invocation of get(), if set() was
   * not already called. The default implementation returns <code>null</code>.
   * Often, this method is overridden to create the appropriate initial object
   * for the current thread's view of the ThreadLocal.
   *
   * @return the initial value of the variable in this thread
   */
  protected Object initialValue()
  {
    return null;
  }

  /**
   * Gets the value associated with the ThreadLocal object for the currently
   * executing Thread. If this is the first time the current thread has called
   * get(), and it has not already called set(), the value is obtained by
   * <code>initialValue()</code>.
   *
   * @return the value of the variable in this thread
   */
  public Object get()
  {
    Thread currentThread = Thread.currentThread();
    // Note that we don't have to synchronize, as only this thread will
    // ever modify the returned value and valueMap is a synchronizedMap.
    Object value = valueMap.get(currentThread);
    if (value == null)
      {
        value = initialValue();
        valueMap.put(currentThread, value == null ? NULL : value);
      }
    return value == NULL ? null : value;
  }

  /**
   * Sets the value associated with the ThreadLocal object for the currently
   * executing Thread. This overrides any existing value associated with the
   * current Thread and prevents <code>initialValue()</code> from being
   * called if this is the first access to this ThreadLocal in this Thread.
   *
   * @param value the value to set this thread's view of the variable to
   */
  public void set(Object value)
  {
    // Note that we don't have to synchronize, as only this thread will
    // ever modify the returned value and valueMap is a synchronizedMap.
    valueMap.put(Thread.currentThread(), value == null ? NULL : value);
  }
}
