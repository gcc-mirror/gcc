/* java.lang.ThreadLocal
   Copyright (C) 2000 Free Software Foundation, Inc.

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

import java.util.Map;
import java.util.WeakHashMap;

/**
 * ThreadLocal objects have a different state associated with every
 * Thread that accesses them. Every access to the ThreadLocal object
 * (through the <code>get()</code> and <code>set()</code> methods)
 * only affects the state of the object as seen by the currently
 * executing Thread.
 * <p>
 * The first time a ThreadLocal object is accessed on a particular
 * Thread (and no state is associated with that Thread yet)
 * the state for that Thread is set by executing the method
 * <code>initialValue()</code>.
 * <p>
 * An example how you can use this:
 * <pre>
 * class Connection {
 *     private static ThreadLocal owner = new ThreadLocal() {
 *        public Object initialValue() {
 *            return("nobody");
 *        }
 *     };
 * ...
 * }
 * </pre>
 * Now all instances of connection can see who the owner of the currently
 * executing Thread is by calling <code>owner.get()</code>. By default any
 * Thread would be associated with 'nobody'. But the Connection object could
 * offer a method that changes the owner associated with the Thread on
 * which the method was called by calling <code>owner.put("somebody")</code>.
 * (Such an owner changing method should then be guarded by security checks.)
 * <p>
 * When a Thread is garbage collected all references to values of
 * the ThreadLocal objects associated with that Thread are removed.
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class ThreadLocal {
	
	/**
	 * Trivial container to wrap the stored values.
	 * Needed to see if the value is null or not yet set.
	 * If it is not yet set we must call intialValue() once.
	 * Package local so InheritableThreadLocal can see it.
	 */
	final static class Value {
		final Object value;
		
		Value(Object value) {
			this.value = value;
		}
		
		Object getValue() {
			return value;
		}
	}
	
	/**
	 * Maps Threads to Values. Uses a WeakHashMap so if a Thread is garbage
	 * collected the reference to the Value will disappear. Only the
	 * <code>set(Thread, Value)</code> and <code>get(Thread)</code> methods
	 * access it. Since this can happen from multiple Threads simultaniously
	 * those methods are synchronized.
	 */
	private final Map valueMap = new WeakHashMap();
	
	/**
	 * Creates a ThreadLocal object without associating any value to it
	 * yet.
	 */
	public ThreadLocal() {
	}
	
	/**
	 * Gets the value associated with the ThreadLocal object for the
	 * currently executing Thread. If there is no value is associated
	 * with this Thread yet then the valued returned by the
	 * <code>initialValue()</code> method is assosiated with this Thread
	 * and returned.
	 */
	public Object get() {
		Thread currentThread = Thread.currentThread();
		Value v = get(currentThread);
		if (v == null) {
			v = new Value(initialValue());
			set(currentThread, v);
		}
		return v.getValue();
	}
	
	/**
	 * Gets the Value of this ThreadLocal for a particular Thread.
	 * It is synchronized so the <code>set(Thread, Value)</code> method cannot
	 * simultaniously modify the </code>valueMap</code> from another thread.
	 * Package local so InheritableThreadLocal can access it when a new child
	 * Thread inherits values from its parent Thread.
	 */
	synchronized final Value get(Thread thread) {
		return (Value)valueMap.get(thread);
	}
	
	/**
	 * Sets the value associated with the ThreadLocal object for the
	 * currently executing Thread. This overrides any existing value
	 * associated with the current Thread and does not call the
	 * <code>initialValue()</code> method, even if this is the first
	 * time this Thread accesses this ThreadLocal.
	 */
	public void set(Object value) {
		Thread currentThread = Thread.currentThread();
		Value v = new Value(value);
		set(currentThread, v);
	}
	
	/**
	 * Sets the Value for this ThreadLocal for a particular Thread.
	 * It is synchronized so the <code>get(Thread)</code> method cannot
	 * simultaniously read the </code>valueMap</code> from another thread.
	 * Package local so InheritableThreadLocal can access it when a new child
	 * Thread inherits values from its parent Thread.
	 */
	synchronized final void set(Thread thread, Value value) {
		valueMap.put(thread, value);
	}
	
	/**
	 * Called when <code>get()</code> is called and no state is associated
	 * with the currently executing Thread yet.
	 * <p>
	 * The default implementation returns <code>null</code>.
	 */
	protected Object initialValue() {
		return null;
	}
}
