/* java.lang.InheritableThreadLocal
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

/**
 * ThreadLocal whose value is inherited by child Threads.
 * The value of the InheritableThreadLocal associated with the (parent) Thread
 * on the moment that it creates a new (child) Thread is set as the value that
 * is associated with the new (child) Thread.
 * <p>
 * It is possible to make the value associated with the child Thread a function
 * of the value that is associated with the parent Thread by overriding the
 * <code>childValue()</code> method.
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class InheritableThreadLocal extends ThreadLocal {
	
	/**
	 * Maps Threads to a Set of InheritableThreadLocals
	 * (the heritage of that Thread).
	 * Uses a WeakHashMap so if the Thread is garbage collected the reference
	 * to that Set disappears.
	 * Both <code>AddToHeritage</code> access and modify it so they have to
	 * synchronize on the threadMap when they do.
	 */
	private static Map threadMap = new WeakHashMap();
	
	/**
	 * Creates a new InheritableThreadLocal that has no values associated
	 * with it yet.
	 */
	public InheritableThreadLocal() {
		super();
	}
	
	/**
	 * Determines the value associated with a newly created child Thread
	 * as a function of the value associated with the currently executing
	 * (parent) Thread.
	 * <p>
	 * The default implementation just returns the parentValue.
	 */
	protected Object childValue(Object parentValue) {
		return parentValue;
	}
	
	/**
	 * Adds this <code>InheritableThreadLocal</code> to the heritage of the
	 * current Thread and returns the value of the <code>ThreadLocal</code>
	 * for the Thread. The value will be either the last value that the
	 * current Thread has set, or the childValue of the last value that the
	 * parent Thread set before the current Thread was created, or the
	 * initialValue of the <code>ThreadLocal</code>.
	 *
	 * @see ThreadLocal#get()
	 */
	public Object get() {
		addToHeritage(); 
		return super.get();
	}
	
	/**
	 * Adds this <code>InheritableThreadLocal</code> to the heritage of the
	 * current Thread and sets the value of the <code>ThreadLocal</code>
	 * for the Thread.
	 *
	 * @see ThreadLocal#set(Object)
	 */
	public void set(Object value) {
		addToHeritage();
		super.set(value);
	}
	
	/**
	 * Adds this <code>InheritableThreadLocal</code> to the heritage
	 * of the current Thread.
	 */
	private void addToHeritage() {
		Thread currentThread = Thread.currentThread();
		Set heritage;
		synchronized(threadMap) {
			heritage = (Set)threadMap.get(currentThread);
		}
		// Note that we don't have to synchronize on the heritage Set
		// since only this Thread (or the parent Thread when creating
		// the heritage) ever modifies it.
		if (heritage == null) {
			heritage = new HashSet();
			synchronized(threadMap) {
				threadMap.put(currentThread, heritage);
			}
		}
		if (!heritage.contains(this)) {
			heritage.add(this);
		}
	}
	
	/**
	 * Generates the childValues of all <code>InheritableThreadLocal</code>s
	 * that are in the heritage of the current Thread for the newly created
	 * childThread.
	 * Should be called from the contructor of java.lang.Thread.
	 */
	static void newChildThread(Thread childThread) {
		// The currentThread is the parent of the new thread
		Thread parentThread = Thread.currentThread();
		
		// Inherit all the InheritableThreadLocals of the parent thread
		Set heritage;
		synchronized(threadMap) {
			heritage = (Set)threadMap.get(parentThread);
		}
		// Note that we don't have to synchronize on the heritage Set
		// since only this Thread (or the parent Thread when creating
		// the heritage) ever modifies it.
		if (heritage != null) {
			synchronized(threadMap) {
				threadMap.put(childThread, new HashSet(heritage));
			}
			// And constructs all the new child values
			// (has to be done now that we are executing in the parentThread)
			Iterator it = heritage.iterator();
			while (it.hasNext()) {
				InheritableThreadLocal local =
					(InheritableThreadLocal) it.next();
				// Note that the parentValue cannot be null
				// If it was it would not be in the heritage
				Object parentValue = local.get(parentThread).getValue();
				Object childValue = local.childValue(parentValue);
				ThreadLocal.Value v = new ThreadLocal.Value(childValue);
				local.set(childThread, v);
			}
		}
	}
}
