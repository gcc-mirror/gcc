/* java.beans.beancontext.BeanContextEvent
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


package java.beans.beancontext;

import java.util.EventObject;

/**
 * Generic superclass for events fired by <code>BeanContext</code>s.
 *
 * @author John Keiser
 * @since JDK1.2
 */

public abstract class BeanContextEvent extends EventObject {
	/**
	 * The <code>BeanContext</code> that most recently passed this
	 * event on.
	 */
	protected BeanContext propagatedFrom;

	/**
	 * Create a new event, from the specified <code>BeanContext</code>.
	 * <code>propagatedFrom</code> will be initialized to
	 * <code>null</code>.
	 *
	 * @param source the source of the event.
	 */
	protected BeanContextEvent(BeanContext source) {
		super(source);
	}

	/**
	 * Get the <code>BeanContext</code> that originated this event.
	 * @return the originator of this event.
	 */
	public BeanContext getBeanContext() {
		return (BeanContext)getSource();
	}

	/**
	 * Get the most recent propagator of this event.
	 * If this value is <code>null</code>, you have received the event
	 * straight from the source.
	 *
	 * @return the most recent propagator of this event.
	 */
	public BeanContext getPropagatedFrom() {
		return propagatedFrom;
	}

	/**
	 * Tell whether this event has been propagated.
	 * @return <code>true</code> iff <code>getPropagatedFrom() != null</code>.
	 */
	public boolean isPropagated() {
		return propagatedFrom != null;
	}

	/**
	 * Set the most recent propagator of this event.
	 * @param propagator the most recent propagator of this event.
	 */
	public void setPropagatedFrom(BeanContext propagator) {
		propagatedFrom = propagator;
	}
}
