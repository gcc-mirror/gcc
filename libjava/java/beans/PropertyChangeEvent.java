/* java.beans.PropertyChangeEvent
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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


package java.beans;

/**
 ** PropertyChangeEvents are fired in the PropertyChange
 ** and VetoableChange event classes.  They represent the
 ** old and new values as well as the source Bean.<P>
 **
 ** If the old or new value is a primitive type, it must
 ** be wrapped in the appropriate wrapper type
 ** (java.lang.Integer for int, etc., etc.).<P>
 **
 ** If the old or new values are unknown (although why
 ** that would be I do not know), they may be null.<P>
 **
 ** Right now Sun put in a propagationId, reserved for
 ** future use.  Read the comments on the constructor
 ** and on setPropagationId for more information.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 29 Jul 1998
 **/

public class PropertyChangeEvent extends java.util.EventObject {
	String propertyName;
	Object oldValue;
	Object newValue;
	Object propagationId;

	private static final long serialVersionUID = 7042693688939648123L;

	/** Create a new PropertyChangeEvent.  Remember that if
	 ** you received a PropertyChangeEvent and are sending
	 ** a new one, you should also set the propagation ID
	 ** from the old PropertyChangeEvent.
	 ** @param source the Bean containing the property.
	 ** @param propertyName the property's name.
	 ** @param oldValue the old value of the property.
	 ** @param newValue the new value of the property.
	 **/
	public PropertyChangeEvent(Object source, String propertyName, Object oldVal, Object newVal) {
		super(source);
		this.propertyName = propertyName;
		oldValue = oldVal;
		newValue = newVal;
	}

	/** Get the property name.
	 ** @return the property name.
	 **/
	public String getPropertyName() {
		return propertyName;
	}

	/** Get the property's old value.
	 ** @return the property's old value.
	 **/
	public Object getOldValue() {
		return oldValue;
	}

	/** Get the property's new value.
	 ** @return the property's new value.
	 **/
	public Object getNewValue() {
		return newValue;
	}

	/** Set the propagation ID.  This is a way for the event
	 ** to be passed from hand to hand and retain a little
	 ** extra state.  Right now it is unused, but it should
	 ** be propagated anyway so that future versions of
	 ** JavaBeans can use it, for God knows what.
	 ** @param propagationId the propagation ID.
	 **/
	public void setPropagationId(Object propagationId) {
		this.propagationId = propagationId;
	}

	/** Get the propagation ID.
	 ** @return the propagation ID.
	 **/
	public Object getPropagationId() {
		return propagationId;
	}
}
