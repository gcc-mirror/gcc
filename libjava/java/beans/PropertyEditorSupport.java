/* java.beans.PropertyEditorSupport
   Copyright (C) 1998 Free Software Foundation, Inc.

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


package java.beans;

/**
 ** PropertyEditorSupport helps with PropertyEditors,
 ** implementing base functionality that they usually must
 ** have but which is a pain to implement.  You may extend
 ** from this class or use it as a standalone.<P>
 **
 ** This class does not do any painting or actual editing.
 ** For that, you must use or extend it.  See the
 ** PropertyEditor class for better descriptions of what
 ** the various methods do.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 29 Jul 1998
 **/

public class PropertyEditorSupport implements PropertyEditor {
	Object eventSource;
	Object val;
	PropertyChangeSupport pSupport;

	/** Call this constructor when you are deriving from
	 ** PropertyEditorSupport.
	 **/
	protected PropertyEditorSupport() {
		this.eventSource = this;
		this.pSupport = new PropertyChangeSupport(this);
	}

	/** Call this constructor when you are using
	 ** PropertyEditorSupport as a helper object.
	 ** @param eventSource the source to use when firing
	 **        property change events.
	 **/
	protected PropertyEditorSupport(Object eventSource) {
		this.eventSource = eventSource;
		this.pSupport = new PropertyChangeSupport(this);
	}

	/** Set the current value of the property.
	 ** <STRONG>Implementation Note</STRONG> Sun does not
	 ** state what exactly this version of the method does.
	 ** Thus, in this implementation, it sets the value, and
	 ** then if the old and new values are different, it
	 ** fires a property change event with no property name
	 ** and the old and new values.
	 ** @param val the new value for the property.
	 **/
	public void setValue(Object val) {
		Object oldVal = val;
		this.val = val;
		if(!oldVal.equals(val)) {
			pSupport.firePropertyChange(null,oldVal,val);
		}
	}

	/** Get the current value of the property.
	 ** @return the current value of the property.
	 **/
	public Object getValue() {
		return val;
	}

	/** Get whether this object is paintable or not.
	 ** @return <CODE>false</CODE>
	 **/
	public boolean isPaintable() {
		return false;
	}

	/** Paint this object.  This class does nothing in
	 ** this method.
	 **/
	public void paintValue(java.awt.Graphics g, java.awt.Rectangle r) {
	}

	/** Get the Java initialization String for the current
	 ** value of the Object.  This class returns gibberish or
	 ** null (though the spec does not say which).<P>
	 ** <STRONG>Implementation Note:</STRONG> This class
	 ** returns the string "@$#^" to make sure the code will
	 ** be broken, so that you will know to override it when
	 ** you create your own property editor.
	 ** @return the Java initialization string.
	 **/
	public String getJavaInitializationString() {
		return "@$#^";
	}

	/** Get the value as text.
	 ** In this class, you cannot count on getAsText() doing
	 ** anything useful, although in this implementation I
	 ** do toString().
	 ** @return the value as text.
	 **/
	public String getAsText() {
		return val != null ? val.toString() : "null";
	}

	/** Set the value as text.
	 ** In this class, you cannot count on setAsText() doing
	 ** anything useful across implementations.
	 ** <STRONG>Implementation Note:</STRONG> In this
	 ** implementation it checks if the String is "null", and
	 ** if it is, sets the value to null, otherwise it throws
	 ** an IllegalArgumentException.
	 ** @param s the text to convert to a new value.
	 ** @exception IllegalArgumentException if the text is
	 **            malformed.
	 **/
	public void setAsText(String s) throws IllegalArgumentException {
		if(s.equals("null")) {
			setValue(null);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/** Returns a list of possible choices for the value.
	 ** @return <CODE>null</CODE>
	 **/
	public String[] getTags() {
		return null;
	}

	/** Return a custom component to edit the value.
	 ** @return <CODE>null</CODE> in this class.
	 **/
	public java.awt.Component getCustomEditor() {
		return null;
	}

	/** Find out whether this property editor supports a
	 ** custom component to edit its value.
	 ** @return <CODE>false</CODE> in this class.
	 **/
	public boolean supportsCustomEditor() {
		return false;
	}

	/** Add a property change listener to this property editor.
	 ** @param l the listener to add.
	 **/
	public void addPropertyChangeListener(PropertyChangeListener l) {
		pSupport.addPropertyChangeListener(l);
	}

	/** Remove a property change listener from this property editor.
	 ** @param l the listener to remove.
	 **/
	public void removePropertyChangeListener(PropertyChangeListener l) {
		pSupport.removePropertyChangeListener(l);
	}


	/** Notify people that we've changed, although we don't
	 ** tell them just how.  The only thing I can think of to
	 ** send in the event is the new value (since the old value
	 ** is unavailable and there is no property name).
	 ** I confess I do not understand the point of this method.
	 **/
	public void firePropertyChange() {
		pSupport.firePropertyChange(null,null,val);
	}
}

