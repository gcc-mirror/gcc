/* java.beans.PropertyEditor
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
 ** PropertyEditors are custom GUI editors for specific types of values.
 **
 ** A PropertyEditor can be used, for example, if you are editing a type of value
 ** that can be more easily represented graphically, such as a Point, or one that
 ** can be more easily represented by a list, such as a boolean (true/false).<P>
 **
 ** A PropertyEditor must be able to display its contents when asked to and
 ** be able to allow the user to change its underlying field value.  However, it
 ** is not the PropertyEditor's responsibility to make the change to the
 ** underlying Object; in fact, the PropertyEditor does not even know about the
 ** Object it is actually editing--only about the property it is currently
 ** editing.  When a change is made to the property, the PropertyEditor must
 ** simply fire a PropertyChangeEvent and allow the RAD tool to actually set
 ** the property in the underlying Bean.<P>
 **
 ** PropertyEditors should not change the Objects they are given by setValue().
 ** These Objects may or may not be the actual Objects which are properties of
 ** the Bean being edited.  Instead, PropertyEditors should create a new Object
 ** and fire a PropertyChangeEvent with the old and new values.<P>
 **
 ** PropertyEditors also must support the ability to return a Java
 ** initialization string.  See the getJavaInitializationString() method for
 ** details.<P>
 **
 ** There are several different ways a PropertyEditor may display and control
 ** editing of its value.  When multiple types of input and display are
 ** given by a single PropertyEditor, the RAD tool may decide which of the call
 ** to support.  Some RAD tools may even be text-only, so even if you support
 ** a graphical set and get, it may choose the text set and get whenever it can.
 ** <OL>
 **   <LI>Every PropertyEditor must support getValue() and setValue().  For
 **       setValue(), the component must only support it when the argument is
 **       the same type that the PropertyEditor supports.</LI>
 **   <LI>Every PropertyEditor must support getJavaInitializationString().</LI>
 **   <LI>You may support painting the value yourself if you wish.  To do this,
 **       have isPaintable() return true and implement the paintValue() method.
 **       This method does not determine in any way how the value is edited;
 **       merely how it is displayed.</LI>
 **   <LI>Let the caller of the PropertyEditor give the user a text input.  Do
 **       this by returning a non-null String from getAsText().  If you support
 **       text input, you *must* support setAsText().</LI>
 **   <LI>Give the caller a set of possible values, such as "true"/"false", that
 **       the user must select from.  To do this, return the list of Strings
 **       from the getTags() method.  The RAD tool may choose to implement the
 **       user input any way it wishes, and only guarantees that setAsText() will
 **       only be called with one of the Strings returned from getTags().</LI>
 **   <LI>You may support a whole custom editing control by supporting
 **       getCustomEditor().  To do this, return true from supportsCustomEditor()
 **       and return a Component that does the job.  It is the component's job,
 **       or the PropertyEditor's job, to make sure that when the editor changes
 **       its value, the PropertyChangeEvent is thrown.</LI>
 ** </OL>
 **
 ** The PropertyEditor for a particular Bean can be found using the
 ** PropertyEditorManager class, which goes through a series of different
 ** checks to find the appropriate class.<P>
 **
 ** A PropertyChangeEvent should be thrown from the PropertyEditor whenever a
 ** bound  property (a property PropertyDescriptor.isBound() set to true)
 ** changes.  When this happens, the editor itself should *not* change the value
 ** itself, but rather allow the RAD tool to call setValue() or setAsText().
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 30 June 1998
 ** @see java.beans.PropertyEditorManager
 ** @see java.beans.PropertyEditorSupport
 **/

public interface PropertyEditor {
	/** Called by the RAD tool to set the value of this property for the PropertyEditor.
	 ** If the property type is native, it should be wrapped in the appropriate
	 ** wrapper type.
	 ** @param value the value to set this property to.
	 **/
	void setValue(Object value);

	/** Accessor method to get the current value the PropertyEditor is working with.
	 ** If the property type is native, it will be wrapped in the appropriate
	 ** wrapper type.
	 ** @return the current value of the PropertyEditor.
	 **/
	Object getValue();


	/** Set the value of this property using a String.
	 ** Whether or not this PropertyEditor is editing a String type, this converts
	 ** the String into the type of the PropertyEditor.
	 ** @param text the text to set it to.
	 ** @exception IllegalArgumentException if the String is in the wrong format or setAsText() is not supported.
	 **/
	void setAsText(String text) throws IllegalArgumentException;

	/** Get the value of this property in String format.
	 ** Many times this can simply use Object.toString().<P>
	 ** Return null if you do not support getAsText()/setAsText().
	 ** <code>setAsText(getAsText())</code> should be valid; i.e. the stuff you spit out in
	 ** getAsText() should be able to go into setAsText().
	 ** @return the value of this property in String format.
	 **/
	String getAsText();

	/** Get a list of possible Strings which this property type can have.
	 ** The value of these will be used by the RAD tool to construct some sort
	 ** of list box or to check text box input, and the resulting String passed
	 ** to setAsText() should be one of these.  Note, however, that like most things
	 ** with this mammoth, unwieldy interface, this is not guaranteed.  Thus, you
	 ** must check the value in setAsText() anyway.
	 ** @return the list of possible String values for this property type.
	 **/
	String[] getTags();


	/** The RAD tool calls this to find out whether the PropertyEditor can paint itself.
	 ** @return true if it can paint itself graphically, false if it cannot.
	 **/
	boolean isPaintable();

	/** The RAD tool calls this to paint the actual value of the property.
	 ** The Graphics context will have the same current font, color, etc. as the
	 ** parent Container.  You may safely change the font, color, etc. and not
	 ** change them back.<P>
	 ** This method should do a silent no-op if isPaintable() is false.
	 ** @param g the Graphics context to paint on
	 ** @param bounds the rectangle you have reserved to work in
	 **/
	void paintValue(java.awt.Graphics g, java.awt.Rectangle bounds);


	/** The RAD tool calls this to find out whether the PropertyEditor supports a custom component to edit and display itself.
	 ** @return true if getCustomEditor() will return a component, false if not.
	 **/
	boolean supportsCustomEditor();

	/** The RAD tool calls this to grab the component that can edit this type.
	 ** The component may be painted anywhere the RAD tool wants to paint it--
	 ** even in its own window.<P>
	 ** The component must hook up with the PropertyEditor and, whenever a
	 ** change to the value is made, fire a PropertyChangeEvent to the source.<P>
	 ** @return the custom editor for this property type.
	 **/
	java.awt.Component getCustomEditor();


	/** Adds a property change listener to this PropertyEditor.
	 ** @param listener the listener to add
	 **/
	void addPropertyChangeListener(PropertyChangeListener listener);

	/** Removes a property change listener from this PropertyEditor.
	 ** @param listener the listener to remove
	 **/
	void removePropertyChangeListener(PropertyChangeListener listener);

	/** Get a Java language-specific String which could be used to create an Object
	 ** of the specified type.  Every PropertyEditor must support this.<P>
	 ** The reason for this is that while most RAD tools will serialize the Beans
	 ** and deserialize them at runtime, some RAD tools will generate code that
	 ** creates the Beans.  Examples of Java initialization strings would be:<P>
	 ** <OL>
	 **     <LI><CODE>2</CODE></LI>
	 **     <LI><CODE>"I am a String"</CODE></LI>
	 **     <LI><CODE>new MyObject(2, "String", new StringBuffer())</CODE></LI>
	 ** </OL>
	 ** @return the initialization string for this object in Java.
	 **/
	String getJavaInitializationString();
}
