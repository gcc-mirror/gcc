/* SwingPropertyChangeSupport.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing.event;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventListener;
import java.util.Hashtable;

/**
 * SwingPropertyChangeSupport
 * @author Andrew Selkirk
*/
public final	class SwingPropertyChangeSupport
				extends PropertyChangeSupport {

  private static final long serialVersionUID = 7162625831330845068L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * listeners
	 */
	private transient EventListenerList listeners;

	/**
	 * propertyListeners
	 */
	private Hashtable propertyListeners;

	/**
	 * source
	 */
	private Object source;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor SwingPropertyChangeSupport
	 * @param source TODO
	 */
	public SwingPropertyChangeSupport(Object source) {
		super(source);
		this.source = source;
		this.listeners = new EventListenerList();
		this.propertyListeners = new Hashtable();
	} // SwingPropertyChangeSupport()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * readObject
	 * @param stream TODO
	 * @exception ClassNotFoundException TODO
	 * @exception IOException TODO
	 */
	private void readObject(ObjectInputStream stream) throws ClassNotFoundException, IOException {
		// TODO
	} // readObject()

	/**
	 * addPropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void addPropertyChangeListener(PropertyChangeListener listener) {
		listeners.add(PropertyChangeListener.class, listener);
	} // addPropertyChangeListener()

	/**
	 * addPropertyChangeListener
	 * @param propertyName TODO
	 * @param listener TODO
	 */
	public synchronized void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {

		// Variables
		EventListenerList	list;

		// Get Listener list
		list = (EventListenerList) propertyListeners.get(propertyName);
		if (list == null) {
			list = new EventListenerList();
			propertyListeners.put(propertyName, list);
		} // if

		// Add Listeners
		list.add(PropertyChangeListener.class, listener);

	} // addPropertyChangeListener()

	/**
	 * removePropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void removePropertyChangeListener(PropertyChangeListener listener) {
		listeners.remove(PropertyChangeListener.class, listener);
	} // removePropertyChangeListener()

	/**
	 * removePropertyChangeListener
	 * @param propertyName TODO
	 * @param listener TODO
	 */
	public synchronized void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {

		// Variables
		EventListenerList	list;

		// Get Listener list
		list = (EventListenerList) propertyListeners.get(propertyName);
		if (list == null) {
			return;
		} // if

		// Remove Listeners
		list.remove(PropertyChangeListener.class, listener);

		// Clean up propertyListeners
		if (list.getListenerCount() == 0) {
			propertyListeners.remove(propertyName);
		} // if

	} // removePropertyChangeListener()

	/**
	 * firePropertyChange
	 * @param propertyName TODO
	 * @param oldValue TODO
	 * @param newValue TODO
	 */
	public void firePropertyChange(String propertyName, Object oldValue, Object newValue) {

		// Variables
		PropertyChangeEvent	event;

		// Create Property Change Event
		event = new PropertyChangeEvent(source, propertyName, oldValue, newValue);

		// Fire Event
		firePropertyChange(event);

	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param event TODO
	 */
	public void firePropertyChange(PropertyChangeEvent event) {

		// Variables
		EventListenerList		list;
		EventListener[]			listenerList;
		int						index;
		PropertyChangeListener	listener;

		// Check Values if they are equal
		if (event.getOldValue() == null || event.getNewValue() == null ||
			event.getOldValue().equals(event.getNewValue()) == true) {
			return;
		} // if

		// Process Main Listener List
		listenerList = listeners.getListeners(PropertyChangeListener.class);
		for (index = 0; index < listenerList.length; index++) {
			listener = (PropertyChangeListener) listenerList[index];
			listener.propertyChange(event);
		} // for

		// Process Property Listener List
		list = (EventListenerList) propertyListeners.get(event.getPropertyName());
		if (list != null) {
			listenerList = list.getListeners(PropertyChangeListener.class);
			for (index = 0; index < listenerList.length; index++) {
				listener = (PropertyChangeListener) listenerList[index];
				listener.propertyChange(event);
			} // for
		} // if

	} // firePropertyChange()

	/**
	 * hasListeners
	 * @param propertyName TODO
	 * @returns boolean
	 */
	public synchronized boolean hasListeners(String propertyName) {

		// Get Listener list
		if (propertyListeners.get(propertyName) == null) {
			return false;
		} // if

		return true;

	} // hasListeners()


} // SwingPropertyChangeSupport
