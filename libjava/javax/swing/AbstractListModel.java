/* AbstractListModel.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing;

// Imports
import java.io.*;
import java.util.*;
import javax.swing.event.*;

/**
 * AbstractListModel
 * A2uthor	Ronald Veldema
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class AbstractListModel implements ListModel, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * listenerList
	 */
	protected EventListenerList listenerList = new EventListenerList();


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor AbstractListModel
	 */
	public AbstractListModel() {
	} // AbstractListModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * addListDataListener
	 * @param listener TODO
	 */
	public void addListDataListener(ListDataListener listener) {
		listenerList.add(ListDataListener.class, (EventListener) listener);
	} // addListDataListener()

	/**
	 * removeListDataListener
	 * @param listener TODO
	 */
	public void removeListDataListener(ListDataListener listener) {
		listenerList.remove(ListDataListener.class, (EventListener) listener);
	} // removeListDataListener()

	/**
	 * fireContentsChanged
	 * @param source TODO
	 * @param startIndex TODO
	 * @param endIndex TODO
	 */
	protected void fireContentsChanged(Object source, int startIndex, int endIndex) {

		// Variables
		ListDataEvent	event;
		EventListener[]		listeners;
		ListDataListener	listener;
		int					index;

		// Create Event
		event = new ListDataEvent(source, ListDataEvent.CONTENTS_CHANGED,
					startIndex, endIndex);

		// Get Listeners
		listeners = listenerList.getListeners(ListDataListener.class);

		// Process Listeners
		for (index = 0; index < listeners.length; index++) {
			listener = (ListDataListener) listeners[index];
			listener.contentsChanged(event);
		} // for

	} // fireContentsChanged()

	/**
	 * fireIntervalAdded
	 * @param source TODO
	 * @param startIndex TODO
	 * @param endIndex TODO
	 */
	protected void fireIntervalAdded(Object source, int startIndex, int endIndex) {

		// Variables
		ListDataEvent	event;
		EventListener[]		listeners;
		ListDataListener	listener;
		int					index;

		// Create Event
		event = new ListDataEvent(source, ListDataEvent.INTERVAL_ADDED,
					startIndex, endIndex);

		// Get Listeners
		listeners = listenerList.getListeners(ListDataListener.class);

		// Process Listeners
		for (index = 0; index < listeners.length; index++) {
			listener = (ListDataListener) listeners[index];
			listener.intervalAdded(event);
		} // for

	} // fireIntervalAdded()

	/**
	 * fireIntervalRemoved
	 * @param source TODO
	 * @param startIndex TODO
	 * @param endIndex TODO
	 */
	protected void fireIntervalRemoved(Object source, int startIndex, int endIndex) {

		// Variables
		ListDataEvent		event;
		EventListener[]		listeners;
		ListDataListener	listener;
		int					index;

		// Create Event
		event = new ListDataEvent(source, ListDataEvent.INTERVAL_REMOVED,
					startIndex, endIndex);

		// Get Listeners
		listeners = listenerList.getListeners(ListDataListener.class);

		// Process Listeners
		for (index = 0; index < listeners.length; index++) {
			listener = (ListDataListener) listeners[index];
			listener.intervalRemoved(event);
		} // for

	} // fireIntervalRemoved()

	/**
	 * getListeners
	 * @param listenerType TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class listenerType) {
		return listenerList.getListeners(listenerType);
	} // getListeners()

	/**
	 * getListDataListeners
	 */
	public ListDataListener[] getListDataListeners()
	{
          // FIXME: implement this
	  return null;
	}

	/**
	 * getElementAt
	 * @param index TODO
	 * @returns Object
	 */
	public abstract Object getElementAt(int index);

	/**
	 * getSize
	 * @returns int
	 */
	public abstract int getSize();


} // AbstractListModel
