/* EventListenerList.java --
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

package javax.swing.event;

// Imports
import java.io.Serializable;
import java.util.EventListener;

/**
 * EventListenerList
 * @author Andrew Selkirk
 */
public class EventListenerList	extends		Object
								implements	Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------
	
	/**
	 * Listener list
	 */
	protected	Object[]	listenerList	= null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------
	
	/**
	 * EventListenerList constructor
	 */
	public EventListenerList() {
		listenerList = new Object[0];
	} // EventListenerList()

	
	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Add Listener
	 * @param t Class type
	 * @param listener Listener to add
	 */
	public void add(Class t, EventListener listener) {

		// Variables
		Object[]		list;
		int				index;
		Class			checkClass;
		EventListener	checkListener;

		// Create New list in anticipation that listener is not present
		list = new Object[listenerList.length + 2];

		// Search through list looking for listener
		for (index = 0; index < listenerList.length; index += 2) {
			checkClass = (Class) listenerList[index];
			checkListener = (EventListener) listenerList[index + 1];
			if (checkClass.equals(t) == true &&
				checkListener.equals(listener) == true) {
				return;
			} // if
		} // for

		// Add Listener
		list[listenerList.length] = t;
		list[listenerList.length + 1] = listener;

		// Replace Listener List
		listenerList = list;

	} // add()

	/**
	 * Get the total number of listeners
	 * @return Count of listeners
	 */
	public int getListenerCount() {
		return (int) listenerList.length / 2;
	} // getListenerCount

	/**
	 * Get the number of listeners of a particular type
	 * @param t Class type to count
	 * @returns Count of the specified listeners
	 */
	public int getListenerCount(Class t) {

		// Variables
		int		index;
		int		count;
		String	name;

		// Loop through entire list
		count = 0;
		name  = t.getName();
		for (index = 0; index < listenerList.length; index += 2) {
			if (((Class) listenerList[index]).getName().equals(name) == true) {
				count += 1;
			}
		} // for: index

		// Return Count
		return count;

	} // getListenerCount()

	/**
	 * Get a list of listenerType/listener pairs
	 * @returns Listener list
	 */
	public Object[] getListenerList() {
		return listenerList;
	} // getListenerList()

	/**
	 * Get list of listeners of a particular type
	 * @param c Class type
	 * @returns List of listeners of the specified type
	 */
	public EventListener[] getListeners(Class c) {

		// Variables
		int					count;
		EventListener[]		list;
		String				name;
		int					index;

		// Get count of listeners
		count = getListenerCount(c);

		// Create Event Listener list
		list = new EventListener[count];

		// Construct List
		count = 0;
		name  = c.getName();
		for (index = 0; index < listenerList.length; index += 2) {
			if (((Class) listenerList[index]).getName().equals(name) == true) {
				list[count] = (EventListener) listenerList[index];
				count += 1;
			} // if
		} // for: index

		// Return List
		return list;

	} // getListeners()

	/**
	 * Remove a listener
	 * @param t Class type
	 * @param listener Listener to be removed
	 */
	public void remove(Class t, EventListener listener) {

		// Variables
		Object[]		list;
		int				index;
		Class			checkClass;
		EventListener	checkListener;
		int				pointer;
		boolean			found;

		// Create New list in anticipation that listener is not present
		if (listenerList.length == 0) {
			return;
		} // if
		list = new Object[listenerList.length - 2];

		// Search through list looking for listener
		pointer = 0;
		found = false;
		for (index = 0; index < listenerList.length - 2; index += 2) {
			checkClass = (Class) listenerList[index];
			checkListener = (EventListener) listenerList[index + 1];
			if (checkClass.equals(t) == false ||
				checkListener.equals(listener) == false) {
				list[pointer] = checkClass;
				list[pointer + 1] = checkListener;
				pointer += 2;
			} else {
				found = true;
			} // if
		} // for

		// Replace Listener List
		if (found == true) {
			listenerList = list;
		} // if

	} // remove()

	/**
	 * Get a string representation
	 * @returns String representation
	 */
	public String toString() {
		return null; // TODO
	} // toString()


} // EventListenerList
