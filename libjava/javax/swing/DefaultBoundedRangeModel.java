/* DefaultBoundedRangeModel.java --
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
 * DefaultBoundedRangeModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultBoundedRangeModel implements BoundedRangeModel, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * changeEvent
	 */
	protected transient ChangeEvent changeEvent = new ChangeEvent(this);

	/**
	 * listenerList
	 */
	protected EventListenerList listenerList = new EventListenerList();

	/**
	 * value
	 */
	private int value;

	/**
	 * extent
	 */
	private int extent;

	/**
	 * minimum
	 */
	private int minimum;

	/**
	 * maximum
	 */
	private int maximum;

	/**
	 * isAdjusting
	 */
	private boolean isAdjusting;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultBoundedRangeModel
	 */
	public DefaultBoundedRangeModel() {
		setRangeProperties(0, 0, 0, 100, false);
	} // DefaultBoundedRangeModel()

	/**
	 * Constructor DefaultBoundedRangeModel
	 * @param value TODO
	 * @param extent TODO
	 * @param minimum TODO
	 * @param maximum TODO
	 */
	public DefaultBoundedRangeModel(int value, int extent,
				int minimum, int maximum) {
		setRangeProperties(value, extent, minimum, maximum, false);
	} // DefaultBoundedRangeModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		return null; // TODO
	} // toString()

	/**
	 * getValue
	 * @returns int
	 */
	public int getValue() {
		return value;
	} // getValue()

	/**
	 * setValue
	 * @param value TODO
	 */
	public void setValue(int value) {
	
		// Validate Constraints
		if (minimum > value || value > (value + extent) ||
			(value + extent) > maximum) {
			throw new IllegalArgumentException("Invalid value property set");
		} // if
		
		// Set Value
		this.value = value;

		// Notification
		fireStateChanged();

	} // setValue()

	/**
	 * getExtent
	 * @returns int
	 */
	public int getExtent() {
		return extent;
	} // getExtent()

	/**
	 * setExtent
	 * @param extent TODO
	 */
	public void setExtent(int extent) {

		// Validate Constraints
		if (minimum > value || value > (value + extent) ||
			(value + extent) > maximum) {
			throw new IllegalArgumentException("Invalid extent property set");
		} // if
		
		// Set Extent
		this.extent = extent;
		
		// Notification
		fireStateChanged();

	} // setExtent()

	/**
	 * getMinimum
	 * @returns int
	 */
	public int getMinimum() {
		return minimum;
	} // getMinimum()

	/**
	 * setMinimum
	 * @param minimum TODO
	 */
	public void setMinimum(int minimum) {
	
		// Validate Constraints
		if (minimum > value || value > (value + extent) ||
			(value + extent) > maximum) {
			throw new IllegalArgumentException("Invalid minimum property set");
		} // if
		
		// Set Minimum
		this.minimum = minimum;
		
		// Notification
		fireStateChanged();

	} // setMinimum()

	/**
	 * getMaximum
	 * @returns int
	 */
	public int getMaximum() {
		return maximum;
	} // getMaximum()

	/**
	 * setMaximum
	 * @param maximum TODO
	 */
	public void setMaximum(int maximum) {
	
		// Validate Constraints
		if (minimum > value || value > (value + extent) ||
			(value + extent) > maximum) {
			throw new IllegalArgumentException("Invalid maximum property set");
		} // if

		// Set Maximum
		this.maximum = maximum;

		// Notification
		fireStateChanged();

	} // setMaximum()

	/**
	 * getValueIsAdjusting
	 * @returns boolean
	 */
	public boolean getValueIsAdjusting() {
		return isAdjusting;
	} // getValueIsAdjusting()

	/**
	 * setValueIsAdjusting
	 * @param isAdjusting TODO
	 */
	public void setValueIsAdjusting(boolean isAdjusting) {
	
		// Set isAdjusting
		this.isAdjusting = isAdjusting;

		// Notification
		fireStateChanged();

	} // setValueIsAdjusting()

	/**
	 * setRangeProperties
	 * @param value TODO
	 * @param extent TODO
	 * @param minimum TODO
	 * @param maximum TODO
	 * @param isAdjusting TODO
	 */
	public void setRangeProperties(int value, int extent, int minimum,
			int maximum, boolean isAdjusting) {
			
		// Validate Constraints
		if (minimum > value || value > (value + extent) ||
			(value + extent) > maximum) {
			throw new IllegalArgumentException("Invalid property set");
		} // if

		// Set Data
		this.value = value;
		this.extent = extent;
		this.minimum = minimum;
		this.maximum = maximum;
		this.isAdjusting = isAdjusting;
		
		// Notification
		fireStateChanged();

	} // setRangeProperties()

	/**
	 * addChangeListener
	 * @param listener TODO
	 */
	public void addChangeListener(ChangeListener listener) {
		listenerList.add(ChangeListener.class, listener);
	} // addChangeListener()

	/**
	 * removeChangeListener
	 * @param listener TODO
	 */
	public void removeChangeListener(ChangeListener listener) {
		listenerList.remove(ChangeListener.class, listener);
	} // removeChangeListener()

	/**
	 * fireStateChanged
	 */
	protected void fireStateChanged() {

		// Variables
		ChangeListener			listener;
		EventListener[]			listeners;
		int						index;

		// Get Listeners
		listeners = listenerList.getListeners(ChangeListener.class);

		// Process Listeners
		for (index = 0; index < listeners.length; index++) {
			listener = (ChangeListener) listeners[index];
			listener.stateChanged(changeEvent);
		} // for

	} // fireStateChanged()

	/**
	 * getListeners
	 * @param c TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class c) {
		return listenerList.getListeners(c);
	} // getListeners()

	/**
	 * getChangeListeners
	 */
	public ChangeListener[] getChangeListeners()
	{
	  // FIXME: implement this
	  return null;
	}


} // DefaultBoundedRangeModel
