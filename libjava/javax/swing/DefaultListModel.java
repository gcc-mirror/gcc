/* DefaultListModel.java --
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

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Vector;

/**
 * DefaultListModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultListModel extends AbstractListModel
{

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * elements.  Note: Sun obviously implemented the storage as a
	 * Vector according to the similar API on this class.  I choose
	 * instead to implement the model with a proper collection object.
	 * Is this a good choice?  Probably not (ya..I know there are
	 * sync issues by doing this)
	 */
	private ArrayList elements = new ArrayList();


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultListModel
	 */
	public DefaultListModel() {
		// TODO
	} // DefaultListModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * elementAt
	 * @param index TODO
	 * @returns Object
	 */
	public Object elementAt(int index) {
		return elements.get(index);
	} // elementAt()

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		return elements.toString();
	} // toString()

	/**
	 * indexOf
	 * @param element TODO
	 * @returns int
	 */
	public int indexOf(Object element) {
		return elements.indexOf(element);
	} // indexOf()

	/**
	 * indexOf
	 * @param element TODO
	 * @param startIndex TODO
	 * @returns int
	 */
	public int indexOf(Object element, int startIndex) {

		// Variables
		int		index;
		Object	test;

		// Process Elements
		for (index = startIndex; index < elements.size(); index++) {
			test = elements.get(index);
			if (test.equals(element) == true) {
				return index;
			} // if
		} // for
		return -1;

	} // indexOf()

	/**
	 * lastIndexOf
	 * @param element TODO
	 * @returns int
	 */
	public int lastIndexOf(Object element) {
		return elements.lastIndexOf(element);
	} // lastIndexOf()

	/**
	 * lastIndexOf
	 * @param element TODO
	 * @param endIndex TODO
	 * @returns int
	 */
	public int lastIndexOf(Object element, int endIndex) {

		// Variables
		int		index;
		Object	test;

		// Process Elements
		for (index = endIndex; index >= 0; index--) {
			test = elements.get(index);
			if (test.equals(element) == true) {
				return index;
			} // if
		} // for
		return -1;

	} // lastIndexOf()

	/**
	 * get
	 * @param index TODO
	 * @returns Object
	 */
	public Object get(int index) {
		return elements.get(index);
	} // get()

	/**
	 * set
	 * @param index TODO
	 * @param element TODO
	 * @returns Object
	 */
	public Object set(int index, Object element) {

		// Variables
		Object	result;

		// Process Action
		result = elements.set(index, element);

		// Send event
		fireContentsChanged(this, index, index);

		return result;

	} // set()

	/**
	 * add
	 * @param index TODO
	 * @param element TODO
	 */
	public void add(int index, Object element) {

		// Process Action
		elements.add(index, element);

		// Send event
		fireContentsChanged(this, index, index);

	} // add()

	/**
	 * addElement
	 * @param element TODO
	 */
	public void addElement(Object element) {

		// Process Action
		elements.add(element);

		// Send event
		fireIntervalAdded(this, elements.size(), elements.size());

	} // addElement()

	/**
	 * size
	 * @returns int
	 */
	public int size() {
		return elements.size();
	} // size()

	/**
	 * toArray
	 * @returns Object[]
	 */
	public Object[] toArray() {
		return elements.toArray();
	} // toArray()

	/**
	 * contains
	 * @param element TODO
	 * @returns boolean
	 */
	public boolean contains(Object element) {
		return elements.contains(element);
	} // contains()

	/**
	 * copyInto
	 * @param array TODO
	 */
	public void copyInto(Object[] array) {

		// Variables
		int		index;
		int		size;
		Object[]	srcArray;

		// Initialize
		size = size();
		srcArray = toArray();

		// Process Elements
		for (index = 0; index < size; index++) {
			array[index] = srcArray[index];
		} // for

	} // copyInto()

	/**
	 * clear
	 */
	public void clear() {

		// Process Action
		elements.clear();

		// Send event
		fireIntervalRemoved(this, 0, elements.size());

	} // clear()

	/**
	 * remove
	 * @param index TODO
	 * @returns Object
	 */
	public Object remove(int index) {

		// Variables
		Object	result;

		// Process Action
		result = elements.remove(index);

		// Send event
		fireIntervalRemoved(this, index, index);

		return result;

	} // remove()

	/**
	 * isEmpty
	 * @returns boolean
	 */
	public boolean isEmpty() {
		return elements.isEmpty();
	} // isEmpty()

	/**
	 * elements
	 * @returns Enumeration
	 */
	public Enumeration elements() {

		// TODO
		// Note: This is a pathetic implementation.  If Vector
		// was used for storage, this wouldn't be an issue.  I'll
		// have to implement an Enumeration inner class sometime.

		// Variables
		Vector	vector;

		// Get Enumeration
		vector = new Vector(elements);
		return vector.elements();

	} // elements()

	/**
	 * trimToSize
	 */
	public void trimToSize() {
		elements.trimToSize();
	} // trimToSize()

	/**
	 * ensureCapacity
	 * @param size TODO
	 */
	public void ensureCapacity(int size) {
		elements.ensureCapacity(size);
	} // ensureCapacity()

	/**
	 * setSize
	 * @param size TODO
	 */
	public void setSize(int size) {
		elements.ensureCapacity(size);
	} // setSize()

	/**
	 * capacity
	 * @returns int
	 */
	public int capacity() {
		return elements.size();
	} // capacity()

	/**
	 * firstElement
	 * @returns Object
	 */
	public Object firstElement() {

		// Variables
		Object	element;

		try {
			element = elements.get(0);
			return element;
		} catch (IndexOutOfBoundsException e) {
			throw new NoSuchElementException();
		} // try

	} // firstElement()

	/**
	 * lastElement
	 * @returns Object
	 */
	public Object lastElement() {

		// Variables
		Object	element;

		try {
			element = elements.get(elements.size() - 1);
			return element;
		} catch (ArrayIndexOutOfBoundsException e) {
			throw new NoSuchElementException();
		} // try

	} // lastElement()

	/**
	 * setElementAt
	 * @param element TODO
	 * @param index TODO
	 */
	public void setElementAt(Object element, int index) {

		// Process Action
		elements.set(index, element);

		// Send event
		fireContentsChanged(this, index, index);

	} // setElementAt()

	/**
	 * removeElementAt
	 * @param index TODO
	 */
	public void removeElementAt(int index) {

		// Process Action
		elements.remove(index);

		// Send event
		fireIntervalRemoved(this, index, index);

	} // removeElementAt()

	/**
	 * insertElementAt
	 * @param element TODO
	 * @param index TODO
	 */
	public void insertElementAt(Object element, int index) {

		// Process Action
		elements.add(index, element);

		// Send event
		fireIntervalRemoved(this, index, index);

	} // insertElementAt()

	/**
	 * removeElement
	 * @param element TODO
	 * @returns boolean
	 */
	public boolean removeElement(Object element) {

		// Variables
		int		index;

		index = elements.indexOf(element);
		if (index != -1) {
			elements.remove(index);

			// Send event
			fireIntervalRemoved(this, index, index);

			return true;

		} // if

		return false;

	} // removeElement()

	/**
	 * removeAllElements
	 */
	public void removeAllElements() {

		// Variables
		int		size;

		size = size();

		if (size > 0) {

			// Process Action
			elements.clear();

			// Send event
			fireIntervalRemoved(this, 0, size - 1);

		} // if

	} // removeAllElements()

	/**
	 * removeRange
	 * @param startIndex TODO
	 * @param endIndex TODO
	 */
	public void removeRange(int startIndex, int endIndex) {

		// Variables
		int		index;

		// Check Indices
		if (startIndex > endIndex) {
			throw new IllegalArgumentException();
		} // if

		// Process Elements
		for (index = endIndex; index >= startIndex; index--) {
			elements.remove(index);
		} // for

		// Send event
		fireIntervalRemoved(this, startIndex, endIndex);

	} // removeRange()

	/**
	 * getSize
	 * @returns int
	 */
	public int getSize() {
		return elements.size();
	} // getSize()

	/**
	 * getElementAt
	 * @param index TODO
	 * @returns Object
	 */
	public Object getElementAt(int index) {
		return elements.get(index);
	} // getElementAt()


} // DefaultListModel
