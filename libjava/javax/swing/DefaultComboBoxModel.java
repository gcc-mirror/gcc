/* DefaultComboBoxModel.java --
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

/**
 * DefaultComboBoxModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultComboBoxModel extends AbstractListModel 
		implements MutableComboBoxModel, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * list
	 */
	private Vector list;

	/**
	 * selectedItem
	 */
	private Object selectedItem;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultComboBoxModel
	 */
	public DefaultComboBoxModel() {
		// TODO
	} // DefaultComboBoxModel()

	/**
	 * Constructor DefaultComboBoxModel
	 * @param items TODO
	 */
	public DefaultComboBoxModel(Object[] items) {
		// TODO
	} // DefaultComboBoxModel()

	/**
	 * Constructor DefaultComboBoxModel
	 * @param vector TODO
	 */
	public DefaultComboBoxModel(Vector vector) {
		// TODO
	} // DefaultComboBoxModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * addElement
	 * @param object TODO
	 */
	public void addElement(Object object) {
		// TODO
	} // addElement()

	/**
	 * removeElementAt
	 * @param index TODO
	 */
	public void removeElementAt(int index) {
		// TODO
	} // removeElementAt()

	/**
	 * insertElementAt
	 * @param object TODO
	 * @param index TODO
	 */
	public void insertElementAt(Object object, int index) {
		// TODO
	} // insertElementAt()

	/**
	 * removeElement
	 * @param object TODO
	 */
	public void removeElement(Object object) {
		// TODO
	} // removeElement()

	/**
	 * removeAllElements
	 */
	public void removeAllElements() {
		// TODO
	} // removeAllElements()

	/**
	 * getSize
	 * @returns int
	 */
	public int getSize() {
		return 0; // TODO
	} // getSize()

	/**
	 * setSelectedItem
	 * @param object TODO
	 */
	public void setSelectedItem(Object object) {
		// TODO
	} // setSelectedItem()

	/**
	 * getSelectedItem
	 * @returns Object
	 */
	public Object getSelectedItem() {
		return null; // TODO
	} // getSelectedItem()

	/**
	 * getElementAt
	 * @param index TODO
	 * @returns Object
	 */
	public Object getElementAt(int index) {
		return null; // TODO
	} // getElementAt()

	/**
	 * getIndexOf
	 * @param object TODO
	 * @returns int
	 */
	public int getIndexOf(Object object) {
		return 0; // TODO
	} // getIndexOf()


} // DefaultComboBoxModel
