/* InputMap.java --
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
import java.util.*;
import java.io.*;

/**
 * InputMap
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class InputMap implements Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * inputMap
	 */
	private Map inputMap = new HashMap();

	/**
	 * parent
	 */
	private InputMap parent = null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor InputMap
	 */
	public InputMap() {
		// TODO
	} // InputMap()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * get
	 * @param value0 TODO
	 * @returns Object
	 */
	public Object get(KeyStroke keystroke) {

		// Variables
		Object	result;

		// Check Local store
		result = inputMap.get(keystroke);

		// Check Parent
		if (result == null) {
			result = parent.get(keystroke);
		} // if

		return result;

	} // get()

	/**
	 * put
	 * @param keystroke TODO
	 * @param actionMapKey TODO
	 */
	public void put(KeyStroke keystroke, Object actionMapKey) {
		if (actionMapKey == null) {
			inputMap.remove(keystroke);
		} else {
			inputMap.put(keystroke, actionMapKey);
		} // if
	} // put()

	/**
	 * remove
	 * @param keystroke TODO
	 */
	public void remove(KeyStroke keystroke) {
		inputMap.remove(keystroke);
	} // remove()

	/**
	 * getParent
	 * @returns InputMap
	 */
	public InputMap getParent() {
		return parent;
	} // getParent()

	/**
	 * setParent
	 * @param parentMap TODO
	 */
	public void setParent(InputMap parentMap) {
		parent = parentMap;
	} // setParent()

	/**
	 * size
	 * @returns int
	 */
	public int size() {
		return inputMap.size();
	} // size()

	/**
	 * clear
	 */
	public void clear() {
		inputMap.clear();
	} // clear()

	/**
	 * keys
	 * @returns KeyStroke[]
	 */
	public KeyStroke[] keys() {
		return convertSet(inputMap.keySet());
	} // keys()

	/**
	 * allKeys
	 * @returns KeyStroke[]
	 */
	public KeyStroke[] allKeys() {

		// Variables
		Set			set;

		// Initialize
		set = new HashSet();

		// Get Key Sets
		if (parent != null) {
			set.addAll(Arrays.asList(parent.allKeys()));
		} // if
		set.addAll(inputMap.keySet());

		return convertSet(set);

	} // allKeys()

	private KeyStroke[] convertSet(Set set) {

		// Variables
		int			index;
		Iterator	iterator;
		KeyStroke[]	keys;

		// Create Final array
		keys = new KeyStroke[set.size()];
		iterator = set.iterator();
		index = 0;
		while (iterator.hasNext()) {
			keys[index++] = (KeyStroke) iterator.next();
		} // while

		return keys;

	} // convertSet()


	//-------------------------------------------------------------
	// Interface: Serializable ------------------------------------
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


} // InputMap
