/* ActionMap.java --
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

package javax.swing;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * ActionMap
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class ActionMap implements Serializable
{
  static final long serialVersionUID = -6277518704513986346L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * actionMap
	 */
	private Map actionMap = new HashMap();

	/**
	 * parent
	 */
	private ActionMap parent = null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor ActionMap
	 */
	public ActionMap() {
	} // ActionMap()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * get
	 * @param key TODO
	 * @returns Action
	 */
	public Action get(Object key) {

		// Variables
		Object	result;

		// Check Local store
		result = actionMap.get(key);

		// Check Parent
		if (result == null) {
			result = parent.get(key);
		} // if

		return (Action) result;

	} // get()

	/**
	 * put
	 * @param key TODO
	 * @param action TODO
	 */
	public void put(Object key, Action action) {
		if (action == null) {
			actionMap.remove(key);
		} else {
			actionMap.put(key, action);
		} // if
	} // put()

	/**
	 * remove
	 * @param key TODO
	 */
	public void remove(Object key) {
		actionMap.remove(key);
	} // remove()

	/**
	 * getParent
	 * @returns ActionMap
	 */
	public ActionMap getParent() {
		return parent;
	} // getParent()

	/**
	 * setParent
	 * @param parentMap TODO
	 */
	public void setParent(ActionMap parentMap) {
		parent = parentMap;
	} // setParent()

	/**
	 * size
	 * @returns int
	 */
	public int size() {
		return actionMap.size();
	} // size()

	/**
	 * clear
	 */
	public void clear() {
		actionMap.clear();
	} // clear()

	/**
	 * keys
	 * @returns Object[]
	 */
	public Object[] keys() {
		return convertSet(actionMap.keySet());
	} // keys()

	/**
	 * allKeys
	 * @returns Object[]
	 */
	public Object[] allKeys() {

		// Variables
		Set			set;

		// Initialize
		set = new HashSet();

		// Get Key Sets
		if (parent != null) {
			set.addAll(Arrays.asList(parent.allKeys()));
		} // if
		set.addAll(actionMap.keySet());

		return convertSet(set);

	} // allKeys()

	private Object[] convertSet(Set set) {

		// Variables
		int			index;
		Iterator	iterator;
		Object[]	keys;

		// Create Final array
		keys = new Object[set.size()];
		iterator = set.iterator();
		index = 0;
		while (iterator.hasNext()) {
			keys[index++] = iterator.next();
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
	private void writeObject(ObjectOutputStream value0) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * readObject
	 * @param stream TODO
	 * @exception ClassNotFoundException TODO
	 * @exception IOException TODO
	 */
	private void readObject(ObjectInputStream value0) throws ClassNotFoundException, IOException {
		// TODO
	} // readObject()


} // ActionMap
