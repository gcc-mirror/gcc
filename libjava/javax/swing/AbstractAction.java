/* AbstractAction.java --
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

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.HashMap;
import javax.swing.event.SwingPropertyChangeSupport;

/**
 * AbstractAction
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class AbstractAction
  implements Action, Cloneable, Serializable
{
  static final long serialVersionUID = -6803159439231523484L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * enabled
	 */
	protected boolean enabled = true;

	/**
	 * changeSupport
	 */
	protected SwingPropertyChangeSupport changeSupport =
				new SwingPropertyChangeSupport(this);

	/**
	 * store
	 */
	private transient HashMap store = new HashMap();


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor AbstractAction
	 */
	public AbstractAction() {
		this(""); // TODO: default name
	} // AbstractAction()

	/**
	 * Constructor AbstractAction
	 * @param name TODO
	 */
	public AbstractAction(String name) {
		this(name, null); // TODO: default icon??
	} // AbstractAction()

	/**
	 * Constructor AbstractAction
	 * @param name TODO
	 * @param icon TODO
	 */
	public AbstractAction(String name, Icon icon) {
		putValue(NAME, name);
		putValue(SMALL_ICON, icon);
	} // AbstractAction()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * readObject
	 * @param stream TODO
	 * @exception ClassNotFoundException TODO
	 * @exception IOException TODO
	 */
	private void readObject(ObjectInputStream stream) 
			throws ClassNotFoundException, IOException {
		// TODO
	} // readObject()

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * clone
	 * @exception CloneNotSupportedException TODO
	 * @returns Object
	 */
	protected Object clone() throws CloneNotSupportedException {
		// What to do??
		return null;
	} // clone()

	/**
	 * getValue
	 * @param key TODO
	 * @returns Object
	 */
	public Object getValue(String key) {
		return store.get(key);
	} // getValue()

	/**
	 * putValue
	 * @param key TODO
	 * @param value TODO
	 */
	public void putValue(String key, Object value) {
		store.put(key, value);
	} // putValue()

	/**
	 * isEnabled
	 * @returns boolean
	 */
	public boolean isEnabled() {
		return enabled;
	} // isEnabled()

	/**
	 * setEnabled
	 * @param enabled TODO
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	} // setEnabled()

	/**
	 * getKeys
	 * @returns Object[]
	 */
	public Object[] getKeys() {
		return store.keySet().toArray();
	} // getKeys()

	/**
	 * firePropertyChange
	 * @param propertyName TODO
	 * @param oldValue TODO
	 * @param newValue TODO
	 */
	protected void firePropertyChange(String propertyName,
			Object oldValue, Object newValue) {
		changeSupport.firePropertyChange(propertyName, oldValue, newValue);
	} // firePropertyChange()

	/**
	 * addPropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void addPropertyChangeListener(PropertyChangeListener listener) {
		changeSupport.addPropertyChangeListener(listener);
	} // addPropertyChangeListener()

	/**
	 * removePropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void removePropertyChangeListener(PropertyChangeListener listener) {
		changeSupport.removePropertyChangeListener(listener);
	} // removePropertyChangeListener()
}
