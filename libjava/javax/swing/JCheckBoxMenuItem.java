/* JCheckBoxMenuItem.java --
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
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * JCheckBoxMenuItem
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JCheckBoxMenuItem extends JMenuItem implements SwingConstants, Accessible
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJCheckBoxMenuItem
	 */
	protected class AccessibleJCheckBoxMenuItem extends AccessibleJMenuItem {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJCheckBoxMenuItem
		 * @param component TODO
		 */
		protected AccessibleJCheckBoxMenuItem(JCheckBoxMenuItem component) {
			super(component);
			// TODO
		} // AccessibleJCheckBoxMenuItem()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.CHECK_BOX;
		} // getAccessibleRole()


	} // AccessibleJCheckBoxMenuItem


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "CheckBoxMenuItemUI";


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JCheckBoxMenuItem
	 */
	public JCheckBoxMenuItem() {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param icon TODO
	 */
	public JCheckBoxMenuItem(Icon icon) {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param text TODO
	 */
	public JCheckBoxMenuItem(String text) {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param action TODO
	 */
	public JCheckBoxMenuItem(Action action) {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param text TODO
	 * @param icon TODO
	 */
	public JCheckBoxMenuItem(String text, Icon icon) {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param text TODO
	 * @param state TODO
	 */
	public JCheckBoxMenuItem(String text, boolean state) {
		// TODO
	} // JCheckBoxMenuItem()

	/**
	 * Constructor JCheckBoxMenuItem
	 * @param text TODO
	 * @param icon TODO
	 * @param state TODO
	 */
	public JCheckBoxMenuItem(String text, Icon icon, boolean state) {
		// TODO
	} // JCheckBoxMenuItem()


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
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getState
	 * @returns boolean
	 */
	public boolean getState() {
		return false; // TODO
	} // getState()

	/**
	 * setState
	 * @param state TODO
	 */
	public synchronized void setState(boolean state) {
		// TODO
	} // setState()

	/**
	 * getSelectedObjects
	 * @returns Object[]
	 */
	public Object[] getSelectedObjects() {
		return null; // TODO
	} // getSelectedObjects()

	/**
	 * requestFocus
	 */
	public void requestFocus() {
		// TODO
	} // requestFocus()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJCheckBoxMenuItem(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JCheckBoxMenuItem
