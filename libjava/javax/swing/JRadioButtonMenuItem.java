/* JRadioButtonMenuItem.java --
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
 * JRadioButtonMenuItem
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JRadioButtonMenuItem extends JMenuItem implements Accessible
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJRadioButtonMenuItem
	 */
	protected class AccessibleJRadioButtonMenuItem extends AccessibleJMenuItem {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJRadioButtonMenuItem
		 * @param component TODO
		 */
		protected AccessibleJRadioButtonMenuItem(JRadioButtonMenuItem component) {
			super(component);
			// TODO
		} // AccessibleJRadioButtonMenuItem()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.RADIO_BUTTON;
		} // getAccessibleRole()


	} // AccessibleJRadioButtonMenuItem


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "RadioButtonMenuItemUI";


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JRadioButtonMenuItem
	 */
	public JRadioButtonMenuItem() {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param icon TODO
	 */
	public JRadioButtonMenuItem(Icon icon) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param text TODO
	 */
	public JRadioButtonMenuItem(String text) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param action TODO
	 */
	public JRadioButtonMenuItem(Action action) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param text TODO
	 * @param icon TODO
	 */
	public JRadioButtonMenuItem(String text, Icon icon) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param text TODO
	 * @param selected TODO
	 */
	public JRadioButtonMenuItem(String text, boolean selected) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param icon TODO
	 * @param selected TODO
	 */
	public JRadioButtonMenuItem(Icon icon, boolean selected) {
		// TODO
	} // JRadioButtonMenuItem()

	/**
	 * Constructor JRadioButtonMenuItem
	 * @param text TODO
	 * @param icon TODO
	 * @param selected TODO
	 */
	public JRadioButtonMenuItem(String text, Icon icon, boolean selected) {
		// TODO
	} // JRadioButtonMenuItem()


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
			accessibleContext = new AccessibleJRadioButtonMenuItem(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JRadioButtonMenuItem
