/* JSeparator.java --
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
import javax.accessibility.*;
import javax.swing.plaf.*;

/**
 * JSeparator
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JSeparator extends JComponent 
		implements SwingConstants, Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJSeparator
	 */
	protected class AccessibleJSeparator extends AccessibleJComponent {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJSeparator
		 * @param component TODO
		 */
		protected AccessibleJSeparator(JSeparator component) {
			super(component);
			// TODO
		} // AccessibleJSeparator()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.SEPARATOR;
		} // getAccessibleRole()


	} // AccessibleJSeparator


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "SeparatorUI";

	/**
	 * orientation
	 */
	private int orientation;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JSeparator
	 */
	public JSeparator() {
		this(HORIZONTAL);
	} // JSeparator()

	/**
	 * Constructor JSeparator
	 * @param value0 TODO
	 */
	public JSeparator(int orientation) {
		this.orientation = orientation;
		// TODO
	} // JSeparator()


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
	 * getUI
	 * @returns SeparatorUI
	 */
	public SeparatorUI getUI() {
		return (SeparatorUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(SeparatorUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((SeparatorUI) UIManager.get(this));
		invalidate();
	} // updateUI()

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getOrientation
	 * @returns int
	 */
	public int getOrientation() {
		return orientation;
	} // getOrientation()

	/**
	 * setOrientation
	 * @param orientation TODO
	 */
	public void setOrientation(int orientation) {
		this.orientation = orientation;
		// TODO
	} // setOrientation()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * isFocusTraversable
	 * @returns boolean
	 */
	public boolean isFocusTraversable() {
		return false;
	} // isFocusTraversable()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJSeparator(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JSeparator
