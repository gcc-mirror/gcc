/* FocusManager.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.DefaultKeyboardFocusManager;
import java.awt.event.KeyEvent;

/**
 * FocusManager
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class FocusManager extends DefaultKeyboardFocusManager
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * DisabledFocusManager
	 */
	static class DisabledFocusManager extends FocusManager {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor DisabledFocusManager
		 */
		DisabledFocusManager() {
			// TODO
		} // DisabledFocusManager()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * processKeyEvent
		 * @param component TODO
		 * @param event TODO
		 */
		public void processKeyEvent(Component component, KeyEvent event) {
			// TODO
		} // processKeyEvent()

		/**
		 * focusNextComponent
		 * @param component TODO
		 */
		public void focusNextComponent(Component component) {
			// TODO
		} // focusNextComponent()

		/**
		 * focusPreviousComponent
		 * @param value0 TODO
		 */
		public void focusPreviousComponent(Component value0) {
			// TODO
		} // focusPreviousComponent()


	} // DisabledFocusManager


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * FOCUS_MANAGER_CLASS_PROPERTY
	 */
	public static final String FOCUS_MANAGER_CLASS_PROPERTY = "FocusManagerClassName";


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor FocusManager
	 */
	public FocusManager() {
		// TODO
	} // FocusManager()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getCurrentManager
	 * @returns FocusManager
	 */
	public static FocusManager getCurrentManager() {
		return null; // TODO
	} // getCurrentManager()

	/**
	 * setCurrentManager
	 * @param manager TODO
	 */
	public static void setCurrentManager(FocusManager manager) {
		// TODO
	} // setCurrentManager()

	/**
	 * disableSwingFocusManager
	 * @deprecated 1.4
	 */
	public static void disableSwingFocusManager() {
		// TODO
	} // disableSwingFocusManager()

	/**
	 * isFocusManagerEnabled
	 * @return boolean
	 * @deprecated 1.4
	 */
	public static boolean isFocusManagerEnabled() {
		return false; // TODO
	} // isFocusManagerEnabled()

	/**
	 * processKeyEvent
	 * @param component TODO
	 * @param event TODO
	 */
	public abstract void processKeyEvent(Component component, KeyEvent event);

	/**
	 * focusNextComponent
	 * @param component TODO
	 */
	public abstract void focusNextComponent(Component component);

	/**
	 * focusPreviousComponent
	 * @param component TODO
	 */
	public abstract void focusPreviousComponent(Component component);


} // FocusManager
