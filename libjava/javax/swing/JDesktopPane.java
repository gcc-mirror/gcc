/* JDesktopPane.java --
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
import javax.swing.plaf.DesktopPaneUI;

/**
 * JDesktopPane
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JDesktopPane extends JLayeredPane implements Accessible
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJDesktopPane
	 */
	protected class AccessibleJDesktopPane extends AccessibleJComponent {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJDesktopPane
		 * @param component TODO
		 */
		protected AccessibleJDesktopPane(JDesktopPane component) {
			super(component);
			// TODO
		} // AccessibleJDesktopPane()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.DESKTOP_PANE;
		} // getAccessibleRole()


	} // AccessibleJDesktopPane


	//-------------------------------------------------------------
	// Constants --------------------------------------------------
	//-------------------------------------------------------------
	
	/**
	 * LIVE_DRAG_MODE
	 */
	public static int LIVE_DRAG_MODE = 0;

	/**
	 * OUTLINE_DRAG_MODE
	 */
	public static int OUTLINE_DRAG_MODE = 1;

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "DesktopPaneUI";


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * selectedFrame
	 */
	private transient JInternalFrame selectedFrame;

        /**
         * desktopManager
         */
	private transient DesktopManager desktopManager;


	/**
	 * dragMode
	 */
	private int dragMode;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JDesktopPane
	 */
	public JDesktopPane() {
		// TODO
	} // JDesktopPane()


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
	 * @returns DesktopPaneUI
	 */
	public DesktopPaneUI getUI() {
		return (DesktopPaneUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(DesktopPaneUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * setDragMode
	 * @param mode TODO
	 */
	public void setDragMode(int mode) {
		this.dragMode = mode;
		// TODO
	} // setDragMode()

	/**
	 * getDragMode
	 * @returns int
	 */
	public int getDragMode() {
		return dragMode;
	} // getDragMode()

	/**
	 * getDesktopManager
	 * @returns DesktopManager
	 */
	public DesktopManager getDesktopManager() {
		return desktopManager;
	} // getDesktopManager()

	/**
	 * setDesktopManager
	 * @param manager TODO
	 */
	public void setDesktopManager(DesktopManager manager) {
		this.desktopManager = manager;
		// TODO
	} // setDesktopManager()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((DesktopPaneUI) UIManager.get(this));
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
	 * getAllFrames
	 * @returns JInternalFrame[]
	 */
	public JInternalFrame[] getAllFrames() {
		return null; // TODO
	} // getAllFrames()

	/**
	 * getSelectedFrame
	 * @returns JInternalFrame
	 */
	public JInternalFrame getSelectedFrame() {
		return null; // TODO
	} // getSelectedFrame()

	/**
	 * setSelectedFrame
	 * @param frame TODO
	 */
	public void setSelectedFrame(JInternalFrame frame) {
		// TODO
	} // setSelectedFrame()

	/**
	 * getAllFramesInLayer
	 * @param layer TODO
	 * @returns JInternalFrame[]
	 */
	public JInternalFrame[] getAllFramesInLayer(int layer) {
		return null; // TODO
	} // getAllFramesInLayer()

	/**
	 * isOpaque
	 * @returns boolean
	 */
	public boolean isOpaque() {
		return true;
	} // isOpaque()

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
			accessibleContext = new AccessibleJDesktopPane(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JDesktopPane
