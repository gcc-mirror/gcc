/* CellRendererPane.java --
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
import java.awt.*;
import java.io.*;
import javax.accessibility.*;

/**
 * CellRendererPane
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class CellRendererPane extends Container implements Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleCellRendererPane
	 */
	protected class AccessibleCellRendererPane extends AccessibleAWTContainer {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleCellRendererPane
		 * @param component TODO
		 */
		protected AccessibleCellRendererPane(CellRendererPane component) {
			super();
			// TODO
		} // AccessibleCellRendererPane()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.PANEL;
		} // getAccessibleRole()


	} // AccessibleCellRendererPane


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * accessibleContext
	 */
	protected AccessibleContext accessibleContext = null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor CellRendererPane
	 */
	public CellRendererPane() {
		// TODO
	} // CellRendererPane()


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
	 * update
	 * @param graphics TODO
	 */
	public void update(Graphics graphics) {
		// TODO
	} // update()

	/**
	 * invalidate
	 */
	public void invalidate() {
		// TODO
	} // invalidate()

	/**
	 * paint
	 * @param graphics TODO
	 */
	public void paint(Graphics graphics) {
		// TODO
	} // paint()

	/**
	 * addImpl
	 * @param c TODO
	 * @param constraints TODO
	 * @param index TODO
	 */
	protected void addImpl(Component c, Object constraints, int index) {
		// TODO
	} // addImpl()

	/**
	 * paintComponent
	 * @param graphics TODO
	 * @param c TODO
	 * @param p TODO
	 * @param x TODO
	 * @param y TODO
	 * @param w TODO
	 * @param h TODO
	 * @param shouldValidate TODO
	 */
	public void paintComponent(Graphics graphics, Component c,
			Container p, int x, int y, int w, int h, 
			boolean shouldValidate) {
		// TODO
	} // paintComponent()

	/**
	 * paintComponent
	 * @param graphics TODO
	 * @param c TODO
	 * @param p TODO
	 * @param x TODO
	 * @param y TODO
	 * @param w TODO
	 * @param h TODO
	 */
	public void paintComponent(Graphics graphics, Component c,
			Container p, int x, int y, int w, int h) {
		// TODO
	} // paintComponent()

	/**
	 * paintComponent
	 * @param graphics TODO
	 * @param c TODO
	 * @param p TODO
	 * @param r TODO
	 */
	public void paintComponent(Graphics graphics, Component c,
			Container p, Rectangle r) {
		// TODO
	} // paintComponent()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleCellRendererPane(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // CellRendererPane
