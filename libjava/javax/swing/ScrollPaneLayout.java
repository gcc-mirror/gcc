/* ScrollPaneLayout.java --
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
import javax.swing.plaf.*;

/**
 * ScrollPaneLayout
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class ScrollPaneLayout implements LayoutManager, ScrollPaneConstants, Serializable {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * UIResource
	 */
	public static class UIResource extends ScrollPaneLayout 
			implements javax.swing.plaf.UIResource {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor UIResource
		 */
		public UIResource() {
			// TODO
		} // UIResource()


	} // UIResource


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * viewport
	 */
	protected JViewport viewport;

	/**
	 * vsb
	 */
	protected JScrollBar vsb;

	/**
	 * hsb
	 */
	protected JScrollBar hsb;

	/**
	 * rowHead
	 */
	protected JViewport rowHead;

	/**
	 * colHead
	 */
	protected JViewport colHead;

	/**
	 * lowerLeft
	 */
	protected Component lowerLeft;

	/**
	 * lowerRight
	 */
	protected Component lowerRight;

	/**
	 * upperLeft
	 */
	protected Component upperLeft;

	/**
	 * upperRight
	 */
	protected Component upperRight;

	/**
	 * vsbPolicy
	 */
	protected int vsbPolicy;

	/**
	 * hsbPolicy
	 */
	protected int hsbPolicy;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor ScrollPaneLayout
	 */
	public ScrollPaneLayout() {
		// TODO
	} // ScrollPaneLayout()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * syncWithScrollPane
	 * @param scrollPane TODO
	 */
	public void syncWithScrollPane(JScrollPane scrollPane) {
		// TODO
	} // syncWithScrollPane()

	/**
	 * addSingletonComponent
	 * @param oldComponent TODO
	 * @param newComponent TODO
	 * @returns Component
	 */
	protected Component addSingletonComponent(Component oldComponent,
			Component newComponent) {
		return null; // TODO
	} // addSingletonComponent()

	/**
	 * addLayoutComponent
	 * @param string TODO
	 * @param component TODO
	 */
	public void addLayoutComponent(String string, Component component) {
		// TODO
	} // addLayoutComponent()

	/**
	 * removeLayoutComponent
	 * @param component TODO
	 */
	public void removeLayoutComponent(Component component) {
		// TODO
	} // removeLayoutComponent()

	/**
	 * getVerticalScrollBarPolicy
	 * @returns int
	 */
	public int getVerticalScrollBarPolicy() {
		return 0; // TODO
	} // getVerticalScrollBarPolicy()

	/**
	 * setVerticalScrollBarPolicy
	 * @param policy TODO
	 */
	public void setVerticalScrollBarPolicy(int policy) {
		// TODO
	} // setVerticalScrollBarPolicy()

	/**
	 * getHorizontalScrollBarPolicy
	 * @returns int
	 */
	public int getHorizontalScrollBarPolicy() {
		return 0; // TODO
	} // getHorizontalScrollBarPolicy()

	/**
	 * setHorizontalScrollBarPolicy
	 * @param policy TODO
	 */
	public void setHorizontalScrollBarPolicy(int policy) {
		// TODO
	} // setHorizontalScrollBarPolicy()

	/**
	 * getViewport
	 * @returns JViewport
	 */
	public JViewport getViewport() {
		return null; // TODO
	} // getViewport()

	/**
	 * getHorizontalScrollBar
	 * @returns JScrollBar
	 */
	public JScrollBar getHorizontalScrollBar() {
		return null; // TODO
	} // getHorizontalScrollBar()

	/**
	 * getVerticalScrollBar
	 * @returns JScrollBar
	 */
	public JScrollBar getVerticalScrollBar() {
		return null; // TODO
	} // getVerticalScrollBar()

	/**
	 * getRowHeader
	 * @returns JViewport
	 */
	public JViewport getRowHeader() {
		return null; // TODO
	} // getRowHeader()

	/**
	 * getColumnHeader
	 * @returns JViewport
	 */
	public JViewport getColumnHeader() {
		return null; // TODO
	} // getColumnHeader()

	/**
	 * getCorner
	 * @param key TODO
	 * @returns Component
	 */
	public Component getCorner(String key) {
		return null; // TODO
	} // getCorner()

	/**
	 * preferredLayoutSize
	 * @param parent TODO
	 * @returns Dimension
	 */
	public Dimension preferredLayoutSize(Container parent) {
		return null; // TODO
	} // preferredLayoutSize()

	/**
	 * minimumLayoutSize
	 * @param parent TODO
	 * @returns Dimension
	 */
	public Dimension minimumLayoutSize(Container parent) {
		return null; // TODO
	} // minimumLayoutSize()

	/**
	 * layoutContainer
	 * @param parent TODO
	 */
	public void layoutContainer(Container parent) {
		// TODO
	} // layoutContainer()

	/**
	 * getViewportBorderBounds
	 * @param value0 TODO
	 * @returns Rectangle
	 */
	public Rectangle getViewportBorderBounds(JScrollPane scrollPane) {
		return null; // TODO
	} // getViewportBorderBounds()


} // ScrollPaneLayout
