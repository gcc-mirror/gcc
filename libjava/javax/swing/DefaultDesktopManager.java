/* DefaultDesktopManager.java --
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

import java.awt.Rectangle;
import java.io.Serializable;

/**
 * DefaultDesktopManager
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultDesktopManager implements DesktopManager, Serializable
{
  static final long serialVersionUID = 4657624909838017887L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * HAS_BEEN_ICONIFIED_PROPERTY
	 */
	static final String HAS_BEEN_ICONIFIED_PROPERTY = ""; // TODO

	/**
	 * DEFAULT_DRAG_MODE
	 */
	static final int DEFAULT_DRAG_MODE = 0; // TODO

	/**
	 * OUTLINE_DRAG_MODE
	 */
	static final int OUTLINE_DRAG_MODE = 0; // TODO

	/**
	 * FASTER_DRAG_MODE
	 */
	static final int FASTER_DRAG_MODE = 0; // TODO

	/**
	 * dragMode
	 */
	int dragMode;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultDesktopManager
	 */
	public DefaultDesktopManager() {
		// TODO
	} // DefaultDesktopManager()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * openFrame
	 * @param frame TODO
	 */
	public void openFrame(JInternalFrame frame) {
		// TODO
	} // openFrame()

	/**
	 * closeFrame
	 * @param frame TODO
	 */
	public void closeFrame(JInternalFrame frame) {
		// TODO
	} // closeFrame()

	/**
	 * maximizeFrame
	 * @param frame TODO
	 */
	public void maximizeFrame(JInternalFrame frame) {
		// TODO
	} // maximizeFrame()

	/**
	 * minimizeFrame
	 * @param frame TODO
	 */
	public void minimizeFrame(JInternalFrame frame) {
		// TODO
	} // minimizeFrame()

	/**
	 * iconifyFrame
	 * @param frame TODO
	 */
	public void iconifyFrame(JInternalFrame frame) {
		// TODO
	} // iconifyFrame()

	/**
	 * deiconifyFrame
	 * @param frame TODO
	 */
	public void deiconifyFrame(JInternalFrame frame) {
		// TODO
	} // deiconifyFrame()

	/**
	 * activateFrame
	 * @param frame TODO
	 */
	public void activateFrame(JInternalFrame frame) {
		// TODO
	} // activateFrame()

	/**
	 * deactivateFrame
	 * @param frame TODO
	 */
	public void deactivateFrame(JInternalFrame frame) {
		// TODO
	} // deactivateFrame()

	/**
	 * beginDraggingFrame
	 * @param component TODO
	 */
	public void beginDraggingFrame(JComponent component) {
		// TODO
	} // beginDraggingFrame()

	/**
	 * dragFrame
	 * @param component TODO
	 * @param newX TODO
	 * @param newY TODO
	 */
	public void dragFrame(JComponent component, int newX, int newY) {
		// TODO
	} // dragFrame()

	/**
	 * endDraggingFrame
	 * @param component TODO
	 */
	public void endDraggingFrame(JComponent component) {
		// TODO
	} // endDraggingFrame()

	/**
	 * beginResizingFrame
	 * @param component TODO
	 * @param direction TODO
	 */
	public void beginResizingFrame(JComponent component, int direction) {
		// TODO
	} // beginResizingFrame()

	/**
	 * resizeFrame
	 * @param component TODO
	 * @param newX TODO
	 * @param newY TODO
	 * @param newWidth TODO
	 * @param newHeight TODO
	 */
	public void resizeFrame(JComponent component, int newX, int newY,
			int newWidth, int newHeight) {
		// TODO
	} // resizeFrame()

	/**
	 * endResizingFrame
	 * @param component TODO
	 */
	public void endResizingFrame(JComponent component) {
		// TODO
	} // endResizingFrame()

	/**
	 * setBoundsForFrame
	 * @param component TODO
	 * @param newX TODO
	 * @param newY TODO
	 * @param newWidth TODO
	 * @param newHeight TODO
	 */
	public void setBoundsForFrame(JComponent component, int newX,
			int newY, int newWidth, int newHeight) {
		// TODO
	} // setBoundsForFrame()

	/**
	 * removeIconFor
	 * @param frame TODO
	 */
	protected void removeIconFor(JInternalFrame frame) {
		// TODO
	} // removeIconFor()

	/**
	 * getBoundsForIconOf
	 * @param frame TODO
	 * @returns Rectangle
	 */
	protected Rectangle getBoundsForIconOf(JInternalFrame frame) {
		return null; // TODO
	} // getBoundsForIconOf()

	/**
	 * setPreviousBounds
	 * @param frame TODO
	 * @param rect TODO
	 */
	protected void setPreviousBounds(JInternalFrame frame, Rectangle rect) {
		// TODO
	} // setPreviousBounds()

	/**
	 * getPreviousBounds
	 * @param frame TODO
	 * @returns Rectangle
	 */
	protected Rectangle getPreviousBounds(JInternalFrame frame) {
		return null; // TODO
	} // getPreviousBounds()

	/**
	 * setWasIcon
	 * @param frame TODO
	 * @param value TODO
	 */
	protected void setWasIcon(JInternalFrame frame, Boolean value) {
		// TODO
	} // setWasIcon()

	/**
	 * wasIcon
	 * @param frame TODO
	 * @returns boolean
	 */
	protected boolean wasIcon(JInternalFrame frame) {
		return false; // TODO
	} // wasIcon()


} // DefaultDesktopManager
