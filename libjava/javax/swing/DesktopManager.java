/* DesktopManager.java --
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

/**
 * DesktopManager
 * @author	Andrew Selkirk
 * @version	1.0
 */
public interface DesktopManager {

	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * openFrame
	 * @param frame TODO
	 */
	void openFrame(JInternalFrame frame);

	/**
	 * closeFrame
	 * @param frame TODO
	 */
	void closeFrame(JInternalFrame frame);

	/**
	 * maximizeFrame
	 * @param frame TODO
	 */
	void maximizeFrame(JInternalFrame frame);

	/**
	 * minimizeFrame
	 * @param frame TODO
	 */
	void minimizeFrame(JInternalFrame frame);

	/**
	 * iconifyFrame
	 * @param frame TODO
	 */
	void iconifyFrame(JInternalFrame frame);

	/**
	 * deiconifyFrame
	 * @param frame TODO
	 */
	void deiconifyFrame(JInternalFrame frame);

	/**
	 * activateFrame
	 * @param frame TODO
	 */
	void activateFrame(JInternalFrame vframe);

	/**
	 * deactivateFrame
	 * @param frame TODO
	 */
	void deactivateFrame(JInternalFrame frame);

	/**
	 * beginDraggingFrame
	 * @param frame TODO
	 */
	void beginDraggingFrame(JComponent frame);

	/**
	 * dragFrame
	 * @param frame TODO
	 * @param x TODO
	 * @param y TODO
	 */
	void dragFrame(JComponent frame, int x, int y);

	/**
	 * endDraggingFrame
	 * @param frame TODO
	 */
	void endDraggingFrame(JComponent frame);

	/**
	 * beginResizingFrame
	 * @param frame TODO
	 * @param direction TODO
	 */
	void beginResizingFrame(JComponent frame, int direction);

	/**
	 * resizeFrame
	 * @param frame TODO
	 * @param x TODO
	 * @param y TODO
	 * @param width TODO
	 * @param height TODO
	 */
	void resizeFrame(JComponent frame, int x, int y, 
					int width, int height);

	/**
	 * endResizingFrame
	 * @param frame TODO
	 */
	void endResizingFrame(JComponent frame);

	/**
	 * setBoundsForFrame
	 * @param frame TODO
	 * @param x TODO
	 * @param y TODO
	 * @param width TODO
	 * @param height TODO
	 */
	void setBoundsForFrame(JComponent frame, int x, int y, 
					int width, int height);


} // DesktopManager
