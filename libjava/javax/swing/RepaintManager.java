/* RepaintManager.java --
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Rectangle;
import java.util.Hashtable;
import java.util.Vector;

/**
 * RepaintManager
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class RepaintManager {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * dirtyComponents
	 */
	Hashtable dirtyComponents;

	/**
	 * tmpDirtyComponents
	 */
	Hashtable tmpDirtyComponents;

	/**
	 * invalidComponents
	 */
	Vector invalidComponents;

	/**
	 * doubleBufferingEnabled
	 */
	boolean doubleBufferingEnabled;

	/**
	 * doubleBuffer
	 */
	Image doubleBuffer;

	/**
	 * doubleBufferSize
	 */
	Dimension doubleBufferSize;

	/**
	 * doubleBufferMaxSize
	 */
	private Dimension doubleBufferMaxSize;

	/**
	 * resetDoubleBuffer
	 */
	private boolean resetDoubleBuffer;

	/**
	 * repaintManagerKey
	 */
	private static final Object repaintManagerKey = null; // TODO

	/**
	 * tmp
	 */
	Rectangle tmp;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor RepaintManager
	 */
	public RepaintManager() {
		// TODO
	} // RepaintManager()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * toString
	 * @returns String
	 */
	public synchronized String toString() {
		return null; // TODO
	} // toString()

	/**
	 * currentManager
	 * @param component TODO
	 * @returns RepaintManager
	 */
	public static RepaintManager currentManager(Component component) {
		return null; // TODO
	} // currentManager()

	/**
	 * currentManager
	 * @param component TODO
	 * @returns RepaintManager
	 */
	public static RepaintManager currentManager(JComponent component) {
		return null; // TODO
	} // currentManager()

	/**
	 * setCurrentManager
	 * @param manager TODO
	 */
	public static void setCurrentManager(RepaintManager manager) {
		// TODO
	} // setCurrentManager()

	/**
	 * addInvalidComponent
	 * @param component TODO
	 */
	public synchronized void addInvalidComponent(JComponent component) {
		// TODO
	} // addInvalidComponent()

	/**
	 * removeInvalidComponent
	 * @param component TODO
	 */
	public synchronized void removeInvalidComponent(JComponent component) {
		// TODO
	} // removeInvalidComponent()

	/**
	 * addDirtyRegion
	 * @param component TODO
	 * @param x TODO
	 * @param y TODO
	 * @param w TODO
	 * @param h TODO
	 */
	public synchronized void addDirtyRegion(JComponent component, int x,
			int y, int w, int h) {
		// TODO
	} // addDirtyRegion()

	/**
	 * getDirtyRegion
	 * @param component TODO
	 * @returns Rectangle
	 */
	public Rectangle getDirtyRegion(JComponent component) {
		return null; // TODO
	} // getDirtyRegion()

	/**
	 * markCompletelyDirty
	 * @param component TODO
	 */
	public void markCompletelyDirty(JComponent component) {
		// TODO
	} // markCompletelyDirty()

	/**
	 * markCompletelyClean
	 * @param component TODO
	 */
	public void markCompletelyClean(JComponent component) {
		// TODO
	} // markCompletelyClean()

	/**
	 * isCompletelyDirty
	 * @param component TODO
	 * @returns boolean
	 */
	public boolean isCompletelyDirty(JComponent component) {
		return false; // TODO
	} // isCompletelyDirty()

	/**
	 * validateInvalidComponents
	 */
	public void validateInvalidComponents() {
		// TODO
	} // validateInvalidComponents()

	/**
	 * paintDirtyRegions
	 */
	public void paintDirtyRegions() {
		// TODO
	} // paintDirtyRegions()

	/**
	 * getOffscreenBuffer
	 * @param component TODO
	 * @param proposedWidth TODO
	 * @param proposedHeight TODO
	 * @returns Image
	 */
	public Image getOffscreenBuffer(Component component,
			int proposedWidth, int proposedHeight) {
		return null; // TODO
	} // getOffscreenBuffer()

	/**
	 * getDoubleBufferMaximumSize
	 * @returns Dimension
	 */
	public Dimension getDoubleBufferMaximumSize() {
		return null; // TODO
	} // getDoubleBufferMaximumSize()

	/**
	 * setDoubleBufferMaximumSize
	 * @param size TODO
	 */
	public void setDoubleBufferMaximumSize(Dimension size) {
		// TODO
	} // setDoubleBufferMaximumSize()

	/**
	 * setDoubleBufferingEnabled
	 * @param buffer TODO
	 */
	public void setDoubleBufferingEnabled(boolean buffer) {
		// TODO
	} // setDoubleBufferingEnabled()

	/**
	 * isDoubleBufferingEnabled
	 * @returns boolean
	 */
	public boolean isDoubleBufferingEnabled() {
		return false; // TODO
	} // isDoubleBufferingEnabled()


} // RepaintManager
