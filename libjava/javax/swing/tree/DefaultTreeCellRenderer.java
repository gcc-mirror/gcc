/* DefaultTreeCellRenderer.java --
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

package javax.swing.tree;

// Imports
import java.awt.*;
import javax.swing.*;

/**
 * DefaultTreeCellRenderer
 * @author Andrew Selkirk
 */
public class DefaultTreeCellRenderer extends JLabel implements TreeCellRenderer {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * selected
	 */
	protected boolean selected;

	/**
	 * hasFocus
	 */
	protected boolean hasFocus;

	/**
	 * drawsFocusBorderAroundIcon
	 */
	private boolean drawsFocusBorderAroundIcon;

	/**
	 * closedIcon
	 */
	protected transient Icon closedIcon;

	/**
	 * leafIcon
	 */
	protected transient Icon leafIcon;

	/**
	 * openIcon
	 */
	protected transient Icon openIcon;

	/**
	 * textSelectionColor
	 */
	protected Color textSelectionColor;

	/**
	 * textNonSelectionColor
	 */
	protected Color textNonSelectionColor;

	/**
	 * backgroundSelectionColor
	 */
	protected Color backgroundSelectionColor;

	/**
	 * backgroundNonSelectionColor
	 */
	protected Color backgroundNonSelectionColor;

	/**
	 * borderSelectionColor
	 */
	protected Color borderSelectionColor;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTreeCellRenderer
	 */
	public DefaultTreeCellRenderer() {
		// TODO
	} // DefaultTreeCellRenderer()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getDefaultOpenIcon
	 * @returns Icon
	 */
	public Icon getDefaultOpenIcon() {
		return null; // TODO
	} // getDefaultOpenIcon()

	/**
	 * getDefaultClosedIcon
	 * @returns Icon
	 */
	public Icon getDefaultClosedIcon() {
		return null; // TODO
	} // getDefaultClosedIcon()

	/**
	 * getDefaultLeafIcon
	 * @returns Icon
	 */
	public Icon getDefaultLeafIcon() {
		return null; // TODO
	} // getDefaultLeafIcon()

	/**
	 * setOpenIcon
	 * @param value0 TODO
	 */
	public void setOpenIcon(Icon value0) {
		// TODO
	} // setOpenIcon()

	/**
	 * getOpenIcon
	 * @returns Icon
	 */
	public Icon getOpenIcon() {
		return null; // TODO
	} // getOpenIcon()

	/**
	 * setClosedIcon
	 * @param value0 TODO
	 */
	public void setClosedIcon(Icon value0) {
		// TODO
	} // setClosedIcon()

	/**
	 * getClosedIcon
	 * @returns Icon
	 */
	public Icon getClosedIcon() {
		return null; // TODO
	} // getClosedIcon()

	/**
	 * setLeafIcon
	 * @param value0 TODO
	 */
	public void setLeafIcon(Icon value0) {
		// TODO
	} // setLeafIcon()

	/**
	 * getLeafIcon
	 * @returns Icon
	 */
	public Icon getLeafIcon() {
		return null; // TODO
	} // getLeafIcon()

	/**
	 * setTextSelectionColor
	 * @param value0 TODO
	 */
	public void setTextSelectionColor(Color value0) {
		// TODO
	} // setTextSelectionColor()

	/**
	 * getTextSelectionColor
	 * @returns Color
	 */
	public Color getTextSelectionColor() {
		return null; // TODO
	} // getTextSelectionColor()

	/**
	 * setTextNonSelectionColor
	 * @param value0 TODO
	 */
	public void setTextNonSelectionColor(Color value0) {
		// TODO
	} // setTextNonSelectionColor()

	/**
	 * getTextNonSelectionColor
	 * @returns Color
	 */
	public Color getTextNonSelectionColor() {
		return null; // TODO
	} // getTextNonSelectionColor()

	/**
	 * setBackgroundSelectionColor
	 * @param value0 TODO
	 */
	public void setBackgroundSelectionColor(Color value0) {
		// TODO
	} // setBackgroundSelectionColor()

	/**
	 * getBackgroundSelectionColor
	 * @returns Color
	 */
	public Color getBackgroundSelectionColor() {
		return null; // TODO
	} // getBackgroundSelectionColor()

	/**
	 * setBackgroundNonSelectionColor
	 * @param value0 TODO
	 */
	public void setBackgroundNonSelectionColor(Color value0) {
		// TODO
	} // setBackgroundNonSelectionColor()

	/**
	 * getBackgroundNonSelectionColor
	 * @returns Color
	 */
	public Color getBackgroundNonSelectionColor() {
		return null; // TODO
	} // getBackgroundNonSelectionColor()

	/**
	 * setBorderSelectionColor
	 * @param value0 TODO
	 */
	public void setBorderSelectionColor(Color value0) {
		// TODO
	} // setBorderSelectionColor()

	/**
	 * getBorderSelectionColor
	 * @returns Color
	 */
	public Color getBorderSelectionColor() {
		return null; // TODO
	} // getBorderSelectionColor()

	/**
	 * setFont
	 * @param value0 TODO
	 */
	public void setFont(Font value0) {
		// TODO
	} // setFont()

	/**
	 * setBackground
	 * @param value0 TODO
	 */
	public void setBackground(Color value0) {
		// TODO
	} // setBackground()

	/**
	 * getTreeCellRendererComponent
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 * @param value4 TODO
	 * @param value5 TODO
	 * @param value6 TODO
	 * @returns Component
	 */
	public Component getTreeCellRendererComponent(JTree value0, Object value1, boolean value2, boolean value3, boolean value4, int value5, boolean value6) {
		return null; // TODO
	} // getTreeCellRendererComponent()

	/**
	 * paint
	 * @param value0 TODO
	 */
	public void paint(Graphics value0) {
		// TODO
	} // paint()

	/**
	 * getPreferredSize
	 * @returns Dimension
	 */
	public Dimension getPreferredSize() {
		return null; // TODO
	} // getPreferredSize()

	/**
	 * validate
	 */
	public void validate() {
		// TODO
	} // validate()

	/**
	 * revalidate
	 */
	public void revalidate() {
		// TODO
	} // revalidate()

	/**
	 * repaint
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 * @param value4 TODO
	 */
	public void repaint(long value0, int value1, int value2, int value3, int value4) {
		// TODO
	} // repaint()

	/**
	 * repaint
	 * @param value0 TODO
	 */
	public void repaint(Rectangle value0) {
		// TODO
	} // repaint()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	protected void firePropertyChange(String value0, Object value1, Object value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, byte value1, byte value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, char value1, char value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, short value1, short value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, int value1, int value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, long value1, long value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, float value1, float value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, double value1, double value2) {
		// TODO
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void firePropertyChange(String value0, boolean value1, boolean value2) {
		// TODO
	} // firePropertyChange()


} // DefaultTreeCellRenderer
