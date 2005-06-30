/* OverlayLayout.java --
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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager2;
import java.io.Serializable;

/**
 * OverlayLayout
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class OverlayLayout
  implements LayoutManager2, Serializable
{
  private static final long serialVersionUID = 18082829169631543L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * target
	 */
	private Container target;

	/**
	 * xChildren
	 */
	private SizeRequirements[] xChildren;

	/**
	 * yChildren
	 */
	private SizeRequirements[] yChildren;

	/**
	 * xTotal
	 */
	private SizeRequirements xTotal;

	/**
	 * yTotal
	 */
	private SizeRequirements yTotal;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor OverlayLayout
	 * @param target TODO
	 */
	public OverlayLayout(Container target) {
		// TODO
	} // OverlayLayout()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * invalidateLayout
	 * @param target TODO
	 */
	public void invalidateLayout(Container target) {
		// TODO
	} // invalidateLayout()

	/**
	 * addLayoutComponent
	 * @param string TODO
	 * @param component TODO
	 */
	public void addLayoutComponent(String string, Component component) {
		// TODO
	} // addLayoutComponent()

	/**
	 * addLayoutComponent
	 * @param component TODO
	 * @param constraints TODO
	 */
	public void addLayoutComponent(Component component, Object constraints) {
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
	 * preferredLayoutSize
	 * @param target TODO
	 * @returns Dimension
	 */
	public Dimension preferredLayoutSize(Container target) {
		return null; // TODO
	} // preferredLayoutSize()

	/**
	 * minimumLayoutSize
	 * @param target TODO
	 * @returns Dimension
	 */
	public Dimension minimumLayoutSize(Container target) {
		return null; // TODO
	} // minimumLayoutSize()

	/**
	 * maximumLayoutSize
	 * @param target TODO
	 * @returns Dimension
	 */
	public Dimension maximumLayoutSize(Container target) {
		return null; // TODO
	} // maximumLayoutSize()

	/**
	 * getLayoutAlignmentX
	 * @param target TODO
	 * @returns float
	 */
	public float getLayoutAlignmentX(Container target) {
		return (float) 0.0; // TODO
	} // getLayoutAlignmentX()

	/**
	 * getLayoutAlignmentY
	 * @param target TODO
	 * @returns float
	 */
	public float getLayoutAlignmentY(Container target) {
		return (float) 0.0; // TODO
	} // getLayoutAlignmentY()

	/**
	 * layoutContainer
	 * @param target TODO
	 */
	public void layoutContainer(Container target) {
		// TODO
	} // layoutContainer()


} // OverlayLayout
