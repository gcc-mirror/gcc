/* JSplitPane.java --
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

import java.awt.Component;
import java.awt.Graphics;
import java.io.IOException;
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.plaf.SplitPaneUI;

/**
 * JSplitPane
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JSplitPane extends JComponent implements Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJSplitPane
	 */
	protected class AccessibleJSplitPane extends AccessibleJComponent 
			implements AccessibleValue {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJSplitPane
		 * @param component TODO
		 */
		protected AccessibleJSplitPane(JSplitPane component) {
			super(component);
			// TODO
		} // AccessibleJSplitPane()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleStateSet
		 * @returns AccessibleStateSet
		 */
		public AccessibleStateSet getAccessibleStateSet() {
			return null; // TODO
		} // getAccessibleStateSet()

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.SPLIT_PANE;
		} // getAccessibleRole()

		/**
		 * getAccessibleValue
		 * @returns AccessibleValue
		 */
		public AccessibleValue getAccessibleValue() {
			return null; // TODO
		} // getAccessibleValue()

		/**
		 * getCurrentAccessibleValue
		 * @returns Number
		 */
		public Number getCurrentAccessibleValue() {
			return null; // TODO
		} // getCurrentAccessibleValue()

		/**
		 * setCurrentAccessibleValue
		 * @param value0 TODO
		 * @returns boolean
		 */
		public boolean setCurrentAccessibleValue(Number value0) {
			return false; // TODO
		} // setCurrentAccessibleValue()

		/**
		 * getMinimumAccessibleValue
		 * @returns Number
		 */
		public Number getMinimumAccessibleValue() {
			return null; // TODO
		} // getMinimumAccessibleValue()

		/**
		 * getMaximumAccessibleValue
		 * @returns Number
		 */
		public Number getMaximumAccessibleValue() {
			return null; // TODO
		} // getMaximumAccessibleValue()


	} // AccessibleJSplitPane


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "SplitPaneUI";

	/**
	 * VERTICAL_SPLIT
	 */
	public static final int VERTICAL_SPLIT = 0;

	/**
	 * HORIZONTAL_SPLIT
	 */
	public static final int HORIZONTAL_SPLIT = 1;

	/**
	 * LEFT
	 */
	public static final String LEFT = "left";

	/**
	 * RIGHT
	 */
	public static final String RIGHT = "right";

	/**
	 * TOP
	 */
	public static final String TOP = "top";

	/**
	 * BOTTOM
	 */
	public static final String BOTTOM = "bottom";

	/**
	 * DIVIDER
	 */
	public static final String DIVIDER = "divider";

	/**
	 * ORIENTATION_PROPERTY
	 */
	public static final String ORIENTATION_PROPERTY = "orientation";

	/**
	 * CONTINUOUS_LAYOUT_PROPERTY
	 */
	public static final String CONTINUOUS_LAYOUT_PROPERTY = "continuousLayout";

	/**
	 * DIVIDER_SIZE_PROPERTY
	 */
	public static final String DIVIDER_SIZE_PROPERTY = "dividerSize";

	/**
	 * ONE_TOUCH_EXPANDABLE_PROPERTY
	 */
	public static final String ONE_TOUCH_EXPANDABLE_PROPERTY = "oneTouchExpandable";

	/**
	 * LAST_DIVIDER_LOCATION_PROPERTY
	 */
	public static final String LAST_DIVIDER_LOCATION_PROPERTY = "lastDividerLocation";

	/**
	 * DIVIDER_LOCATION_PROPERTY
	 */
	public static final String DIVIDER_LOCATION_PROPERTY = "dividerLocation";

	/**
	 * RESIZE_WEIGHT_PROPERTY
	 */
	public static final String RESIZE_WEIGHT_PROPERTY = "resizeWeight";

	/**
	 * orientation
	 */
	protected int orientation;

	/**
	 * continuousLayout
	 */
	protected boolean continuousLayout;

	/**
	 * leftComponent
	 */
	protected Component leftComponent;

	/**
	 * rightComponent
	 */
	protected Component rightComponent;

	/**
	 * dividerSize
	 */
	protected int dividerSize;

	/**
	 * oneTouchExpandable
	 */
	protected boolean oneTouchExpandable;

	/**
	 * lastDividerLocation
	 */
	protected int lastDividerLocation;

	/**
	 * resizeWeight
	 */
	private double resizeWeight;

	/**
	 * dividerLocation
	 */
	private int dividerLocation;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JSplitPane
	 */
	public JSplitPane() {
		// TODO
	} // JSplitPane()

	/**
	 * Constructor JSplitPane
	 * @param value0 TODO
	 */
	public JSplitPane(int value0) {
		// TODO
	} // JSplitPane()

	/**
	 * Constructor JSplitPane
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public JSplitPane(int value0, boolean value1) {
		// TODO
	} // JSplitPane()

	/**
	 * Constructor JSplitPane
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public JSplitPane(int value0, Component value1, Component value2) {
		// TODO
	} // JSplitPane()

	/**
	 * Constructor JSplitPane
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 */
	public JSplitPane(int value0, boolean value1, Component value2, Component value3) {
		// TODO
	} // JSplitPane()


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
	 * remove
	 * @param value0 TODO
	 */
	public void remove(Component value0) {
		// TODO
	} // remove()

	/**
	 * remove
	 * @param value0 TODO
	 */
	public void remove(int value0) {
		// TODO
	} // remove()

	/**
	 * removeAll
	 */
	public void removeAll() {
		// TODO
	} // removeAll()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(SplitPaneUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * getUI
	 * @returns SplitPaneUI
	 */
	public SplitPaneUI getUI() {
		return (SplitPaneUI) ui;
	} // getUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((SplitPaneUI) UIManager.get(this));
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
	 * setDividerSize
	 * @param value0 TODO
	 */
	public void setDividerSize(int value0) {
		// TODO
	} // setDividerSize()

	/**
	 * getDividerSize
	 * @returns int
	 */
	public int getDividerSize() {
		return 0; // TODO
	} // getDividerSize()

	/**
	 * setLeftComponent
	 * @param value0 TODO
	 */
	public void setLeftComponent(Component value0) {
		// TODO
	} // setLeftComponent()

	/**
	 * getLeftComponent
	 * @returns Component
	 */
	public Component getLeftComponent() {
		return null; // TODO
	} // getLeftComponent()

	/**
	 * setTopComponent
	 * @param value0 TODO
	 */
	public void setTopComponent(Component value0) {
		// TODO
	} // setTopComponent()

	/**
	 * getTopComponent
	 * @returns Component
	 */
	public Component getTopComponent() {
		return null; // TODO
	} // getTopComponent()

	/**
	 * setRightComponent
	 * @param value0 TODO
	 */
	public void setRightComponent(Component value0) {
		// TODO
	} // setRightComponent()

	/**
	 * getRightComponent
	 * @returns Component
	 */
	public Component getRightComponent() {
		return null; // TODO
	} // getRightComponent()

	/**
	 * setBottomComponent
	 * @param value0 TODO
	 */
	public void setBottomComponent(Component value0) {
		// TODO
	} // setBottomComponent()

	/**
	 * getBottomComponent
	 * @returns Component
	 */
	public Component getBottomComponent() {
		return null; // TODO
	} // getBottomComponent()

	/**
	 * setOneTouchExpandable
	 * @param value0 TODO
	 */
	public void setOneTouchExpandable(boolean value0) {
		// TODO
	} // setOneTouchExpandable()

	/**
	 * isOneTouchExpandable
	 * @returns boolean
	 */
	public boolean isOneTouchExpandable() {
		return false; // TODO
	} // isOneTouchExpandable()

	/**
	 * setLastDividerLocation
	 * @param value0 TODO
	 */
	public void setLastDividerLocation(int value0) {
		// TODO
	} // setLastDividerLocation()

	/**
	 * getLastDividerLocation
	 * @returns int
	 */
	public int getLastDividerLocation() {
		return 0; // TODO
	} // getLastDividerLocation()

	/**
	 * setOrientation
	 * @param value0 TODO
	 */
	public void setOrientation(int value0) {
		// TODO
	} // setOrientation()

	/**
	 * getOrientation
	 * @returns int
	 */
	public int getOrientation() {
		return 0; // TODO
	} // getOrientation()

	/**
	 * setContinuousLayout
	 * @param value0 TODO
	 */
	public void setContinuousLayout(boolean value0) {
		// TODO
	} // setContinuousLayout()

	/**
	 * isContinuousLayout
	 * @returns boolean
	 */
	public boolean isContinuousLayout() {
		return false; // TODO
	} // isContinuousLayout()

	/**
	 * setResizeWeight
	 * @param value0 TODO
	 */
	public void setResizeWeight(double value0) {
		// TODO
	} // setResizeWeight()

	/**
	 * getResizeWeight
	 * @returns double
	 */
	public double getResizeWeight() {
		return 0.0; // TODO
	} // getResizeWeight()

	/**
	 * resetToPreferredSizes
	 */
	public void resetToPreferredSizes() {
		// TODO
	} // resetToPreferredSizes()

	/**
	 * setDividerLocation
	 * @param value0 TODO
	 */
	public void setDividerLocation(double value0) {
		// TODO
	} // setDividerLocation()

	/**
	 * setDividerLocation
	 * @param value0 TODO
	 */
	public void setDividerLocation(int value0) {
		// TODO
	} // setDividerLocation()

	/**
	 * getDividerLocation
	 * @returns int
	 */
	public int getDividerLocation() {
		return 0; // TODO
	} // getDividerLocation()

	/**
	 * getMinimumDividerLocation
	 * @returns int
	 */
	public int getMinimumDividerLocation() {
		return 0; // TODO
	} // getMinimumDividerLocation()

	/**
	 * getMaximumDividerLocation
	 * @returns int
	 */
	public int getMaximumDividerLocation() {
		return 0; // TODO
	} // getMaximumDividerLocation()

	/**
	 * isValidateRoot
	 * @returns boolean
	 */
	public boolean isValidateRoot() {
		return false; // TODO
	} // isValidateRoot()

	/**
	 * addImpl
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	protected void addImpl(Component value0, Object value1, int value2) {
		// TODO
	} // addImpl()

	/**
	 * paintChildren
	 * @param value0 TODO
	 */
	protected void paintChildren(Graphics value0) {
		// TODO
	} // paintChildren()

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
			accessibleContext = new AccessibleJSplitPane(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JSplitPane
