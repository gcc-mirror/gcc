/* JProgressBar.java --
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

import java.awt.Graphics;
import java.io.IOException;
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ProgressBarUI;

/**
 * JProgressBar
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JProgressBar extends JComponent implements SwingConstants, Accessible
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------


	/**
	 * AccessibleJProgressBar
	 */
	protected class AccessibleJProgressBar extends AccessibleJComponent 
			implements AccessibleValue {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJProgressBar
		 * @param component TODO
		 */
		protected AccessibleJProgressBar(JProgressBar component) {
			super(component);
			// TODO
		} // AccessibleJProgressBar()


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
			return AccessibleRole.PROGRESS_BAR;
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


	} // AccessibleJProgressBar


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "ProgressBarUI";

	/**
	 * orientation
	 */
	protected int orientation;

	/**
	 * paintBorder
	 */
	protected boolean paintBorder;

	/**
	 * model
	 */
	protected BoundedRangeModel model;

	/**
	 * progressString
	 */
	protected String progressString;

	/**
	 * paintString
	 */
	protected boolean paintString;

	/**
	 * changeEvent
	 */
	protected transient ChangeEvent changeEvent;

	/**
	 * changeListener
	 */
	protected ChangeListener changeListener;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JProgressBar
	 */
	public JProgressBar() {
		// TODO
	} // JProgressBar()

	/**
	 * Constructor JProgressBar
	 * @param orientation TODO
	 */
	public JProgressBar(int orientation) {
		// TODO
	} // JProgressBar()

	/**
	 * Constructor JProgressBar
	 * @param minimum TODO
	 * @param maximum TODO
	 */
	public JProgressBar(int minimum, int maximum) {
		// TODO
	} // JProgressBar()

	/**
	 * Constructor JProgressBar
	 * @param minimum TODO
	 * @param maximum TODO
	 * @param orientation TODO
	 */
	public JProgressBar(int minimum, int maximum, int orientation) {
		// TODO
	} // JProgressBar()

	/**
	 * Constructor JProgressBar
	 * @param model TODO
	 */
	public JProgressBar(BoundedRangeModel model) {
		// TODO
	} // JProgressBar()


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
	 * getValue
	 * @returns int
	 */
	public int getValue() {
		return 0; // TODO
	} // getValue()

	/**
	 * setValue
	 * @param value TODO
	 */
	public void setValue(int value) {
		// TODO
	} // setValue()

	/**
	 * paintBorder
	 * @param graphics TODO
	 */
	protected void paintBorder(Graphics graphics) {
		// TODO
	} // paintBorder()

	/**
	 * getOrientation
	 * @returns int
	 */
	public int getOrientation() {
		return 0; // TODO
	} // getOrientation()

	/**
	 * setOrientation
	 * @param orientation TODO
	 */
	public void setOrientation(int orientation) {
		// TODO
	} // setOrientation()

	/**
	 * isStringPainted
	 * @returns boolean
	 */
	public boolean isStringPainted() {
		return false; // TODO
	} // isStringPainted()

	/**
	 * setStringPainted
	 * @param painted TODO
	 */
	public void setStringPainted(boolean painted) {
		// TODO
	} // setStringPainted()

	/**
	 * getString
	 * @returns String
	 */
	public String getString() {
		return null; // TODO
	} // getString()

	/**
	 * setString
	 * @param string TODO
	 */
	public void setString(String string) {
		// TODO
	} // setString()

	/**
	 * getPercentComplete
	 * @returns double
	 */
	public double getPercentComplete() {
		return 0.0; // TODO
	} // getPercentComplete()

	/**
	 * isBorderPainted
	 * @returns boolean
	 */
	public boolean isBorderPainted() {
		return false; // TODO
	} // isBorderPainted()

	/**
	 * setBorderPainted
	 * @param painted TODO
	 */
	public void setBorderPainted(boolean painted) {
		// TODO
	} // setBorderPainted()

	/**
	 * getUI
	 * @returns ProgressBarUI
	 */
	public ProgressBarUI getUI() {
		return (ProgressBarUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(ProgressBarUI ui) {
		super.setUI(ui);
		// TODO
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((ProgressBarUI) UIManager.get(this));
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
	 * createChangeListener
	 * @returns ChangeListener
	 */
	protected ChangeListener createChangeListener() {
		return null; // TODO
	} // createChangeListener()

	/**
	 * addChangeListener
	 * @param listener TODO
	 */
	public void addChangeListener(ChangeListener listener) {
		// TODO
	} // addChangeListener()

	/**
	 * removeChangeListener
	 * @param listener TODO
	 */
	public void removeChangeListener(ChangeListener valulistener) {
		// TODO
	} // removeChangeListener()

	/**
	 * fireStateChanged
	 */
	protected void fireStateChanged() {
		// TODO
	} // fireStateChanged()

	/**
	 * getModel
	 * @returns BoundedRangeModel
	 */
	public BoundedRangeModel getModel() {
		return null; // TODO
	} // getModel()

	/**
	 * setModel
	 * @param model TODO
	 */
	public void setModel(BoundedRangeModel model) {
		// TODO
	} // setModel()

	/**
	 * getMinimum
	 * @returns int
	 */
	public int getMinimum() {
		return 0; // TODO
	} // getMinimum()

	/**
	 * setMinimum
	 * @param minimum TODO
	 */
	public void setMinimum(int minimum) {
		// TODO
	} // setMinimum()

	/**
	 * getMaximum
	 * @returns int
	 */
	public int getMaximum() {
		return 0; // TODO
	} // getMaximum()

	/**
	 * setMaximum
	 * @param maximum TODO
	 */
	public void setMaximum(int maximum) {
		// TODO
	} // setMaximum()

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
			accessibleContext = new AccessibleJProgressBar(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JProgressBar
