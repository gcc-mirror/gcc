/* JSlider.java --
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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Dictionary;
import java.util.Hashtable;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SliderUI;

/**
 * JSlider
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JSlider
  extends JComponent
  implements SwingConstants, Accessible
{
  static final long serialVersionUID = -1441275936141218479L;

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------


	/**
	 * AccessibleJSlider
	 */
	protected class AccessibleJSlider extends JComponent.AccessibleJComponent implements AccessibleValue {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJSlider
		 * @param value0 TODO
		 */
		protected AccessibleJSlider(JSlider value0) {
			super(value0);
			// TODO
		} // AccessibleJSlider()


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
			return null; // TODO
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


	} // AccessibleJSlider

	/**
	 * ModelListener
	 */
	private class ModelListener implements ChangeListener, Serializable {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor ModelListener
		 * @param value0 TODO
		 */
		private ModelListener(JSlider value0) {
			// TODO
		} // ModelListener()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * stateChanged
		 * @param value0 TODO
		 */
		public void stateChanged(ChangeEvent value0) {
			// TODO
		} // stateChanged()


	} // ModelListener


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "SliderUI";

	/**
	 * paintTicks
	 */
	private boolean paintTicks;

	/**
	 * paintTrack
	 */
	private boolean paintTrack;

	/**
	 * paintLabels
	 */
	private boolean paintLabels;

	/**
	 * isInverted
	 */
	private boolean isInverted;

	/**
	 * sliderModel
	 */
	protected BoundedRangeModel sliderModel;

	/**
	 * majorTickSpacing
	 */
	protected int majorTickSpacing;

	/**
	 * minorTickSpacing
	 */
	protected int minorTickSpacing;

	/**
	 * snapToTicks
	 */
	protected boolean snapToTicks;

	/**
	 * snapToValue
	 */
	boolean snapToValue;

	/**
	 * orientation
	 */
	protected int orientation;

	/**
	 * labelTable
	 */
	private Dictionary labelTable;

	/**
	 * changeListener
	 */
	protected ChangeListener changeListener;

	/**
	 * changeEvent
	 */
	protected transient ChangeEvent changeEvent;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JSlider
	 */
	public JSlider() {
		// TODO
	} // JSlider()

	/**
	 * Constructor JSlider
	 * @param value0 TODO
	 */
	public JSlider(int orientation) {
		// TODO
	} // JSlider()

	/**
	 * Constructor JSlider
	 * @param minimum TODO
	 * @param maximum TODO
	 */
	public JSlider(int minimum, int maximum) {
		// TODO
	} // JSlider()

	/**
	 * Constructor JSlider
	 * @param minimum TODO
	 * @param maximum TODO
	 * @param value TODO
	 */
	public JSlider(int minimum, int maximum, int value) {
		// TODO
	} // JSlider()

	/**
	 * Constructor JSlider
	 * @param orientation TODO
	 * @param minimum TODO
	 * @param maximum TODO
	 * @param value TODO
	 */
	public JSlider(int orientation, int minimum, int maximum, int value) {
		// TODO
	} // JSlider()

	/**
	 * Constructor JSlider
	 * @param value0 TODO
	 */
	public JSlider(BoundedRangeModel model) {
		// TODO
	} // JSlider()


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
	 * @param value0 TODO
	 */
	public void setValue(int value) {
		// TODO
	} // setValue()

	/**
	 * getUI
	 * @returns SliderUI
	 */
	public SliderUI getUI() {
		return (SliderUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(SliderUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((SliderUI) UIManager.get(this));
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
	public void removeChangeListener(ChangeListener listener) {
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
	 * getValueIsAdjusting
	 * @returns boolean
	 */
	public boolean getValueIsAdjusting() {
		return false; // TODO
	} // getValueIsAdjusting()

	/**
	 * setValueIsAdjusting
	 * @param adjusting TODO
	 */
	public void setValueIsAdjusting(boolean adjusting) {
		// TODO
	} // setValueIsAdjusting()

	/**
	 * getExtent
	 * @returns int
	 */
	public int getExtent() {
		return 0; // TODO
	} // getExtent()

	/**
	 * setExtent
	 * @param vextent TODO
	 */
	public void setExtent(int extent) {
		// TODO
	} // setExtent()

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
	 * getLabelTable
	 * @returns Dictionary
	 */
	public Dictionary getLabelTable() {
		return null; // TODO
	} // getLabelTable()

	/**
	 * setLabelTable
	 * @param table TODO
	 */
	public void setLabelTable(Dictionary table) {
		// TODO
	} // setLabelTable()

	/**
	 * updateLabelUIs
	 */
	protected void updateLabelUIs() {
		// TODO
	} // updateLabelUIs()

	/**
	 * createStandardLabels
	 * @param increment TODO
	 * @returns Hashtable
	 */
	public Hashtable createStandardLabels(int increment) {
		return null; // TODO
	} // createStandardLabels()

	/**
	 * createStandardLabels
	 * @param increment TODO
	 * @param start TODO
	 * @returns Hashtable
	 */
	public Hashtable createStandardLabels(int increment, int start) {
		return null; // TODO
	} // createStandardLabels()

	/**
	 * getInverted
	 * @returns boolean
	 */
	public boolean getInverted() {
		return false; // TODO
	} // getInverted()

	/**
	 * setInverted
	 * @param inverted TODO
	 */
	public void setInverted(boolean inverted) {
		// TODO
	} // setInverted()

	/**
	 * getMajorTickSpacing
	 * @returns int
	 */
	public int getMajorTickSpacing() {
		return 0; // TODO
	} // getMajorTickSpacing()

	/**
	 * setMajorTickSpacing
	 * @param spacing TODO
	 */
	public void setMajorTickSpacing(int spacing) {
		// TODO
	} // setMajorTickSpacing()

	/**
	 * getMinorTickSpacing
	 * @returns int
	 */
	public int getMinorTickSpacing() {
		return 0; // TODO
	} // getMinorTickSpacing()

	/**
	 * setMinorTickSpacing
	 * @param spacing TODO
	 */
	public void setMinorTickSpacing(int spacing) {
		// TODO
	} // setMinorTickSpacing()

	/**
	 * getSnapToTicks
	 * @returns boolean
	 */
	public boolean getSnapToTicks() {
		return false; // TODO
	} // getSnapToTicks()

	/**
	 * getSnapToValue
	 * @returns boolean
	 */
	boolean getSnapToValue() {
		return false; // TODO
	} // getSnapToValue()

	/**
	 * setSnapToTicks
	 * @param snap TODO
	 */
	public void setSnapToTicks(boolean snap) {
		// TODO
	} // setSnapToTicks()

	/**
	 * getPaintTicks
	 * @returns boolean
	 */
	public boolean getPaintTicks() {
		return false; // TODO
	} // getPaintTicks()

	/**
	 * setPaintTicks
	 * @param paint TODO
	 */
	public void setPaintTicks(boolean paint) {
		// TODO
	} // setPaintTicks()

	/**
	 * getPaintTrack
	 * @returns boolean
	 */
	public boolean getPaintTrack() {
		return false; // TODO
	} // getPaintTrack()

	/**
	 * setPaintTrack
	 * @param paint TODO
	 */
	public void setPaintTrack(boolean paint) {
		// TODO
	} // setPaintTrack()

	/**
	 * getPaintLabels
	 * @returns boolean
	 */
	public boolean getPaintLabels() {
		return false; // TODO
	} // getPaintLabels()

	/**
	 * setPaintLabels
	 * @param paint TODO
	 */
	public void setPaintLabels(boolean paint) {
		// TODO
	} // setPaintLabels()

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
			accessibleContext = new AccessibleJSlider(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JSlider
