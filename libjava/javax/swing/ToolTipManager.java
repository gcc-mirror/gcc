/* ToolTipManager.java --
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
import java.awt.event.*;

/**
 * ToolTipManager
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class ToolTipManager extends MouseAdapter implements MouseMotionListener {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * stillInsideTimerAction
	 */
	protected class stillInsideTimerAction implements ActionListener {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor stillInsideTimerAction
		 * @param manager TODO
		 */
		protected stillInsideTimerAction(ToolTipManager manager) {
			// TODO
		} // stillInsideTimerAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // stillInsideTimerAction

	/**
	 * outsideTimerAction
	 */
	protected class outsideTimerAction implements ActionListener {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor outsideTimerAction
		 * @param manager TODO
		 */
		protected outsideTimerAction(ToolTipManager manager) {
			// TODO
		} // outsideTimerAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param value0 TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // outsideTimerAction

	/**
	 * insideTimerAction
	 */
	protected class insideTimerAction implements ActionListener {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor insideTimerAction
		 * @param manager TODO
		 */
		protected insideTimerAction(ToolTipManager manager) {
			// TODO
		} // insideTimerAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // insideTimerAction


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * enterTimer
	 */
	Timer enterTimer;

	/**
	 * exitTimer
	 */
	Timer exitTimer;

	/**
	 * insideTimer
	 */
	Timer insideTimer;

	/**
	 * toolTipText
	 */
	String toolTipText;

	/**
	 * mouseEvent
	 */
	MouseEvent mouseEvent;

	/**
	 * showImmediately
	 */
	boolean showImmediately;

	/**
	 * tip
	 */
	JToolTip tip;

	/**
	 * enabled
	 */
	boolean enabled;

	/**
	 * timerEnter
	 */
	private long timerEnter;

	/**
	 * lightWeightPopupEnabled
	 */
	protected boolean lightWeightPopupEnabled;

	/**
	 * heavyWeightPopupEnabled
	 */
	protected boolean heavyWeightPopupEnabled;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor ToolTipManager
	 */
	ToolTipManager() {
		// TODO
	} // ToolTipManager()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * sharedInstance
	 * @returns ToolTipManager
	 */
	public static ToolTipManager sharedInstance() {
		return null; // TODO
	} // sharedInstance()

	/**
	 * setEnabled
	 * @param enabled TODO
	 */
	public void setEnabled(boolean enabled) {
		// TODO
	} // setEnabled()

	/**
	 * isEnabled
	 * @returns boolean
	 */
	public boolean isEnabled() {
		return false; // TODO
	} // isEnabled()

	/**
	 * isLightWeightPopupEnabled
	 * @returns boolean
	 */
	public boolean isLightWeightPopupEnabled() {
		return false; // TODO
	} // isLightWeightPopupEnabled()

	/**
	 * setLightWeightPopupEnabled
	 * @param enabled TODO
	 */
	public void setLightWeightPopupEnabled(boolean enabled) {
		// TODO
	} // setLightWeightPopupEnabled()

	/**
	 * getInitialDelay
	 * @returns int
	 */
	public int getInitialDelay() {
		return 0; // TODO
	} // getInitialDelay()

	/**
	 * setInitialDelay
	 * @param delay TODO
	 */
	public void setInitialDelay(int delay) {
		// TODO
	} // setInitialDelay()

	/**
	 * getDismissDelay
	 * @returns int
	 */
	public int getDismissDelay() {
		return 0; // TODO
	} // getDismissDelay()

	/**
	 * setDismissDelay
	 * @param delay TODO
	 */
	public void setDismissDelay(int delay) {
		// TODO
	} // setDismissDelay()

	/**
	 * getReshowDelay
	 * @returns int
	 */
	public int getReshowDelay() {
		return 0; // TODO
	} // getReshowDelay()

	/**
	 * setReshowDelay
	 * @param delay TODO
	 */
	public void setReshowDelay(int delay) {
		// TODO
	} // setReshowDelay()

	/**
	 * registerComponent
	 * @param component TODO
	 */
	public void registerComponent(JComponent component) {
		// TODO
	} // registerComponent()

	/**
	 * unregisterComponent
	 * @param component TODO
	 */
	public void unregisterComponent(JComponent component) {
		// TODO
	} // unregisterComponent()

	/**
	 * mouseEntered
	 * @param event TODO
	 */
	public void mouseEntered(MouseEvent event) {
		// TODO
	} // mouseEntered()

	/**
	 * mouseExited
	 * @param event TODO
	 */
	public void mouseExited(MouseEvent event) {
		// TODO
	} // mouseExited()

	/**
	 * mousePressed
	 * @param event TODO
	 */
	public void mousePressed(MouseEvent event) {
		// TODO
	} // mousePressed()

	/**
	 * mouseDragged
	 * @param event TODO
	 */
	public void mouseDragged(MouseEvent event) {
		// TODO
	} // mouseDragged()

	/**
	 * mouseMoved
	 * @param event TODO
	 */
	public void mouseMoved(MouseEvent event) {
		// TODO
	} // mouseMoved()


} // ToolTipManager
