/* ProgressMonitor.java --
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

/**
 * ProgressMonitor
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class ProgressMonitor {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * parentComponent
	 */
	private Component component;

	/**
	 * note
	 */
	private String note;

	/**
	 * message
	 */
	private Object message;

	/**
	 * millisToDecideToPopup
	 */
	private int millisToDecideToPopup;

	/**
	 * millisToPopup
	 */
	private int millisToPopup;

	/**
	 * min
	 */
	private int minimum;

	/**
	 * max
	 */
	private int maximum;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor ProgressMonitor
	 * @param component TODO
	 * @param message TODO
	 * @param note TODO
	 * @param minimum TODO
	 * @param maximum TODO
	 */
	public ProgressMonitor(Component component, Object message,
			String note, int minimum, int maximum) {
			
		// Set Data
		this.component = component;
		this.message = message;
		this.note = note;
		this.minimum = minimum;
		this.maximum = maximum;

		// TODO
	} // ProgressMonitor()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * close
	 */
	public void close() {
		// TODO
	} // close()

	/**
	 * setProgress
	 * @param progress TODO
	 */
	public void setProgress(int progress) {
		// TODO
	} // setProgress()

	/**
	 * getMinimum
	 * @returns int
	 */
	public int getMinimum() {
		return minimum; // TODO
	} // getMinimum()

	/**
	 * setMinimum
	 * @param minimum TODO
	 */
	public void setMinimum(int minimum) {
		this.minimum = minimum;
		// TODO
	} // setMinimum()

	/**
	 * getMaximum
	 * @returns int
	 */
	public int getMaximum() {
		return maximum; // TODO
	} // getMaximum()

	/**
	 * setMaximum
	 * @param maximum TODO
	 */
	public void setMaximum(int maximum) {
		this.maximum = maximum;
		// TODO
	} // setMaximum()

	/**
	 * isCanceled
	 * @returns boolean
	 */
	public boolean isCanceled() {
		return false; // TODO
	} // isCanceled()

	/**
	 * getMillisToDecideToPopup
	 * @returns int
	 */
	public int getMillisToDecideToPopup() {
		return millisToDecideToPopup; // TODO
	} // getMillisToDecideToPopup()

	/**
	 * setMillisToDecideToPopup
	 * @param time TODO
	 */
	public void setMillisToDecideToPopup(int time) {
		millisToDecideToPopup = time;
		// TODO
	} // setMillisToDecideToPopup()

	/**
	 * getMillisToPopup
	 * @returns int
	 */
	public int getMillisToPopup() {
		return millisToPopup; // TODO
	} // getMillisToPopup()

	/**
	 * setMillisToPopup
	 * @param time TODO
	 */
	public void setMillisToPopup(int time) {
		millisToPopup = time;
		// TODO
	} // setMillisToPopup()

	/**
	 * getNote
	 * @returns String
	 */
	public String getNote() {
		return note; // TODO
	} // getNote()

	/**
	 * setNote
	 * @param note TODO
	 */
	public void setNote(String note) {
		this.note = note;
		// TODO
	} // setNote()


} // ProgressMonitor
