/* AbstractTableModel.java --
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

package javax.swing.undo;

// Imports
import java.util.*;
import javax.swing.event.*;

/**
 * UndoableEditSupport
 * @author	Andrew Selkirk
 */
public class UndoableEditSupport {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * updateLevel
	 */
	protected int updateLevel;

	/**
	 * compoundEdit
	 */
	protected CompoundEdit compoundEdit;

	/**
	 * listeners
	 */
	protected Vector listeners = new Vector();

	/**
	 * realSource
	 */
	protected Object realSource;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor UndoableEditSupport
	 */
	public UndoableEditSupport() {
		// TODO
	} // UndoableEditSupport()

	/**
	 * Constructor UndoableEditSupport
	 * @param object TODO
	 */
	public UndoableEditSupport(Object object) {
		realSource = object;
	} // UndoableEditSupport()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		return null; // TODO
	} // toString()

	/**
	 * addUndoableEditListener
	 * @param value0 TODO
	 */
	public synchronized void addUndoableEditListener(UndoableEditListener value0) {
		// TODO
	} // addUndoableEditListener()

	/**
	 * removeUndoableEditListener
	 * @param value0 TODO
	 */
	public synchronized void removeUndoableEditListener(UndoableEditListener value0) {
		// TODO
	} // removeUndoableEditListener()

	/**
	 * _postEdit
	 * @param value0 TODO
	 */
	protected void _postEdit(UndoableEdit value0) {
		// TODO
	} // _postEdit()

	/**
	 * postEdit
	 * @param value0 TODO
	 */
	public synchronized void postEdit(UndoableEdit value0) {
		// TODO
	} // postEdit()

	/**
	 * getUpdateLevel
	 * @returns int
	 */
	public int getUpdateLevel() {
		return 0; // TODO
	} // getUpdateLevel()

	/**
	 * beginUpdate
	 */
	public synchronized void beginUpdate() {
		// TODO
	} // beginUpdate()

	/**
	 * createCompoundEdit
	 * @returns CompoundEdit
	 */
	protected CompoundEdit createCompoundEdit() {
		return null; // TODO
	} // createCompoundEdit()

	/**
	 * endUpdate
	 */
	public synchronized void endUpdate() {
		// TODO
	} // endUpdate()


} // UndoableEditSupport
