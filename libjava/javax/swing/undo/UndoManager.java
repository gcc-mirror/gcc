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

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;

/**
 * UndoManager
 * @author	Andrew Selkirk
 */
public class UndoManager extends CompoundEdit implements UndoableEditListener {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * indexOfNextAdd
	 */
	int indexOfNextAdd;

	/**
	 * limit
	 */
	int limit;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor UndoManager
	 */
	public UndoManager() {
		// TODO
	} // UndoManager()


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
	 * end
	 */
	public synchronized void end() {
		// TODO
	} // end()

	/**
	 * getLimit
	 * @returns int
	 */
	public synchronized int getLimit() {
		return 0; // TODO
	} // getLimit()

	/**
	 * discardAllEdits
	 */
	public synchronized void discardAllEdits() {
		// TODO
	} // discardAllEdits()

	/**
	 * trimForLimit
	 */
	protected void trimForLimit() {
		// TODO
	} // trimForLimit()

	/**
	 * trimEdits
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	protected void trimEdits(int value0, int value1) {
		// TODO
	} // trimEdits()

	/**
	 * setLimit
	 * @param value0 TODO
	 */
	public synchronized void setLimit(int value0) {
		// TODO
	} // setLimit()

	/**
	 * editToBeUndone
	 * @returns UndoableEdit
	 */
	protected UndoableEdit editToBeUndone() {
		return null; // TODO
	} // editToBeUndone()

	/**
	 * editToBeRedone
	 * @returns UndoableEdit
	 */
	protected UndoableEdit editToBeRedone() {
		return null; // TODO
	} // editToBeRedone()

	/**
	 * undoTo
	 * @param value0 TODO
	 * @exception CannotUndoException TODO
	 */
	protected void undoTo(UndoableEdit value0) throws CannotUndoException {
		// TODO
	} // undoTo()

	/**
	 * redoTo
	 * @param value0 TODO
	 * @exception CannotRedoException TODO
	 */
	protected void redoTo(UndoableEdit value0) throws CannotRedoException {
		// TODO
	} // redoTo()

	/**
	 * undoOrRedo
	 * @exception CannotRedoException TODO
	 * @exception CannotUndoException TODO
	 */
	public synchronized void undoOrRedo() throws CannotRedoException, CannotUndoException {
		// TODO
	} // undoOrRedo()

	/**
	 * canUndoOrRedo
	 * @returns boolean
	 */
	public synchronized boolean canUndoOrRedo() {
		return false; // TODO
	} // canUndoOrRedo()

	/**
	 * undo
	 * @exception CannotUndoException TODO
	 */
	public synchronized void undo() throws CannotUndoException {
		// TODO
	} // undo()

	/**
	 * canUndo
	 * @returns boolean
	 */
	public synchronized boolean canUndo() {
		return false; // TODO
	} // canUndo()

	/**
	 * redo
	 * @exception CannotRedoException TODO
	 */
	public synchronized void redo() throws CannotRedoException {
		// TODO
	} // redo()

	/**
	 * canRedo
	 * @returns boolean
	 */
	public synchronized boolean canRedo() {
		return false; // TODO
	} // canRedo()

	/**
	 * addEdit
	 * @param value0 TODO
	 * @returns boolean
	 */
	public synchronized boolean addEdit(UndoableEdit value0) {
		return false; // TODO
	} // addEdit()

	/**
	 * getUndoOrRedoPresentationName
	 * @returns String
	 */
	public synchronized String getUndoOrRedoPresentationName() {
		return null; // TODO
	} // getUndoOrRedoPresentationName()

	/**
	 * getUndoPresentationName
	 * @returns String
	 */
	public synchronized String getUndoPresentationName() {
		return null; // TODO
	} // getUndoPresentationName()

	/**
	 * getRedoPresentationName
	 * @returns String
	 */
	public synchronized String getRedoPresentationName() {
		return null; // TODO
	} // getRedoPresentationName()

	/**
	 * undoableEditHappened
	 * @param value0 TODO
	 */
	public void undoableEditHappened(UndoableEditEvent value0) {
		// TODO
	} // undoableEditHappened()


} // UndoManager
