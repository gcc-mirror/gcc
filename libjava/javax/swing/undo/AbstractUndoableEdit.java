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
import java.io.Serializable;

/**
 * AbstractUndoableEdit
 * @author Andrew Selkirk
 */
public class AbstractUndoableEdit	extends		Object
									implements	UndoableEdit,
												Serializable {

	//-------------------------------------------------------------
	// Constants --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * String returned by getRedoPresentationName()
	 */
	protected static	String	RedoName	= "Redo";

	/**
	 * String returned by getUndoPresentationName()
	 */
	protected static	String	UndoName	= "Undo";


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * TODO
	 */
	private 			boolean	hasBeenDone	= false;

	/**
	 * The edit is alive
	 */
	private				boolean	alive 		= true;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Create new AbstractUndoableEdit
	 */
	public AbstractUndoableEdit() {
	} // AbstractUndoableEdit()


	//-------------------------------------------------------------
	// Interface: UndoableEdit ------------------------------------
	//-------------------------------------------------------------

	/**
	 * addEdit
	 * @param anEdit TODO
	 * @returns TODO
	 */
	public boolean addEdit(UndoableEdit anEdit) {
		return false;
	} // addEdit()

	/**
	 * canRedo()
	 * @returns true if redoable, false otherwise
	 */
	public boolean canRedo() {
		if (alive == true && hasBeenDone == false) {
			return true;
		} // if
		return false;
	} // canRedo()

	/**
	 * canUndo()
	 * @returns true if undoable, false otherwise
	 */
	public boolean canUndo() {
		if (alive == true && hasBeenDone == true) {
			return true;
		} // if
		return false;
	} // canUndo()

	/**
	 * die
	 */
	public void die() {
		alive = false;
	} // die()

	/**
	 * getPresentation
	 * @returns TODO
	 */
	public String getPresentationName() {
		return "";
	} // getPresentationName()

	/**
	 * getRedoPresentationName
	 * @returns TODO
	 */
	public String getRedoPresentationName()	{
		if (getPresentationName().equals("") == true) {
			return RedoName;
		} else {
			return RedoName + " " + getPresentationName();
		}
	} // getRedoPresentationName()

	/**
	 * getUndoPresentationName
	 * @returns TODO
	 */
	public String getUndoPresentationName()	{
		if (getPresentationName().equals("") == true) {
			return UndoName;
		} else {
			return UndoName + " " + getPresentationName();
		}
	} // getUndoPresentationName()

	/**
	 * isSignificant
	 * @returns true
	 */
	public boolean isSignificant() {
		return true;
	} // isSignificant()

	/**
	 * redo
	 * @throws CannotRedoException TODO
	 */
	public void redo() throws CannotRedoException {
		if (canRedo() == false) {
			throw new CannotRedoException();
		}
		hasBeenDone = true;
	} // redo()

	/**
	 * replaceEdit
	 * @param anEdit TODO
	 * @returns TODO
	 */
	public boolean replaceEdit(UndoableEdit anEdit) {
		return false;
	} // replaceEdit()

	/**
	 * String representation
	 * @returns String representation
	 */
	public String toString() {
		return null; // TODO
	} // toString()

	/**
	 * undo
	 * @throws CannotUndoException TODO
	 */
	public void undo() throws CannotUndoException {
		if (canUndo() == false) {
			throw new CannotUndoException();
		}
		hasBeenDone = false;
	} // undo()


} // AbstractUndoableEdit
