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
import java.util.Vector;

/**
 * CompoundEdit
 * @author Andrew Selkirk
 */
public class CompoundEdit extends AbstractUndoableEdit {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * The collection of UndoableEdits undone/redone en
	 * masse by this CompoundEdit
	 */
	protected	Vector	edits		= new Vector();

	/**
	 * TODO
	 */
	private		boolean	inProgress	= false;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Create new Compound Edit
	 */
	public CompoundEdit() {
	} // CompoundEdit()


	//-------------------------------------------------------------
	// Interface: UndoableEdit ------------------------------------
	//-------------------------------------------------------------

	/**
	 * addEdit
	 * @param anEdit TODO
	 * @returns TODO
	 */
	public boolean addEdit(UndoableEdit anEdit) {

		// Variables
		UndoableEdit	lastEdit;

		if (inProgress == true) {

			// Get Last Edit
			lastEdit = lastEdit();

			// Check for null
			if (lastEdit != null) {

				if (lastEdit.addEdit(anEdit) == false) {
					if (lastEdit.replaceEdit(anEdit) == false) {
						edits.add(anEdit);
					}
				}

			} // if: lastEdit

			return true;

		} else {
			return false;
		}
	} // addEdit()

	/**
	 * canRedo
	 * @returns TODO
	 */
	public boolean canRedo() {
		if (isInProgress() == true || super.canRedo() == false) {
			return false;
		}
		return true;
	} // canRedo()

	/**
	 * canUndo
	 * @returns TODO
	 */
	public boolean canUndo() {
		if (isInProgress() == true || super.canUndo() == false) {
			return false;
		}
		return true;
	} // canUndo()

	/**
	 * die
	 */
	public void die() {

		// Variables
		int				index;
		UndoableEdit	current;

		// Loop through all contained UndoableEdits
		for (index = edits.size() - 1; index >= 0; index--) {
			current = (UndoableEdit) edits.elementAt(index);
			current.die();
		} // for: index

	} // die()

	/**
	 * end
	 */
	public void end() {
		inProgress = false;
	} // end()

	/**
	 * getPresentationName
	 * @returns TODO
	 */
	public String getPresentationName() {
		if (edits.size() == 0) {
			return super.getPresentationName();
		} else {
			return lastEdit().getPresentationName();
		}
	} // getPresentationName()

	/**
	 * getRedoPresentationName
	 * @returns TODO
	 */
	public String getRedoPresentationName()	{
		if (edits.size() == 0) {
			return super.getRedoPresentationName();
		} else {
			return lastEdit().getRedoPresentationName();
		}
	} // getRedoPresentationName()

	/**
	 * getUndoPresentationName
	 * @returns TODO
	 */
	public String getUndoPresentationName()	{
		if (edits.size() == 0) {
			return super.getUndoPresentationName();
		} else {
			return lastEdit().getUndoPresentationName();
		}
	} // getUndoPresentationName()

	/**
	 * isInProgress
	 * @returns TODO
	 */
	public boolean isInProgress() {
		return inProgress;
	} // isInProgress()


	/**
	 * isSignigicant
	 * @returns TODO
	 */
	public boolean isSignificant() {

		// Variables
		int				index;
		UndoableEdit	current;

		// Check each edit
		for (index = 0; index < edits.size(); index++) {
			current = (UndoableEdit) edits.elementAt(index);
			if (current.isSignificant() == true) {
				return true;
			}
		} // for: index

		return false;

	} // isSignificant()

	/**
	 * lastEdit
	 * @returns TODO
	 */
	protected UndoableEdit lastEdit() {
		if (edits.size() == 0) {
			return null;
		}
		return (UndoableEdit) edits.elementAt(edits.size() - 1);
	} // lastEdit()

	/**
	 * redo
	 * @throws CannotRedoException TODO
	 */
	public void redo() throws CannotRedoException {

		// Variables
		int				index;
		UndoableEdit	current;

		// Loop through all contained UndoableEdits
		for (index = 0; index < edits.size(); index++) {
			current = (UndoableEdit) edits.elementAt(index);
			current.redo();
		} // for: index

	} // redo()

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

		// Variables
		int				index;
		UndoableEdit	current;

		// Loop through all contained UndoableEdits
		for (index = edits.size() - 1; index >= 0; index--) {
			current = (UndoableEdit) edits.elementAt(index);
			current.undo();
		} // for: index

	} // undo()


} // CompoundEdit
