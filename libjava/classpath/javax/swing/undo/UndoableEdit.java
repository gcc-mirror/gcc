/* UndoableEdit.java --
   Copyright (C) 2002, 2006, Free Software Foundation, Inc.

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

package javax.swing.undo;

/**
 * An editing operation that supports undo/redoability.
 *
 * @author Andrew Selkirk
 */
public interface UndoableEdit
{

  /**
   * Incorporates another editing action into this one, thus forming a
   * combined action.
   *
   * @param edit the editing action to be incorporated.
   *
   * @return <code>true</code> if the edit was combined successfully, and
   *         <code>false</code> if it could not be combined.
   */
  boolean addEdit(UndoableEdit edit);

  /**
   * Determines whether it would be possible to redo this editing
   * action.
   *
   * @return <code>true</code> to indicate that this action can be
   * redone, <code>false</code> otherwise.
   *
   * @see #redo()
   * @see #canUndo()
   */
  boolean canRedo();

  /**
   * Determines whether it would be possible to undo this editing
   * action.
   *
   * @return <code>true</code> to indicate that this action can be
   * undone, <code>false</code> otherwise.
   *
   * @see #undo()
   * @see #canRedo()
   */
  boolean canUndo();

  /**
   * Informs this edit action that it will no longer be used. Some
   * actions might use this information to release resources, for
   * example open files.  Called by {@link UndoManager} before this
   * action is removed from the edit queue.
   */
  void die();

  /**
   * Returns a human-readable, localized name that describes this
   * editing action and can be displayed to the user.
   *
   * @return The presentation name.
   */
  String getPresentationName();

  /**
   * Returns the redo presentation name.
   *
   * @return The redo presentation name.
   */
  String getRedoPresentationName();

  /**
   * Returns the undo presentation name.
   *
   * @return The undo presentation name.
   */
  String getUndoPresentationName();

  /**
   * Determines whether this editing action is significant enough for
   * being seperately undoable by the user. A typical significant
   * action would be the resizing of an object. However, changing the
   * selection in a text document would usually not be considered
   * significant.
   *
   * @return <code>true</code> to indicate that the action is
   * significant enough for being separately undoable, or
   * <code>false</code> otherwise.
   */
  boolean isSignificant();

  /**
   * Redoes this editing action.
   *
   * @throws CannotRedoException if the edit cannot be undone.
   *
   * @see #canRedo()
   * @see #undo()
   */
  void redo() throws CannotRedoException;

  /**
   * Incorporates another editing action into this one, thus forming a
   * combined action that replaces the argument action.
   *
   * @param edit the editing action to be replaced.
   *
   * @return <code>true</code> if the edit is successfully replaced, and
   *         <code>false</code> otherwise.
   */
  boolean replaceEdit(UndoableEdit edit);

  /**
   * Undoes this editing action.
   *
   * @throws CannotUndoException if the edit cannot be undone.
   *
   * @see #canUndo()
   * @see #redo()
   */
  void undo() throws CannotUndoException;

}
