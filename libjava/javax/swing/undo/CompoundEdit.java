/* CompoundEdit.java -- Combines multiple UndoableEdits.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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

import java.util.Vector;

/**
 * An editing action that consists of multiple
 * <code>UndoableEdits</code>.
 *
 * <p>The use of a <code>CompoundEdit</code> is divided in two separate
 * phases.
 *
 * <ol><li>In the first phase, the <code>CompoundEdit</code> is
 * initialized.  After a new instance of <code>CompoundEdit</code> has
 * been created, {@link #addEdit(UndoableEdit)} is called for each
 * element of the compound.  To terminate the initialization phase,
 * call {@link #end()}.</li>
 *
 * <li>In the second phase, the the <code>CompoundEdit</code> can be
 * used, typically by invoking {@link #undo()} and {@link
 * #redo()}.</li></ol>
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class CompoundEdit
  extends AbstractUndoableEdit
{
  /**
   * The identifier of this class in object serialization. Determined
   * using the serialver tool of Sun J2SE 1.4.1_01.
   */
  private static final long serialVersionUID = -6512679249930119683L;


  /**
   * The <code>UndoableEdit</code>s being combined into a compound
   * editing action.
   */
  protected Vector edits;


  /**
   * Indicates whether the creation of this CompoundEdit is still in
   * progress. Initially, the value of this flag is
   * <code>true</code>. The {@link #end()} method changes the flag to
   * <code>false</code>. 
   */
  private boolean inProgress;


  /**
   * Constructs a new CompoundEdit.
   */
  public CompoundEdit()
  {
    edits = new Vector();
    inProgress = true;
  }
  

  /**
   * Undoes all edits that are part of of this
   * <code>CompoundEdit</code>. The compound elements will receive the
   * <code>undo</code> message in the reverse order of addition.
   *
   * @throws CannotUndoException if {@link #canUndo()} returns
   * <code>false</code>. This can happen if {@link #end()} has not
   * been called on this <code>CompoundEdit</code>, or if this edit
   * has already been undone.
   *
   * @see #canUndo()
   * @see #redo()
   */
  public void undo()
    throws CannotUndoException
  {
    // AbstractUndoableEdit.undo() will throw a CannotUndoException if
    // canUndo returns false.
    super.undo();

    for (int i = edits.size() - 1; i >= 0; i--)
      ((UndoableEdit) edits.elementAt(i)).undo();
  }


  /**
   * Redoes all edits that are part of of this
   * <code>CompoundEdit</code>. The compound elements will receive the
   * <code>undo</code> message in the same order as they were added.
   *
   * @throws CannotRedoException if {@link #canRedo()} returns
   * <code>false</code>. This can happen if {@link #end()} has not
   * been called on this <code>CompoundEdit</code>, or if this edit
   * has already been redone.
   *
   * @see #canRedo()
   * @see #undo()
   */
  public void redo()
    throws CannotRedoException
  {
    // AbstractUndoableEdit.redo() will throw a CannotRedoException if
    // canRedo returns false.
    super.redo();

    for (int i = 0; i < edits.size(); i++)
      ((UndoableEdit) edits.elementAt(i)).redo();
  }

  
  /**
   * Returns the the <code>UndoableEdit</code> that was last added to
   * this compound.
   */
  protected UndoableEdit lastEdit()
  {
    if (edits.size() == 0)
      return null;
    else
      return (UndoableEdit) edits.elementAt(edits.size() - 1);
  }


  /**
   * Informs this edit action, and all compound edits, that they will
   * no longer be used. Some actions might use this information to
   * release resources such as open files.  Called by {@link
   * UndoManager} before this action is removed from the edit queue.
   *
   * <p>The compound elements will receive the
   * <code>die</code> message in the reverse order of addition.
   */
  public void die()
  {
    for (int i = edits.size() - 1; i >= 0; i--)
      ((UndoableEdit) edits.elementAt(i)).die();

    super.die();
  }


  /**
   * Incorporates another editing action into this one, thus forming a
   * combined edit.
   *
   * <p>If this edit&#x2019;s {@link #end()} method has been called
   * before, <code>false</code> is returned immediately. Otherwise,
   * the {@linkplain #lastEdit() last added edit} is given the
   * opportunity to {@linkplain UndoableEdit#addEdit(UndoableEdit)
   * incorporate} <code>edit</code>.  If this fails, <code>edit</code>
   * is given the opportunity to {@linkplain
   * UndoableEdit#replaceEdit(UndoableEdit) replace} the last added
   * edit.  If this fails as well, <code>edit</code> gets added as a
   * new compound to {@link #edits}.
   * 
   * @param edit the editing action being added.
   *
   * @return <code>true</code> if <code>edit</code> could somehow be
   * incorporated; <code>false</code> if <code>edit</code> has not
   * been incorporated because {@link #end()} was called before.
   */
  public boolean addEdit(UndoableEdit edit)
  {
    UndoableEdit last;

    // If end has been called before, do nothing.
    if (!inProgress)
      return false;

    last = lastEdit();

    // If edit is the very first edit, just add it to the list.
    if (last == null)
    {
      edits.add(edit);
      return true;
    }

    // Try to incorporate edit into last.
    if (last.addEdit(edit))
      return true;

    // Try to replace last by edit.
    if (edit.replaceEdit(last))
    {
      edits.set(edits.size() - 1, edit);
      return true;
    }

    // If everything else has failed, add edit to the list of compound
    // edits.
    edits.add(edit);
    return true;
  }


  /**
   * Informs this <code>CompoundEdit</code> that its construction
   * phase has been completed. After this method has been called,
   * {@link #undo()} and {@link #redo()} may be called, {@link
   * #isInProgress()} will return <code>false</code>, and all attempts
   * to {@linkplain #addEdit(UndoableEdit) add further edits} will
   * fail.
   */
  public void end()
  {
    inProgress = false;
  }


  /**
   * Determines whether it would be possible to undo this editing
   * action. The result will be <code>true</code> if {@link #end()}
   * has been called on this <code>CompoundEdit</code>, {@link #die()}
   * has not yet been called, and the edit has not been undone
   * already.
   *
   * @return <code>true</code> to indicate that this action can be
   * undone; <code>false</code> otherwise.
   *
   * @see #undo()
   * @see #canRedo()
   */
  public boolean canUndo()
  {
    return !inProgress && super.canUndo();
  }


  /**
   * Determines whether it would be possible to redo this editing
   * action. The result will be <code>true</code> if {@link #end()}
   * has been called on this <code>CompoundEdit</code>, {@link #die()}
   * has not yet been called, and the edit has not been redone
   * already.
   *
   * @return <code>true</code> to indicate that this action can be
   * redone; <code>false</code> otherwise.
   *
   * @see #redo()
   * @see #canUndo()
   */
  public boolean canRedo()
  {
    return !inProgress && super.canRedo();
  }


  /**
   * Determines whether the initial construction phase of this
   * <code>CompoundEdit</code> is still in progress.  During this
   * phase, edits {@linkplain #addEdit(UndoableEdit) may be
   * added}. After initialization has been terminated by calling
   * {@link #end()}, {@link #undo()} and {@link #redo()} can be used.
   *
   * @return <code>true</code> if the initialization phase is still in
   * progress; <code>false</code> if {@link #end()} has been called.
   *
   * @see #end()
   */
  public boolean isInProgress()
  {
    return inProgress;
  }


  /**
   * Determines whether this editing action is significant enough for
   * being seperately undoable by the user. A typical significant
   * action would be the resizing of an object. However, changing the
   * selection in a text document would usually not be considered
   * significant.
   *
   * <p>A <code>CompoundEdit</code> is significant if any of its
   * elements are significant.
   */
  public boolean isSignificant()
  {
    for (int i = edits.size() - 1; i >= 0; i--)
      if (((UndoableEdit) edits.elementAt(i)).isSignificant())
        return true;

    return false;
  }
  

  /**
   * Returns a human-readable, localized name that describes this
   * editing action and can be displayed to the user.
   *
   * <p>The implementation delegates the call to the {@linkplain
   * #lastEdit() last added edit action}. If no edit has been added
   * yet, the inherited implementation will be invoked, which always
   * returns an empty string.
   */
  public String getPresentationName()
  {
    UndoableEdit last;

    last = lastEdit();
    if (last == null)
      return super.getPresentationName();
    else
      return last.getPresentationName();
  }


  /**
   * Calculates a localized message text for presenting the undo
   * action to the user.
   *
   * <p>The implementation delegates the call to the {@linkplain
   * #lastEdit() last added edit action}. If no edit has been added
   * yet, the {@linkplain
   * AbstractUndoableEdit#getUndoPresentationName() inherited
   * implementation} will be invoked.
   */
  public String getUndoPresentationName()
  {
    UndoableEdit last;

    last = lastEdit();
    if (last == null)
      return super.getUndoPresentationName();
    else
      return last.getUndoPresentationName();
  }


  /**
   * Calculates a localized message text for presenting the redo
   * action to the user.
   *
   * <p>The implementation delegates the call to the {@linkplain
   * #lastEdit() last added edit action}. If no edit has been added
   * yet, the {@linkplain
   * AbstractUndoableEdit#getRedoPresentationName() inherited
   * implementation} will be invoked.
   */
  public String getRedoPresentationName()
  {
    UndoableEdit last;

    last = lastEdit();
    if (last == null)
      return super.getRedoPresentationName();
    else
      return last.getRedoPresentationName();
  }
  
  
  /**
   * Calculates a string that may be useful for debugging.
   */
  public String toString()
  {
    return super.toString()
      + " inProgress: " + inProgress
      + " edits: " + edits;
  }
}
