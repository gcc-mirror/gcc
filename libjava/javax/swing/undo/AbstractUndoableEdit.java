/* AbstractUndoableEdit.java --
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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

import java.io.Serializable;

import javax.swing.UIManager;

/**
 * A default implementation of <code>UndoableEdit</code> that can be
 * used as a base for implementing editing operations.
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class AbstractUndoableEdit
  implements UndoableEdit, Serializable
{
  /**
   * The serialization ID.  Verified using the <code>serialver</code>
   * tool of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5, and Sun JDK
   * 1.4.1_01 on GNU/Linux.
   */
  static final long serialVersionUID = 580150227676302096L;


  /**
   * The constant string &#x201c;Undo&#x201d;, which was returned by
   * {@link #getUndoPresentationName()} on early versions of the
   * platform. However, this field has become obsolete with version
   * 1.3.1.  That method now retrieves a localized string from the
   * {@link javax.swing.UIManager}, using the key
   * <code>&#x201c;AbstractUndoableEdit.undoText&#x201d;</code>.
   */
  protected static final String UndoName = "Undo";


  /**
   * The constant string &#x201c;Redo&#x201d;, which was returned by
   * {@link #getRedoPresentationName()} on early versions of the
   * platform. However, this field has become obsolete with version
   * 1.3.1.  That method now retrieves a localized string from the
   * {@link javax.swing.UIManager}, using the key
   * <code>&#x201c;AbstractUndoableEdit.redoText&#x201d;</code>.
   */
  protected static final String RedoName = "Redo";


  /**
   * Indicates whether this editing action has been executed.  A value
   * of <code>true</code> means that the action was performed, or that
   * a redo operation was successful. A value of <code>false</code>
   * means that the action has not yet performed, or that an undo
   * operation was successful.
   */
  private boolean hasBeenDone;


  /**
   * Indicates whether this editing action is still alive. The value
   * is set to <code>true</code> by the constructor, and to
   * <code>false</code> by the {@link #die()} method.
   */
  private boolean alive;


  /**
   * Constructs a new <code>AbstractUndoableEdit</code>. The initial
   * state is that the editing action is alive, and
   * <code>hasBeenDone</code> is <code>true</code>.
   */
  public AbstractUndoableEdit()
  {
    // The API specification is not clear, but Mauve test code has
    // determined that hasBeenDone is initially set to true.
    alive = hasBeenDone = true;
  }


  /**
   * Undoes this editing action.
   *
   * @throws CannotUndoException if {@link #canUndo()} returns
   * <code>false</code>, for example because this action has already
   * been undone.
   *
   * @see #canUndo()
   * @see #redo()
   */
  public void undo()
    throws CannotUndoException
  {
    if (!canUndo())
      throw new CannotUndoException();
    hasBeenDone = false;
  }
  
  
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
  public boolean canUndo()
  {
    return alive && hasBeenDone;
  }
  
  
  /**
   * Redoes this editing action.
   *
   * @throws CannotRedoException if {@link #canRedo()} returns
   * <code>false</code>, for example because this action has not
   * yet been undone.
   *
   * @see #canRedo()
   * @see #undo()
   */
  public void redo()
    throws CannotRedoException
  {
    if (!canRedo())
      throw new CannotRedoException();
    hasBeenDone = true;
  }
  
  
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
  public boolean canRedo()
  {
    return alive && !hasBeenDone;
  }


  /**
   * Informs this edit action that it will no longer be used. Some
   * actions might use this information to release resources, for
   * example open files.  Called by {@link UndoManager} before this
   * action is removed from the edit queue.
   */
  public void die()
  {
    alive = false;
  }


  /**
   * Incorporates another editing action into this one, thus forming a
   * combined action.
   *
   * <p>The default implementation always returns <code>false</code>,
   * indicating that the editing action could not be incorporated.
   *
   * @param edit the editing action to be incorporated.
   */
  public boolean addEdit(UndoableEdit edit)
  {
    return false;
  }
  
  
  /**
   * Incorporates another editing action into this one, thus forming a
   * combined action that replaces the argument action.
   *
   * <p>The default implementation always returns <code>false</code>,
   * indicating that the argument action should not be replaced.
   *
   * @param edit the editing action to be replaced.
   */
  public boolean replaceEdit(UndoableEdit edit)
  {
    return false;
  }
  
  
  /**
   * Determines whether this editing action is significant enough for
   * being seperately undoable by the user. A typical significant
   * action would be the resizing of an object. However, changing the
   * selection in a text document would usually not be considered
   * significant.
   *
   * <p>The default implementation returns <code>true</code>.
   *
   * @return <code>true</code> to indicate that the action is
   * significant enough for being separately undoable, or
   * <code>false</code> otherwise.
   */
  public boolean isSignificant()
  {
    return true;
  }
  
  
  /**
   * Returns a human-readable, localized name that describes this
   * editing action and can be displayed to the user.
   *
   * <p>The default implementation returns an empty string.
   */
  public String getPresentationName()
  {
    return "";
  }


  /**
   * Calculates a localized name for presenting the undo action to the
   * user.
   *
   * <p>The default implementation returns the concatenation of the
   * string &#x201c;Undo&#x201d; and the action name, which is
   * determined by calling {@link #getPresentationName()}.
   *
   * <p>The string &#x201c;Undo&#x201d; is retrieved from the {@link
   * javax.swing.UIManager}, using the key
   * <code>&#x201c;AbstractUndoableEdit.undoText&#x201d;</code>.  This
   * allows the text to be localized.
   */
  public String getUndoPresentationName()
  {
    String msg, pres;

    msg = UIManager.getString("AbstractUndoableEdit.undoText");
    if (msg == null)
      msg = UndoName;

    pres = getPresentationName();
    if ((pres == null) || (pres.length() == 0))
      return msg;
    else
      return msg + ' ' + pres;
  }


  /**
   * Calculates a localized name for presenting the redo action to the
   * user.
   *
   * <p>The default implementation returns the concatenation of the
   * string &#x201c;Redo&#x201d; and the action name, which is
   * determined by calling {@link #getPresentationName()}.
   *
   * <p>The string &#x201c;Redo&#x201d; is retrieved from the {@link
   * javax.swing.UIManager}, using the key
   * <code>&#x201c;AbstractUndoableEdit.redoText&#x201d;</code>.  This
   * allows the text to be localized.
   */
  public String getRedoPresentationName()
  {
    String msg, pres;

    msg = UIManager.getString("AbstractUndoableEdit.redoText");
    if (msg == null)
      msg = RedoName;

    pres = getPresentationName();
    if ((pres == null) || (pres.length() == 0))
      return msg;
    else
      return msg + ' ' + pres;
  }


  public String toString()
  {
    return super.toString()
      + " hasBeenDone: " + hasBeenDone
      + " alive: " + alive;
  }
}
