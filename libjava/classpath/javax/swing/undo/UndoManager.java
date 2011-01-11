/* UndoManager.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import javax.swing.UIManager;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;


/**
 * A manager for providing an application&#x2019;s undo/redo
 * functionality.
 *
 * <p>Tyipcally, an application will create only one single instance
 * of UndoManager. When the user performs an undoable action, for
 * instance changing the color of an object from green to blue, the
 * application registers an {@link UndoableEdit} object with the
 * <code>UndoManager</code>. To implement the &#x201c;undo&#x201d; and
 * &#x201c;redo&#x201d; menu commands, the application invokes the
 * UndoManager&#x2019;s {@link #undo} and {@link #redo} methods.  The
 * human-readable text of these menu commands is provided by {@link
 * #getUndoPresentationName} and {@link #getRedoPresentationName},
 * respectively. To determine whether the menu item should be
 * selectable or greyed out, use {@link #canUndo} and {@link
 * #canRedo}.
 *
 * <p>The UndoManager will only keep a specified number of editing
 * actions, the <em>limit</em>. The value of this parameter can be
 * retrieved by calling {@link #getLimit} and set with {@link
 * #setLimit}.  If more UndoableEdits are added to the UndoManager,
 * the oldest actions will be discarded.
 *
 * <p>Some applications do not provide separate menu commands for
 * &#x201c;undo&#x201d; and &#x201c;redo.&#x201d; Instead, they
 * have just a single command whose text switches between the two.
 * Such applications would use an UndoManager with a <code>limit</code>
 * of 1. The text of this combined menu item is available via
 * {@link #getUndoOrRedoPresentationName}, and it is implemented
 * by calling {@link #undoOrRedo}.
 *
 * <p><b>Thread Safety:</b> In constrast to the other classes of the
 * <code>javax.swing.undo</code> package, the public methods of an
 * <code>UndoManager</code> are safe to call from concurrent threads.
 * The caller does not need to perform external synchronization, and
 * {@link javax.swing.event.UndoableEditEvent} sources do not need to
 * broadcast their events from inside the Swing worker thread.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class UndoManager
  extends CompoundEdit
  implements UndoableEditListener
{
  /**
   * The unique ID for serializing instances of this class. Determined
   * using the <code>serialver</code> tool of Sun JDK 1.4.1_01 on
   * GNU/Linux.
   */
  static final long serialVersionUID = -2077529998244066750L;


  /**
   * An index into the inherited {@link #edits} Vector that indicates
   * at which position newly added editing actions would get inserted.
   *
   * <p>Normally, the value of <code>indexOfNextAdd</code> equals
   * the number of UndoableEdits stored by this UndoManager, i.e.
   * <code>edits.size()</code>. For each call to {@link #undo},
   * <code>indexOfNextAdd</code> is decremented by one. For each
   * call to {@link #redo}, it is incremented again.
   */
  int indexOfNextAdd;


  /**
   * The maximum number of UndoableEdits stored by this UndoManager.
   */
  int limit;


  /**
   * Constructs an UndoManager.
   *
   * <p>The <code>limit</code> of the freshly constructed UndoManager
   * is 100.
   */
  public UndoManager()
  {
    limit = 100;
  }


  /**
   * Returns a string representation for this UndoManager. This may be
   * useful for debugging purposes. For the text of menu items, please
   * refer to {@link #getUndoPresentationName}, {@link
   * #getRedoPresentationName}, and {@link
   * #getUndoOrRedoPresentationName}.
   */
  public String toString()
  {
    return super.toString()
      + " limit: " + limit
      + " indexOfNextAdd: " + indexOfNextAdd;
  }


  /**
   * Puts this UndoManager into a state where it acts as a normal
   * {@link CompoundEdit}. It is unlikely that an application would
   * want to do this.
   */
  public synchronized void end()
  {
    super.end();
    trimEdits(indexOfNextAdd, edits.size() - 1);
  }


  /**
   * Returns how many edits this UndoManager can maximally hold.
   *
   * @see #setLimit
   */
  public synchronized int getLimit()
  {
    return limit;
  }


  /**
   * Changes the maximal number of edits that this UndoManager can
   * process. If there are currently more edits than the new limit
   * allows, they will receive a {@link UndoableEdit#die() die}
   * message in reverse order of addition.
   *
   * @param limit the new limit.
   *
   * @throws IllegalStateException if {@link #end()} has already been
   * called on this UndoManager.
   */
  public synchronized void setLimit(int limit)
  {
    if (!isInProgress())
      throw new IllegalStateException();

    this.limit = limit;
    trimForLimit();
  }


  /**
   * Discards all editing actions that are currently registered with
   * this UndoManager. Each {@link UndoableEdit} will receive a {@link
   * UndoableEdit#die() die message}.
   */
  public synchronized void discardAllEdits()
  {
    int size;

    size = edits.size();
    for (int i = size - 1; i >= 0; i--)
      edits.get(i).die();
    indexOfNextAdd = 0;
    edits.clear();
  }


  /**
   * Called by various internal methods in order to enforce
   * the <code>limit</code> value.
   */
  protected void trimForLimit()
  {
    int high, s;

    s = edits.size();

    /* The Sun J2SE1.4.1_01 implementation can be observed to do
     * nothing (instead of throwing an exception) with a negative or
     * zero limit. It may be debatable whether this is the best
     * behavior, but we replicate it for sake of compatibility.
     */
    if (limit <= 0 || s <= limit)
      return;

    high = Math.min(indexOfNextAdd + limit/2 - 1, s - 1);
    trimEdits(high + 1, s - 1);
    trimEdits(0, high - limit);
  }


  /**
   * Discards a range of edits. All edits in the range <code>[from
   * .. to]</code> will receive a {@linkplain UndoableEdit#die() die
   * message} before being removed from the edits array.  If
   * <code>from</code> is greater than <code>to</code>, nothing
   * happens.
   *
   * @param from the lower bound of the range of edits to be
   * discarded.
   *
   * @param to the upper bound of the range of edits to be discarded.
   */
  protected void trimEdits(int from, int to)
  {
    if (from > to)
      return;

    for (int i = to; i >= from; i--)
        edits.get(i).die();

    // Remove the range [from .. to] from edits. If from == to, which
    // is likely to be a very common case, we can do better than
    // creating a sub-list and clearing it.
    if (to == from)
      edits.remove(from);
    else
      edits.subList(from, to + 1).clear();

    if (indexOfNextAdd > to)
      indexOfNextAdd = indexOfNextAdd - to + from - 1;
    else if (indexOfNextAdd >= from)
      indexOfNextAdd = from;
  }


  /**
   * Determines which significant edit would be undone if {@link
   * #undo()} was called.
   *
   * @return the significant edit that would be undone, or
   * <code>null</code> if no significant edit would be affected by
   * calling {@link #undo()}.
   */
  protected UndoableEdit editToBeUndone()
  {
    UndoableEdit result;

    for (int i = indexOfNextAdd - 1; i >= 0; i--)
      {
        result = edits.get(i);
        if (result.isSignificant())
          return result;
      }

    return null;
  }


  /**
   * Determines which significant edit would be redone if {@link
   * #redo()} was called.
   *
   * @return the significant edit that would be redone, or
   * <code>null</code> if no significant edit would be affected by
   * calling {@link #redo()}.
   */
  protected UndoableEdit editToBeRedone()
  {
    UndoableEdit result;

    for (int i = indexOfNextAdd; i < edits.size(); i++)
      {
        result = edits.get(i);
        if (result.isSignificant())
          return result;
      }

    return null;
  }


  /**
   * Undoes all editing actions in reverse order of addition,
   * up to the specified action,
   *
   * @param edit the last editing action to be undone.
   */
  protected void undoTo(UndoableEdit edit)
    throws CannotUndoException
  {
    UndoableEdit cur;

    if (!edits.contains(edit))
      throw new CannotUndoException();

    while (true)
      {
        indexOfNextAdd -= 1;
        cur = edits.get(indexOfNextAdd);
        cur.undo();
        if (cur == edit)
          return;
      }
  }


  /**
   * Redoes all editing actions in the same order as they were
   * added to this UndoManager, up to the specified action.
   *
   * @param edit the last editing action to be redone.
   */
  protected void redoTo(UndoableEdit edit)
    throws CannotRedoException
  {
    UndoableEdit cur;

    if (!edits.contains(edit))
      throw new CannotRedoException();

    while (true)
      {
        cur = edits.get(indexOfNextAdd);
        indexOfNextAdd += 1;
        cur.redo();
        if (cur == edit)
          return;
      }
  }


  /**
   * Undoes or redoes the last action. If the last action has already
   * been undone, it will be re-done, and vice versa.
   *
   * <p>This is useful for applications that do not present a separate
   * undo and redo facility, but just have a single menu item for
   * undoing and redoing the very last action. Such applications will
   * use an <code>UndoManager</code> whose <code>limit</code> is 1.
   */
  public synchronized void undoOrRedo()
    throws CannotRedoException, CannotUndoException
  {
    if (indexOfNextAdd == edits.size())
      undo();
    else
      redo();
  }


  /**
   * Determines whether it would be possible to either undo or redo
   * this editing action.
   *
   * <p>This is useful for applications that do not present a separate
   * undo and redo facility, but just have a single menu item for
   * undoing and redoing the very last action. Such applications will
   * use an <code>UndoManager</code> whose <code>limit</code> is 1.
   *
   * @return <code>true</code> to indicate that this action can be
   * undone or redone; <code>false</code> if neither is possible at
   * the current time.
   */
  public synchronized boolean canUndoOrRedo()
  {
    return indexOfNextAdd == edits.size() ? canUndo() : canRedo();
  }


  /**
   * Undoes one significant edit action. If insignificant actions have
   * been posted after the last signficant action, the insignificant
   * ones will be undone first.
   *
   * <p>However, if {@link #end()} has been called on this
   * UndoManager, it will behave like a normal {@link
   * CompoundEdit}. In this case, all actions will be undone in
   * reverse order of addition. Typical applications will never call
   * {@link #end()} on their <code>UndoManager</code>.
   *
   * @throws CannotUndoException if no action can be undone.
   *
   * @see #canUndo()
   * @see #redo()
   * @see #undoOrRedo()
   */
  public synchronized void undo()
    throws CannotUndoException
  {
    if (!isInProgress())
      {
        super.undo();
        return;
      }

    UndoableEdit edit = editToBeUndone();
    if (edit == null)
      throw new CannotUndoException();

    undoTo(edit);
  }


  /**
   * Determines whether it would be possible to undo this editing
   * action.
   *
   * @return <code>true</code> to indicate that this action can be
   * undone; <code>false</code> otherwise.
   *
   * @see #undo()
   * @see #canRedo()
   * @see #canUndoOrRedo()
   */
  public synchronized boolean canUndo()
  {
    UndoableEdit edit;

    if (!isInProgress())
      return super.canUndo();

    edit = editToBeUndone();
    return edit != null && edit.canUndo();
  }



  /**
   * Redoes one significant edit action. If insignificant actions have
   * been posted in between, the insignificant ones will be redone
   * first.
   *
   * <p>However, if {@link #end()} has been called on this
   * UndoManager, it will behave like a normal {@link
   * CompoundEdit}. In this case, <em>all</em> actions will be redone
   * in order of addition. Typical applications will never call {@link
   * #end()} on their <code>UndoManager</code>.
   *
   * @throws CannotRedoException if no action can be redone.
   *
   * @see #canRedo()
   * @see #redo()
   * @see #undoOrRedo()
   */
  public synchronized void redo()
    throws CannotRedoException
  {
    if (!isInProgress())
      {
        super.redo();
        return;
      }

    UndoableEdit edit = editToBeRedone();
    if (edit == null)
      throw new CannotRedoException();

    redoTo(edit);
  }


  /**
   * Determines whether it would be possible to redo this editing
   * action.
   *
   * @return <code>true</code> to indicate that this action can be
   * redone; <code>false</code> otherwise.
   *
   * @see #redo()
   * @see #canUndo()
   * @see #canUndoOrRedo()
   */
  public synchronized boolean canRedo()
  {
    UndoableEdit edit;

    if (!isInProgress())
      return super.canRedo();

    edit = editToBeRedone();
    return edit != null && edit.canRedo();
  }


  /**
   * Registers an undoable editing action with this UndoManager.  If
   * the capacity <code>limit</code> is reached, the oldest action
   * will be discarded (and receives a {@linkplain UndoableEdit#die()
   * die message}. Equally, any actions that were undone (but not re-done)
   * will be discarded, too.
   *
   * @param edit the editing action that is added to this UndoManager.
   *
   * @return <code>true</code> if <code>edit</code> could be
   * incorporated; <code>false</code> if <code>edit</code> has not
   * been incorporated because {@link #end()} has already been called
   * on this <code>UndoManager</code>.
   */
  public synchronized boolean addEdit(UndoableEdit edit)
  {
    boolean result;

    // Discard any edits starting at indexOfNextAdd.
    trimEdits(indexOfNextAdd, edits.size() - 1);

    result = super.addEdit(edit);
    indexOfNextAdd = edits.size();
    trimForLimit();
    return result;
  }


  /**
   * Calculates a localized text for presenting the undo or redo
   * action to the user, for example in the form of a menu command.
   *
   * <p>This is useful for applications that do not present a separate
   * undo and redo facility, but just have a single menu item for
   * undoing and redoing the very last action. Such applications will
   * use an <code>UndoManager</code> whose <code>limit</code> is 1.
   *
   * @return the redo presentation name if the last action has already
   * been undone, or the undo presentation name otherwise.
   *
   * @see #getUndoPresentationName()
   * @see #getRedoPresentationName()
   */
  public synchronized String getUndoOrRedoPresentationName()
  {
    if (indexOfNextAdd == edits.size())
      return getUndoPresentationName();
    else
      return getRedoPresentationName();
  }


  /**
   * Calculates a localized text for presenting the undo action
   * to the user, for example in the form of a menu command.
   */
  public synchronized String getUndoPresentationName()
  {
    UndoableEdit edit;

    if (!isInProgress())
      return super.getUndoPresentationName();

    edit = editToBeUndone();
    if (edit == null)
      return UIManager.getString("AbstractUndoableEdit.undoText");
    else
      return edit.getUndoPresentationName();
  }


  /**
   * Calculates a localized text for presenting the redo action
   * to the user, for example in the form of a menu command.
   */
  public synchronized String getRedoPresentationName()
  {
    UndoableEdit edit;

    if (!isInProgress())
      return super.getRedoPresentationName();

    edit = editToBeRedone();
    if (edit == null)
      return UIManager.getString("AbstractUndoableEdit.redoText");
    else
      return edit.getRedoPresentationName();
  }


  /**
   * Registers the edit action of an {@link UndoableEditEvent}
   * with this UndoManager.
   *
   * <p><b>Thread Safety:</b> This method may safely be invoked from
   * concurrent threads.  The caller does not need to perform external
   * synchronization. This means that {@link
   * javax.swing.event.UndoableEditEvent} sources do not need to broadcast
   * their events from inside the Swing worker thread.
   *
   * @param event the event whose <code>edit</code> will be
   * passed to {@link #addEdit}.
   *
   * @see UndoableEditEvent#getEdit()
   * @see #addEdit
   */
  public void undoableEditHappened(UndoableEditEvent event)
  {
    // Note that this method does not need to be synchronized,
    // because addEdit will obtain and release the mutex.
    addEdit(event.getEdit());
  }
}
