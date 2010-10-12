/* UndoableEditSupport.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import java.util.Iterator;
import java.util.Vector;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;

/**
 * A helper class for supporting {@link
 * javax.swing.event.UndoableEditListener}.
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class UndoableEditSupport
{
  /**
   * The number of times that {@link #beginUpdate()} has been called
   * without a matching call to {@link #endUpdate()}.
   */
  protected int updateLevel;


  /**
   * compoundEdit
   */
  protected CompoundEdit compoundEdit;


  /**
   * The currently registered listeners.
   */
  protected Vector<UndoableEditListener> listeners =
    new Vector<UndoableEditListener>();


  /**
   * The source of the broadcast UndoableEditEvents.
   */
  protected Object realSource;


  /**
   * Constructs a new helper for broadcasting UndoableEditEvents.  The
   * events will indicate the newly constructed
   * <code>UndoableEditSupport</code> instance as their source.
   *
   * @see #UndoableEditSupport(java.lang.Object)
   */
  public UndoableEditSupport()
  {
    realSource = this;
  }


  /**
   * Constructs a new helper for broadcasting UndoableEditEvents.
   *
   * @param realSource the source of the UndoableEditEvents that will
   * be broadcast by this helper. If <code>realSource</code> is
   * <code>null</code>, the events will indicate the newly constructed
   * <code>UndoableEditSupport</code> instance as their source.
   */
  public UndoableEditSupport(Object realSource)
  {
    if (realSource == null)
      realSource = this;
    this.realSource = realSource;
  }


  /**
   * Returns a string representation of this object that may be useful
   * for debugging.
   */
  public String toString()
  {
    // Note that often, this.realSource == this. Therefore, dumping
    // realSource without additional checks may lead to infinite
    // recursion. See Classpath bug #7119.
    return super.toString() + " updateLevel: " + updateLevel
      + " listeners: " + listeners + " compoundEdit: " + compoundEdit;
  }


  /**
   * Registers a listener.
   *
   * @param val the listener to be added.
   */
  public synchronized void addUndoableEditListener(UndoableEditListener val)
  {
    listeners.add(val);
  }


  /**
   * Unregisters a listener.
   * @param val the listener to be removed.
   */
  public synchronized void removeUndoableEditListener(UndoableEditListener val)
  {
    listeners.removeElement(val);
  }


  /**
   * Returns an array containing the currently registered listeners.
   */
  public synchronized UndoableEditListener[] getUndoableEditListeners()
  {
    UndoableEditListener[] result = new UndoableEditListener[listeners.size()];
    return listeners.toArray(result);
  }


  /**
   * Notifies all registered listeners that an {@link
   * UndoableEditEvent} has occured.
   *
   * <p><b>Lack of Thread Safety:</b> It is <em>not</em> safe to call
   * this method from concurrent threads, unless the call is protected
   * by a synchronization on this <code>UndoableEditSupport</code>
   * instance.
   *
   * @param edit the edit action to be posted.
   */
  protected void _postEdit(UndoableEdit edit)
  {
    UndoableEditEvent event;
    Iterator<UndoableEditListener> iter;

    // Do nothing if we have no listeners.
    if (listeners.isEmpty())
      return;

    event = new UndoableEditEvent(realSource, edit);

    // We clone the vector because this allows listeners to register
    // or unregister listeners in their undoableEditHappened method.
    // Otherwise, this would throw exceptions (in the case of
    // Iterator, a java.util.ConcurrentModificationException; in the
    // case of a direct loop over the Vector elements, some
    // index-out-of-bounds exception).
    iter = new Vector<UndoableEditListener>(listeners).iterator();
    while (iter.hasNext())
      iter.next().undoableEditHappened(event);
  }


  /**
   * If {@link #beginUpdate} has been called (so that the current
   * update level is greater than zero), adds the specified edit
   * to {@link #compoundEdit}. Otherwise, notify listeners of the
   * edit by calling {@link #_postEdit(UndoableEdit)}.
   *
   * <p><b>Thread Safety:</b> It is safe to call this method from any
   * thread without external synchronization.
   *
   * @param edit the edit action to be posted.
   */
  public synchronized void postEdit(UndoableEdit edit)
  {
    if (compoundEdit != null)
      compoundEdit.addEdit(edit);
    else
      _postEdit(edit);
  }


  /**
   * Returns the current update level.
   */
  public int getUpdateLevel()
  {
    return updateLevel;
  }


  /**
   * Starts a (possibly nested) update session. If the current update
   * level is zero, {@link #compoundEdit} is set to the result of the
   * {@link #createCompoundEdit} method. In any case, the update level
   * is increased by one.
   *
   * <p><b>Thread Safety:</b> It is safe to call this method from any
   * thread without external synchronization.
   */
  public synchronized void beginUpdate()
  {
    if (compoundEdit == null)
      compoundEdit = createCompoundEdit();
    ++updateLevel;
  }


  /**
   * Creates a new instance of {@link CompoundEdit}. Called by {@link
   * #beginUpdate}. If a subclass wants {@link #beginUpdate} to work
   * on a specific {@link #compoundEdit}, it should override this
   * method.
   *
   * @return a newly created instance of {@link CompoundEdit}.
   */
  protected CompoundEdit createCompoundEdit()
  {
    return new CompoundEdit();
  }


  /**
   * Ends an update session. If the terminated session was the
   * outermost session, {@link #compoundEdit} will receive an
   * <code>end</code> message, and {@link #_postEdit} gets called in
   * order to notify any listeners. Finally, the
   * <code>compoundEdit</code> is discarded.
   *
   * <p><b>Thread Safety:</b> It is safe to call this method from any
   * thread without external synchronization.
   */
  public synchronized void endUpdate()
  {
    if (updateLevel == 0)
      throw new IllegalStateException();

    if (--updateLevel > 0)
      return;

    compoundEdit.end();
    _postEdit(compoundEdit);
    compoundEdit = null;
  }
}
