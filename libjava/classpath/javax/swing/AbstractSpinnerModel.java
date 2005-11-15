/* AbstractSpinnerModel.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing;

import java.util.EventListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * Provides standard implementations for some of the methods in
 * {@link SpinnerModel}.
 *
 * @author Ka-Hing Cheung
 */
public abstract class AbstractSpinnerModel implements SpinnerModel
{
  private ChangeEvent changeEvent = new ChangeEvent(this);
  
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * Creates an <code>AbstractSpinnerModel</code>.
   */
  public AbstractSpinnerModel()
  {
    // Nothing to do here.
  }

  /**
   * Adds a <code>ChangeListener</code>.
   *
   * @param listener the listener to add
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Gets all the listeners that are of a particular type.
   *
   * @param c the type of listener
   * @return the listeners that are of the specific type
   */
  public EventListener[] getListeners(Class c)
  {
    return listenerList.getListeners(c);
  }

  /**
   * Gets all the <code>ChangeListener</code>s.
   *
   * @return all the <code>ChangeListener</code>s
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Remove a particular listener.
   *
   * @param listener the listener to remove
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Fires a <code>ChangeEvent</code> to all the <code>ChangeListener</code>s
   * added to this model
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for(int i = 0; i < listeners.length; ++i)
      listeners[i].stateChanged(changeEvent);
  }
}
