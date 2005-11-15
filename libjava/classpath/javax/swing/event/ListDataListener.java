/* ListDataListener.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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

package javax.swing.event;

import java.util.EventListener;

import javax.swing.ListModel;

/**
 * A <code>ListDataListener</code> can register with a {@link ListModel} and
 * receive notification of updates to the model.
 * 
 * @author Andrew Selkirk
 * @author Ronald Veldema
 */
public interface ListDataListener extends EventListener 
{

  /**
   * Notifies the listener that the contents of the list have changed
   * in some way.  This method will be called if the change cannot be
   * notified via the {@link #intervalAdded(ListDataEvent)} or the
   * {@link #intervalRemoved(ListDataEvent)} methods.
   * 
   * @param event  the event.
   */
  void contentsChanged(ListDataEvent event);

  /**
   * Notifies the listener that one or more items have been added to the
   * list.  The <code>event</code> argument can supply the indices for the
   * range of items added.
   * 
   * @param event  the event.
   */
  void intervalAdded(ListDataEvent event);

  /**
   * Notifies the listener that one or more items have been removed from
   * the list.  The <code>event</code> argument can supply the indices for 
   * the range of items removed.
   * 
   * @param event  the event.
   */
  void intervalRemoved(ListDataEvent event);

}
