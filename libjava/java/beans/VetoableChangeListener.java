/* VetoableChangeListener.java -- listen for a change which can be vetoed
   Copyright (C) 1998, 2000, 2002 Free Software Foundation, Inc.

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


package java.beans;

import java.util.EventListener;

/**
 * VetoableChangeListener allows a class to monitor proposed changes to
 * properties of a Bean and, if desired, prevent them from occurring. A
 * vetoableChange() event will be fired <em>after</em> the property change has
 * been requested, but before it is permanent. If any listener rejects the
 * change by throwing the PropertyChangeException, a new vetoableChange()
 * event will be fired to all listeners who received a vetoableChange() event
 * in the first place, informing them to revert back to the old value. Thus,
 * the listener that threw the exception the first time should be prepared
 * to rethrow it the second time. The value, of course, never actually changed.
 *
 * <p><strong>Note:</strong> This class may not be reliably used to determine
 * whether a property has actually changed.  Use the PropertyChangeListener
 * interface for that instead.
 *
 * @author John Keiser
 * @see java.beans.PropertyChangeListener
 * @see java.beans.VetoableChangeSupport
 * @since 1.1
 * @status updated to 1.4
 */
public interface VetoableChangeListener extends EventListener
{
  /**
   * Fired before a Bean's property changes.
   *
   * @param e the change (containing the old and new values)
   * @throws PropertyVetoException if the change is vetoed by the listener
   */
  void vetoableChange(PropertyChangeEvent e) throws PropertyVetoException;
} // interface VetoableChangeListener
