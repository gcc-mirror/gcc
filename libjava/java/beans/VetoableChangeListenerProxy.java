/* VetoableChangeListenerProxy.java -- adds a name to a vetoable listener
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package java.beans;

import java.util.EventListenerProxy;

/**
 * This class provides an extension to <code>VetoableChangeListener</code> -
 * associating a name with the listener. This can be used to filter the
 * changes that one is interested in.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.4
 * @status udpated to 1.4
 */
public class VetoableChangeListenerProxy extends EventListenerProxy
  implements VetoableChangeListener
{
  /**
   * The name of the property to listen for. Package visible for use by
   * VetoableChangeSupport.
   */
  final String propertyName;

  /**
   * Create a new proxy which filters property change events and only passes
   * changes to the named property on to the delegate.
   *
   * @param propertyName the property's name to filter on
   * @param listener the delegate listener
   */
  public VetoableChangeListenerProxy(String propertyName,
                                     VetoableChangeListener listener)
  {
    super(listener);
    this.propertyName = propertyName;
  }

  /**
   * Forwards the event on to the delegate if the property name matches.
   *
   * @param event the event to pass on, if it meets the filter
   * @throws NullPointerException if the delegate this was created with is null
   * @throws PropertyVetoException if the change is vetoed by the listener
   */
  public void vetoableChange(PropertyChangeEvent event)
    throws PropertyVetoException
  {
    // Note: Sun does not filter, under the assumption that since
    // VetoableChangeSupport unwraps proxys, this method should never be
    // called by normal use of listeners.
    String name = event == null ? null : event.getPropertyName();
    if (name == null ? propertyName == null : name.equals(propertyName))
      ((VetoableChangeListener) getListener()).vetoableChange(event);
  }

  /**
   * Gets the name of the property this proxy is filtering on.
   *
   * @return the property name
   */
  public String getPropertyName()
  {
    return propertyName;
  }
} // class VetoableChangeListenerProxy
