/* ListenerData.java - Class to contain data about management bean listeners
   Copyright (C) 2006 Free Software Foundation

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

package gnu.javax.management;

import javax.management.NotificationFilter;
import javax.management.NotificationListener;

/**
 * Container for data on management listeners.  Wraps
 * a {@link javax.management.NotificationListener},
 * {@link javax.management.NotificationFilter} and
 * passback object in one class.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ListenerData
{
  /**
   * The listener itself.
   */
  private NotificationListener listener;

  /**
   * A filter to apply to incoming events.
   */
  private NotificationFilter filter;

  /**
   * An object to pass back to the listener on an
   * event occurring.
   */
  private Object passback;

  /**
   * Constructs a new {@link ListenerData} with the specified
   * listener, filter and passback object.
   *
   * @param listener the listener itself.
   * @param filter the filter for incoming events.
   * @param passback the object to passback on an incoming event.
   */
  public ListenerData(NotificationListener listener,
                      NotificationFilter filter, Object passback)
  {
    this.listener = listener;
    this.filter = filter;
    this.passback = passback;
  }

  /**
   * Returns the listener.
   *
   * @return the listener.
   */
  public NotificationListener getListener()
  {
    return listener;
  }

  /**
   * Returns the filter.
   *
   * @return the filter.
   */
  public NotificationFilter getFilter()
  {
    return filter;
  }

  /**
   * Returns the passback object.
   *
   * @return the passback object.
   */
  public Object getPassback()
  {
    return passback;
  }

  /**
   * Returns true if the supplied object is an instance of
   * {@link ListenerData} and has the same listener, filter
   * and passback object.
   *
   * @param obj the object to check.
   * @return true if <code>obj</code> is equal to this.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ListenerData)
      {
        ListenerData data = (ListenerData) obj;
        return (data.getListener() == listener &&
                data.getFilter() == filter &&
                data.getPassback() == passback);
      }
    return false;
  }

}
