/* NotificationBroadcaster.java -- Interface for broadcasters.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

/**
 * <p>
 * Represents a bean that can emit notifications when
 * events occur.  Other beans can use this interface
 * to add themselves to the list of recipients of such
 * notifications.
 * </p>
 * <p>
 * <strong>Note</strong>: New classes should use
 * {@link NotificationEmitter}, a subinterface of this,
 * in preference to using this interface directly.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface NotificationBroadcaster
{
  
  /**
   * Registers the specified listener as a new recipient of
   * notifications from this bean.  If non-null, the filter
   * argument will be used to select which notifications are
   * delivered.  The supplied object will also be passed to
   * the recipient with each notification.  This should not
   * be modified by the broadcaster, but instead should be
   * passed unmodified to the listener.
   *
   * @param listener the new listener, who will receive
   *                 notifications from this broadcasting bean.
   * @param filter a filter to determine which notifications are
   *               delivered to the listener, or <code>null</code>
   *               if no filtering is required.
   * @param passback an object to be passed to the listener with
   *                 each notification.
   * @throws IllegalArgumentException if <code>listener</code> is
   *                                  <code>null</code>.
   * @see #removeNotificationListener(NotificationListener)
   */
  void addNotificationListener(NotificationListener listener,
			       NotificationFilter filter,
			       Object passback)
    throws IllegalArgumentException;

  /**
   * Returns an array describing the notifications this
   * bean may send to its registered listeners.  Ideally, this
   * array should be complete, but in some cases, this may
   * not be possible.  However, be aware that some listeners
   * may expect this to be so.
   *
   * @return the array of possible notifications.
   */
  MBeanNotificationInfo[] getNotificationInfo();

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from this bean.  This includes all combinations
   * of filters and passback objects registered for this listener.
   * For more specific removal of listeners, see the subinterface
   * {@link NotificationEmitter}.
   *
   * @param listener the listener to remove.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with this bean.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
   */
  void removeNotificationListener(NotificationListener listener)
    throws ListenerNotFoundException;

}

