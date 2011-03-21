/* TargetedNotificaton.java -- Wrapper for a notification and identifier pair.
   Copyright (C) 2008 Free Software Foundation, Inc.

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

package javax.management.remote;

import java.io.Serializable;

import javax.management.Notification;

/**
 * Wraps a notification with an identifier that specifies
 * the listener which received it.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class TargetedNotification
  implements Serializable
{

  /**
   * Compatible with JDK 1.6
   */
  private static final long serialVersionUID = 7676132089779300926L;

  /**
   * The notification that was recieved by the listener.
   */
  private Notification notif;

  /**
   * The identifier for the listener that received the notification;
   */
  private Integer id;

  /**
   * Constructs a new {@link TargetedNotification} which connects
   * the supplied notification with the specified identifier.  The
   * identifier matches one of those returned by a previous call
   * to add a new notification listener.
   *
   * @param notif the notification.
   * @param id the identifier of the listener that received the
   *           notification.
   * @throws IllegalArgumentException if either argument is
   *                                  <code>null</code>.
   */
  public TargetedNotification(Notification notif, Integer id)
  {
    if (notif == null)
      throw new IllegalArgumentException("The notification is null.");
    if (id == null)
      throw new IllegalArgumentException("The identifier is null.");
    this.notif = notif;
    this.id = id;
  }

  /**
   * Returns the notification.
   *
   * @return the notification.
   */
  public Notification getNotification()
  {
    return notif;
  }

  /**
   * Returns the identifier for the listener
   * which received the notification.
   *
   * @return the identifier.
   */
  public Integer getListenerID()
  {
    return id;
  }

  /**
   * Returns a textual representation of the object.
   *
   * @return a textual representation.
   */
  public String toString()
  {
    return getClass().getName() +
      "[notif=" + notif +
      ",id=" + id +
      "]";
  }

}
