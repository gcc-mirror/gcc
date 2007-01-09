/* MBeanServerNotification.java -- The registration notification.
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
 * The notification emitted by a management server on a registration
 * or de-registration event.  Events are emitted via the delegate
 * management bean of the server.  Other objects can listen for
 * such events by registering their interest with the delegate
 * bean.  The bean can be obtained via the {@link ObjectName}
 * <code>JMImplementation:type=MBeanServerDelegate</code>.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanServerNotification
  extends Notification
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 2876477500475969677L;

  /**
   * Notification type for the registration event.
   */
  public static final String REGISTRATION_NOTIFICATION = "JMX.mbean.registered";

  /**
   * Notification type for the de-registration event.
   */
  public static final String UNREGISTRATION_NOTIFICATION = "JMX.mbean.unregistered";

  /**
   * The name of the bean which forms the subject of this notification.
   */
  private ObjectName objectName;

  /**
   * Constructs a new {@link MBeanServerNotification} of the specified
   * type for an event relating to the supplied bean, with the given
   * source and sequence number.
   *
   * @param type the type of notification (registration or de-registration).
   * @param source the source of the notification.
   * @param seqNo the sequence number of this notification, used to order
   *              multiple such notifications.
   * @param name the name of the bean concerned by this event.
   */
  public MBeanServerNotification(String type, Object source, long seqNo,
				 ObjectName name)
  {
    super(type, source, seqNo);
    objectName = name;
  }

  /**
   * Returns the name of the bean this notification concerns.
   *
   * @return the name of the bean.
   */
  public ObjectName getMBeanName()
  {
    return objectName;
  }

}
