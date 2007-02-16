/* MBeanServerDelegate.java -- The management server delegate.
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

import gnu.javax.management.ListenerData;
import gnu.classpath.SystemProperties;

import java.net.InetAddress;
import java.net.UnknownHostException;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * Provides an implementation of a delegate bean, which is associated
 * with a management server.  The delegate bean is responsible
 * for providing metadata about the server and handling the
 * registration and deregistration notifications.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanServerDelegate
  implements MBeanServerDelegateMBean, NotificationEmitter
{

  /**
   * The identifier of the server associated with this delegate.
   */
  private String id;

  /**
   * The listeners registered with the delegate.
   */
  private final List listeners = new ArrayList();

  /**
   * The sequence identifier used by the delegate.
   */
  private long seqNo;

  /**
   * Default constructor which generates the id.
   */
  public MBeanServerDelegate()
  {
    String hostName;
    try
      {
        hostName = InetAddress.getLocalHost().getHostName();
      }
    catch (UnknownHostException e)
      {
        hostName = "Unknown host";
      }
    id = hostName + "_" + new Date().getTime();
  }

  /**
   * Registers the specified listener as a new recipient of
   * notifications from the delegate.  If non-null, the filter
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
  public void addNotificationListener(NotificationListener listener,
				      NotificationFilter filter,
				      Object passback)
    throws IllegalArgumentException
  {
    if (listener == null)
      throw new IllegalArgumentException("A null listener was supplied.");
    listeners.add(new ListenerData(listener, filter, passback));
  }

  /**
   * Returns the name of this Java Management eXtensions (JMX) implementation.
   *
   * @return the implementation name.
   */
  public String getImplementationName()
  {
    return "GNU JMX";
  }

  /**
   * Returns the vendor of this Java Management eXtensions (JMX) implementation.
   *
   * @return the implementation vendor.
   */
  public String getImplementationVendor()
  {
    return "The GNU Classpath Project";
  }

  /**
   * Returns the version of this Java Management eXtensions (JMX) implementation.
   *
   * @return the implementation version.
   */
  public String getImplementationVersion()
  {
    return SystemProperties.getProperty("gnu.classpath.version");
  }

  /**
   * Returns the unique identifier for this management server.
   *
   * @return the unique id of the server.
   */
  public String getMBeanServerId()
  {
    return id;
  }

  /**
   * Returns an array describing the notifications this
   * bean may send to its registered listeners.  Ideally, this
   * array should be complete, but in some cases, this may
   * not be possible.  However, be aware that some listeners
   * may expect this to be so.
   *
   * @return the array of possible notifications.
   */
  public MBeanNotificationInfo[] getNotificationInfo()
  {
    return new MBeanNotificationInfo[] 
      {
	new MBeanNotificationInfo(new String[]
	  {
	    MBeanServerNotification.REGISTRATION_NOTIFICATION,
	    MBeanServerNotification.UNREGISTRATION_NOTIFICATION,
	  },
				  MBeanServerNotification.class.getName(),
				  "Server registration notifications")
      };
  }

  /**
   * Returns the name of this Java Management eXtensions (JMX) specification.
   *
   * @return the specification name.
   */
  public String getSpecificationName()
  {
    return "JMX";
  }


  /**
   * Returns the vendor of this Java Management eXtensions (JMX) specification.
   *
   * @return the specification vendor.
   */
  public String getSpecificationVendor()
  {
    return "Sun Microsystems";
  }

  /**
   * Returns the version of this Java Management eXtensions (JMX) specification.
   *
   * @return the specification version.
   */
  public String getSpecificationVersion()
  {
    return "1.2";
  }

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from this bean.  This includes all combinations
   * of filters and passback objects registered for this listener.
   * For more specific removal of listeners, see
   * {@link #removeNotificationListener(NotificationListener,
   * NotificationFilter, java.lang.Object)}
   *
   * @param listener the listener to remove.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with this bean.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
   */
  public void removeNotificationListener(NotificationListener listener)
    throws ListenerNotFoundException
  {
    Iterator it = listeners.iterator();
    boolean foundOne = false;
    while (it.hasNext())
      {
	ListenerData data = (ListenerData) it.next();
	if (data.getListener() == listener)
	  {
	    it.remove();
	    foundOne = true;
	  }
      }
    if (!foundOne)
      throw new ListenerNotFoundException("The specified listener, " + listener +
					  "is not registered with this bean.");
  }

  /**
   * Removes the specified listener from the list of recipients
   * of notifications from this delegate.  Only the first instance with
   * the supplied filter and passback object is removed.
   * <code>null</code> is used as a valid value for these parameters,
   * rather than as a way to remove all registration instances for
   * the specified listener; for this behaviour instead, see
   * {@link #removeNotificationListener(NotificationListener)}.
   *
   * @param listener the listener to remove.
   * @param filter the filter of the listener to remove.
   * @param passback the passback object of the listener to remove.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with this bean.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
   * @see #removeNotificationListener(NotificationListener)
   */
  public void removeNotificationListener(NotificationListener listener,
					 NotificationFilter filter,
					 Object passback)
    throws ListenerNotFoundException
  {
    if (!(listeners.remove(new ListenerData(listener, filter, passback))))
      {
	throw new ListenerNotFoundException("The specified listener, " + listener +
					    " with filter " + filter + 
					    "and passback " + passback + 
					    ", is not registered with this bean.");
      }
  }

  /**
   * Allows the server to use the delegate to send a notification.
   * If the supplied notification has a sequence number <= 0, then
   * it is replaced with the delegate's own sequence number.
   *
   * @param notification the notification to send.
   */
  public void sendNotification(Notification notification)
  {
    if (notification.getSequenceNumber() <= 0)
      notification.setSequenceNumber(++seqNo);
    Iterator it = listeners.iterator();
    while (it.hasNext())
      {
	ListenerData ldata = (ListenerData) it.next();
	NotificationFilter filter = ldata.getFilter();
	if (filter == null || filter.isNotificationEnabled(notification))
	  ldata.getListener().handleNotification(notification, ldata.getPassback());
      }
  }

}
