/* NotificationBroadcasterSupport.java -- Supporting implementation.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.util.concurrent.Executor;

/**
 * <p>
 * Provides an implementation of the {@link NotificationEmitter}
 * interface, which beans may utilise by extension.  By default,
 * a synchronous dispatch system is provided, whereby the
 * {@link #handleNotification(NotificationListener, Notification,
 * Object)} is called once per listener by
 * {*@link #sendNotification(Notification)} before returning.
 * Thus, unless the listener is remote, it will have received
 * the notification before the method returns.
 * This may be changed by overriding the <code>handleNotification</code>
 * method, or by providing an {@link java.util.concurrent.Executor} to
 * use.  With the latter, the dispatch of a notification to a particular
 * listener will form one task performed by the executor.
 * </p>
 * <p>
 * Any exceptions thrown by the dispatch process will be caught, allowing
 * other listeners to still receive the notification.  However, any
 * {@link Error}s that are thrown will be propogated to the caller of
 * {@link #sendNotification(Notification)}.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class NotificationBroadcasterSupport
  implements NotificationEmitter
{

  /**
   * The executor for dispatch, or
   * <code>null</code> if this thread should
   * handle dispatch itself.
   */
  private Executor executor;

  /**
   * An array containing information on each
   * notification, or <code>null</code> if an
   * empty array should be returned by
   * {@link #getNotificationInfo()}.
   */
  private MBeanNotificationInfo[] info;

  /**
   * The list of listeners registered with
   * this broadcaster.
   */
  private final List<ListenerData> listeners =
    new ArrayList<ListenerData>();

  /**
   * Constructs a {@link NotificationBroadcasterSupport} using
   * the default synchronous dispatch model, where a single
   * thread sends the notification to all listeners.  This
   * is equivalent to calling
   * <code>NotificationBroadcasterSupport(null, null)</code>.
   */
  public NotificationBroadcasterSupport()
  {
    this(null, null);
  }

  /**
   * Constructs a {@link NotificationBroadcasterSupport} where
   * the specified (@link java.util.concurrent.Executor} is used
   * to perform each invocation of the
   * {@link #handleNotification(NotificationListener, Notification,
   * Object)} method.  Filtering is performed beforehand, by this
   * thread; only calls which have successfully passed through the
   * filter are sent to the executor.  This is equivalent to calling
   * <code>NotificationBroadcasterSupport(executor, null)</code>.
   * 
   * @param executor the executor to use for each call to
   *                 <code>handleNotification()</code>.
   * @since 1.6
   */
  public NotificationBroadcasterSupport(Executor executor)
  {
    this(executor, null);
  }

  /**
   * Constructs a {@link NotificationBroadcasterSupport} using
   * the default synchronous dispatch model, where a single
   * thread sends the notification to all listeners. The specified
   * {@link MBeanNotificationInfo} array is used to provide
   * information about the notifications on calls to
   * {@link #getNotificationInfo()}, where a clone will be
   * returned if the array is non-empty.   This is equivalent to
   * calling <code>NotificationBroadcasterSupport(null, info)</code>.
   *
   * @param info an array of {@link MBeanNotificationInfo} objects
   *             describing the notifications delivered by this 
   *             broadcaster.  This may be <code>null</code>, which
   *             is taken as being equivalent to an empty array.
   */
  public NotificationBroadcasterSupport(MBeanNotificationInfo... info)
  {
    this(null, info);
  }

  /**
   * Constructs a {@link NotificationBroadcasterSupport} where
   * the specified (@link java.util.concurrent.Executor} is used
   * to perform each invocation of the
   * {@link #handleNotification(NotificationListener, Notification,
   * Object)} method.  Filtering is performed beforehand, by this
   * thread; only calls which have successfully passed through the
   * filter are sent to the executor.  The specified
   * {@link MBeanNotificationInfo} array is used to provide
   * information about the notifications on calls to
   * {@link #getNotificationInfo()}, where a clone will be
   * returned if the array is non-empty.
   * 
   * @param executor the executor to use for each call to
   *                 <code>handleNotification()</code>.
   * @param info an array of {@link MBeanNotificationInfo} objects
   *             describing the notifications delivered by this 
   *             broadcaster.  This may be <code>null</code>, which
   *             is taken as being equivalent to an empty array.
   * @since 1.6
   */
  public NotificationBroadcasterSupport(Executor executor,
					MBeanNotificationInfo... info)
  {
    this.executor = executor;
    this.info = info;
  }

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
  public void addNotificationListener(NotificationListener listener,
				      NotificationFilter filter,
				      Object passback)
    throws IllegalArgumentException
  {
    if (listener == null)
      throw new IllegalArgumentException("Null listener added to bean.");
    listeners.add(new ListenerData(listener, filter, passback));
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
    if (info == null || info.length == 0)
      return new MBeanNotificationInfo[0];
    return (MBeanNotificationInfo[]) info.clone();
  }

  /**
   * This method is called on a per-listener basis, either
   * from this thread or the supplied executor, and may be
   * overridden by subclasses which wish to change how
   * notifications are delivered.  The default
   * implementation simply calls
   * <code>listener.handleNotification(notif, passback)</code>.
   *
   * @param listener the listener to send the notification to.
   * @param notif the notification to dispatch.
   * @param passback the passback object of the listener.
   */
  protected void handleNotification(NotificationListener listener,
				    Notification notif,
				    Object passback)
  {
    listener.handleNotification(notif, passback);
  }

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
  public void removeNotificationListener(NotificationListener listener)
    throws ListenerNotFoundException
  {
    Iterator<ListenerData> it = listeners.iterator();
    boolean foundOne = false;
    while (it.hasNext())
      {
	if (it.next().getListener() == listener)
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
   * of notifications from this bean.  Only the first instance with
   * the supplied filter and passback object is removed.
   * <code>null</code> is used as a valid value for these parameters,
   * rather than as a way to remove all registration instances for
   * the specified listener; for this behaviour instead, see the details
   * of the same method in {@link NotificationBroadcaster}.
   *
   * @param listener the listener to remove.
   * @param filter the filter of the listener to remove.
   * @param passback the passback object of the listener to remove.
   * @throws ListenerNotFoundException if the specified listener
   *                                   is not registered with this bean.
   * @see #addNotificationListener(NotificationListener, NotificationFilter,
   *                               java.lang.Object)
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
   * <p>
   * Performs delivery of the notification.  If an executor
   * was specified on construction, this will be used to call
   * {@link #handleNotification(NotificationListener, Notification,
   * Object)}.  If the executor is <code>null</code>, however,
   * this thread calls the method itself in order to perform a
   * synchronous dispatch of the notification to all eligible
   * listeners.
   * </p>
   * <p>
   * Prior to either process taking place, the listeners are filtered.
   * Notifications are only delivered if the filter is either
   * <code>null</code> or returns true from the
   * {@link NotificationFilter#isNotificationEnabled(Notification)}
   * method.
   * </p>
   *
   * @param notif the notification to send.
   */
  public void sendNotification(Notification notif)
  {
    for (ListenerData ldata : listeners)
      {
	NotificationFilter filter = ldata.getFilter();
	if (filter == null || filter.isNotificationEnabled(notif))
	  {
	    if (executor == null)
	      try
		{
		  handleNotification(ldata.getListener(), notif,
				     ldata.getPassback());
		}
	      catch (Exception e) { /* Ignore */ }
	    else
	      executor.execute(new DispatchTask(ldata, notif));
	  }
      }
  }

  /**
   * The dispatch task to be performed by an executor.
   */
  private final class DispatchTask
    implements Runnable
  {

    /**
     * The data on the listener being called.
     */
    private ListenerData ldata;

    /**
     * The notification to send.
     */
    private Notification notif;

    /**
     * Construct a new {@link DispatchTask}.
     *
     * @param ldata the listener data.
     * @param notif the notification to send.
     */
    public DispatchTask(ListenerData ldata,
			Notification notif)
    {
      this.ldata = ldata;
      this.notif = notif;
    }

    /**
     * Dispatch the notification.
     */
    public void run()
    {
      try
	{
	  handleNotification(ldata.getListener(), notif,
			     ldata.getPassback());
	}
      catch (Exception e) { /* Ignore */ }
    }
  }

}

