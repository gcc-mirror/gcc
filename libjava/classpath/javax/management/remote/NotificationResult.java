/* NotificationResult.java -- Wrapper for a series of buffered notifications.
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

/**
 * <p>
 * Wraps the result of a query for buffered notifications.  In a remote
 * scenario, it may be more practical for the server to buffer individual
 * notifications from its beans and then return them in bulk on request.
 * This class contains the notifications returned by such a request.
 * </p>
 * <p>
 * It consists of a series of {@link Notification} and identifier pairs,
 * wrapped in a {@link TargetedNotification} object.  The identifiers
 * serve to pair up the notification with the listener that requested
 * it.  Two positive numbers are also included: the first sequence number
 * used by the returned notifications, and the sequence number of the
 * notification which will be returned by the next query.  The first
 * sequence number may be greater than the next sequence number if some
 * notifications have been lost.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class NotificationResult
  implements Serializable
{

  /**
   * Compatible with JDK 1.6
   */
  private static final long serialVersionUID = 1191800228721395279L;

  /**
   * The sequence number of the first notification.
   */
  private long earliestSequenceNumber;

  /**
   * The sequence number of the next notification to be
   * returned by a future query.
   */
  private long nextSequenceNumber;

  /**
   * The pairs of notifications and identifiers returned
   * by the query.
   */
  private TargetedNotification[] targetedNotifications;

  /**
   * Constructs a new {@link NotificationResult} using the specified
   * sequence numbers and the supplied array of notification pairs.
   *
   * @param startSeqNumber the sequence number of the first notification
   *                       being returned.
   * @param nextSeqNumber the sequence numbr of the next notification
   *                      that will be returned from a future query.
   * @param notifications the notification and identifier pairs.  This
   *                      may be empty.
   * @throws IllegalArgumentException if a sequence number is negative
   *                                  or <code>notifications</code> is
   *                                  <code>null</code>.
   */
  public NotificationResult(long startSeqNumber, long nextSeqNumber,
			    TargetedNotification[] notifications)
  {
    if (startSeqNumber < 0)
      throw new IllegalArgumentException("Starting sequence number is " +
					 "less than 0.");
    if (nextSeqNumber < 0)
      throw new IllegalArgumentException("Next sequence number is " +
					 "less than 0.");
    if (notifications == null)
      throw new IllegalArgumentException("The array of notifications is null.");
    earliestSequenceNumber = startSeqNumber;
    nextSequenceNumber = nextSeqNumber;
    targetedNotifications = notifications;
  }

  /**
   * Returns the sequence number of the earliest notification
   * in the buffer.
   *
   * @return the sequence number of the earliest notification.
   */
  public long getEarliestSequenceNumber()
  {
    return earliestSequenceNumber;
  }

  /**
   * Returns the sequence number of the next notification to
   * be returned by a future query.
   *
   * @return the sequence number of the next notification.
   */
  public long getNextSequenceNumber()
  {
    return nextSequenceNumber;
  }

  /**
   * Returns the notification and identifier pairs returned
   * by the query.
   *
   * @return the notification and identifier pairs.
   */
  public TargetedNotification[] getTargetedNotifications()
  {
    return targetedNotifications;
  }

  /**
   * Returns a textual representation of the object.
   *
   * @return a textual representation.
   */
  public String toString()
  {
    return getClass().getName() +
      "[earliestSequenceNumber=" + earliestSequenceNumber +
      ",nextSequenceNumber=" + nextSequenceNumber + 
      ",targetedNotifications=" + targetedNotifications +
      "]";
  }

}
