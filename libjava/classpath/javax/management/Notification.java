/* Notification.java -- A notification emitted by a bean.
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

import java.io.IOException;
import java.io.ObjectOutputStream;

import java.util.Date;
import java.util.EventObject;

/**
 * <p>
 * A notification message that may be emitted by a bean.
 * Notifications have both a message and a type, so individual
 * notifications can be grouped by type.  They also incorporate
 * sequencing, so that the recipient can order the delivered
 * messages correctly (there is no guarantee that they will
 * be delivered in order).
 * </p>
 * <p>
 * Notifications also include a reference to the source of
 * the notification.  The source bean is represented either
 * by an {@link ObjectName} or by a direct reference to the
 * bean.  The former is preferable, and notifications emitted
 * via a {@link MBeanServer} will automatically have the source
 * transformed into an {@link ObjectName}.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class Notification
  extends EventObject
{

  /**
   * Compatible with JDK 1.6
   */
  private static final long serialVersionUID = -7516092053498031989L;

  /**
   * The notification message.
   *
   * @serial the notification message.
   */
  private String message;

  /**
   * The notification's sequence number, relative to the notifications
   * emitted by the bean.
   *
   * @serial the notification sequence number.
   */
  private long sequenceNumber;

  /**
   * The source of the notification.  This is redeclared in order to
   * replace the <code>source</code> variable in {@link java.util.EventObject}
   * with a non-transient version.
   *
   * @serial the notification source.
   */
  protected Object source;

  /**
   * The time the notification was generated.
   *
   * @serial the notification timestamp.
   */
  private long timeStamp;

  /**
   * The type of notification sent.  This utilises the same style
   * as Java property and package names.  For example,
   * <code>gnu.gcj.compiler</code> may be one type of notifications.
   *
   * @serial the notification type.
   */
  private String type;

  /**
   * The user data associated with the notification.  This includes
   * any additional data which should be transmitted with the notification,
   * but can't be achieved using the {@link java.lang.String} format
   * of the <code>message</code>.
   *
   * @serial the notification user data.
   */
  private Object userData;

  /**
   * Creates a new {@link Notification} object with the specified type,
   * source and sequence number.  The timestamp is created using the
   * current date and time.
   *
   * @param type the type of the notification.
   * @param source the source of the notification.
   * @param sequenceNumber the sequence number of the notifcation.
   */
  public Notification(String type, Object source, long sequenceNumber)
  {
    this(type, source, sequenceNumber, new Date().getTime());
  }

  /**
   * Creates a new {@link Notification} object with the specified type,
   * source, sequence number and timestamp.  
   *
   * @param type the type of the notification.
   * @param source the source of the notification.
   * @param sequenceNumber the sequence number of the notifcation.
   * @param timeStamp the time the notification was emitted.
   */
  public Notification(String type, Object source, long sequenceNumber,
		      long timeStamp)
  {
    this(type, source, sequenceNumber, timeStamp, "");
  }

  /**
   * Creates a new {@link Notification} object with the specified type,
   * source, sequence number, timestamp and message.  
   *
   * @param type the type of the notification.
   * @param source the source of the notification.
   * @param sequenceNumber the sequence number of the notifcation.
   * @param timeStamp the time the notification was emitted.
   * @param message the message contained in the notification.
   */
  public Notification(String type, Object source, long sequenceNumber,
		      long timeStamp, String message)
  {
    super(source);
    this.type = type;
    this.source = source;
    this.sequenceNumber = sequenceNumber;
    this.timeStamp = timeStamp;
    this.message = message;
  }

  /**
   * Creates a new {@link Notification} object with the specified type,
   * source, sequence number and message.  The timestamp is created using
   * the current date and time.
   *
   * @param type the type of the notification.
   * @param source the source of the notification.
   * @param sequenceNumber the sequence number of the notifcation.
   * @param message the message contained in the notification.
   */
  public Notification(String type, Object source, long sequenceNumber,
		      String message)
  {
    this(type, source, sequenceNumber, new Date().getTime(), message);
  }

  /**
   * Returns the message contained in this notification.  The message
   * is in {@link java.lang.String} form, and is thus intended for
   * display to the end-user.  Data transferred as part of the notification
   * which shouldn't be displayed is included in the <code>userData</code>
   * field.
   *
   * @return the notification message.
   * @see #getUserData()
   * @see #setUserData(java.lang.Object)
   */
  public String getMessage()
  {
    return message;
  }

  /**
   * Returns the sequence number of this notification.  This
   * can be used to determine the order in which notifications
   * were emitted by the broadcasting bean.
   *
   * @return the sequence number.
   * @see #setSequenceNumber(long)
   */
  public long getSequenceNumber()
  {
    return sequenceNumber;
  }

  /**
   * Returns the date and time at which this notification was
   * emitted.
   *
   * @return the notification timestamp.
   * @see #setTimeStamp(long)
   */
  public long getTimeStamp()
  {
    return timeStamp;
  }

  /**
   * Returns the type of this notification.  Types take the same
   * form as Java package and property names.
   *
   * @return the type of the notification.
   */
  public String getType()
  {
    return type;
  }

  /**
   * Returns the additional user data associated with the notification.
   * This is used to attach additional non-textual information to the
   * notification.
   *
   * @return the user data associated with the notification.
   * @see #setUserData(java.lang.Object)
   */
  public Object getUserData()
  {
    return userData;
  }

  /**
   * Sets the sequence number to the value specified.
   *
   * @param sequenceNumber the new sequence number.
   * @see #getSequenceNumber()
   */
  public void setSequenceNumber(long sequenceNumber)
  {
    this.sequenceNumber = sequenceNumber;
  }

  /**
   * Sets the source of this notification to the value
   * specified.
   *
   * @param source the new source of the notification.
   * @see java.util.EventSource#getSource()
   */
  public void setSource(Object source)
  {
    this.source = source;
  }

  /**
   * Sets the date and time at which this notification
   * was emitted.
   *
   * @param timeStamp the new time stamp of the notification.
   * @see #getTimeStamp()
   */
  public void setTimeStamp(long timeStamp)
  {
    this.timeStamp = timeStamp;
  }

  /**
   * Sets the additional user data associated with the notification
   * to the specified value.  This is used to attach additional
   * non-textual information to the notification.
   *
   * @param userData the new user data associated with the notification.
   * @see #getUserData()
   */
  public void setUserData(Object userData)
  {
    this.userData = userData;
  }

  /**
   * A textual representation of the notification.
   * 
   * @return the notification in {@link java.lang.String} form.
   */
  public String toString()
  {
    return getClass().getName()
      + "[message=" + message 
      + ", sequenceNumber=" + sequenceNumber 
      + ", source=" + source 
      + ", timeStamp=" + timeStamp
      + ", type=" + type
      + ", userData=" + userData
      + "]";
  }

  /**
   * Serialize the {@link Notification}.
   *
   * @param out the output stream to write to.
   * @throws IOException if an I/O error occurs.
   */
  private void writeObject(ObjectOutputStream out)
    throws IOException
  {
    out.defaultWriteObject();
  }

}

