/* AttributeChangeNotification.java -- Notification for attribute changes
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

/**
 * Defines the notification used to let listeners know of
 * an attribute change.  The bean itself is responsible
 * for creating and transmitting the notification when the
 * attribute changes, by implementing
 * {@link NotificationBroadcaster}.  For example, if a
 * bean increments the integer, <code>count</code>, it
 * should send a notification with the
 * <code>attributeName</code>, <code>"count"</code>,
 * the <code>attributeType</code>, <code>"Integer"</code>
 * and the old and new values of the attribute.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class AttributeChangeNotification
  extends Notification
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 535176054565814134L;

  /**
   * The attribute type for attribute change
   * notifications.
   */
  public static final String ATTRIBUTE_CHANGE = "jmx.attribute.change";
  
  /**
   * The name of the attribute that changed.
   */
  private String attributeName;

  /**
   * The type of the attribute that changed.
   */
  private String attributeType;

  /**
   * The old value of the attribute.
   */
  private Object oldValue;

  /**
   * The new value of the attribute.
   */
  private Object newValue;

  /**
   * Constructs a new {@link AttributeChangeNotification}
   * with the specified source, sequence number, timestamp,
   * message, and the attribute name, type, old value and
   * new value.
   *
   * @param source the producer of the notification, which
   *               is usually the bean that changed the
   *               attribute.
   * @param sequenceNumber the sequence number of the
   *                       notification.
   * @param timeStamp the date and time of the notification.
   * @param msg the message content of the notification.
   * @param name the name of the attribute.
   * @param type the type of the attribute.
   * @param oldVal the old value of the attribute.
   * @param newVal the new value of the attribute.
   */
  public AttributeChangeNotification(Object source,
				     long sequenceNumber,
				     long timeStamp,
				     String msg, String name,
				     String type, Object oldVal,
				     Object newVal)
  {
    super(ATTRIBUTE_CHANGE, source, sequenceNumber,
	  timeStamp, msg);
    attributeName = name;
    attributeType = type;
    oldValue = oldVal;
    newValue = newVal;
  }

  /**
   * Returns the name of the attribute that changed.
   *
   * @return the name of the attribute.
   */
  public String getAttributeName()
  {
    return attributeName;
  }

  /**
   * Returns the type of the attribute that changed.
   *
   * @return the type of the attribute.
   */
  public String getAttributeType()
  {
    return attributeType;
  }

  /**
   * Returns the old value of the attribute.
   *
   * @return the old value.
   */
  public Object getOldValue()
  {
    return oldValue;
  }

  /**
   * Returns the new value of the attribute.
   *
   * @return the new value.
   */
  public Object getNewValue()
  {
    return newValue;
  }

}


