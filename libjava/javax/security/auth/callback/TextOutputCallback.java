/* TextOutputCallback.java -- callback for text output.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package javax.security.auth.callback;

import java.io.Serializable;

/**
 * <p>Underlying security services instantiate and pass a
 * <code>TextOutputCallback</code> to the <code>handle()</code> method of a
 * {@link CallbackHandler} to display information messages, warning messages and
 * error messages.</p>
 *
 * @see CallbackHandler
 */
public class TextOutputCallback implements Callback, Serializable
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /** Information message */
  public static final int INFORMATION = 0;

  /** Warning message */
  public static final int WARNING = 1;

  /** Error message */
  public static final int ERROR = 2;

  /**
   * @serial
   * @since 1.4
   */
  private int messageType;

  /**
   * @serial
   * @since 1.4
   */
  private String message;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * <p>Construct a <code>TextOutputCallback</code> with a message type and
   * message to be displayed.</p>
   *
   * @param messageType the message type (INFORMATION, WARNING or ERROR).
   * @param message the message to be displayed.
   * @throws IllegalArgumentException if <code>messageType</code> is not either
   * <code>INFORMATION</code>, <code>WARNING</code> or <code>ERROR</code>, if
   * <code>message</code> is <code>null</code>, or if <code>message</code> has
   * a length of <code>0</code>.
   */
  public TextOutputCallback(int messageType, String message)
    throws IllegalArgumentException
  {
    switch (messageType)
      {
      case INFORMATION:
      case WARNING:
      case ERROR: this.messageType = messageType; break;
      default: throw new IllegalArgumentException("invalid message type");
      }

    setMessage(message);
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  /**
   * <p>Returns the message's <code>messageType</code>.</p>
   *
   * @return the message type (INFORMATION, WARNING or ERROR).
   */
  public int getMessageType()
  {
    return messageType;
  }

  /**
   * <p>Returns the <code>message</code> to be displayed.</p>
   *
   * @return the message to be displayed.
   */
  public String getMessage()
  {
    return message;
  }

  private void setMessage(String message) throws IllegalArgumentException
  {
    if ((message == null) || (message.length() == 0))
      {
	throw new IllegalArgumentException("invalid message");
      }
    this.message = message;
  }
}
