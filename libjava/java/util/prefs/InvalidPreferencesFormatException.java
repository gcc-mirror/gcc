/* InvalidPreferencesFormatException - indicates reading prefs from stream
   failed
   Copyright (C) 2001, 2002, 2005  Free Software Foundation, Inc.

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

package java.util.prefs;

import java.io.NotSerializableException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * Indicates reading prefs from stream failed. Thrown by the
 * <code>importPreferences()</code> method when the supplied input stream
 * could not be read because it was not in the correct XML format.
 *
 * <p>Note that although this class inherits the Serializable interface, an
 * attempt to serialize will fail with a <code>NotSerializableException</code>.
 * </p>
 *
 * @author Mark Wielaard (mark@klomp.org)
 * @see Preferences
 * @since 1.4
 * @status updated to 1.4
 */
public class InvalidPreferencesFormatException extends Exception
{
  static final long serialVersionUID = -791715184232119669L;

  /**
   * Creates a new exception with a descriptive message. The cause remains
   * uninitialized.
   *
   * @param message the message
   */
  public InvalidPreferencesFormatException(String message)
  {
    super(message);
  }

  /**
   * Creates a new exception with the given cause.
   *
   * @param cause the cause
   */
  public InvalidPreferencesFormatException(Throwable cause)
  {
    super(cause);
  }

  /**
   * Creates a new exception with a descriptive message and a cause.
   *
   * @param message the message
   * @param cause the cause
   */
  public InvalidPreferencesFormatException(String message, Throwable cause)
  {
    super(message, cause);
  }

  /**
   * This class should not be serialized.
   *
   * @param o the output stream
   */
  private void writeObject(ObjectOutputStream o) throws NotSerializableException
  {
    throw new NotSerializableException
      ("java.util.prefs.InvalidPreferencesFormatException");
  }

  /**
   * This class should not be serialized.
   *
   * @param i the input stream
   */
  private void readObject(ObjectInputStream i) throws NotSerializableException
  {
    throw new NotSerializableException
      ("java.util.prefs.InvalidPreferencesFormatException");
  }
}
