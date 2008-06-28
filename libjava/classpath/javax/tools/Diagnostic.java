/* Diagnostic.java --
   Copyright (C) 2008  Free Software Foundation, Inc.

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

package javax.tools;

import java.util.Locale;

/**
 * Encapsulates diagnostic information from a tool. This usually includes
 * (but is not required) a position in a source file, line and column number
 * information and a message.
 *
 * @author Roman Kennke (roman@kennke.org)
 *
 * @param <S> the type of the source object
 *
 * @since 1.6
 */
public interface Diagnostic<S>
{
  /**
   * The kind of diagnostic information.
   */
  public static enum Kind
  {
    /**
     * Indicates and error.
     */
    ERROR,

    /**
     * Indicates a warning.
     */
    WARNING,

    /**
     * Indicates a mandatory warning.
     */
    MANDATORY_WARNING,

    /**
     * Indicates a note.
     */
    NOTE,

    /**
     * Indicates something else.
     */
    OTHER
  }

  /**
   * Indicates that this diagnostic object doesn't carry position information.
   */
  public static final long NOPOS = -1L;

  /**
   * Returns the kind of this diagnostic object.
   *
   * @return the kind of this diagnostic object
   */
  Kind getKind();

  /**
   * Returns the source of this diagnostic object.
   *
   * @return the source of this diagnostic object
   */
  S getSource();

  /**
   * Returns the position in the source object. This is a zero based value,
   * or {@link # NOPOS}, indicating that this doesn't carry position
   * information.
   *
   * @return the position in the source object
   */
  long getPosition();

  /**
   * Returns the start position in the source object. This is a zero based
   * value, or {@link #NOPOS}, indicating that this doesn't carry position
   * information.
   *
   * @return the start position in the source object
   */
  long getStartPosition();

  /*
  * Returns the end position in the source object. This is a zero based
  * value, or {@link #NOPOS}, indicating that this doesn't carry position
  * information.
  *
  * @return the end position in the source object
  */
  long getEndPosition();

  /**
   * Returns the line number or {@link #NOPOS}, indicating that this doesn't
   * carry position information. This is a 1-based value indicating the line
   * in the source object.
   *
   * @return the line number
   */
  long getLineNumber();

  /**
   * Returns the column number or {@link #NOPOS}, indicating that this doesn't
   * carry position information. This is a 1-based value indicating the column
   * in the source object.
   *
   * @return the column number
   */
  long getColumnNumber();

  /**
   * Return a diagnostic code. This is implementation dependend and might
   * be <code>null</code>.
   *
   * @return a diagnostic code or <code>null</code>
   */
  String getCode();

  /**
   * Returns a localized message. This is implementation dependend. If
   * <code>locale</code> is <code>null</code> this uses the default locale.
   *
   * @param locale the locale, or <code>null</code>
   *
   * @return a localized message
   */
  String getMessage(Locale locale);
}
