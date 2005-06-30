/* SimpleFormatter.java --
   A class for formatting log records into short human-readable messages
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package java.util.logging;

import java.text.DateFormat;
import java.util.Date;

/**
 * A <code>SimpleFormatter</code> formats log records into
 * short human-readable messages, typically one or two lines.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class SimpleFormatter
  extends Formatter
{
  /**
   * Constructs a SimpleFormatter.
   */
  public SimpleFormatter()
  {
  }


  /**
   * An instance of a DateFormatter that is used for formatting
   * the time of a log record into a human-readable string,
   * according to the rules of the current locale.  The value
   * is set after the first invocation of format, since it is
   * common that a JVM will instantiate a SimpleFormatter without
   * ever using it.
   */
  private DateFormat dateFormat;

  /**
   * The character sequence that is used to separate lines in the
   * generated stream. Somewhat surprisingly, the Sun J2SE 1.4
   * reference implementation always uses UNIX line endings, even on
   * platforms that have different line ending conventions (i.e.,
   * DOS). The GNU implementation does not replicate this bug.
   *
   * @see Sun bug parade, bug #4462871,
   * "java.util.logging.SimpleFormatter uses hard-coded line separator".
   */
  static final String lineSep = System.getProperty("line.separator");


  /**
   * Formats a log record into a String.
   *
   * @param the log record to be formatted.
   *
   * @return a short human-readable message, typically one or two
   *   lines.  Lines are separated using the default platform line
   *   separator.
   *
   * @throws NullPointerException if <code>record</code>
   *         is <code>null</code>.
   */
  public String format(LogRecord record)
  {
    StringBuffer buf = new StringBuffer(180);

    if (dateFormat == null)
      dateFormat = DateFormat.getDateTimeInstance();

    buf.append(dateFormat.format(new Date(record.getMillis())));
    buf.append(' ');
    buf.append(record.getSourceClassName());
    buf.append(' ');
    buf.append(record.getSourceMethodName());
    buf.append(lineSep);

    buf.append(record.getLevel());
    buf.append(": ");
    buf.append(formatMessage(record));

    buf.append(lineSep);

    return buf.toString();
  }
}
