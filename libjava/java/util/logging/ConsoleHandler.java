/* ConsoleHandler.java
   -- a class for publishing log messages to System.err

Copyright (C) 2002 Free Software Foundation, Inc.

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
exception statement from your version.

*/


package java.util.logging;

/**
 * A <code>ConsoleHandler</code> publishes log records to
 * <code>System.err</code>.
 *
 * <p><strong>Configuration:</strong> Values of the subsequent
 * <code>LogManager</code> properties are taken into consideration
 * when a <code>ConsoleHandler</code> is initialized.
 * If a property is not defined, or if it has an invalid
 * value, a default is taken without an exception being thrown.
 *
 * <ul>
 *
 * <li><code>java.util.logging.ConsoleHandler.level</code> - specifies
 *     the initial severity level threshold. Default value:
 *     <code>Level.INFO</code>.</li>
 *
 * <li><code>java.util.logging.ConsoleHandler.filter</code> - specifies
 *     the name of a Filter class. Default value: No Filter.</li>
 *
 * <li><code>java.util.logging.ConsoleHandler.formatter</code> - specifies
 *     the name of a Formatter class. Default value:
 *     <code>java.util.logging.SimpleFormatter</code>.</li>
 *
 * <li><code>java.util.logging.ConsoleHandler.encoding</code> - specifies
 *     the name of the character encoding. Default value:
 *     the default platform encoding.
 *
 * </ul>
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class ConsoleHandler
  extends StreamHandler
{
  /**
   * Constructs a <code>StreamHandler</code> that publishes
   * log records to <code>System.err</code>.  The initial
   * configuration is determined by the <code>LogManager</code>
   * properties described above.
   */
  public ConsoleHandler()
  {
    super(System.err, "java.util.logging.ConsoleHandler", Level.INFO,
	 /* formatter */ null, SimpleFormatter.class);
  }


  /**
   * Forces any data that may have been buffered to the underlying
   * output device, but does <i>not</i> close <code>System.err</code>.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>ConsoleHandler</code> will be informed, but the caller
   * of this method will not receive an exception.
   */
  public void close()
  {
    flush();
  }


  /**
   * Publishes a <code>LogRecord</code> to the console, provided the
   * record passes all tests for being loggable.
   *
   * <p>Most applications do not need to call this method directly.
   * Instead, they will use use a <code>Logger</code>, which will
   * create LogRecords and distribute them to registered handlers.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>SocketHandler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * <p>The GNU implementation of <code>ConsoleHandler.publish</code>
   * calls flush() for every request to publish a record, so
   * they appear immediately on the console.
   *
   * @param record the log event to be published.
   */
  public void publish(LogRecord record)
  {
    super.publish(record);
    flush();
  }
}
