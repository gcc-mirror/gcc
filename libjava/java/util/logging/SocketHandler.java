/* SocketHandler.java -- a class for publishing log messages to network sockets
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
exception statement from your version. */


package java.util.logging;


/**
 * A <code>SocketHandler</code> publishes log records to
 * a TCP/IP socket.
 *
 * <p><strong>Configuration:</strong> Values of the subsequent
 * <code>LogManager</code> properties are taken into consideration
 * when a <code>SocketHandler</code> is initialized.
 * If a property is not defined, or if it has an invalid
 * value, a default is taken without an exception being thrown.
 *
 * <ul>
 *
 * <li><code>java.util.SocketHandler.level</code> - specifies
 *     the initial severity level threshold. Default value:
 *     <code>Level.ALL</code>.</li>
 *
 * <li><code>java.util.SocketHandler.filter</code> - specifies
 *     the name of a Filter class. Default value: No Filter.</li>
 *
 * <li><code>java.util.SocketHandler.formatter</code> - specifies
 *     the name of a Formatter class. Default value:
 *     <code>java.util.logging.XMLFormatter</code>.</li>
 *
 * <li><code>java.util.SocketHandler.encoding</code> - specifies
 *     the name of the character encoding. Default value:
 *     the default platform encoding.</li>
 *
 * <li><code>java.util.SocketHandler.host</code> - specifies
 *     the name of the host to which records are published.
 *     There is no default value for this property; if it is
 *     not set, the SocketHandler constructor will throw
 *     an exception.</li>
 *
 * <li><code>java.util.SocketHandler.port</code> - specifies
 *     the TCP/IP port to which records are published.
 *     There is no default value for this property; if it is
 *     not set, the SocketHandler constructor will throw
 *     an exception.</li>
 *
 * </ul>
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class SocketHandler
  extends StreamHandler
{
  /**
   * Constructs a <code>SocketHandler</code> that publishes log
   * records to a TCP/IP socket.  Tthe initial configuration is
   * determined by the <code>LogManager</code> properties described
   * above.
   *
   * @throws java.io.IOException if the connection to the specified
   *         network host and port cannot be established.
   *
   * @throws java.lang.IllegalArgumentException if either the
   *         <code>java.util.logging.SocketHandler.host</code>
   *         or <code>java.util.logging.SocketHandler.port</code>
   *         LogManager properties is not defined, or specifies
   *         an invalid value.
   */
  public SocketHandler()
    throws java.io.IOException
  {
    this(LogManager.getLogManager().getProperty("java.util.logging.SocketHandler.host"),
	 getPortNumber());
  }

    
  /**
   * Constructs a <code>SocketHandler</code> that publishes log
   * records to a TCP/IP socket.  With the exception of the internet
   * host and port, the initial configuration is determined by the
   * <code>LogManager</code> properties described above.
   *
   * @param host the Internet host to which log records will be
   *        forwarded.
   *
   * @param port the port at the host which will accept a request
   *        for a TCP/IP connection.
   *
   * @throws java.io.IOException if the connection to the specified
   *         network host and port cannot be established.
   *
   * @throws java.lang.IllegalArgumentException if either
   *         <code>host</code> or <code>port</code> specify
   *         an invalid value.
   */
  public SocketHandler(String host, int port)
    throws java.io.IOException
  {
    super(createSocket(host, port),
	  "java.util.logging.SocketHandler",
	  /* default level */ Level.ALL,
	  /* formatter */ null,
	  /* default formatter */ XMLFormatter.class);
  }


  /**
   * Retrieves the port number from the java.util.logging.SocketHandler.port
   * LogManager property.
   *
   * @throws IllegalArgumentException if the property is not defined or
   *         does not specify an integer value.
   */
  private static int getPortNumber()
  {
    try {
      return Integer.parseInt(LogManager.getLogManager().getProperty("java.util.logging.SocketHandler.port"));
    } catch (Exception ex) {
      throw new IllegalArgumentException();
    }
  }


  /**
   * Creates an OutputStream for publishing log records to an Internet
   * host and port.  This private method is a helper for use by the
   * constructor of SocketHandler.
   *
   * @param host the Internet host to which log records will be
   *        forwarded.
   *
   * @param port the port at the host which will accept a request
   *        for a TCP/IP connection.
   *
   * @throws java.io.IOException if the connection to the specified
   *         network host and port cannot be established.
   *
   * @throws java.lang.IllegalArgumentException if either
   *         <code>host</code> or <code>port</code> specify
   *         an invalid value.
   */
  private static java.io.OutputStream createSocket(String host, int port)
    throws java.io.IOException, java.lang.IllegalArgumentException
  {
    java.net.Socket  socket;

    if ((host == null) || (port < 1))
      throw new IllegalArgumentException();

    socket = new java.net.Socket(host, port);

    socket.shutdownInput();

    /* The architecture of the logging framework provides replaceable
     * formatters.  Because these formatters perform their task by
     * returning one single String for each LogRecord to be formatted,
     * there is no need to buffer.
     */
    socket.setTcpNoDelay(true);

    return socket.getOutputStream();
  }


  /**
   * Publishes a <code>LogRecord</code> to the network socket,
   * provided the record passes all tests for being loggable.
   * In addition, all data that may have been buffered will
   * be forced to the network stream.
   *
   * <p>Most applications do not need to call this method directly.
   * Instead, they will use a {@link Logger} instance, which will
   * create LogRecords and distribute them to registered handlers.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>SocketHandler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * @param record the log event to be published.
   */
  public void publish(LogRecord record)
  {
    super.publish(record);
    flush();
  }
}
    
