/* SocketTransport.java -- a socket transport
   Copyright (C) 2005, 2007 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.transport;

import gnu.classpath.jdwp.transport.ITransport;
import gnu.classpath.jdwp.transport.TransportException;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;

import javax.net.ServerSocketFactory;
import javax.net.SocketFactory;

/**
 * A socket-based transport. This transport uses
 * configury string that looks like "name=dt_socket,
 * address=localhost:1234,server=y".
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
class SocketTransport
  implements ITransport
{
  /**
   * Name of this transport
   */
  public static final String NAME = "dt_socket";

  // Configure properties
  private static final String _PROPERTY_ADDRESS = "address";
  private static final String _PROPERTY_SERVER = "server";

  // Port number
  private int _port;

  // Host name
  private String _host;

  // Are we acting as a server?
  private boolean _server = false;

  // Socket
  private Socket _socket;

  /**
   * Setup the connection configuration from the given properties
   *
   * @param  properties  the properties of the JDWP session
   * @throws TransportException for any configury errors
   */
  public void configure(HashMap properties)
    throws TransportException
  {
    // Get server [form: "y" or "n"]
    String p = (String) properties.get(_PROPERTY_SERVER);
    if (p != null)
      {
        if (p.toLowerCase().equals("y"))
          _server = true;
      }

    // Get address [form: "hostname:port"]
    p = (String) properties.get(_PROPERTY_ADDRESS);
    if (p != null)
      {
        String[] s = p.split(":");
        if (s.length == 1)
          {
            // Port number only. Assume "localhost"
            _port = Integer.parseInt(s[0]);
            _host = "localhost";
          }
        else
          {
            if (s[0].length() == 0)
              _host = "localhost";
            else
              _host = s[0];
            _port = Integer.parseInt(s[1]);
          }
      }
  }

  /**
   * Initialize this socket connection. This includes
   * connecting to the host (or listening for it).
   *
   * @throws TransportException if a transport-related error occurs
   */
  public void initialize ()
    throws TransportException
  {
    try
      {
        if (_server)
          {
            // Get a server socket
            ServerSocketFactory ssf = ServerSocketFactory.getDefault ();
            ServerSocket ss = ssf.createServerSocket (_port, 1);
            _socket = ss.accept ();
          }
        else
          {
            // Get a client socket (the factory will connect it)
            SocketFactory sf = SocketFactory.getDefault ();
            _socket = sf.createSocket (_host, _port);
          }
      }
    catch (IOException ioe)
      {
        // This will grab UnknownHostException, too.
        throw new TransportException (ioe);
      }
  }

  /**
   * Shutdown the socket. This could cause SocketExceptions
   * for anyone blocked on socket i/o
   */
  public void shutdown ()
  {
    try
      {
        _socket.close ();
      }
    catch (Throwable t)
      {
        // We don't really care about errors at this point
      }
  }

  /**
   * Returns an <code>InputStream</code> for the transport
   *
   * @throws IOException if an I/O error occurs creating the stream
   *                     or the socket is not connected
   */
  public InputStream getInputStream ()
    throws IOException
  {
    return _socket.getInputStream ();
  }

  /**
   * Returns an <code>OutputStream</code> for the transport
   *
   * @throws IOException if an I/O error occurs creating the stream
   *                     or the socket is not connected
   */
  public OutputStream getOutputStream ()
    throws IOException
  {
    return _socket.getOutputStream ();
  }
}
