/* ServerSocketFactory.java -- factory for server sockets.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.net;

import java.io.IOException;

import java.net.InetAddress;
import java.net.ServerSocket;

import java.security.Security;

/**
 * A factory for server sockets. The purpose of this class is to serve
 * as the superclass of server socket factories that produce server
 * sockets of a particular type, such as <i>Secure Socket Layer</i>
 * (<b>SSL</b>) server sockets.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public abstract class ServerSocketFactory
{

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Default 0-argument constructor.
   */
  protected ServerSocketFactory()
  {
    super();
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the default server socket factory. The type of factory
   * returned may depend upon the installation.
   *
   * @return The default server socket factory.
   */
  public static synchronized ServerSocketFactory getDefault()
  {
    try
      {
        String s = Security.getProperty("gnu.defaultServerSocketFactory");
        if (s != null)
          {
            Class c = Class.forName(s);
            return (ServerSocketFactory) c.newInstance();
          }
      }
    catch (Exception e)
      {
      }
    return new VanillaServerSocketFactory();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Create an unbound server socket.
   *
   * @return The new server socket.
   * @throws IOException If a networking error occurs.
   */
  public ServerSocket createServerSocket() throws IOException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Create a server socket bound to the given port.
   *
   * @param port The port to bind the server socket to.
   * @return A server socket bound to <i>port</i>.
   * @throws IOException If a networking error occurs.
   */
  public abstract ServerSocket createServerSocket(int port) throws IOException;

  public abstract ServerSocket createServerSocket(int port, int backlog) throws IOException;

  public abstract ServerSocket createServerSocket(int port, int backlog, InetAddress bindAddress) throws IOException;
}
