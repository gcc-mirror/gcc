/* SocketFactory.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.interfaces;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * This class produces sockets for serving and submitting CORBA requests. The
 * socket factory can be set using {@link gnuOrb.setSocketFactory()} for
 * producting all sockets for that ORB. This is needed for using secure sockets,
 * for implementing the desired timeout policies, for HTTP tunnels and in some
 * other similar cases. While such functionality is provided by near all
 * existing CORBA implementations, no standard mechanism is defined.
 *
 * The socket factory may need to put additional information to the IORs of the
 * objects, released by the ORB. Because of this reason, this interface extends
 * IORInterceptorOperations.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface SocketFactory
{
  /**
   * The name of the ORB property that forces the ORB to use the socket
   * factory class, the name of that (String) is the value of this property.
   */
  final String PROPERTY = "gnu.CORBA.SocketFactory";

  /**
   * Create a server socket that should serve remote invocations on the given
   * port. The ORB may use this socket to serve either one or more objects.
   *
   * @param port the port, on that the socket should be listening for requests.
   * The port policy can be controlled by {@link gnuPortManager}.
   *
   * @throws IOException if the socket cannot be created on the given port due
   * any reasons. The ORB may try to open the socket on another port, calling
   * this method with the different parameter.
   */
  ServerSocket createServerSocket(int port)
    throws IOException;

  /**
   * Create a client socket that should send a request to the remote side. When
   * returned, the socket should be opened and ready to communicate.
   *
   * @param port the port, on that the socket should be openend. The port is
   * usually part of the internet profile.
   *
   * @throws IOException if the socket cannot be created on the given port due
   * any reasons. The ORB may try to open the socket on another port, calling
   * this method with the different parameter.
   */
  Socket createClientSocket(String host, int port)
    throws IOException;

}
