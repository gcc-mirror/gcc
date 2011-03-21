/* SSLServerSocketFactoryImpl.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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


package gnu.javax.net.ssl.provider;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;

import javax.net.ssl.SSLServerSocketFactory;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SSLServerSocketFactoryImpl extends SSLServerSocketFactory
{
  private final SSLContextImpl contextImpl;

  public SSLServerSocketFactoryImpl(SSLContextImpl contextImpl)
  {
    this.contextImpl = contextImpl;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocketFactory#getDefaultCipherSuites()
   */
  @Override public String[] getDefaultCipherSuites()
  {
    return SSLEngineImpl.defaultSuites();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocketFactory#getSupportedCipherSuites()
   */
  @Override public String[] getSupportedCipherSuites()
  {
    return CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  /* (non-Javadoc)
   * @see javax.net.ServerSocketFactory#createServerSocket(int)
   */
  @Override public SSLServerSocketImpl createServerSocket(int port)
    throws IOException
  {
    SSLServerSocketImpl socket = new SSLServerSocketImpl(contextImpl);
    socket.bind(new InetSocketAddress(port));
    return socket;
  }

  /* (non-Javadoc)
   * @see javax.net.ServerSocketFactory#createServerSocket(int, int)
   */
  @Override public SSLServerSocketImpl createServerSocket(int port, int backlog)
    throws IOException
  {
    SSLServerSocketImpl socket = new SSLServerSocketImpl(contextImpl);
    socket.bind(new InetSocketAddress(port), backlog);
    return socket;
  }

  /* (non-Javadoc)
   * @see javax.net.ServerSocketFactory#createServerSocket(int, int, java.net.InetAddress)
   */
  @Override public SSLServerSocketImpl createServerSocket(int port, int backlog,
                                                          InetAddress bindAddress)
    throws IOException
  {
    SSLServerSocketImpl socket = new SSLServerSocketImpl(contextImpl);
    socket.bind(new InetSocketAddress(bindAddress, port), backlog);
    return socket;
  }
}
