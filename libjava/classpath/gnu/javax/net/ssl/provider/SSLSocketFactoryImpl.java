/* SSLSocketFactoryImpl.java -- 
   Copyright (C) 2006, 2007  Free Software Foundation, Inc.

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
import java.net.Socket;
import java.net.UnknownHostException;

import javax.net.ssl.SSLSocketFactory;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SSLSocketFactoryImpl extends SSLSocketFactory
{
  /**
   * The SSLContextImpl that created us.
   */
  private final SSLContextImpl contextImpl;
  
  public SSLSocketFactoryImpl(SSLContextImpl contextImpl)
  {
    this.contextImpl = contextImpl;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocketFactory#createSocket(java.net.Socket, java.lang.String, int, boolean)
   */
  @Override public Socket createSocket(Socket socket, String host, int port,
                                       boolean autoClose)
    throws IOException
  {
    return new SSLSocketImpl(contextImpl, host, port, socket, autoClose);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocketFactory#getDefaultCipherSuites()
   */
  @Override public String[] getDefaultCipherSuites()
  {
    return SSLEngineImpl.defaultSuites();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocketFactory#getSupportedCipherSuites()
   */
  @Override public String[] getSupportedCipherSuites()
  {
    return CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  /* (non-Javadoc)
   * @see javax.net.SocketFactory#createSocket(java.lang.String, int)
   */
  @Override public SSLSocketImpl createSocket(String host, int port)
    throws IOException, UnknownHostException
  {
    return createSocket(host, port, null, 0);
  }

  /* (non-Javadoc)
   * @see javax.net.SocketFactory#createSocket(java.lang.String, int, java.net.InetAddress, int)
   */
  @Override public SSLSocketImpl createSocket(String host, int port,
                                              InetAddress localHost, int localPort)
    throws IOException, UnknownHostException
  {
    SSLSocketImpl socket = new SSLSocketImpl(contextImpl, host, port);
    InetSocketAddress endpoint = new InetSocketAddress(host, port);
    socket.bind(new InetSocketAddress(localHost, localPort));
    socket.connect(endpoint);
    return socket;
  }

  /* (non-Javadoc)
   * @see javax.net.SocketFactory#createSocket(java.net.InetAddress, int)
   */
  @Override public SSLSocketImpl createSocket(InetAddress host, int port)
    throws IOException
  {
    return createSocket(host, port, null, 0);
  }

  /* (non-Javadoc)
   * @see javax.net.SocketFactory#createSocket(java.net.InetAddress, int, java.net.InetAddress, int)
   */
  @Override public SSLSocketImpl createSocket(InetAddress host, int port,
                                              InetAddress localHost, int localPort)
    throws IOException
  {
    SSLSocketImpl socket = new SSLSocketImpl(contextImpl,
                                             host.getCanonicalHostName(), port);
    socket.bind(new InetSocketAddress(localHost, localPort));
    socket.connect(new InetSocketAddress(host, port));
    return socket;
  }

  /* (non-Javadoc)
   * @see javax.net.SocketFactory#createSocket()
   */
  @Override public Socket createSocket() throws IOException
  {
    return new SSLSocketImpl(contextImpl, null, -1, new Socket(), true);
  }
}
