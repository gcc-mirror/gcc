/* SSLSocketFactory.java -- factory for SSL sockets.
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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.SecureRandom;

import javax.net.ssl.X509TrustManager;
import javax.net.ssl.X509KeyManager;

class SSLSocketFactory extends javax.net.ssl.SSLSocketFactory
{

  // Fields.
  // -------------------------------------------------------------------------

  private final X509TrustManager trustManager;
  private final X509KeyManager keyManager;
  private final SecureRandom random;
  private final SessionContext sessionContext;

  // Constructor.
  // -------------------------------------------------------------------------

  SSLSocketFactory(X509TrustManager trustManager, X509KeyManager keyManager,
                   SecureRandom random, SessionContext sessionContext)
  {
    this.trustManager = trustManager;
    this.keyManager = keyManager;
    this.random = random;
    this.sessionContext = sessionContext;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public String[] getDefaultCipherSuites()
  {
    return (String[]) CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  public String[] getSupportedCipherSuites()
  {
    return getDefaultCipherSuites();
  }

  public Socket createSocket(Socket socket, String host, int port, boolean autoClose)
    throws IOException
  {
    return setup(new SSLSocket(socket, host, port, autoClose));
  }

  public Socket createSocket() throws IOException
  {
    return setup(new SSLSocket());
  }

  public Socket createSocket(String host, int port)
    throws IOException, UnknownHostException
  {
    return setup(new SSLSocket(host, port));
  }

  public Socket createSocket(String host, int port, InetAddress localAddr, int localPort)
    throws IOException, UnknownHostException
  {
    return setup(new SSLSocket(host, port, localAddr, localPort));
  }

  public Socket createSocket(InetAddress address, int port) throws IOException
  {
    return setup(new SSLSocket(address, port));
  }

  public Socket createSocket(InetAddress address, int port,
                             InetAddress localAddr, int localPort)
    throws IOException
  {
    return setup(new SSLSocket(address, port, localAddr, localPort));
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private SSLSocket setup(SSLSocket s)
  {
    s.setTrustManager(trustManager);
    s.setKeyManager(keyManager);
    s.setRandom(random);
    s.setSessionContext(sessionContext);
    s.setUseClientMode(true);
    return s;
  }
}
