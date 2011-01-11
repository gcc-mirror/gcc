/* SSLServerSocketImpl.java --
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

import javax.net.ssl.SSLServerSocket;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SSLServerSocketImpl extends SSLServerSocket
{
  private final SSLContextImpl contextImpl;

  private boolean enableSessionCreation;
  private String[] enabledCipherSuites;
  private String[] enabledProtocols;
  private boolean needClientAuth;
  private boolean wantClientAuth;
  private boolean clientMode;

  public SSLServerSocketImpl(SSLContextImpl contextImpl) throws IOException
  {
    super();
    this.contextImpl = contextImpl;
    enableSessionCreation = true;
    enabledCipherSuites = SSLEngineImpl.defaultSuites();
    enabledProtocols = new String[] { ProtocolVersion.SSL_3.toString(),
                                      ProtocolVersion.TLS_1.toString(),
                                      ProtocolVersion.TLS_1_1.toString() };
    needClientAuth = false;
    wantClientAuth = false;
    clientMode = false;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getEnableSessionCreation()
   */
  @Override public boolean getEnableSessionCreation()
  {
    return enableSessionCreation;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getEnabledCipherSuites()
   */
  @Override public String[] getEnabledCipherSuites()
  {
    return (String[]) enabledCipherSuites.clone();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getEnabledProtocols()
   */
  @Override public String[] getEnabledProtocols()
  {
    return (String[]) enabledProtocols.clone();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getNeedClientAuth()
   */
  @Override public boolean getNeedClientAuth()
  {
    return needClientAuth;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getSupportedCipherSuites()
   */
  @Override public String[] getSupportedCipherSuites()
  {
    return CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getSupportedProtocols()
   */
  @Override public String[] getSupportedProtocols()
  {
    return new String[] { ProtocolVersion.SSL_3.toString(),
                          ProtocolVersion.TLS_1.toString(),
                          ProtocolVersion.TLS_1_1.toString() };
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getUseClientMode()
   */
  @Override public boolean getUseClientMode()
  {
    return clientMode;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#getWantClientAuth()
   */
  @Override public boolean getWantClientAuth()
  {
    return wantClientAuth;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setEnableSessionCreation(boolean)
   */
  @Override public void setEnableSessionCreation(final boolean enabled)
  {
    enableSessionCreation = enabled;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setEnabledCipherSuites(java.lang.String[])
   */
  @Override public void setEnabledCipherSuites(final String[] suites)
  {
    enabledCipherSuites = (String[]) suites.clone();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setEnabledProtocols(java.lang.String[])
   */
  @Override public void setEnabledProtocols(final String[] protocols)
  {
    enabledProtocols = (String[]) protocols.clone();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setNeedClientAuth(boolean)
   */
  @Override public void setNeedClientAuth(final boolean needAuth)
  {
    needClientAuth = needAuth;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setUseClientMode(boolean)
   */
  @Override public void setUseClientMode(final boolean clientMode)
  {
    this.clientMode = clientMode;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLServerSocket#setWantClientAuth(boolean)
   */
  @Override public void setWantClientAuth(final boolean wantAuth)
  {
    wantClientAuth = wantAuth;
  }

  @Override public SSLSocketImpl accept() throws IOException
  {
    SSLSocketImpl socketImpl = new SSLSocketImpl(contextImpl, null, -1);
    implAccept(socketImpl);
    socketImpl.setEnableSessionCreation(enableSessionCreation);
    socketImpl.setEnabledCipherSuites(enabledCipherSuites);
    socketImpl.setEnabledProtocols(enabledProtocols);
    socketImpl.setNeedClientAuth(needClientAuth);
    socketImpl.setUseClientMode(clientMode);
    socketImpl.setWantClientAuth(wantClientAuth);
    return socketImpl;
  }
}
