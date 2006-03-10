/* SSLServerSocket.java -- SSL server socket.
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

import java.security.SecureRandom;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.net.ssl.X509KeyManager;
import javax.net.ssl.X509TrustManager;

import gnu.javax.net.ssl.SRPTrustManager;

class SSLServerSocket extends javax.net.ssl.SSLServerSocket
{

  // Fields.
  // -------------------------------------------------------------------------

  private SessionContext sessions;
  private SortedSet enabledProtocols = new TreeSet(SSLSocket.supportedProtocols);
  private List enabledSuites = new ArrayList(SSLSocket.supportedSuites);
  private boolean clientMode = false;
  private boolean needClientAuth = false;
  private boolean wantClientAuth = false;
  private boolean createSessions = true;
  private SRPTrustManager srpTrustManager;
  private X509TrustManager trustManager;
  private X509KeyManager keyManager;
  private SecureRandom random;

  // Constructors.
  // -------------------------------------------------------------------------

  SSLServerSocket() throws IOException
  {
    super();
  }

  SSLServerSocket(int port) throws IOException
  {
    super(port);
  }

  SSLServerSocket(int port, int backlog) throws IOException
  {
    super(port, backlog);
  }

  SSLServerSocket(int port, int backlog, InetAddress address)
    throws IOException
  {
    super(port, backlog, address);
  }

  // SSL methods.
  // -------------------------------------------------------------------------

  public String[] getSupportedCipherSuites()
  {
    return (String[]) CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  public String[] getEnabledCipherSuites()
  {
    synchronized (enabledSuites)
      {
        String[] s = new String[enabledSuites.size()];
        int i = 0;
        for (Iterator it = enabledSuites.iterator(); it.hasNext(); )
          s[i++] = it.next().toString();
        return s;
      }
  }

  public void setEnabledCipherSuites(String[] suites)
  {
    if (suites == null || suites.length == 0)
      throw new IllegalArgumentException();
    for (int i = 0; i < suites.length; i++)
      if (CipherSuite.forName(suites[i]) == null)
        throw new IllegalArgumentException("unsupported suite: " +
                                           suites[i]);
    synchronized (enabledSuites)
      {
        enabledSuites.clear();
        for (int i = 0; i < suites.length; i++)
          {
            CipherSuite suite = CipherSuite.forName(suites[i]);
            if (!enabledSuites.contains(suite))
              enabledSuites.add(suite);
          }
      }
  }

  public String[] getSupportedProtocols()
  {
    return new String[] { "SSLv3", "TLSv1", "TLSv1.1" };
  }

  public String[] getEnabledProtocols()
  {
    synchronized (enabledProtocols)
      {
        String[] s = new String[enabledProtocols.size()];
        int i = 0;
        for (Iterator it = enabledProtocols.iterator(); it.hasNext(); )
          s[i++] = it.next().toString();
        return s;
      }
  }

  public void setEnabledProtocols(String[] protocols)
  {
    if (protocols == null || protocols.length == 0)
      throw new IllegalArgumentException();
    for (int i = 0; i < protocols.length; i++)
      {
        if (!(protocols[i].equalsIgnoreCase("SSLv3") ||
              protocols[i].equalsIgnoreCase("TLSv1") ||
              protocols[i].equalsIgnoreCase("TLSv1.1")))
          {
            throw new
              IllegalArgumentException("unsupported protocol: " +
                                       protocols[i]);
          }
      }
    synchronized (enabledProtocols)
      {
        enabledProtocols.clear();
        for (int i = 0; i < protocols.length; i++)
          {
            if (protocols[i].equalsIgnoreCase("SSLv3"))
              enabledProtocols.add(ProtocolVersion.SSL_3);
            else if (protocols[i].equalsIgnoreCase("TLSv1"))
              enabledProtocols.add(ProtocolVersion.TLS_1);
            else
              enabledProtocols.add(ProtocolVersion.TLS_1_1);
          }
      }
  }

  public void setUseClientMode(boolean clientMode)
  {
    this.clientMode = clientMode;
  }

  public boolean getUseClientMode()
  {
    return clientMode;
  }

  public void setNeedClientAuth(boolean needClientAuth)
  {
    this.needClientAuth = needClientAuth;
  }

  public boolean getNeedClientAuth()
  {
    return needClientAuth;
  }

  public void setWantClientAuth(boolean wantClientAuth)
  {
    this.wantClientAuth = wantClientAuth;
  }

  public boolean getWantClientAuth()
  {
    return wantClientAuth;
  }

  // I misspelled this method in javax.net.SSLServerSocket, and that version
  // made it into kaffe 1.1.4.
  public void setEnabledSessionCreation(boolean createSessions)
  {
    setEnableSessionCreation(createSessions);
  }

  public void setEnableSessionCreation(boolean createSessions)
  {
    this.createSessions = createSessions;
  }

  public boolean getEnableSessionCreation()
  {
    return createSessions;
  }

  // Socket methods.
  // -------------------------------------------------------------------------

  public Socket accept() throws IOException
  {
    SSLSocket socket = new SSLSocket();
    implAccept(socket);
    socket.setUseClientMode(clientMode);
    socket.setNeedClientAuth(needClientAuth);
    socket.setWantClientAuth(wantClientAuth);
    socket.setEnableSessionCreation(createSessions);
    socket.setSessionContext(sessions);
    socket.setEnabledCipherSuites(new ArrayList(enabledSuites));
    socket.setEnabledProtocols(new TreeSet(enabledProtocols));
    socket.setSRPTrustManager(srpTrustManager);
    socket.setTrustManager(trustManager);
    socket.setKeyManager(keyManager);
    socket.setRandom(random);
    return socket;
  }

  // Package methods.
  // -------------------------------------------------------------------------

  void setSessionContext(SessionContext sessions)
  {
    this.sessions = sessions;
  }

  void setKeyManager(X509KeyManager keyManager)
  {
    this.keyManager = keyManager;
  }

  void setTrustManager(X509TrustManager trustManager)
  {
    this.trustManager = trustManager;
  }

  void setSRPTrustManager(SRPTrustManager srpTrustManager)
  {
    this.srpTrustManager = srpTrustManager;
  }

  void setRandom(SecureRandom random)
  {
    this.random = random;
  }
}
