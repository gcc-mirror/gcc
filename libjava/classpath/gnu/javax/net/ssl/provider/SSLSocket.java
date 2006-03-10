/* SSLSocket.java -- the SSL socket class.
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

import java.io.BufferedOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

import java.math.BigInteger;

import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;

import java.nio.channels.SocketChannel;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Security;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import javax.net.ssl.HandshakeCompletedEvent;
import javax.net.ssl.HandshakeCompletedListener;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLProtocolException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.X509KeyManager;
import javax.net.ssl.X509TrustManager;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextInputCallback;

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;

import gnu.java.security.Registry;
import gnu.javax.security.auth.callback.DefaultCallbackHandler;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.javax.crypto.key.IKeyAgreementParty;
import gnu.javax.crypto.key.KeyAgreementFactory;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.OutgoingMessage;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.dh.DiffieHellmanKeyAgreement;
import gnu.javax.crypto.key.dh.ElGamalKeyAgreement;
import gnu.javax.crypto.key.dh.GnuDHPrivateKey;
import gnu.javax.crypto.key.dh.GnuDHPublicKey;
import gnu.javax.crypto.key.srp6.SRPPrivateKey;
import gnu.javax.crypto.key.srp6.SRPPublicKey;
import gnu.javax.crypto.key.srp6.SRP6KeyAgreement;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.prng.ARCFour;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.sasl.srp.SRPAuthInfoProvider;
import gnu.javax.crypto.sasl.srp.SRPRegistry;
import gnu.java.security.sig.ISignature;
import gnu.java.security.sig.SignatureFactory;
import gnu.java.security.sig.dss.DSSSignature;
import gnu.java.security.sig.rsa.EME_PKCS1_V1_5;
import gnu.java.security.sig.rsa.RSA;

import gnu.javax.net.ssl.SRPTrustManager;

/**
 * This is the core of the Jessie SSL implementation; it implements the {@link
 * javax.net.ssl.SSLSocket} for normal and "wrapped" sockets, and handles all
 * protocols implemented by this library.
 */
final class SSLSocket extends javax.net.ssl.SSLSocket
{

  // This class is almost unbearably large and complex, but is laid out
  // as follows:
  //
  // 1. Fields.
  // 2. Constructors.
  // 3. SSLSocket methods. These are the public methods defined in
  //    javax.net.ssl.SSLSocket.
  // 4. Socket methods. These override the public methods of java.net.Socket,
  //    and delegate the method call to either the underlying socket if this is
  //    a wrapped socket, or to the superclass.
  // 5. Package-private methods that various pieces of Jessie use.
  // 6. Private methods. These compose the SSL handshake.
  //
  // Each part is preceeded by a form feed.

// Constants and fields.
  // -------------------------------------------------------------------------

  // Debuggery.
  private static final boolean DEBUG_HANDSHAKE_LAYER = true;
  private static final boolean DEBUG_KEY_EXCHANGE = false;
  private static final Logger logger = SystemLogger.SYSTEM;

  // Fields for using this class as a wrapped socket.
  private Socket underlyingSocket;
  private int underlyingPort;
  private boolean autoClose;

  // Cryptography fields.
  SessionContext sessionContext;
  Session session;
  LinkedList handshakeListeners;
  private boolean clientMode, wantClientAuth, needClientAuth, createSessions;
  private boolean handshakeDone;

  // I/O fields.
  private String remoteHost;
  private InputStream socketIn;
  private OutputStream socketOut;
  private InputStream applicationIn;
  private OutputStream applicationOut;
  private InputStream handshakeIn;
  private OutputStream handshakeOut;
//   private ThreadGroup recordLayer;
  RecordInput recordInput;
//  RecordOutput recordOutput;
  private long handshakeTime;

  private SocketChannel channel;

  static SortedSet supportedProtocols = new TreeSet();
  static List supportedSuites = new ArrayList(30);

// Static initializer.
  // -------------------------------------------------------------------------

  static
  {
    //supportedProtocols.add(ProtocolVersion.TLS_1_1);
    supportedProtocols.add(ProtocolVersion.TLS_1);
    supportedProtocols.add(ProtocolVersion.SSL_3);

    // These are in preference order. It's my preference order, but I'm not
    // a total idiot.
    supportedSuites.add(CipherSuite.TLS_DHE_DSS_WITH_AES_256_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_RSA_WITH_AES_256_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_DSS_WITH_AES_256_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_RSA_WITH_AES_256_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_AES_256_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_DSS_WITH_AES_128_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_RSA_WITH_AES_128_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_DSS_WITH_AES_128_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_RSA_WITH_AES_128_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_AES_128_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_3DES_EDE_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_RC4_128_MD5);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_RC4_128_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_DSS_WITH_DES_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_RSA_WITH_DES_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_DSS_WITH_DES_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_RSA_WITH_DES_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_DES_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_EXPORT_WITH_DES40_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_EXPORT_WITH_RC4_40_MD5);
    supportedSuites.add(CipherSuite.TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_NULL_MD5);
    supportedSuites.add(CipherSuite.TLS_RSA_WITH_NULL_SHA);
  }

// Constructors.
  // -------------------------------------------------------------------------

  SSLSocket(Socket socket, String host, int port, boolean autoClose)
    throws IOException
  {
    underlyingSocket = socket;
    remoteHost = host;
    underlyingPort = port;
    this.autoClose = autoClose;
    initialize();
  }

  SSLSocket (Socket socket, SocketChannel channel) throws IOException
  {
    underlyingSocket = socket;
    this.channel = channel;
    initialize ();
  }

  SSLSocket() throws IOException
  {
    super();
    initialize();
  }

  SSLSocket(InetAddress addr, int port) throws IOException
  {
    super(addr, port);
    initialize();
    remoteHost = addr.getHostName();
    if (remoteHost == null)
      {
        remoteHost = addr.getHostAddress();
      }
  }

  SSLSocket(InetAddress addr, int port, InetAddress laddr, int lport)
    throws IOException
  {
    super(addr, port, laddr, lport);
    initialize();
    remoteHost = addr.getHostName();
    if (remoteHost == null)
      remoteHost = addr.getHostAddress();
  }

  SSLSocket(String host, int port) throws IOException
  {
    super(host, port);
    initialize();
    remoteHost = host;
  }

  SSLSocket(String host, int port, InetAddress laddr, int lport)
    throws IOException
  {
    super(host, port, laddr, lport);
    initialize();
    remoteHost = host;
  }

  private void initialize()
  {
    session = new Session();
    session.enabledSuites = new ArrayList(supportedSuites);
    session.enabledProtocols = new TreeSet(supportedProtocols);
    session.protocol = ProtocolVersion.TLS_1;
    session.params.setVersion (ProtocolVersion.TLS_1);
    handshakeListeners = new LinkedList();
    handshakeDone = false;
  }

// SSL methods.
  // -------------------------------------------------------------------------

  public void addHandshakeCompletedListener(HandshakeCompletedListener l)
  {
    synchronized (handshakeListeners)
      {
        if (l == null)
          throw new NullPointerException();
        if (!handshakeListeners.contains(l))
          handshakeListeners.add(l);
      }
  }

  public void removeHandshakeCompletedListener(HandshakeCompletedListener l)
  {
    synchronized (handshakeListeners)
      {
        handshakeListeners.remove(l);
      }
  }

  public String[] getEnabledProtocols()
  {
    synchronized (session.enabledProtocols)
      {
        try
          {
            return (String[]) Util.transform(session.enabledProtocols.toArray(),
                                             String.class, "toString", null);
          }
        catch (Exception x)
          {
            RuntimeException re = new RuntimeException (x.getMessage());
            re.initCause (x);
            throw re;
          }
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
    synchronized (session.enabledProtocols)
      {
        session.enabledProtocols.clear();
        for (int i = 0; i < protocols.length; i++)
          {
            if (protocols[i].equalsIgnoreCase("SSLv3"))
              {
                session.enabledProtocols.add(ProtocolVersion.SSL_3);
              }
            else if (protocols[i].equalsIgnoreCase("TLSv1"))
              {
                session.enabledProtocols.add(ProtocolVersion.TLS_1);
              }
            else
              {
                session.enabledProtocols.add(ProtocolVersion.TLS_1_1);
              }
          }
      }
  }

  public String[] getSupportedProtocols()
  {
    return new String[] { /* "TLSv1.1", */ "TLSv1", "SSLv3" };
  }

  public String[] getEnabledCipherSuites()
  {
    synchronized (session.enabledSuites)
      {
        try
          {
            return (String[]) Util.transform(session.enabledSuites.toArray(),
                                             String.class, "toString", null);
          }
        catch (Exception x)
          {
            RuntimeException re = new RuntimeException (x.getMessage());
            re.initCause (x);
            throw re;
          }
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
    synchronized (session.enabledSuites)
      {
        session.enabledSuites.clear();
        for (int i = 0; i < suites.length; i++)
          {
            CipherSuite suite = CipherSuite.forName(suites[i]);
            if (!session.enabledSuites.contains(suite))
              {
                session.enabledSuites.add(suite);
              }
          }
      }
  }

  public String[] getSupportedCipherSuites()
  {
    return (String[]) CipherSuite.availableSuiteNames().toArray(new String[52]);
  }

  public SSLSession getSession()
  {
    return session;
  }

  public boolean getEnableSessionCreation()
  {
    return createSessions;
  }

  public void setEnableSessionCreation(boolean flag)
  {
    createSessions = flag;
  }

  public boolean getNeedClientAuth()
  {
    return needClientAuth;
  }

  public void setNeedClientAuth(boolean flag)
  {
    needClientAuth = flag;
  }

  public boolean getWantClientAuth()
  {
    return wantClientAuth;
  }

  public void setWantClientAuth(boolean flag)
  {
    wantClientAuth = flag;
  }

  public boolean getUseClientMode()
  {
    return clientMode;
  }

  public void setUseClientMode(boolean flag)
  {
    this.clientMode = flag;
  }

  public synchronized void startHandshake() throws IOException
  {
    if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "startHandshake called in {0}",
                    Thread.currentThread());
        handshakeTime = System.currentTimeMillis();
      }
    if (handshakeDone)
      {
        if (clientMode)
          {
            handshakeDone = false;
            doClientHandshake();
          }
        else
          {
            Handshake req = new Handshake(Handshake.Type.HELLO_REQUEST, null);
            req.write (handshakeOut, session.protocol);
            handshakeOut.flush();
//             recordOutput.setHandshakeAvail(req.write(handshakeOut, session.protocol));
          }
        return;
      }
    if (recordInput == null)
      {
        setupIO();
      }
    if (clientMode)
      {
        doClientHandshake();
      }
    else
      {
        doServerHandshake();
      }
  }

// Socket methods.
  // -------------------------------------------------------------------------

  public InetAddress getInetAddress()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getInetAddress();
      }
    else
      {
        return super.getInetAddress();
      }
  }

  public InetAddress getLocalAddress()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getLocalAddress();
      }
    else
      {
        return super.getLocalAddress();
      }
  }

  public int getPort()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getPort();
      }
    else
      {
        return super.getPort();
      }
  }

  public int getLocalPort()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getLocalPort();
      }
    else
      {
        return super.getLocalPort();
      }
  }

  public InputStream getInputStream() throws IOException
  {
    if (applicationIn == null)
      {
        setupIO();
      }
    return applicationIn;
  }

  public OutputStream getOutputStream() throws IOException
  {
    if (applicationOut == null)
      {
        setupIO();
      }
    return applicationOut;
  }

  public void setTcpNoDelay(boolean flag) throws SocketException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.setTcpNoDelay(flag);
      }
    else
      {
        super.setTcpNoDelay(flag);
      }
  }

  public boolean getTcpNoDelay() throws SocketException
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getTcpNoDelay();
      }
    else
      {
        return super.getTcpNoDelay();
      }
  }

  public void setSoLinger(boolean flag, int linger) throws SocketException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.setSoLinger(flag, linger);
      }
    else
      {
        super.setSoLinger(flag, linger);
      }
  }

  public int getSoLinger() throws SocketException
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getSoLinger();
      }
    else
      {
        return super.getSoLinger();
      }
  }

  public void sendUrgentData(int data) throws IOException
  {
    throw new UnsupportedOperationException("not implemented");
  }

  public void setSoTimeout(int timeout) throws SocketException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.setSoTimeout(timeout);
      }
    else
      {
        super.setSoTimeout(timeout);
      }
  }

  public int getSoTimeout() throws SocketException
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getSoTimeout();
      }
    else
      {
        return super.getSoTimeout();
      }
  }

  public void setSendBufferSize(int size) throws SocketException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.setSendBufferSize(size);
      }
    else
      {
        super.setSendBufferSize(size);
      }
  }

  public int getSendBufferSize() throws SocketException
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getSendBufferSize();
      }
    else
      {
        return super.getSendBufferSize();
      }
  }

  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.setReceiveBufferSize(size);
      }
    else
      {
        super.setReceiveBufferSize(size);
      }
  }

  public int getReceiveBufferSize() throws SocketException
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getReceiveBufferSize();
      }
    else
      {
        return super.getReceiveBufferSize();
      }
  }

  public synchronized void close() throws IOException
  {
    if (recordInput == null)
      {
        if (underlyingSocket != null)
          {
            if (autoClose)
              underlyingSocket.close();
          }
        else
          super.close();
        return;
      }
//     while (recordOutput.applicationDataPending()) Thread.yield();
    Alert close = new Alert (Alert.Level.WARNING, Alert.Description.CLOSE_NOTIFY);
    sendAlert (close);
    long wait = System.currentTimeMillis() + 60000L;
    while (session.currentAlert == null && !recordInput.pollClose())
      {

        Thread.yield();
        if (wait <= System.currentTimeMillis())
          {
            break;
          }
      }
    boolean gotClose = session.currentAlert != null &&
      session.currentAlert.getDescription() == Alert.Description.CLOSE_NOTIFY;
//     recordInput.setRunning(false);
//     recordOutput.setRunning(false);
//     recordLayer.interrupt();
    recordInput = null;
//     recordOutput = null;
//     recordLayer = null;
    if (underlyingSocket != null)
      {
        if (autoClose)
          underlyingSocket.close();
      }
    else
      super.close();
    if (!gotClose)
      {
        session.invalidate();
        throw new SSLException("did not receive close notify");
      }
  }

  public String toString()
  {
    if (underlyingSocket != null)
      {
        return SSLSocket.class.getName() + " [ " + underlyingSocket + " ]";
      }
    else
      {
        return SSLSocket.class.getName() + " [ " + super.toString() + " ]";
      }
  }

  // Configuration insanity begins here.

  public void connect(SocketAddress saddr) throws IOException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.connect(saddr);
      }
    else
      {
        super.connect(saddr);
      }
  }

  public void connect(SocketAddress saddr, int timeout) throws IOException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.connect(saddr, timeout);
      }
    else
      {
        super.connect(saddr, timeout);
      }
  }

  public void bind(SocketAddress saddr) throws IOException
  {
    if (underlyingSocket != null)
      {
        underlyingSocket.bind(saddr);
      }
    else
      {
        super.bind(saddr);
      }
  }

  public SocketAddress getLocalSocketAddress()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.getLocalSocketAddress();
      }
    else
      {
        return super.getLocalSocketAddress();
      }
  }

  public SocketChannel getChannel()
  {
    return channel;
  }

  public boolean isBound()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.isBound();
      }
    else
      {
        return super.isBound();
      }
    //throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean isClosed()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.isClosed();
      }
    else
      {
        return super.isClosed();
      }
    //throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  //public SocketAddress getRemoteSocketAddress()
  //{
  //  if (underlyingSocket != null)
  //    {
  //      return underlyingSocket.getRemoteSocketAddress();
  //    }
  //  else
  //    {
  //      return super.getRemoteSocketAddress();
  //    }
  //}

  public void setOOBInline(boolean flag) throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.setOOBInline(flag);
    //  }
    //else
    //  {
    //    super.setOOBInline(flag);
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean getOOBInline() throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.getOOBInline();
    //  }
    //else
    //  {
    //    return super.getOOBInline();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public void setKeepAlive(boolean flag) throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.setKeepAlive(flag);
    //  }
    //else
    //  {
    //    super.setKeepAlive(flag);
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean getKeepAlive() throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.getKeepAlive();
    //  }
    //else
    //  {
    //    return super.getKeepAlive();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public void setTrafficClass(int clazz) throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.setTrafficClass(clazz);
    //  }
    //else
    //  {
    //    super.setTrafficClass(clazz);
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public int getTrafficClass() throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.getTrafficClass();
    //  }
    //else
    //  {
    //    return super.getTrafficClass();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public void setReuseAddress(boolean flag) throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.setReuseAddress(flag);
    //  }
    //else
    //  {
    //    super.setReuseAddress(flag);
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean getReuseAddress() throws SocketException
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.getReuseAddress();
    //  }
    //else
    //  {
    //    return super.getReuseAddress();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public void shutdownInput() throws IOException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.shutdownInput();
    //  }
    //else
    //  {
    //    super.shutdownInput();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public void shutdownOutput() throws IOException
  {
    //if (underlyingSocket != null)
    //  {
    //    underlyingSocket.shutdownOutput();
    //  }
    //else
    //  {
    //    super.shutdownOutput();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean isConnected()
  {
    if (underlyingSocket != null)
      {
        return underlyingSocket.isConnected();
      }
    else
      {
        return super.isConnected();
      }
    //throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean isInputShutdown()
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.isInputShutdown();
    //  }
    //else
    //  {
    //    return super.isInputShutdown();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  public boolean isOutputShutdown()
  {
    //if (underlyingSocket != null)
    //  {
    //    return underlyingSocket.isOutputShutdown();
    //  }
    //else
    //  {
    //    return super.isOutputShutdown();
    //  }
    throw new UnsupportedOperationException("1.4 methods not enabled");
  }

  protected void finalize()
  {
    if (session.currentAlert == null)
      {
        try
          {
            close();
          }
        catch (Exception ignore) { }
      }
  }

// Package methods.
  // -------------------------------------------------------------------------

  void setSessionContext(SessionContext sessionContext)
  {
    this.sessionContext = sessionContext;
  }

  void setEnabledCipherSuites(List suites)
  {
    session.enabledSuites = suites;
  }

  void setEnabledProtocols(SortedSet protocols)
  {
    session.enabledProtocols = protocols;
  }

  void setSRPTrustManager(SRPTrustManager srpTrustManager)
  {
    session.srpTrustManager = srpTrustManager;
  }

  void setTrustManager(X509TrustManager trustManager)
  {
    session.trustManager = trustManager;
  }

  void setKeyManager(X509KeyManager keyManager)
  {
    session.keyManager = keyManager;
  }

  void setRandom(SecureRandom random)
  {
    session.random = random;
  }

  void sendAlert (Alert alert) throws IOException
  {
    RecordOutputStream out =
      new RecordOutputStream (socketOut, ContentType.ALERT, session.params);
    out.write (alert.getEncoded ());
  }

  /**
   * Gets the most-recently-received alert message.
   *
   * @return The alert message.
   */
  Alert checkAlert()
  {
    return session.currentAlert;
  }

  synchronized void checkHandshakeDone() throws IOException
  {
    if (!handshakeDone)
      {
        startHandshake();
      }
    Alert alert = session.currentAlert;
    if (alert != null && alert.getLevel() == Alert.Level.FATAL)
      {
        throw new AlertException(alert, false);
      }
    if (handshakeIn.available() > 0 && !clientMode)
      {
        handshakeDone = false;
        startHandshake();
      }
  }

// Own methods.
  // -------------------------------------------------------------------------

  private static final byte[] SENDER_CLIENT =
    new byte[] { 0x43, 0x4C, 0x4E, 0x54 };
  private static final byte[] SENDER_SERVER =
    new byte[] { 0x53, 0x52, 0x56, 0x52 };

  private void changeCipherSpec () throws IOException
  {
    RecordOutputStream out =
      new RecordOutputStream (socketOut, ContentType.CHANGE_CIPHER_SPEC, session.params);
    out.write (1);
  }

  private void readChangeCipherSpec () throws IOException
  {
    RecordInputStream in =
      new RecordInputStream (recordInput, ContentType.CHANGE_CIPHER_SPEC);
    if (in.read() != 1)
      {
        throw new SSLProtocolException ("bad change cipher spec message");
      }
  }

  /**
   * Initializes the application data streams and starts the record layer
   * threads.
   */
  private synchronized void setupIO() throws IOException
  {
    if (recordInput != null)
      {
        return;
      }
    if (underlyingSocket != null)
      {
        socketIn = underlyingSocket.getInputStream();
        socketOut = underlyingSocket.getOutputStream();
      }
    else
      {
        socketIn = super.getInputStream();
        socketOut = super.getOutputStream();
      }
//     recordLayer = new ThreadGroup("record_layer");
//     recordInput = new RecordInput(in, session, recordLayer);
//     recordOutput = new RecordOutput(out, session, recordLayer);
//     recordInput.setRecordOutput(recordOutput);
//     recordLayer.setDaemon(true);
//     recordInput.start();
//     recordOutput.start();
    recordInput = new RecordInput (socketIn, session);
    applicationIn = new SSLSocketInputStream(
      new RecordInputStream (recordInput, ContentType.APPLICATION_DATA), this);
    applicationOut = new SSLSocketOutputStream(
      new RecordOutputStream (socketOut, ContentType.APPLICATION_DATA, session.params), this);
    handshakeIn = new SSLSocketInputStream(
      new RecordInputStream (recordInput, ContentType.HANDSHAKE), this, false);
    handshakeOut = new BufferedOutputStream (new SSLSocketOutputStream(
      new RecordOutputStream (socketOut, ContentType.HANDSHAKE, session.params), this, false), 8096);
  }

  private void handshakeCompleted ()
  {
    handshakeDone = true;
    HandshakeCompletedEvent event = new HandshakeCompletedEvent (this, session);
    for (Iterator it = handshakeListeners.iterator (); it.hasNext (); )
      {
        try
          {
            ((HandshakeCompletedListener) it.next ()).handshakeCompleted (event);
          }
        catch (Throwable t) { }
      }
    if (createSessions)
      {
        synchronized (session)
          {
            sessionContext.addSession (session.sessionId, session);
            session.access ();
          }
      }

    if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "Handshake finished in {0}",
                    Thread.currentThread());
        handshakeTime = System.currentTimeMillis() - handshakeTime;
        logger.log (Component.SSL_HANDSHAKE, "Elapsed time {0}s",
                    new Long (handshakeTime / 1000));
      }
  }

  /*
   * Perform the client handshake. The process looks like this:
   *
   *   ClientHello          -->
   *   ServerHello         <--
   *   Certificate*        <--
   *   ServerKeyExchange*  <--
   *   CertificateRequest* <--
   *   ServerHelloDone*    <--
   *   Certificate*         -->
   *   ClientKeyExchange    -->
   *   CertificateVerify*   -->
   *   [ChangeCipherSpec]   -->
   *   Finished             -->
   *   [ChangeCipherSpec]  <--
   *   Finished            <--
   *
   * With --> denoting output and <-- denoting input. * denotes optional
   * messages.
   *
   * Alternatively, this may be an abbreviated handshake if we are resuming
   * a session:
   *
   *   ClientHello          -->
   *   ServerHello         <--
   *   [ChangeCipherSpec]  <--
   *   Finished            <--
   *   [ChangeCipherSpec]   -->
   *   Finished             -->
   */
  private void doClientHandshake() throws IOException
  {
    if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "starting client handshake in {0}",
                    Thread.currentThread());
      }

    IMessageDigest md5 = HashFactory.getInstance(Registry.MD5_HASH);
    IMessageDigest sha = HashFactory.getInstance(Registry.SHA160_HASH);
    DigestInputStream din = new DigestInputStream(handshakeIn, md5, sha);
    DigestOutputStream dout = new DigestOutputStream(handshakeOut, md5, sha);
    Session continuedSession = null;
    byte[] sessionId = new byte[0];
    List extensions = null;
    String user = null;
    CertificateType certType = CertificateType.X509;

    // Look through the available sessions to see if an appropriate one is
    // available.
    for (Enumeration e = sessionContext.getIds(); e.hasMoreElements(); )
      {
        byte[] id = (byte[]) e.nextElement();
        continuedSession = (Session) sessionContext.getSession(id);
        if (continuedSession == null)
          {
            continue;
          }
        if (!session.enabledProtocols.contains(continuedSession.protocol))
          {
            continue;
          }
        if (continuedSession.getPeerHost().equals(remoteHost))
          {
            sessionId = id;
            break;
          }
      }

    // If a SRP suite is enabled, ask for a username so we can include it
    // with our extensions list.
    for (Iterator i = session.enabledSuites.iterator(); i.hasNext(); )
      {
        CipherSuite s = (CipherSuite) i.next();
        if (s.getKeyExchange() == "SRP")
          {
            extensions = new LinkedList();
            user = askUserName(remoteHost);
            byte[] b = user.getBytes("UTF-8");
            if (b.length > 255)
              {
                handshakeFailure();
                throw new SSLException("SRP username too long");
              }
            extensions.add(new Extension(Extension.Type.SRP,
              Util.concat(new byte[] { (byte) b.length }, b)));

            break;
          }
      }

    // If the jessie.fragment.length property is set, add the appropriate
    // extension to the list. The fragment length is only actually set if
    // the server responds with the same extension.
    try
      {
        int flen = Integer.parseInt(Util.getSecurityProperty("jessie.fragment.length"));
        byte[] ext = new byte[1];
        if (flen == 512)
          ext[0] = 1;
        else if (flen == 1024)
          ext[0] = 2;
        else if (flen == 2048)
          ext[0] = 3;
        else if (flen == 4096)
          ext[0] = 4;
        else
          throw new NumberFormatException();
        if (extensions == null)
          extensions = new LinkedList();
        extensions.add(new Extension(Extension.Type.MAX_FRAGMENT_LENGTH, ext));
      }
    catch (NumberFormatException nfe) { }

    // FIXME: set certificate types.

    // Send the client hello.
    ProtocolVersion version = session.protocol;
    Random clientRandom =
      new Random(Util.unixTime(), session.random.generateSeed(28));
    session.protocol = (ProtocolVersion) session.enabledProtocols.last();
    List comp = new ArrayList(2);
    comp.add(CompressionMethod.ZLIB);
    comp.add(CompressionMethod.NULL);
    ClientHello clientHello =
      new ClientHello(session.protocol, clientRandom, sessionId,
                      session.enabledSuites, comp, extensions);
    Handshake msg = new Handshake(Handshake.Type.CLIENT_HELLO, clientHello);
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    msg.write (dout, version);
//     recordOutput.setHandshakeAvail(msg.write(dout, version));
    dout.flush();
//     try
//       {
//         Thread.sleep(150);
//       }
//     catch (InterruptedException ie)
//       {
//       }

    // Receive the server hello.
    msg = Handshake.read(din);
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    if (msg.getType() != Handshake.Type.SERVER_HELLO)
      {
        throwUnexpectedMessage();
      }
    ServerHello serverHello = (ServerHello) msg.getBody();
    Random serverRandom = serverHello.getRandom();
    version = serverHello.getVersion();

    // If we don't directly support the server's protocol version, choose
    // the highest one we support that is less than the server's version.
    if (!session.enabledProtocols.contains(version))
      {
        ProtocolVersion v1 = null, v2 = null;
        for (Iterator it = session.enabledProtocols.iterator();
             it.hasNext(); )
          {
            v1 = (ProtocolVersion) it.next();
            if (v1.compareTo(version) > 0)
              break;
            v2 = v1;
          }
        version = v1;
      }

    // The server's version is either unsupported by us (unlikely) or the user
    // has only enabled incompatible versions.
    if (version == null)
      {
        Alert.Description desc = null;
        if (serverHello.getVersion() == ProtocolVersion.SSL_3)
          {
            desc = Alert.Description.HANDSHAKE_FAILURE;
          }
        else
          {
            desc = Alert.Description.PROTOCOL_VERSION;
          }
        Alert alert = new Alert(Alert.Level.FATAL, desc);
        sendAlert(alert);
        session.currentAlert = alert;
        fatal();
        throw new AlertException(alert, true);
      }

    if (serverHello.getExtensions() != null)
      {
        for (Iterator it = serverHello.getExtensions().iterator();
             it.hasNext(); )
          {
            Extension e = (Extension) it.next();
            if (e.getType() == Extension.Type.MAX_FRAGMENT_LENGTH)
              {
                int len = Extensions.getMaxFragmentLength(e).intValue();
                session.params.setFragmentLength(len);
//                 recordOutput.setFragmentLength(len);
//                 recordInput.setFragmentLength(len);
              }
            else if (e.getType() == Extension.Type.CERT_TYPE)
              {
                certType = Extensions.getServerCertType(e);
              }
          }
      }

    CipherSuite suite = serverHello.getCipherSuite().resolve(version);
    boolean newSession = true;
    if (sessionId.length > 0 &&
        Arrays.equals(sessionId, serverHello.getSessionId()))
      {
        SecurityParameters params = session.params;
        SecureRandom random = session.random;
        session = (Session) continuedSession.clone();
        session.params = params;
        session.random = random;
        recordInput.setSession(session);
//         recordOutput.setSession(session);
        suite = session.cipherSuite;
        newSession = false;
      }
    else
      {
        sessionContext.removeSession(new Session.ID(sessionId));
      }
    if (newSession)
      {
        session.peerHost = remoteHost;
        session.sessionId = new Session.ID(serverHello.getSessionId());
        session.cipherSuite = suite;
      }
    session.params.reset();
//     session.params.setInMac(null);
//     session.params.setOutMac(null);
//     session.params.setInRandom(null);
//     session.params.setOutRandom(null);
//     session.params.setInCipher(null);
//     session.params.setOutCipher(null);
    session.currentAlert = null;
    session.valid = true;
    session.protocol = version;

    // If the server responded with the same session id that we sent, we
    // assume that the session will be continued, and skip the bulk of the
    // handshake.
    if (newSession)
      {
        PublicKey serverKey = null, serverKex = null;
        KeyPair clientKeys = null, clientKex = null;
        CertificateRequest certReq;
        boolean sendKeyExchange = false;
        BigInteger srp_x = null;
        IKeyAgreementParty clientKA = null;
        IncomingMessage in; // used for key agreement protocol exchange
        OutgoingMessage out = null;

        if (suite.getKeyExchange() == "SRP")
          {
            String password = askPassword(user);
            if (DEBUG_KEY_EXCHANGE)
              {
                logger.log (Component.SSL_KEY_EXCHANGE,
                            "SRP: password read is ''{0}''", password);
              }
            byte[] userSrpPassword = password.getBytes("UTF-8");

            // instantiate and setup client-side key agreement party
            clientKA = KeyAgreementFactory.getPartyAInstance(Registry.SRP_TLS_KA);
            Map clientAttributes = new HashMap();
            clientAttributes.put(SRP6KeyAgreement.HASH_FUNCTION,
                                 Registry.SHA160_HASH);
            clientAttributes.put(SRP6KeyAgreement.USER_IDENTITY, user);
            clientAttributes.put(SRP6KeyAgreement.USER_PASSWORD, userSrpPassword);
            try
              {
                clientKA.init(clientAttributes);
                // initiate the exchange
                out = clientKA.processMessage(null);
              }
            catch (KeyAgreementException x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                  }
                throwHandshakeFailure();
              }
          }

        if (suite.getSignature() != "anon")
          {
            msg = Handshake.read(din, certType);
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            if (msg.getType() != Handshake.Type.CERTIFICATE)
              {
                throwUnexpectedMessage();
              }
            Certificate serverCertificate = (Certificate) msg.getBody();
            X509Certificate[] peerCerts = serverCertificate.getCertificates();
            try
              {
                session.trustManager.checkServerTrusted(peerCerts,
                                                        suite.getAuthType());
                if (suite.getSignature() == "RSA" &&
                    !(peerCerts[0].getPublicKey() instanceof RSAPublicKey))
                  throw new InvalidKeyException("improper public key");
                if (suite.getKeyExchange() == "DH" &&
                    !(peerCerts[0].getPublicKey() instanceof DHPublicKey))
                  throw new InvalidKeyException("improper public key");
                if (suite.getKeyExchange() == "DHE")
                  {
                    if (suite.getSignature() == "RSA" &&
                        !(peerCerts[0].getPublicKey() instanceof RSAPublicKey))
                      throw new InvalidKeyException("improper public key");
                    if (suite.getSignature() == "DSS" &&
                        !(peerCerts[0].getPublicKey() instanceof DSAPublicKey))
                      throw new InvalidKeyException("improper public key");
                  }
                session.peerCerts = peerCerts;
                session.peerVerified = true;
              }
            catch (InvalidKeyException ike)
              {
                throwHandshakeFailure();
              }
            catch (Exception x)
              {
                if (!checkCertificates(peerCerts))
                  {
                    peerUnverified(peerCerts);
                    SSLPeerUnverifiedException e =
                      new SSLPeerUnverifiedException ("could not verify peer certificate: "+
                                                      peerCerts[0].getSubjectDN());
                    e.initCause (x);
                    throw e;
                  }
                session.peerCerts = peerCerts;
                session.peerVerified = true;
              }
            serverKey = peerCerts[0].getPublicKey();
            serverKex = serverKey;
          }

        msg = Handshake.read(din, suite, serverKey);

        // Receive the server's key exchange.
        if (msg.getType() == Handshake.Type.SERVER_KEY_EXCHANGE)
          {
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            ServerKeyExchange skex = (ServerKeyExchange) msg.getBody();
            serverKex = skex.getPublicKey();
            if (suite.getSignature() != "anon")
              {
                ISignature sig = null;
                if (suite.getSignature() == "RSA")
                  {
                    sig = new SSLRSASignature();
                  }
                else if (suite.getSignature() == "DSS")
                  {
                    sig = SignatureFactory.getInstance(Registry.DSS_SIG);
                  }
                sig.setupVerify(Collections.singletonMap(
                  ISignature.VERIFIER_KEY, serverKey));
                byte[] buf = clientRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                buf = serverRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                if (suite.getKeyExchange() == "RSA")
                  {
                    updateSig(sig, ((RSAPublicKey) serverKex).getModulus());
                    updateSig(sig, ((RSAPublicKey) serverKex).getPublicExponent());
                  }
                else if (suite.getKeyExchange() == "DHE")
                  {
                    updateSig(sig, ((DHPublicKey) serverKex).getParams().getP());
                    updateSig(sig, ((DHPublicKey) serverKex).getParams().getG());
                    updateSig(sig, ((DHPublicKey) serverKex).getY());
                  }
                else if (suite.getKeyExchange() == "SRP")
                  {
                    updateSig(sig, ((SRPPublicKey) serverKex).getN());
                    updateSig(sig, ((SRPPublicKey) serverKex).getG());
                    byte[] srpSalt = skex.getSRPSalt();
                    sig.update((byte) srpSalt.length);
                    sig.update(srpSalt, 0, srpSalt.length);
                    updateSig(sig, ((SRPPublicKey) serverKex).getY());
                  }
                if (!sig.verify(skex.getSignature().getSigValue()))
                  {
                    throwHandshakeFailure();
                  }
              }

            if (suite.getKeyExchange() == "SRP")
              {
                // use server's key exchange data to continue
                // agreement protocol by faking a received incoming
                // message.  again the following code can be broken
                // into multiple blocks for more accurate exception
                // handling
                try
                  {
                    out = new OutgoingMessage();
                    out.writeMPI(((SRPPublicKey) serverKex).getN());
                    out.writeMPI(((SRPPublicKey) serverKex).getG());
                    out.writeMPI(new BigInteger(1, skex.getSRPSalt()));
                    out.writeMPI(((SRPPublicKey) serverKex).getY());

                    in = new IncomingMessage(out.toByteArray());

                    out = clientKA.processMessage(in);
                    if (DEBUG_KEY_EXCHANGE)
                      {
                        logger.log (Component.SSL_KEY_EXCHANGE, "clientKA isComplete? {0}",
                                    Boolean.valueOf (clientKA.isComplete()));
                      }
                  }
                catch (KeyAgreementException x)
                  {
                    if (DEBUG_KEY_EXCHANGE)
                      {
                        logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                      }
                    throwHandshakeFailure();
                  }
              }
            msg = Handshake.read(din, suite, serverKey);
          }

        // See if the server wants us to send our certificates.
        certReq = null;
        if (msg.getType() == Handshake.Type.CERTIFICATE_REQUEST)
          {
            if (suite.getSignature() == "anon")
              {
                throwHandshakeFailure();
              }
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            certReq = (CertificateRequest) msg.getBody();
            msg = Handshake.read(din);
          }

        // Read ServerHelloDone.
        if (msg.getType() != Handshake.Type.SERVER_HELLO_DONE)
          {
            throwUnexpectedMessage();
          }
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);

        // Send our certificate chain if the server asked for it.
        if (certReq != null)
          {
            String alias = session.keyManager.chooseClientAlias(
              certReq.getTypeStrings(), certReq.getAuthorities(), null);
            if (alias == null && version == ProtocolVersion.SSL_3)
              {
                Alert alert =
                  new Alert(Alert.Level.WARNING, Alert.Description.NO_CERTIFICATE);
                sendAlert(alert);
              }
            else
              {
                X509Certificate[] chain =
                  session.keyManager.getCertificateChain(alias);
                PrivateKey key = session.keyManager.getPrivateKey(alias);
                if (chain == null)
                  {
                    chain = new X509Certificate[0];
                  }
                Certificate cert = new Certificate(chain);
                msg = new Handshake(Handshake.Type.CERTIFICATE, cert);
                if (DEBUG_HANDSHAKE_LAYER)
                  logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
                msg.write(dout, version);
//                 recordOutput.setHandshakeAvail(msg.write(dout, version));;
                dout.flush();
                if (chain.length > 0)
                  {
                    session.localCerts = chain;
                    clientKeys = new KeyPair(chain[0].getPublicKey(), key);
                  }
              }
          }

        // Send our key exchange.
        byte[] preMasterSecret = null;
        ClientKeyExchange ckex = null;
        if (suite.getKeyExchange() == "RSA")
          {
            ProtocolVersion v =
              (ProtocolVersion) session.enabledProtocols.last();
            byte[] b = new byte[46];
            session.random.nextBytes (b);
            preMasterSecret = Util.concat(v.getEncoded(), b);
            EME_PKCS1_V1_5 pkcs1 = EME_PKCS1_V1_5.getInstance((RSAPublicKey) serverKex);
            BigInteger bi = new BigInteger(1,
              pkcs1.encode(preMasterSecret, session.random));
            bi = RSA.encrypt((RSAPublicKey) serverKex, bi);
            ckex = new ClientKeyExchange(Util.trim(bi));
          }
        else if (suite.getKeyExchange().startsWith("DH"))
          {
            if (clientKeys == null ||
                !(clientKeys.getPublic() instanceof DHPublicKey))
              {
                GnuDHPrivateKey tmpKey =
                  new GnuDHPrivateKey(null, ((DHPublicKey) serverKex).getParams().getP(),
                      ((DHPublicKey) serverKex).getParams().getG(), null);
                clientKA = KeyAgreementFactory.getPartyBInstance(Registry.DH_KA);
                Map attr = new HashMap();
                attr.put(DiffieHellmanKeyAgreement.KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY,
                         tmpKey);
                attr.put(DiffieHellmanKeyAgreement.SOURCE_OF_RANDOMNESS,
                         session.random);
                try
                  {
                    clientKA.init(attr);
                    out = new OutgoingMessage();
                    out.writeMPI(((DHPublicKey) serverKex).getY());
                    in = new IncomingMessage(out.toByteArray());
                    out = clientKA.processMessage(in);
                    in = new IncomingMessage(out.toByteArray());
                    ckex = new ClientKeyExchange(in.readMPI());
                  }
                catch (KeyAgreementException kae)
                  {
                    if (DEBUG_KEY_EXCHANGE)
                      {
                        logger.log (Component.SSL_KEY_EXCHANGE, "DH exception", kae);
                      }
                    internalError();
                    RuntimeException re = new RuntimeException (kae.getMessage());
                    re.initCause (kae);
                    throw re;
                  }
              }
            else
              {
                clientKA = KeyAgreementFactory.getPartyBInstance(Registry.ELGAMAL_KA);
                Map attr = new HashMap();
                attr.put(ElGamalKeyAgreement.KA_ELGAMAL_RECIPIENT_PRIVATE_KEY,
                         clientKeys.getPrivate());
                try
                  {
                    // The key exchange is already complete here; our public
                    // value was sent with our certificate.
                    clientKA.init(attr);
                  }
                catch (KeyAgreementException kae)
                  {
                    if (DEBUG_KEY_EXCHANGE)
                      logger.log (Component.SSL_KEY_EXCHANGE, "DH exception", kae);
                    internalError();
                    RuntimeException re = new RuntimeException (kae.getMessage());
                    re.initCause (kae);
                    throw re;
                  }
                ckex = new ClientKeyExchange(new byte[0]);
              }
          }
        else if (suite.getKeyExchange() == "SRP")
          {
            // at this point, out --the outgoing message-- already contains
            // what we want. so...
            BigInteger A = null;
            try
              {
                in = new IncomingMessage(out.toByteArray());
                A = in.readMPI();
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "client A:{0}", A);
                  }
              }
            catch (KeyAgreementException x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                  }
                throwHandshakeFailure();
              }
            ckex = new ClientKeyExchange(A);
          }
        msg = new Handshake(Handshake.Type.CLIENT_KEY_EXCHANGE, ckex);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write (dout, version);
//         recordOutput.setHandshakeAvail(msg.write(dout, version));;

        // Generate the master secret.
        if (suite.getKeyExchange().startsWith("DH"))
          {
            try
              {
                preMasterSecret = clientKA.getSharedSecret();
              }
            catch (KeyAgreementException kae)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "DH exception", kae);
                  }
                internalError();
                RuntimeException re = new RuntimeException (kae.getMessage());
                re.initCause (kae);
                throw re;
              }
          }
        else if (suite.getKeyExchange() == "SRP")
          {
            try
              {
                preMasterSecret = clientKA.getSharedSecret();
              }
            catch (KeyAgreementException x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                  }
                throwHandshakeFailure();
              }
            finally
              {
                clientKA = null;
              }
          }
        if (DEBUG_KEY_EXCHANGE)
          {
            logger.log (Component.SSL_KEY_EXCHANGE, "preMasterSecret:\n{0}",
                        Util.toHexString (preMasterSecret, ':'));
            logger.log (Component.SSL_KEY_EXCHANGE, "client.random:\n{0}",
                        Util.toHexString(clientRandom.getEncoded(), ':'));
            logger.log (Component.SSL_KEY_EXCHANGE, "server.random:\n{0}",
                        Util.toHexString(serverRandom.getEncoded(), ':'));
          }
        IRandom genSecret = null;
        if (version == ProtocolVersion.SSL_3)
          {
            genSecret = new SSLRandom();
            HashMap attr = new HashMap();
            attr.put(SSLRandom.SECRET, preMasterSecret);
            attr.put(SSLRandom.SEED,
              Util.concat(clientRandom.getEncoded(), serverRandom.getEncoded()));
            genSecret.init(attr);
          }
        else
          {
            genSecret = new TLSRandom();
            HashMap attr = new HashMap();
            attr.put(TLSRandom.SECRET, preMasterSecret);
            attr.put(TLSRandom.SEED,
              Util.concat(("master secret").getBytes("UTF-8"),
              Util.concat(clientRandom.getEncoded(), serverRandom.getEncoded())));
            genSecret.init(attr);
          }
        session.masterSecret = new byte[48];
        try
          {
            genSecret.nextBytes(session.masterSecret, 0, 48);
            for (int i = 0; i < preMasterSecret.length; i++)
              {
                preMasterSecret[i] = 0;
              }
          }
        catch (LimitReachedException shouldNotHappen)
          {
            internalError();
            RuntimeException re = new RuntimeException (shouldNotHappen.getMessage());
            re.initCause (shouldNotHappen);
            throw re;
          }

        if (DEBUG_KEY_EXCHANGE)
          {
            logger.log (Component.SSL_KEY_EXCHANGE, "masterSecret: {0}",
                        Util.toHexString(session.masterSecret, ':'));
          }

        // Send our certificate verify message.
        if (certReq != null && clientKeys != null)
          {
            IMessageDigest vMD5 = (IMessageDigest) md5.clone();
            IMessageDigest vSHA = (IMessageDigest) sha.clone();
            PrivateKey key = clientKeys.getPrivate();
            Object sig = null;
            String sigAlg = null;
            try
              {
                if (key instanceof DSAPrivateKey)
                  {
                    sig = DSSSignature.sign((DSAPrivateKey) key, vSHA.digest(),
                                            session.random);
                    sigAlg = "DSS";
                  }
                else if (key instanceof RSAPrivateKey)
                  {
                    SSLRSASignature rsa = new SSLRSASignature(vMD5, vSHA);
                    rsa.setupSign(Collections.singletonMap(ISignature.SIGNER_KEY, key));
                    sig = rsa.sign();
                    sigAlg = "RSA";
                  }
                else
                  {
                    throw new InvalidKeyException("no appropriate key");
                  }
              }
            catch (Exception x)
              {
                throwHandshakeFailure();
              }
            CertificateVerify verify = new CertificateVerify(sig, sigAlg);
            msg = new Handshake(Handshake.Type.CERTIFICATE_VERIFY, verify);
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            msg.write(dout, version);
//             recordOutput.setHandshakeAvail(msg.write(dout, version));;
          }
        dout.flush();
      }

    byte[][] keys = null;
    try
      {
        keys = generateKeys(serverRandom.getEncoded(),
                            clientRandom.getEncoded(), version);
      }
    catch (Exception x)
      {
        internalError();
        RuntimeException re = new RuntimeException (x.getMessage());
        re.initCause (x);
        throw re;
      }

    session.params.setVersion (version);

    // Initialize the algorithms with the derived keys.
    Object readMac = null, writeMac = null;
    Object readCipher = null, writeCipher = null;
    try
      {
        if (session.params instanceof GNUSecurityParameters)
          {
            HashMap attr = new HashMap();
            writeMac = CipherSuite.getMac(suite.getMac());
            readMac  = CipherSuite.getMac(suite.getMac());
            attr.put(IMac.MAC_KEY_MATERIAL, keys[0]);
            ((IMac) writeMac).init(attr);
            attr.put(IMac.MAC_KEY_MATERIAL, keys[1]);
            ((IMac) readMac).init(attr);
            if (suite.getCipher() == "RC4")
              {
                writeCipher = new ARCFour();
                readCipher = new ARCFour();
                attr.clear();
                attr.put(ARCFour.ARCFOUR_KEY_MATERIAL, keys[2]);
                ((ARCFour) writeCipher).init(attr);
                attr.put(ARCFour.ARCFOUR_KEY_MATERIAL, keys[3]);
                ((ARCFour) readCipher).init(attr);
              }
            else if (!suite.isStreamCipher())
              {
                writeCipher = CipherSuite.getCipher(suite.getCipher());
                readCipher = CipherSuite.getCipher(suite.getCipher());
                attr.clear();
                attr.put(IMode.KEY_MATERIAL, keys[2]);
                attr.put(IMode.IV, keys[4]);
                attr.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
                ((IMode) writeCipher).init(attr);
                attr.put(IMode.KEY_MATERIAL, keys[3]);
                attr.put(IMode.IV, keys[5]);
                attr.put(IMode.STATE, new Integer(IMode.DECRYPTION));
                ((IMode) readCipher).init(attr);
              }
          }
        else // JCESecurityParameters
          {
            writeMac = CipherSuite.getJCEMac (suite.getMac());
            readMac = CipherSuite.getJCEMac (suite.getMac());
            writeCipher = CipherSuite.getJCECipher (suite.getCipher());
            readCipher = CipherSuite.getJCECipher (suite.getCipher());
            ((Mac) writeMac).init (new SecretKeySpec (keys[0], suite.getMac()));
            ((Mac) readMac).init (new SecretKeySpec (keys[1], suite.getMac()));
            if (!suite.isStreamCipher())
              {
                ((Cipher) writeCipher).init (Cipher.ENCRYPT_MODE,
                                             new SecretKeySpec (keys[2], suite.getCipher()),
                                             new IvParameterSpec (keys[4]));
                ((Cipher) readCipher).init (Cipher.DECRYPT_MODE,
                                            new SecretKeySpec (keys[3], suite.getCipher()),
                                            new IvParameterSpec (keys[5]));
              }
            else
              {
                ((Cipher) writeCipher).init (Cipher.ENCRYPT_MODE,
                                             new SecretKeySpec (keys[2], suite.getCipher()));
                ((Cipher) readCipher).init (Cipher.DECRYPT_MODE,
                                            new SecretKeySpec (keys[3], suite.getCipher()));
              }
          }
      }
    // These should technically never happen, if our key generation is not
    // broken.
    catch (InvalidKeyException ike)
      {
        internalError();
        RuntimeException re = new RuntimeException (ike.getMessage());
        re.initCause(ike);
        throw re;
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        internalError();
        RuntimeException re = new RuntimeException (iape.getMessage());
        re.initCause (iape);
        throw re;
      }
    // These indicate a configuration error with the JCA.
    catch (NoSuchAlgorithmException nsae)
      {
        session.enabledSuites.remove (suite);
        internalError();
        SSLException x = new SSLException ("suite " + suite + " not available in this configuration");
        x.initCause (nsae);
        throw x;
      }
    catch (NoSuchPaddingException nspe)
      {
        session.enabledSuites.remove (suite);
        internalError();
        SSLException x = new SSLException ("suite " + suite + " not available in this configuration");
        x.initCause (nspe);
        throw x;
      }

    Finished finis = null;

    if (newSession)
      {
        changeCipherSpec();
        session.params.setDeflating(serverHello.getCompressionMethod() == CompressionMethod.ZLIB);
        session.params.setOutMac(writeMac);
        session.params.setOutCipher(writeCipher);
        finis = generateFinished(version, (IMessageDigest) md5.clone(),
                                 (IMessageDigest) sha.clone(), true);
        msg = new Handshake(Handshake.Type.FINISHED, finis);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write(dout, version);
        dout.flush();
      }

    if (session.currentAlert != null &&
        session.currentAlert.getLevel() == Alert.Level.FATAL)
      {
        fatal();
        throw new AlertException(session.currentAlert, false);
      }

    synchronized (session.params)
      {
        readChangeCipherSpec ();
        session.params.setInflating(serverHello.getCompressionMethod() == CompressionMethod.ZLIB);
        session.params.setInMac(readMac);
        session.params.setInCipher(readCipher);
        session.params.notifyAll();
      }

    Finished verify = generateFinished(version, (IMessageDigest) md5.clone(),
                                       (IMessageDigest) sha.clone(), false);

    msg = Handshake.read(din, suite, null);
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    if (msg.getType() != Handshake.Type.FINISHED)
      {
        throwUnexpectedMessage();
      }
    finis = (Finished) msg.getBody();
    if (version == ProtocolVersion.SSL_3)
      {
        if (!Arrays.equals(finis.getMD5Hash(), verify.getMD5Hash()) ||
            !Arrays.equals(finis.getSHAHash(), verify.getSHAHash()))
          {
            throwHandshakeFailure();
          }
      }
    else
      {
        if (!Arrays.equals(finis.getVerifyData(), verify.getVerifyData()))
          {
            throwHandshakeFailure();
          }
      }

    if (!newSession)
      {
        changeCipherSpec();
        session.params.setDeflating(serverHello.getCompressionMethod() == CompressionMethod.ZLIB);
        session.params.setOutMac(writeMac);
        session.params.setOutCipher(writeCipher);
        finis = generateFinished(version, md5, sha, true);
        msg = new Handshake(Handshake.Type.FINISHED, finis);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write(dout, version);
        dout.flush();
      }

    handshakeCompleted();
  }

  /**
   * Perform the server handshake.
   */
  private void doServerHandshake() throws IOException
  {
    if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "doing server handshake in {0}",
                    Thread.currentThread());
      }

    if (remoteHost == null)
      {
        remoteHost = getInetAddress().getHostName();
      }
    if (remoteHost == null)
      {
        remoteHost = getInetAddress().getHostAddress();
      }

    IMessageDigest md5 = HashFactory.getInstance(Registry.MD5_HASH);
    IMessageDigest sha = HashFactory.getInstance(Registry.SHA160_HASH);
    DigestInputStream din = new DigestInputStream(handshakeIn, md5, sha);
    DigestOutputStream dout = new DigestOutputStream(handshakeOut, md5, sha);

    // Read the client hello.
    Handshake msg = Handshake.read(din);
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    if (msg.getType() != Handshake.Type.CLIENT_HELLO)
      {
        throwUnexpectedMessage();
      }
    ClientHello clientHello = (ClientHello) msg.getBody();
    Random clientRandom = clientHello.getRandom();
    ProtocolVersion version = clientHello.getVersion();
    ProtocolVersion server =
      (ProtocolVersion) session.enabledProtocols.last();
    CompressionMethod comp;
    if (clientHello.getCompressionMethods().contains(CompressionMethod.ZLIB))
      comp = CompressionMethod.ZLIB;
    else
      comp = CompressionMethod.NULL;
    if (!session.enabledProtocols.contains(version)
        && version.compareTo(server) < 0)
      {
        Alert alert = new Alert(Alert.Level.FATAL,
                                Alert.Description.PROTOCOL_VERSION);
        sendAlert(alert);
        session.currentAlert = alert;
        throw new AlertException(alert, true);
      }

    // Look through the extensions sent by the client (if any), and react to
    // them appropriately.
    List extensions = null;
    String remoteUser = null;
    if (clientHello.getExtensions() != null)
      {
        for (Iterator it = clientHello.getExtensions().iterator(); it.hasNext();)
          {
            Extension ex = (Extension) it.next();
            if (ex.getType() == Extension.Type.SERVER_NAME)
              {
                if (extensions == null)
                  {
                    extensions = new LinkedList();
                  }
                extensions.add(ex);
              }
            else if (ex.getType() == Extension.Type.MAX_FRAGMENT_LENGTH)
              {
                int maxLen = Extensions.getMaxFragmentLength(ex).intValue();
//                 recordInput.setFragmentLength(maxLen);
//                 recordOutput.setFragmentLength(maxLen);
                session.params.setFragmentLength(maxLen);
                if (extensions == null)
                  {
                    extensions = new LinkedList();
                  }
                extensions.add(ex);
              }
            else if (ex.getType() == Extension.Type.SRP)
              {
                if (extensions == null)
                  {
                    extensions = new LinkedList();
                  }
                byte[] b = ex.getValue();
                remoteUser = new String(ex.getValue(), 1, b[0] & 0xFF, "UTF-8");
                session.putValue("srp-username", remoteUser);
              }
          }
      }

    CipherSuite suite = selectSuite(clientHello.getCipherSuites(), version);
    if (suite == null)
      {
        return;
      }

    // If the selected suite turns out to be SRP, set up the key exchange
    // objects.
    IKeyAgreementParty serverKA = null;
    IncomingMessage in;
    OutgoingMessage out = null;
    if (suite.getKeyExchange() == "SRP")
      {
        // FIXME
        // Uhm, I don't think this can happen, because if remoteUser is null
        // we cannot choose an SRP ciphersuite...
        if (remoteUser == null)
          {
            Alert alert = new Alert(Alert.Level.FATAL,
              Alert.Description.MISSING_SRP_USERNAME);
            sendAlert(alert);
            throw new AlertException(alert, true);
          }

        SRPAuthInfoProvider srpDB = new SRPAuthInfoProvider();
        Map dbAttributes = new HashMap();
        dbAttributes.put(SRPRegistry.PASSWORD_DB,
          session.srpTrustManager.getPasswordFile());
        srpDB.activate(dbAttributes);

        // FIXME
        // We can also fake that the user exists, and generate a dummy (and
        // invalid) master secret, and let the handshake fail at the Finished
        // message. This is better than letting the connecting side know that
        // the username they sent isn't valid.
        //
        // But how to implement this?
        if (!srpDB.contains(remoteUser))
          {
            Alert alert = new Alert(Alert.Level.FATAL,
              Alert.Description.UNKNOWN_SRP_USERNAME);
            sendAlert(alert);
            throw new AlertException(alert, true);
          }

        serverKA = KeyAgreementFactory.getPartyBInstance(Registry.SRP_TLS_KA);
        Map serverAttributes = new HashMap();
        serverAttributes.put(SRP6KeyAgreement.HASH_FUNCTION,
                             Registry.SHA160_HASH);
        serverAttributes.put(SRP6KeyAgreement.HOST_PASSWORD_DB, srpDB);

        try
          {
            serverKA.init(serverAttributes);
            out = new OutgoingMessage();
            out.writeString(remoteUser);
            in = new IncomingMessage(out.toByteArray());
            out = serverKA.processMessage(in);
          }
        catch (KeyAgreementException x)
          {
            throwHandshakeFailure();
          }
      }

    // Check if the session specified by the client's ID corresponds
    // to a saved session, and if so, continue it.
    boolean newSession = true;
    if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "saved sessions: {0}", sessionContext);
      }
    if (sessionContext.containsSessionID(
        new Session.ID(clientHello.getSessionId())))
      {
        Session old = session;
        session = (Session) sessionContext.getSession(clientHello.getSessionId());
        if (!clientHello.getCipherSuites().contains(session.cipherSuite))
          {
            throwHandshakeFailure();
          }
        if (session.getPeerHost().equals(remoteHost) &&
            old.enabledProtocols.contains(session.protocol))
          {
            session = (Session) session.clone();
            suite = session.cipherSuite;
            newSession = false;
            recordInput.setSession(session);
            session.currentAlert = null;
            session.params = old.params;
            session.random = old.random;
          }
        else
          {
            if (DEBUG_HANDSHAKE_LAYER)
              {
                logger.log (Component.SSL_HANDSHAKE, "rejected section; hosts equal? {0}, same suites? {1}",
                            new Object[] { Boolean.valueOf (session.getPeerHost().equals(remoteHost)),
                                           Boolean.valueOf (old.enabledProtocols.contains(session.protocol)) });
              }
            session = old;
            session.peerHost = remoteHost;
            newSession = true;
          }
      }
    else if (DEBUG_HANDSHAKE_LAYER)
      {
        logger.log (Component.SSL_HANDSHAKE, "rejected session; have session id? {0}, saved sessions: {1}",
                    new Object[] { Boolean.valueOf (sessionContext.containsSessionID(new Session.ID(clientHello.getSessionId()))),
                                   sessionContext });
      }
    if (newSession)
      {
        byte[] buf = new byte[32];
        Session.ID sid = null;
        do
          {
            session.random.nextBytes(buf);
            sid = new Session.ID(buf);
          }
        while (sessionContext.containsSessionID(sid));
        session.sessionId = sid;
      }
    session.valid = true;
    session.peerHost = remoteHost;
    session.cipherSuite = suite;
    session.protocol = version;
    session.params.setVersion (version);

    // Send the server hello.
    Random serverRandom = new Random(Util.unixTime(),
                                     session.random.generateSeed(28));
    ServerHello serverHello = new ServerHello(version, serverRandom,
                                              session.getId(), suite,
                                              comp, extensions);
    msg = new Handshake(Handshake.Type.SERVER_HELLO, serverHello);
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    msg.write(dout, version);
//     recordOutput.setHandshakeAvail(msg.write(dout, version));
    dout.flush();

    if (newSession)
      {
        X509Certificate[] certs = null;
        PrivateKey serverKey = null;
        if (suite.getSignature() != "anon")
          {
            // Send our CA-issued certificate to the client.
            String alias = session.keyManager.chooseServerAlias(suite.getAuthType(),
                                                                null, null);
            certs = session.keyManager.getCertificateChain(alias);
            serverKey = session.keyManager.getPrivateKey(alias);
            if (certs == null || serverKey == null)
              {
                throwHandshakeFailure();
              }
            session.localCerts = certs;
            Certificate serverCert = new Certificate(certs);
            msg = new Handshake(Handshake.Type.CERTIFICATE, serverCert);
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            msg.write(dout, version);
//             recordOutput.setHandshakeAvail(msg.write(dout, version));;
            dout.flush();
          }

        // If the certificate we sent does not contain enough information to
        // do the key exchange (in the case of ephemeral Diffie-Hellman,
        // export RSA, and SRP) we send a signed public key to be used for the
        // key exchange.
        KeyPair signPair = null;
        if (certs != null)
          {
            signPair = new KeyPair(certs[0].getPublicKey(), serverKey);
          }
        KeyPair kexPair = signPair;
        ServerKeyExchange skex = null;

        // Set up our key exchange, and/or prepare our ServerKeyExchange
        // message.
        if ((suite.getKeyExchange() == "RSA" && suite.isExportable() &&
             ((RSAPrivateKey) serverKey).getModulus().bitLength() > 512))
          {
            kexPair = KeyPool.generateRSAKeyPair();
            RSAPublicKey pubkey = (RSAPublicKey) kexPair.getPublic();
            Signature s = null;
            if (suite.getSignature() != "anon")
              {
                SSLRSASignature sig = new SSLRSASignature();
                sig.setupSign(Collections.singletonMap(ISignature.SIGNER_KEY,
                                                       signPair.getPrivate()));
                byte[] buf = clientRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                buf = serverRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                updateSig(sig, pubkey.getModulus());
                updateSig(sig, pubkey.getPublicExponent());
                s = new Signature(sig.sign(), "RSA");
              }
            skex = new ServerKeyExchange(pubkey, s);
          }
        else if (suite.getKeyExchange() == "DH")
          {
            serverKA = KeyAgreementFactory.getPartyBInstance(Registry.ELGAMAL_KA);
            Map attr = new HashMap();
            attr.put(ElGamalKeyAgreement.KA_ELGAMAL_RECIPIENT_PRIVATE_KEY,
                     serverKey);
            try
              {
                serverKA.init(attr);
              }
            catch (KeyAgreementException kae)
              {
                if (DEBUG_KEY_EXCHANGE)
                  logger.log (Component.SSL_KEY_EXCHANGE, "DH exception", kae);
                internalError();
                RuntimeException re = new RuntimeException (kae.getMessage());
                re.initCause (kae);
                throw re;
              }
            // We don't send a ServerKeyExchange for this suite.
          }
        else if (suite.getKeyExchange() == "DHE")
          {
            serverKA = KeyAgreementFactory.getPartyAInstance(Registry.DH_KA);
            Map attr = new HashMap();
            GnuDHPrivateKey servParams = DiffieHellman.getParams();
            attr.put(DiffieHellmanKeyAgreement.KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY,
                     servParams);
            attr.put(DiffieHellmanKeyAgreement.SOURCE_OF_RANDOMNESS,
                     session.random);
            BigInteger serv_y = null;
            try
              {
                serverKA.init(attr);
                out = serverKA.processMessage(null);
                in = new IncomingMessage(out.toByteArray());
                serv_y = in.readMPI();
              }
            catch (KeyAgreementException kae)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "DHE exception", kae);
                  }
                internalError();
                RuntimeException re = new RuntimeException (kae.getMessage());
                re.initCause (kae);
                throw re;
              }
            GnuDHPublicKey pubkey =
              new GnuDHPublicKey(null, servParams.getParams().getP(),
                                 servParams.getParams().getG(), serv_y);
            Signature s = null;
            if (suite.getSignature() != "anon")
              {
                ISignature sig = null;
                if (suite.getSignature() == "RSA")
                  {
                    sig = new SSLRSASignature();
                  }
                else
                  {
                    sig = SignatureFactory.getInstance(Registry.DSS_SIG);
                  }
                sig.setupSign(Collections.singletonMap(ISignature.SIGNER_KEY,
                                                       signPair.getPrivate()));
                byte[] buf = clientRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                buf = serverRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                updateSig(sig, pubkey.getParams().getP());
                updateSig(sig, pubkey.getParams().getG());
                updateSig(sig, pubkey.getY());
                s = new Signature(sig.sign(), suite.getSignature());
              }
            skex = new ServerKeyExchange(pubkey, s);
          }
        else if (suite.getKeyExchange() == "SRP")
          {
            BigInteger N = null;
            BigInteger g = null;
            BigInteger salt = null;
            BigInteger B = null;
            try
              {
                in = new IncomingMessage(out.toByteArray());
                N = in.readMPI();
                g = in.readMPI();
                salt = in.readMPI();
                B = in.readMPI();
              }
            catch (KeyAgreementException x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                  }
                throwHandshakeFailure();
              }
            Signature s = null;
            final byte[] srpSalt = Util.trim(salt);
            if (suite.getSignature() != "anon")
              {
                ISignature sig = null;
                if (suite.getSignature() == "RSA")
                  {
                    sig = new SSLRSASignature();
                  }
                else
                  {
                    sig = SignatureFactory.getInstance(Registry.DSS_SIG);
                  }
                sig.setupSign(Collections.singletonMap(ISignature.SIGNER_KEY,
                                                       signPair.getPrivate()));
                byte[] buf = clientRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                buf = serverRandom.getEncoded();
                sig.update(buf, 0, buf.length);
                updateSig(sig, N);
                updateSig(sig, g);
                sig.update((byte) srpSalt.length);
                sig.update(srpSalt, 0, srpSalt.length);
                updateSig(sig, B);
                s = new Signature(sig.sign(), suite.getSignature());
              }
            final SRPPublicKey pubkey = new SRPPublicKey(N, g, B);
            skex = new ServerKeyExchange(pubkey, s, srpSalt);
          }
        if (skex != null)
          {
            msg = new Handshake(Handshake.Type.SERVER_KEY_EXCHANGE, skex);
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            msg.write(dout, version);
//             recordOutput.setHandshakeAvail(msg.write(dout, version));;
            dout.flush();
          }

        // If we are configured to want or need client authentication, then
        // ask for it.
        if (wantClientAuth || needClientAuth)
          {
            Principal[] auths = null;
            CertificateRequest.ClientType[] types =
              new CertificateRequest.ClientType[] {
                CertificateRequest.ClientType.RSA_SIGN,
                CertificateRequest.ClientType.DSS_SIGN,
                CertificateRequest.ClientType.RSA_FIXED_DH,
                CertificateRequest.ClientType.DSS_FIXED_DH
              };
            try
              {
                auths = (Principal[])
                  Util.transform(session.trustManager.getAcceptedIssuers(),
                    Principal.class, "getSubjectDN", null);
              }
            catch (Exception x)
              {
                internalError();
                RuntimeException re = new RuntimeException (x.getMessage());
                re.initCause (x);
                throw re;
              }
            CertificateRequest req = new CertificateRequest(types, auths);
            msg = new Handshake(Handshake.Type.CERTIFICATE_REQUEST, req);
            msg.write(dout, version);
            dout.flush();
          }

        // Send our server hello done.
        msg = new Handshake(Handshake.Type.SERVER_HELLO_DONE, null);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write(dout, version);
        dout.flush();

        if (suite.getKeyExchange() == "RSA")
          {
            msg = Handshake.read(din, suite, kexPair.getPublic());
          }
        else
          {
            msg = Handshake.read(din, suite, null);
          }
        boolean clientCertOk = false;
        boolean clientCanSign = false;
        X509Certificate[] clientChain = null;
        PublicKey clientKey = null;

        // Read the client's certificate, if sent.
        if (msg.getType() == Handshake.Type.CERTIFICATE)
          {
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
            Certificate cliCert = (Certificate) msg.getBody();
            clientChain = cliCert.getCertificates();
            try
              {
                session.trustManager.checkClientTrusted(clientChain,
                                                        suite.getAuthType());
                session.peerCerts = clientChain;
                session.peerVerified = true;
                clientKey = clientChain[0].getPublicKey();
              }
            catch (Exception x)
              {
              }
            clientCanSign = ((clientKey instanceof DSAPublicKey) ||
                             (clientKey instanceof RSAPublicKey));
            if (suite.getKeyExchange().startsWith("DH"))
              {
                msg = Handshake.read(din, suite, clientKey);
              }
            else
              {
                msg = Handshake.read(din, suite, kexPair.getPublic());
              }
          }

        // If we require client authentication, and the client sent an
        // unverifiable certificate or no certificate at all, drop the
        // connection.
        if (!session.peerVerified && needClientAuth)
          {
            throwHandshakeFailure();
          }

        // Read the client key exchange.
        if (msg.getType() != Handshake.Type.CLIENT_KEY_EXCHANGE)
          {
            throwUnexpectedMessage();
          }
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        ClientKeyExchange ckex = (ClientKeyExchange) msg.getBody();
        byte[] preMasterSecret = null;
        if (suite.getKeyExchange() == "RSA")
          {
            byte[] enc = (byte[]) ckex.getExchangeObject();
            BigInteger bi = new BigInteger(1, enc);
            try
              {
                bi = RSA.decrypt(kexPair.getPrivate(), bi);
                EME_PKCS1_V1_5 pkcs1 = EME_PKCS1_V1_5.getInstance(
                  (RSAPrivateKey) kexPair.getPrivate());
                preMasterSecret = pkcs1.decode(Util.concat(new byte[1], bi.toByteArray()));
                //rsa.init(kexPair);
                //preMasterSecret = rsa.decrypt(enc);
              }
            catch (Exception x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "RSA exception", x);
                  }
                // Generate a fake pre-master secret if the RSA decryption
                // fails.
                byte[] b = new byte[46];
                session.random.nextBytes (b);
                preMasterSecret = Util.concat(version.getEncoded(), b);
              }
          }
        else if (suite.getKeyExchange().startsWith("DH"))
          {
            try
              {
                out = new OutgoingMessage();
                if (clientKey == null)
                  out.writeMPI((BigInteger) ckex.getExchangeObject());
                else
                  out.writeMPI(((DHPublicKey) clientKey).getY());
                in = new IncomingMessage(out.toByteArray());
                serverKA.processMessage(in);
                preMasterSecret = serverKA.getSharedSecret();
              }
            catch (KeyAgreementException kae)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "DH exception", kae);
                  }
                internalError();
                RuntimeException re = new RuntimeException (kae.getMessage());
                re.initCause (kae);
                throw re;
              }
          }
        else if (suite.getKeyExchange() == "SRP")
          {
            BigInteger A = (BigInteger) ckex.getExchangeObject();
            if (DEBUG_KEY_EXCHANGE)
              {
                logger.log (Component.SSL_KEY_EXCHANGE, "SRP: client A: {0}", A);
              }
            try
              {
                out = new OutgoingMessage();
                out.writeMPI(A);
                in = new IncomingMessage(out.toByteArray());
                out = serverKA.processMessage(in);
                preMasterSecret = serverKA.getSharedSecret();
              }
            catch (KeyAgreementException x)
              {
                if (DEBUG_KEY_EXCHANGE)
                  {
                    logger.log (Component.SSL_KEY_EXCHANGE, "SRP exception", x);
                  }
                throwHandshakeFailure();
              }
            finally
              {
                serverKA = null;
              }
          }

        if (DEBUG_KEY_EXCHANGE)
          {
            logger.log (Component.SSL_KEY_EXCHANGE, "preMasterSecret:\n{0}",
                        Util.toHexString(preMasterSecret, ':'));
            logger.log (Component.SSL_KEY_EXCHANGE, "client.random:\n{0}",
                        Util.toHexString(clientRandom.getEncoded(), ':'));
            logger.log (Component.SSL_KEY_EXCHANGE, "server.random:\n{0}",
                        Util.toHexString(serverRandom.getEncoded(), ':'));
          }

        // Generate the master secret.
        IRandom genSecret = null;
        if (version == ProtocolVersion.SSL_3)
          {
            genSecret = new SSLRandom();
            HashMap attr = new HashMap();
            attr.put(SSLRandom.SECRET, preMasterSecret);
            attr.put(SSLRandom.SEED, Util.concat(clientRandom.getEncoded(),
                                                 serverRandom.getEncoded()));
            genSecret.init(attr);
          }
        else
          {
            genSecret = new TLSRandom();
            HashMap attr = new HashMap();
            attr.put(TLSRandom.SECRET, preMasterSecret);
            attr.put(TLSRandom.SEED,
                     Util.concat(("master secret").getBytes("UTF-8"),
                                 Util.concat(clientRandom.getEncoded(),
                                             serverRandom.getEncoded())));
            genSecret.init(attr);
          }
        session.masterSecret = new byte[48];
        try
          {
            genSecret.nextBytes(session.masterSecret, 0, 48);
            for (int i = 0; i < preMasterSecret.length; i++)
              {
                preMasterSecret[i] = 0;
              }
          }
        catch (LimitReachedException shouldNotHappen)
          {
            internalError();
            RuntimeException re = new RuntimeException();
            re.initCause (shouldNotHappen);
            throw re;
          }

        if (DEBUG_KEY_EXCHANGE)
          {
            logger.log (Component.SSL_KEY_EXCHANGE, "masterSecret: {0}",
                        Util.toHexString(session.masterSecret, ':'));
          }

        // Read the client's certificate verify message, if needed.
        if (clientCanSign && (wantClientAuth || needClientAuth))
          {
            msg = Handshake.read(din);
            if (msg.getType() != Handshake.Type.CERTIFICATE_VERIFY)
              {
                throwUnexpectedMessage();
              }
            CertificateVerify verify = (CertificateVerify) msg.getBody();
            if (clientChain != null && clientChain.length > 0)
              {
                IMessageDigest cvMD5 = (IMessageDigest) md5.clone();
                IMessageDigest cvSHA = (IMessageDigest) sha.clone();
                clientKey = clientChain[0].getPublicKey();
                if (clientKey instanceof RSAPublicKey)
                  {
                    SSLRSASignature sig = new SSLRSASignature(cvMD5, cvSHA);
                    sig.setupVerify(Collections.singletonMap(ISignature.VERIFIER_KEY, clientKey));
                    if (!sig.verify(verify.getSigValue()))
                      {
                        handshakeFailure();
                        throw new SSLHandshakeException("client certificate verify failed");
                      }
                  }
                else if (clientKey instanceof DSAPublicKey)
                  {
                    try
                      {
                        if (!DSSSignature.verify((DSAPublicKey) clientKey, cvSHA.digest(),
                                                 (BigInteger[]) verify.getSigValue()))
                          {
                            throw new Exception("client's certificate could not be verified");
                          }
                      }
                    catch (Exception x)
                      {
                        handshakeFailure();
                        SSLHandshakeException e = new SSLHandshakeException (x.getMessage());
                        e.initCause (x);
                        throw e;
                      }
                  }
              }
          }
      }

    // Generate the session keys.
    byte[][] keys = null;
    try
      {
        keys = generateKeys(serverRandom.getEncoded(),
                            clientRandom.getEncoded(), version);
      }
    catch (Exception x)
      {
        internalError();
        RuntimeException re = new RuntimeException (x.getMessage());
        re.initCause (x);
        throw re;
      }

    // Initialize the algorithms with the derived keys.
    Object readMac = null, writeMac = null;
    Object readCipher = null, writeCipher = null;
    try
      {
        if (session.params instanceof GNUSecurityParameters)
          {
            HashMap attr = new HashMap();
            writeMac = CipherSuite.getMac(suite.getMac());
            readMac  = CipherSuite.getMac(suite.getMac());
            attr.put(IMac.MAC_KEY_MATERIAL, keys[1]);
            ((IMac) writeMac).init(attr);
            attr.put(IMac.MAC_KEY_MATERIAL, keys[0]);
            ((IMac) readMac).init(attr);
            if (suite.getCipher() == "RC4")
              {
                writeCipher = new ARCFour();
                readCipher = new ARCFour();
                attr.clear();
                attr.put(ARCFour.ARCFOUR_KEY_MATERIAL, keys[3]);
                ((ARCFour) writeCipher).init(attr);
                attr.put(ARCFour.ARCFOUR_KEY_MATERIAL, keys[2]);
                ((ARCFour) readCipher).init(attr);
              }
            else if (!suite.isStreamCipher())
              {
                writeCipher = CipherSuite.getCipher(suite.getCipher());
                readCipher = CipherSuite.getCipher(suite.getCipher());
                attr.clear();
                attr.put(IMode.KEY_MATERIAL, keys[3]);
                attr.put(IMode.IV, keys[5]);
                attr.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
                ((IMode) writeCipher).init(attr);
                attr.put(IMode.KEY_MATERIAL, keys[2]);
                attr.put(IMode.IV, keys[4]);
                attr.put(IMode.STATE, new Integer(IMode.DECRYPTION));
                ((IMode) readCipher).init(attr);
              }
          }
        else // JCESecurityParameters
          {
            writeMac = CipherSuite.getJCEMac (suite.getMac());
            readMac = CipherSuite.getJCEMac (suite.getMac());
            writeCipher = CipherSuite.getJCECipher (suite.getCipher());
            readCipher = CipherSuite.getJCECipher (suite.getCipher());
            ((Mac) writeMac).init (new SecretKeySpec (keys[1], suite.getMac()));
            ((Mac) readMac).init (new SecretKeySpec (keys[0], suite.getMac()));
            if (!suite.isStreamCipher())
              {
                ((Cipher) writeCipher).init (Cipher.ENCRYPT_MODE,
                                             new SecretKeySpec (keys[3], suite.getCipher()),
                                             new IvParameterSpec (keys[5]));
                ((Cipher) readCipher).init (Cipher.DECRYPT_MODE,
                                            new SecretKeySpec (keys[2], suite.getCipher()),
                                            new IvParameterSpec (keys[4]));
              }
            else
              {
                ((Cipher) writeCipher).init (Cipher.ENCRYPT_MODE,
                                             new SecretKeySpec (keys[3], suite.getCipher()));
                ((Cipher) readCipher).init (Cipher.DECRYPT_MODE,
                                            new SecretKeySpec (keys[2], suite.getCipher()));
              }
          }
      }
    // These should technically never happen, if our key generation is not
    // broken.
    catch (InvalidKeyException ike)
      {
        internalError();
        RuntimeException re = new RuntimeException (ike.getMessage());
        re.initCause (ike);
        throw new RuntimeException (String.valueOf (ike));
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        internalError();
        RuntimeException re = new RuntimeException (iape.getMessage());
        re.initCause (iape);
        throw re;
      }
    // These indicate a configuration error with the JCA.
    catch (NoSuchAlgorithmException nsae)
      {
        session.enabledSuites.remove (suite);
        internalError();
        SSLException e = new SSLException ("suite " + suite + " not available in this configuration");
        e.initCause (nsae);
        throw e;
      }
    catch (NoSuchPaddingException nspe)
      {
        session.enabledSuites.remove (suite);
        internalError();
        SSLException e = new SSLException ("suite " + suite + " not available in this configuration");
        e.initCause (nspe);
        throw e;
      }

    Finished finis = null;
    // If we are continuing a session, we send our Finished message first.
    if (!newSession)
      {
        changeCipherSpec();
        session.params.setDeflating(comp == CompressionMethod.ZLIB);
        session.params.setOutMac(writeMac);
        session.params.setOutCipher(writeCipher);
        finis = generateFinished(version, (IMessageDigest) md5.clone(),
                                 (IMessageDigest) sha.clone(), false);
        msg = new Handshake(Handshake.Type.FINISHED, finis);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write(dout, version);
        dout.flush();
      }

    if (session.currentAlert != null &&
        session.currentAlert.getLevel() == Alert.Level.FATAL)
      {
        fatal();
        throw new AlertException(session.currentAlert, false);
      }

    // Wait until we receive a ChangeCipherSpec, then change the crypto
    // algorithms for the incoming side.
    synchronized (session.params)
      {
        readChangeCipherSpec ();
        session.params.setInflating(comp == CompressionMethod.ZLIB);
        session.params.setInMac(readMac);
        session.params.setInCipher(readCipher);
        session.params.notifyAll();
      }

    // Receive and verify the client's finished message.
    Finished verify = generateFinished(version, (IMessageDigest) md5.clone(),
                                       (IMessageDigest) sha.clone(), true);
    msg = Handshake.read(din, suite, null);
    if (msg.getType() != Handshake.Type.FINISHED)
      {
        throwUnexpectedMessage();
      }
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
    finis = (Finished) msg.getBody();
    if (version == ProtocolVersion.SSL_3)
      {
        if (!Arrays.equals(finis.getMD5Hash(), verify.getMD5Hash()) ||
            !Arrays.equals(finis.getSHAHash(), verify.getSHAHash()))
          {
            throwHandshakeFailure();
          }
      }
    else
      {
        if (!Arrays.equals(finis.getVerifyData(), verify.getVerifyData()))
          {
            throwHandshakeFailure();
          }
      }

    // Send our Finished message last for new sessions.
    if (newSession)
      {
        changeCipherSpec();
        session.params.setDeflating(comp == CompressionMethod.ZLIB);
        session.params.setOutMac(writeMac);
        session.params.setOutCipher(writeCipher);
        finis = generateFinished(version, md5, sha, false);
        msg = new Handshake(Handshake.Type.FINISHED, finis);
        if (DEBUG_HANDSHAKE_LAYER)
          logger.log (Component.SSL_HANDSHAKE, "{0}", msg);
        msg.write(dout, version);
        dout.flush();
      }

    handshakeCompleted();
  }

  /**
   * Generate the keys from the master secret.
   *
   * @param server The server's random value.
   * @param client The client's random value.
   * @param activeVersion The negotiated protocol version.
   * @return The generated keys.
   */
  private byte[][] generateKeys(byte[] server, byte[] client,
                                ProtocolVersion activeVersion)
    throws LimitReachedException, IOException
  {
    CipherSuite suite = session.cipherSuite;
    int macLen = (suite.getMac().indexOf("MD5") >= 0) ? 16 : 20;
    int keyLen = suite.getKeyLength();
    int ivLen = 0;
    if (suite.getCipher().indexOf("DES") >= 0)
      {
        ivLen = 8;
      }
    else if (suite.getCipher() == "AES")
      {
        ivLen = 16;
      }
    byte[][] keyMaterial = new byte[6][];
    keyMaterial[0] = new byte[macLen]; // client_write_MAC_secret
    keyMaterial[1] = new byte[macLen]; // server_write_MAC_secret
    keyMaterial[2] = new byte[keyLen]; // client_write_key
    keyMaterial[3] = new byte[keyLen]; // server_write_key
    keyMaterial[4] = new byte[ivLen];  // client_write_IV
    keyMaterial[5] = new byte[ivLen];  // server_write_IV
    IRandom prf = null;
    if (activeVersion == ProtocolVersion.SSL_3)
      {
        prf = new SSLRandom();
        HashMap attr = new HashMap();
        attr.put(SSLRandom.SECRET, session.masterSecret);
        attr.put(SSLRandom.SEED, Util.concat(server, client));
        prf.init(attr);
      }
    else
      {
        prf = new TLSRandom();
        HashMap attr = new HashMap();
        attr.put(TLSRandom.SECRET, session.masterSecret);
        attr.put(TLSRandom.SEED, Util.concat("key expansion".getBytes("UTF-8"),
                 Util.concat(server, client)));
        prf.init(attr);
      }
    for (int i = 0; i < keyMaterial.length; i++)
      {
        prf.nextBytes(keyMaterial[i], 0, keyMaterial[i].length);
      }

    // Exportable ciphers transform their keys once more, and use a
    // nonsecret IV for block ciphers.
    if (suite.isExportable())
      {
        int finalLen = suite.getCipher() == "DES" ? 8 : 16;
        if (activeVersion == ProtocolVersion.SSL_3)
          {
            IMessageDigest md5 = HashFactory.getInstance(Registry.MD5_HASH);
            md5.update(keyMaterial[2], 0, keyMaterial[2].length);
            md5.update(client, 0, client.length);
            md5.update(server, 0, server.length);
            keyMaterial[2] = Util.trim(md5.digest(), finalLen);
            md5.update(keyMaterial[3], 0, keyMaterial[3].length);
            md5.update(server, 0, server.length);
            md5.update(client, 0, client.length);
            keyMaterial[3] = Util.trim(md5.digest(), finalLen);
            if (!suite.isStreamCipher())
              {
                md5.update(client, 0, client.length);
                md5.update(server, 0, server.length);
                keyMaterial[4] = Util.trim(md5.digest(), ivLen);
                md5.update(server, 0, server.length);
                md5.update(client, 0, client.length);
                keyMaterial[5] = Util.trim(md5.digest(), ivLen);
              }
          }
        else
          {
            HashMap attr = new HashMap();
            attr.put(TLSRandom.SECRET, keyMaterial[2]);
            attr.put(TLSRandom.SEED,
                     Util.concat("client write key".getBytes("UTF-8"),
                                 Util.concat(client, server)));
            prf.init(attr);
            keyMaterial[2] = new byte[finalLen];
            prf.nextBytes(keyMaterial[2], 0, finalLen);
            attr.put(TLSRandom.SECRET, keyMaterial[3]);
            attr.put(TLSRandom.SEED,
                     Util.concat("server write key".getBytes("UTF-8"),
                                 Util.concat(client, server)));
            prf.init(attr);
            keyMaterial[3] = new byte[finalLen];
            prf.nextBytes(keyMaterial[3], 0, finalLen);
            if (!suite.isStreamCipher())
              {
                attr.put(TLSRandom.SECRET, new byte[0]);
                attr.put(TLSRandom.SEED, Util.concat("IV block".getBytes("UTF-8"),
                                                     Util.concat(client, server)));
                prf.init(attr);
                prf.nextBytes(keyMaterial[4], 0, keyMaterial[4].length);
                prf.nextBytes(keyMaterial[5], 0, keyMaterial[5].length);
              }
          }
      }

    if (DEBUG_KEY_EXCHANGE)
      {
        logger.log (Component.SSL_KEY_EXCHANGE, "Generated keys:");
        for (int i = 0; i < keyMaterial.length; i++)
          logger.log (Component.SSL_KEY_EXCHANGE, "[{0}] {1}",
                      new Object[] { new Integer (i),
                                     Util.toHexString(keyMaterial[i], ':') });
      }

    return keyMaterial;
  }

  /**
   * Generate a "finished" message, based on the hashes of the handshake
   * messages, the agreed version, and a label.
   *
   * @param version The agreed version.
   * @param md5 The current state of the handshake MD5 hash.
   * @param sha The current state of the handshake SHA hash.
   * @param client Should be true if the message is generated by the client.
   */
  private Finished generateFinished(ProtocolVersion version, IMessageDigest md5,
                                    IMessageDigest sha, boolean client)
  {
    if (version == ProtocolVersion.SSL_3)
      {
        if (client)
          {
            md5.update(SENDER_CLIENT, 0, 4);
          }
        else
          {
            md5.update(SENDER_SERVER, 0, 4);
          }
        byte[] ms = session.masterSecret;
        md5.update(ms, 0, ms.length);
        for (int i = 0; i < 48; i++)
          {
            md5.update(SSLHMac.PAD1);
          }
        byte[] b = md5.digest();
        md5.update(ms, 0, ms.length);
        for (int i = 0; i < 48; i++)
          {
            md5.update(SSLHMac.PAD2);
          }
        md5.update(b, 0, b.length);

        if (client)
          {
            sha.update(SENDER_CLIENT, 0, 4);
          }
        else
          {
            sha.update(SENDER_SERVER, 0, 4);
          }
        sha.update(ms, 0, ms.length);
        for (int i = 0; i < 40; i++)
          {
            sha.update(SSLHMac.PAD1);
          }
        b = sha.digest();
        sha.update(ms, 0, ms.length);
        for (int i = 0; i < 40; i++)
          {
            sha.update(SSLHMac.PAD2);
          }
        sha.update(b, 0, b.length);
        return new Finished(md5.digest(), sha.digest());
      }
    else
      {
        byte[] h1 = md5.digest();
        byte[] h2 = sha.digest();
        String label = client ? "client finished" : "server finished";
        byte[] seed = null;
        try
          {
            seed = Util.concat(label.getBytes("UTF-8"), Util.concat(h1, h2));
          }
        catch (java.io.UnsupportedEncodingException uee)
          {
            RuntimeException re = new RuntimeException (uee.getMessage());
            re.initCause (uee);
            throw re;
          }
        IRandom prf = new TLSRandom();
        HashMap attr = new HashMap();
        attr.put(TLSRandom.SECRET, session.masterSecret);
        attr.put(TLSRandom.SEED, seed);
        prf.init(attr);
        byte[] finishedValue = new byte[12];
        try
          {
            prf.nextBytes(finishedValue, 0, 12);
          }
        catch (LimitReachedException lre)
          {
            RuntimeException re = new RuntimeException (lre.getMessage());
            re.initCause (lre);
            throw re;
          }
        return new Finished(finishedValue);
      }
  }

  /**
   * Send a fatal unexpected_message alert.
   */
  private Alert unexpectedMessage() throws IOException
  {
    Alert alert = new Alert(Alert.Level.FATAL,
      Alert.Description.UNEXPECTED_MESSAGE);
    sendAlert(alert);
    fatal();
    return alert;
  }

  private void throwUnexpectedMessage() throws IOException
  {
    throw new AlertException(unexpectedMessage(), true);
  }

  /**
   * Send a fatal handshake_failure alert.
   */
  private Alert handshakeFailure() throws IOException
  {
    Alert alert = new Alert(Alert.Level.FATAL,
      Alert.Description.HANDSHAKE_FAILURE);
    sendAlert(alert);
    fatal();
    return alert;
  }

  private void throwHandshakeFailure() throws IOException
  {
    throw new AlertException(handshakeFailure(), true);
  }

  /**
   * Send an internal_error alert.
   */
  private Alert internalError() throws IOException
  {
    Alert alert = new Alert(Alert.Level.FATAL,
      Alert.Description.INTERNAL_ERROR);
    sendAlert(alert);
    fatal();
    return alert;
  }

  private void throwInternalError() throws IOException
  {
    throw new AlertException(internalError(), true);
  }

  private Alert peerUnverified(X509Certificate[] chain) throws IOException
  {
    Alert alert = new Alert(Alert.Level.FATAL,
      Alert.Description.HANDSHAKE_FAILURE);
    sendAlert(alert);
    fatal();
    return alert;
  }

  private void throwPeerUnverified(X509Certificate[] chain) throws IOException
  {
    peerUnverified (chain);
    throw new SSLPeerUnverifiedException("could not verify: "+
                                         chain[0].getSubjectDN());
  }

  /**
   * Grab the first suite that is both in the client's requested suites
   * and in our enabled suites, and for which we have the proper
   * credentials.
   *
   * @param suites The client's requested suites.
   * @param version The version being negotiated.
   * @return The selected cipher suite.
   * @throws SSLException If no appropriate suite can be selected.
   */
  private CipherSuite selectSuite(List suites, ProtocolVersion version)
    throws IOException
  {
    if (DEBUG_HANDSHAKE_LAYER)
      logger.log (Component.SSL_HANDSHAKE, "selectSuite req:{0} suites:{1}",
                  new Object[] { suites, session.enabledSuites });
    boolean srpSuiteNoUser = false;
    for (Iterator i = suites.iterator(); i.hasNext(); )
      {
        CipherSuite herSuite = (CipherSuite) i.next();
        for (Iterator j = session.enabledSuites.iterator(); j.hasNext(); )
          {
            CipherSuite mySuite = (CipherSuite) j.next();
            if (!mySuite.equals(herSuite))
              {
                continue;
              }
            if (DEBUG_HANDSHAKE_LAYER)
              logger.log (Component.SSL_HANDSHAKE, "{0} == {1}",
                          new Object[] { mySuite, herSuite });
            if (mySuite.getSignature() != "anon" && session.keyManager != null &&
                session.keyManager.chooseServerAlias(mySuite.getAuthType(), null, null) == null)
              {
                if (DEBUG_HANDSHAKE_LAYER)
                  logger.log (Component.SSL_HANDSHAKE, "{0}: no certificate/private key",
                              mySuite);
                continue;
              }
            if (mySuite.getKeyExchange() == "SRP")
              {
                if (session.getValue("srp-username") == null)
                  {
                    if (DEBUG_HANDSHAKE_LAYER)
                      logger.log (Component.SSL_HANDSHAKE, "no SRP username");
                    srpSuiteNoUser = true;
                    continue;
                  }
                if (session.srpTrustManager == null)
                  {
                    if (DEBUG_HANDSHAKE_LAYER)
                      logger.log (Component.SSL_HANDSHAKE, "no SRP password file");
                    continue;
                  }
              }
            return mySuite.resolve(version);
          }
      }
    Alert alert = null;
    if (srpSuiteNoUser)
      {
        alert = new Alert(Alert.Level.WARNING,
                          Alert.Description.MISSING_SRP_USERNAME);
        sendAlert(alert);
        return null;
      }
    else
      alert = new Alert(Alert.Level.FATAL,
                        Alert.Description.INSUFFICIENT_SECURITY);
    sendAlert(alert);
    fatal();
    throw new AlertException(alert, true);
  }

  /**
   * Ask the user for their user name.
   *
   * @param remoteHost The remote host being connected to.
   * @return The user name.
   */
  private String askUserName(String remoteHost)
  {
    CallbackHandler handler = new DefaultCallbackHandler();
    try
      {
        Class c = Class.forName(Util.getSecurityProperty("jessie.srp.user.handler"));
        handler = (CallbackHandler) c.newInstance();
      }
    catch (Exception x) { }
    TextInputCallback user =
      new TextInputCallback("User name for " + remoteHost + ": ",
                            Util.getProperty("user.name"));
    try
      {
        handler.handle(new Callback[] { user });
      }
    catch (Exception x) { }
    return user.getText();
  }

  /**
   * Ask the user for a password.
   *
   * @param user The user name.
   * @return The password.
   */
  private String askPassword(String user)
  {
    CallbackHandler handler = new DefaultCallbackHandler();
    try
      {
        Class c = Class.forName(Util.getSecurityProperty("jessie.srp.password.handler"));
        handler = (CallbackHandler) c.newInstance();
      }
    catch (Exception x) { }
    PasswordCallback passwd = new PasswordCallback(user + "'s password: ", false);
    try
      {
        handler.handle(new Callback[] { passwd });
      }
    catch (Exception x) { }
    return new String(passwd.getPassword());
  }

  /**
   * Ask the user (via a callback) if they will accept a certificate that
   * could not be verified.
   *
   * @param chain The certificate chain in question.
   * @return true if the user accepts the certificate chain.
   */
  private boolean checkCertificates(X509Certificate[] chain)
  {
    CallbackHandler handler = new DefaultCallbackHandler();
    try
      {
        Class c = Class.forName(Util.getSecurityProperty("jessie.certificate.handler"));
        handler = (CallbackHandler) c.newInstance();
      }
    catch (Exception x)
      {
      }
    String nl = Util.getProperty("line.separator");
    ConfirmationCallback confirm = new ConfirmationCallback(
      "The server's certificate could not be verified. There is no proof" + nl +
      "that this server is who it claims to be, or that their certificate" + nl +
      "is valid. Do you wish to continue connecting?",
      ConfirmationCallback.ERROR, ConfirmationCallback.YES_NO_OPTION,
      ConfirmationCallback.NO);
    try
      {
        handler.handle(new Callback[] { confirm });
      }
    catch (Exception x)
      {
        return false;
      }
    return confirm.getSelectedIndex() == ConfirmationCallback.YES;
  }

  /**
   * Update a signature object with a BigInteger, trimming the leading
   * "00" octet if present.
   *
   * @param sig The signature being updated.
   * @param bi  The integer to feed into the signature.
   */
  private void updateSig(ISignature sig, BigInteger bi)
  {
    byte[] buf = Util.trim(bi);
    sig.update((byte) (buf.length >>> 8));
    sig.update((byte)  buf.length);
    sig.update(buf, 0, buf.length);
  }

  /**
   * Teardown everything on fatal errors.
   */
  private void fatal() throws IOException
  {
    if (session != null)
      {
        session.invalidate();
      }
//     recordInput.setRunning(false);
//     recordOutput.setRunning(false);
    if (underlyingSocket != null)
      {
        underlyingSocket.close();
      }
    else
      {
        super.close();
      }
  }
}
