/* HTTPConnection.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.net.protocol.http;

import gnu.classpath.Configuration;
import gnu.classpath.SystemProperties;
import gnu.java.net.EmptyX509TrustManager;
import gnu.java.net.protocol.http.event.ConnectionEvent;
import gnu.java.net.protocol.http.event.ConnectionListener;
import gnu.java.net.protocol.http.event.RequestEvent;
import gnu.java.net.protocol.http.event.RequestListener;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.net.ssl.HandshakeCompletedListener;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

/**
 * A connection to an HTTP server.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class HTTPConnection
{

  /**
   * The default HTTP port.
   */
  public static final int HTTP_PORT = 80;

  /**
   * The default HTTPS port.
   */
  public static final int HTTPS_PORT = 443;

  private static final String userAgent = SystemProperties.getProperty("http.agent");

  /**
   * The host name of the server to connect to.
   */
  protected final String hostname;

  /**
   * The port to connect to.
   */
  protected final int port;

  /**
   * Whether the connection should use transport level security (HTTPS).
   */
  protected final boolean secure;

  /**
   * The connection timeout for connecting the underlying socket.
   */
  protected final int connectionTimeout;

  /**
   * The read timeout for reads on the underlying socket.
   */
  protected final int timeout;

  /**
   * The host name of the proxy to connect to.
   */
  protected String proxyHostname;

  /**
   * The port on the proxy to connect to.
   */
  protected int proxyPort;

  /**
   * The major version of HTTP supported by this client.
   */
  protected int majorVersion;

  /**
   * The minor version of HTTP supported by this client.
   */
  protected int minorVersion;

  private final List connectionListeners;
  private final List requestListeners;
  private final List handshakeCompletedListeners;

  /**
   * The socket this connection communicates on.
   */
  protected Socket socket;

  /**
   * The SSL socket factory to use.
   */
  private SSLSocketFactory sslSocketFactory;

  /**
   * The socket input stream.
   */
  protected InputStream in;

  /**
   * The socket output stream.
   */
  protected OutputStream out;

  /**
   * Nonce values seen by this connection.
   */
  private Map nonceCounts;

  /**
   * The cookie manager for this connection.
   */
  protected CookieManager cookieManager;

  /**
   * Creates a new HTTP connection.
   * @param hostname the name of the host to connect to
   */
  public HTTPConnection(String hostname)
  {
    this(hostname, HTTP_PORT, false, 0, 0);
  }

  /**
   * Creates a new HTTP or HTTPS connection.
   * @param hostname the name of the host to connect to
   * @param secure whether to use a secure connection
   */
  public HTTPConnection(String hostname, boolean secure)
  {
    this(hostname, secure ? HTTPS_PORT : HTTP_PORT, secure, 0, 0);
  }

  /**
   * Creates a new HTTP or HTTPS connection on the specified port.
   * @param hostname the name of the host to connect to
   * @param secure whether to use a secure connection
   * @param connectionTimeout the connection timeout
   * @param timeout the socket read timeout
   */
  public HTTPConnection(String hostname, boolean secure,
                        int connectionTimeout, int timeout)
  {
    this(hostname, secure ? HTTPS_PORT : HTTP_PORT, secure,
         connectionTimeout, timeout);
  }
  
  /**
   * Creates a new HTTP connection on the specified port.
   * @param hostname the name of the host to connect to
   * @param port the port on the host to connect to
   */
  public HTTPConnection(String hostname, int port)
  {
    this(hostname, port, false, 0, 0);
  }

  /**
   * Creates a new HTTP or HTTPS connection on the specified port.
   * @param hostname the name of the host to connect to
   * @param port the port on the host to connect to
   * @param secure whether to use a secure connection
   */
  public HTTPConnection(String hostname, int port, boolean secure)
  {
    this(hostname, port, secure, 0, 0);
  }
  
  /**
   * Creates a new HTTP or HTTPS connection on the specified port.
   * @param hostname the name of the host to connect to
   * @param port the port on the host to connect to
   * @param secure whether to use a secure connection
   * @param connectionTimeout the connection timeout
   * @param timeout the socket read timeout
   */
  public HTTPConnection(String hostname, int port, boolean secure,
                        int connectionTimeout, int timeout)
  {
    this.hostname = hostname;
    this.port = port;
    this.secure = secure;
    this.connectionTimeout = connectionTimeout;
    this.timeout = timeout;
    majorVersion = minorVersion = 1;
    connectionListeners = new ArrayList(4);
    requestListeners = new ArrayList(4);
    handshakeCompletedListeners = new ArrayList(2);
  }

  /**
   * Returns the name of the host to connect to.
   */
  public String getHostName()
  {
    return hostname;
  }

  /**
   * Returns the port on the host to connect to.
   */
  public int getPort()
  {
    return port;
  }

  /**
   * Indicates whether to use a secure connection or not.
   */
  public boolean isSecure()
  {
    return secure;
  }

  /**
   * Returns the HTTP version string supported by this connection.
   * @see #version
   */
  public String getVersion()
  {
    return "HTTP/" + majorVersion + '.' + minorVersion;
  }

  /**
   * Sets the HTTP version supported by this connection.
   * @param majorVersion the major version
   * @param minorVersion the minor version
   */
  public void setVersion(int majorVersion, int minorVersion)
  {
    if (majorVersion != 1)
      {
        throw new IllegalArgumentException("major version not supported: " +
                                           majorVersion);
      }
    if (minorVersion < 0 || minorVersion > 1)
      {
        throw new IllegalArgumentException("minor version not supported: " +
                                           minorVersion);
      }
    this.majorVersion = majorVersion;
    this.minorVersion = minorVersion;
  }

  /**
   * Directs this connection to use the specified proxy.
   * @param hostname the proxy host name
   * @param port the port on the proxy to connect to
   */
  public void setProxy(String hostname, int port)
  {
    proxyHostname = hostname;
    proxyPort = port;
  }

  /**
   * Indicates whether this connection is using an HTTP proxy.
   */
  public boolean isUsingProxy()
  {
    return (proxyHostname != null && proxyPort > 0);
  }

  /**
   * Sets the cookie manager to use for this connection.
   * @param cookieManager the cookie manager
   */
  public void setCookieManager(CookieManager cookieManager)
  {
    this.cookieManager = cookieManager;
  }

  /**
   * Returns the cookie manager in use for this connection.
   */
  public CookieManager getCookieManager()
  {
    return cookieManager;
  }

  /**
   * Creates a new request using this connection.
   * @param method the HTTP method to invoke
   * @param path the URI-escaped RFC2396 <code>abs_path</code> with
   * optional query part
   */
  public Request newRequest(String method, String path)
  {
    if (method == null || method.length() == 0)
      {
        throw new IllegalArgumentException("method must have non-zero length");
      }
    if (path == null || path.length() == 0)
      {
        path = "/";
      }
    Request ret = new Request(this, method, path);
    if ((secure && port != HTTPS_PORT) ||
        (!secure && port != HTTP_PORT))
      {
        ret.setHeader("Host", hostname + ":" + port);
      }
    else
      {
        ret.setHeader("Host", hostname);
      }
    ret.setHeader("User-Agent", userAgent);
    ret.setHeader("Connection", "keep-alive");
    ret.setHeader("Accept-Encoding",
                  "chunked;q=1.0, gzip;q=0.9, deflate;q=0.8, " +
                  "identity;q=0.6, *;q=0");
    if (cookieManager != null)
      {
        Cookie[] cookies = cookieManager.getCookies(hostname, secure, path);
        if (cookies != null && cookies.length > 0)
          {
            StringBuffer buf = new StringBuffer();
            buf.append("$Version=1");
            for (int i = 0; i < cookies.length; i++)
              {
                buf.append(',');
                buf.append(' ');
                buf.append(cookies[i].toString());
              }
            ret.setHeader("Cookie", buf.toString());
          }
      }
    fireRequestEvent(RequestEvent.REQUEST_CREATED, ret);
    return ret;
  }

  /**
   * Closes this connection.
   */
  public void close()
    throws IOException
  {
    try
      {
        closeConnection();
      }
    finally
      {
        fireConnectionEvent(ConnectionEvent.CONNECTION_CLOSED);
      }
  }

  /**
   * Retrieves the socket associated with this connection.
   * This creates the socket if necessary.
   */
  protected Socket getSocket()
    throws IOException
  {
    if (socket == null)
      {
        String connectHostname = hostname;
        int connectPort = port;
        if (isUsingProxy())
          {
            connectHostname = proxyHostname;
            connectPort = proxyPort;
          }
        socket = new Socket();
        InetSocketAddress address =
          new InetSocketAddress(connectHostname, connectPort);
        if (connectionTimeout > 0)
          {
            socket.connect(address, connectionTimeout);
          }
        else
          {
            socket.connect(address);
          }
        if (timeout > 0)
          {
            socket.setSoTimeout(timeout);
          }
        if (secure)
          {
            try
              {
                SSLSocketFactory factory = getSSLSocketFactory();
                SSLSocket ss =
                  (SSLSocket) factory.createSocket(socket, connectHostname,
                                                   connectPort, true);
                String[] protocols = { "TLSv1", "SSLv3" };
                ss.setEnabledProtocols(protocols);
                ss.setUseClientMode(true);
                synchronized (handshakeCompletedListeners)
                  {
                    if (!handshakeCompletedListeners.isEmpty())
                      {
                        for (Iterator i =
                             handshakeCompletedListeners.iterator();
                             i.hasNext(); )
                          {
                            HandshakeCompletedListener l =
                              (HandshakeCompletedListener) i.next();
                            ss.addHandshakeCompletedListener(l);
                          }
                      }
                  }
                ss.startHandshake();
                socket = ss;
              }
            catch (GeneralSecurityException e)
              {
                throw new IOException(e.getMessage());
              }
          }
        in = socket.getInputStream();
        in = new BufferedInputStream(in);
        out = socket.getOutputStream();
        out = new BufferedOutputStream(out);
      }
    return socket;
  }

  SSLSocketFactory getSSLSocketFactory()
    throws GeneralSecurityException
  {
    if (sslSocketFactory == null)
      {
        TrustManager tm = new EmptyX509TrustManager();
        SSLContext context = SSLContext.getInstance("SSL");
        TrustManager[] trust = new TrustManager[] { tm };
        context.init(null, trust, null);
        sslSocketFactory = context.getSocketFactory();
      }
    return sslSocketFactory;
  }

  void setSSLSocketFactory(SSLSocketFactory factory)
  {
    sslSocketFactory = factory;
  }

  protected InputStream getInputStream()
    throws IOException
  {
    if (socket == null)
      {
        getSocket();
      }
    return in;
  }

  protected OutputStream getOutputStream()
    throws IOException
  {
    if (socket == null)
      {
        getSocket();
      }
    return out;
  }

  /**
   * Closes the underlying socket, if any.
   */
  protected void closeConnection()
    throws IOException
  {
    if (socket != null)
      {
        try
          {
            socket.close();
          }
        finally
          {
            socket = null;
          }
      }
  }

  /**
   * Returns a URI representing the connection.
   * This does not include any request path component.
   */
  protected String getURI()
  {
    StringBuffer buf = new StringBuffer();
    buf.append(secure ? "https://" : "http://");
    buf.append(hostname);
    if (secure)
      {
        if (port != HTTPConnection.HTTPS_PORT)
          {
            buf.append(':');
            buf.append(port);
          }
      }
    else
      {
        if (port != HTTPConnection.HTTP_PORT)
          {
            buf.append(':');
            buf.append(port);
          }
      }
    return buf.toString();
  }

  /**
   * Get the number of times the specified nonce has been seen by this
   * connection.
   */
  int getNonceCount(String nonce)
  {
    if (nonceCounts == null)
      {
        return 0;
      }
    return((Integer) nonceCounts.get(nonce)).intValue();
  }

  /**
   * Increment the number of times the specified nonce has been seen.
   */
  void incrementNonce(String nonce)
  {
    int current = getNonceCount(nonce);
    if (nonceCounts == null)
      {
        nonceCounts = new HashMap();
      }
    nonceCounts.put(nonce, new Integer(current + 1));
  }

  // -- Events --
  
  public void addConnectionListener(ConnectionListener l)
  {
    synchronized (connectionListeners)
      {
        connectionListeners.add(l);
      }
  }

  public void removeConnectionListener(ConnectionListener l)
  {
    synchronized (connectionListeners)
      {
        connectionListeners.remove(l);
      }
  }

  protected void fireConnectionEvent(int type)
  {
    ConnectionEvent event = new ConnectionEvent(this, type);
    ConnectionListener[] l = null;
    synchronized (connectionListeners)
      {
        l = new ConnectionListener[connectionListeners.size()];
        connectionListeners.toArray(l);
      }
    for (int i = 0; i < l.length; i++)
      {
        switch (type)
          {
          case ConnectionEvent.CONNECTION_CLOSED:
            l[i].connectionClosed(event);
            break;
          }
      }
  }

  public void addRequestListener(RequestListener l)
  {
    synchronized (requestListeners)
      {
        requestListeners.add(l);
      }
  }

  public void removeRequestListener(RequestListener l)
  {
    synchronized (requestListeners)
      {
        requestListeners.remove(l);
      }
  }

  protected void fireRequestEvent(int type, Request request)
  {
    RequestEvent event = new RequestEvent(this, type, request);
    RequestListener[] l = null;
    synchronized (requestListeners)
      {
        l = new RequestListener[requestListeners.size()];
        requestListeners.toArray(l);
      }
    for (int i = 0; i < l.length; i++)
      {
        switch (type)
          {
          case RequestEvent.REQUEST_CREATED:
            l[i].requestCreated(event);
            break;
          case RequestEvent.REQUEST_SENDING:
            l[i].requestSent(event);
            break;
          case RequestEvent.REQUEST_SENT:
            l[i].requestSent(event);
            break;
          }
      }
  }

  void addHandshakeCompletedListener(HandshakeCompletedListener l)
  {
    synchronized (handshakeCompletedListeners)
      {
        handshakeCompletedListeners.add(l);
      }
  }
  void removeHandshakeCompletedListener(HandshakeCompletedListener l)
  {
    synchronized (handshakeCompletedListeners)
      {
        handshakeCompletedListeners.remove(l);
      }
  }

}

