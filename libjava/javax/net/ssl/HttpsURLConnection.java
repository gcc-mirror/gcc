/* HttpsURLConnection.java -- an HTTPS connection.
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


package javax.net.ssl;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.cert.Certificate;

/**
 * A URL connection that connects via the <i>Secure Socket Layer</i>
 * (<b>SSL</b>) for HTTPS connections.
 *
 * <p>This class may be used in the same way as {@link
 * HttpURLConnection}, and it will transparently negotiate the SSL
 * connection.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public abstract class HttpsURLConnection extends HttpURLConnection
{

  // Fields.
  // ------------------------------------------------------------------

  /** The default verifier. */
  private static HostnameVerifier defaultVerifier;

  /** The default factory. */
  private static SSLSocketFactory defaultFactory;

  /**
   * The hostname verifier used for this connection.
   */
  protected HostnameVerifier hostnameVerifier;

  /**
   * This connection's socket factory.
   */
  private SSLSocketFactory factory;

  // Static initializer.
  // ------------------------------------------------------------------

  static {
    defaultVerifier = new TrivialHostnameVerifier();
    try
      {
        defaultFactory = (SSLSocketFactory) SSLSocketFactory.getDefault();
      }
    catch (Throwable t)
      {
        t.printStackTrace();
      }
  }

  // Constructor.
  // ------------------------------------------------------------------

  /**
   * Creates a new HTTPS URL connection.
   *
   * @param url The URL of the connection being established.
   * @throws IOException If the connection cannot be established.
   */
  protected HttpsURLConnection(URL url) throws IOException
  {
    super(url);
    hostnameVerifier = defaultVerifier;
    factory = defaultFactory;
  }

  // Class methods.
  // ------------------------------------------------------------------

  /**
   * Returns the default hostname verifier used in all new
   * connections.
   *
   * @return The default hostname verifier.
   */
  public static HostnameVerifier getDefaultHostnameVerifier()
  {
    return defaultVerifier;
  }

  /**
   * Sets the default hostname verifier to be used in all new
   * connections.
   *
   * @param newDefault The new default hostname verifier.
   * @throws IllegalArgumentException If <i>newDefault</i> is null.
   * @throws SecurityException If there is a security manager
   *   currently installed and the caller does not have the {@link
   *   SSLPermission} "setHostnameVerifier".
   */
  public static void setDefaultHostnameVerifier(HostnameVerifier newDefault)
  {
    if (newDefault == null)
      throw new IllegalArgumentException("default verifier cannot be null");
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SSLPermission("setHostnameVerifier"));
    defaultVerifier = newDefault;
  }

  /**
   * Returns the default SSL socket factory used in all new
   * connections.
   *
   * @return The default SSL socket factory.
   */
  public static SSLSocketFactory getDefaultSSLSocketFactory()
  {
    return defaultFactory;
  }

  /**
   * Sets the default SSL socket factory to be used in all new
   * connections.
   *
   * @param newDefault The new socket factory.
   * @throws IllegalArgumentException If <i>newDefault</i> is null.
   * @throws SecurityException If there is a security manager
   *   installed and a call to {@link
   *   SecurityManager#checkSetFactory()} fails.
   */
  public static void setDefaultSSLSocketFactory(SSLSocketFactory newDefault)
  {
    if (newDefault == null)
      throw new IllegalArgumentException("default factory cannot be null");
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSetFactory();
    defaultFactory = newDefault;
  }

  // Instance methods.
  // ------------------------------------------------------------------

  /**
   * Returns the current hostname verifier for this instance.
   *
   * @return The hostname verifier.
   */
  public HostnameVerifier getHostnameVerifier()
  {
    return hostnameVerifier;
  }

  /**
   * Sets the hostname verifier for this instance.
   *
   * @param hostnameVerifier The new verifier.
   * @throws IllegalArgumentException If <i>hostnameVerifier</i> is
   *   null.
   */
  public void setHostnameVerifier(HostnameVerifier hostnameVerifier)
  {
    if (hostnameVerifier == null)
      throw new IllegalArgumentException("verifier cannot be null");
    this.hostnameVerifier = hostnameVerifier;
  }

  /**
   * Returns the current SSL socket factory for this instance.
   *
   * @return The current SSL socket factory.
   */
  public SSLSocketFactory getSSLSocketFactory()
  {
    return factory;
  }

  /**
   * Sets the SSL socket factory for this instance.
   *
   * @param factory The new factory.
   * @throws IllegalArgumentException If <i>factory</i> is null.
   */
  public void setSSLSocketFactory(SSLSocketFactory factory)
  {
    if (factory == null)
      throw new IllegalArgumentException("factory cannot be null");
    this.factory = factory;
  }

  // Abstract methods.
  // -------------------------------------------------------------------

  /**
   * Returns the cipher name negotiated for this connection.
   *
   * @return The cipher name.
   * @throws IllegalStateException If the connection has not yet been
   *   established.
   */
  public abstract String getCipherSuite();

  /**
   * Returns the certificates used on the local side in this
   * connection.
   *
   * @return The local certificates.
   * @throws IllegalStateException If the connection has not yet been
   *  established.
   */
  public abstract Certificate[] getLocalCertificates();

  /**
   * Returns the certificates sent by the other party.
   *
   * @return The peer's certificates.
   * @throws IllegalStateException If the connection has not yet been
   *   established.
   * @throws SSLPeerUnverifiedException If the peer could not be
   *   verified.
   */
  public abstract Certificate[] getServerCertificates() throws SSLPeerUnverifiedException;
}
