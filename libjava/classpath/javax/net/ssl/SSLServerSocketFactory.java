/* SSLServerSocketFactory.java -- factory for SSL server sockets.
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


package javax.net.ssl;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.security.KeyStore;
import java.security.Security;

import javax.net.ServerSocketFactory;

/**
 * A server socket factory for <i>Secure Socket Layer</i> (<b>SSL</b>)
 * server sockets.
 */
public abstract class SSLServerSocketFactory extends ServerSocketFactory
{
  // Field.
  // -------------------------------------------------------------------------

  private static SSLContext context;

  // Constructor.
  // -------------------------------------------------------------------------

  protected SSLServerSocketFactory()
  {
    super();
  }

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * Returns a default implementation of a SSL server socket factory.
   *
   * <p>To control the class that gets returned by this method, set the
   * security property "ssl.ServerSocketFactory.provider" to the class
   * name of a concrete implementation of this class. If not set, a
   * system-dependent implementation will be used.</p>
   *
   * <p>The implementation returned is created by the first implementation
   * of the {@link SSLContext} class found, which is initialized with
   * default parameters. To control the key and trust manager factory
   * algorithms used as defaults, set the security properties
   * "ssl.keyManagerFactory.algorithm" and "ssl.trustManagerFactory.algorithm"
   * to the appropriate names.</p>
   *
   * <p>Using this method is not recommended. Instead, use the methods of
   * {@link SSLContext}, which provide much better control over the
   * creation of server socket factories.</p>
   *
   * @return The default server socket factory.
   * @throws RuntimeException If no default can be created.
   */
  public static synchronized ServerSocketFactory getDefault()
  {
    try
      {
        String s = Security.getProperty("ssl.ServerSocketFactory.provider");
        ClassLoader cl = ClassLoader.getSystemClassLoader();
        if (s != null && cl != null)
          {
            return (ServerSocketFactory) cl.loadClass(s).newInstance();
          }
      }
    catch (Exception e)
      {
      }
    if (context == null)
      {
        KeyManager[] km = null;
        TrustManager[] tm = null;

        // 1. Determine which algorithms to use for the key and trust
        // manager factories.
        String kmAlg = KeyManagerFactory.getDefaultAlgorithm();
        String tmAlg = TrustManagerFactory.getDefaultAlgorithm();
        // 2. Try to initialize the factories with default parameters.
        try
          {
            KeyManagerFactory kmf = KeyManagerFactory.getInstance(kmAlg);
            kmf.init(null, null);
            km = kmf.getKeyManagers();
          }
        catch (Exception ex)
          {
          }
        try
          {
            TrustManagerFactory tmf = TrustManagerFactory.getInstance(tmAlg);
            tmf.init((KeyStore) null);
            tm = tmf.getTrustManagers();
          }
        catch (Exception ex)
          {
          }

        // 3. Create and initialize a context.
        try
          {
            context = SSLContext.getInstance("SSLv3");
            context.init(km, tm, null);
          }
        catch (Exception ex)
          {
            return new ErrorServerSocketFactory(new RuntimeException(
                "error instantiating default server socket factory: "
                                       + ex.toString(), ex));
          }
      }
    try
      {
        return context.getServerSocketFactory();
      }
    catch (Exception e)
      {
      }
    return new ErrorServerSocketFactory(new RuntimeException(
        "no SSLSocketFactory implementation available"));
  }

  private static final class ErrorServerSocketFactory
    extends SSLServerSocketFactory
  {
    private RuntimeException x;

    ErrorServerSocketFactory(RuntimeException x)
    {
      this.x = x;
    }

    public ServerSocket createServerSocket() throws IOException
    {
      throw (IOException) new IOException().initCause(x);
    }

    public ServerSocket createServerSocket(int port) throws IOException
    {
      throw (IOException) new IOException().initCause(x);
    }

    public ServerSocket createServerSocket(int port, int backlog)
      throws IOException
    {
      throw (IOException) new IOException().initCause(x);
    }

    public ServerSocket createServerSocket(int port, int backlog,
                                           InetAddress ifAddress)
      throws IOException
    {
      throw (IOException) new IOException().initCause(x);
    }

    public String[] getDefaultCipherSuites()
    {
      throw new RuntimeException(x);
    }

    public String[] getSupportedCipherSuites()
    {
      throw new RuntimeException(x);
    }
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the list of cipher suites that will be enabled in server sockets
   * created by this factory.
   *
   * @return The default cipher suites.
   */
  public abstract String[] getDefaultCipherSuites();

  /**
   * Returns the list of all cipher suites supported by this factory.
   *
   * @return The list of supported cipher suites.
   */
  public abstract String[] getSupportedCipherSuites();
}
