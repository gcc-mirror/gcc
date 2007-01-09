/* SSLContext.java -- an SSL protocol context.
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

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;

/**
 * A "meta-factory" for protocol-specific socket and server socket
 * factories. This class serves as a clearinghouse for socket
 * factories and cached session contexts for a particular protocol,
 * such as SSLv3.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class SSLContext
{
  // Constants and fields.
  // ------------------------------------------------------------------

  /** Service name for SSL contexts. */
  private static final String SSL_CONTEXT = "SSLContext";

  /** The underlying engine. */
  private final SSLContextSpi ctxSpi;

  /** The provider of the engine class. */
  private final Provider provider;

  /** The protocal name. */
  private final String protocol;

  // Constructor.
  // ------------------------------------------------------------------

  /**
   * Create a new SSL context.
   *
   * @param ctxSpi The context engine.
   * @param provider The provider of the implementation.
   * @param protocol The name of the SSL protocol.
   */
  protected SSLContext(SSLContextSpi ctxSpi, Provider provider,
                       String protocol)
  {
    this.ctxSpi = ctxSpi;
    this.provider = provider;
    this.protocol = protocol;
  }

  /**
   * Get an instance of a context for the specified protocol from the first
   * provider that implements it.
   * 
   * @param protocol The name of the protocol to get a context for.
   * @return The new context.
   * @throws NoSuchAlgorithmException If no provider implements the given
   *           protocol.
   * @throws IllegalArgumentException if <code>protocol</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final SSLContext getInstance(String protocol)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(protocol, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(protocol);
  }

  /**
   * Get an instance of a context for the specified protocol from the named
   * provider.
   * 
   * @param protocol The name of the protocol to get a context for.
   * @param provider The name of the provider to get the implementation from.
   * @return The new context.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           given protocol.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>protocol</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>protocol</code> is an empty string.
   */
  public static final SSLContext getInstance(String protocol, String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(protocol, p);
  }

  /**
   * Get an instance of a context for the specified protocol from the specified
   * provider.
   * 
   * @param protocol The name of the protocol to get a context for.
   * @param provider The name of the provider to get the implementation from.
   * @return The new context.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           given protocol.
   * @throws IllegalArgumentException if either <code>protocol</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>protocol</code> is an empty string.
   */
  public static final SSLContext getInstance(String protocol, Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("SSLContext for protocol [")
        .append(protocol).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SSL_CONTEXT, protocol, provider);
        return new SSLContext((SSLContextSpi) spi, provider, protocol);
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    NoSuchAlgorithmException x = new NoSuchAlgorithmException(sb.toString());
    x.initCause(cause);
    throw x;
  }

  /**
   * Creates a new {@link SSLEngine} for this context.
   *
   * @return The new SSLEngine.
   * @since 1.5
   */
  public final SSLEngine createSSLEngine ()
  {
    return ctxSpi.engineCreateSSLEngine ();
  }

  /**
   * Creates a new {@link SSLEngine} for this context, with a given
   * host name and port number.
   *
   * @param host The local host name.
   * @param port The local port number.
   * @return The new SSLEngine.
   * @since 1.5
   */
  public final SSLEngine createSSLEngine (final String host, final int port)
  {
    return ctxSpi.engineCreateSSLEngine (host, port);
  }

  /**
   * Returns the set of SSL contexts available for client connections.
   *
   * @return The set of SSL contexts available for client connections.
   */
  public final SSLSessionContext getClientSessionContext()
  {
    return ctxSpi.engineGetClientSessionContext();
  }

  /**
   * Returns the protocol name of this context.
   *
   * @return The protocol name of this context.
   */
  public final String getProtocol()
  {
    return protocol;
  }

  /**
   * Returns the provider of this implementation.
   *
   * @return The provider of this implementation.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Returns the set of SSL contexts available for server connections.
   *
   * @return The set of SSL contexts available for server connections.
   */
  public final SSLSessionContext getServerSessionContext()
  {
    return ctxSpi.engineGetServerSessionContext();
  }

  /**
   * Returns the factory for server SSL sockets.
   *
   * @return The factory for server SSL sockets.
   */
  public final SSLServerSocketFactory getServerSocketFactory()
  {
    return ctxSpi.engineGetServerSocketFactory();
  }

  /**
   * Returns the factory for client SSL sockets.
   *
   * @return The factory for client SSL sockets.
   */
  public final SSLSocketFactory getSocketFactory()
  {
    return ctxSpi.engineGetSocketFactory();
  }

  /**
   * Initializes this context and prepares it for producing socket
   * factories. All of the parameters are optional; default values are
   * used if left unspecified.
   *
   * @param keyManagers The set of key managers to use.
   * @param trustManagers The set of trust managers to use.
   * @param random A source of random bits to use.
   * @throws KeyManagementException If initialization fails.
   */
  public final void init(KeyManager[] keyManagers,
                         TrustManager[] trustManagers,
                         SecureRandom random)
    throws KeyManagementException
  {
    ctxSpi.engineInit(keyManagers, trustManagers, random);
  }
}
