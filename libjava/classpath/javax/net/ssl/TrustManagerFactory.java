/* TrustManagerFactory.java -- factory for trust managers.
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
import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.security.Security;

/**
 * A factory for creating trust manager objects.
 */
public class TrustManagerFactory
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  /** The service name for trust manager factories. */
  private static final String TRUST_MANAGER_FACTORY = "TrustManagerFactory";

  /** The system default trust manager algorithm. */
  private static final String DEFAULT_ALGORITHM = "JessieX509";

  /** The underlying engine class. */
  private final TrustManagerFactorySpi tmfSpi;

  /** The provider of the engine class. */
  private final Provider provider;

  /** The name of this trust manager algorithm. */
  private final String algorithm;

  // Constructor.
  // -------------------------------------------------------------------------

  /**
   * Creates a new trust manager factory.
   *
   * @param tmfSpi The underlying engine class.
   * @param provider The provider of the engine class.
   * @param algorithm The trust manager algorithm name.
   */
  protected TrustManagerFactory(TrustManagerFactorySpi tmfSpi,
                                Provider provider, String algorithm)
  {
    this.tmfSpi = tmfSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Returns an instance of a trust manager factory for the given algorithm from
   * the first provider that implements it.
   * 
   * @param algorithm The name of the algorithm to get.
   * @return The instance of the trust manager factory.
   * @throws NoSuchAlgorithmException If no provider implements the given
   *           algorithm.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final TrustManagerFactory getInstance(String algorithm)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Returns an instance of a trust manager factory for the given algorithm from
   * the named provider.
   * 
   * @param algorithm The name of the algorithm to get.
   * @param provider The name of the provider to get the instance from.
   * @return The instance of the trust manager factory.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           given algorithm.
   * @throws NoSuchProviderException If there is no such named provider.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final TrustManagerFactory getInstance(String algorithm,
                                                      String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Returns an instance of a trust manager factory for the given algorithm from
   * the specified provider.
   * 
   * @param algorithm The name of the algorithm to get.
   * @param provider The provider to get the instance from.
   * @return The instance of the trust manager factory.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           given algorithm.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final TrustManagerFactory getInstance(String algorithm,
                                                      Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("TrustManagerFactory algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(TRUST_MANAGER_FACTORY, algorithm, provider);
        return new TrustManagerFactory((TrustManagerFactorySpi) spi,
                                       provider,
                                       algorithm);
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
   * Returns the default algorithm for trust manager factories. The value
   * returned is either the value of the security property
   * "ssl.TrustManagerFactory.algorithm" if it is set, or the value "JessieX509"
   * if not.
   *
   * @return The default algorithm name.
   * @see Security.getProperty(java.lang.String)
   */
  public static final String getDefaultAlgorithm()
  {
    String alg = null;
    try
      {
        alg = (String) AccessController.doPrivileged(
          new PrivilegedAction()
          {
            public Object run()
            {
              return Security.getProperty("ssl.TrustManagerFactory.algorithm");
            }
          }
        );
      }
    catch (SecurityException se)
      {
      }
    if (alg == null)
      alg = DEFAULT_ALGORITHM;
    return alg;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the name of this trust manager algorithm.
   *
   * @return The algorithm name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns the provider of the underlying implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Returns the trust managers created by this factory.
   *
   * @return The trust managers.
   */
  public final TrustManager[] getTrustManagers()
  {
    return tmfSpi.engineGetTrustManagers();
  }

  /**
   * Initialize this instance with some algorithm-specific parameters.
   *
   * @param params The parameters.
   * @throws InvalidAlgorithmParameterException If the supplied parameters
   *   are inappropriate for this instance.
   */
  public final void init(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    tmfSpi.engineInit(params);
  }

  /**
   * Initialize this instance with a key store. The key store may be null,
   * in which case a default will be used.
   *
   * @param store The key store.
   * @throws KeyStoreException If there is a problem reading from the
   *   key store.
   */
  public final void init(KeyStore store) throws KeyStoreException
  {
    tmfSpi.engineInit(store);
  }
}
