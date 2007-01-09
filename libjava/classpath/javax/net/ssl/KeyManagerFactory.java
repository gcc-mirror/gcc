/* KeyManagerFactory.java -- factory for key managers.
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
import java.security.UnrecoverableKeyException;

/**
 * A class that creates key manager implementations based on a
 * requested algorithm.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class KeyManagerFactory
{

  // Constants and fields.
  // ------------------------------------------------------------------

  /** The service name for key manager factories. */
  private static final String KEY_MANAGER_FACTORY = "KeyManagerFactory";

  /** The system default trust manager algorithm. */
  private static final String DEFAULT_ALGORITHM = "JessieX509";

  /** The underlying engine. */
  private final KeyManagerFactorySpi kmfSpi;

  /** The provider of this implementation. */
  private final Provider provider;

  /** The name of this algorithm. */
  private final String algorithm;

  // Constructor.
  // ------------------------------------------------------------------

  /**
   * Create a new key manager factory.
   *
   * @param kmfSpi The underlying engine.
   * @param provider The engine's provider.
   * @param algorithm The name of this algorithm.
   */
  protected KeyManagerFactory(KeyManagerFactorySpi kmfSpi,
                              Provider provider, String algorithm)
  {
    this.kmfSpi = kmfSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  // Class methods.
  // ------------------------------------------------------------------

  /**
   * Get the default algorithm name. This value may be specified at
   * run-time via the security property
   * "ssl.KeyManagerFactory.algorithm". If this property is
   * not specified, this method returns "JessieX509".
   *
   * @return The default key manager factory algorithm's name.
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
              return Security.getProperty("ssl.KeyManagerFactory.algorithm");
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

  /**
   * Create an instance of the named key manager factory, from the first
   * provider that implements it.
   * 
   * @param algorithm The type of key manager factory to get.
   * @return An appropriate implementation of that algoritm.
   * @throws NoSuchAlgorithmException If no provider implements the requested
   *           algorithm.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final KeyManagerFactory getInstance(String algorithm)
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
   * Create an instance of the named key manager factory, from the named
   * provider.
   * 
   * @param algorithm The type of key manager factory to get.
   * @param provider The name of the provider to get the implementation from.
   * @return An appropriate implementation of that algorithm.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           requested algorithm.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyManagerFactory getInstance(String algorithm,
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
   * Create an instance of the named key manager factory, from the given
   * provider.
   * 
   * @param algorithm The type of key manager factory to get.
   * @param provider The provider to get the implementation from.
   * @return An appropriate implementation of that algorithm.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           requested algorithm.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyManagerFactory getInstance(String algorithm,
                                                    Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("KeyManagerFactory algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(KEY_MANAGER_FACTORY, algorithm, provider);
        return new KeyManagerFactory((KeyManagerFactorySpi) spi, provider, algorithm);
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
   * Returns the name of this key manager factory algorithm.
   *
   * @return The name of this key manager factory algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Get an array of key managers appropriate for this algorithm, with
   * the most preferred manager first.
   *
   * @return The array of key managers.
   */
  public final KeyManager[] getKeyManagers()
  {
    return kmfSpi.engineGetKeyManagers();
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
   * Initialize this instance with an implementation-dependent
   * parameter object.
   *
   * @param params The parameters to initialize with.
   * @throws InvalidAlgorithmParameterException If the specified
   *   parameters are inappropriate.
   */
  public final void init(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    kmfSpi.engineInit(params);
  }

  /**
   * Initialize this instance with a key store and a password for
   * private key entries.
   *
   * @param store The key store to read.
   * @param passwd The password protecting private keys in the store.
   * @throws KeyStoreException If an error occurs reading the keys.
   * @throws NoSuchAlgorithmException If an algorithm (such as a
   *   certificate algorithm) is not available.
   * @throws UnrecoverableKeyException If the password is incorrect.
   */
  public final void init(KeyStore store, char[] passwd)
    throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException
  {
    kmfSpi.engineInit(store, passwd);
  }
}
