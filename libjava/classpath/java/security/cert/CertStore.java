/* CertStore -- stores and retrieves certificates.
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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


package java.security.cert;

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.security.Security;
import java.util.Collection;

/**
 * A CertStore is a read-only repository for certificates and
 * certificate revocation lists.
 *
 * @since 1.4
 */
public class CertStore
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Service name for CertStore. */
  private static final String CERT_STORE = "CertStore";

  /** The underlying implementation. */
  private CertStoreSpi storeSpi;

  /** This implementation's provider. */
  private Provider provider;

  /** The name of this key store type. */
  private String type;

  /** The parameters used to initialize this instance, if any. */
  private CertStoreParameters params;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new CertStore.
   *
   * @param storeSpi The underlying implementation.
   * @param provider The provider of this implementation.
   * @param type     The type of CertStore this class represents.
   * @param params   The parameters used to initialize this instance, if any.
   */
  protected CertStore(CertStoreSpi storeSpi, Provider provider, String type,
                      CertStoreParameters params)
  {
    this.storeSpi = storeSpi;
    this.provider = provider;
    this.type = type;
    this.params = params;
  }

// Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the default certificate store type.
   *
   * <p>This value can be set at run-time via the security property
   * "certstore.type"; if not specified than the default type will be
   * "LDAP".
   *
   * @return The default CertStore type.
   */
  public static final synchronized String getDefaultType()
  {
    String type = null;
    type = (String) java.security.AccessController.doPrivileged(
      new PrivilegedAction() {
        public Object run() {
          return Security.getProperty("certstore.type");
        }
      }
    );
    if (type == null)
      type = "LDAP";
    return type;
  }

  /**
   * Returns an instance of the given certificate store type from the first
   * installed provider.
   * 
   * @param type The type of <code>CertStore</code> to create.
   * @param params The parameters to initialize this cert store with.
   * @return The new instance.
   * @throws InvalidAlgorithmParameterException If the instance rejects the
   *           specified parameters.
   * @throws NoSuchAlgorithmException If no installed provider implements the
   *           specified CertStore.
   * @throws IllegalArgumentException if <code>type</code> is
   *           <code>null</code> or is an empty string.
   */
  public static CertStore getInstance(String type, CertStoreParameters params)
    throws InvalidAlgorithmParameterException, NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(type, params, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(type);
  }

  /**
   * Returns an instance of the given certificate store type from a named
   * provider.
   * 
   * @param type The type of <code>CertStore</code> to create.
   * @param params The parameters to initialize this cert store with.
   * @param provider The name of the provider to use.
   * @return The new instance.
   * @throws InvalidAlgorithmParameterException If the instance rejects the
   *           specified parameters.
   * @throws NoSuchAlgorithmException If the specified provider does not
   *           implement the specified CertStore.
   * @throws NoSuchProviderException If no provider named <i>provider</i> is
   *           installed.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static CertStore getInstance(String type, CertStoreParameters params,
                                      String provider)
    throws InvalidAlgorithmParameterException, NoSuchAlgorithmException,
           NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(type, params, p);
  }

  /**
   * Returns an instance of the given certificate store type from a given
   * provider.
   *
   * @param type The type of <code>CertStore</code> to create.
   * @param params   The parameters to initialize this cert store with.
   * @param provider The provider to use.
   * @return The new instance.
   * @throws InvalidAlgorithmParameterException If the instance rejects
   *         the specified parameters.
   * @throws NoSuchAlgorithmException If the specified provider does not
   *         implement the specified CertStore.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static CertStore getInstance(String type, CertStoreParameters params,
                                      Provider provider)
      throws InvalidAlgorithmParameterException, NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("CertStore of type [")
        .append(type).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object[] args = new Object[] { params };
        Object spi = Engine.getInstance(CERT_STORE, type, provider, args);
        return new CertStore((CertStoreSpi) spi, provider, type, params);
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
   * Return the type of certificate store this instance represents.
   *
   * @return The CertStore type.
   */
  public final String getType()
  {
    return type;
  }

  /**
   * Return the provider of this implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Get the parameters this instance was created with, if any. The
   * parameters will be cloned before they are returned.
   *
   * @return The parameters, or null.
   */
  public final CertStoreParameters getCertStoreParameters()
  {
    return params != null ? (CertStoreParameters) params.clone() : null;
  }

  /**
   * Get a collection of certificates from this CertStore, optionally
   * filtered by the specified CertSelector. The Collection returned may
   * be empty, but will never be null.
   *
   * <p>Implementations may not allow a null argument, even if no
   * filtering is desired.
   *
   * @param selector The certificate selector.
   * @return The collection of certificates.
   * @throws CertStoreException If the certificates cannot be retrieved.
   */
  public final Collection<? extends Certificate> getCertificates(CertSelector selector)
    throws CertStoreException
  {
    return storeSpi.engineGetCertificates(selector);
  }

  /**
   * Get a collection of certificate revocation lists from this CertStore,
   * optionally filtered by the specified CRLSelector. The Collection
   * returned may be empty, but will never be null.
   *
   * <p>Implementations may not allow a null argument, even if no
   * filtering is desired.
   *
   * @param selector The certificate selector.
   * @return The collection of certificate revocation lists.
   * @throws CertStoreException If the CRLs cannot be retrieved.
   */
  public final Collection<? extends CRL> getCRLs(CRLSelector selector)
    throws CertStoreException
  {
    return storeSpi.engineGetCRLs(selector);
  }
}
