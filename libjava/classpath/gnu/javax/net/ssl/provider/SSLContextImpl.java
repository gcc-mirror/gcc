/* SSLContextImpl.java --
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

import gnu.java.security.action.GetSecurityPropertyAction;
import gnu.javax.net.ssl.AbstractSessionContext;
import gnu.javax.net.ssl.NullManagerParameters;
import gnu.javax.net.ssl.PreSharedKeyManager;
import gnu.javax.net.ssl.SRPTrustManager;

import java.security.AccessController;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.UnrecoverableKeyException;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContextSpi;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509ExtendedKeyManager;
import javax.net.ssl.X509TrustManager;

/**
 * Our implementation of {@link SSLContextSpi}.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public final class SSLContextImpl extends SSLContextSpi
{
  AbstractSessionContext serverContext;
  AbstractSessionContext clientContext;

  PreSharedKeyManager pskManager;
  X509ExtendedKeyManager keyManager;
  X509TrustManager trustManager;
  SRPTrustManager srpTrustManager;
  SecureRandom random;

  public SSLContextImpl()
  {
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineCreateSSLEngine()
   */
  protected @Override SSLEngine engineCreateSSLEngine()
  {
    return engineCreateSSLEngine(null, -1);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineCreateSSLEngine(java.lang.String, int)
   */
  protected @Override SSLEngine engineCreateSSLEngine(String host, int port)
  {
    return new SSLEngineImpl(this, host, port);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineGetClientSessionContext()
   */
  protected @Override synchronized SSLSessionContext engineGetClientSessionContext()
  {
    if (clientContext == null)
      {
        try
          {
            clientContext = AbstractSessionContext.newInstance();
          }
        catch (SSLException ssle)
          {
            // XXX Ignore?
          }
      }
    return clientContext;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineGetServerSessionContext()
   */
  protected @Override synchronized SSLSessionContext engineGetServerSessionContext()
  {
    if (serverContext == null)
      {
        try
          {
            serverContext = AbstractSessionContext.newInstance();
          }
        catch (SSLException ssle)
          {
            // XXX Ignore?
          }
      }
    return serverContext;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineGetServerSocketFactory()
   */
  protected @Override SSLServerSocketFactory engineGetServerSocketFactory()
  {
    return new SSLServerSocketFactoryImpl(this);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineGetSocketFactory()
   */
  protected @Override SSLSocketFactory engineGetSocketFactory()
  {
    return new SSLSocketFactoryImpl(this);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLContextSpi#engineInit(javax.net.ssl.KeyManager[], javax.net.ssl.TrustManager[], java.security.SecureRandom)
   */
  protected @Override void engineInit(KeyManager[] keyManagers,
                                      TrustManager[] trustManagers,
                                      SecureRandom random)
    throws KeyManagementException
  {
    keyManager = null;
    trustManager = null;
    srpTrustManager = null;
    if (keyManagers != null)
      {
        for (int i = 0; i < keyManagers.length; i++)
          {
            if ((keyManagers[i] instanceof X509ExtendedKeyManager)
                && keyManager == null)
              keyManager = (X509ExtendedKeyManager) keyManagers[i];
            if (keyManagers[i] instanceof PreSharedKeyManager
                && pskManager == null)
              pskManager = (PreSharedKeyManager) keyManagers[i];
          }
      }
    if (keyManager == null)
      keyManager = defaultKeyManager();
    if (trustManagers != null)
      {
        for (int i = 0; i < trustManagers.length; i++)
          {
            if (trustManagers[i] instanceof X509TrustManager)
              {
                if (trustManager == null)
                  trustManager = (X509TrustManager) trustManagers[i];
              }
            else if (trustManagers[i] instanceof SRPTrustManager)
              {
                if (srpTrustManager == null)
                  srpTrustManager = (SRPTrustManager) trustManagers[i];
              }
          }
      }
    if (trustManager == null && srpTrustManager == null)
      {
        trustManager = defaultTrustManager();
      }
    if (random != null)
      {
        this.random = random;
      }
    else
      {
        this.random = defaultRandom();
      }
  }

  /**
   * Create and return a default key manager. The default is the JessieX509
   * algorithm, loaded from either the jssecerts file, or the cacerts file.
   *
   * @return The default key manager instance.
   * @throws KeyManagementException If the instance cannot be created.
   */
  private X509ExtendedKeyManager defaultKeyManager() throws KeyManagementException
  {
    KeyManagerFactory fact = null;
    try
      {
        fact = KeyManagerFactory.getInstance("JessieX509", "Jessie");
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new KeyManagementException(nsae);
      }
    catch (NoSuchProviderException nspe)
      {
        throw new KeyManagementException(nspe);
      }
    try
      {
        fact.init(null, null);
        return (X509ExtendedKeyManager) fact.getKeyManagers()[0];
      }
    catch (NoSuchAlgorithmException nsae) { }
    catch (KeyStoreException kse) { }
    catch (UnrecoverableKeyException uke) { }
    catch (IllegalStateException ise) { }

    try
      {
        fact.init(new NullManagerParameters());
        return (X509ExtendedKeyManager) fact.getKeyManagers()[0];
      }
    catch (Exception shouldNotHappen)
      {
        throw new Error(shouldNotHappen.toString());
      }
  }

  /**
   * Create and return a default trust manager. The default is the JessieX509
   * algorithm, loaded from either the jssecerts file, or the cacerts file.
   *
   * @return The default trust manager instance.
   * @throws KeyManagementException If the instance cannot be created.
   */
  private X509TrustManager defaultTrustManager() throws KeyManagementException
  {
    try
      {
        TrustManagerFactory fact =
          TrustManagerFactory.getInstance("JessieX509", "Jessie");
        fact.init((KeyStore) null);
        return (X509TrustManager) fact.getTrustManagers()[0];
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new KeyManagementException(nsae);
      }
    catch (NoSuchProviderException nspe)
      {
        throw new KeyManagementException(nspe);
      }
    catch (KeyStoreException kse)
      {
        throw new KeyManagementException(kse);
      }
  }

  /**
   * Create a default secure PRNG. This is defined as either the algorithm
   * given in the <code>gnu.javax.net.ssl.secureRandom</code> security
   * property, or Fortuna if that property is not set. If none of these
   * algorithms can be found, and instance created with the SecureRandom
   * constructor is returned.
   *
   * @return The default secure PRNG instance.
   */
  private SecureRandom defaultRandom()
  {
    GetSecurityPropertyAction gspa
      = new GetSecurityPropertyAction("gnu.javax.net.ssl.secureRandom");
    String alg = AccessController.doPrivileged(gspa);
    if (alg == null)
      alg = "Fortuna";
    SecureRandom rand = null;
    try
      {
        rand = SecureRandom.getInstance(alg);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        rand = new SecureRandom();
      }

    return rand;
  }
}
