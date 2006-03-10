/* Context.java -- SSLContext implementation.
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

import java.io.File;
import java.io.InputStream;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStoreException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.sql.SQLException;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContextSpi;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509KeyManager;
import javax.net.ssl.X509TrustManager;

import gnu.javax.net.ssl.NullManagerParameters;
import gnu.javax.net.ssl.SRPTrustManager;
import gnu.javax.net.ssl.StaticTrustAnchors;

/**
 * This is Jessie's implementation of a {@link javax.net.ssl.SSLContext}
 * engine, and is available under the algorithm names ``SSLv3'', ``SSL'',
 * ``TLSv1'', and ``TLS''.
 */
public final class Context extends SSLContextSpi
{

  // Fields.
  // -------------------------------------------------------------------------

  private SessionContext clientSessions;
  private SessionContext serverSessions;
  private X509KeyManager keyManager;
  private X509TrustManager trustManager;
  private SRPTrustManager srpTrustManager;
  private SecureRandom random;

  // Constructor.
  // -------------------------------------------------------------------------

  public Context()
  {
    String codec = Util.getSecurityProperty("jessie.clientSessionContext.codec");
    String codecClass = null;
    if (codec == null)
      {
        codec = "null";
      }
    if (codec.equalsIgnoreCase("xml"))
      {
        codecClass = "gnu.javax.net.ssl.provider.XMLSessionContext";
      }
    else if (codec.equalsIgnoreCase("jdbc"))
      {
        codecClass = "gnu.javax.net.ssl.provider.JDBCSessionContext";
      }
    else if (codec.equalsIgnoreCase("null"))
      {
        codecClass = "gnu.javax.net.ssl.provider.SessionContext";
      }
    else
      {
        throw new IllegalArgumentException("no such codec: " + codec);
      }
    try
      {
        ClassLoader cl = Context.class.getClassLoader();
        if (cl == null)
          {
            cl = ClassLoader.getSystemClassLoader();
          }
        clientSessions = (SessionContext) cl.loadClass(codecClass).newInstance();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        throw new IllegalArgumentException(ex.toString());
      }

    codec = Util.getSecurityProperty("jessie.serverSessionContext.codec");
    if (codec == null)
      {
        codec = "null";
      }
    if (codec.equalsIgnoreCase("xml"))
      {
        codecClass = "gnu.javax.net.ssl.provider.XMLSessionContext";
      }
    else if (codec.equalsIgnoreCase("jdbc"))
      {
        codecClass = "gnu.javax.net.ssl.provider.JDBCSessionContext";
      }
    else if (codec.equalsIgnoreCase("null"))
      {
        codecClass = "gnu.javax.net.ssl.provider.SessionContext";
      }
    else
      {
        throw new IllegalArgumentException("no such codec: " + codec);
      }
    try
      {
        ClassLoader cl = Context.class.getClassLoader();
        if (cl == null)
          {
            cl = ClassLoader.getSystemClassLoader();
          }
        serverSessions = (SessionContext) cl.loadClass(codecClass).newInstance();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        throw new IllegalArgumentException(ex.toString());
      }
  }

  // Engine methods.
  // -------------------------------------------------------------------------

  protected SSLSessionContext engineGetClientSessionContext()
  {
    return clientSessions;
  }

  protected SSLSessionContext engineGetServerSessionContext()
  {
    return serverSessions;
  }

  protected javax.net.ssl.SSLServerSocketFactory engineGetServerSocketFactory()
  {
    if (keyManager == null || (trustManager == null && srpTrustManager == null)
        || random == null)
      {
        throw new IllegalStateException();
      }
    return new SSLServerSocketFactory(trustManager, srpTrustManager, keyManager,
                                      random, serverSessions);
  }

  protected javax.net.ssl.SSLSocketFactory engineGetSocketFactory()
  {
    if (keyManager == null || trustManager == null || random == null)
      {
        throw new IllegalStateException();
      }
    return new SSLSocketFactory(trustManager, keyManager, random, clientSessions);
  }

  protected void engineInit(KeyManager[] keyManagers,
                            TrustManager[] trustManagers, SecureRandom random)
    throws KeyManagementException
  {
    keyManager = null;
    trustManager = null;
    srpTrustManager = null;
    if (keyManagers != null)
      {
        for (int i = 0; i < keyManagers.length; i++)
          {
            if (keyManagers[i] instanceof X509KeyManager)
              {
                keyManager = (X509KeyManager) keyManagers[i];
                break;
              }
          }
      }
    if (keyManager == null)
      {
        keyManager = defaultKeyManager();
      }
    if (trustManagers != null)
      {
        for (int i = 0; i < trustManagers.length; i++)
          {
            if (trustManagers[i] instanceof X509TrustManager)
              {
                if (trustManager == null)
                  {
                    trustManager = (X509TrustManager) trustManagers[i];
                  }
              }
            else if (trustManagers[i] instanceof SRPTrustManager)
              {
                if (srpTrustManager == null)
                  {
                    srpTrustManager = (SRPTrustManager) trustManagers[i];
                  }
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

  // Own methods.
  // -------------------------------------------------------------------------

  private X509KeyManager defaultKeyManager() throws KeyManagementException
  {
    KeyManagerFactory fact = null;
    try
      {
        fact = KeyManagerFactory.getInstance("JessieX509", "Jessie");
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new KeyManagementException();
      }
    catch (NoSuchProviderException nspe)
      {
        throw new KeyManagementException();
      }
    try
      {
        fact.init(null, null);
        return (X509KeyManager) fact.getKeyManagers()[0];
      }
    catch (NoSuchAlgorithmException nsae) { }
    catch (KeyStoreException kse) { }
    catch (UnrecoverableKeyException uke) { }
    catch (IllegalStateException ise) { }

    try
      {
        fact.init(new NullManagerParameters());
        return (X509KeyManager) fact.getKeyManagers()[0];
      }
    catch (Exception shouldNotHappen)
      {
        throw new Error(shouldNotHappen.toString());
      }
  }

  private X509TrustManager defaultTrustManager() throws KeyManagementException
  {
    try
      {
        TrustManagerFactory fact =
          TrustManagerFactory.getInstance("JessieX509", "Jessie");
        fact.init(StaticTrustAnchors.CA_CERTS);
        return (X509TrustManager) fact.getTrustManagers()[0];
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new KeyManagementException(nsae.toString());
      }
    catch (NoSuchProviderException nspe)
      {
        throw new KeyManagementException(nspe.toString());
      }
    catch (InvalidAlgorithmParameterException kse)
      {
        throw new KeyManagementException(kse.toString());
      }
  }

  private SecureRandom defaultRandom() throws KeyManagementException
  {
    String alg = Util.getSecurityProperty("jessie.secure.random");
    if (alg == null)
      {
        alg = "Fortuna";
      }
    SecureRandom rand = null;
    try
      {
        rand = SecureRandom.getInstance(alg);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new KeyManagementException(nsae.toString());
      }

    return rand;
  }
}
