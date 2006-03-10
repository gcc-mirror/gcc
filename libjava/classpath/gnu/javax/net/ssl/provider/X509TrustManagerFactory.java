/* X509TrustManagerFactory.java -- X.509 trust manager factory.
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

import java.io.FileInputStream;
import java.io.IOException;

import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedList;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Security;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.ManagerFactoryParameters;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactorySpi;
import javax.net.ssl.X509TrustManager;

import gnu.javax.net.ssl.NullManagerParameters;
import gnu.javax.net.ssl.StaticTrustAnchors;

/**
 * This class implements a {@link javax.net.ssl.TrustManagerFactory} engine
 * for the ``JessieX509'' algorithm.
 */
public class X509TrustManagerFactory extends TrustManagerFactorySpi
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  /**
   * The location of the JSSE key store.
   */
  private static final String JSSE_CERTS = Util.getProperty("java.home")
    + Util.getProperty("file.separator") + "lib"
    + Util.getProperty("file.separator") + "security"
    + Util.getProperty("file.separator") + "jssecerts";

  /**
   * The location of the system key store, containing the CA certs.
   */
  private static final String CA_CERTS = Util.getProperty("java.home")
    + Util.getProperty("file.separator") + "lib"
    + Util.getProperty("file.separator") + "security"
    + Util.getProperty("file.separator") + "cacerts";

  private Manager current;

  // Construtors.
  // -------------------------------------------------------------------------

  public X509TrustManagerFactory()
  {
    super();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  protected TrustManager[] engineGetTrustManagers()
  {
    if (current == null)
      {
        throw new IllegalStateException("not initialized");
      }
    return new TrustManager[] { current };
  }

  protected void engineInit(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    if (params instanceof StaticTrustAnchors)
      {
        current = new Manager(((StaticTrustAnchors) params).getCertificates());
      }
    else if (params instanceof NullManagerParameters)
      {
        current = new Manager(new X509Certificate[0]);
      }
    else
      {
        throw new InvalidAlgorithmParameterException();
      }
  }

  protected void engineInit(KeyStore store) throws KeyStoreException
  {
    if (store == null)
      {
        String s = Util.getProperty("javax.net.ssl.trustStoreType");
        if (s == null)
          s = KeyStore.getDefaultType();
        store = KeyStore.getInstance(s);
        try
          {
            s = Util.getProperty("javax.net.ssl.trustStore");
            FileInputStream in = null;
            if (s == null)
              {
                try
                  {
                    in = new FileInputStream(JSSE_CERTS);
                  }
                catch (IOException e)
                  {
                    in = new FileInputStream(CA_CERTS);
                  }
              }
            else
              {
                in = new FileInputStream(s);
              }
            String p = Util.getProperty("javax.net.ssl.trustStorePassword");
            store.load(in, p != null ? p.toCharArray() : null);
          }
        catch (IOException ioe)
          {
            throw new KeyStoreException(ioe.toString());
          }
        catch (CertificateException ce)
          {
            throw new KeyStoreException(ce.toString());
          }
        catch (NoSuchAlgorithmException nsae)
          {
            throw new KeyStoreException(nsae.toString());
          }
      }

    LinkedList l = new LinkedList();
    Enumeration aliases = store.aliases();
    while (aliases.hasMoreElements())
      {
        String alias = (String) aliases.nextElement();
        if (!store.isCertificateEntry(alias))
          continue;
        Certificate c = store.getCertificate(alias);
        if (!(c instanceof X509Certificate))
          continue;
        l.add(c);
      }
    current = this.new Manager((X509Certificate[])
                               l.toArray(new X509Certificate[l.size()]));
  }

  // Inner class.
  // -------------------------------------------------------------------------

  /**
   * The actual manager implementation returned.
   */
  private class Manager implements X509TrustManager
  {

    // Fields.
    // -----------------------------------------------------------------------

    private final X509Certificate[] trusted;

    // Constructor.
    // -----------------------------------------------------------------------

    Manager(X509Certificate[] trusted)
    {
      this.trusted = trusted;
    }

    // Instance methodns.
    // -----------------------------------------------------------------------

    public void checkClientTrusted(X509Certificate[] chain, String authType)
      throws CertificateException
    {
      checkTrusted(chain, authType);
    }

    public void checkServerTrusted(X509Certificate[] chain, String authType)
      throws CertificateException
    {
      checkTrusted(chain, authType);
    }

    public X509Certificate[] getAcceptedIssuers()
    {
      if (trusted == null)
        return new X509Certificate[0];
      return (X509Certificate[]) trusted.clone();
    }

    // Own methods.
    // -----------------------------------------------------------------------

    private void checkTrusted(X509Certificate[] chain, String authType)
      throws CertificateException
    {
      // NOTE: this is not a full-featured path validation algorithm.
      //
      // Step 0: check if the target is valid now.
      chain[0].checkValidity();

      // Step 1: verify that the chain is complete and valid.
      for (int i = 1; i < chain.length; i++)
        {
          chain[i].checkValidity();
          try
            {
              chain[i-1].verify(chain[i].getPublicKey());
            }
          catch (NoSuchAlgorithmException nsae)
            {
              throw new CertificateException(nsae.toString());
            }
          catch (NoSuchProviderException nspe)
            {
              throw new CertificateException(nspe.toString());
            }
          catch (InvalidKeyException ike)
            {
              throw new CertificateException(ike.toString());
            }
          catch (SignatureException se)
            {
              throw new CertificateException(se.toString());
            }
        }

      // Step 2: verify that the root of the chain was issued by a trust anchor.
      if (trusted == null || trusted.length == 0)
        throw new CertificateException("no trust anchors");
      for (int i = 0; i < trusted.length; i++)
        {
          try
            {
              trusted[i].checkValidity();
              chain[chain.length-1].verify(trusted[i].getPublicKey());
              return;
            }
          catch (Exception e)
            {
            }
          //catch (CertificateException ce) { }
          //catch (NoSuchAlgorithmException nsae) { }
          //catch (NoSuchProviderException nspe) { }
          //catch (InvalidKeyException ike) { }
          //catch (SignatureException se) { }
        }
      throw new CertificateException();
    }
  }
}
