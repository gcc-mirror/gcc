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
import java.util.Set;

import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.PKIXParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509Certificate;

import javax.net.ssl.ManagerFactoryParameters;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactorySpi;
import javax.net.ssl.X509TrustManager;

import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.x509.X509CertPath;
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

  private static final String sep
    = AccessController.doPrivileged(new GetPropertyAction("file.separator"));

  /**
   * The location of the JSSE key store.
   */
  private static final String JSSE_CERTS
    = AccessController.doPrivileged(new GetPropertyAction("java.home"))
      + sep + "lib" + sep + "security" + sep + "jssecerts";

  /**
   * The location of the system key store, containing the CA certs.
   */
  private static final String CA_CERTS
    = AccessController.doPrivileged(new GetPropertyAction("java.home"))
      + sep + "lib" + sep + "security" + sep + "cacerts";

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
        GetPropertyAction gpa = new GetPropertyAction("javax.net.ssl.trustStoreType");
        String s = AccessController.doPrivileged(gpa);
        if (s == null)
          s = KeyStore.getDefaultType();
        store = KeyStore.getInstance(s);
        try
          {
            s = AccessController.doPrivileged(gpa.setParameters("javax.net.ssl.trustStore"));
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
            String p = AccessController.doPrivileged(gpa.setParameters("javax.net.ssl.trustStorePassword"));
            store.load(in, p != null ? p.toCharArray() : null);
          }
        catch (IOException ioe)
          {
            throw new KeyStoreException(ioe);
          }
        catch (CertificateException ce)
          {
            throw new KeyStoreException(ce);
          }
        catch (NoSuchAlgorithmException nsae)
          {
            throw new KeyStoreException(nsae);
          }
      }

    LinkedList<X509Certificate> l = new LinkedList<X509Certificate>();
    Enumeration aliases = store.aliases();
    while (aliases.hasMoreElements())
      {
        String alias = (String) aliases.nextElement();
        if (!store.isCertificateEntry(alias))
          continue;
        Certificate c = store.getCertificate(alias);
        if (!(c instanceof X509Certificate))
          continue;
        l.add((X509Certificate) c);
      }
    current = this.new Manager(l.toArray(new X509Certificate[l.size()]));
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

    private final Set<TrustAnchor> anchors;

    // Constructor.
    // -----------------------------------------------------------------------

    Manager(X509Certificate[] trusted)
    {
      anchors = new HashSet<TrustAnchor>();
      if (trusted != null)
        {
          for (X509Certificate cert : trusted)
            {
              anchors.add(new TrustAnchor(cert, null));
            }
        }
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
      return anchors.toArray(new X509Certificate[anchors.size()]);
    }

    // Own methods.
    // -----------------------------------------------------------------------

    private void checkTrusted(X509Certificate[] chain, String authType)
      throws CertificateException
    {
      CertPathValidator validator = null;

      try
        {
          validator = CertPathValidator.getInstance("PKIX");
        }
      catch (NoSuchAlgorithmException nsae)
        {
          throw new CertificateException(nsae);
        }

      CertPath path = new X509CertPath(Arrays.asList(chain));

      PKIXParameters params = null;
      try
        {
          params = new PKIXParameters(anchors);
          // XXX we probably do want to enable revocation, but it's a pain
          // in the ass.
          params.setRevocationEnabled(false);
        }
      catch (InvalidAlgorithmParameterException iape)
        {
          throw new CertificateException(iape);
        }

      try
        {
          validator.validate(path, params);
        }
      catch (CertPathValidatorException cpve)
        {
          throw new CertificateException(cpve);
        }
      catch (InvalidAlgorithmParameterException iape)
        {
          throw new CertificateException(iape);
        }
    }
  }
}
