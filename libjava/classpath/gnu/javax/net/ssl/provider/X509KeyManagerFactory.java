/* X509KeyManagerFactory.java -- X.509 key manager factory.
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
import java.net.Socket;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Enumeration;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.List;

import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactorySpi;
import javax.net.ssl.ManagerFactoryParameters;
import javax.net.ssl.X509KeyManager;

import gnu.javax.net.ssl.NullManagerParameters;
import gnu.javax.net.ssl.PrivateCredentials;

/**
 * This class implements a {@link javax.net.ssl.KeyManagerFactory} engine
 * for the ``JessieX509'' algorithm.
 */
public class X509KeyManagerFactory extends KeyManagerFactorySpi
{

  // Fields.
  // -------------------------------------------------------------------------

  private Manager current;

  // Constructor.
  // -------------------------------------------------------------------------

  public X509KeyManagerFactory()
  {
    super();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  protected KeyManager[] engineGetKeyManagers()
  {
    if (current == null)
      {
        throw new IllegalStateException();
      }
    return new KeyManager[] { current };
  }

  protected void engineInit(ManagerFactoryParameters params)
    throws InvalidAlgorithmParameterException
  {
    if (params instanceof NullManagerParameters)
      {
        current = new Manager(Collections.EMPTY_MAP, Collections.EMPTY_MAP);
      }
    else if (params instanceof PrivateCredentials)
      {
        List chains = ((PrivateCredentials) params).getCertChains();
        List keys = ((PrivateCredentials) params).getPrivateKeys();
        int i = 0;
        HashMap certMap = new HashMap();
        HashMap keyMap = new HashMap();
        Iterator c = chains.iterator();
        Iterator k = keys.iterator();
        while (c.hasNext() && k.hasNext())
          {
            certMap.put(String.valueOf(i), c.next());
            keyMap.put(String.valueOf(i), k.next());
            i++;
          }
        current = new Manager(keyMap, certMap);
      }
    else
      {
        throw new InvalidAlgorithmParameterException();
      }
  }

  protected void engineInit(KeyStore store, char[] passwd)
    throws KeyStoreException, NoSuchAlgorithmException,
           UnrecoverableKeyException
  {
    if (store == null)
      {
        String s = Util.getProperty("javax.net.ssl.keyStoreType");
        if (s == null)
          s = KeyStore.getDefaultType();
        store = KeyStore.getInstance(s);
        s = Util.getProperty("javax.net.ssl.keyStore");
        if (s == null)
          return;
        String p = Util.getProperty("javax.net.ssl.keyStorePassword");
        try
          {
            store.load(new FileInputStream(s), p != null ? p.toCharArray() : null);
          }
        catch (IOException ioe)
          {
            throw new KeyStoreException(ioe.toString());
          }
        catch (CertificateException ce)
          {
            throw new KeyStoreException(ce.toString());
          }
      }

    HashMap p = new HashMap();
    HashMap c = new HashMap();
    Enumeration aliases = store.aliases();
    UnrecoverableKeyException exception = null;
    while (aliases.hasMoreElements())
      {
        String alias = (String) aliases.nextElement();
        if (!store.isKeyEntry(alias))
          {
            continue;
          }
        X509Certificate[] chain = null;
        Certificate[] chain2 = store.getCertificateChain (alias);
        if (chain2 != null && chain2.length > 0 &&
            (chain2[0] instanceof X509Certificate))
          {
            chain = toX509Chain(chain2);
          }
        else
          {
            continue;
          }
        PrivateKey key = null;
        try
          {
            key = (PrivateKey) store.getKey(alias, passwd);
          }
        catch (UnrecoverableKeyException uke)
          {
            exception = uke;
            continue;
          }
        if (key == null)
          {
            continue;
          }
        p.put(alias, key);
        c.put(alias, chain);
      }
    if (p.isEmpty () && c.isEmpty ())
      {
        if (exception != null)
          {
            throw exception;
          }
        throw new KeyStoreException ("no private credentials found");
      }
    current = this.new Manager(p, c);
  }

  private static X509Certificate[] toX509Chain(Certificate[] chain)
  {
    if (chain instanceof X509Certificate[])
      {
        return (X509Certificate[]) chain;
      }
    X509Certificate[] _chain = new X509Certificate[chain.length];
    for (int i = 0; i < chain.length; i++)
      _chain[i] = (X509Certificate) chain[i];
    return _chain;
  }

  // Inner class.
  // -------------------------------------------------------------------------

  private class Manager implements X509KeyManager
  {
    // Fields.
    // -----------------------------------------------------------------------

    private final Map privateKeys;
    private final Map certChains;

    // Constructor.
    // -----------------------------------------------------------------------

    Manager(Map privateKeys, Map certChains)
    {
      this.privateKeys = privateKeys;
      this.certChains = certChains;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public String chooseClientAlias(String[] keyTypes, Principal[] issuers,
                                    Socket socket)
    {
      for (int i = 0; i < keyTypes.length; i++)
        {
          String[] s = getClientAliases(keyTypes[i], issuers);
          if (s.length > 0)
            return s[0];
        }
      return null;
    }

    public String[] getClientAliases(String keyType, Principal[] issuers)
    {
      return getAliases(keyType, issuers);
    }

    public String chooseServerAlias(String keyType, Principal[] issuers,
                                    Socket socket)
    {
      String[] s = getServerAliases(keyType, issuers);
      if (s.length > 0)
        return s[0];
      return null;
    }

    public String[] getServerAliases(String keyType, Principal[] issuers)
    {
      return getAliases(keyType, issuers);
    }

    private String[] getAliases(String keyType, Principal[] issuers)
    {
      LinkedList l = new LinkedList();
      for (Iterator i = privateKeys.keySet().iterator(); i.hasNext(); )
        {
          String alias = (String) i.next();
          X509Certificate[] chain = getCertificateChain(alias);
          if (chain.length == 0)
            continue;
          PrivateKey privKey = getPrivateKey(alias);
          if (privKey == null)
            continue;
          PublicKey pubKey = chain[0].getPublicKey();
          if (keyType.equals("RSA") || keyType.equals("DHE_RSA") ||
              keyType.equals("SRP_RSA") || keyType.equals("rsa_sign"))
            {
              if (!(privKey instanceof RSAPrivateKey) ||
                  !(pubKey instanceof RSAPublicKey))
                continue;
            }
          if (keyType.equals("DHE_DSS") || keyType.equals("dss_sign") ||
              keyType.equals("SRP_DSS"))
            {
              if (!(privKey instanceof DSAPrivateKey) ||
                  !(pubKey instanceof DSAPublicKey))
                continue;
            }
          if (keyType.equals("DH_RSA") || keyType.equals("rsa_fixed_dh"))
            {
              if (!(privKey instanceof DHPrivateKey) ||
                  !(pubKey instanceof DHPublicKey))
                continue;
              if (!chain[0].getSigAlgName().equalsIgnoreCase("RSA"))
                continue;
            }
          if (keyType.equals("DH_DSS") || keyType.equals("dss_fixed_dh"))
            {
              if (!(privKey instanceof DHPrivateKey) ||
                  !(pubKey instanceof DHPublicKey))
                continue;
              if (!chain[0].getSigAlgName().equalsIgnoreCase("DSA"))
                continue;
            }
          if (issuers == null || issuers.length == 0)
            {
              l.add(alias);
              continue;
            }
          for (int j = 0; j < issuers.length; j++)
            if (chain[0].getIssuerDN().equals(issuers[j]))
              {
                l.add(alias);
                break;
              }
        }
      return (String[]) l.toArray(new String[l.size()]);
    }

    public X509Certificate[] getCertificateChain(String alias)
    {
      X509Certificate[] c = (X509Certificate[]) certChains.get(alias);
      return c != null ? (X509Certificate[]) c.clone() : null;
    }

    public PrivateKey getPrivateKey(String alias)
    {
      return (PrivateKey) privateKeys.get(alias);
    }
  }
}
