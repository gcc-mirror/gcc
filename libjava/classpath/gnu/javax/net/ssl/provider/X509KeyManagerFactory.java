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
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

import java.util.Collections;
import java.util.Map;
import java.util.List;

import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactorySpi;
import javax.net.ssl.ManagerFactoryParameters;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.X509ExtendedKeyManager;
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
        List<X509Certificate[]> chains
          = ((PrivateCredentials) params).getCertChains();
        List<PrivateKey> keys
          = ((PrivateCredentials) params).getPrivateKeys();
        int i = 0;
        HashMap<String, X509Certificate[]> certMap
          = new HashMap<String, X509Certificate[]>();
        HashMap<String, PrivateKey> keyMap
          = new HashMap<String, PrivateKey>();
        Iterator<X509Certificate[]> c = chains.iterator();
        Iterator<PrivateKey> k = keys.iterator();
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

    HashMap<String, PrivateKey> p = new HashMap<String, PrivateKey>();
    HashMap<String, X509Certificate[]> c
      = new HashMap<String, X509Certificate[]>();
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

  private class Manager extends X509ExtendedKeyManager
  {
    // Fields.
    // -----------------------------------------------------------------------

    private final Map<String, PrivateKey> privateKeys;
    private final Map<String, X509Certificate[]> certChains;

    // Constructor.
    // -----------------------------------------------------------------------

    Manager(Map<String, PrivateKey> privateKeys,
            Map<String, X509Certificate[]> certChains)
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
    
    public @Override String chooseEngineClientAlias(String[] keyTypes,
                                                    Principal[] issuers,
                                                    SSLEngine engine)
    {
      for (String type : keyTypes)
        {
          String[] s = getClientAliases(type, issuers);
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
    
    public @Override String chooseEngineServerAlias(String keyType,
                                                    Principal[] issuers,
                                                    SSLEngine engine)
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
      LinkedList<String> l = new LinkedList<String>();
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
          if (keyType.equalsIgnoreCase("RSA")
              || keyType.equalsIgnoreCase("DHE_RSA")
              || keyType.equalsIgnoreCase("SRP_RSA")
              || keyType.equalsIgnoreCase("rsa_sign")
              || keyType.equalsIgnoreCase("RSA_PSK"))
            {
              if (!(privKey instanceof RSAPrivateKey) ||
                  !(pubKey instanceof RSAPublicKey))
                continue;
            }
          else if (keyType.equalsIgnoreCase("DHE_DSS")
              || keyType.equalsIgnoreCase("dss_sign")
              || keyType.equalsIgnoreCase("SRP_DSS")
              || keyType.equalsIgnoreCase("DSA"))
            {
              if (!(privKey instanceof DSAPrivateKey) ||
                  !(pubKey instanceof DSAPublicKey))
                continue;
            }
          else if (keyType.equalsIgnoreCase("DH_RSA")
              || keyType.equalsIgnoreCase("rsa_fixed_dh"))
            {
              if (!(privKey instanceof DHPrivateKey) ||
                  !(pubKey instanceof DHPublicKey))
                continue;
              if (!chain[0].getSigAlgName().equalsIgnoreCase("RSA"))
                continue;
            }
          else if (keyType.equalsIgnoreCase("DH_DSS")
              || keyType.equalsIgnoreCase("dss_fixed_dh"))
            {
              if (!(privKey instanceof DHPrivateKey) ||
                  !(pubKey instanceof DHPublicKey))
                continue;
              if (!chain[0].getSigAlgName().equalsIgnoreCase("DSA"))
                continue;
            }
          else // Unknown key type; ignore it.
            continue;
          if (issuers == null || issuers.length == 0)
            {
              l.add(alias);
              continue;
            }
          for (Principal issuer : issuers)
            {
              if (chain[0].getIssuerDN().equals(issuer))
                {
                  l.add(alias);
                  break;
                }
            }
        }
      return l.toArray(new String[l.size()]);
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
