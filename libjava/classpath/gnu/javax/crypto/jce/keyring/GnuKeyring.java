/* GnuKeyring.java -- 
   Copyright (C) 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.keyring;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.security.Key;
import java.security.KeyStoreSpi;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import java.util.Arrays;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import javax.crypto.SecretKey;

import gnu.java.security.Registry;
import gnu.javax.crypto.keyring.IKeyring;
import gnu.javax.crypto.keyring.IPrivateKeyring;
import gnu.javax.crypto.keyring.IPublicKeyring;
import gnu.javax.crypto.keyring.GnuPrivateKeyring;
import gnu.javax.crypto.keyring.GnuPublicKeyring;
import gnu.javax.crypto.keyring.MalformedKeyringException;
import gnu.javax.crypto.keyring.PrimitiveEntry;

public class GnuKeyring extends KeyStoreSpi
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private boolean loaded;

  private IKeyring keyring;

  // Constructor.
  // ------------------------------------------------------------------------

  public GnuKeyring()
  {
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public Enumeration engineAliases()
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return new Enumeration()
        {
          public boolean hasMoreElements()
          {
            return false;
          }

          public Object nextElement()
          {
            throw new NoSuchElementException();
          }
        };
      }
    return keyring.aliases();
  }

  public boolean engineContainsAlias(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return false;
      }
    return keyring.containsAlias(alias);
  }

  public void engineDeleteEntry(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring != null)
      {
        keyring.remove(alias);
      }
  }

  public Certificate engineGetCertificate(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return null;
      }
    if (!(keyring instanceof IPublicKeyring))
      {
        throw new IllegalStateException("not a public keyring");
      }
    return ((IPublicKeyring) keyring).getCertificate(alias);
  }

  public String engineGetCertificateAlias(Certificate cert)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return null;
      }
    if (!(keyring instanceof IPublicKeyring))
      {
        throw new IllegalStateException("not a public keyring");
      }
    Enumeration aliases = keyring.aliases();
    while (aliases.hasMoreElements())
      {
        String alias = (String) aliases.nextElement();
        Certificate cert2 = ((IPublicKeyring) keyring).getCertificate(alias);
        if (cert.equals(cert2))
          {
            return alias;
          }
      }
    return null;
  }

  public void engineSetCertificateEntry(String alias, Certificate cert)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        keyring = new GnuPublicKeyring("HMAC-SHA-1", 20);
      }
    if (!(keyring instanceof IPublicKeyring))
      {
        throw new IllegalStateException("not a public keyring");
      }
    ((IPublicKeyring) keyring).putCertificate(alias, cert);
  }

  public Certificate[] engineGetCertificateChain(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return null;
      }
    if (!(keyring instanceof IPrivateKeyring))
      {
        throw new IllegalStateException("not a private keyring");
      }
    return ((IPrivateKeyring) keyring).getCertPath(alias);
  }

  public Date engineGetCreationDate(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return null;
      }
    List entries = keyring.get(alias);
    if (entries.size() == 0)
      {
        return null;
      }
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Object o = it.next();
        if (o instanceof PrimitiveEntry)
          {
            return ((PrimitiveEntry) o).getCreationDate();
          }
      }
    return null;
  }

  public Key engineGetKey(String alias, char[] password)
      throws UnrecoverableKeyException
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return null;
      }
    if (!(keyring instanceof IPrivateKeyring))
      {
        throw new IllegalStateException("not a private keyring");
      }
    if (password == null)
      {
        if (((IPrivateKeyring) keyring).containsPublicKey(alias))
          {
            return ((IPrivateKeyring) keyring).getPublicKey(alias);
          }
      }
    if (((IPrivateKeyring) keyring).containsPrivateKey(alias))
      {
        return ((IPrivateKeyring) keyring).getPrivateKey(alias, password);
      }
    return null;
  }

  public void engineSetKeyEntry(String alias, Key key, char[] password,
                                Certificate[] chain) throws KeyStoreException
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        keyring = new GnuPrivateKeyring("HMAC-SHA-1", 20, "AES", "OFB", 16);
      }
    if (!(keyring instanceof IPrivateKeyring))
      {
        throw new IllegalStateException("not a private keyring");
      }
    if (key instanceof PublicKey)
      {
        ((IPrivateKeyring) keyring).putPublicKey(alias, (PublicKey) key);
        return;
      }
    if (!(key instanceof PrivateKey) && !(key instanceof SecretKey))
      {
        throw new KeyStoreException("cannot store keys of type "
                                    + key.getClass().getName());
      }
    try
      {
        CertificateFactory fact = CertificateFactory.getInstance("X.509");
        ((IPrivateKeyring) keyring).putCertPath(alias, chain);
      }
    catch (CertificateException ce)
      {
        throw new KeyStoreException(ce.toString());
      }
    ((IPrivateKeyring) keyring).putPrivateKey(alias, key, password);
  }

  public void engineSetKeyEntry(String alias, byte[] key, Certificate[] chain)
      throws KeyStoreException
  {
    throw new KeyStoreException("method not supported");
  }

  public boolean engineIsCertificateEntry(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return false;
      }
    if (!(keyring instanceof IPublicKeyring))
      {
        return false;
      }
    return ((IPublicKeyring) keyring).containsCertificate(alias);
  }

  public boolean engineIsKeyEntry(String alias)
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return false;
      }
    if (!(keyring instanceof IPrivateKeyring))
      {
        return false;
      }
    return ((IPrivateKeyring) keyring).containsPublicKey(alias)
           || ((IPrivateKeyring) keyring).containsPrivateKey(alias);
  }

  public void engineLoad(InputStream in, char[] password) throws IOException
  {
    if (in != null)
      {
        if (!in.markSupported())
          {
            in = new BufferedInputStream(in);
          }
        in.mark(5);
        for (int i = 0; i < 4; i++)
          if (in.read() != Registry.GKR_MAGIC[i])
            throw new MalformedKeyringException("incorrect magic");
        int usage = in.read();
        in.reset();
        HashMap attr = new HashMap();
        attr.put(IKeyring.KEYRING_DATA_IN, in);
        attr.put(IKeyring.KEYRING_PASSWORD, password);
        switch (usage)
          {
          case GnuPublicKeyring.USAGE:
            keyring = new GnuPublicKeyring();
            break;
          case GnuPrivateKeyring.USAGE:
            keyring = new GnuPrivateKeyring();
            break;
          default:
            throw new MalformedKeyringException("unsupported ring usage: "
                                                + Integer.toBinaryString(usage));
          }
        keyring.load(attr);
      }
    loaded = true;
  }

  public void engineStore(OutputStream out, char[] password) throws IOException
  {
    if (!loaded || keyring == null)
      {
        throw new IllegalStateException ("not loaded");
      }
    HashMap attr = new HashMap();
    attr.put(IKeyring.KEYRING_DATA_OUT, out);
    attr.put(IKeyring.KEYRING_PASSWORD, password);
    keyring.store(attr);
  }

  public int engineSize()
  {
    if (!loaded)
      {
        throw new IllegalStateException ("not loaded");
      }
    if (keyring == null)
      {
        return 0;
      }
    return keyring.size();
  }
}