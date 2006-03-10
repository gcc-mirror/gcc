/* GnuPrivateKeyring.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.keyring;

import gnu.java.security.Registry;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Key;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * <p>.</p>
 */
public class GnuPrivateKeyring extends BaseKeyring implements IPrivateKeyring
{

  // Constants and variables
  // -------------------------------------------------------------------------

  public static final int USAGE = Registry.GKR_PRIVATE_KEYS
                                  | Registry.GKR_PUBLIC_CREDENTIALS;

  protected String mac;

  protected int maclen;

  protected String cipher;

  protected String mode;

  protected int keylen;

  // Constructor(s)
  // -------------------------------------------------------------------------

  public GnuPrivateKeyring(String mac, int maclen, String cipher, String mode,
                           int keylen)
  {
    keyring = new PasswordAuthenticatedEntry(mac, maclen, new Properties());
    keyring2 = new CompressedEntry(new Properties());
    keyring.add(keyring2);
    this.mac = mac;
    this.maclen = maclen;
    this.cipher = cipher;
    this.mode = mode;
    this.keylen = keylen;
  }

  public GnuPrivateKeyring()
  {
    this("HMAC-SHA-1", 20, "AES", "OFB", 16);
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  public boolean containsPrivateKey(String alias)
  {
    if (!containsAlias(alias))
      {
        return false;
      }
    List l = get(alias);
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        if (it.next() instanceof PasswordAuthenticatedEntry)
          {
            return true;
          }
      }
    return false;
  }

  public Key getPrivateKey(String alias, char[] password)
      throws UnrecoverableKeyException
  {
    if (!containsAlias(alias))
      {
        return null;
      }
    List l = get(alias);
    PasswordAuthenticatedEntry e1 = null;
    PasswordEncryptedEntry e2 = null;
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof PasswordAuthenticatedEntry)
          {
            e1 = (PasswordAuthenticatedEntry) e;
            break;
          }
      }
    if (e1 == null)
      {
        return null;
      }
    try
      {
        e1.verify(password);
      }
    catch (Exception e)
      {
        throw new UnrecoverableKeyException("authentication failed");
      }
    for (Iterator it = e1.getEntries().iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof PasswordEncryptedEntry)
          {
            e2 = (PasswordEncryptedEntry) e;
            break;
          }
      }
    if (e2 == null)
      {
        return null;
      }
    try
      {
        e2.decrypt(password);
      }
    catch (Exception e)
      {
        throw new UnrecoverableKeyException("decryption failed");
      }
    for (Iterator it = e2.get(alias).iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof PrivateKeyEntry)
          {
            return ((PrivateKeyEntry) e).getKey();
          }
      }
    return null;
  }

  public void putPrivateKey(String alias, Key key, char[] password)
  {
    if (containsPrivateKey(alias))
      {
        return;
      }
    alias = fixAlias(alias);
    Properties p = new Properties();
    p.put("alias", alias);
    PrivateKeyEntry pke = new PrivateKeyEntry(key, new Date(), p);
    PasswordEncryptedEntry enc = new PasswordEncryptedEntry(cipher, mode,
                                                            keylen,
                                                            new Properties());
    PasswordAuthenticatedEntry auth = new PasswordAuthenticatedEntry(
                                                                     mac,
                                                                     maclen,
                                                                     new Properties());
    enc.add(pke);
    auth.add(enc);
    try
      {
        enc.encode(null, password);
        auth.encode(null, password);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException(ioe.toString());
      }
    keyring.add(auth);
  }

  public boolean containsPublicKey(String alias)
  {
    if (!containsAlias(alias))
      {
        return false;
      }
    List l = get(alias);
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        if (it.next() instanceof PublicKeyEntry)
          {
            return true;
          }
      }
    return false;
  }

  public PublicKey getPublicKey(String alias)
  {
    if (!containsAlias(alias))
      {
        return null;
      }
    List l = get(alias);
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof PublicKeyEntry)
          {
            return ((PublicKeyEntry) e).getKey();
          }
      }
    return null;
  }

  public void putPublicKey(String alias, PublicKey key)
  {
    if (containsPublicKey(alias))
      {
        return;
      }
    Properties p = new Properties();
    p.put("alias", fixAlias(alias));
    add(new PublicKeyEntry(key, new Date(), p));
  }

  public boolean containsCertPath(String alias)
  {
    if (!containsAlias(alias))
      {
        return false;
      }
    List l = get(alias);
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        if (it.next() instanceof CertPathEntry)
          {
            return true;
          }
      }
    return false;
  }

  public Certificate[] getCertPath(String alias)
  {
    if (!containsAlias(alias))
      {
        return null;
      }
    List l = get(alias);
    for (Iterator it = l.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof CertPathEntry)
          {
            return ((CertPathEntry) e).getCertPath();
          }
      }
    return null;
  }

  public void putCertPath(String alias, Certificate[] path)
  {
    if (containsCertPath(alias))
      {
        return;
      }
    Properties p = new Properties();
    p.put("alias", fixAlias(alias));
    add(new CertPathEntry(path, new Date(), p));
  }

  protected void load(InputStream in, char[] password) throws IOException
  {
    if (in.read() != USAGE)
      {
        throw new MalformedKeyringException("incompatible keyring usage");
      }
    if (in.read() != PasswordAuthenticatedEntry.TYPE)
      {
        throw new MalformedKeyringException(
                                            "expecting password-authenticated entry tag");
      }
    keyring = PasswordAuthenticatedEntry.decode(new DataInputStream(in),
                                                password);
  }

  protected void store(OutputStream out, char[] password) throws IOException
  {
    out.write(USAGE);
    keyring.encode(new DataOutputStream(out), password);
  }
}
