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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Key;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 */
public class GnuPrivateKeyring
    extends BaseKeyring
    implements IPrivateKeyring
{
  private static final Logger log = Logger.getLogger(GnuPrivateKeyring.class.getName());
  public static final int USAGE = Registry.GKR_PRIVATE_KEYS
                                  | Registry.GKR_PUBLIC_CREDENTIALS;
  protected String mac;
  protected int maclen;
  protected String cipher;
  protected String mode;
  protected int keylen;

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

  public boolean containsPrivateKey(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "containsPrivateKey", alias);
    boolean result = false;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        if (it.next() instanceof PasswordAuthenticatedEntry)
          {
            result = true;
            break;
          }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "containsPrivateKey",
                  Boolean.valueOf(result));
    return result;
  }

  public Key getPrivateKey(String alias, char[] password)
      throws UnrecoverableKeyException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getPrivateKey", alias);
    Key result = null;
    if (containsAlias(alias))
      {
        PasswordAuthenticatedEntry e1 = null;
        for (Iterator it = get(alias).iterator(); it.hasNext();)
          {
            Entry e = (Entry) it.next();
            if (Configuration.DEBUG)
              log.finest("Entry: " + e);
            if (e instanceof PasswordAuthenticatedEntry)
              {
                e1 = (PasswordAuthenticatedEntry) e;
                break;
              }
          }
        if (Configuration.DEBUG)
          log.fine("e1 = " + e1);
        if (e1 != null)
          {
            try
              {
                e1.verify(password);
              }
            catch (Exception e)
              {
                if (Configuration.DEBUG)
                  log.throwing(this.getClass().getName(), "getPrivateKey", e);
                throw new UnrecoverableKeyException("authentication failed");
              }
            PasswordEncryptedEntry e2 = null;
            for (Iterator it = e1.getEntries().iterator(); it.hasNext();)
              {
                Entry e = (Entry) it.next();
                if (e instanceof PasswordEncryptedEntry)
                  {
                    e2 = (PasswordEncryptedEntry) e;
                    break;
                  }
              }
            if (e2 != null)
              {
                try
                  {
                    e2.decrypt(password);
                  }
                catch (Exception e)
                  {
                    log.throwing(this.getClass().getName(), "getPrivateKey", e);
                    throw new UnrecoverableKeyException("decryption failed");
                  }
                for (Iterator it = e2.get(alias).iterator(); it.hasNext();)
                  {
                    Entry e = (Entry) it.next();
                    if (e instanceof PrivateKeyEntry)
                      {
                        result = ((PrivateKeyEntry) e).getKey();
                        break;
                      }
                  }
              }
          }
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getPrivateKey",
                  result == null ? "null" : result.getClass().getName());
    return result;
  }

  public void putPrivateKey(String alias, Key key, char[] password)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "putPrivateKey",
                   new Object[] { alias, key.getClass().getName() });
    if (! containsPrivateKey(alias))
      {
        alias = fixAlias(alias);
        Properties p = new Properties();
        p.put("alias", alias);
        PrivateKeyEntry pke = new PrivateKeyEntry(key, new Date(), p);
        if (Configuration.DEBUG)
          log.fine("About to encrypt the key...");
        PasswordEncryptedEntry enc;
        enc = new PasswordEncryptedEntry(cipher, mode, keylen, new Properties());
        enc.add(pke);
        try
          {
            enc.encode(null, password);
          }
        catch (IOException x)
          {
            if (Configuration.DEBUG)
              log.log(Level.FINE, "Exception while encrypting the key. "
                                  + "Rethrow as IllegalArgumentException", x);
            throw new IllegalArgumentException(x.toString());
          }
        if (Configuration.DEBUG)
          log.fine("About to authenticate the encrypted key...");
        PasswordAuthenticatedEntry auth;
        auth = new PasswordAuthenticatedEntry(mac, maclen, new Properties());
        auth.add(enc);
        try
          {
            auth.encode(null, password);
          }
        catch (IOException x)
          {
            if (Configuration.DEBUG)
              log.log(Level.FINE, "Exception while authenticating the encrypted "
                                  + "key. Rethrow as IllegalArgumentException", x);
            throw new IllegalArgumentException(x.toString());
          }
        keyring.add(auth);
      }
    else if (Configuration.DEBUG)
      log.fine("Keyring already contains alias: " + alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "putPrivateKey");
  }

  public boolean containsPublicKey(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "containsPublicKey", alias);
    boolean result = false;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        if (it.next() instanceof PublicKeyEntry)
          {
            result = true;
            break;
          }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "containsPublicKey",
                  Boolean.valueOf(result));
    return result;
  }

  public PublicKey getPublicKey(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getPublicKey", alias);
    PublicKey result = null;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        {
          Entry e = (Entry) it.next();
          if (e instanceof PublicKeyEntry)
            {
              result = ((PublicKeyEntry) e).getKey();
              break;
            }
        }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getPublicKey",
                  result == null ? "null" : result.getClass().getName());
    return result;
  }

  public void putPublicKey(String alias, PublicKey key)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "putPublicKey",
                   new Object[] { alias, key.getClass().getName() });
    if (! containsPublicKey(alias))
      {
        Properties p = new Properties();
        p.put("alias", fixAlias(alias));
        add(new PublicKeyEntry(key, new Date(), p));
      }
    else if (Configuration.DEBUG)
      log.fine("Keyring already contains alias: " + alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "putPublicKey");
  }

  public boolean containsCertPath(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "containsCertPath", alias);
    boolean result = false;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        if (it.next() instanceof CertPathEntry)
          {
            result = true;
            break;
          }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "containsCertPath",
                  Boolean.valueOf(result));
    return result;
  }

  public Certificate[] getCertPath(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getCertPath", alias);
    Certificate[] result = null;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        {
          Entry e = (Entry) it.next();
          if (e instanceof CertPathEntry)
            {
              result = ((CertPathEntry) e).getCertPath();
              break;
            }
        }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getCertPath", result);
    return result;
  }

  public void putCertPath(String alias, Certificate[] path)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "putCertPath",
                   new Object[] { alias, path });
    if (! containsCertPath(alias))
      {
        Properties p = new Properties();
        p.put("alias", fixAlias(alias));
        add(new CertPathEntry(path, new Date(), p));
      }
    else if (Configuration.DEBUG)
      log.fine("Keyring already contains alias: " + alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "putCertPath");
  }

  protected void load(InputStream in, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "load");
    if (in.read() != USAGE)
      throw new MalformedKeyringException("incompatible keyring usage");
    if (in.read() != PasswordAuthenticatedEntry.TYPE)
      throw new MalformedKeyringException(
          "expecting password-authenticated entry tag");
    keyring = PasswordAuthenticatedEntry.decode(new DataInputStream(in), password);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "load");
  }

  protected void store(OutputStream out, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "store");
    out.write(USAGE);
    keyring.encode(new DataOutputStream(out), password);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "store");
  }
}
