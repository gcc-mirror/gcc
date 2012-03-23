/* GnuKeyring.java -- KeyStore adapter for a pair of private and public Keyrings
   Copyright (C) 2003, 2006, 2010  Free Software Foundation, Inc.

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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.javax.crypto.keyring.GnuPrivateKeyring;
import gnu.javax.crypto.keyring.GnuPublicKeyring;
import gnu.javax.crypto.keyring.IKeyring;
import gnu.javax.crypto.keyring.IPrivateKeyring;
import gnu.javax.crypto.keyring.IPublicKeyring;
import gnu.javax.crypto.keyring.MalformedKeyringException;
import gnu.javax.crypto.keyring.PrimitiveEntry;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Key;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

/**
 * An <i>Adapter</i> over a pair of one private, and one public keyrings to
 * emulate the keystore operations.
 */
public class GnuKeyring
    extends KeyStoreSpi
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(GnuKeyring.class.getName()) : null;
  private static final String NOT_LOADED = "not loaded";

  /** TRUE if the keystore is loaded; FALSE otherwise. */
  private boolean loaded;
  /** our underlying private keyring. */
  private IPrivateKeyring privateKR;
  /** our underlying public keyring. */
  private IPublicKeyring publicKR;

  // default 0-arguments constructor

  public Enumeration engineAliases()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineAliases");
    ensureLoaded();
    Enumeration result;
    if (privateKR == null)
      result = Collections.enumeration(Collections.EMPTY_SET);
    else
      {
        Set aliases = new HashSet();
        for (Enumeration e = privateKR.aliases(); e.hasMoreElements();)
          {
            String alias = (String) e.nextElement();
            if (alias != null)
              {
                alias = alias.trim();
                if (alias.length() > 0)
                  {
                    if (Configuration.DEBUG)
                      log.fine("Adding alias (from private keyring): " + alias);
                    aliases.add(alias);
                  }
              }
          }
        for (Enumeration e = publicKR.aliases(); e.hasMoreElements();)
          {
            String alias = (String) e.nextElement();
            if (alias != null)
              {
                alias = alias.trim();
                if (alias.length() > 0)
                  {
                    if (Configuration.DEBUG)
                      log.fine("Adding alias (from public keyring): " + alias);
                    aliases.add(alias);
                  }
              }
          }
        if (Configuration.DEBUG)
          log.fine("Will enumerate: " + aliases);
        result = Collections.enumeration(aliases);
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineAliases");
    return result;
  }

  public boolean engineContainsAlias(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineContainsAlias", alias);
    ensureLoaded();
    boolean inPrivateKR = privateKR.containsAlias(alias);
    if (Configuration.DEBUG)
      log.fine("inPrivateKR=" + inPrivateKR);
    boolean inPublicKR = publicKR.containsAlias(alias);
    if (Configuration.DEBUG)
      log.fine("inPublicKR=" + inPublicKR);
    boolean result = inPrivateKR || inPublicKR;
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineContainsAlias",
                  Boolean.valueOf(result));
    return result;
  }

  public void engineDeleteEntry(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineDeleteEntry", alias);
    ensureLoaded();
    if (privateKR.containsAlias(alias))
      privateKR.remove(alias);
    else if (publicKR.containsAlias(alias))
      publicKR.remove(alias);
    else if (Configuration.DEBUG)
      log.fine("Unknwon alias: " + alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineDeleteEntry");
  }

  public Certificate engineGetCertificate(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineGetCertificate", alias);
    ensureLoaded();
    Certificate result = publicKR.getCertificate(alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineGetCertificate", result);
    return result;
  }

  public String engineGetCertificateAlias(Certificate cert)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineGetCertificateAlias", cert);
    ensureLoaded();
    String result = null;
    for (Enumeration aliases = publicKR.aliases(); aliases.hasMoreElements();)
      {
        String alias = (String) aliases.nextElement();
        Certificate cert2 = publicKR.getCertificate(alias);
        if (cert.equals(cert2))
          {
            result = alias;
            break;
          }
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineGetCertificateAlias", result);
    return result;
  }

  public void engineSetCertificateEntry(String alias, Certificate cert)
      throws KeyStoreException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineSetCertificateEntry",
                   new Object[] { alias, cert });
    ensureLoaded();
    if (privateKR.containsAlias(alias))
      throw new KeyStoreException("Alias [" + alias
                                  + "] already exists and DOES NOT identify a "
                                  + "Trusted Certificate Entry");
    if (publicKR.containsCertificate(alias))
      {
        if (Configuration.DEBUG)
          log.fine("Public keyring already contains Alias [" + alias
                   + "]. Will remove it");
        publicKR.remove(alias);
      }
    publicKR.putCertificate(alias, cert);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineSetCertificateEntry");
  }

  public Certificate[] engineGetCertificateChain(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineGetCertificateChain", alias);
    ensureLoaded();
    Certificate[] result = privateKR.getCertPath(alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineGetCertificateChain", result);
    return result;
  }

  public Date engineGetCreationDate(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineGetCreationDate", alias);
    ensureLoaded();
    Date result = getCreationDate(alias, privateKR);
    if (result == null)
      result = getCreationDate(alias, publicKR);

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineGetCreationDate", result);
    return result;
  }

  public Key engineGetKey(String alias, char[] password)
      throws UnrecoverableKeyException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineGetKey", alias);
    ensureLoaded();
    Key result = null;
    if (password == null)
      {
        if (privateKR.containsPublicKey(alias))
          result = privateKR.getPublicKey(alias);
      }
    else if (privateKR.containsPrivateKey(alias))
      result = privateKR.getPrivateKey(alias, password);

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineGetKey",
                  result == null ? "null" : result.getClass().getName());
    return result;
  }

  public void engineSetKeyEntry(String alias, Key key, char[] password,
                                Certificate[] chain)
      throws KeyStoreException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineSetKeyEntry",
                   new Object[] { alias, key.getClass().getName(), chain });
    ensureLoaded();
    if (publicKR.containsAlias(alias))
      throw new KeyStoreException("Alias [" + alias
                                  + "] already exists and DOES NOT identify a "
                                  + "Key Entry");
    if (key instanceof PublicKey)
      {
        privateKR.remove(alias);
        PublicKey pk = (PublicKey) key;
        privateKR.putPublicKey(alias, pk);
      }
    else
      {
        if (! (key instanceof PrivateKey) && ! (key instanceof SecretKey))
          throw new KeyStoreException("cannot store keys of type "
                                      + key.getClass().getName());
        privateKR.remove(alias);
        privateKR.putCertPath(alias, chain);
        if (Configuration.DEBUG)
          log.fine("About to put private key in keyring...");
        privateKR.putPrivateKey(alias, key, password);
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineSetKeyEntry");
  }

  public void engineSetKeyEntry(String alias, byte[] key, Certificate[] chain)
      throws KeyStoreException
  {
    KeyStoreException x = new KeyStoreException("method not supported");
    if (Configuration.DEBUG)
      log.throwing(this.getClass().getName(), "engineSetKeyEntry(3)", x);
    throw x;
  }

  public boolean engineIsCertificateEntry(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineIsCertificateEntry", alias);
    ensureLoaded();
    boolean result = publicKR.containsCertificate(alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineIsCertificateEntry",
                  Boolean.valueOf(result));
    return result;
  }

  public boolean engineIsKeyEntry(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineIsKeyEntry", alias);
    ensureLoaded();
    boolean result = privateKR.containsPublicKey(alias)
                  || privateKR.containsPrivateKey(alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineIsKeyEntry",
                  Boolean.valueOf(result));
    return result;
  }

  public void engineLoad(InputStream in, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineLoad");
    if (in != null)
      {
        if (! in.markSupported())
          in = new BufferedInputStream(in);

        loadPrivateKeyring(in, password);
        loadPublicKeyring(in, password);
      }
    else
      createNewKeyrings();

    loaded = true;
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineLoad");
  }

  public void engineStore(OutputStream out, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineStore");
    ensureLoaded();
    HashMap attr = new HashMap();
    attr.put(IKeyring.KEYRING_DATA_OUT, out);
    attr.put(IKeyring.KEYRING_PASSWORD, password);

    privateKR.store(attr);
    publicKR.store(attr);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineStore");
  }

  public int engineSize()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineSize");
    int result = 0;
    for (Enumeration e = engineAliases(); e.hasMoreElements(); result++)
      e.nextElement();

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineSize", Integer.valueOf(result));
    return result;
  }

  /**
   * Ensure that the underlying keyring pair is loaded. Throw an exception if it
   * isn't; otherwise returns silently.
   *
   * @throws IllegalStateException if the keyring is not loaded.
   */
  private void ensureLoaded()
  {
    if (! loaded)
      throw new IllegalStateException(NOT_LOADED);
  }

  /**
   * Load the private keyring from the designated input stream.
   *
   * @param in the input stream to process.
   * @param password the password protecting the keyring.
   * @throws MalformedKeyringException if the keyring is not a private one.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  private void loadPrivateKeyring(InputStream in, char[] password)
      throws MalformedKeyringException, IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "loadPrivateKeyring");
    in.mark(5);
    for (int i = 0; i < 4; i++)
      if (in.read() != Registry.GKR_MAGIC[i])
        throw new MalformedKeyringException("incorrect magic");

    int usage = in.read();
    in.reset();
    if (usage != GnuPrivateKeyring.USAGE)
      throw new MalformedKeyringException(
          "Was expecting a private keyring but got a wrong USAGE: "
          + Integer.toBinaryString(usage));
    HashMap attr = new HashMap();
    attr.put(IKeyring.KEYRING_DATA_IN, in);
    attr.put(IKeyring.KEYRING_PASSWORD, password);
    privateKR = new GnuPrivateKeyring();
    privateKR.load(attr);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "loadPrivateKeyring");
  }

  /**
   * Load the public keyring from the designated input stream.
   *
   * @param in the input stream to process.
   * @param password the password protecting the keyring.
   * @throws MalformedKeyringException if the keyring is not a public one.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  private void loadPublicKeyring(InputStream in, char[] password)
      throws MalformedKeyringException, IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "loadPublicKeyring");
    in.mark(5);
    for (int i = 0; i < 4; i++)
      if (in.read() != Registry.GKR_MAGIC[i])
        throw new MalformedKeyringException("incorrect magic");

    int usage = in.read();
    in.reset();
    if (usage != GnuPublicKeyring.USAGE)
      throw new MalformedKeyringException(
          "Was expecting a public keyring but got a wrong USAGE: "
          + Integer.toBinaryString(usage));
    HashMap attr = new HashMap();
    attr.put(IKeyring.KEYRING_DATA_IN, in);
    attr.put(IKeyring.KEYRING_PASSWORD, password);
    publicKR = new GnuPublicKeyring();
    publicKR.load(attr);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "loadPublicKeyring");
  }

  /**
   * Return the creation date of a named alias in a designated keyring.
   *
   * @param alias the alias to look for.
   * @param keyring the keyring to search.
   * @return the creattion date of the entry named <code>alias</code>. Return
   *         <code>null</code> if <code>alias</code> was not found in
   *         <code>keyring</code>.
   */
  private Date getCreationDate(String alias, IKeyring keyring)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getCreationDate",
                   new Object[] { alias, keyring });
    Date result = null;
    if (keyring != null)
      for (Iterator it = keyring.get(alias).iterator(); it.hasNext();)
        {
          Object o = it.next();
          if (o instanceof PrimitiveEntry)
            {
              result = ((PrimitiveEntry) o).getCreationDate();
              break;
            }
        }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getCreationDate", result);
    return result;
  }

  /** Create empty keyrings. */
  private void createNewKeyrings()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "createNewKeyrings");
    privateKR = new GnuPrivateKeyring("HMAC-SHA-1", 20, "AES", "OFB", 16);
    publicKR = new GnuPublicKeyring("HMAC-SHA-1", 20);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "createNewKeyrings");
  }
}
