/* KeyStore.java --- Key Store Class
   Copyright (C) 1999, 2002, 2003, 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.security;

import gnu.java.security.Engine;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.cert.CertificateException;
import java.util.Date;
import java.util.Enumeration;

/**
 * Keystore represents an in-memory collection of keys and
 * certificates. There are two types of entries:
 *
 * <dl>
 * <dt>Key Entry</dt>
 *
 * <dd><p>This type of keystore entry store sensitive crytographic key
 * information in a protected format.Typically this is a secret
 * key or a private key with a certificate chain.</p></dd>
 *
 * <dt>Trusted Ceritificate Entry</dt>
 *
 * <dd><p>This type of keystore entry contains a single public key
 * certificate belonging to annother entity. It is called trusted
 * because the keystore owner trusts that the certificates
 * belongs to the subject (owner) of the certificate.</p></dd>
 * </dl>
 *
 * <p>Entries in a key store are referred to by their "alias": a simple
 * unique string.
 *
 * <p>The structure and persistentence of the key store is not
 * specified. Any method could be used to protect sensitive
 * (private or secret) keys. Smart cards or integrated
 * cryptographic engines could be used or the keystore could
 * be simply stored in a file.</p>
 *
 * @see java.security.cert.Certificate
 * @see Key
 */
public class KeyStore
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Service name for key stores. */
  private static final String KEY_STORE = "KeyStore";

  private KeyStoreSpi keyStoreSpi;
  private Provider provider;
  private String type;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
     Creates an instance of KeyStore

     @param keyStoreSpi A KeyStore engine to use
     @param provider A provider to use
     @param type The type of KeyStore
   */
  protected KeyStore(KeyStoreSpi keyStoreSpi, Provider provider, String type)
  {
    this.keyStoreSpi = keyStoreSpi;
    this.provider = provider;
    this.type = type;
  }

  /**
   * Returns an instance of a <code>KeyStore</code> representing the specified
   * type, from the first provider that implements it.
   *
   * @param type the type of keystore to create.
   * @return a <code>KeyStore</code> repesenting the desired type.
   * @throws KeyStoreException if the designated type of is not implemented by
   *           any provider, or the implementation could not be instantiated.
   * @throws IllegalArgumentException if <code>type</code> is
   *           <code>null</code> or is an empty string.
   */
  public static KeyStore getInstance(String type) throws KeyStoreException
  {
    Provider[] p = Security.getProviders();
    KeyStoreException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(type, p[i]);
        }
      catch (KeyStoreException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new KeyStoreException(type);
  }

  /**
   * Returns an instance of a <code>KeyStore</code> representing the specified
   * type, from the named provider.
   *
   * @param type the type of keystore to create.
   * @param provider the name of the provider to use.
   * @return a <code>KeyStore</code> repesenting the desired type.
   * @throws KeyStoreException if the designated type is not implemented by the
   *           given provider.
   * @throws NoSuchProviderException if the provider is not found.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static KeyStore getInstance(String type, String provider)
    throws KeyStoreException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    provider = provider.trim();
    if (provider.length() == 0)
      throw new IllegalArgumentException("provider MUST NOT be empty");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(type, p);
  }

  /**
   * Returns an instance of a <code>KeyStore</code> representing the specified
   * type, from the specified provider.
   *
   * @param type the type of keystore to create.
   * @param provider the provider to use.
   * @return a <code>KeyStore</code> repesenting the desired type.
   * @throws KeyStoreException if the designated type is not implemented by the
   *           given provider.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   * @since 1.4
   */
  public static KeyStore getInstance(String type, Provider provider)
      throws KeyStoreException
  {
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(KEY_STORE, type, provider);
        return new KeyStore((KeyStoreSpi) spi, provider, type);
      }
    catch (NoSuchAlgorithmException x)
      {
        cause = x;
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause() != null ? x.getCause() : x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    KeyStoreException x = new KeyStoreException(type);
    x.initCause(cause);
    throw x;
  }

  /**
   * Returns the default KeyStore type. This method looks up the
   * type in &lt;JAVA_HOME&gt;/lib/security/java.security with the
   * property "keystore.type" or if that fails then "gkr" .
   */
  public static final String getDefaultType()
  {
    // Security reads every property in java.security so it
    // will return this property if it exists.
    String tmp = AccessController.doPrivileged(new PrivilegedAction<String> () {
        public String run()
        {
          return Security.getProperty("keystore.type");
        }
      });

    if (tmp == null)
      tmp = "gkr";

    return tmp;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
     Gets the provider that the class is from.

     @return the provider of this class
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Returns the type of the KeyStore supported

     @return A string with the type of KeyStore
   */
  public final String getType()
  {
    return type;
  }

  /**
     Returns the key associated with given alias using the
     supplied password.

     @param alias an alias for the key to get
     @param password password to access key with

     @return the requested key, or null otherwise

     @throws NoSuchAlgorithmException if there is no algorithm
     for recovering the key
     @throws UnrecoverableKeyException key cannot be reocovered
     (wrong password).
   */
  public final Key getKey(String alias, char[]password)
    throws KeyStoreException, NoSuchAlgorithmException,
    UnrecoverableKeyException
  {
    return keyStoreSpi.engineGetKey(alias, password);
  }

  /**
     Gets a Certificate chain for the specified alias.

     @param alias the alias name

     @return a chain of Certificates ( ordered from the user's
     certificate to the Certificate Authority's ) or
     null if the alias does not exist or there is no
     certificate chain for the alias ( the alias refers
     to a trusted certificate entry or there is no entry).
   */
  public final java.security.cert.
    Certificate[] getCertificateChain(String alias) throws KeyStoreException
  {
    return keyStoreSpi.engineGetCertificateChain(alias);
  }

  /**
     Gets a Certificate for the specified alias.

     If there is a trusted certificate entry then that is returned.
     it there is a key entry with a certificate chain then the
     first certificate is return or else null.

     @param alias the alias name

     @return a Certificate or null if the alias does not exist
     or there is no certificate for the alias
   */
  public final java.security.cert.Certificate getCertificate(String alias)
    throws KeyStoreException
  {
    return keyStoreSpi.engineGetCertificate(alias);
  }

  /**
     Gets entry creation date for the specified alias.

     @param alias the alias name

     @returns the entry creation date or null
   */
  public final Date getCreationDate(String alias) throws KeyStoreException
  {
    return keyStoreSpi.engineGetCreationDate(alias);
  }

  /**
     Assign the key to the alias in the keystore, protecting it
     with the given password. It will overwrite an existing
     entry and if the key is a PrivateKey, also add the
     certificate chain representing the corresponding public key.

     @param alias the alias name
     @param key the key to add
     @password the password to protect with
     @param chain the certificate chain for the corresponding
     public key

     @throws KeyStoreException if it fails
   */
  public final void setKeyEntry(String alias, Key key, char[]password,
                                java.security.cert.
                                Certificate[]chain) throws KeyStoreException
  {
    keyStoreSpi.engineSetKeyEntry(alias, key, password, chain);
  }

  /**
     Assign the key to the alias in the keystore. It will overwrite
     an existing entry and if the key is a PrivateKey, also
     add the certificate chain representing the corresponding
     public key.

     @param alias the alias name
     @param key the key to add
     @param chain the certificate chain for the corresponding
     public key

     @throws KeyStoreException if it fails
   */
  public final void setKeyEntry(String alias, byte[]key,
                                java.security.cert.
                                Certificate[]chain) throws KeyStoreException
  {
    keyStoreSpi.engineSetKeyEntry(alias, key, chain);
  }

  /**
     Assign the certificate to the alias in the keystore. It
     will overwrite an existing entry.

     @param alias the alias name
     @param cert the certificate to add

     @throws KeyStoreException if it fails
   */
  public final void setCertificateEntry(String alias,
                                        java.security.cert.
                                        Certificate cert) throws
    KeyStoreException
  {
    keyStoreSpi.engineSetCertificateEntry(alias, cert);
  }

  /**
     Deletes the entry for the specified entry.

     @param alias the alias name

     @throws KeyStoreException if it fails
   */
  public final void deleteEntry(String alias) throws KeyStoreException
  {
    keyStoreSpi.engineDeleteEntry(alias);
  }

  /**
     Generates a list of all the aliases in the keystore.

     @return an Enumeration of the aliases
   */
  public final Enumeration<String> aliases() throws KeyStoreException
  {
    return keyStoreSpi.engineAliases();
  }

  /**
     Determines if the keystore contains the specified alias.

     @param alias the alias name

     @return true if it contains the alias, false otherwise
   */
  public final boolean containsAlias(String alias) throws KeyStoreException
  {
    return keyStoreSpi.engineContainsAlias(alias);
  }

  /**
     Returns the number of entries in the keystore.

     @returns the number of keystore entries.
   */
  public final int size() throws KeyStoreException
  {
    return keyStoreSpi.engineSize();
  }

  /**
     Determines if the keystore contains a key entry for
     the specified alias.

     @param alias the alias name

     @return true if it is a key entry, false otherwise
   */
  public final boolean isKeyEntry(String alias) throws KeyStoreException
  {
    return keyStoreSpi.engineIsKeyEntry(alias);
  }


  /**
     Determines if the keystore contains a certificate entry for
     the specified alias.

     @param alias the alias name

     @return true if it is a certificate entry, false otherwise
   */
  public final boolean isCertificateEntry(String alias)
    throws KeyStoreException
  {
    return keyStoreSpi.engineIsCertificateEntry(alias);
  }

  /**
     Determines if the keystore contains the specified certificate
     entry and returns the alias.

     It checks every entry and for a key entry checks only the
     first certificate in the chain.

     @param cert Certificate to look for

     @return alias of first matching certificate, null if it
     does not exist.
   */
  public final String getCertificateAlias(java.security.cert.Certificate cert)
    throws KeyStoreException
  {
    return keyStoreSpi.engineGetCertificateAlias(cert);
  }

  /**
     Stores the keystore in the specified output stream and it
     uses the specified key it keep it secure.

     @param stream the output stream to save the keystore to
     @param password the password to protect the keystore integrity with

     @throws IOException if an I/O error occurs.
     @throws NoSuchAlgorithmException the data integrity algorithm
     used cannot be found.
     @throws CertificateException if any certificates could not be
     stored in the output stream.
   */
  public final void store(OutputStream stream, char[]password)
    throws KeyStoreException, IOException, NoSuchAlgorithmException,
    CertificateException
  {
    keyStoreSpi.engineStore(stream, password);
  }

  /**
     Loads the keystore from the specified input stream and it
     uses the specified password to check for integrity if supplied.

     @param stream the input stream to load the keystore from
     @param password the password to check the keystore integrity with

     @throws IOException if an I/O error occurs.
     @throws NoSuchAlgorithmException the data integrity algorithm
     used cannot be found.
     @throws CertificateException if any certificates could not be
     stored in the output stream.
   */
  public final void load(InputStream stream, char[]password)
    throws IOException, NoSuchAlgorithmException, CertificateException
  {
    keyStoreSpi.engineLoad(stream, password);
  }

}
