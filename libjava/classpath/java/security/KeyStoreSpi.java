/* KeyStoreSpi.java --- Key Store Service Provider Interface
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.cert.CertificateException;
import java.util.Date;
import java.util.Enumeration;

/**
 * KeyStoreSpi is the Service Provider Interface (SPI) for the
 * KeyStore class. This is the interface for providers to
 * supply to implement a keystore for a particular keystore
 * type.
 *
 * @since 1.2
 * @author Mark Benvenuto
 */
public abstract class KeyStoreSpi
{
  /**
   * Constructs a new KeyStoreSpi
   */
  public KeyStoreSpi()
  {
  }

  /**
   * Returns the key associated with given alias using the
   * supplied password.
   *
   * @param alias an alias for the key to get
   * @param password password to access key with
   *
   * @return the requested key, or null otherwise
   *
   * @throws NoSuchAlgorithmException if there is no algorithm
   * for recovering the key
   * @throws UnrecoverableKeyException key cannot be reocovered
   * (wrong password).
   */
  public abstract Key engineGetKey(String alias, char[]password)
    throws NoSuchAlgorithmException, UnrecoverableKeyException;

  /**
   * Gets a Certificate chain for the specified alias.
   *
   * @param alias the alias name
   *
   * @return a chain of Certificates ( ordered from the user's
   * certificate to the Certificate Authority's ) or
   * null if the alias does not exist or there is no
   * certificate chain for the alias ( the alias refers
   * to a trusted certificate entry or there is no entry).
   */
  public abstract java.security.cert.
    Certificate[] engineGetCertificateChain(String alias);


  /**
   * Gets a Certificate for the specified alias.
   *
   * If there is a trusted certificate entry then that is returned.
   * it there is a key entry with a certificate chain then the
   * first certificate is return or else null.
   *
   * @param alias the alias name
   *
   * @return a Certificate or null if the alias does not exist
   * or there is no certificate for the alias
   */
  public abstract java.security.cert.
    Certificate engineGetCertificate(String alias);

  /**
   * Gets entry creation date for the specified alias.
   *
   * @param alias the alias name
   *
   * @returns the entry creation date or null
   */
  public abstract Date engineGetCreationDate(String alias);

  /**
   * Assign the key to the alias in the keystore, protecting it
   * with the given password. It will overwrite an existing
   * entry and if the key is a PrivateKey, also add the
   * certificate chain representing the corresponding public key.
   *
   * @param alias the alias name
   * @param key the key to add
   * @password the password to protect with
   * @param chain the certificate chain for the corresponding
   * public key
   *
   * @throws KeyStoreException if it fails
   */
  public abstract void engineSetKeyEntry(String alias, Key key,
                                         char[]password,
                                         java.security.cert.
                                         Certificate[]chain) throws
    KeyStoreException;

  /**
   * Assign the key to the alias in the keystore. It will overwrite
   * an existing entry and if the key is a PrivateKey, also
   * add the certificate chain representing the corresponding
   * public key.
   *
   * @param alias the alias name
   * @param key the key to add
   * @param chain the certificate chain for the corresponding
   * public key
   *
   * @throws KeyStoreException if it fails
   */
  public abstract void engineSetKeyEntry(String alias, byte[]key,
                                         java.security.cert.
                                         Certificate[]chain) throws
    KeyStoreException;


  /**
   * Assign the certificate to the alias in the keystore. It
   * will overwrite an existing entry.
   *
   * @param alias the alias name
   * @param cert the certificate to add
   *
   * @throws KeyStoreException if it fails
   */
  public abstract void engineSetCertificateEntry(String alias,
                                                 java.security.cert.
                                                 Certificate cert) throws
    KeyStoreException;

  /**
   * Deletes the entry for the specified entry.
   *
   * @param alias the alias name
   *
   * @throws KeyStoreException if it fails
   */
  public abstract void engineDeleteEntry(String alias)
    throws KeyStoreException;

  /**
   * Generates a list of all the aliases in the keystore.
   *
   * @return an Enumeration of the aliases
   */
  public abstract Enumeration<String> engineAliases();

  /**
   * Determines if the keystore contains the specified alias.
   *
   * @param alias the alias name
   *
   * @return true if it contains the alias, false otherwise
   */
  public abstract boolean engineContainsAlias(String alias);

  /**
   * Returns the number of entries in the keystore.
   *
   * @returns the number of keystore entries.
   */
  public abstract int engineSize();

  /**
   * Determines if the keystore contains a key entry for
   * the specified alias.
   *
   * @param alias the alias name
   *
   * @return true if it is a key entry, false otherwise
   */
  public abstract boolean engineIsKeyEntry(String alias);

  /**
   * Determines if the keystore contains a certificate entry for
   * the specified alias.
   *
   * @param alias the alias name
   *
   * @return true if it is a certificate entry, false otherwise
   */
  public abstract boolean engineIsCertificateEntry(String alias);

  /**
   * Determines if the keystore contains the specified certificate
   * entry and returns the alias.
   *
   * It checks every entry and for a key entry checks only the
   * first certificate in the chain.
   *
   * @param cert Certificate to look for
   *
   * @return alias of first matching certificate, null if it
   * does not exist.
   */
  public abstract String engineGetCertificateAlias(java.security.cert.
                                                   Certificate cert);

  /**
   * Stores the keystore in the specified output stream and it
   * uses the specified key it keep it secure.
   *
   * @param stream the output stream to save the keystore to
   * @param password the password to protect the keystore integrity with
   *
   * @throws IOException if an I/O error occurs.
   * @throws NoSuchAlgorithmException the data integrity algorithm
   * used cannot be found.
   * @throws CertificateException if any certificates could not be
   * stored in the output stream.
   */
  public abstract void engineStore(OutputStream stream, char[]password)
    throws IOException, NoSuchAlgorithmException, CertificateException;


  /**
   * Loads the keystore from the specified input stream and it
   * uses the specified password to check for integrity if supplied.
   *
   * @param stream the input stream to load the keystore from
   * @param password the password to check the keystore integrity with
   *
   * @throws IOException if an I/O error occurs.
   * @throws NoSuchAlgorithmException the data integrity algorithm
   * used cannot be found.
   * @throws CertificateException if any certificates could not be
   * stored in the output stream.
   */
  public abstract void engineLoad(InputStream stream, char[]password)
    throws IOException, NoSuchAlgorithmException, CertificateException;
}
