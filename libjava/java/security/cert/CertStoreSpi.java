/* CertStoreSpi -- certificate store service provider interface.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.security.cert;

import java.util.Collection;

/**
 * The <i>service provider interface</i> (<b>SPI</b>) for the {@link
 * CertStore} class.
 *
 * <p>Providers wishing to implement a CertStore must subclass this
 * class, implementing all the abstract methods. Providers may also
 * implement the {@link CertStoreParameters} interface, if they require
 * parameters.
 *
 * @since JDK 1.4
 * @see CertStore
 * @see CollectionCertStoreParameters
 * @see LDAPCertStoreParameters
 */
public abstract class CertStoreSpi
{

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Creates a new CertStoreSpi.
   *
   * @param params The parameters to initialize this instance with, or
   *        null if no parameters are required.
   * @throws InvalidAlgorithmParameterException If the specified
   *         parameters are inappropriate for this class.
   */
  public CertStoreSpi(CertStoreParameters params)
    throws java.security.InvalidAlgorithmParameterException
  {
    super();
  }

  // Abstract methods.
  // ------------------------------------------------------------------------

  /**
   * Get the certificates from this store, filtering them through the
   * specified CertSelector.
   *
   * @param selector The CertSelector to filter certificates.
   * @return A (non-null) collection of certificates.
   * @throws CertStoreException If the certificates cannot be retrieved.
   */
  public abstract Collection engineGetCertificates(CertSelector selector)
  throws CertStoreException;

  /**
   * Get the certificate revocation list from this store, filtering them
   * through the specified CRLSelector.
   *
   * @param selector The CRLSelector to filter certificate revocation
   *        lists.
   * @return A (non-null) collection of certificate revocation list.
   * @throws CertStoreException If the CRLs cannot be retrieved.
   */
  public abstract Collection engineGetCRLs(CRLSelector selector)
  throws CertStoreException;
}
