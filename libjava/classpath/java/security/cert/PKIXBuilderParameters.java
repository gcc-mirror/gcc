/* PKIXBuilderParameters.java -- parameters for PKIX cert path builders
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


package java.security.cert;

import gnu.java.lang.CPStringBuilder;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;

import java.util.Set;

/**
 * Parameters for building certificate paths using the PKIX algorithm.
 *
 * @see CertPathBuilder
 * @since 1.4
 */
public class PKIXBuilderParameters extends PKIXParameters
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The maximum path length. */
  private int maxPathLength;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new PKIXBuilderParameters object, populating the trusted
   * certificates set with all X.509 certificates found in the given key
   * store. All certificates found in the key store are assumed to be
   * trusted by this constructor.
   *
   * @param keystore The key store.
   * @param targetConstraints The target certificate constraints.
   * @throws KeyStoreException If the certificates cannot be retrieved
   *         from the key store.
   * @throws InvalidAlgorithmParameterException If there are no
   *         certificates in the key store.
   * @throws NullPointerException If <i>keystore</i> is null.
   */
  public PKIXBuilderParameters(KeyStore keystore,
                               CertSelector targetConstraints)
    throws KeyStoreException, InvalidAlgorithmParameterException
  {
    super(keystore);
    setTargetCertConstraints(targetConstraints);
    maxPathLength = 5;
  }

  /**
   * Create a new PKIXBuilderParameters object, populating the trusted
   * certificates set with the elements of the given set, each of which
   * must be a {@link TrustAnchor}.
   *
   * @param trustAnchors The set of trust anchors.
   * @param targetConstraints The target certificate constraints.
   * @throws InvalidAlgorithmParameterException If there are no
   *         certificates in the set.
   * @throws NullPointerException If <i>trustAnchors</i> is null.
   * @throws ClassCastException If every element in <i>trustAnchors</i>
   *         is not a {@link TrustAnchor}.
   */
  public PKIXBuilderParameters(Set<TrustAnchor> trustAnchors,
                               CertSelector targetConstraints)
    throws InvalidAlgorithmParameterException
  {
    super(trustAnchors);
    setTargetCertConstraints(targetConstraints);
    maxPathLength = 5;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the maximum length of certificate paths to build.
   *
   * <p>If this value is 0 it is taken to mean that the certificate path
   * should contain only one certificate. A value of -1 means that the
   * certificate path length is unconstrained. The default value is 5.
   *
   * @return The maximum path length.
   */
  public int getMaxPathLength()
  {
    return maxPathLength;
  }

  /**
   * Sets the maximum length of certificate paths to build.
   *
   * @param maxPathLength The new path length.
   * @throws IllegalArgumentException If <i>maxPathLength</i> is less
   *         than -1.
   */
  public void setMaxPathLength(int maxPathLength)
  {
    if (maxPathLength < -1)
      throw new IllegalArgumentException();
    this.maxPathLength = maxPathLength;
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder(super.toString());
    buf.insert(buf.length() - 2, "; Max Path Length=" + maxPathLength);
    return buf.toString();
  }
}
