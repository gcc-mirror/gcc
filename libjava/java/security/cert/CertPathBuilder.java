/* CertPathBuilder.java -- bulids CertPath objects from Certificates.
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.security.Engine;

import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;

/**
 * This class builds certificate paths (also called certificate chains),
 * which can be used to establish trust for a particular certificate by
 * building a path from a trusted certificate (a trust anchor) to the
 * untrusted certificate.
 *
 * @see CertPath
 */
public class CertPathBuilder
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Service name for CertPathBuilder. */
  private static final String CERT_PATH_BUILDER = "CertPathBuilder";

  /** The underlying implementation. */
  private CertPathBuilderSpi cpbSpi;

  /** The provider of this implementation. */
  private Provider provider;

  /** The name of this implementation. */
  private String algorithm;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Creates a new CertPathBuilder.
   *
   * @param cpbSpi    The underlying implementation.
   * @param provider  The provider of the implementation.
   * @param algorithm This implementation's name.
   */
  protected CertPathBuilder(CertPathBuilderSpi cpbSpi, Provider provider,
                            String algorithm)
  {
    this.cpbSpi = cpbSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Get the default cert path builder type.
   *
   * <p>This value can be set at run-time by the security property
   * <code>"certpathbuilder.type"</code>. If this property is not set,
   * then the value returned is <code>"PKIX"</code>.
   *
   * @return The default CertPathBuilder algorithm.
   */
  public static final String getDefaultType()
  {
    String type = Security.getProperty("certpathbuilder.type");
    if (type == null)
      type = "PKIX";
    return type;
  }

  /**
   * Get an instance of a named CertPathBuilder, from the first provider
   * that implements it.
   *
   * @param algorithm The name of the CertPathBuilder to create.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If no installed provider
   *   implements the named algorithm.
   */
  public static CertPathBuilder getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();

    for (int i = 0; i < p.length; i++)
      {
        try
          {
            return getInstance(algorithm, p[i]);
          }
        catch (NoSuchAlgorithmException e)
          {
	    // Ignored.
          }
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Get an instance of a named CertPathBuilder from the named
   * provider.
   *
   * @param algorithm The name of the CertPathBuilder to create.
   * @param provider  The name of the provider from which to get the
   *   implementation.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If no installed provider
   *   implements the named algorithm.
   * @throws NoSuchProviderException If the named provider does not
   *   exist.
   */
  public static CertPathBuilder getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Get an instance of a named CertPathBuilder from the specified
   * provider.
   *
   * @param algorithm The name of the CertPathBuilder to create.
   * @param provider  The provider from which to get the implementation.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If no installed provider
   *   implements the named algorithm.
   * @throws IllegalArgumentException If <i>provider</i> in
   *   <tt>null</tt>.
   */
  public static CertPathBuilder getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("null provider");
    try
      {
        return new CertPathBuilder((CertPathBuilderSpi)
          Engine.getInstance(CERT_PATH_BUILDER, algorithm, provider),
          provider, algorithm);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
        throw new NoSuchAlgorithmException(algorithm);
      }
    catch (ClassCastException cce)
      {
        throw new NoSuchAlgorithmException(algorithm);
      }
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the name of this CertPathBuilder algorithm.
   *
   * @return The algorithm name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Return the provider of this instance's implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Builds a certificate path. The {@link CertPathParameters} parameter
   * passed to this method is implementation-specific, but in general
   * should contain some number of certificates and some number of
   * trusted certificates (or "trust anchors").
   *
   * @param params The parameters.
   * @retrun The certificate path result.
   * @throws CertPathBuilderException If the certificate path cannot be
   *   built.
   * @throws InvalidAlgorithmParameterException If the implementation
   *   rejects the specified parameters.
   */
  public final CertPathBuilderResult build(CertPathParameters params)
    throws CertPathBuilderException, InvalidAlgorithmParameterException
  {
    return cpbSpi.engineBuild(params);
  }
}
