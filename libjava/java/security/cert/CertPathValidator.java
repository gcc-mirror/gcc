/* CertPathValidator -- validates certificate paths.
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

import gnu.java.security.Engine;

import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.security.Security;

/**
 * Generic interface to classes that validate certificate paths.
 *
 * <p>Using this class is similar to all the provider-based security
 * classes; the method of interest, {@link
 * #validate(java.security.cert.CertPath,java.security.cert.CertPathParameters)},
 * which takes provider-specific implementations of {@link
 * CertPathParameters}, and return provider-specific implementations of
 * {@link CertPathValidatorResult}.
 *
 * @since JDK 1.4
 * @see CertPath
 */
public class CertPathValidator {

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Service name for CertPathValidator. */
  private static final String CERT_PATH_VALIDATOR = "CertPathValidator";

  /** The underlying implementation. */
  private final CertPathValidatorSpi validatorSpi;

  /** The provider of this implementation. */
  private final Provider provider;

  /** The algorithm's name. */
  private final String algorithm;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Creates a new CertPathValidator.
   *
   * @param validatorSpi The underlying implementation.
   * @param provider     The provider of the implementation.
   * @param algorithm    The algorithm name.
   */
  protected CertPathValidator(CertPathValidatorSpi validatorSpi,
                              Provider provider, String algorithm)
  {
    this.validatorSpi = validatorSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the default validator type.
   *
   * <p>This value may be set at run-time via the security property
   * "certpathvalidator.type", or the value "PKIX" if this property is
   * not set.
   *
   * @return The default validator type.
   */
  public static synchronized String getDefaultType() {
    String type = (String) AccessController.doPrivileged(
      new PrivilegedAction()
        {
          public Object run()
          {
            return Security.getProperty("certpathvalidator.type");
          }
        }
    );
    if (type == null)
      type = "PKIX";
    return type;
  }

  /**
   * Get an instance of the given validator from the first provider that
   * implements it.
   *
   * @param algorithm The name of the algorithm to get.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If no installed provider
   * implements the requested algorithm.
   */
  public static CertPathValidator getInstance(String algorithm)
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
   * Get an instance of the given validator from the named provider.
   *
   * @param algorithm The name of the algorithm to get.
   * @param provider  The name of the provider from which to get the
   * implementation.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If the named provider does not
   * implement the algorithm.
   * @throws NoSuchProviderException If no provider named
   * <i>provider</i> is installed.
   */
  public static CertPathValidator getInstance(String algorithm,
                                              String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);

    return getInstance(algorithm, p);
  }

  /**
   * Get an instance of the given validator from the given provider.
   *
   * @param algorithm The name of the algorithm to get.
   * @param provider  The provider from which to get the implementation.
   * @return The new instance.
   * @throws NoSuchAlgorithmException If the provider does not implement
   * the algorithm.
   * @throws IllegalArgumentException If <i>provider</i> is null.
   */
  public static CertPathValidator getInstance(String algorithm,
                                              Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("null provider");

    try
      {
        return new CertPathValidator((CertPathValidatorSpi)
          Engine.getInstance(CERT_PATH_VALIDATOR, algorithm, provider),
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
   * Return the name of this validator.
   *
   * @return This validator's name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Return the provider of this implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Attempt to validate a certificate path.
   *
   * @param certPath The path to validate.
   * @param params   The algorithm-specific parameters.
   * @return The result of this validation attempt.
   * @throws CertPathValidatorException If the certificate path cannot
   * be validated.
   * @throws InvalidAlgorithmParameterException If this implementation
   * rejects the specified parameters.
   */
  public final CertPathValidatorResult validate(CertPath certPath,
                                                CertPathParameters params)
    throws CertPathValidatorException, InvalidAlgorithmParameterException
  {
    return validatorSpi.engineValidate(certPath, params);
  }
}
