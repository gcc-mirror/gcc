/* AlgorithmParameters.java --- Algorithm Parameters Implementation Class
   Copyright (C) 1999, 2003 Free Software Foundation, Inc.

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

package java.security;

import java.security.spec.InvalidParameterSpecException;
import java.security.spec.AlgorithmParameterSpec;
import java.io.IOException;

import gnu.java.security.Engine;

/**
 * <p>This class is used as an opaque representation of cryptographic
 * parameters.</p>
 *
 * <p>An <code>AlgorithmParameters</code> object for managing the parameters
 * for a particular algorithm can be obtained by calling one of the
 * <code>getInstance()</code> factory methods (static methods that return
 * instances of a given class).</p>
 *
 * <p>There are two ways to request such an implementation: by specifying
 * either just an algorithm name, or both an algorithm name and a package
 * provider.</p>
 *
 * <ul>
 *    <li>If just an algorithm name is specified, the system will determine if
 *    there is an AlgorithmParameters implementation for the algorithm requested
 *    available in the environment, and if there is more than one, if there is
 *    a preferred one.</li>
 *    <li>If both an algorithm name and a package provider are specified, the
 *    system will determine if there is an implementation in the package
 *    requested, and throw an exception if there is not.</li>
 * </ul>
 *
 * <p>Once an <code>AlgorithmParameters</code> object is returned, it must be
 * initialized via a call to <code>init()</code>, using an appropriate
 * parameter specification or parameter encoding.</p>
 *
 * <p>A transparent parameter specification is obtained from an
 * <ocde>AlgorithmParameters</code> object via a call to
 * <code>getParameterSpec()</code>, and a byte encoding of the parameters is
 * obtained via a call to <code>getEncoded()</code>.</p>
 *
 * @author Mark Benvenuto
 * @since 1.2
 * @see AlgorithmParameterSpec
 * @see java.security.spec.DSAParameterSpec
 * @see KeyPairGenerator
 */
public class AlgorithmParameters
{
  /** Service name for algorithm parameters. */
  private static final String ALGORITHM_PARAMETERS = "AlgorithmParameters";

  private AlgorithmParametersSpi paramSpi;
  private Provider provider;
  private String algorithm;

  /**
   * Creates an <code>AlgorithmParameters</code> object.
   *
   * @param paramSpi the delegate.
   * @param provider the provider.
   * @param algorithm the algorithm.
   */
  protected AlgorithmParameters(AlgorithmParametersSpi paramSpi,
                                Provider provider, String algorithm)
  {
    this.paramSpi = paramSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Returns the name of the algorithm associated with this parameter object.
   *
   * @return the algorithm name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * <p>Generates a parameter object for the specified algorithm.</p>
   *
   * <p>If the default provider package provides an implementation of the
   * requested algorithm, an instance of <code>AlgorithmParameters</code>
   * containing that implementation is returned. If the algorithm is not
   * available in the default package, other packages are searched.</p>
   *
   * <p>The returned parameter object must be initialized via a call to
   * <code>init()</code>, using an appropriate parameter specification or
   * parameter encoding.</p>
   *
   * @param algorithm the name of the algorithm requested.
   * @return the new parameter object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * environment.
   */
  public static AlgorithmParameters getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException ignored) {}

    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * <p>Generates a parameter object for the specified algorithm, as supplied
   * by the specified provider, if such an algorithm is available from the
   * provider.</p>
   *
   * <p>The returned parameter object must be initialized via a call to
   * <code>init()</code>, using an appropriate parameter specification or
   * parameter encoding.</p>
   *
   * @param algorithm the name of the algorithm requested.
   * @param provider the name of the provider.
   * @return the new parameter object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * package supplied by the requested provider.
   * @throws NoSuchProviderException if the provider is not available in the
   * environment.
   * @throws IllegalArgumentException if the provider name is null or empty.
   * @see Provider
   */
  public static AlgorithmParameters getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null || provider.length() == 0)
      throw new IllegalArgumentException("Illegal provider");
    
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException();

    return getInstance(algorithm, p);
  }

  /**
   * Generates an <code>AlgorithmParameterGenerator</code> object for the
   * requested algorithm, as supplied from the specified provider, if such a
   * parameter generator is available from the provider. Note: the
   * <code>provider</code> doesn't have to be registered.
   *
   * @param algorithm the string name of the algorithm.
   * @param provider the provider.
   * @return the new <code>AlgorithmParameterGenerator</code> object.
   * @throws NoSuchAlgorithmException if the <code>algorithm</code> is not
   * available from the <code>provider</code>.
   * @throws IllegalArgumentException if the <code>provider</code> is
   * <code>null</code>.
   * @since 1.4
   */
  public static AlgorithmParameters getInstance(String algorithm,
                                                Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    try
      {
	return new AlgorithmParameters((AlgorithmParametersSpi)
	  Engine.getInstance(ALGORITHM_PARAMETERS, algorithm, provider),
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

  /**
   * Returns the provider of this parameter object.
   *
   * @return the provider of this parameter object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes this parameter object using the parameters specified in
   * <code>paramSpec</code>.
   *
   * @param paramSpec the parameter specification.
   * @throws InvalidParameterSpecException if the given parameter specification
   * is inappropriate for the initialization of this parameter object, or if
   * this parameter object has already been initialized.
   */
  public final void init(AlgorithmParameterSpec paramSpec)
    throws InvalidParameterSpecException
  {
    paramSpi.engineInit(paramSpec);
  }

  /**
   * Imports the specified parameters and decodes them according to the primary
   * decoding format for parameters. The primary decoding format for parameters
   * is ASN.1, if an ASN.1 specification for this type of parameters exists.
   *
   * @param params the encoded parameters.
   * @throws IOException on decoding errors, or if this parameter object has
   * already been initialized.
   */
  public final void init(byte[]params) throws IOException
  {
    paramSpi.engineInit(params);
  }

  /**
   * Imports the parameters from params and decodes them according to the
   * specified decoding scheme. If <code>format</code> is <code>null</code>,
   * the primary decoding format for parameters is used. The primary decoding
   * format is ASN.1, if an ASN.1 specification for these parameters exists.
   *
   * @param params the encoded parameters.
   * @param format the name of the decoding scheme.
   * @throws IOException on decoding errors, or if this parameter object has
   * already been initialized.
   */
  public final void init(byte[]params, String format) throws IOException
  {
    paramSpi.engineInit(params, format);
  }

  /**
   * Returns a (transparent) specification of this parameter object.
   * <code>paramSpec</code> identifies the specification class in which the
   * parameters should be returned. It could, for example, be
   * <code>DSAParameterSpec.class</code>, to indicate that the parameters should
   * be returned in an instance of the {@link java.security.spec.DSAParameterSpec}
   * class.
   *
   * @param paramSpec the specification class in which the parameters should be
   * returned.
   * @return the parameter specification.
   * @throws InvalidParameterSpecException if the requested parameter
   * specification is inappropriate for this parameter object, or if this
   * parameter object has not been initialized.
   */
  public final AlgorithmParameterSpec getParameterSpec(Class paramSpec)
    throws InvalidParameterSpecException
  {
    return paramSpi.engineGetParameterSpec(paramSpec);
  }

  /**
   * Returns the parameters in their primary encoding format. The primary
   * encoding format for parameters is ASN.1, if an ASN.1 specification for
   * this type of parameters exists.
   *
   * @return the parameters encoded using their primary encoding format.
   * @throws IOException on encoding errors, or if this parameter object has not
   * been initialized.
   */
  public final byte[] getEncoded() throws IOException
  {
    return paramSpi.engineGetEncoded();
  }

  /**
   * Returns the parameters encoded in the specified scheme. If format is
   * <code>null</code>, the primary encoding format for parameters is used. The
   * primary encoding format is ASN.1, if an ASN.1 specification for these
   * parameters exists.
   *
   * @param format the name of the encoding format.
   * @return the parameters encoded using the specified encoding scheme.
   * @throws IOException on encoding errors, or if this parameter object has
   * not been initialized.
   */
  public final byte[] getEncoded(String format) throws IOException
  {
    return paramSpi.engineGetEncoded(format);
  }

  /**
   * Returns a formatted string describing the parameters.
   *
   * @return a formatted string describing the parameters, or <code>null</code>
   * if this parameter object has not been initialized.
   */
  public final String toString()
  {
    return paramSpi.engineToString();
  }
}
