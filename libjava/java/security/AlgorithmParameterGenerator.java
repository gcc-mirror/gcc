/* AlgorithmParameterGenerator.java --- Algorithm Parameter Generator
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

import java.security.spec.AlgorithmParameterSpec;

import gnu.java.security.Engine;

/**
 * <p>The <code>AlgorithmParameterGenerator</code> class is used to generate a
 * set of parameters to be used with a certain algorithm. Parameter generators
 * are constructed using the <code>getInstance()</code> factory methods (static
 * methods that return instances of a given class).</p>
 *
 * <p>The object that will generate the parameters can be initialized in two
 * different ways: in an algorithm-independent manner, or in an
 * algorithm-specific manner:</p>
 *
 * <ul>
 *  <li>The algorithm-independent approach uses the fact that all parameter
 *  generators share the concept of a <i>"size"</i> and a <i>source of
 *  randomness</i>. The measure of <i>size</i> is universally shared by all
 *  algorithm parameters, though it is interpreted differently for different
 *  algorithms. For example, in the case of parameters for the <i>DSA</i>
 *  algorithm, <i>"size"</i> corresponds to the size of the prime modulus (in
 *  bits). When using this approach, algorithm-specific parameter generation
 *  values - if any - default to some standard values, unless they can be
 *  derived from the specified size.</li>
 *  <li>The other approach initializes a parameter generator object using
 *  algorithm-specific semantics, which are represented by a set of
 *  algorithm-specific parameter generation values. To generate Diffie-Hellman
 *  system parameters, for example, the parameter generation values usually
 *  consist of the size of the prime modulus and the size of the random
 *  exponent, both specified in number of bits.</li>
 * <ul>
 *
 * <p>In case the client does not explicitly initialize the
 * <code>AlgorithmParameterGenerator</code> (via a call to an <code>init()</code>
 * method), each provider must supply (and document) a default initialization.
 * For example, the <b>GNU</b> provider uses a default modulus prime size of
 * <code>1024</code> bits for the generation of <i>DSA</i> parameters.
 *
 * @author Mark Benvenuto
 * @since 1.2
 * @see AlgorithmParameters
 * @see AlgorithmParameterSpec
 */
public class AlgorithmParameterGenerator
{
  /** Service name for algorithm parameter generators. */
  private static final String ALGORITHM_PARAMETER_GENERATOR =
    "AlgorithmParameterGenerator";

  private AlgorithmParameterGeneratorSpi paramGenSpi;
  private Provider provider;
  private String algorithm;

  /**
   * Creates an <code>AlgorithmParameterGenerator</code> object.
   *
   * @param paramGenSpi the delegate.
   * @param provider the provider.
   * @param algorithm the algorithm.
   */
  protected AlgorithmParameterGenerator(AlgorithmParameterGeneratorSpi
					paramGenSpi, Provider provider,
					String algorithm)
  {
    this.paramGenSpi = paramGenSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Returns the standard name of the algorithm this parameter generator is
   * associated with.
   *
   * @return the string name of the algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Generates an <code>AlgorithmParameterGenerator</code> object that
   * implements the specified digest algorithm. If the default provider package
   * provides an implementation of the requested digest algorithm, an instance
   * of <code>AlgorithmParameterGenerator</code> containing that implementation
   * is returned. If the algorithm is not available in the default package,
   * other packages are searched.
   *
   * @param algorithm the string name of the algorithm this parameter generator
   * is associated with.
   * @return the new <code>AlgorithmParameterGenerator</code> object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * environment.
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm)
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
   * Generates an <code>AlgorithmParameterGenerator</code> object for the
   * requested algorithm, as supplied from the specified provider, if such a
   * parameter generator is available from the provider.
   *
   * @param algorithm the string name of the algorithm.
   * @param provider the string name of the provider.
   * @return the new <code>AlgorithmParameterGenerator</code> object.
   * @throws NoSuchAlgorithmException if the <code>algorithm</code> is not
   * available from the <code>provider</code>.
   * @throws NoSuchProviderException if the <code>provider</code> is not
   * available in the environment.
   * @throws IllegalArgumentException if the <code>provider</code> name is
   * <code>null</code> or empty.
   * @see Provider
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm,
							String provider)
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
   * Generates an AlgorithmParameterGenerator object for the requested
   * algorithm, as supplied from the specified provider, if such a parameter
   * generator is available from the provider. Note: the <code>provider</code>
   * doesn't have to be registered.
   *
   * @param algorithm the string name of the algorithm.
   * @param provider the provider.
   * @return the new AlgorithmParameterGenerator object.
   * @throws NoSuchAlgorithmException if the algorithm is not available from
   * the provider.
   * @throws IllegalArgumentException if the provider is null.
   * @since 1.4
   * @see Provider
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm,
                                                        Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    try
      {
	return new AlgorithmParameterGenerator(
	  (AlgorithmParameterGeneratorSpi) Engine.getInstance(
	    ALGORITHM_PARAMETER_GENERATOR, algorithm, provider),
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
   * Returns the provider of this algorithm parameter generator object.
   *
   * @return the provider of this algorithm parameter generator object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes this parameter generator for a certain <i>size</i>. To create
   * the parameters, the {@link SecureRandom} implementation of the
   * highest-priority installed provider is used as the source of randomness.
   * (If none of the installed providers supply an implementation of
   * {@link SecureRandom}, a system-provided source of randomness is used.)
   *
   * @param size the size (number of bits).
   */
  public final void init(int size)
  {
    init(size, new SecureRandom());
  }

  /**
   * Initializes this parameter generator for a certain size and source of
   * randomness.
   *
   * @param size the size (number of bits).
   * @param random the source of randomness.
   */
  public final void init(int size, SecureRandom random)
  {
    paramGenSpi.engineInit(size, random);
  }

  /**
   * Initializes this parameter generator with a set of algorithm-specific
   * parameter generation values. To generate the parameters, the {@link
   * SecureRandom} implementation of the highest-priority installed provider is
   * used as the source of randomness. (If none of the installed providers
   * supply an implementation of {@link SecureRandom}, a system-provided source
   * of randomness is used.)
   *
   * @param genParamSpec the set of algorithm-specific parameter generation
   * values.
   * @throws InvalidAlgorithmParameterException if the given parameter
   * generation values are inappropriate for this parameter generator.
   */
  public final void init(AlgorithmParameterSpec genParamSpec)
    throws InvalidAlgorithmParameterException
  {
    init(genParamSpec, new SecureRandom());
  }

  /**
   * Initializes this parameter generator with a set of algorithm-specific
   * parameter generation values.
   *
   * @param genParamSpec the set of algorithm-specific parameter generation
   * values.
   * @param random the source of randomness.
   * @throws InvalidAlgorithmParameterException if the given parameter
   * generation values are inappropriate for this parameter generator.
   */
  public final void init(AlgorithmParameterSpec genParamSpec,
                         SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    paramGenSpi.engineInit(genParamSpec, random);
  }

  /**
   * Generates the parameters.
   *
   * @return the new {@link AlgorithmParameters} object.
   */
  public final AlgorithmParameters generateParameters()
  {
    return paramGenSpi.engineGenerateParameters();
  }
}
