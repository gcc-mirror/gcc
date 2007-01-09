/* AlgorithmParameterGenerator.java --- Algorithm Parameter Generator
   Copyright (C) 1999, 2003, 2004  Free Software Foundation, Inc.

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

import java.lang.reflect.InvocationTargetException;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <code>AlgorithmParameterGenerator</code> is used to generate algorithm
 * parameters for specified algorithms.
 * 
 * <p>In case the client does not explicitly initialize the
 * <code>AlgorithmParameterGenerator</code> (via a call to an
 * <code>init()</code> method), each provider must supply (and document) a
 * default initialization. For example, the <b>GNU</b> provider uses a default
 * modulus prime size of <code>1024</code> bits for the generation of <i>DSA</i>
 * parameters.
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
   * Constructs a new instance of <code>AlgorithmParameterGenerator</code>.
   * 
   * @param paramGenSpi
   *          the generator to use.
   * @param provider
   *          the provider to use.
   * @param algorithm
   *          the algorithm to use.
   */
  protected AlgorithmParameterGenerator(AlgorithmParameterGeneratorSpi
					paramGenSpi, Provider provider,
					String algorithm)
  {
    this.paramGenSpi = paramGenSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /** @return the name of the algorithm. */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns a new <code>AlgorithmParameterGenerator</code> instance which
   * generates algorithm parameters for the specified algorithm.
   * 
   * @param algorithm the name of algorithm to use.
   * @return the new instance.
   * @throws NoSuchAlgorithmException if <code>algorithm</code> is not
   *           implemented by any provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Returns a new <code>AlgorithmParameterGenerator</code> instance which
   * generates algorithm parameters for the specified algorithm.
   * 
   * @param algorithm the name of algorithm to use.
   * @param provider the name of the {@link Provider} to use.
   * @return the new instance.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm,
                                                        String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    provider = provider.trim();
    if (provider.length() == 0)
      throw new IllegalArgumentException("provider MUST NOT be empty");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Returns a new <code>AlgorithmParameterGenerator</code> instance which
   * generates algorithm parameters for the specified algorithm.
   * 
   * @param algorithm the name of algorithm to use.
   * @param provider the {@link Provider} to use.
   * @return the new instance.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by
   *           {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   * @since 1.4
   * @see Provider
   */
  public static AlgorithmParameterGenerator getInstance(String algorithm,
                                                        Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder()
        .append("AlgorithmParameterGenerator for algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(ALGORITHM_PARAMETER_GENERATOR,
                                        algorithm,
                                        provider);
        return new AlgorithmParameterGenerator((AlgorithmParameterGeneratorSpi) spi,
                                               provider,
                                               algorithm);
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    NoSuchAlgorithmException x = new NoSuchAlgorithmException(sb.toString());
    x.initCause(cause);
    throw x;
  }

  /** @return the {@link Provider} of this generator. */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes this instance with the specified size. Since no source of
   * randomness is supplied, a default one will be used.
   * 
   * @param size
   *          size (in bits) to use.
   */
  public final void init(int size)
  {
    init(size, new SecureRandom());
  }

  /**
   * Initializes this instance with the specified key-size and source of
   * randomness.
   * 
   * @param size
   *          the size (in bits) to use.
   * @param random
   *          the {@link SecureRandom} to use.
   */
  public final void init(int size, SecureRandom random)
  {
    paramGenSpi.engineInit(size, random);
  }

  /**
   * Initializes this instance with the specified {@link AlgorithmParameterSpec}.
   * Since no source of randomness is supplied, a default one will be used.
   * 
   * @param genParamSpec
   *          the {@link AlgorithmParameterSpec} to use.
   * @throws InvalidAlgorithmParameterException
   *           if <code>genParamSpec</code> is invalid.
   */
  public final void init(AlgorithmParameterSpec genParamSpec)
      throws InvalidAlgorithmParameterException
  {
    init(genParamSpec, new SecureRandom());
  }

  /**
   * Initializes this instance with the specified {@link AlgorithmParameterSpec}
   * and source of randomness.
   * 
   * @param genParamSpec
   *          the {@link AlgorithmParameterSpec} to use.
   * @param random
   *          the {@link SecureRandom} to use.
   * @throws InvalidAlgorithmParameterException
   *           if <code>genParamSpec</code> is invalid.
   */
  public final void init(AlgorithmParameterSpec genParamSpec,
                         SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    paramGenSpi.engineInit(genParamSpec, random);
  }

  /** @return a new instance of {@link AlgorithmParameters}. */
  public final AlgorithmParameters generateParameters()
  {
    return paramGenSpi.engineGenerateParameters();
  }
}
