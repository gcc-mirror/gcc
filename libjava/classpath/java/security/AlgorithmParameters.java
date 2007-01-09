/* AlgorithmParameters.java --- Algorithm Parameters Implementation Class
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

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;

/**
 * <code>AlgorithmParameters</code> is an Algorithm Parameters class which
 * provides an interface through which the user can manage the parameters of an
 * Algorithm.
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
   * Constructs a new instance of <code>AlgorithmParameters</code>.
   * 
   * @param paramSpi
   *          the engine to use.
   * @param provider
   *          the provider to use.
   * @param algorithm
   *          the algorithm to use.
   */
  protected AlgorithmParameters(AlgorithmParametersSpi paramSpi,
                                Provider provider, String algorithm)
  {
    this.paramSpi = paramSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /** @return A string with the name of the algorithm used. */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns a new instance of <code>AlgorithmParameters</code> representing
   * the specified algorithm parameters.
   * <p>
   * The returned <code>AlgorithmParameters</code> must still be initialized
   * with an <code>init()</code> method.
   * 
   * @param algorithm the algorithm to use.
   * @return the new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by any
   *           provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static AlgorithmParameters getInstance(String algorithm)
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
   * Returns a new instance of <code>AlgorithmParameters</code> representing
   * the specified algorithm parameters from a named provider.
   * <p>
   * The returned <code>AlgorithmParameters</code> must still be intialized
   * with an <code>init()</code> method.
   * </p>
   * 
   * @param algorithm the algorithm to use.
   * @param provider the name of the {@link Provider} to use.
   * @return the new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static AlgorithmParameters getInstance(String algorithm,
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
   * Returns a new instance of <code>AlgorithmParameters</code> representing
   * the specified algorithm parameters from the specified {@link Provider}.
   * <p>
   * The returned <code>AlgorithmParameters</code> must still be intialized
   * with an <code>init()</code> method.
   * 
   * @param algorithm the algorithm to use.
   * @param provider the {@link Provider} to use.
   * @return the new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   * @since 1.4
   */
  public static AlgorithmParameters getInstance(String algorithm,
                                                Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("AlgorithmParameters for algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(ALGORITHM_PARAMETERS, algorithm, provider);
        return new AlgorithmParameters((AlgorithmParametersSpi) spi,
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

  /** @return the provider of this parameter object. */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes the engine with the specified {@link AlgorithmParameterSpec}.
   * 
   * @param paramSpec
   *          A {@link AlgorithmParameterSpec} to use.
   * @throws InvalidParameterSpecException
   *           if <code>paramSpec</code> is invalid.
   */
  public final void init(AlgorithmParameterSpec paramSpec)
    throws InvalidParameterSpecException
  {
    paramSpi.engineInit(paramSpec);
  }

  /**
   * Initializes the engine with the specified parameters stored in the byte
   * array and decodes them according to the ASN.1 specification. If the ASN.1
   * specification exists then it succeeds otherwise an {@link IOException} is
   * thrown.
   * 
   * @param params
   *          the parameters to use.
   * @throws IOException
   *           if a decoding error occurs.
   */
  public final void init(byte[]params) throws IOException
  {
    paramSpi.engineInit(params);
  }

  /**
   * Initializes the engine with the specified parameters stored in the byte
   * array and decodes them according to the specified decoding specification.
   * If <code>format</code> is <code>null</code>, then this method decodes the
   * byte array using the ASN.1 specification if it exists, otherwise it throws
   * an {@link IOException}.
   * 
   * @param params
   *          the parameters to use.
   * @param format
   *          the name of decoding format to use.
   * @throws IOException
   *           if a decoding error occurs.
   */
  public final void init(byte[]params, String format) throws IOException
  {
    paramSpi.engineInit(params, format);
  }

  /**
   * Returns a new instance of <code>AlgorithmParameters</code> as a
   * designated parameter specification {@link Class}.
   * 
   * @param paramSpec
   *          the {@link Class} to use.
   * @return the parameter specification.
   * @throws InvalidParameterSpecException
   *           if <code>paramSpec</code> is invalid.
   */
  public final <T extends AlgorithmParameterSpec>
  T getParameterSpec(Class<T> paramSpec)
    throws InvalidParameterSpecException
  {
    return paramSpi.engineGetParameterSpec(paramSpec);
  }

  /**
   * Returns the parameters in the default encoding format. The primary encoding
   * format is ASN.1 if it exists for the specified type.
   * 
   * @return byte array representing the parameters.
   */
  public final byte[] getEncoded() throws IOException
  {
    return paramSpi.engineGetEncoded();
  }

  /**
   * Returns the parameters in the specified encoding format. If
   * <code>format</code> is <code>null</code> then the ASN.1 encoding
   * format is used if it exists for the specified type.
   * 
   * @param format
   *          the name of the encoding format to use.
   * @return the parameters encoded using the specified encoding scheme.
   * @throws IOException
   *           if an encoding exception occurs, or if this parameter object has
   *           not been initialized.
   */
  public final byte[] getEncoded(String format) throws IOException
  {
    return paramSpi.engineGetEncoded(format);
  }

  /**
   * Returns a string representation of the encoded form.
   * 
   * @return a string representation of the encoded form.
   */
  public final String toString()
  {
    return paramSpi.engineToString();
  }
}
