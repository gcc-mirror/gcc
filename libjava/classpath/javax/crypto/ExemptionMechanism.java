/* ExemptionMechanism.java -- Generic crypto-weakening mechanism.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.crypto;

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;

/**
 * An exemption mechanism, which will conditionally allow cryptography
 * where it is not normally allowed, implements things such as <i>key
 * recovery</i>, <i>key weakening</i>, or <i>key escrow</i>.
 *
 * <p><b>Implementation note</b>: this class is present for
 * API-compatibility only; it is not actually used anywhere in this library
 * and this library does not, in general, support crypto weakening.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class ExemptionMechanism
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private static final String SERVICE = "ExemptionMechanism";
  private ExemptionMechanismSpi emSpi;
  private Provider provider;
  private String mechanism;
  private boolean virgin;

  // Constructor.
  // ------------------------------------------------------------------------

  protected ExemptionMechanism(ExemptionMechanismSpi emSpi, Provider provider,
                               String mechanism)
  {
    this.emSpi = emSpi;
    this.provider = provider;
    this.mechanism = mechanism;
    virgin = true;
  }

  /**
   * Create an instance of <code>ExemptionMechanism</code> for a designated
   * <code>mechanism</code> from the first Security Provider offering it.
   * 
   * @param mechanism the name of the exemption mechanism to create.
   * @return a newly created instance of <code>ExemptionMechanism</code>.
   * @throws IllegalArgumentException if the provider is null.
   * @throws NoSuchAlgorithmException if no such exemption mechanism is
   *           available from any known Security Provider.
   * @throws IllegalArgumentException if <code>mechanism</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final ExemptionMechanism getInstance(String mechanism)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(mechanism, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(mechanism);
  }

  /**
   * Create an instance of <code>ExemptionMechanism</code> for a designated
   * <code>mechanism</code> from a named <code>provider</code>.
   * 
   * @param mechanism the name of the exemption mechanism to create.
   * @param provider the security provider to provide the exemption
   *          <code>mechanism</code>.
   * @return a newly created instance of <code>ExemptionMechanism</code>.
   * @throws NoSuchAlgorithmException if no such exemption mechanism is
   *           available from the named <code>provider</code>.
   * @throws NoSuchProviderException if no Security Provider with the designated
   *           name is known to the underlying JVM.
   * @throws IllegalArgumentException if either <code>mechanism</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>mechanism</code> is an empty string.
   */
  public static final ExemptionMechanism getInstance(String mechanism,
                                                     String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(mechanism, p);
  }

  /**
   * Create an instance of <code>ExemptionMechanism</code> for a designated
   * <code>mechanism</code> from a designated <code>provider</code>.
   * 
   * @param mechanism the name of the exemption mechanism to create.
   * @param provider the security provider to provide the exemption
   *          <code>mechanism</code>.
   * @return a newly created instance of <code>ExemptionMechanism</code>.
   * @throws NoSuchAlgorithmException if an exemption mechanism could not be
   *           created.
   * @throws IllegalArgumentException if either <code>mechanism</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>mechanism</code> is an empty string.
   */
  public static final ExemptionMechanism getInstance(String mechanism,
                                                     Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("ExemptionMechanism [")
        .append(mechanism).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SERVICE, mechanism, provider);
        return new ExemptionMechanism((ExemptionMechanismSpi) spi,
                                      provider,
                                      mechanism);
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

  public final byte[] genExemptionBlob()
    throws IllegalStateException, ExemptionMechanismException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return emSpi.engineGenExemptionBlob();
  }

  public final int genExemptionBlob(byte[] output)
    throws IllegalStateException, ExemptionMechanismException,
           ShortBufferException
  {
    return genExemptionBlob(output, 0);
  }

  public final int genExemptionBlob(byte[] output, int outputOffset)
    throws IllegalStateException, ExemptionMechanismException,
           ShortBufferException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return emSpi.engineGenExemptionBlob(output, outputOffset);
  }

  public final String getName()
  {
    return mechanism;
  }

  public final int getOutputSize(int inputLength) throws IllegalStateException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return emSpi.engineGetOutputSize(inputLength);
  }

  public final Provider getProvider()
  {
    return provider;
  }

  public final void init(Key key)
    throws ExemptionMechanismException, InvalidKeyException
  {
    emSpi.engineInit(key);
    virgin = false;
  }

  public final void init(Key key, AlgorithmParameters params)
    throws ExemptionMechanismException, InvalidAlgorithmParameterException,
           InvalidKeyException
  {
    emSpi.engineInit(key, params);
    virgin = false;
  }

  public final void init(Key key, AlgorithmParameterSpec params)
    throws ExemptionMechanismException, InvalidAlgorithmParameterException,
           InvalidKeyException
  {
    emSpi.engineInit(key, params);
    virgin = false;
  }

  public final boolean isCryptoAllowed(Key key)
    throws ExemptionMechanismException
  {
    return true;
  }

  protected void finalize()
  {
  }
}
