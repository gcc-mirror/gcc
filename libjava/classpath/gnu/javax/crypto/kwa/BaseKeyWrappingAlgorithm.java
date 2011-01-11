/* BaseKeyWrappingAlgorithm.java -- FIXME: briefly describe file purpose
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.kwa;

import gnu.java.security.util.PRNG;

import java.security.InvalidKeyException;
import java.util.Collections;
import java.util.Map;

import javax.crypto.ShortBufferException;

/**
 * A base class to facilitate implementation of concrete Key Wrapping
 * Algorithms.
 */
public abstract class BaseKeyWrappingAlgorithm
     implements IKeyWrappingAlgorithm
{
  /** The canonical name of the key wrapping algorithm. */
  protected String name;
  /** A source of randomness if/when needed by concrete implementations. */
  private PRNG prng;

  /**
   * Protected constructor.
   *
   * @param name the key wrapping algorithm canonical name.
   */
  protected BaseKeyWrappingAlgorithm(String name)
  {
    super();
  }

  public String name()
  {
    return this.name;
  }

  public void init(Map attributes) throws InvalidKeyException
  {
    if (attributes == null)
      attributes = Collections.EMPTY_MAP;

    engineInit(attributes);
  }

  public int wrap(byte[] in, int inOffset, int length, byte[] out, int outOffset)
      throws ShortBufferException
  {
    if (outOffset < 0)
      throw new IllegalArgumentException("Output offset MUST NOT be negative");
    byte[] result = wrap(in, inOffset, length);
    if (outOffset + result.length > out.length)
      throw new ShortBufferException();
    System.arraycopy(result, 0, out, outOffset, result.length);
    return result.length;
  }

  public byte[] wrap(byte[] in, int inOffset, int length)
  {
    if (inOffset < 0)
      throw new IllegalArgumentException("Input offset MUST NOT be negative");
    if (length < 0)
      throw new IllegalArgumentException("Input length MUST NOT be negative");

    return engineWrap(in, inOffset, length);
  }

  public int unwrap(byte[] in, int inOffset, int length,
                    byte[] out, int outOffset)
      throws ShortBufferException, KeyUnwrappingException
  {
    if (outOffset < 0)
      throw new IllegalArgumentException("Output offset MUST NOT be negative");
    byte[] result = engineUnwrap(in, inOffset, length);
    if (outOffset + result.length > out.length)
      throw new ShortBufferException();
    System.arraycopy(result, 0, out, outOffset, result.length);
    return result.length;
  }

  public byte[] unwrap(byte[] in, int inOffset, int length)
      throws KeyUnwrappingException
  {
    if (inOffset < 0)
      throw new IllegalArgumentException("Input offset MUST NOT be negative");
    if (length < 0)
      throw new IllegalArgumentException("Input length MUST NOT be negative");

    return engineUnwrap(in, inOffset, length);
  }

  protected abstract void engineInit(Map attributes) throws InvalidKeyException;

  protected abstract byte[] engineWrap(byte[] in, int inOffset, int length);

  protected abstract byte[] engineUnwrap(byte[] in, int inOffset, int length)
      throws KeyUnwrappingException;

  /** @return a strong pseudo-random number generator if/when needed. */
  protected PRNG getDefaultPRNG()
  {
    if (prng == null)
      prng = PRNG.getInstance();

    return prng;
  }
}
