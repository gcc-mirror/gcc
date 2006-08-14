/* TMMHParameterSpec.java -- 
   Copyright (C) 2002, 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.jce.spec;

import gnu.java.security.prng.IRandom;

import java.security.spec.AlgorithmParameterSpec;

/**
 * This class represents the algorithm parameters for the Truncated
 * Multi-Modular Hash function for use with JCE-derived instances of
 * {@link gnu.javax.crypto.mac.TMMH16}.
 * <p>
 * This class is little more than a container for the key stream, tag length,
 * and prefix parameters for the TMMH algorithm.
 */
public class TMMHParameterSpec
    implements AlgorithmParameterSpec
{
  /** The keystream. */
  protected IRandom keystream;
  /** The tag length. */
  protected Integer tagLength;
  /** The prefix. */
  protected byte[] prefix;

  /**
   * Create a new parameter specification.
   * 
   * @param keystream The (PRNG) key stream.
   * @param tagLength The tag length.
   * @param prefix The prefix.
   */
  public TMMHParameterSpec(IRandom keystream, Integer tagLength, byte[] prefix)
  {
    this.keystream = keystream;
    this.tagLength = tagLength;
    this.prefix = prefix;
  }

  /**
   * Create a new parameter specification with no prefix.
   * 
   * @param keystream The (PRNG) key stream.
   * @param tagLength The tag length.
   */
  public TMMHParameterSpec(IRandom keystream, Integer tagLength)
  {
    this(keystream, tagLength, null);
  }

  /**
   * Return the key stream this specification was initialized with.
   * 
   * @return The key stream.
   */
  public IRandom getKeystream()
  {
    return keystream;
  }

  /**
   * Return the tag length this specification was initialized with.
   * 
   * @return The tag length.
   */
  public Integer getTagLength()
  {
    return tagLength;
  }

  /**
   * Return the prefix, or <code>null</code> if no prefix was specified.
   * 
   * @return The prefix.
   */
  public byte[] getPrefix()
  {
    return prefix;
  }
}
