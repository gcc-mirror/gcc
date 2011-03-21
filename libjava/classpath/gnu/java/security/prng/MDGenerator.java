/* MDGenerator.java --
   Copyright (C) 2001, 2002, 2006  Free Software Foundation, Inc.

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


package gnu.java.security.prng;

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;

import java.util.Map;

/**
 * A simple pseudo-random number generator that relies on a hash algorithm, that
 * (a) starts its operation by hashing a <code>seed</code>, and then (b)
 * continuously re-hashing its output. If no hash algorithm name is specified in
 * the {@link Map} of attributes used to initialise the instance then the
 * SHA-160 algorithm is used as the underlying hash function. Also, if no
 * <code>seed</code> is given, an empty octet sequence is used.
 */
public class MDGenerator
    extends BasePRNG
    implements Cloneable
{
  /** Property name of underlying hash algorithm for this generator. */
  public static final String MD_NAME = "gnu.crypto.prng.md.hash.name";

  /** Property name of seed material. */
  public static final String SEEED = "gnu.crypto.prng.md.seed";

  /** The underlying hash instance. */
  private IMessageDigest md;

  /** Trivial 0-arguments constructor. */
  public MDGenerator()
  {
    super(Registry.MD_PRNG);
  }

  public void setup(Map attributes)
  {
    // find out which hash to use
    String underlyingMD = (String) attributes.get(MD_NAME);
    if (underlyingMD == null)
      {
        if (md == null)
          { // happy birthday
            // ensure we have a reliable implementation of this hash
            md = HashFactory.getInstance(Registry.SHA160_HASH);
          }
        else // a clone. reset it for reuse
          md.reset();
      }
    else // ensure we have a reliable implementation of this hash
      md = HashFactory.getInstance(underlyingMD);
    // get the seeed
    byte[] seed = (byte[]) attributes.get(SEEED);
    if (seed == null)
      seed = new byte[0];

    md.update(seed, 0, seed.length);
  }

  public void fillBlock() throws LimitReachedException
  {
    IMessageDigest mdc = (IMessageDigest) md.clone();
    buffer = mdc.digest();
    md.update(buffer, 0, buffer.length);
  }

  public void addRandomByte(final byte b)
  {
    if (md == null)
      throw new IllegalStateException("not initialized");
    md.update(b);
  }

  public void addRandomBytes(final byte[] buf, final int off, final int len)
  {
    if (md == null)
      throw new IllegalStateException("not initialized");
    md.update(buf, off, len);
  }

  public Object clone() throws CloneNotSupportedException
  {
    MDGenerator result = (MDGenerator) super.clone();
    if (this.md != null)
      result.md = (IMessageDigest) this.md.clone();

    return result;
  }
}
