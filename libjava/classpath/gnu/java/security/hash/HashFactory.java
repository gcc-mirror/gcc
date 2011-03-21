/* HashFactory.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.hash;

import gnu.java.security.Registry;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A <i>Factory</i> to instantiate message digest algorithm instances.
 */
public class HashFactory
{
  /** Trivial constructor to enforce <i>Singleton</i> pattern. */
  private HashFactory()
  {
    super();
  }

  /**
   * Return an instance of a hash algorithm given its name.
   *
   * @param name the name of the hash algorithm.
   * @return an instance of the hash algorithm, or null if none found.
   * @exception InternalError if the implementation does not pass its self-
   *              test.
   */
  public static IMessageDigest getInstance(String name)
  {
    if (name == null)
      return null;

    name = name.trim();
    IMessageDigest result = null;
    if (name.equalsIgnoreCase(Registry.WHIRLPOOL_HASH))
      result = new Whirlpool();
    else if (name.equalsIgnoreCase(Registry.RIPEMD128_HASH)
             || name.equalsIgnoreCase(Registry.RIPEMD_128_HASH))
      result = new RipeMD128();
    else if (name.equalsIgnoreCase(Registry.RIPEMD160_HASH)
             || name.equalsIgnoreCase(Registry.RIPEMD_160_HASH))
      result = new RipeMD160();
    else if (name.equalsIgnoreCase(Registry.SHA160_HASH)
             || name.equalsIgnoreCase(Registry.SHA_1_HASH)
             || name.equalsIgnoreCase(Registry.SHA1_HASH)
             || name.equalsIgnoreCase(Registry.SHA_HASH))
      result = new Sha160();
    else if (name.equalsIgnoreCase(Registry.SHA256_HASH))
      result = new Sha256();
    else if (name.equalsIgnoreCase(Registry.SHA384_HASH))
      result = new Sha384();
    else if (name.equalsIgnoreCase(Registry.SHA512_HASH))
      result = new Sha512();
    else if (name.equalsIgnoreCase(Registry.TIGER_HASH))
      result = new Tiger();
    else if (name.equalsIgnoreCase(Registry.HAVAL_HASH))
      result = new Haval();
    else if (name.equalsIgnoreCase(Registry.MD5_HASH))
      result = new MD5();
    else if (name.equalsIgnoreCase(Registry.MD4_HASH))
      result = new MD4();
    else if (name.equalsIgnoreCase(Registry.MD2_HASH))
      result = new MD2();
    else if (name.equalsIgnoreCase(Registry.HAVAL_HASH))
      result = new Haval();

    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * Returns a {@link Set} of names of hash algorithms supported by this
   * <i>Factory</i>.
   *
   * @return a {@link Set} of hash names (Strings).
   */
  public static final Set getNames()
  {
    HashSet hs = new HashSet();
    hs.add(Registry.WHIRLPOOL_HASH);
    hs.add(Registry.RIPEMD128_HASH);
    hs.add(Registry.RIPEMD160_HASH);
    hs.add(Registry.SHA160_HASH);
    hs.add(Registry.SHA256_HASH);
    hs.add(Registry.SHA384_HASH);
    hs.add(Registry.SHA512_HASH);
    hs.add(Registry.TIGER_HASH);
    hs.add(Registry.HAVAL_HASH);
    hs.add(Registry.MD5_HASH);
    hs.add(Registry.MD4_HASH);
    hs.add(Registry.MD2_HASH);

    return Collections.unmodifiableSet(hs);
  }
}
