/* CipherFactory.java --
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


package gnu.javax.crypto.cipher;

import gnu.java.security.Registry;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A <i>Factory</i> to instantiate symmetric block cipher instances.
 */
public class CipherFactory
    implements Registry
{
  /** Trivial constructor to enforce Singleton pattern. */
  private CipherFactory()
  {
    super();
  }

  /**
   * Returns an instance of a block cipher given its name.
   *
   * @param name the case-insensitive name of the symmetric-key block cipher
   *          algorithm.
   * @return an instance of the designated cipher algorithm, or
   *         <code>null</code> if none is found.
   * @exception InternalError if the implementation does not pass its self-test.
   */
  public static final IBlockCipher getInstance(String name)
  {
    if (name == null)
      return null;
    name = name.trim();
    IBlockCipher result = null;
    if (name.equalsIgnoreCase(ANUBIS_CIPHER))
      result = new Anubis();
    else if (name.equalsIgnoreCase(BLOWFISH_CIPHER))
      result = new Blowfish();
    else if (name.equalsIgnoreCase(DES_CIPHER))
      result = new DES();
    else if (name.equalsIgnoreCase(KHAZAD_CIPHER))
      result = new Khazad();
    else if (name.equalsIgnoreCase(RIJNDAEL_CIPHER)
             || name.equalsIgnoreCase(AES_CIPHER))
      result = new Rijndael();
    else if (name.equalsIgnoreCase(SERPENT_CIPHER))
      result = new Serpent();
    else if (name.equalsIgnoreCase(SQUARE_CIPHER))
      result = new Square();
    else if (name.equalsIgnoreCase(TRIPLEDES_CIPHER)
             || name.equalsIgnoreCase(DESEDE_CIPHER))
      result = new TripleDES();
    else if (name.equalsIgnoreCase(TWOFISH_CIPHER))
      result = new Twofish();
    else if (name.equalsIgnoreCase(CAST5_CIPHER)
             || (name.equalsIgnoreCase(CAST128_CIPHER)
                 || (name.equalsIgnoreCase(CAST_128_CIPHER))))
      result = new Cast5();
    else if (name.equalsIgnoreCase(NULL_CIPHER))
      result = new NullCipher();

    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * Returns a {@link Set} of symmetric key block cipher implementation names
   * supported by this <i>Factory</i>.
   *
   * @return a {@link Set} of block cipher names (Strings).
   */
  public static final Set getNames()
  {
    HashSet hs = new HashSet();
    hs.add(ANUBIS_CIPHER);
    hs.add(BLOWFISH_CIPHER);
    hs.add(DES_CIPHER);
    hs.add(KHAZAD_CIPHER);
    hs.add(RIJNDAEL_CIPHER);
    hs.add(SERPENT_CIPHER);
    hs.add(SQUARE_CIPHER);
    hs.add(TRIPLEDES_CIPHER);
    hs.add(TWOFISH_CIPHER);
    hs.add(CAST5_CIPHER);
    hs.add(NULL_CIPHER);
    return Collections.unmodifiableSet(hs);
  }
}
