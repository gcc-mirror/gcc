/* RC5ParameterSpec.java -- parameters for RC5.
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


package javax.crypto.spec;

import java.security.spec.AlgorithmParameterSpec;

/**
 * A wrapper for parameters to the <a
 * href="http://www.rsasecurity.com/rsalabs/faq/3-6-4.html">RC5</a>
 * block cipher.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class RC5ParameterSpec implements AlgorithmParameterSpec
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The IV. */
  private byte[] iv;

  /** The number of rounds. */
  private int rounds;

  /** The version number. */
  private int version;

  /** The word size, in bits. */
  private int wordSize;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create RC5 parameters without an IV.
   *
   * @param version  The version number.
   * @param rounds   The number of rounds.
   * @param wordSize The size of a word, in bits.
   */
  public RC5ParameterSpec(int version, int rounds, int wordSize)
  {
    this.version = version;
    this.rounds = rounds;
    this.wordSize = wordSize;
  }

  /**
   * Create RC5 parameters with an IV. The bytes in <code>iv</code> in
   * the range <code>[0, 2*(wordSize/8)-1]</code> are used.
   *
   * @param version  The version number.
   * @param rounds   The number of rounds.
   * @param wordSize The size of a word, in bits.
   * @param iv       The IV data.
   */
  public RC5ParameterSpec(int version, int rounds, int wordSize, byte[] iv)
  {
    this(version, rounds, wordSize, iv, 0);
  }

  /**
   * Create RC5 parameters with an IV. The bytes in <code>iv</code> in
   * the range <code>[off, off+2*(wordSize/8)-1]</code> are used.
   *
   * @param version  The version number.
   * @param rounds   The number of rounds.
   * @param wordSize The size of a word, in bits.
   * @param iv       The IV data.
   * @param off      From where in the array the IV starts.
   */
  public
  RC5ParameterSpec(int version, int rounds, int wordSize, byte[] iv, int off)
  {
    this(version, rounds, wordSize);
    int ivLength = 2 * (wordSize / 8);
    if (off < 0)
      throw new IllegalArgumentException();
    if (iv.length - off < ivLength)
      {
        throw new IllegalArgumentException("IV too short");
      }
    this.iv = new byte[ivLength];
    System.arraycopy(iv, off, this.iv, 0, ivLength);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the initializaiton vector, or <code>null</code> if none was
   * specified.
   *
   * @return The IV, or null.
   */
  public byte[] getIV()
  {
    return iv;
  }

  /**
   * Get the number of rounds.
   *
   * @return The number of rounds.
   */
  public int getRounds()
  {
    return rounds;
  }

  /**
   * Get the version number.
   *
   * @return The version number.
   */
  public int getVersion()
  {
    return version;
  }

  /**
   * Get the word size, in bits.
   *
   * @return The word size, in bits.
   */
  public int getWordSize()
  {
    return wordSize;
  }

  public boolean equals(Object o)
  {
    if (this == o) return true;
    byte[] oiv = ((RC5ParameterSpec) o).getIV();
    if (iv != oiv)
      {
        if (iv == null || oiv == null) return false;
        if (iv.length != oiv.length) return false;
        for (int i = 0; i < iv.length; i++)
          {
            if (iv[i] != oiv[i])
              {
                return false;
              }
          }
      }
    return rounds   == ((RC5ParameterSpec) o).getRounds()
        && version  == ((RC5ParameterSpec) o).getVersion()
        && wordSize == ((RC5ParameterSpec) o).getWordSize();
  }

  public int hashCode()
  {
    int code = rounds + version + wordSize;
    if (iv != null)
      {
        for (int i = 0; i < iv.length; i++)
          {
            code += iv[i];
          }
      }
    return code;
  }
}
