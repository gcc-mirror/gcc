/* RC2ParameterSpec.java -- Wrapper for RC2 parameters.
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
 * A wrapper for parameters for the <a
 * href="http://www.rsasecurity.com/rsalabs/faq/3-6-2.html">RC2</a>
 * block cipher ("RC" means either "Rivest Cipher" or "Ron's Code",
 * depending upon who you ask and when).
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class RC2ParameterSpec implements AlgorithmParameterSpec
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** The length of an RC2 IV, in bytes. */
  private static final int RC2_IV_LENGTH = 8;

  /** The effective key length, in bits. */
  private int effectiveKeyBits;

  /** The initialization vector. */
  private byte[] iv;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create RC2 parameters without an IV.
   *
   * @param effectiveKeyBits The number of effective key bits.
   */
  public RC2ParameterSpec(int effectiveKeyBits)
  {
    this.effectiveKeyBits = effectiveKeyBits;
  }

  /**
   * Create RC2 parameters with an IV.
   *
   * @param effectiveKeyBits The number of effective key bits.
   * @param iv               The IV; the first eight bytes of this array
   *                         are used.
   */
  public RC2ParameterSpec(int effectiveKeyBits, byte[] iv)
  {
    this(effectiveKeyBits, iv, 0);
  }

  /**
   * Create RC2 parameters with an IV.
   *
   * @param effectiveKeyBits The number of effective key bits.
   * @param iv               The IV; the first eight bytes of this array
   *                         after <code>offset</code> are used.
   * @param offset           From whence to start in the array.
   */
  public RC2ParameterSpec(int effectiveKeyBits, byte[] iv, int offset)
  {
    if (iv.length - offset < RC2_IV_LENGTH)
      {
        throw new IllegalArgumentException("IV too short");
      }
    this.effectiveKeyBits = effectiveKeyBits;
    this.iv = new byte[RC2_IV_LENGTH];
    System.arraycopy(iv, offset, this.iv, 0, RC2_IV_LENGTH);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Get the number of effective key bits.
   *
   * @return The numer of effective key bits.
   */
  public int getEffectiveKeyBits()
  {
    return effectiveKeyBits;
  }

  /**
   * Return the initialization vector, or <code>null</code> if none was
   * specified.
   *
   * @return The IV, or null.
   */
  public byte[] getIV()
  {
    return iv;
  }

  public boolean equals(Object o)
  {
    if (this == o) return true;
    byte[] oiv = ((RC2ParameterSpec) o).getIV();
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
    return effectiveKeyBits == ((RC2ParameterSpec) o).getEffectiveKeyBits();
  }

  public int hashCode()
  {
    int code = effectiveKeyBits;
    if (iv != null)
      {
        for (int i = 0; i < RC2_IV_LENGTH; i++)
          {
            code += iv[i];
          }
      }
    return code;
  }
}
