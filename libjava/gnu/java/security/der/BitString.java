/* BitString.java -- Java representation of the BIT STRING type.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.security.der;

import java.math.BigInteger;
import java.util.Arrays;

/**
 * Immutable representation of a bit string, which is equivalent to a
 * byte array except some number of the rightmost bits are ignored. For
 * example, this could be the bit string:
 *
 * <pre>   00010101 11101101 11010xxx</pre>
 *
 * <p>Where the "xxx" represents three bits that should be ignored, and
 * can have any value.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class BitString implements Cloneable, Comparable
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The bits themselves. */
  private final byte[] bytes;

  /**
   * The exportable byte array. This array has the ignored bits
   * removed.
   */
  private transient byte[] externBytes;

  /** The number of bits ignored at the end of the byte array. */
  private final int ignoredBits;

  /** This bit string as a boolean array. */
  private transient boolean[] boolVal;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new bit string, shifting the given byte array if needed.
   *
   * @param bytes The byte array holding the bit string.
   * @param ignoredBits The number of bits to ignore.
   * @param doShift Pass true in this parameter if the byte array has
   * not yet been shifted left by <i>ignoredBits</i>.
   * @throws IllegalArgumentException If <i>ignoredBits</i> is negative
   * or greater than 7.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes, int ignoredBits, boolean doShift)
  {
    this(bytes, 0, bytes.length, ignoredBits, doShift);
  }

  /**
   * Create a new bit string, shifting the given byte array if needed.
   *
   * @param bytes The byte array holding the bit string.
   * @param offset The offset where the meaningful bytes begin.
   * @param length The number of meaningful bytes.
   * @param ignoredBits The number of bits to ignore.
   * @param doShift Pass true in this parameter if the byte array has
   * not yet been shifted left by <i>ignoredBits</i>.
   * @throws IllegalArgumentException If <i>ignoredBits</i> is negative
   * or greater than 7.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes, int offset, int length,
                   int ignoredBits, boolean doShift)
  {
    if (ignoredBits < 0 || ignoredBits > 7)
      throw new IllegalArgumentException();
    if (bytes == null)
      throw new NullPointerException();
    if (doShift && ignoredBits > 0)
      {
        this.externBytes = new byte[length];
        System.arraycopy(bytes, offset, externBytes, 0, length);
        this.bytes = new BigInteger(externBytes).shiftLeft(ignoredBits)
                       .toByteArray();
      }
    else
      {
        this.bytes = new byte[length];
        System.arraycopy(bytes, offset, this.bytes, 0, length);
      }
    this.ignoredBits = ignoredBits;
  }

  /**
   * Create a new bit string.
   *
   * @param bytes The byte array holding the bit string.
   * @param offset The offset where the meaningful bytes begin.
   * @param length The number of meaningful bytes.
   * @param ignoredBits The number of bits to ignore.
   * @throws IllegalArgumentException If <i>ignoredBits</i> is negative
   * or greater than 7.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes, int offset, int length, int ignoredBits)
  {
    this(bytes, offset, length, ignoredBits, false);
  }

  /**
   * Create a new bit string.
   *
   * @param bytes The byte array holding the bit string.
   * @param ignoredBits The number of bits to ignore.
   * @throws IllegalArgumentException If <i>ignoredBits</i> is negative
   * or greater than 7.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes, int ignoredBits)
  {
    this(bytes, 0, bytes.length, ignoredBits, false);
  }

  /**
   * Create a new bit string.
   *
   * @param bytes The byte array holding the bit string.
   * @param offset The offset where the meaningful bytes begin.
   * @param length The number of meaningful bytes.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes, int offset, int length)
  {
    this(bytes, offset, length, 0, false);
  }

  /**
   * Create a new bit string.
   *
   * @param bytes The byte array holding the bit string.
   * @throws NullPointerException If <i>bytes</i> is null.
   */
  public BitString(byte[] bytes)
  {
    this(bytes, 0, bytes.length, 0, false);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return this bit string as a byte array, with the ignored bits
   * trimmed off. The byte array is cloned every time this method is
   * called to prevent modification.
   *
   * @return The trimmed byte array.
   */
  public byte[] toByteArray()
  {
    if (ignoredBits == 0)
      return (byte[]) bytes.clone();
    if (externBytes == null)
      externBytes = new BigInteger(bytes).shiftRight(ignoredBits).toByteArray();
    return (byte[]) externBytes.clone();
  }

  /**
   * Returns this bit string as a byte array, with the ignored bits
   * present. The byte array is cloned every time this method is
   * called to prevent modification.
   *
   * @return The byte array.
   */
  public byte[] getShiftedByteArray()
  {
    return (byte[]) bytes.clone();
  }

  /**
   * Returns the number of ignored bits.
   *
   * @return The number of ignored bits.
   */
  public int getIgnoredBits()
  {
    return ignoredBits;
  }

  /**
   * Returns the size, in bits, of this bit string.
   *
   * @return The size of this bit string.
   */
  public int size()
  {
    return (bytes.length << 3) - ignoredBits;
  }

  /**
   * Return this bit string as a boolean array. The value returned is of
   * size {@link #size()}, and each <code>true</code> value
   * corresponding to each "1" in this bit string. The boolean array is
   * cloned before it is returned.
   *
   * @return The boolean array.
   */
  public boolean[] toBooleanArray()
  {
    if (boolVal == null)
      {
        boolVal = new boolean[size()];
        for (int i = 0, j = 7, k = 0; i < boolVal.length; i++)
          {
            boolVal[i] = (bytes[k] & 1 << j--) != 0;
            if (j < 0)
              {
                j = 7;
                k++;
              }
          }
      }
    return (boolean[]) boolVal.clone();
  }

  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException cce)
      {
        throw new InternalError(cce.getMessage());
      }
  }

  public int compareTo(Object o)
  {
    BitString that = (BitString) o;
    if (this.equals(that))
      return 0;
    if (this.bytes.length != that.bytes.length)
      return (this.bytes.length < that.bytes.length) ? -1 : 1;
    if (this.ignoredBits != that.ignoredBits)
      return (this.ignoredBits < that.ignoredBits) ? -1 : 1;
    for (int i = 0; i < this.bytes.length; i++)
      if (this.bytes[i] != that.bytes[i])
        return (this.bytes[i] < that.bytes[i]) ? -1 : 1;
    return 0; // not reached.
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof BitString))
      return false;
    BitString that = (BitString) o;
    // True for cloned instances.
    if (this.bytes == that.bytes && this.ignoredBits == that.ignoredBits)
      return true;
    if (this.ignoredBits == that.ignoredBits)
      return Arrays.equals(this.bytes, that.bytes);
    return false;
  }

  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    for (int i = 0, j = 7, k = 0; i < size(); i++)
      {
        sb.append((bytes[k] & 1 << j) != 0 ? "1" : "0");
        j--;
        if (j < 0)
          {
            j = 7;
            k++;
          }
      }
    return sb.toString();
  }
}
