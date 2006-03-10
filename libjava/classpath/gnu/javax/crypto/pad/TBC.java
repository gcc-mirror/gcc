/* TBC.java -- 
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.pad;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.io.PrintWriter;

/**
 * <p>The implementation of the Trailing Bit Complement (TBC) padding algorithm.</p>
 *
 * <p>In this mode, "...the data string is padded at the trailing end with the
 * complement of the trailing bit of the unpadded message: if the trailing bit
 * is <tt>1</tt>, then <tt>0</tt> bits are appended, and if the trailing bit is
 * <tt>0</tt>, then <tt>1</tt> bits are appended. As few bits are added as are
 * necessary to meet the formatting size requirement."</p>
 *
 * References:<br>
 * <a href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 * Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 * Morris Dworkin.<p>
 */
public final class TBC extends BasePad
{

  // Debugging methods and variables
  // -------------------------------------------------------------------------

  private static final String NAME = "tbc";

  private static final boolean DEBUG = false;

  private static final int debuglevel = 9;

  private static final PrintWriter err = new PrintWriter(System.out, true);

  private static void debug(String s)
  {
    err.println(">>> " + NAME + ": " + s);
  }

  // Constants and variables
  // -------------------------------------------------------------------------

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Trivial package-private constructor for use by the <i>Factory</i> class.<p>
   *
   * @see gnu.crypto.pad.PadFactory
   */
  TBC()
  {
    super(Registry.TBC_PAD);
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Implementation of abstract methods in BasePad
  // -------------------------------------------------------------------------

  public void setup()
  {
    if (blockSize < 1 || blockSize > 256)
      {
        throw new IllegalArgumentException();
      }
  }

  public byte[] pad(byte[] in, int offset, int length)
  {
    int padLength = blockSize;
    if (length % blockSize != 0)
      {
        padLength = blockSize - length % blockSize;
      }
    byte[] result = new byte[padLength];
    int lastBit = in[offset + length - 1] & 0x01;
    if (lastBit == 0)
      {
        for (int i = 0; i < padLength;)
          {
            result[i++] = 0x01;
          }
      } // else it's already set to zeroes by virtue of initialisation

    if (DEBUG && debuglevel > 8)
      {
        debug("padding: 0x" + Util.toString(result));
      }
    return result;
  }

  public int unpad(byte[] in, int offset, int length)
      throws WrongPaddingException
  {
    int limit = offset + length - 1;
    int lastBit = in[limit] & 0xFF;
    int result = 0;
    while (lastBit == (in[limit] & 0xFF))
      {
        result++;
        limit--;
      }

    if (result > length)
      {
        throw new WrongPaddingException();
      }

    if (DEBUG && debuglevel > 8)
      {
        debug("padding length: " + String.valueOf(result));
      }
    return result;
  }
}