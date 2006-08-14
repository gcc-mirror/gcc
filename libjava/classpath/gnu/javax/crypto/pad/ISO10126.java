/* ISO10126.java -- An implementation of the ISO 10126-2 padding scheme
   Copyright (C) 2006  Free Software Foundation, Inc.

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
import gnu.java.security.util.PRNG;

/**
 * The implementation of the ISO 10126-2 padding algorithm.
 * <p>
 * The last byte of the padding block is the number of padding bytes, all other
 * padding bytes are random.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://www.w3.org/TR/xmlenc-core/">XML Encryption Syntax and
 *    Processing</a> Section "5.2 Block Encryption Algorithms"; "Padding".</li>
 * </ol>
 */
public final class ISO10126
    extends BasePad
{
  /** Used to generate random numbers for padding bytes. */
  private PRNG prng;

  ISO10126()
  {
    super(Registry.ISO10126_PAD);
    prng = PRNG.getInstance();
  }

  public void setup()
  {
    // Nothing to do here
  }

  public byte[] pad(byte[] in, int offset, int length)
  {
    int padLength = blockSize - (length % blockSize);
    final byte[] pad = new byte[padLength];

    // generate random numbers for the padding bytes except for the last byte
    prng.nextBytes(pad, 0, padLength - 1);
    // the last byte contains the number of padding bytes
    pad[padLength - 1] = (byte) padLength;

    return pad;
  }

  public int unpad(byte[] in, int offset, int length)
      throws WrongPaddingException
  {
    // the last byte contains the number of padding bytes
    int padLength = in[offset + length - 1] & 0xFF;
    if (padLength > length)
      throw new WrongPaddingException();

    return padLength;
  }

  /**
   * The default self-test in the super-class would take too long to finish
   * with this type of padder --due to the large amount of random data needed.
   * We override the default test and replace it with a simple one for a 16-byte
   * block-size (default AES block-size). The Mauve test TestOfISO10126 will
   * exercise all block-sizes that the default self-test uses for the other
   * padders.
   */
  public boolean selfTest()
  {
    return test1BlockSize(16, new byte[1024]);
  }
}
