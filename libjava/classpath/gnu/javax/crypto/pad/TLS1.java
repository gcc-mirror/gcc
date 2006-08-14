/* TLS1.java -- TLSv1 padding scheme.
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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

/**
 * The padding scheme used by the Transport Layer Security protocol, version 1.
 * This padding scheme is used in the block-ciphered struct, e.g.:
 * <pre>
 *  block-ciphered struct {
 *    opaque content[TLSCompressed.length];
 *    opaque MAC[CipherSpec.hash_size];
 *    uint8 padding[GenericBlockCipher.padding_length];
 *    uint8 padding_length;
 *  } GenericBlockCipher;
 * </pre>
 * <p>
 * Where <i>padding_length</i> is any multiple of <i>cipher_block_size</i> -
 * ((<i>SSLCompressed.length</i> + <i>CipherSpec.hash_size</i>) %
 * <i>cipher_block_size</i>) - 1 that is less than 255. Every byte of the
 * padding must be equal to <i>padding_length</i>. That is, the end of the
 * plaintext is <i>n</i> + 1 copies of the unsigned byte <i>n</i>.
 */
public class TLS1
    extends BasePad
{
  public TLS1()
  {
    super("tls1");
  }

  public void setup()
  {
    if (blockSize <= 0 || blockSize > 255)
      throw new IllegalArgumentException("invalid block size: " + blockSize);
  }

  public byte[] pad(final byte[] in, final int off, final int len)
  {
    int padlen = blockSize - (len % blockSize);
    byte[] pad = new byte[padlen];
    for (int i = 0; i < padlen; i++)
      pad[i] = (byte)(padlen - 1);
    return pad;
  }

  public int unpad(final byte[] in, final int off, final int len)
      throws WrongPaddingException
  {
    int padlen = in[off + len - 1] & 0xFF;
    for (int i = off + (len - padlen - 1); i < off + len - 1; i++)
      if ((in[i] & 0xFF) != padlen)
        throw new WrongPaddingException();
    return padlen + 1;
  }
}
