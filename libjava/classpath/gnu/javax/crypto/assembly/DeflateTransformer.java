/* DeflateTransformer.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.assembly;

import java.util.Map;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/**
 * A {@link Transformer} Adapter allowing inclusion of a DEFLATE compression
 * algorithm in an {@link Assembly} chain. The {@link Direction#FORWARD}
 * transformation is a compression (deflate) of input data, while the
 * {@link Direction#REVERSED} one is a decompression (inflate) that restores the
 * original data.
 * <p>
 * This {@link Transformer} uses a {@link Deflater} instance to carry on the
 * compression, and an {@link Inflater} to do the decompression.
 * <p>
 * When using such a {@link Transformer}, in an {@link Assembly}, there must
 * be at least one element behind this instance in the constructed chain;
 * otherwise, a {@link TransformerException} is thrown at initialisation time.
 */
class DeflateTransformer
    extends Transformer
{
  private Deflater compressor;

  private Inflater decompressor;

  private int outputBlockSize = 512; // default zlib buffer size

  private byte[] zlibBuffer;

  DeflateTransformer()
  {
    super();

  }

  void initDelegate(Map attributes) throws TransformerException
  {
    if (tail == null)
      {
        IllegalStateException cause = new IllegalStateException(
            "Compression transformer missing its tail!");
        throw new TransformerException("initDelegate()", cause);
      }
    outputBlockSize = tail.currentBlockSize();
    zlibBuffer = new byte[outputBlockSize];
    Direction flow = (Direction) attributes.get(DIRECTION);
    if (flow == Direction.FORWARD)
      compressor = new Deflater();
    else
      decompressor = new Inflater();
  }

  int delegateBlockSize()
  {
    return 1;
  }

  void resetDelegate()
  {
    compressor = null;
    decompressor = null;
    outputBlockSize = 1;
    zlibBuffer = null;
  }

  byte[] updateDelegate(byte[] in, int offset, int length)
      throws TransformerException
  {
    byte[] result;
    if (wired == Direction.FORWARD)
      {
        compressor.setInput(in, offset, length);
        while (! compressor.needsInput())
          compress();
      }
    else // decompression: inflate first and then update tail
      decompress(in, offset, length);
    result = inBuffer.toByteArray();
    inBuffer.reset();
    return result;
  }

  byte[] lastUpdateDelegate() throws TransformerException
  {
    // process multiples of blocksize as much as possible
    if (wired == Direction.FORWARD) // compressing
      {
        if (! compressor.finished())
          {
            compressor.finish();
            while (! compressor.finished())
              compress();
          }
      }
    else // decompressing
      {
        if (! decompressor.finished())
          {
            IllegalStateException cause = new IllegalStateException(
                "Compression transformer, after last update, must be finished "
                + "but isn't");
            throw new TransformerException("lastUpdateDelegate()", cause);
          }
      }
    byte[] result = inBuffer.toByteArray();
    inBuffer.reset();
    return result;
  }

  private void compress()
  {
    int len = compressor.deflate(zlibBuffer);
    if (len > 0)
      inBuffer.write(zlibBuffer, 0, len);
  }

  private void decompress(byte[] in, int offset, int length)
      throws TransformerException
  {
    decompressor.setInput(in, offset, length);
    int len = 1;
    while (len > 0)
      {
        try
          {
            len = decompressor.inflate(zlibBuffer);
          }
        catch (DataFormatException x)
          {
            throw new TransformerException("decompress()", x);
          }
        if (len > 0)
          inBuffer.write(zlibBuffer, 0, len);
      }
  }
}
