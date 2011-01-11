/* PaddingTransformer.java --
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

import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.WrongPaddingException;

import java.util.Map;

/**
 * An Adapter to use any {@link IPad} as a {@link Transformer} in an
 * {@link Assembly}.
 * <p>
 * When using such a {@link Transformer}, in an {@link Assembly}, there must
 * be at least one element behind this instance in the constructed chain;
 * otherwise, a {@link TransformerException} is thrown at initialisation time.
 */
class PaddingTransformer
    extends Transformer
{
  private IPad delegate;

  private int outputBlockSize = 1;

  PaddingTransformer(IPad padding)
  {
    super();

    this.delegate = padding;
  }

  void initDelegate(Map attributes) throws TransformerException
  {
    if (tail == null)
      {
        IllegalStateException cause = new IllegalStateException(
            "Padding transformer missing its tail!");
        throw new TransformerException("initDelegate()", cause);
      }
    outputBlockSize = tail.currentBlockSize();
    delegate.init(outputBlockSize);
  }

  int delegateBlockSize()
  {
    return outputBlockSize;
  }

  void resetDelegate()
  {
    delegate.reset();
    outputBlockSize = 1;
  }

  byte[] updateDelegate(byte[] in, int offset, int length)
      throws TransformerException
  {
    inBuffer.write(in, offset, length);
    byte[] tmp = inBuffer.toByteArray();
    inBuffer.reset();
    byte[] result;
    if (wired == Direction.FORWARD) // padding
      {
        // buffers remaining bytes from (inBuffer + in) that are less than 1
        // block
        if (tmp.length < outputBlockSize)
          {
            inBuffer.write(tmp, 0, tmp.length);
            result = new byte[0];
          }
        else
          {
            int newlen = outputBlockSize * (tmp.length / outputBlockSize);
            inBuffer.write(tmp, newlen, tmp.length - newlen);
            result = new byte[newlen];
            System.arraycopy(tmp, 0, result, 0, newlen);
          }
      }
    else // unpadding
      {
        // always keep in own buffer a max of 1 block to cater for lastUpdate
        if (tmp.length < outputBlockSize)
          {
            inBuffer.write(tmp, 0, tmp.length);
            result = new byte[0];
          }
        else
          {
            result = new byte[tmp.length - outputBlockSize];
            System.arraycopy(tmp, 0, result, 0, result.length);
            inBuffer.write(tmp, result.length, outputBlockSize);
          }
      }
    return result;
  }

  byte[] lastUpdateDelegate() throws TransformerException
  {
    byte[] result;
    // process multiples of blocksize as much as possible
    // catenate result from processing inBuffer with last-update( tail )
    if (wired == Direction.FORWARD) // padding
      {
        result = inBuffer.toByteArray();
        byte[] padding = delegate.pad(result, 0, result.length);
        inBuffer.write(padding, 0, padding.length);
      }
    else // unpadding
      {
        byte[] tmp = inBuffer.toByteArray();
        inBuffer.reset();
        int realLength;
        try
          {
            realLength = tmp.length; // should be outputBlockSize
            realLength -= delegate.unpad(tmp, 0, tmp.length);
          }
        catch (WrongPaddingException x)
          {
            throw new TransformerException("lastUpdateDelegate()", x);
          }
        inBuffer.write(tmp, 0, realLength);
      }
    result = inBuffer.toByteArray();
    inBuffer.reset();
    return result;
  }
}
