/* CascadeTransformer.java --
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

import java.security.InvalidKeyException;
import java.util.Map;

/**
 * An Adapter to use any {@link Cascade} as a {@link Transformer} in an
 * {@link Assembly}.
 */
class CascadeTransformer
    extends Transformer
{
  private Cascade delegate;

  private int blockSize;

  CascadeTransformer(Cascade delegate)
  {
    super();

    this.delegate = delegate;
  }

  void initDelegate(Map attributes) throws TransformerException
  {
    attributes.put(Cascade.DIRECTION, wired);
    try
      {
        delegate.init(attributes);
      }
    catch (InvalidKeyException x)
      {
        throw new TransformerException("initDelegate()", x);
      }
    blockSize = delegate.currentBlockSize();
  }

  int delegateBlockSize()
  {
    return blockSize;
  }

  void resetDelegate()
  {
    delegate.reset();
    blockSize = 0;
  }

  byte[] updateDelegate(byte[] in, int offset, int length)
      throws TransformerException
  {
    byte[] result = updateInternal(in, offset, length);
    return result;
  }

  byte[] lastUpdateDelegate() throws TransformerException
  {
    if (inBuffer.size() != 0)
      {
        IllegalStateException cause = new IllegalStateException(
            "Cascade transformer, after last update, must be empty but isn't");
        throw new TransformerException("lastUpdateDelegate()", cause);
      }
    return new byte[0];
  }

  private byte[] updateInternal(byte[] in, int offset, int length)
  {
    byte[] result;
    for (int i = 0; i < length; i++)
      {
        inBuffer.write(in[offset++] & 0xFF);
        if (inBuffer.size() >= blockSize)
          {
            result = inBuffer.toByteArray();
            inBuffer.reset();
            delegate.update(result, 0, result, 0);
            outBuffer.write(result, 0, blockSize);
          }
      }
    result = outBuffer.toByteArray();
    outBuffer.reset();
    return result;
  }
}
