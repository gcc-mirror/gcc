/* ModeStage.java -- 
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

import gnu.javax.crypto.mode.IMode;

import java.security.InvalidKeyException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * An {@link IMode} {@link Stage} in a {@link Cascade} Cipher chain.
 * <p>
 * Such a stage wraps an implementation of a Block Cipher Mode of Operation
 * ({@link IMode}) to allow inclusion of such an instance in a cascade of block
 * ciphers.
 */
class ModeStage
    extends Stage
{
  private IMode delegate;

  private transient Set cachedBlockSizes;

  ModeStage(IMode mode, Direction forwardDirection)
  {
    super(forwardDirection);

    delegate = mode;
    cachedBlockSizes = null;
  }

  public Set blockSizes()
  {
    if (cachedBlockSizes == null)
      {
        HashSet result = new HashSet();
        for (Iterator it = delegate.blockSizes(); it.hasNext();)
          result.add(it.next());
        cachedBlockSizes = Collections.unmodifiableSet(result);
      }
    return cachedBlockSizes;
  }

  void initDelegate(Map attributes) throws InvalidKeyException
  {
    Direction flow = (Direction) attributes.get(DIRECTION);
    attributes.put(IMode.STATE,
                   Integer.valueOf(flow.equals(forward) ? IMode.ENCRYPTION
                                                        : IMode.DECRYPTION));
    delegate.init(attributes);
  }

  public int currentBlockSize() throws IllegalStateException
  {
    return delegate.currentBlockSize();
  }

  void resetDelegate()
  {
    delegate.reset();
  }

  void updateDelegate(byte[] in, int inOffset, byte[] out, int outOffset)
  {
    delegate.update(in, inOffset, out, outOffset);
  }

  public boolean selfTest()
  {
    return delegate.selfTest();
  }
}
