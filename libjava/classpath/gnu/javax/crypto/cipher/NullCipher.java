/* NullCipher.java -- 
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


package gnu.javax.crypto.cipher;

import gnu.java.security.Registry;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

/**
 * The implementation of a Null block cipher.
 * <p>
 * This cipher does not alter its input at all, claims to process block sizes
 * 128-, 192- and 256-bit long, and key sizes from 64- to 512-bit in 8-bit
 * increments.
 */
public final class NullCipher
    extends BaseCipher
{
  /** Trivial 0-arguments constructor. */
  public NullCipher()
  {
    super(Registry.NULL_CIPHER, 16, 16);
  }

  public Object clone()
  {
    NullCipher result = new NullCipher();
    result.currentBlockSize = this.currentBlockSize;
    return result;
  }

  public Iterator blockSizes()
  {
    ArrayList al = new ArrayList();
    al.add(Integer.valueOf(64 / 8));
    al.add(Integer.valueOf(128 / 8));
    al.add(Integer.valueOf(192 / 8));
    al.add(Integer.valueOf(256 / 8));
    return Collections.unmodifiableList(al).iterator();
  }

  public Iterator keySizes()
  {
    ArrayList al = new ArrayList();
    for (int n = 8; n < 64; n++)
      al.add(Integer.valueOf(n));
    return Collections.unmodifiableList(al).iterator();
  }

  public Object makeKey(byte[] uk, int bs) throws InvalidKeyException
  {
    return new Object();
  }

  public void encrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    System.arraycopy(in, i, out, j, bs);
  }

  public void decrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    System.arraycopy(in, i, out, j, bs);
  }

  public boolean selfTest()
  {
    return true;
  }
}
