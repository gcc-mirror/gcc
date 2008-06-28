/* TLSHMac.java -- HMAC used in TLS.
   Copyright (C) 2001, 2002, 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.net.ssl.provider;

import java.security.InvalidKeyException;
import java.util.Map;

import gnu.java.security.hash.IMessageDigest;
import gnu.javax.crypto.mac.HMac;

/**
 * The operation of this HMac is identical to normal HMacs, but this one
 * allows keys with short lengths (including zero).
 */
class TLSHMac extends HMac
{

  // Constants.
  // -------------------------------------------------------------------------

  private static final byte IPAD_BYTE = 0x36;
  private static final byte OPAD_BYTE = 0x5C;

  // Constructor.
  // -------------------------------------------------------------------------

  TLSHMac(IMessageDigest hash)
  {
    super(hash);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void init(Map attributes)
    throws InvalidKeyException, IllegalStateException
  {
    Integer ts = (Integer) attributes.get(TRUNCATED_SIZE);
    truncatedSize = (ts == null ? macSize : ts.intValue());
    if (truncatedSize < (macSize / 2)) {
      throw new IllegalArgumentException("Truncated size too small");
    } else if (truncatedSize < 10) {
      throw new IllegalArgumentException("Truncated size less than 80 bits");
    }

    // we dont use/save the key outside this method
    byte[] K = (byte[]) attributes.get(MAC_KEY_MATERIAL);
    if (K == null) { // take it as an indication to re-use previous key if set
      if (ipadHash == null)
        {
          throw new InvalidKeyException("Null key");
        }
      // we already went through the motions; ie. up to step #4.  re-use
      underlyingHash = (IMessageDigest) ipadHash.clone();
      return;
    }

    if (K.length > blockSize)
      {
        // (0) replace K with HASH(K) if K is larger than the hash's
        //     block size. Then pad with zeros until it is the correct
        //     size (the next `if').
        underlyingHash.update(K, 0, K.length);
        K = underlyingHash.digest();
      }
    if (K.length < blockSize)
      {
        // (1) append zeros to the end of K to create a B byte string
        //     (e.g., if K is of length 20 bytes and B=64, then K will be
        //     appended with 44 zero bytes 0x00)
        int limit = (K.length > blockSize) ? blockSize : K.length;
        byte[] newK = new byte[blockSize];
        System.arraycopy(K, 0, newK, 0, limit);
        K = newK;
      }

    underlyingHash.reset();
    opadHash = (IMessageDigest) underlyingHash.clone();
    if (ipad == null)
      {
        ipad = new byte[blockSize];
      }
    // (2) XOR (bitwise exclusive-OR) the B byte string computed in step
    //     (1) with ipad
    // (3) append the stream of data 'text' to the B byte string resulting
    //     from step (2)
    // (4) apply H to the stream generated in step (3)
    for (int i = 0; i < blockSize; i++)
      {
        ipad[i] = (byte)(K[i] ^ IPAD_BYTE);
      }
    for (int i = 0; i < blockSize; i++)
      {
        opadHash.update((byte)(K[i] ^ OPAD_BYTE));
      }

    underlyingHash.update(ipad, 0, blockSize);
    ipadHash = (IMessageDigest) underlyingHash.clone();
    K = null;
  }
}
