/* SHA.java -- Class implementing the SHA-1 algorithm as specified in [1].
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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

  
package gnu.java.security.provider;

import java.security.MessageDigest;

/**
   This class implements the SHA-1 algorithm as described in [1].

   [1] Federal Information Processing Standards Publication 180-1.
   Specifications for the Secure Hash Standard.  April 17, 1995.

   @see java.security.MessageDigest
*/
public class SHA extends MessageDigest implements Cloneable
{
  public SHA ()
  {
    super("SHA");
    engineReset ();
  }

  public int engineGetDigestLength()
  {
    return 20;
  }

  public void engineUpdate (byte b)
  {
    int i = ((int)bytecount) & 0x3f; //wgs
    int shift = (3 - i % 4) << 3;
    int idx = i / 4;

    i = (int)b;
    W[idx] = (W[idx] & ~(0xff << shift)) | ((i & 0xff) << shift);

    // if we've filled up a block, then process it
    if (((++bytecount) & 0x3f) == 0)
      munch ();
  }

  // This could be optimized.
  public void engineUpdate (byte bytes[], int off, int len)
  {
    if (len < 0)
      throw new ArrayIndexOutOfBoundsException ();

    int end = off + len;
    while (off < end)
      engineUpdate (bytes[off++]);
  }

  public void engineReset ()
  {
    bytecount = 0;
    // magic numbers from [1] p. 10.
    H0 = 0x67452301;
    H1 = 0xefcdab89;
    H2 = 0x98badcfe;
    H3 = 0x10325476;
    H4 = 0xc3d2e1f0;
  }

  public byte[] engineDigest ()
  {
    long bitcount = bytecount << 3;
    engineUpdate ((byte)0x80); // 10000000 in binary; the start of the padding

    // add the rest of the padding to fill this block out, but leave 8
    // bytes to put in the original bytecount
    while ((bytecount & 0x3f) != 56)
      engineUpdate ((byte)0);

    // add the length of the original, unpadded block to the end of
    // the padding
    W[14] = (int)(bitcount >>> 32);
    W[15] = (int)bitcount;
    bytecount += 8;

    // digest the fully padded block
    munch ();

    byte[] result
      = new byte[] {(byte)(H0 >>> 24), (byte)(H0 >>> 16),
		    (byte)(H0 >>> 8), (byte)H0,
		    (byte)(H1 >>> 24), (byte)(H1 >>> 16),
		    (byte)(H1 >>> 8), (byte)H1,
		    (byte)(H2 >>> 24), (byte)(H2 >>> 16),
		    (byte)(H2 >>> 8), (byte)H2,
		    (byte)(H3 >>> 24), (byte)(H3 >>> 16),
		    (byte)(H3 >>> 8), (byte)H3,
		    (byte)(H4 >>> 24), (byte)(H4 >>> 16),
		    (byte)(H4 >>> 8), (byte)H4};
    
    engineReset ();
    return result;
  }

  // Process a single block.  This is pretty much copied verbatim from
  // [1] pp. 9, 10.
  private void munch ()
  {
    for (int t = 16; t < 80; ++ t)
      {
	int Wt = W[t - 3] ^ W[t - 8] ^ W[t - 14] ^ W[t - 16];
	W[t] = Wt << 1 | Wt >>> 31;
      }

    int A = H0;
    int B = H1;
    int C = H2;
    int D = H3;
    int E = H4;

    for (int t = 0; t < 20; ++ t)
      {
	int TEMP = (A << 5 | A >>> 27) // S^5(A)
	  + ((B & C) | (~B & D))       // f_t(B,C,D)
	  + E + W[t]
	  + 0x5a827999;                // K_t

	E = D;
	D = C;
	C = B << 30 | B >>> 2;         // S^30(B)
	B = A;
	A = TEMP;
      }

    for (int t = 20; t < 40; ++ t)
      {
	int TEMP = (A << 5 | A >>> 27) // S^5(A)
	  + (B ^ C ^ D)                // f_t(B,C,D)
	  + E + W[t]                   
	  + 0x6ed9eba1;                // K_t

	E = D;
	D = C;
	C = B << 30 | B >>> 2;         // S^30(B)
	B = A;
	A = TEMP;
      }

    for (int t = 40; t < 60; ++ t)
      {
	int TEMP = (A << 5 | A >>> 27) // S^5(A)
	  + (B & C | B & D | C & D)    // f_t(B,C,D)
	  + E + W[t]
	  + 0x8f1bbcdc;                // K_t

	E = D;
	D = C;
	C = B << 30 | B >>> 2;         // S^30(B)
	B = A;
	A = TEMP;
      }

    for (int t = 60; t < 80; ++ t)
      {
	int TEMP = (A << 5 | A >>> 27) // S^5(A)
	  + (B ^ C ^ D)                // f_t(B,C,D)
	  + E + W[t]
	  + 0xca62c1d6;                // K_t

	E = D;
	D = C;
	C = B << 30 | B >>> 2;         // S^30(B)
	B = A;
	A = TEMP;
      }

    H0 += A;
    H1 += B;
    H2 += C;
    H3 += D;
    H4 += E;

    // Reset W by clearing it.
    for (int t = 0; t < 80; ++ t)
      W[t] = 0;
  }
  
  public Object clone ()
  {
    return new SHA (this);
  }

  private SHA (SHA copy)
  {
    this ();
    bytecount = copy.bytecount;
    H0 = copy.H0;
    H1 = copy.H1;
    H2 = copy.H2;
    H3 = copy.H3;
    H4 = copy.H4;
    System.arraycopy (copy.W, 0, W, 0, 80);
  }
  
  private final int W[] = new int[80];
  private long bytecount;
  private int H0;
  private int H1;
  private int H2;
  private int H3;
  private int H4;
}
