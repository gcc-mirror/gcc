/* MD5.java -- Class implementing the MD5 algorithm as specified in RFC1321.
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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
   This class implements the MD5 algorithm as described in RFC1321.

   @see java.security.MessageDigest
*/
public class MD5 extends MessageDigest implements Cloneable
{
  private final int W[] = new int[16];
  private long bytecount;
  private int A;
  private int B;
  private int C;
  private int D;

  public MD5()
  {
    super("MD5");
    engineReset ();
  }

  public Object clone()
  {
    return new MD5 (this);
  }

  private MD5 (MD5 copy)
  {
    this ();
    bytecount = copy.bytecount;
    A = copy.A;
    B = copy.B;
    C = copy.C;
    D = copy.D;
    System.arraycopy (copy.W, 0, W, 0, 16);
  }

  public int engineGetDigestLength()
  {
    return 16;
  }

  // Intialize the A,B,C,D needed for the hash
  public void engineReset()
  {
    bytecount = 0;
    A = 0x67452301;
    B = 0xefcdab89;
    C = 0x98badcfe;
    D = 0x10325476;
    for(int i = 0; i < 16; i++)
      W[i] = 0;
  }

  public void engineUpdate (byte b)
  {
    int i = (int)bytecount % 64;
    int shift = (3 - i % 4) * 8;
    int idx = i / 4;

    // if you could index ints, this would be: W[idx][shift/8] = b
    W[idx] = (W[idx] & ~(0xff << shift)) | ((b & 0xff) << shift);

    // if we've filled up a block, then process it
    if ((++ bytecount) % 64 == 0)
      munch ();
  }

  public void engineUpdate (byte bytes[], int off, int len)
  {
    if (len < 0)
      throw new ArrayIndexOutOfBoundsException ();

    int end = off + len;
    while (off < end)
      engineUpdate (bytes[off++]);
  }

  public byte[] engineDigest()
  {
    long bitcount = bytecount * 8;
    engineUpdate ((byte)0x80); // 10000000 in binary; the start of the padding

    // add the rest of the padding to fill this block out, but leave 8
    // bytes to put in the original bytecount
    while ((int)bytecount % 64 != 56)
      engineUpdate ((byte)0);

    // add the length of the original, unpadded block to the end of
    // the padding
    W[14] = SWAP((int)(0xffffffff & bitcount));
    W[15] = SWAP((int)(0xffffffff & (bitcount >>> 32)));
    bytecount += 8;

    // digest the fully padded block
    munch ();

    A = SWAP(A);
    B = SWAP(B);
    C = SWAP(C);
    D = SWAP(D);
    byte[] result = new byte[] {(byte)(A >>> 24), (byte)(A >>> 16),
				(byte)(A >>> 8), (byte)A,
				(byte)(B >>> 24), (byte)(B >>> 16),
				(byte)(B >>> 8), (byte)B,
				(byte)(C >>> 24), (byte)(C >>> 16),
				(byte)(C >>> 8), (byte)C,
				(byte)(D >>> 24), (byte)(D >>> 16),
				(byte)(D >>> 8), (byte)D};
    
    engineReset ();
    return result;
  }

  private int F( int X, int Y, int Z)
  {
    return ((X & Y) | (~X & Z));
  }

  private int G( int X, int Y, int Z)
  {
    return ((X & Z) | (Y & ~Z));
  }

  private int H( int X, int Y, int Z)
  {
    return (X ^ Y ^ Z);
  }

  private int I( int X, int Y, int Z)
  {
    return (Y ^ (X | ~Z));
  }

  private int rotateLeft( int i, int count)
  {
    //Taken from FIPS 180-1
    return ( (i << count) | (i >>> (32 - count)) ) ;
  }

  /* Round 1. */
  private int FF( int a, int b, int c, int d, int k, int s, int i)
  {
    /* Let [abcd k s i] denote the operation */
    a += F(b,c,d) + k + i;
    return b + rotateLeft(a, s); 
  } 
  /* Round 2. */
  private int GG( int a, int b, int c, int d, int k, int s, int i)
  {
    /* Let [abcd k s i] denote the operation */
    a += G(b,c,d) + k + i;
    return b + rotateLeft(a, s);
  } 
  /* Round 3. */
  private int HH( int a, int b, int c, int d, int k, int s, int i)
  {
    /* Let [abcd k s t] denote the operation */
    a += H(b,c,d) + k + i;
    return b + rotateLeft(a, s);
  } 

  /* Round 4. */
  private int II( int a, int b, int c, int d, int k, int s, int i)
  {
    /* Let [abcd k s t] denote the operation */
    a += I(b,c,d) + k + i;
    return b + rotateLeft(a, s);
  } 

  private int SWAP(int n)
  {
    //Copied from md5.c in FSF Gnu Privacy Guard 0.9.2
    return (( (0xff & n) << 24) | ((n & 0xff00) << 8) | ((n >>> 8) & 0xff00) | (n >>> 24));
  }

  private void munch()
  {
    int AA,BB,CC,DD, j;
    int X[] = new int[16];

    /* Copy block i into X. */
    for(j = 0; j < 16; j++)
      X[j] = SWAP(W[j]);

    /* Save A as AA, B as BB, C as CC, and D as DD. */
    AA = A;
    BB = B;
    CC = C;
    DD = D;

    /* The hex constants are from md5.c 
       in FSF Gnu Privacy Guard 0.9.2 */
    /* Round 1. */
    /* Let [abcd k s i] denote the operation
       a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. */
    A = FF(A,B,C,D,  X[0],  7,  0xd76aa478);
    D = FF(D,A,B,C,  X[1], 12,  0xe8c7b756);
    C = FF(C,D,A,B,  X[2], 17,  0x242070db);
    B = FF(B,C,D,A,  X[3], 22,  0xc1bdceee);

    A = FF(A,B,C,D,  X[4],  7,  0xf57c0faf);
    D = FF(D,A,B,C,  X[5], 12,  0x4787c62a);
    C = FF(C,D,A,B,  X[6], 17,  0xa8304613);
    B = FF(B,C,D,A,  X[7], 22,  0xfd469501);

    A = FF(A,B,C,D,  X[8],  7,  0x698098d8);
    D = FF(D,A,B,C,  X[9], 12,  0x8b44f7af);
    C = FF(C,D,A,B, X[10], 17,  0xffff5bb1);
    B = FF(B,C,D,A, X[11], 22,  0x895cd7be);

    A = FF(A,B,C,D, X[12],  7,  0x6b901122);
    D = FF(D,A,B,C, X[13], 12,  0xfd987193);
    C = FF(C,D,A,B, X[14], 17,  0xa679438e);
    B = FF(B,C,D,A, X[15], 22,  0x49b40821);

    /* Round 2. */
    /* Let [abcd k s i] denote the operation
       a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. */
    A = GG(A,B,C,D,  X[1],  5, 0xf61e2562);
    D = GG(D,A,B,C,  X[6],  9, 0xc040b340);
    C = GG(C,D,A,B, X[11], 14, 0x265e5a51);
    B = GG(B,C,D,A,  X[0], 20, 0xe9b6c7aa);

    A = GG(A,B,C,D,  X[5],  5, 0xd62f105d);
    D = GG(D,A,B,C, X[10],  9, 0x02441453);
    C = GG(C,D,A,B, X[15], 14, 0xd8a1e681);
    B = GG(B,C,D,A,  X[4], 20, 0xe7d3fbc8);

    A = GG(A,B,C,D,  X[9],  5, 0x21e1cde6);
    D = GG(D,A,B,C, X[14],  9, 0xc33707d6);
    C = GG(C,D,A,B,  X[3], 14, 0xf4d50d87);
    B = GG(B,C,D,A,  X[8], 20, 0x455a14ed);

    A = GG(A,B,C,D, X[13],  5, 0xa9e3e905);
    D = GG(D,A,B,C,  X[2],  9, 0xfcefa3f8);
    C = GG(C,D,A,B,  X[7], 14, 0x676f02d9);
    B = GG(B,C,D,A, X[12], 20, 0x8d2a4c8a);

    /* Round 3. */
    /* Let [abcd k s t] denote the operation
       a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. */
    A = HH(A,B,C,D,  X[5],  4, 0xfffa3942);
    D = HH(D,A,B,C,  X[8], 11, 0x8771f681);
    C = HH(C,D,A,B, X[11], 16, 0x6d9d6122);
    B = HH(B,C,D,A, X[14], 23, 0xfde5380c);

    A = HH(A,B,C,D,  X[1],  4, 0xa4beea44);
    D = HH(D,A,B,C,  X[4], 11, 0x4bdecfa9);
    C = HH(C,D,A,B,  X[7], 16, 0xf6bb4b60);
    B = HH(B,C,D,A, X[10], 23, 0xbebfbc70);

    A = HH(A,B,C,D, X[13],  4, 0x289b7ec6);
    D = HH(D,A,B,C,  X[0], 11, 0xeaa127fa);
    C = HH(C,D,A,B,  X[3], 16, 0xd4ef3085);
    B = HH(B,C,D,A,  X[6], 23, 0x04881d05);

    A = HH(A,B,C,D,  X[9],  4, 0xd9d4d039);
    D = HH(D,A,B,C, X[12], 11, 0xe6db99e5);
    C = HH(C,D,A,B, X[15], 16, 0x1fa27cf8);
    B = HH(B,C,D,A,  X[2], 23, 0xc4ac5665);

    /* Round 4. */
    /* Let [abcd k s t] denote the operation
       a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. */
    A = II(A,B,C,D,  X[0],  6, 0xf4292244);
    D = II(D,A,B,C,  X[7], 10, 0x432aff97);
    C = II(C,D,A,B, X[14], 15, 0xab9423a7);
    B = II(B,C,D,A,  X[5], 21, 0xfc93a039);

    A = II(A,B,C,D, X[12],  6, 0x655b59c3);
    D = II(D,A,B,C,  X[3], 10, 0x8f0ccc92);
    C = II(C,D,A,B, X[10], 15, 0xffeff47d);
    B = II(B,C,D,A,  X[1], 21, 0x85845dd1);

    A = II(A,B,C,D,  X[8],  6, 0x6fa87e4f);
    D = II(D,A,B,C, X[15], 10, 0xfe2ce6e0);
    C = II(C,D,A,B,  X[6], 15, 0xa3014314);
    B = II(B,C,D,A, X[13], 21, 0x4e0811a1);

    A = II(A,B,C,D,  X[4],  6, 0xf7537e82);
    D = II(D,A,B,C, X[11], 10, 0xbd3af235);
    C = II(C,D,A,B,  X[2], 15, 0x2ad7d2bb);
    B = II(B,C,D,A,  X[9], 21, 0xeb86d391);

    /* Then perform the following additions. (That is increment each
       of the four registers by the value it had before this block
       was started.) */
    A = A + AA;
    B = B + BB;
    C = C + CC;
    D = D + DD;
  }
}
