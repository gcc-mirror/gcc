/* SHA1PRNG.java --- Secure Random SPI SHA1PRNG
   Copyright (C) 1999, 2001, 2003 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandomSpi;
import java.util.Random;

public class SHA1PRNG extends SecureRandomSpi implements Serializable
{
  MessageDigest digest;
  byte seed[];
  byte data[];
  int seedpos;
  int datapos;
  private boolean seeded = false; // set to true when we seed this

  public SHA1PRNG()
  {
    try {
      digest = MessageDigest.getInstance("SHA");
    } catch ( NoSuchAlgorithmException nsae) {
//      System.out.println("Failed to find SHA Message Digest: " + nsae);
//      nsae.printStackTrace();
      throw new InternalError ("no SHA implementation found");
    }

    seed = new byte[20];
    seedpos = 0;
    data = new byte[40];
    datapos = 20;  // try to force hashing a first block
  }

  public void engineSetSeed(byte[] seed)
  {
    for(int i = 0; i < seed.length; i++)
      this.seed[seedpos++ % 20] ^= seed[i];
    seedpos %= 20;

  }

  public void engineNextBytes(byte[] bytes)
  {
    ensureIsSeeded ();
    int loc = 0;
    while (loc < bytes.length)
      {
	int copy = Math.min (bytes.length - loc, 20 - datapos);

	if (copy > 0)
	  {
	    System.arraycopy (data, datapos, bytes, loc, copy);
	    datapos += copy;
	    loc += copy;
	  }
	else
	  {
	    // No data ready for copying, so refill our buffer.
	    System.arraycopy( seed, 0, data, 20, 20);
	    byte[] digestdata = digest.digest( data );
	    System.arraycopy( digestdata, 0, data, 0, 20);
	    datapos = 0;
	  }
      }
  }

  public byte[] engineGenerateSeed(int numBytes)
  {
    byte tmp[] = new byte[numBytes];
	
    engineNextBytes( tmp );
    return tmp;
  }

  private void ensureIsSeeded()
  {
    if (!seeded)
      {
        new Random(0L).nextBytes(seed);

        byte[] digestdata = digest.digest(data);
        System.arraycopy(digestdata, 0, data, 0, 20);

        seeded = true;
      }
  }

}
