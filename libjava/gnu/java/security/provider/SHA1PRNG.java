/* SHA1PRNG.java --- Secure Random SPI SHA1PRNG
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.security.provider;

import java.util.Random;
import java.security.SecureRandomSpi;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.io.Serializable;

public class SHA1PRNG extends SecureRandomSpi implements Serializable
{
  MessageDigest digest;
  byte seed[];
  byte data[];
  int seedpos;
  int datapos;

  public SHA1PRNG()
  {
    try {
      digest = MessageDigest.getInstance("SHA");
    } catch ( NoSuchAlgorithmException nsae) {
      System.out.println("Failed to find SHA Message Digest: " + nsae);
      nsae.printStackTrace();
    }

    seed = new byte[20];
    seedpos = 0;
    data = new byte[40];
    datapos = 0;

    new Random().nextBytes(seed);

    byte digestdata[];
    digestdata = digest.digest( data );
    System.arraycopy( digestdata, 0, data, 0, 20);

  }

  public void engineSetSeed(byte[] seed)
  {
    for(int i = 0; i < seed.length; i++)
      this.seed[seedpos++ % 20] ^= seed[i];
    seedpos %= 20;

  }

  public void engineNextBytes(byte[] bytes)
  {
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
}
