/* X509EncodedKeySpec.java --- X.509 Encoded Key Specificaton class
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.security.spec;

/**
	X.509 Encoded Key Specification class which is used to store 
	"X.509" byte encoded keys.

	@since JDK 1.2

	@author Mark Benvenuto
*/
public class X509EncodedKeySpec extends EncodedKeySpec
{

  /**
     Constructs a new X509EncodedKeySpec with the specified encoded key.

     @param encodedKey A key to store, assumed to be "X.509"
  */
  public X509EncodedKeySpec(byte[] encodedKey)
  {
    super( encodedKey );
  }

  /**
     Gets the encoded key in byte format.

     @returns the encoded key
  */
  public byte[] getEncoded()
  {
    return super.getEncoded();
  }

  /**
     Returns the name of the key format used which is "X.509"

     @return a string representing the name
  */
  public String getFormat()
  {
    return "X.509";
  }

}
