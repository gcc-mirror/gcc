/* PKCS8EncodedKeySpec.java --- PKCS8 Encoded Key Specificaton class
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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.security.spec;

/**
        PKCS8 Encoded Key Specification class which is used to store
        "PKCS#8" byte encoded keys.

        @since JDK 1.2

        @author Mark Benvenuto
*/
public class PKCS8EncodedKeySpec extends EncodedKeySpec
{
  /**
     Constructs a new PKCS8EncodedKeySpec with the specified encoded key.

     @param encodedKey A key to store, assumed to be "PKCS#8"
  */
  public PKCS8EncodedKeySpec(byte[] encodedKey)
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
        Returns the name of the key format used which is "PKCS#8"

        @return a string representing the name
*/
  public final String getFormat()
  {
    return "PKCS#8";
  }

}
