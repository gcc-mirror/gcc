/* KeyPair.java --- Key Pair Class
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

package java.security;
import java.io.Serializable;

/**
   KeyPair serves as a simple container for public and private keys.
   If properly initialized, this class should be treated like the
   private key since it contains it and take approriate security
   measures.

   @author Mark Benvenuto
 */
public final class KeyPair implements Serializable
{
  private PublicKey publicKey;
  private PrivateKey privateKey;

  /**
     Initializes the KeyPair with a pubilc and private key.

     @param publicKey Public Key to store
     @param privateKey Private Key to store
   */
  public KeyPair(PublicKey publicKey, PrivateKey privateKey)
  {
    this.publicKey = publicKey;
    this.privateKey = privateKey;
  }

  /**
     Returns the public key stored in the KeyPair

     @return The public key
   */
  public PublicKey getPublic()
  {
    return publicKey;
  }

  /**
     Returns the private key stored in the KeyPair

     @return The private key
   */
  public PrivateKey getPrivate()
  {
    return privateKey;
  }
}
