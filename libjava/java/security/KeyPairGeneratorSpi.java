/* KeyPairGeneratorSpi.java --- Key Pair Generator SPI Class
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
import java.security.spec.AlgorithmParameterSpec;

/**
   KeyPairGeneratorSpi is the interface used to generate key pairs
   for security algorithms.

   @author Mark Benvenuto
 */
public abstract class KeyPairGeneratorSpi
{
  /**
     Constructs a new KeyPairGeneratorSpi
   */
  public KeyPairGeneratorSpi()
  {
  }

  /**
     Initialize the KeyPairGeneratorSpi with the specified
     key size and source of randomness

     @param keysize size of the key to generate
     @param random A SecureRandom source of randomness  
   */
  public abstract void initialize(int keysize, SecureRandom random);

  /**
     Initialize the KeyPairGeneratorSpi with the specified
     AlgorithmParameterSpec and source of randomness

     This is a concrete method. It may be overridden by the provider
     and if the AlgorithmParameterSpec class is invalid
     throw InvalidAlgorithmParameterException. By default this
     method just throws UnsupportedOperationException.

     @param params A AlgorithmParameterSpec to intialize with
     @param random A SecureRandom source of randomness  

     @throws InvalidAlgorithmParameterException
   */
  public void initialize(AlgorithmParameterSpec params, SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    throw new java.lang.UnsupportedOperationException();
  }

  /**
     Generates a KeyPair according the rules for the algorithm.
     Unless intialized, algorithm defaults will be used. It 
     creates a unique key pair each time.

     @return a key pair
   */
  public abstract KeyPair generateKeyPair();
}
