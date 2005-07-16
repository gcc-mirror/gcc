/* KeyPairGeneratorSpi.java --- Key Pair Generator SPI Class
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

  /**
   * We override clone here to make it accessible for use by
   * DummyKeyPairGenerator.
   */
  protected Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }
}
