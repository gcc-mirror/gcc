/* SecretKeyGeneratorImpl.java -- symmetric key pair generator.
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.key;

import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.crypto.KeyGeneratorSpi;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class SecretKeyGeneratorImpl
    extends KeyGeneratorSpi
{
  protected final int defaultKeySize;
  protected final List keySizes;
  protected final String algorithm;
  protected boolean init;
  protected int currentKeySize;
  protected SecureRandom random;

  protected SecretKeyGeneratorImpl(final String algorithm)
  {
    this.algorithm = algorithm;
    IBlockCipher cipher = CipherFactory.getInstance(algorithm);
    if (cipher == null)
      throw new IllegalArgumentException("no such cipher: " + algorithm);
    defaultKeySize = cipher.defaultKeySize();
    keySizes = new LinkedList();
    for (Iterator it = cipher.keySizes(); it.hasNext();)
      keySizes.add(it.next());
    init = false;
  }

  protected SecretKey engineGenerateKey()
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    byte[] buf = new byte[currentKeySize];
    random.nextBytes(buf);
    return new SecretKeySpec(buf, algorithm);
  }

  protected void engineInit(AlgorithmParameterSpec params, SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
    throw new InvalidAlgorithmParameterException(
        algorithm + " does not support algorithm paramaters");
  }

  protected void engineInit(int keySize, SecureRandom random)
  {
    keySize >>>= 3; // Use bytes.
    if (! keySizes.contains(Integer.valueOf(keySize)))
      throw new InvalidParameterException("unsupported key size: " + keySize
                                          + ", valid sizes are: " + keySizes);
    currentKeySize = keySize;
    this.random = random;
    init = true;
  }

  protected void engineInit(SecureRandom random)
  {
    engineInit(defaultKeySize << 3, random);
  }
}
