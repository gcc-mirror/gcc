/* RSAKeyPairGeneratorSpi.java -- JCE RSA KeyPairGenerator Adapter
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.jce.sig;

import gnu.java.security.Registry;
import gnu.java.security.key.rsa.RSAKeyPairGenerator;

import java.security.InvalidAlgorithmParameterException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.RSAKeyGenParameterSpec;
import java.util.HashMap;

/**
 * The implementation of a {@link java.security.KeyPairGenerator} adapter class
 * to wrap GNU RSA keypair generator instances.
 * <p>
 * In case the client does not explicitly initialize the KeyPairGenerator (via a
 * call to an <code>initialize()</code> method), the GNU provider uses a
 * default <i>modulus</i> size (keysize) of 1024 bits.
 */
public class RSAKeyPairGeneratorSpi
    extends KeyPairGeneratorAdapter
{
  public RSAKeyPairGeneratorSpi()
  {
    super(Registry.RSA_KPG);
  }

  public void initialize(int keysize, SecureRandom random)
  {
    HashMap attributes = new HashMap();
    attributes.put(RSAKeyPairGenerator.MODULUS_LENGTH, Integer.valueOf(keysize));
    if (random != null)
      attributes.put(RSAKeyPairGenerator.SOURCE_OF_RANDOMNESS, random);

    attributes.put(RSAKeyPairGenerator.PREFERRED_ENCODING_FORMAT,
                   Integer.valueOf(Registry.ASN1_ENCODING_ID));
    adaptee.setup(attributes);
  }

  public void initialize(AlgorithmParameterSpec params, SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
    HashMap attributes = new HashMap();
    if (params != null)
      {
        if (! (params instanceof RSAKeyGenParameterSpec))
          throw new InvalidAlgorithmParameterException("params");

        attributes.put(RSAKeyPairGenerator.RSA_PARAMETERS, params);
      }
    if (random != null)
      attributes.put(RSAKeyPairGenerator.SOURCE_OF_RANDOMNESS, random);

    attributes.put(RSAKeyPairGenerator.PREFERRED_ENCODING_FORMAT,
                   Integer.valueOf(Registry.ASN1_ENCODING_ID));
    adaptee.setup(attributes);
  }
}
