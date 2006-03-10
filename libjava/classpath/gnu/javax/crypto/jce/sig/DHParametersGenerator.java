/* DHParametersGenerator.java -- JCE Adapter for a generator of DH parameters
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.sig;

import gnu.java.security.Registry;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.crypto.key.dh.GnuDHKeyPairGenerator;
import gnu.javax.crypto.key.dh.RFC2631;

import java.math.BigInteger;
import java.security.AlgorithmParameterGeneratorSpi;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;

import javax.crypto.spec.DHGenParameterSpec;
import javax.crypto.spec.DHParameterSpec;

/**
 * A JCE Adapter for a generator of DH parameters.
 */
public class DHParametersGenerator
    extends AlgorithmParameterGeneratorSpi
{
  private static final Provider GNU_CRYPTO = new GnuCrypto();

  /** Size of the prime (public) modulus in bits. */
  private int modulusSize = -1;

  /** Size of the prime (private) modulus in bits. */
  private int exponentSize = -1;

  /** User specified source of randomness. */
  private SecureRandom rnd;

  /** Our concrete DH parameters generator. */
  private RFC2631 rfc2631;


  protected void engineInit(int size, SecureRandom random)
  {
    if ((size % 256) != 0 || size < GnuDHKeyPairGenerator.DEFAULT_PRIME_SIZE)
      throw new InvalidParameterException("Prime modulus (p) size (in bits) "
                                          + "MUST be a multiple of 256, and "
                                          + "greater than or equal to 1024");
    this.modulusSize = size;
    this.rnd = random;
  }

  protected void engineInit(AlgorithmParameterSpec spec, SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
    if (spec instanceof DHParameterSpec)
      {
        DHParameterSpec dhSpec = (DHParameterSpec) spec;
        BigInteger p = dhSpec.getP();
        int size = p.bitLength();
        this.engineInit(size, random);
      }
    else if (spec instanceof DHGenParameterSpec)
      {
        DHGenParameterSpec dhSpec = (DHGenParameterSpec) spec;
        int size = dhSpec.getPrimeSize();
        this.engineInit(size, random);
        exponentSize = dhSpec.getExponentSize();

        if ((exponentSize % 8) != 0
            || exponentSize < GnuDHKeyPairGenerator.DEFAULT_EXPONENT_SIZE)
          throw new InvalidParameterException("Random exponent size (in bits) "
                                              + "MUST be a multiple of 8, and "
                                              + "greater than or equal to "
                                              + GnuDHKeyPairGenerator.DEFAULT_EXPONENT_SIZE);
        if (exponentSize > modulusSize)
          throw new InvalidParameterException("Random exponent size (in bits) "
                                              + "MUST be less than that of the "
                                              + "public prime modulus (p)");
      }

    throw new InvalidAlgorithmParameterException("Wrong AlgorithmParameterSpec type: "
                                                 + spec.getClass().getName());
  }

  protected AlgorithmParameters engineGenerateParameters()
  {
    if (modulusSize < 1)
      modulusSize = GnuDHKeyPairGenerator.DEFAULT_PRIME_SIZE;

    if (exponentSize < 1)
      exponentSize = GnuDHKeyPairGenerator.DEFAULT_EXPONENT_SIZE;

    rfc2631 = new RFC2631(exponentSize, modulusSize, rnd);
    BigInteger[] params = rfc2631.generateParameters();
    BigInteger p = params[RFC2631.DH_PARAMS_P];
    BigInteger g = params[RFC2631.DH_PARAMS_G];
    int l = params[RFC2631.DH_PARAMS_Q].bitLength();
    DHParameterSpec spec = new DHParameterSpec(p, g, l);
    AlgorithmParameters result = null;
    try
      {
        result = AlgorithmParameters.getInstance(Registry.DH_KPG, GNU_CRYPTO);
        result.init(spec);
      }
    catch (NoSuchAlgorithmException ignore)
      {
      }
    catch (InvalidParameterSpecException ignore)
      {
      }
    return result;
  }
}
