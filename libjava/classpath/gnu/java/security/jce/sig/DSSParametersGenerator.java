/* DSSParametersGenerator.java -- JCE Adapter for a generator of DSS parameters
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


package gnu.java.security.jce.sig;

import gnu.java.security.Registry;
import gnu.java.security.key.dss.DSSKeyPairGenerator;
import gnu.java.security.key.dss.FIPS186;
import gnu.java.security.provider.Gnu;

import java.math.BigInteger;
import java.security.AlgorithmParameterGeneratorSpi;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.security.spec.InvalidParameterSpecException;

/**
 * A JCE Adapter for a generator of DSS parameters.
 */
public class DSSParametersGenerator
    extends AlgorithmParameterGeneratorSpi
{
  private static final Provider GNU = new Gnu();

  /** Size of the public modulus in bits. */
  private int modulusLength = -1;

  /** User specified source of randomness. */
  private SecureRandom rnd;

  /** Our concrete DSS parameters generator. */
  private FIPS186 fips;

  // default 0-arguments constructor

  protected void engineInit(int size, SecureRandom random)
  {
    if ((size % 64) != 0 || size < 512 || size > 1024)
      throw new InvalidParameterException("Modulus size/length (in bits) MUST "
                                          + "be a multiple of 64, greater than "
                                          + "or equal to 512, and less than or "
                                          + "equal to 1024");
    this.modulusLength = size;
    this.rnd = random;
  }

  protected void engineInit(AlgorithmParameterSpec spec, SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
    if (! (spec instanceof DSAParameterSpec))
      throw new InvalidAlgorithmParameterException("Wrong AlgorithmParameterSpec type: "
                                                   + spec.getClass().getName());
    DSAParameterSpec dsaSpec = (DSAParameterSpec) spec;
    BigInteger p = dsaSpec.getP();
    int size = p.bitLength();
    this.engineInit(size, random);
  }

  protected AlgorithmParameters engineGenerateParameters()
  {
    if (modulusLength < 1)
      modulusLength = DSSKeyPairGenerator.DEFAULT_MODULUS_LENGTH;

    fips = new FIPS186(modulusLength, rnd);
    BigInteger[] params = fips.generateParameters();
    BigInteger p = params[3];
    BigInteger q = params[2];
    BigInteger g = params[5];
    DSAParameterSpec spec = new DSAParameterSpec(p, q, g);
    AlgorithmParameters result = null;
    try
      {
        result = AlgorithmParameters.getInstance(Registry.DSS_KPG, GNU);
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
