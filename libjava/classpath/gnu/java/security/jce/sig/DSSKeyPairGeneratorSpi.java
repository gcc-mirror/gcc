/* DSSKeyPairGeneratorSpi.java --
   Copyright 2001, 2002, 2006 Free Software Foundation, Inc.

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
import gnu.java.security.key.dss.DSSKeyPairGenerator;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;
import java.security.SecureRandom;
import java.security.interfaces.DSAKeyPairGenerator;
import java.security.interfaces.DSAParams;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.util.HashMap;

/**
 * The implementation of a {@link java.security.KeyPairGenerator} adapter class
 * to wrap GNU DSS keypair generator instances.
 * <p>
 * In case the client does not explicitly initialize the KeyPairGenerator (via a
 * call to an <code>initialize()</code> method), the GNU provider uses a
 * default <i>modulus</i> size (keysize) of 1024 bits.
 */
public class DSSKeyPairGeneratorSpi
    extends KeyPairGeneratorAdapter
    implements DSAKeyPairGenerator
{
  public DSSKeyPairGeneratorSpi()
  {
    super(Registry.DSS_KPG);
  }

  public void initialize(int keysize, SecureRandom random)
  {
    this.initialize(keysize, false, random);
  }

  public void initialize(AlgorithmParameterSpec params, SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
    HashMap attributes = new HashMap();
    if (params != null)
      {
        if (! (params instanceof DSAParameterSpec))
          throw new InvalidAlgorithmParameterException(
              "Parameters argument is not a non-null instance, or "
              + "sub-instance, of java.security.spec.DSAParameterSpec");
        attributes.put(DSSKeyPairGenerator.DSS_PARAMETERS, params);
      }
    if (random != null)
      attributes.put(DSSKeyPairGenerator.SOURCE_OF_RANDOMNESS, random);

    attributes.put(DSSKeyPairGenerator.PREFERRED_ENCODING_FORMAT,
                   Integer.valueOf(Registry.ASN1_ENCODING_ID));
    try
      {
        adaptee.setup(attributes);
      }
    catch (IllegalArgumentException x)
      {
        throw new InvalidAlgorithmParameterException(x.getMessage(), x);
      }
  }

  public void initialize(DSAParams params, SecureRandom random)
      throws InvalidParameterException
  {
    if (params == null || !(params instanceof DSAParameterSpec))
      throw new InvalidParameterException(
          "Parameters argument is either null or is not an instance, or "
          + "sub-instance, of java.security.spec.DSAParameterSpec");
    DSAParameterSpec spec = (DSAParameterSpec) params;
    try
      {
        this.initialize((AlgorithmParameterSpec) spec, random);
      }
    catch (InvalidAlgorithmParameterException x)
      {
        InvalidParameterException y = new InvalidParameterException(x.getMessage());
        y.initCause(x);
        throw y;
      }
  }

  public void initialize(int modlen, boolean genParams, SecureRandom random)
      throws InvalidParameterException
  {
    HashMap attributes = new HashMap();
    attributes.put(DSSKeyPairGenerator.MODULUS_LENGTH, Integer.valueOf(modlen));
    if (random != null)
      attributes.put(DSSKeyPairGenerator.SOURCE_OF_RANDOMNESS, random);

    attributes.put(DSSKeyPairGenerator.USE_DEFAULTS,
                   Boolean.valueOf(! genParams));
    attributes.put(DSSKeyPairGenerator.STRICT_DEFAULTS, Boolean.TRUE);
    attributes.put(DSSKeyPairGenerator.PREFERRED_ENCODING_FORMAT,
                   Integer.valueOf(Registry.ASN1_ENCODING_ID));
    try
      {
        adaptee.setup(attributes);
      }
    catch (IllegalArgumentException x)
      {
        InvalidParameterException y = new InvalidParameterException(x.getMessage());
        y.initCause(x);
        throw y;
      }
  }
}
