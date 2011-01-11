/* SSLv3HMacSHA.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Collections;
import java.util.Map;

import javax.crypto.MacSpi;
import javax.crypto.SecretKey;

/**
 * @author csm
 */
public class SSLv3HMacSHAImpl extends MacSpi
{
  private final SSLHMac adaptee;

  public SSLv3HMacSHAImpl()
  {
    adaptee = new SSLHMac("SHA-160");
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineDoFinal()
   */
  @Override protected byte[] engineDoFinal()
  {
    return adaptee.digest();
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineGetMacLength()
   */
  @Override protected int engineGetMacLength()
  {
    return adaptee.macSize();
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineInit(java.security.Key, java.security.spec.AlgorithmParameterSpec)
   */
  @Override protected void engineInit(Key key, AlgorithmParameterSpec params)
      throws InvalidAlgorithmParameterException, InvalidKeyException
  {
    if (!(key instanceof SecretKey)
        || !key.getAlgorithm().equalsIgnoreCase("SSLv3HMac-SHA"))
      throw new InvalidKeyException("expecting secret key with algorithm \"SSLv3HMac-SHA\"");
    Map<String,byte[]> attr =
      Collections.singletonMap(SSLHMac.MAC_KEY_MATERIAL, key.getEncoded());
    adaptee.init(attr);
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineReset()
   */
  @Override protected void engineReset()
  {
    adaptee.reset();
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineUpdate(byte)
   */
  @Override protected void engineUpdate(byte input)
  {
    adaptee.update(input);
  }

  /* (non-Javadoc)
   * @see javax.crypto.MacSpi#engineUpdate(byte[], int, int)
   */
  @Override protected void engineUpdate(byte[] input, int offset, int length)
  {
    adaptee.update(input, offset, length);
  }
}
