/* MacAdapter.java --
   Copyright (C) 2002, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.mac;

import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;
import java.util.HashMap;
import java.util.Map;
import javax.crypto.MacSpi;

/**
 * The implementation of a generic {@link javax.crypto.Mac} adapter class to
 * wrap GNU MAC instances.
 * <p>
 * This class defines the <i>Service Provider Interface</i> (<b>SPI</b>) for
 * the {@link javax.crypto.Mac} class, which provides the functionality of a
 * message authentication code algorithm, such as the <i>Hashed Message
 * Authentication Code</i> (<b>HMAC</b>) algorithms.
 */
class MacAdapter
    extends MacSpi
    implements Cloneable
{
  /** Our MAC instance. */
  protected IMac mac;
  /** Our MAC attributes. */
  protected Map attributes;

  /**
   * Creates a new Mac instance for the given name.
   *
   * @param name The name of the mac to create.
   */
  protected MacAdapter(String name)
  {
    mac = MacFactory.getInstance(name);
    attributes = new HashMap();
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param mac a clone of the internal {@link IMac} instance.
   * @param attributes a clone of the current {@link Map} of attributes.
   */
  private MacAdapter(IMac mac, Map attributes)
  {
    super();

    this.mac = mac;
    this.attributes = attributes;
  }

  public Object clone() throws CloneNotSupportedException
  {
    return new MacAdapter((IMac) mac.clone(), new HashMap(attributes));
  }

  protected byte[] engineDoFinal()
  {
    byte[] result = mac.digest();
    engineReset();
    return result;
  }

  protected int engineGetMacLength()
  {
    return mac.macSize();
  }

  protected void engineInit(Key key, AlgorithmParameterSpec params)
      throws InvalidKeyException, InvalidAlgorithmParameterException
  {
    if (! key.getFormat().equalsIgnoreCase("RAW"))
      throw new InvalidKeyException("unknown key format " + key.getFormat());
    attributes.put(IMac.MAC_KEY_MATERIAL, key.getEncoded());
    mac.reset();
    mac.init(attributes);
  }

  protected void engineReset()
  {
    mac.reset();
  }

  protected void engineUpdate(byte b)
  {
    mac.update(b);
  }

  protected void engineUpdate(byte[] in, int off, int len)
  {
    mac.update(in, off, len);
  }
}
