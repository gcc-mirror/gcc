/* IALG.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;

import javax.security.sasl.SaslException;

/**
 * A Factory class that returns IALG (Integrity Algorithm) instances that
 * operate as described in the draft-burdis-cat-sasl-srp-04 and later.
 */
public final class IALG
    implements Cloneable
{
  private IMac hmac;

  /** Private constructor to enforce instantiation through Factory method. */
  private IALG(final IMac hmac)
  {
    super();

    this.hmac = hmac;
  }

  /**
   * Returns an instance of a SASL-SRP IALG implementation.
   * 
   * @param algorithm the name of the HMAC algorithm.
   * @return an instance of this object.
   */
  static synchronized IALG getInstance(final String algorithm)
      throws SaslException
  {
    final IMac hmac;
    hmac = MacFactory.getInstance(algorithm);
    if (hmac == null)
      throw new SaslException("getInstance()",
                              new NoSuchAlgorithmException(algorithm));
    return new IALG(hmac);
  }

  public Object clone() throws CloneNotSupportedException
  {
    return new IALG((IMac) hmac.clone());
  }

  public void init(final KDF kdf) throws SaslException
  {
    try
      {
        final byte[] sk = kdf.derive(hmac.macSize());
        final HashMap map = new HashMap();
        map.put(IMac.MAC_KEY_MATERIAL, sk);
        hmac.init(map);
      }
    catch (InvalidKeyException x)
      {
        throw new SaslException("getInstance()", x);
      }
  }

  public void update(final byte[] data)
  {
    hmac.update(data, 0, data.length);
  }

  public void update(final byte[] data, final int offset, final int length)
  {
    hmac.update(data, offset, length);
  }

  public byte[] doFinal()
  {
    return hmac.digest();
  }

  /**
   * Returns the length (in bytes) of this SASL SRP Integrity Algorithm.
   * 
   * @return the length, in bytes, of this integrity protection algorithm.
   */
  public int length()
  {
    return hmac.macSize();
  }
}
