/* SecretKeySpec.java -- Wrapper for secret keys.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.crypto.spec;

import java.security.spec.KeySpec;

import javax.crypto.SecretKey;

/**
 * This is a simple wrapper around a raw byte array, for ciphers that do
 * not require any key parameters other than the bytes themselves.
 *
 * <p>Since this class implements {@link javax.crypto.SecretKey}, which
 * in turn extends {@link java.security.Key}, so instances of this class
 * may be passed directly to the <code>init()</code> methods of {@link
 * javax.crypto.Cipher}.
 *
 * @see javax.crypto.SecretKey
 * @see javax.crypto.SecretKeyFactory
 */
public class SecretKeySpec implements KeySpec, SecretKey
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Compatible with JDK1.4. */
  private static final long serialVersionUID = 6577238317307289933L;

  /** The key bytes. */
  private byte[] key;

  /** The algorithm's name. */
  private String algorithm;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new secret key spec from an entire byte array.
   *
   * @param key       The key material.
   * @param algorithm The name of the algorithm using this key.
   */
  public SecretKeySpec(byte[] key, String algorithm)
  {
    this(key, 0, key.length, algorithm);
  }

  /**
   * Create a new secret key spec from part of a byte array.
   *
   * @param key       The key material.
   * @param off       The offset at which key material begins.
   * @param len       The length of key material.
   * @param algorithm The name of the algorithm using this key.
   */
  public SecretKeySpec(byte[] key, int off, int len, String algorithm)
  {
    this.key = new byte[len];
    this.algorithm = algorithm;
    System.arraycopy(key, off, this.key, 0, len);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the name of the algorithm associated with this secret key.
   *
   * @return The algorithm's name.
   */
  public String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Return the key as a byte array.
   *
   * @return The key material.
   */
  public byte[] getEncoded()
  {
    return key;
  }

  /**
   * This key's format, which is always "RAW".
   *
   * @return "RAW"
   */
  public String getFormat()
  {
    return "RAW";
  }

  public boolean equals(Object o)
  {
    if (o instanceof SecretKeySpec)
      {
        byte[] okey = ((SecretKeySpec) o).getEncoded();
        if (key.length != okey.length)
          return false;
        for (int i = 0; i < key.length; i++)
          {
            if (key[i] != okey[i])
              return false;
          }
        return algorithm.equals(((SecretKeySpec) o).getAlgorithm());
      }
    else
      {
        return false;
      }
  }

  public int hashCode()
  {
    int code = 0;
    for (int i = 0; i < key.length; i++)
      {
        code ^= (key[i] & 0xff) << (i << 3 & 31);
      }
    return code ^ algorithm.hashCode();
  }
}
