/* IvParameterSpec.java -- A simple wrapper for initialization vectors.
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.security.spec.AlgorithmParameterSpec;

/**
 * A wrapper for an initialization vector. An initialization vector is
 * necessary for any cipher in any <i>feedback mode</i>, e.g. CBC.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class IvParameterSpec implements AlgorithmParameterSpec
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The IV. */
  private byte[] iv;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new initialization vector spec from an entire byte array.
   *
   * @param iv The IV bytes.
   */
  public IvParameterSpec(byte[] iv)
  {
    this(iv, 0, iv.length);
  }

  /**
   * Create a new initialization vector spec from part of a byte array.
   *
   * @param iv  The IV bytes.
   * @param off The offset into the IV bytes.
   * @param len The number of IV bytes.
   */
  public IvParameterSpec(byte[] iv, int off, int len)
  {
    this.iv = new byte[len];
    System.arraycopy(iv, off, this.iv, 0, len);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the IV. This method does not copy the byte array.
   *
   * @return The IV.
   */
  public byte[] getIV()
  {
    return iv;
  }
}
