/* PBEParameterSpec.java -- A wrapper for PBE parameters.
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

import java.security.spec.AlgorithmParameterSpec;

/**
 * A wrapper for the parameters used in <a
 * href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-5/">PKCS #5 -
 * Password-Based Cryptography Standard</a>.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class PBEParameterSpec implements AlgorithmParameterSpec
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The iteration count. */
  private int iterationCount;

  /** The salt. */
  private byte[] salt;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Creates a new password-based encryption parameter specification.
   *
   * @param salt           The salt.
   * @param iterationCount The iteration count.
   */
  public PBEParameterSpec(byte[] salt, int iterationCount)
  {
    this.salt = salt;
    this.iterationCount = iterationCount;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Get the iteration count.
   *
   * @return The iteration count.
   */
  public int getIterationCount()
  {
    return iterationCount;
  }

  /**
   * Get the salt.
   *
   * @return The salt.
   */
  public byte[] getSalt()
  {
    return salt;
  }
}
