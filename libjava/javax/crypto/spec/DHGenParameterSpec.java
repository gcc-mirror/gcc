/* DHGenParameterSpec.java -- Diffie-Hellman parameter generator spec.
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
 * This class represents the parameters needed for generating
 * Diffie-Hellman parameters.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see DHParameterSpec
 */
public class DHGenParameterSpec implements AlgorithmParameterSpec
{

  // Variables.
  // ------------------------------------------------------------------------

  /** The length of the prime, in bits. */
  private int primeSize;

  /** The length of the exponent, in bits. */
  private int exponentSize;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new Diffie-Hellman parameter generator spec.
   *
   * @param primeSize The size of the prime, in bits.
   * @param exponentSize The size of the exponent, in bits.
   */
  public DHGenParameterSpec(int primeSize, int exponentSize)
  {
    this.primeSize = primeSize;
    this.exponentSize = exponentSize;
  }

  // Intance methods.
  // ------------------------------------------------------------------------

  /**
   * Get the size of the exponent, in bits.
   *
   * @return The exponent size.
   */
  public int getExponentSize()
  {
    return exponentSize;
  }

  /**
   * Get the size of the prime, in bits.
   *
   * @return The prime size.
   */
  public int getPrimeSize()
  {
    return primeSize;
  }
}
