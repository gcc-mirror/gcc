/* Key.java -- A abstract representation of a digital key
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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

package java.security;

import java.io.Serializable;

/**
 * This interfaces models the base characteristics that all keys must
 * have.  These are:  a key algorithm, an encoded form, and a format used
 * to encode the key.  Specific key types inherit from this interface.
 * <p>
 * Note that since this interface extends <code>Serializable</code>, all
 * keys may be serialized.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Key extends Serializable
{
 /**
   * The verion identifier used for serialization.
   */
  public static final long serialVersionUID = 6603384152749567654L;

  /**
   * This method returns the name of the algorithm for this key.  This is a
   * <code>String</code> such as "RSA".
   *
   * @return The name of the algorithm in use
   */
  public abstract String getAlgorithm();

  /**
   * This method returns the name of the encoding format for this key.  This
   * is the name of the ASN.1 data format used for this key, such as
   * "X.509" or "PKCS#8".  This method returns <code>null</code> if this key
   * does not have an encoding format.
   *
   * @return The name of the encoding format for this key, or <code>null</code> if there is no such format.
   */
  public abstract String getFormat();

  /**
   * This method returns the encoded form of the key.  If this key does not
   * support encoding, this method returns <code>null</code>
   *
   * @return The encoded form of the key, or <code>null</code> if no encoded form is available.
   */
  public abstract byte[] getEncoded();
}
