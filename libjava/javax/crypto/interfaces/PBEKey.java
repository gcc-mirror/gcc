/* PBEKey.java -- A key derived from a password.
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


package javax.crypto.interfaces;

import javax.crypto.SecretKey;

/**
 * Interface to a password-derived key for password-based encryption
 * (PBE). Applications working with a {@link javax.crypto.SecretKey}
 * that is known to be a password-based key can safely cast such keys to
 * this interface.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public interface PBEKey extends SecretKey
{
  /** Compatible with JDK1.4. */
  long serialVersionUID = -1430015993304333921L;

  /**
   * Retruns the iteration count, or 0 if not specified.
   *
   * @return The iteration count.
   */
  int getIterationCount();

  /**
   * Returns a copy of the password as a character array. It is the
   * caller's responsibility to zero-out the password when it is no
   * longer in use.
   *
   * <p>Although it is not specified in the documentation,
   * implementations should not copy or clone the password array, but
   * rather return the reference to the array itself, so the caller has
   * the ability to erase the password.
   *
   * @return The password.
   */
  char[] getPassword();

  /**
   * Returns a copy of the salt. It is the caller's responsibility to
   * zero-out the salt when it is no longer in use.
   *
   * <p>Although it is not specified in the documentation,
   * implementations should not copy or clone the salt array, but
   * rather return the reference to the array itself, so the caller has
   * the ability to erase the salt.
   *
   * @return The salt.
   */
  byte[] getSalt();
}
