/* SecretKey.java -- A key for symmetric cryptography.
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


package javax.crypto;

import java.security.Key;

/**
 * A secret key for symmetric cryptography.
 *
 * <p>This interface defines no new methods over {@link
 * java.security.Key}, but rather is intended to be a <i>marker
 * interface</i> and to provide type safety for secret keys.</p>
 *
 * <p>The format of secret keys should be <code>RAW</code>, as returned
 * by {@link java.security.Key#getFormat()}.</p>
 *
 * <p>Concrete implementations of this interface should override the
 * {@link java.lang.Object#equals} and {@link java.lang.Object#hashCode}
 * methods of {@link java.lang.Object} to use the actual key data rather
 * than the identity-based default methods.</p>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @see javax.crypto.SecretKeyFactory
 * @see javax.crypto.Cipher
 */
public interface SecretKey extends Key
{
  long serialVersionUID = -4795878709595146952L;
}
