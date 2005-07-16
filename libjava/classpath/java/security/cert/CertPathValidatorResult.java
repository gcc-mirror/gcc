/* CertPathValidatorResult -- result of validating certificate paths
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.security.cert;

/**
 * Interface to the result of calling {@link
 * CertPathValidator#validate(java.security.cert.CertPath,java.security.cert.CertPathParameters)}.
 *
 * <p>This interface defines no methods other than the required
 * {@link java.lang.Cloneable} interface, and is intended to group and
 * provide type safety for validator results. Providers that implement
 * a certificate path validator must also provide an implementation of
 * this interface, possibly defining additional methods.
 *
 * @since JDK 1.4
 * @see CertPathValidator
 */
public interface CertPathValidatorResult extends Cloneable
{

  /**
   * Returns a copy of this validator result.
   *
   * @return The copy.
   */
  Object clone();
}
