/* SecureRandomSpi.java --- Secure Random Service Provider Interface
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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

package java.security;
import java.io.Serializable;

/**
   SecureRandomSpi is the Service Provider Interface for SecureRandom
   providers. It provides an interface for providers to the
   SecureRandom engine to write their own pseudo-random number
   generator.

   @since JDK 1.2

   @author Mark Benvenuto (ivymccough@worldnet.att.net)
 */
public abstract class SecureRandomSpi implements Serializable
{
  private static final long serialVersionUID = -2991854161009191830L;

  /**
     Default Constructor for SecureRandomSpi
   */
  public SecureRandomSpi()
  {
  }

  /**
     Updates the seed for SecureRandomSpi but does not reset seed.
     It does to this so repeated called never decrease randomness.
   */
  protected abstract void engineSetSeed(byte[] seed);

  /**
     Gets a user specified number of bytes depending on the length
     of the array?

     @param bytes array to fill with random bytes
   */
  protected abstract void engineNextBytes(byte[] bytes);

  /**
     Gets a user specified number of bytes specified by the
     parameter.

     @param numBytes number of random bytes to generate

     @return an array full of random bytes
   */
  protected abstract byte[] engineGenerateSeed(int numBytes);
}
