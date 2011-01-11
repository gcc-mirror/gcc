/* InvalidPolicy.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.PortableServer.POAPackage;

import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Raised if any of the policy objects specified is not supported by this
 * ORB implementation, if conflicting policy objects are specified,
 * or if any of the specified policy objects require prior administrative
 * action that has not been performed.
 *
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class InvalidPolicy
  extends UserException
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 3204212102282117205L;

  /**
   * The index in the policies parameter value of the first offending
   * policy object.
   */
  public short index;

  /**
   * Create InvalidPolicy with no explaining
   * message and leaving {@link #index} with default 0 value.
   */
  public InvalidPolicy()
  {
  }

  /**
   * Create the InvalidPolicy with explaining
   * message and initialisintg {@link #index} to the passed value.
   *
   * @param why a string, explaining, why this exception has been thrown.
   * @param a_index a value for index.
   */
  public InvalidPolicy(String why, short a_index)
  {
    super(why);
    this.index = a_index;
  }

  /**
   * Create the InvalidPolicy without explaining
   * message and initialisintg {@link #index} to the passed value.
   *
   * @param a_index a value for index.
   */
  public InvalidPolicy(short a_index)
  {
    this.index = a_index;
  }

  /**
   * Adds {@link #index} to the super.getMessage().
   */
  public String getMessage()
  {
    return super.getMessage() + " at index " + index;
  }
}
