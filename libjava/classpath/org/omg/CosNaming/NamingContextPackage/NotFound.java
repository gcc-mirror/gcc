/* NotFound.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package org.omg.CosNaming.NamingContextPackage;

import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NameComponent;

/**
* The exception is thrown when the binding being searched does not exist
* or has the wrong type
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public final class NotFound
  extends UserException
  implements IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -7539098836265502514L;

  /**
   * The reason, explaining, why the binding cannot be found.
   * This might be either the missing node or the wrong binding type.
   */
  public NotFoundReason why;

  /**
   * Contains the remainder of the non working name.
   */
  public NameComponent[] rest_of_name;

  /**
   * Creates the exception with unitialised fields.
   */
  public NotFound()
  {
  }

  /**
   * Creates the exception, intialising the fields to the given values.
   */
  public NotFound(NotFoundReason _why, NameComponent[] _rest_of_name)
  {
    why = _why;
    rest_of_name = _rest_of_name;
  }

  /**
   * Creates the exception, intialising the fields to the given values and
   * also providing the explaining string.
   *
   * @since 1.4
   */
  public NotFound(String explanation, NotFoundReason _why,
                  NameComponent[] _rest_of_name
                 )
  {
    super(explanation);
    why = _why;
    rest_of_name = _rest_of_name;
  }
}
