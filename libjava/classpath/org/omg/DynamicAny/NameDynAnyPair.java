/* NameDynAnyPair.java --
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


package org.omg.DynamicAny;

import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Stores the named value, representing the name by string and the value by
 * {@link DynAny}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class NameDynAnyPair
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -1992533286932908564L;

  /**
   * The name of the named value.
   */
  public String id;

  /**
   * The value of the named value.
   */
  public DynAny value;

  /**
   * Create unitialised instance with both fields left with default
   * <code>null</code> value.
   */
  public NameDynAnyPair()
  {
  }

  /**
   * Create an instance with the given initial values.
   *
   * @param aName the name of the named value.
   * @param aValue the value of the named value.
   */
  public NameDynAnyPair(String aName, DynAny aValue)
  {
    id = aName;
    value = aValue;
  }
}
