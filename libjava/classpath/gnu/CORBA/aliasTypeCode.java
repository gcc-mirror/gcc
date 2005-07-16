/* aliasTypeCode.java --
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


package gnu.CORBA;

import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;

/**
 * The type code that is an alias of another type code.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class aliasTypeCode
  extends primitiveTypeCode
{
  /**
   * The typecode repository id.
   */
  protected final String id;

  /**
   * The typecode name.
   */
  protected final String name;

  /**
   * The type code for that this typecode is an alias.
   */
  protected final TypeCode aliasFor;

  /**
   * Create the typecode, specifying for that typecode it is an
   * alias and the id and name of the newly created typecode.
   *
   * @param an_aliasFor the typecode, for that this typecode is an
   * alias.
   *
   * @param an_id the repository id fo the newly created typecode.
   *
   * @param a_name the name of the newly created typecode.
   */
  public aliasTypeCode(TypeCode an_aliasFor, String an_id, String a_name)
  {
    super(TCKind.tk_alias);
    aliasFor = an_aliasFor;
    id = an_id;
    name = a_name;
  }

  /**
   * Get the typecode, for that this typecode is an alias.
   */
  public TypeCode content_type()
  {
    return aliasFor;
  }

  /**
   * The objects are assumed to be equal if they repository
   * ids are both equal or both unavailable and the
   * kind values are equal.
   *
   * @param other the other typecode to compare.
   */
  public boolean equal(TypeCode other)
  {
    if (super.equal(other))
      return true;
    try
      {
        return id.equals(other.id());
      }
    catch (BadKind ex)
      {
        return false;
      }
  }

  /**
   * Return true if the given typecode is equal for
   * either this typecode of the alias typecode.
   *
   * @param other the typecode to compare.
   */
  public boolean equivalent(TypeCode other)
  {
    return other.equal(this) || other.equal(aliasFor);
  }

  /**
   * Get the repository id of this typecode.
   */
  public String id()
  {
    return id;
  }

  /**
   * Get the name of this typecode.
   */
  public String name()
  {
    return name;
  }
}
