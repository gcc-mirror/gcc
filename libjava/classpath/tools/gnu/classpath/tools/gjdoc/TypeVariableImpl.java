/* gnu.classpath.tools.gjdoc.TypeVariableImpl
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

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.ProgramElementDoc;
import com.sun.javadoc.Type;
import com.sun.javadoc.TypeVariable;

import java.util.List;

public class TypeVariableImpl
  extends TypeImpl
  implements TypeVariable, WritableType
{

  /**
   * The bounds of this particular type variable.
   */
  Type[] bounds;

  /**
   * The owning program element of this type variable.
   */
  ProgramElementDoc owner;

  /**
   * Constructs a new type variable with the supplied name and owner.
   *
   * @param packageName the name of the package containing the type variable.
   * @param typeName the name of the type variable.
   * @param dimension the dimensions of the type variable (always "").
   * @param owner the owning program element of the type variable.
   */
  TypeVariableImpl(String packageName, String typeName, String dimension,
                   ProgramElementDoc owner)
  {
    super(packageName, typeName, dimension);
    this.owner = owner;
  }

  /**
   * Set the bounds to the contents of the supplied list.
   *
   * @param parsedBounds a list of type bounds.
   */
  void setBounds(List parsedBounds)
  {
    bounds = (Type[]) parsedBounds.toArray(new Type[parsedBounds.size()]);
  }

  /**
   * Returns the bounds of this type variable.
   *
   * @return the bounds of the variable.
   */
  public Type[] bounds()
  {
    return bounds;
  }

  /**
   * Returns the owning program element for this type variable.
   *
   * @return the owning program element, whether a class, interface,
   *         constructor or method.
   */
  public ProgramElementDoc owner()
  {
    return owner;
  }


}
