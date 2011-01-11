/* AttributeDeclaration.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.xml.validation.xmlschema;

import gnu.xml.validation.datatype.Annotation;
import gnu.xml.validation.datatype.SimpleType;
import javax.xml.namespace.QName;

/**
 * An XML Schema attribute declaration schema component.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class AttributeDeclaration
{

  static final int NONE = 0;
  static final int DEFAULT = 1;
  static final int FIXED = 2;

  /**
   * The scope of this attribute declaration (global or local).
   */
  final boolean scope;

  /**
   * The constraint type.
   * One of NONE, DEFAULT, FIXED.
   */
  final int type;

  /**
   * The value constraint.
   */
  final String value;

  /**
   * The name of the attribute to which this declaration refers.
   */
  final QName name;

  /**
   * The type definition corresponding to this attribute.
   */
  final SimpleType datatype;

  /**
   * The annotation associated with this attribute declaration, if any.
   */
  final Annotation annotation;

  AttributeDeclaration(boolean scope, int type, String value, QName name,
                       SimpleType datatype, Annotation annotation)
  {
    this.scope = scope;
    this.type = type;
    this.value = value;
    this.name = name;
    this.datatype = datatype;
    this.annotation = annotation;
  }

}
