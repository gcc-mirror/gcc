/* XMLSchemaAttributeTypeInfo.java -- 
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

import org.w3c.dom.TypeInfo;
import gnu.xml.validation.datatype.SimpleType;

/**
 * Attribute type information provided by validation against an XML Schema.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class XMLSchemaAttributeTypeInfo
  extends XMLSchemaTypeInfo
{

  final XMLSchema schema;
  final AttributeDeclaration decl;
  final SimpleType type;
  boolean id;
  final boolean specified;

  XMLSchemaAttributeTypeInfo(XMLSchema schema, AttributeDeclaration decl,
                             boolean specified)
  {
    this.schema = schema;
    this.decl = decl;
    this.specified = specified;
    type = (decl == null) ? null : decl.datatype;
  }

  public String getTypeName()
  {
    if (type == null)
      {
        return "CDATA";
      }
    return type.name.getLocalPart();
  }

  public String getTypeNamespace()
  {
    if (type == null)
      {
        return "";
      }
    return type.name.getNamespaceURI();
  }

  public boolean isDerivedFrom(String typeNamespace, String typeName,
                               int derivationMethod)
  {
    if (type == null)
      {
        return false;
      }
    else
      {
        return simpleTypeIsDerivedFrom(type, typeNamespace, typeName,
                                       derivationMethod);
      }
  }
  
}

