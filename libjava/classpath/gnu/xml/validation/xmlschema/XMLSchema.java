/* XMLSchema.java --
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

import java.util.LinkedHashMap;
import java.util.Map;
import javax.xml.validation.Schema;
import javax.xml.validation.Validator;
import javax.xml.validation.ValidatorHandler;

/**
 * An XML Schema schema.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class XMLSchema
  extends Schema
{

  static final int FINAL_NONE = 0x00;
  static final int FINAL_EXTENSION = 0x01;
  static final int FINAL_RESTRICTION = 0x02;
  static final int FINAL_LIST = 0x04;
  static final int FINAL_UNION = 0x08;
  static final int FINAL_ALL = 0x0f;

  static final int BLOCK_NONE = 0x00;
  static final int BLOCK_EXTENSION = 0x01;
  static final int BLOCK_RESTRICTION = 0x02;
  static final int BLOCK_SUBSTITUTION = 0x04;
  static final int BLOCK_ALL = 0x07;

  static final int GLOBAL = 0x00;
  static final int LOCAL = 0x01;
  static final int ABSENT = 0x02;

  static final int CONSTRAINT_NONE = 0x00;
  static final int CONSTRAINT_DEFAULT = 0x01;
  static final int CONSTRAINT_FIXED = 0x02;

  static final int CONTENT_EMPTY = 0x00;
  static final int CONTENT_SIMPLE = 0x01;
  static final int CONTENT_MIXED = 0x02;
  static final int CONTENT_ELEMENT_ONLY = 0x03;

  final String targetNamespace;
  final String version;
  final int finalDefault;
  final int blockDefault;
  final boolean attributeFormQualified;
  final boolean elementFormQualified;

  /**
   * The element declarations in this schema.
   */
  final Map elementDeclarations;

  /**
   * The attribute declarations in this schema.
   */
  final Map attributeDeclarations;

  /**
   * The type declarations in this schema.
   */
  final Map types;

  XMLSchema(String targetNamespace, String version,
            int finalDefault, int blockDefault,
            boolean attributeFormQualified,
            boolean elementFormQualified)
  {
    this.targetNamespace = targetNamespace;
    this.version = version;
    this.finalDefault = finalDefault;
    this.blockDefault = blockDefault;
    this.attributeFormQualified = attributeFormQualified;
    this.elementFormQualified = elementFormQualified;
    elementDeclarations = new LinkedHashMap();
    attributeDeclarations = new LinkedHashMap();
    types = new LinkedHashMap();
  }

  public Validator newValidator()
  {
    // TODO
    //return new XMLSchemaValidator(this);
    return null;
  }

  public ValidatorHandler newValidatorHandler()
  {
    // TODO
    //return new XMLSchemaValidatorHandler(this);
    return null;
  }

}
