/* AttributeList.java --
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


package javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.support.gnuStringIntMapper;

import java.io.Serializable;

import java.util.Enumeration;
import java.util.Vector;

/**
 * <p>
 * Stores the attribute information, obtained by parsing SGML (DTD) tag
 * <code>&lt;!ATTLIST .. &gt;</code></p>
 * <p>
 * Elements can have a associated named properties (attributes) having the
 * assigned values. The element start tag can have any number of attribute
 * value pairs, separated by spaces. They can appear in any order.
 * SGML requires you to delimit the attribute values using either double (")
 * or single (') quotation marks.  In HTML, it is possible
 * (but not recommended) to specify the value of an attribute without
 * quotation marks. Such attribute value may only contain
 * letters, digits, hyphens (-) and periods (.) .
 * </p>
 * <p>
 * The <code>AttributeList</code> defines a single attribute that additionally
 * has a pointer referencing the possible subsequent attribute.
 * The whole structure is just a simple linked list, storing all attributes of
 * some <code>Element</code>.
 * Use the <code>getNext()</code> method repeatedly to see all attributes in
 * the list.
 * </p>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class AttributeList
  implements DTDConstants, Serializable
{
  /** Maps between type names and they string values. */
  private static final gnuStringIntMapper mapper =
    new gnuStringIntMapper()
    {
      protected void create()
      {
        add("CDATA", DTDConstants.CDATA);
        add("ENTITY", DTDConstants.ENTITY);
        add("ENTITIES", DTDConstants.ENTITIES);
        add("ID", DTDConstants.ID);
        add("IDREF", DTDConstants.IDREF);
        add("IDREFS", DTDConstants.IDREFS);
        add("NAME", DTDConstants.NAME);
        add("NAMES", DTDConstants.NAMES);
        add("NMTOKEN", DTDConstants.NMTOKEN);
        add("NMTOKENS", DTDConstants.NMTOKENS);
        add("NOTATION", DTDConstants.NOTATION);
        add("NUMBER", DTDConstants.NUMBER);
        add("NUMBERS", DTDConstants.NUMBERS);
        add("NUTOKEN", DTDConstants.NUTOKEN);
        add("NUTOKENS", DTDConstants.NUTOKENS);
      }
    };

  /** Use serialVersionUID for interoperability. */
  private static final long serialVersionUID = -1361214058742015233L;

  /**
   * The value of ( = pointer to ) the next attribute in the linked list,
   * storing all attributes of some Element. Contains null for the
   * last attribute.
   */
  public AttributeList next;

  /**
   * The name of the attribute. The attribute names are case insensitive.
   */
  public String name;

  /**
   * The default value of this attribute. Equals to null if no default value
   * is specified.
   */
  public String value;

  /**
   * The explicit set of the allowed values of this attribute. Equals to
   * null, if this parameter was not specified.
   * Values, defined in DTD, are case insensitive.
   */
  public Vector values;

  /**
   * The modifier of this attribute. This field contains one of the
   * following DTD constants:
   * <ul>
   * <li> REQUIRED if the attribute value is always required,</li>
   * <li> IMPLIED if the user agent must supply the default value itself,</li>
   * <li> FIXED if the attribute value is fixed to some value and cannot
   * be changed.</li>
   * <li> DEFAULT if the attribute default value has been supplied.</li>
   * <li> CURRENT the value that at any point in the document is
   * the last value supplied for that element. A value is required to be
   * supplied for the first* occurrence of an element</li>
   * <li> CONREF specifies the IDREF value of
   * the reference to content in another location of the document.
   * The element with this attribute is empty, the content from
   * that another location must be used instead.</li>
   * </ul>
   */
  public int modifier;

  /**
   * The type of the attribute. The possible values of this field
   * (NUMBER, NAME, ID, CDATA and so on) are defined in DTDConstants.
   */
  public int type;

  /**
   * Creates the attribute with the given name, initializing other fields
   * to the default values ( 0 and null ).
   *
   * @param a_name The name of the attribute.
   */
  public AttributeList(String a_name)
  {
    name = a_name;
  }

  /**
   * Creates the attribute with the given properties.
   * @param a_name The name of the attribute
   * @param a_type The type of the attribute. The possible values are defined
   * in <code> DTDConstants</code>.
   * @param a_modifier The modifier of this attribute. The possible values
   * are defined in <code> DTDConstants</code>.
   * @param a_default The default value of this attribute
   * @param allowed_values The explicit set of the allowed values of
   * this attribute
   * @param a_next The value of the subsequent instance of the AttributeList,
   * representing the next attribute definition for the same element.
   * Equals to null for the last attribute definition.
   */
  public AttributeList(String a_name, int a_type, int a_modifier,
                       String a_default, Vector allowed_values,
                       AttributeList a_next
                      )
  {
    this(a_name);
    type = a_type;
    modifier = a_modifier;
    value = a_default;
    values = allowed_values;
    next = a_next;
  }

  /**
   * Get the modifier of this attribute. This field contains one of the
   * following DTD constants:
   * <ul>
   * <li> REQUIRED if the attribute value is always required,</li>
   * <li> IMPLIED if the user agent must supply the default value itself,</li>
   * <li> FIXED if the attribute value is fixed to some value and cannot
   * be changed.</li>
   * <li> DEFAULT if the attribute default value has been supplied.</li>
   * <li> CURRENT the value that at any point in the document is
   * the last value supplied for that element. A value is required to be
   * supplied for the first* occurrence of an element</li>
   * <li> CONREF specifies the IDREF value of
   * the reference to content in another location of the document.
   * The element with this attribute is empty, the content from
   * that another location must be used instead.</li>
   * </ul>
   */
  public int getModifier()
  {
    return modifier;
  }

  /**
   * Get the name of the attribute.
   * The value is returned as it was supplied to a
   * constructor, preserving the character case.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Get the value of ( = pointer to ) the next attribute in the linked list,
   * storing all attributes of some Element. Contains null for the
   * last attribute.
   */
  public AttributeList getNext()
  {
    return next;
  }

  /**
   * Get the type of the attribute. The possible values of this field
   * (NUMBER, NAME, ID, CDATA and so on) are defined in DTDConstants.
   */
  public int getType()
  {
    return type;
  }

  /**
   * Get the default value of this attribute.
   */
  public String getValue()
  {
    return value;
  }

  /**
   * Get the allowed values of this attribute.
   */
  public Enumeration getValues()
  {
    return values.elements();
  }

  /**
   * Converts a string value, representing a valid SGLM attribute type,
   * into the corresponding value, defined in DTDConstants.
   * @param typeName the name of the type (character case is ignored).
   * @return a value from DTDConstants or DTDConstants.ANY if the
   * string is not representing a known type. The known attribute types
   * in this implementation are CDATA, ENTITY, ENTITIES, ID, IDREF, IDREFS,
   *  NAME, NAMES, NMTOKEN, NMTOKENS, NOTATION, NUMBER, NUMBERS, NUTOKEN and
   *  NUTOKENS.
   * @throws NullPointerException if the passed parameter is null.
   */
  public static int name2type(String typeName)
  {
    return mapper.get(typeName.toUpperCase());
  }

  /**
   * Returns the attribute name.
   */
  public String toString()
  {
    return name;
  }

  /**
   * Converts a value from DTDConstants into the string representation.
   * @param type - an integer value of the public static integer field,
   * defined in the DTDConstants class.
   * @return a corresponding SGML DTD keyword (UPPERCASE) or null if there
   * are no attribute type constant having the given value.
   */
  public static String type2name(int type)
  {
    return mapper.get(type);
  }
}
