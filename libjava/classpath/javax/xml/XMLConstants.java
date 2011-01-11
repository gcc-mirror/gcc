/* XMLConstants.java --
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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

package javax.xml;

/**
 * Repository for well-known XML constants.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 * @since 1.5
 */
public final class XMLConstants
{

  private XMLConstants()
  {
    // to prevent instantiation
  }

  /**
   * Dummy namespace URI indicating that there is no namespace.
   * @see http://www.w3.org/TR/REC-xml-names/#defaulting
   */
  public static final String NULL_NS_URI = "";

  /**
   * Dummy namespace prefix indicating that there is no namespace.
   * @see http://www.w3.org/TR/REC-xml-names/#ns-qualnames
   */
  public static final String DEFAULT_NS_PREFIX = "";

  /**
   * The XML Namespace URI.
   * @see http://www.w3.org/TR/REC-xml-names/#ns-qualnames
   */
  public static final String XML_NS_URI =
                "http://www.w3.org/XML/1998/namespace";

  /**
   * The XML Namespace prefix.
   * @see http://www.w3.org/TR/REC-xml-names/#ns-qualnames
   */
  public static final String XML_NS_PREFIX = "xml";

  /**
   * The XML Namespace declaration URI.
   * @see http://www.w3.org/TR/REC-xml-names/#ns-qualnames
   */
  public static final String XMLNS_ATTRIBUTE_NS_URI =
                "http://www.w3.org/2000/xmlns/";

  /**
   * The XML Namespace declaration attribute.
   * @see http://www.w3.org/TR/REC-xml-names/#ns-qualnames
   */
  public static final String XMLNS_ATTRIBUTE = "xmlns";

  /**
   * The XML Schema (XSD) namespace URI.
   * @see http://www.w3.org/TR/xmlschema-1/#Instance_Document_Constructions
   */
  public static final String W3C_XML_SCHEMA_NS_URI =
                "http://www.w3.org/2001/XMLSchema";

  /**
   * The XML Schema Instance (XSI) namespace URI.
   * @see http://www.w3.org/TR/xmlschema-1/#Instance_Document_Constructions
   */
  public static final String W3C_XML_SCHEMA_INSTANCE_NS_URI =
                "http://www.w3.org/2001/XMLSchema-instance";

  /**
   * The XPath 2.0 datatypes namespace URI.
   * @see http://www.w3.org/TR/xpath-datamodel
   */
  public static final String W3C_XPATH_DATATYPE_NS_URI =
                "http://www.w3.org/2003/11/xpath-datatypes";

  /**
   * The XML DTD namespace URI.
   */
  public static final String XML_DTD_NS_URI = "http://www.w3.org/TR/REC-xml";

  /**
   * The RELAX NG Namespace URI.
   * @see http://relaxng.org/spec-20011203.html
   */
  public static final String RELAXNG_NS_URI =
                "http://relaxng.org/ns/structure/1.0";

  /**
   * DOM feature for secure processing.
   */
  public static final String FEATURE_SECURE_PROCESSING =
                "http://javax.xml.XMLConstants/feature/secure-processing";

}
