/* OutputKeys.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package javax.xml.transform;

/**
 * Constants for XSLT output attributes.
 * 
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class OutputKeys
{

  /**
   * The output method (xml, html, or text).
   */
  public static final String METHOD = "method";
  
  /**
   * The version of the output method.
   */
  public static final String VERSION = "version";
  
  /**
   * The preferred output character encoding.
   */
  public static final String ENCODING = "encoding";
  
  /**
   * Whether not to output an XML declaration (yes or no).
   */
  public static final String OMIT_XML_DECLARATION = "omit-xml-declaration";
  
  /**
   * Whether to output a standalone document declaration (yes or no).
   */
  public static final String STANDALONE = "standalone";
 
   /**
   * The public ID to output in the doctype declaration.
   */
  public static final String DOCTYPE_PUBLIC = "doctype-public";
  
  /**
   * The system ID to output in the doctype declaration.
   */
  public static final String DOCTYPE_SYSTEM = "doctype-system";
  
  /**
   * Whitespace-separated list of element names for which text children
   * should be output as CDATA sections.
   */
  public static final String CDATA_SECTION_ELEMENTS = "cdata-section-elements";
  
  /**
   * Whether to indent the result tree (yes or no).
   */
  public static final String INDENT = "indent";
  
  /**
   * The MIME content type of the output data.
   */
  public static final String MEDIA_TYPE = "media-type";
  
  private OutputKeys()
  {
  }

}
