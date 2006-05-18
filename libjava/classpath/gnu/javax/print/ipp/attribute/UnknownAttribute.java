/* UnknownAttribute.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print.ipp.attribute;

import gnu.javax.print.ipp.IppUtilities;
import gnu.javax.print.ipp.IppValueTag;

import java.net.URI;
import java.net.URISyntaxException;

import javax.print.attribute.Attribute;

/**
 * UnknownAttribute holds all the parsed Attribute information.
 * It provides methods to get the value-tag, name and value.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class UnknownAttribute implements Attribute
{
  private byte tag;
  private String name;
  private byte[] value;

  /**
   * Creates a <code>UnknownAttribute</code> object with the given values.
   *
   * @param tag the value tag 
   * @param name the attribute name
   * @param value the byte[] with the value
   */
  public UnknownAttribute(byte tag, String name, byte[] value)
  {
    this.tag = tag;
    this.name = name;
    this.value = value;
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>UnknownAttribute</code> itself.
   */
  public Class getCategory()
  {
    return UnknownAttribute.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name attributes IPP name.
   */
  public String getName()
  {
    return name;
  }
  
  /**
   * Returns the value tag
   * @return The tag.
   * 
   * @see gnu.javax.print.ipp.IppValueTag
   */
  public byte getValueTag()
  {
    return tag;
  }
  
  /**
   * Returns the name of the attribute.
   * @return The name.
   */
  public String getAttributeName()
  {
    return name;
  }
  
  /**
   * Returns the attribute value origin byte array.
   * @return The value.
   */
  public byte[] getAttributeValue()
  {
    return value;
  }
  
  /**
   * Returns the attribute value decoded as String.
   * @return The value as String.
   */
  public String getAttributeValueAsString()
  {
    return new String(value);
  }
  
  /**
   * Returns the attribute value decoded as int.
   * @return The value as int.
   */
  public int getAttributeValueAsInt()
  {
    return IppUtilities.convertToInt(value);
  }
    
  /**
   * Returns the attribute value decoded as an URI.
   * @return The value as URI.
   */
  public URI getAttributeValueAsUri()
  {
    try
      {
        return new URI(new String(value));
      }
    catch (URISyntaxException e)
      {
        return null;
      }
  }

  /**
   * Provides a string representation for some default
   * tag types (e.g. int, rangeofinteger, string, uri).
   * For other more complex types "No conversion found."
   * is returned. 
   */
  public String toString()
  {
    switch (tag)
      {
      case IppValueTag.INTEGER:
        return "" + getAttributeValueAsInt();
      case IppValueTag.RANGEOFINTEGER:
        int lower = IppUtilities.convertToInt(value[0], value[1], 
                                              value[2], value[3]);
        int upper = IppUtilities.convertToInt(value[4], value[5], 
                                              value[6], value[7]);
        return lower + "-" + upper;
      case IppValueTag.URI:
        return getAttributeValueAsUri().toString();
      case IppValueTag.KEYWORD:
      case IppValueTag.URI_SCHEME:
      case IppValueTag.CHARSET:
      case IppValueTag.NATURAL_LANGUAGE:
      case IppValueTag.MIME_MEDIA_TYPE:
      case IppValueTag.NAME_WITHOUT_LANGUAGE:
      case IppValueTag.TEXT_WITHOUT_LANGUAGE:
        return getAttributeValueAsString();
      default:
        return "No conversion found.";
      }
  }  
}
