/* MimeType.java -- A MIME type as defined in RFC2046 and RFC2047.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import gnu.java.lang.CPStringBuilder;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

/**
 * A MIME content type, as defined in RFCs 2045 and 2046.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class MimeType
  implements Externalizable
{
    
  static final String TSPECIALS = "()<>@,;:/[]?=\\\"";
  
  private String primaryType;
  private String subType;
  private MimeTypeParameterList parameters;
  
  /**
   * Constructor for an <code>application/*</code> content type.
   */
  public MimeType()
  {
    primaryType = "application";
    subType = "*";
    parameters = new MimeTypeParameterList();
  }
  
  /**
   * Constructor that parses a raw String.
   * @param rawdata the MIME type string
   */
  public MimeType(String rawdata)
    throws MimeTypeParseException
  {
    parse(rawdata);
  }
  
  /**
   * Constructor for a new MIME type with the given primary and sub types
   * and an empty parameter list.
   * @param primary the primary type
   * @param sub the subtype
   */
  public MimeType(String primary, String sub)
    throws MimeTypeParseException
  {
    checkValidity(primary, "Primary type is invalid");
    checkValidity(sub, "Sub type is invalid");
    primaryType = primary.toLowerCase();
    subType = sub.toLowerCase();
    parameters = new MimeTypeParameterList();
  }

  /**
   * Returns the primary type.
   */
  public String getPrimaryType()
  {
    return primaryType;
  }
  
  /**
   * Sets the primary type.
   * @param primary the new primary type
   */
  public void setPrimaryType(String primary)
    throws MimeTypeParseException
  {
    checkValidity(primary, "Primary type is invalid");
    primaryType = primary.toLowerCase();
  }
  
  /**
   * Returns the subtype.
   */
  public String getSubType()
  {
    return subType;
  }
  
  /**
   * Sets the subtype.
   * @param sub the new subtype
   */
  public void setSubType(String sub)
    throws MimeTypeParseException
  {
    checkValidity(sub, "Sub type is invalid");
    subType = sub.toLowerCase();
  }
  
  /**
   * Returns the MIME parameters.
   */
  public MimeTypeParameterList getParameters()
  {
    return parameters;
  }
  
  /**
   * Returns the parameter value for the specified name.
   * @param name the parameter name
   */
  public String getParameter(String name)
  {
    return parameters.get(name);
  }
  
  /**
   * Sets the parameter value for the specified name.
   * @param name the parameter name
   * @param value the new value
   */
  public void setParameter(String name, String value)
  {
    parameters.set(name, value);
  }
  
  /**
   * Removes the parameter value for the specified name.
   * @param name the parameter name
   */
  public void removeParameter(String name)
  {
    parameters.remove(name);
  }
  
  /**
   * Returns the complete string representation of this MIME type.
   */
  public String toString()
  {
    return new CPStringBuilder(primaryType)
      .append('/')
      .append(subType)
      .append(parameters.toString())
      .toString();
  }
  
  /**
   * Returns the string representation of this MIME type without
   * parameters.
   */
  public String getBaseType()
  {
    return new CPStringBuilder(primaryType)
      .append('/')
      .append(subType)
      .toString();
  }
  
  /**
   * Returns true if the primary and subtype of this MIME type are the
   * same as in the given MIME type.
   */
  public boolean match(MimeType type)
  {
    String primary2 = type.getPrimaryType();
    String sub2 = type.getSubType();
    return primaryType.equals(primary2) && (subType.equals(sub2) ||
                                            "*".equals(subType) ||
                                            "*".equals(sub2));
  }
  
  /**
   * Returns true if the primary and subtype of this MIME type are the
   * same as in the given MIME type string.
   */
  public boolean match(String rawdata)
    throws MimeTypeParseException
  {
    return match(new MimeType(rawdata));
  }
  
  public void writeExternal(ObjectOutput out)
    throws IOException
  {
    out.writeUTF(toString());
    out.flush();
  }
    
  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    try
      {
        parse(in.readUTF());
      }
    catch (MimeTypeParseException e)
      {
        throw new IOException(e.getMessage());
      }
  }
  
  private void parse(String rawdata)
    throws MimeTypeParseException
  {
    int si = rawdata.indexOf('/');
    int pi = rawdata.indexOf(';');
    if (si == -1)
      {
        throw new MimeTypeParseException("Unable to find a sub type.");
      }
    if (pi == -1)
      {
        primaryType = rawdata.substring(0, si).toLowerCase().trim();
        subType = rawdata.substring(si + 1).toLowerCase().trim();
        parameters = new MimeTypeParameterList();
      }
    else if (si < pi)
      {
        primaryType = rawdata.substring(0, si).toLowerCase().trim();
        subType = rawdata.substring(si + 1, pi).toLowerCase().trim();
        parameters = new MimeTypeParameterList(rawdata.substring(pi));
      }
    else
      {
        throw new MimeTypeParseException("Unable to find a sub type.");
      }
    checkValidity(primaryType, "Primary type is invalid");
    checkValidity(subType, "Sub type is invalid");
  }

  static void checkValidity(String token, String message)
    throws MimeTypeParseException
  {
    int len = token.length();
    if (len == 0)
      {
        throw new MimeTypeParseException(message, token);
      }
    for (int i = 0; i < len; i++)
      {
        char c = token.charAt(i);
        if (!isValidChar(c))
          {
            throw new MimeTypeParseException(message, token);
          }
      }
  }
  
  static boolean isValidChar(char c)
  {
    return c > ' ' && c <= '~' && TSPECIALS.indexOf(c) == -1;
  }

}

