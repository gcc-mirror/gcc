/* MimeType.java -- A helper class for mime handling in DataFlavor
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


package java.awt.datatransfer;

import gnu.java.lang.CPStringBuilder;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * A helper class for mime handling in DataFlavor.
 *
 * A Mauve test for DataFlavor.writeExternal() shows that a non-public
 * class java.awt.datatransfer.MimeType gets serialized. This class
 * is mainly here for serialization compatibility. Of course,
 * now that we have it here, we can just as well implement some
 * mime handling facility here.
 */
class MimeType
  implements Externalizable
{

  /**
   * The primary type.
   */
  private String primaryType;

  /**
   * The subtype.
   */
  private String subType;

  /**
   * Additional parameters to be appended to the mime string.
   */
  private HashMap parameters;

  /**
   * This is only here for deserialization.
   */
  public MimeType()
  {
    parameters = new HashMap();
  }

  /**
   * Creates a new MimeType object.
   *
   * @param mime the mime type
   */
  MimeType(String mime)
    throws MimeTypeParseException
  {
    this();
    parse(mime);
  }

  /**
   * Adds a mime parameter.
   *
   * @param param the parameter key
   * @param value the parameter value
   */
  void addParameter(String param, String value)
  {
    parameters.put(param, value);
  }

  /**
   * Removes the parameter with the specified key.
   *
   * @param param the parameter to remove
   */
  void removeParameter(String param)
  {
    parameters.remove(param);
  }

  /**
   * Returns the parameter for the <code>key</code>.
   *
   * @param key the parameter key
   *
   * @return the parameter for the <code>key</code>
   */
  String getParameter(String key)
  {
    return (String) parameters.get(key);
  }

  /**
   * Returns the primary type.
   *
   * @return the primary type
   */
  String getPrimaryType()
  {
    return primaryType;
  }

  String getSubType()
  {
    return subType;
  }

  /**
   * Returns the base type of this mime type. This is the primary
   * type plus the subtype, separated by '/'.
   *
   * @return the base type of this mime type
   */
  String getBaseType()
  {
    return primaryType + '/' + subType;
  }

  /**
   * Returns <code>true</code> if this mime type and another mime type
   * match. This will be true when their primary types are equal, and their
   * subtypes are equal (or when either subtype is * ).
   *
   * @param other the other mime type
   *
   * @return <code>true</code> if the mime types match, <code>false</code>
   *         otherwise
   */
  boolean matches(MimeType other)
  {
    boolean match = false;
    if (other != null)
      {
        match = primaryType.equals(other.primaryType)
                && (subType.equals("*") || other.subType.equals("*")
                    || subType.equals(other.subType));
      }
    return match;
  }

  /**
   * Serializes the mime type.
   *
   * @param in the input stream to read from
   *
   * @throws ClassNotFoundException not thrown here
   * @throws IOException when something goes wrong on the input stream,
   *         or when the mime type can't be parsed
   */
  public void readExternal(ObjectInput in)
    throws ClassNotFoundException, IOException
  {
    String mime = in.readUTF();
    parameters.clear();
    try
      {
        parse(mime);
      }
    catch (MimeTypeParseException ex)
      {
        IOException ioEx = new IOException();
        ioEx.initCause(ex);
        throw ioEx;
      }
  }

  /**
   * Serializes this mime type.
   *
   * @param out the output stream
   *
   * @throws IOException when something goes wrong on the output stream
   */
  public void writeExternal(ObjectOutput out)
    throws IOException
  {
    out.writeUTF(toString());
  }

  /**
   * Creates a string representation of this mime type.
   *
   * @return a string representation of this mime type
   */
  public String toString()
  {
    CPStringBuilder s = new CPStringBuilder();
    s.append(primaryType);
    s.append('/');
    s.append(subType);
    if (parameters.size() > 0)
      {
        Set entries = parameters.entrySet();
        for (Iterator i = entries.iterator(); i.hasNext();)
          {
            s.append("; ");
            Map.Entry entry = (Map.Entry) i.next();
            s.append(entry.getKey());
            s.append('=');
            s.append(entry.getValue());
          }
      }
    return s.toString();
  }

  /**
   * Parses the specified mime type string and initializes the fields
   * of this object.
   *
   * @param mime the mime type string
   */
  private void parse(String mime)
    throws MimeTypeParseException
  {
    // FIXME: Maybe implement more sophisticated mime string parsing according
    // to RFC 2045 and 2046.
    StringTokenizer tokenizer = new StringTokenizer(mime);
    try
      {
        primaryType = tokenizer.nextToken("/");
        subType = tokenizer.nextToken("/;");
      }
    catch (NoSuchElementException ex)
      {
        throw new MimeTypeParseException("Expected / separator");
      }

    // Add any parameters.
    while (tokenizer.hasMoreTokens())
      {
        String keyValuePair = tokenizer.nextToken(";");
        int i = keyValuePair.indexOf('=');
        if (i == -1)
          throw new MimeTypeParseException("Expected = as parameter separator");
        String key = keyValuePair.substring(0, i).trim();
        String value = keyValuePair.substring(i + 1).trim();
        parameters.put(key, value);
      }
  }

}
