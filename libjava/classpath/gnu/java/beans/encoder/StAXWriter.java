/* StAXWriter.java
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


package gnu.java.beans.encoder;

import java.io.OutputStream;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/** A {@link Writer} implementation based on the StAX API.
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 *
 */
public class StAXWriter implements Writer
{
  XMLStreamWriter writer;

  int indent = 0;

  public StAXWriter(OutputStream os)
  {
    try
      {
        XMLOutputFactory factory = XMLOutputFactory.newInstance();
        writer = factory.createXMLStreamWriter(os);
      }
    catch (XMLStreamException se)
      {
        throw (InternalError)
          new InternalError(
          "Could not instantiate a streaming XML writer.")
          .initCause(se);
      }

  }

  public void flush()
  {
    if (writer != null)
      {
        try
          {
            writer.flush();
          }
        catch (XMLStreamException xse)
          {
            // TODO: find out
          }
      }

  }

  public void close()
  {
    if (writer != null)
      {
        try
          {
            writer.close();
          }
        catch (XMLStreamException xse)
          {
            // TODO: find out
          }
        writer = null;
      }

  }

  public void writePreamble()
  {
    try
      {
        writer.writeStartDocument("UTF-8", "1.0");
      }
    catch (XMLStreamException xmlse)
      {

      }
  }

  public void writeEnd(boolean wasEmpty)
  {
    try
      {
        indent -= 2;

        if (wasEmpty)
          return;

        for (int i = 0; i < indent; i++)
          writer.writeCharacters(" ");

        writer.writeEndElement();

        writer.writeCharacters("\n");
      }
    catch (XMLStreamException xmlse)
      {

      }
  }

  public void writeEndNoChildren()
  {
    try
      {
        writer.writeEndElement();
        writer.writeCharacters("\n");
      }
    catch (XMLStreamException xmlse)
      {

      }
  }

  public void write(String tagName, boolean empty)
  {
    write(tagName, null, null, null, empty);
  }

  public void write(String tagName, String value)
  {
    write(tagName, value, null, null, value == null);
  }

  public void writeNoChildren(String tagName, String value)
  {
    try
      {
        for (int i = 0; i < indent; i++)
          writer.writeCharacters(" ");

        writer.writeStartElement(tagName);

        writer.writeCharacters(value);
      }
    catch (XMLStreamException xmlse)
      {

      }
  }

  public void write(String tagName, String attributeName,
                    String attributeValue, boolean empty)
  {
    write(tagName, null, new String[] { attributeName },
          new String[] { attributeValue }, empty);
  }

  public void write(String tagName, String value, String[] attributeNames,
                    String[] attributeValues, boolean empty)
  {
    try
      {
        for (int i = 0; i < indent; i++)

          writer.writeCharacters(" ");

        if (empty)
          writer.writeEmptyElement(tagName);
        else
          writer.writeStartElement(tagName);

        if (attributeNames != null)
          for (int i = 0; i < attributeNames.length; i++)
            writer.writeAttribute(attributeNames[i], attributeValues[i]);

        writer.writeCharacters("\n");

        indent += 2;

        if (value != null)
          {
            for (int i = 0; i < indent; i++)
              writer.writeCharacters(" ");

            writer.writeCharacters(value);

            writer.writeCharacters("\n");
          }
      }
    catch (XMLStreamException xmlse)
      {

      }
  }

  public void write(String tagName, String[] attributeNames,
                    String[] attributeValues, boolean empty)
  {
    write(tagName, null, attributeNames, attributeValues, empty);
  }

}
