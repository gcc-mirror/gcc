/* XMLOutputFactoryImpl.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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

package gnu.xml.stream;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.io.UnsupportedEncodingException;

import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
import javax.xml.stream.XMLEventWriter;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * Standard output factory.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLOutputFactoryImpl
  extends XMLOutputFactory
{

  protected boolean prefixDefaulting = false;

  public XMLOutputFactoryImpl()
  {
  }

  public XMLStreamWriter createXMLStreamWriter(Writer stream)
    throws XMLStreamException
  {
    // XXX try to determine character encoding of writer?
    return new XMLStreamWriterImpl(stream, null, prefixDefaulting);
  }

  public XMLStreamWriter createXMLStreamWriter(OutputStream stream)
    throws XMLStreamException
  {
    return createXMLStreamWriter(stream, "UTF-8");
  }

  public XMLStreamWriter createXMLStreamWriter(OutputStream stream,
                                               String encoding)
    throws XMLStreamException
  {
    if (encoding == null)
      encoding = "UTF-8";
    try
      {
        Writer writer = new OutputStreamWriter(stream, encoding);
        return new XMLStreamWriterImpl(writer, encoding, prefixDefaulting);
      }
    catch (UnsupportedEncodingException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public XMLStreamWriter createXMLStreamWriter(Result result)
    throws XMLStreamException
  {
    if (result instanceof StreamResult)
      {
        StreamResult sr = (StreamResult) result;
        OutputStream out = sr.getOutputStream();
        if (out != null)
          return createXMLStreamWriter(out);
        Writer writer = sr.getWriter();
        if (writer != null)
          return createXMLStreamWriter(writer);
      }
    throw new UnsupportedOperationException();
  }

  public XMLEventWriter createXMLEventWriter(OutputStream stream)
    throws XMLStreamException
  {
    XMLStreamWriter writer = createXMLStreamWriter(stream);
    return new XMLEventWriterImpl(writer);
  }

  public XMLEventWriter createXMLEventWriter(OutputStream stream,
                                             String encoding)
    throws XMLStreamException
  {
    XMLStreamWriter writer = createXMLStreamWriter(stream, encoding);
    return new XMLEventWriterImpl(writer);
  }

  public XMLEventWriter createXMLEventWriter(Writer stream)
    throws XMLStreamException
  {
    XMLStreamWriter writer = createXMLStreamWriter(stream);
    return new XMLEventWriterImpl(writer);
  }

  public XMLEventWriter createXMLEventWriter(Result result)
    throws XMLStreamException
  {
    if (result instanceof StreamResult)
      {
        StreamResult sr = (StreamResult) result;
        OutputStream out = sr.getOutputStream();
        if (out != null)
          return createXMLEventWriter(out);
        Writer writer = sr.getWriter();
        if (writer != null)
          return createXMLEventWriter(writer);
      }
    throw new UnsupportedOperationException();
  }

  public void setProperty(String name, Object value)
    throws IllegalArgumentException
  {
    if (IS_REPAIRING_NAMESPACES.equals(name))
      prefixDefaulting = ((Boolean) value).booleanValue();
    else
      throw new IllegalArgumentException(name);
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    if (IS_REPAIRING_NAMESPACES.equals(name))
      return new Boolean(prefixDefaulting);
    throw new IllegalArgumentException(name);
  }

  public boolean isPropertySupported(String name)
  {
    if (IS_REPAIRING_NAMESPACES.equals(name))
      return true;
    return false;
  }

  public boolean isPrefixDefaulting()
  {
    return prefixDefaulting;
  }

  public void setPrefixDefaulting(boolean value)
  {
    prefixDefaulting = value;
  }

}
