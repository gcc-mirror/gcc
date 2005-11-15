/* XMLStreamWriterImpl.java -- 
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

import java.io.IOException;
import java.io.Writer;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.helpers.NamespaceSupport;

/**
 * Simple XML stream writer.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLStreamWriterImpl
  implements XMLStreamWriter
{

  /**
   * The underlying character stream to write to.
   */
  protected final Writer writer;

  /**
   * The encoding being used.
   * Note that this must match the encoding of the character stream.
   */
  protected final String encoding;

  /**
   * Whether prefix defaulting is being used.
   * If true and a prefix has not been defined for a namespace specified on
   * an element or an attribute, a new prefix and namespace declaration will
   * be created.
   */
  protected final boolean prefixDefaulting;

  /**
   * The namespace context used to determine the namespace-prefix mappings
   * in scope.
   */
  protected NamespaceContext namespaceContext;
  
  /**
   * The stack of elements in scope.
   * Used to close the remaining elements.
   */
  private LinkedList elements;

  /**
   * Whether a start element has been opened but not yet closed.
   */
  private boolean inStartElement;

  /**
   * Whether we are in an empty element.
   */
  private boolean emptyElement;
  
  private NamespaceSupport namespaces;
  private int count = 0;

  /**
   * Constructor.
   * @see #writer
   * @see #encoding
   * @see #prefixDefaulting
   */
  protected XMLStreamWriterImpl(Writer writer, String encoding,
                                boolean prefixDefaulting)
  {
    this.writer = writer;
    this.encoding = encoding;
    this.prefixDefaulting = prefixDefaulting;
    elements = new LinkedList();
    namespaces = new NamespaceSupport();
  }

  /**
   * Write the end of a start-element event.
   * This will close the element if it was defined to be an empty element.
   */
  private void endStartElement()
    throws IOException
  {
    if (!inStartElement)
      return;
    if (emptyElement)
      {
        writer.write('/');
        elements.removeLast();
        namespaces.popContext();
        emptyElement = false;
      }
    writer.write('>');
    inStartElement = false;
  }

  public void writeStartElement(String localName)
    throws XMLStreamException
  {
    try
      {
        endStartElement();
        namespaces.pushContext();
        
        writer.write('<');
        writer.write(localName);
        
        elements.addLast(new String[] { null, localName });
        inStartElement = true;
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeStartElement(String namespaceURI, String localName)
    throws XMLStreamException
  {
    try
      {
        endStartElement();
        namespaces.pushContext();
        
        String prefix = getPrefix(namespaceURI);
        boolean isDeclared = (prefix != null);
        if (!isDeclared)
          {
            if (prefixDefaulting)
              prefix = createPrefix(namespaceURI);
            else
              throw new XMLStreamException("namespace " + namespaceURI +
                                           " has not been declared");
          }
        writer.write('<');
        if (!"".equals(prefix))
          {
            writer.write(prefix);
            writer.write(':');
          }
        writer.write(localName);
        inStartElement = true;
        if (!isDeclared)
          {
            writeNamespace(prefix, namespaceURI);
          }
        
        elements.addLast(new String[] { prefix, localName });
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  /**
   * Creates a new unique prefix in the document.
   * Subclasses may override this method to provide a suitably unique prefix
   * for the given namespace.
   * @param namespaceURI the namespace URI
   */
  protected String createPrefix(String namespaceURI)
  {
    Set prefixes = new HashSet();
    for (Enumeration e = namespaces.getPrefixes(); e.hasMoreElements(); )
      prefixes.add(e.nextElement());
    String ret;
    do
      {
        ret = "ns" + (count++);
      }
    while (prefixes.contains(ret));
    return ret;
  }

  public void writeStartElement(String prefix, String localName,
                                String namespaceURI)
    throws XMLStreamException
  {
    try
      {
        endStartElement();
        namespaces.pushContext();
        
        String currentPrefix = getPrefix(namespaceURI);
        boolean isCurrent = prefix.equals(currentPrefix);
        writer.write('<');
        if (!"".equals(prefix))
          {
            writer.write(prefix);
            writer.write(':');
          }
        writer.write(localName);
        if (prefixDefaulting && !isCurrent)
          {
            writeNamespace(prefix, namespaceURI);
          }
        
        elements.addLast(new String[] { prefix, localName });
        inStartElement = true;
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeEmptyElement(String namespaceURI, String localName)
    throws XMLStreamException
  {
    writeStartElement(namespaceURI, localName);
    emptyElement = true;
  }

  public void writeEmptyElement(String prefix, String localName,
                                String namespaceURI)
    throws XMLStreamException
  {
    writeStartElement(prefix, localName, namespaceURI);
    emptyElement = true;
  }

  public void writeEmptyElement(String localName)
    throws XMLStreamException
  {
    writeStartElement(localName);
    emptyElement = true;
  }

  public void writeEndElement()
    throws XMLStreamException
  {
    try
      {
        endStartElement();
        String[] element = (String[]) elements.removeLast();
        namespaces.popContext();

        writer.write('<');
        writer.write('/');
        if (element[0] != null && !"".equals(element[0]))
          {
            writer.write(element[0]);
            writer.write(':');
          }
        writer.write(element[1]);
        writer.write('>');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeEndDocument()
    throws XMLStreamException
  {
    while (!elements.isEmpty())
      writeEndElement();
  }

  public void close()
    throws XMLStreamException
  {
    flush();
  }

  public void flush()
    throws XMLStreamException
  {
    try
      {
        writer.flush();
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeAttribute(String localName, String value)
    throws XMLStreamException
  {
    if (!inStartElement)
      throw new IllegalStateException();
    try
      {
        writer.write(' ');
        writer.write(localName);
        writer.write('=');
        writer.write('"');
        writeEncoded(value, true);
        writer.write('"');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeAttribute(String prefix, String namespaceURI,
                             String localName, String value)
    throws XMLStreamException
  {
    if (!inStartElement)
      throw new IllegalStateException();
    try
      {
        String currentPrefix = getPrefix(namespaceURI);
        if (currentPrefix == null)
          {
            if (prefixDefaulting)
              writeNamespace(prefix, namespaceURI);
            else
              throw new XMLStreamException("namespace " + namespaceURI +
                                           " is not bound");
          }
        else if (!currentPrefix.equals(prefix))
          throw new XMLStreamException("namespace " + namespaceURI +
                                       " is bound to prefix " +
                                       currentPrefix);
        writer.write(' ');
        if (!"".equals(prefix))
          {
            writer.write(prefix);
            writer.write(':');
          }
        writer.write(localName);
        writer.write('=');
        writer.write('"');
        writeEncoded(value, true);
        writer.write('"');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeAttribute(String namespaceURI, String localName,
                             String value)
    throws XMLStreamException
  {
    if (!inStartElement)
      throw new IllegalStateException();
    try
      {
        String prefix = getPrefix(namespaceURI);
        if (prefix == null)
          {
            if (prefixDefaulting)
              {
                prefix = XMLConstants.DEFAULT_NS_PREFIX;
                writeNamespace(prefix, namespaceURI);
              }
            else
              throw new XMLStreamException("namespace " + namespaceURI +
                                           " is not bound");
          }
        writer.write(' ');
        if (!"".equals(prefix))
          {
            writer.write(prefix);
            writer.write(':');
          }
        writer.write(localName);
        writer.write('=');
        writer.write('"');
        writeEncoded(value, true);
        writer.write('"');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeNamespace(String prefix, String namespaceURI)
    throws XMLStreamException
  {
    if (!inStartElement)
      throw new IllegalStateException();
    try
      {
        if (prefix == null)
          prefix = XMLConstants.DEFAULT_NS_PREFIX;

        setPrefix(prefix, namespaceURI);
        
        writer.write(' ');
        writer.write("xmlns");
        if (!XMLConstants.DEFAULT_NS_PREFIX.equals(prefix))
          {
            writer.write(':');
            writer.write(prefix);
          }
        writer.write('=');
        writer.write('"');
        writer.write(namespaceURI);
        writer.write('"');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeDefaultNamespace(String namespaceURI)
    throws XMLStreamException
  {
    writeNamespace(XMLConstants.DEFAULT_NS_PREFIX, namespaceURI);
  }

  public void writeComment(String data)
    throws XMLStreamException
  {
    try
      {
        endStartElement();
        
        if (data != null && data.indexOf("--") != -1)
          throw new IllegalArgumentException(data);
        
        writer.write("<!--");
        if (data != null)
          writer.write(data);
        writer.write("-->");
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeProcessingInstruction(String target)
    throws XMLStreamException
  {
    writeProcessingInstruction(target, null);
  }

  public void writeProcessingInstruction(String target, String data)
    throws XMLStreamException
  {
    try
      {
        endStartElement();

        writer.write('<');
        writer.write('?');
        writer.write(target);
        if (data != null)
          {
            writer.write(' ');
            writer.write(data);
          }
        writer.write('?');
        writer.write('>');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeCData(String data)
    throws XMLStreamException
  {
    try
      {
        endStartElement();

        if (data.indexOf("]]") != -1)
          throw new IllegalArgumentException(data);

        writer.write("<![CDATA[");
        writer.write(data);
        writer.write("]]>");
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeDTD(String dtd)
    throws XMLStreamException
  {
    try
      {
        writer.write("<!DOCTYPE ");
        writer.write(dtd);
        writer.write('>');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeEntityRef(String name)
    throws XMLStreamException
  {
    try
      {
        endStartElement();

        writer.write('&');
        writer.write(name);
        writer.write(';');
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeStartDocument()
    throws XMLStreamException
  {
    writeStartDocument(null, null);
  }

  public void writeStartDocument(String version)
    throws XMLStreamException
  {
    writeStartDocument(null, version);
  }

  public void writeStartDocument(String encoding, String version)
    throws XMLStreamException
  {
    if (version == null)
      version = "1.0";
    encoding = this.encoding; // YES: the parameter must be ignored
    if (encoding == null)
      encoding = "UTF-8";
    if (!"1.0".equals(version) && !"1.1".equals(version))
      throw new IllegalArgumentException(version);
    try
      {
        writer.write("<?xml version=\"");
        writer.write(version);
        writer.write("\" encoding=\"");
        writer.write(encoding);
        writer.write("\"?>");
        writer.write(System.getProperty("line.separator"));
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeCharacters(String text)
    throws XMLStreamException
  {
    try
      {
        endStartElement();

        if (text != null)
          writeEncoded(text, false);
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public void writeCharacters(char[] text, int start, int len)
    throws XMLStreamException
  {
    try
      {
        endStartElement();

        int end = start + len;
        len = 0;
        for (int i = start; i < end; i++)
          {
            char c = text[i];
            if (c == '<' || c == '>' || c == '&')
              {
                writer.write(text, start, len);
                if (c == '<')
                  writer.write("&lt;");
                else if (c == '>')
                  writer.write("&gt;");
                else
                  writer.write("&amp;");
                start = i + 1;
                len = 0;
              }
            else
              len++;
          }
        if (len > 0)
          writer.write(text, start, len);
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public String getPrefix(String uri)
    throws XMLStreamException
  {
    String prefix = namespaces.getPrefix(uri);
    if (prefix == null && namespaceContext != null)
      prefix = namespaceContext.getPrefix(uri);
    return prefix;
  }

  public void setPrefix(String prefix, String uri)
    throws XMLStreamException
  {
    if (!namespaces.declarePrefix(prefix, uri))
      throw new XMLStreamException("illegal prefix " + prefix);
  }

  public void setDefaultNamespace(String uri)
    throws XMLStreamException
  {
    if (!namespaces.declarePrefix(XMLConstants.DEFAULT_NS_PREFIX, uri))
      throw new XMLStreamException("illegal default namespace prefix");
  }

  public void setNamespaceContext(NamespaceContext context)
    throws XMLStreamException
  {
    namespaceContext = context;
  }

  public NamespaceContext getNamespaceContext()
  {
    return namespaceContext;
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    throw new IllegalArgumentException(name);
  }

  /**
   * Write the specified text, ensuring that the content is suitably encoded
   * for XML.
   * @param text the text to write
   * @param inAttr whether we are in an attribute value
   */
  private void writeEncoded(String text, boolean inAttr)
    throws IOException
  {
    char[] chars = text.toCharArray();
    int start = 0;
    int end = chars.length;
    int len = 0;
    for (int i = start; i < end; i++)
      {
        char c = chars[i];
        if (c == '<' || c == '>' || c == '&')
          {
            writer.write(chars, start, len);
            if (c == '<')
              writer.write("&lt;");
            else if (c == '>')
              writer.write("&gt;");
            else
              writer.write("&amp;");
            start = i + 1;
            len = 0;
          }
        else if (inAttr && (c == '"' || c == '\''))
          {
            writer.write(chars, start, len);
            if (c == '"')
              writer.write("&quot;");
            else
              writer.write("&apos;");
            start = i + 1;
            len = 0;
          }
        else
          len++;
      }
    if (len > 0)
      writer.write(chars, start, len);
  }
  
}

