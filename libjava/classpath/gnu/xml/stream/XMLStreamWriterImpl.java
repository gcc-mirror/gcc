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

  private boolean xml11;
  private boolean hasXML11RestrictedChars;

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
        if (!isName(localName))
          throw new IllegalArgumentException("illegal Name: " + localName);

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
        if (namespaceURI != null && !isURI(namespaceURI))
          throw new IllegalArgumentException("illegal URI: " + namespaceURI);
        if (!isName(localName))
          throw new IllegalArgumentException("illegal Name: " + localName);

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
            writeNamespaceImpl(prefix, namespaceURI);
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
        if (namespaceURI != null && !isURI(namespaceURI))
          throw new IllegalArgumentException("illegal URI: " + namespaceURI);
        if (prefix != null && !isPrefix(prefix))
          throw new IllegalArgumentException("illegal NCName: " + prefix);
        if (!isNCName(localName))
          throw new IllegalArgumentException("illegal NCName: " + localName);

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
            writeNamespaceImpl(prefix, namespaceURI);
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
    if (elements.isEmpty())
      throw new IllegalStateException("no matching start element");
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
        if (!isName(localName))
          throw new IllegalArgumentException("illegal Name: " + localName);
        if (!isChars(value))
          throw new IllegalArgumentException("illegal character: " + value);

        writer.write(' ');
        writer.write(localName);
        writer.write('=');
        writer.write('"');
        if (hasXML11RestrictedChars)
          writeEncodedWithRestrictedChars(value, true);
        else
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
        if (namespaceURI != null && !isURI(namespaceURI))
          throw new IllegalArgumentException("illegal URI: " + namespaceURI);
        if (prefix != null && !isPrefix(prefix))
          throw new IllegalArgumentException("illegal NCName: " + prefix);
        if (!isNCName(localName))
          throw new IllegalArgumentException("illegal NCName: " + localName);
        if (!isChars(value))
          throw new IllegalArgumentException("illegal character: " + value);

        String currentPrefix = getPrefix(namespaceURI);
        if (currentPrefix == null)
          {
            if (prefixDefaulting)
              writeNamespaceImpl(prefix, namespaceURI);
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
        if (hasXML11RestrictedChars)
          writeEncodedWithRestrictedChars(value, true);
        else
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
        if (namespaceURI != null && !isURI(namespaceURI))
          throw new IllegalArgumentException("illegal URI: " + namespaceURI);
        if (!isName(localName))
          throw new IllegalArgumentException("illegal Name: " + localName);
        if (!isChars(value))
          throw new IllegalArgumentException("illegal character: " + value);

        String prefix = getPrefix(namespaceURI);
        if (prefix == null)
          {
            if (prefixDefaulting)
              {
                prefix = XMLConstants.DEFAULT_NS_PREFIX;
                writeNamespaceImpl(prefix, namespaceURI);
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
        if (hasXML11RestrictedChars)
          writeEncodedWithRestrictedChars(value, true);
        else
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
    if (prefix == null || "".equals(prefix) || "xmlns".equals(prefix))
    {
      writeDefaultNamespace(namespaceURI);
      return;
    }
    if (!inStartElement)
      throw new IllegalStateException();
    try
      {
        if (!isURI(namespaceURI))
          throw new IllegalArgumentException("illegal URI: " + namespaceURI);
        if (!isPrefix(prefix))
          throw new IllegalArgumentException("illegal NCName: " + prefix);
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
    writeNamespaceImpl(prefix, namespaceURI);
  }

  private void writeNamespaceImpl(String prefix, String namespaceURI)
    throws XMLStreamException
  {
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
    if (!inStartElement)
      throw new IllegalStateException();
    if (!isURI(namespaceURI))
      throw new IllegalArgumentException("illegal URI: " + namespaceURI);
    writeNamespaceImpl(XMLConstants.DEFAULT_NS_PREFIX, namespaceURI);
  }

  public void writeComment(String data)
    throws XMLStreamException
  {
    if (data == null)
      return;
    try
      {
        if (!isChars(data))
          throw new IllegalArgumentException("illegal XML character: " + data);
        if (data.indexOf("--") != -1)
          throw new IllegalArgumentException("illegal comment: " + data);

        endStartElement();

        writer.write("<!--");
        if (hasXML11RestrictedChars)
          {
            int[] seq = UnicodeReader.toCodePointArray(data);
            for (int i = 0; i < seq.length; i++)
              {
                int c = seq[i];
                if (XMLParser.isXML11RestrictedChar(c))
                  writer.write("&#x" + Integer.toHexString(c) + ";");
                else
                  writer.write(Character.toChars(i));
              }
          }
        else
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
        if (!isName(target) || "xml".equalsIgnoreCase(target))
          throw new IllegalArgumentException("illegal PITarget: " + target);
        if (data != null && !isChars(data))
          throw new IllegalArgumentException("illegal XML character: " + data);

        endStartElement();

        writer.write('<');
        writer.write('?');
        writer.write(target);
        if (data != null)
          {
            writer.write(' ');
            if (hasXML11RestrictedChars)
              {
                int[] seq = UnicodeReader.toCodePointArray(data);
                for (int i = 0; i < seq.length; i++)
                  {
                    int c = seq[i];
                    if (XMLParser.isXML11RestrictedChar(c))
                      writer.write("&#x" + Integer.toHexString(c) + ";");
                    else
                      writer.write(Character.toChars(i));
                  }
              }
            else
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
        if (!isChars(data) || hasXML11RestrictedChars)
          throw new IllegalArgumentException("illegal XML character: " + data);
        if (data.indexOf("]]") != -1)
          throw new IllegalArgumentException("illegal CDATA section: " + data);

        endStartElement();

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
        // XXX: Should we parse the doctypedecl at this point to ensure
        // wellformedness?
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
        if (!isName(name))
          throw new IllegalArgumentException("illegal Name: " + name);

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
    else if ("1.1".equals(version))
      xml11 = true;
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
    if (text == null)
      return;
    try
      {
        if (!isChars(text))
          throw new IllegalArgumentException("illegal XML character: " + text);

        endStartElement();

        if (hasXML11RestrictedChars)
          writeEncodedWithRestrictedChars(text, false);
        else
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
    writeCharacters(new String(text, start, len));
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
    try
      {
        if (!isURI(uri))
          throw new IllegalArgumentException("illegal URI: " + uri);
        if (!isPrefix(prefix))
          throw new IllegalArgumentException("illegal NCName: " + prefix);
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
    if (!namespaces.declarePrefix(prefix, uri))
      throw new XMLStreamException("illegal prefix " + prefix);
  }

  public void setDefaultNamespace(String uri)
    throws XMLStreamException
  {
    if (!isURI(uri))
      throw new IllegalArgumentException("illegal URI: " + uri);
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

  /**
   * Writes the specified text, in the knowledge that some of the
   * characters are XML 1.1 restricted characters.
   */
  private void writeEncodedWithRestrictedChars(String text, boolean inAttr)
    throws IOException
  {
    int[] seq = UnicodeReader.toCodePointArray(text);
    for (int i = 0; i < seq.length; i++)
      {
        int c = seq[i];
        switch (c)
          {
          case 0x3c: // '<'
            writer.write("&lt;");
            break;
          case 0x3e: // '>'
            writer.write("&gt;");
            break;
          case 0x26: // '&'
            writer.write("&amp;");
            break;
          case 0x22: // '"'
            if (inAttr)
              writer.write("&quot;");
            else
              writer.write(c);
            break;
          case 0x27: // '\''
            if (inAttr)
              writer.write("&apos;");
            else
              writer.write(c);
            break;
          default:
            if (XMLParser.isXML11RestrictedChar(c))
              writer.write("&#x" + Integer.toHexString(c) + ";");
            else
              {
                char[] chars = Character.toChars(c);
                writer.write(chars, 0, chars.length);
              }
          }
      }
  }

  private boolean isName(String text)
    throws IOException
  {
    if (text == null)
      return false;
    int[] seq = UnicodeReader.toCodePointArray(text);
    if (seq.length < 1)
      return false;
    if (!XMLParser.isNameStartCharacter(seq[0], xml11))
      return false;
    for (int i = 1; i < seq.length; i++)
      {
        if (!XMLParser.isNameCharacter(seq[i], xml11))
          return false;
      }
    return true;
  }

  private boolean isPrefix(String text)
    throws IOException
  {
    if (XMLConstants.DEFAULT_NS_PREFIX.equals(text)) {
        return true;
    }
    return isNCName(text);
  }

  private boolean isNCName(String text)
    throws IOException
  {
    if (text == null)
      return false;
    int[] seq = UnicodeReader.toCodePointArray(text);
    if (seq.length < 1)
      return false;
    if (!XMLParser.isNameStartCharacter(seq[0], xml11) || seq[0] == 0x3a)
      return false;
    for (int i = 1; i < seq.length; i++)
      {
        if (!XMLParser.isNameCharacter(seq[i], xml11) || seq[i] == 0x3a)
          return false;
      }
    return true;
  }

  private boolean isChars(String text)
    throws IOException
  {
    if (text == null)
      return false;
    int[] seq = UnicodeReader.toCodePointArray(text);
    hasXML11RestrictedChars = false;
    if (xml11)
      {
        for (int i = 0; i < seq.length; i++)
          {
            if (!XMLParser.isXML11Char(seq[i]))
              return false;
            if (XMLParser.isXML11RestrictedChar(seq[i]))
              hasXML11RestrictedChars = true;
          }
      }
    else
      {
        for (int i = 0; i < seq.length; i++)
          {
            if (!XMLParser.isChar(seq[i]))
              return false;
          }
      }
    return true;
  }

  private boolean isURI(String text)
  {
    if (text == null)
      return false;
    char[] chars = text.toCharArray();
    if (chars.length < 1)
      return false;
    for (int i = 0; i < chars.length; i++)
      {
        if (chars[i] < 0x20 || chars[i] >= 0x7f)
          return false;
      }
    return true;
  }

}
