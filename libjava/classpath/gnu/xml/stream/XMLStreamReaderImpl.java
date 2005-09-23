/* XMLStreamReaderImpl.java -- 
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

import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.Location;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLReporter;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.Comment;
import javax.xml.stream.events.DTD;
import javax.xml.stream.events.EndDocument;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.EndEntity;
import javax.xml.stream.events.EntityDeclaration;
import javax.xml.stream.events.EntityReference;
import javax.xml.stream.events.Namespace;
import javax.xml.stream.events.NotationDeclaration;
import javax.xml.stream.events.ProcessingInstruction;
import javax.xml.stream.events.StartDocument;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.StartEntity;
import javax.xml.stream.events.XMLEvent;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.Attributes2;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.ext.Locator2;
import org.xml.sax.helpers.NamespaceSupport;

/**
 * An XML parser.
 *
 * This implementation uses SAX to create a series of events in memory,
 * and then iterates over this series. This has the advantage of being simple
 * and unifying the existing XML parsing code. However, it is quite
 * memory-inefficient and obviously won't cope with streams of arbitrary
 * length.
 *
 * A future task could be to write a real, progressive/incremental
 * implementation of this class. In that case we should consider making that
 * the default XML parser implementation and using a SAX wrapper to it to
 * provide the GNU SAX implementation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLStreamReaderImpl
  implements XMLStreamReader, NamespaceContext
{

  private LinkedList events;
  private XMLEvent currentEvent;
  private int eventType;
  private NamespaceSupport namespaces;

  protected String publicId;
  protected String systemId;

  protected XMLResolver resolver;
  protected XMLReporter reporter;
  protected boolean validating;
  protected boolean namespaceAware;
  protected boolean coalescing;
  protected boolean replacingEntityReferences;
  protected boolean externalEntities;
  protected boolean supportDTD;

  protected XMLStreamReaderImpl(InputStream in,
                                String publicId,
                                String systemId,
                                XMLResolver resolver,
                                XMLReporter reporter,
                                boolean validating,
                                boolean namespaceAware,
                                boolean coalescing,
                                boolean replacingEntityReferences,
                                boolean externalEntities,
                                boolean supportDTD)
    throws XMLStreamException
  {
    //this.in = in;
    this.publicId = publicId;
    this.systemId = systemId;
    this.resolver = resolver;
    this.reporter = reporter;
    this.validating = validating;
    this.namespaceAware = namespaceAware;
    this.coalescing = coalescing;
    this.replacingEntityReferences = replacingEntityReferences;
    this.externalEntities = externalEntities;
    this.supportDTD = supportDTD;
    namespaces = new NamespaceSupport();
    events = new LinkedList();
    
    // Configure the SAX parser and perform the parse
    try
      {
        SAXParserFactory f = SAXParserFactory.newInstance();
        f.setNamespaceAware(namespaceAware);
        f.setValidating(validating);
        SAXParser p = f.newSAXParser();
        XMLReader r = p.getXMLReader();
        CallbackHandler ch = this.new CallbackHandler(r);
        r.setFeature("http://xml.org/sax/features/external-general-entities", 
                     externalEntities);
        r.setFeature("http://xml.org/sax/features/namespaces", 
                     namespaceAware);
        r.setContentHandler(ch);
        r.setDTDHandler(ch);
        r.setEntityResolver(ch);
        r.setErrorHandler(ch);
        r.setProperty("http://xml.org/sax/properties/lexical-handler",
                      ch);
        InputSource source = new InputSource(in);
        source.setSystemId(systemId);
        r.parse(source);
      }
    catch (SAXException e)
      {
        events.add(e);
      }
    catch (IOException e)
      {
        events.add(e);
      }
    catch (ParserConfigurationException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  protected XMLStreamReaderImpl(Reader reader,
                                String publicId,
                                String systemId,
                                XMLResolver resolver,
                                XMLReporter reporter,
                                boolean validating,
                                boolean namespaceAware,
                                boolean coalescing,
                                boolean replacingEntityReferences,
                                boolean externalEntities,
                                boolean supportDTD)
    throws XMLStreamException
  {
    //this.reader = reader;
    this.publicId = publicId;
    this.systemId = systemId;
    this.resolver = resolver;
    this.reporter = reporter;
    this.validating = validating;
    this.namespaceAware = namespaceAware;
    this.coalescing = coalescing;
    this.replacingEntityReferences = replacingEntityReferences;
    this.externalEntities = externalEntities;
    this.supportDTD = supportDTD;
    namespaces = new NamespaceSupport();
    events = new LinkedList();
    
    // Configure the SAX parser and perform the parse
    try
      {
        SAXParserFactory f = SAXParserFactory.newInstance();
        f.setNamespaceAware(namespaceAware);
        f.setValidating(validating);
        SAXParser p = f.newSAXParser();
        XMLReader r = p.getXMLReader();
        CallbackHandler ch = this.new CallbackHandler(r);
        r.setFeature("http://xml.org/sax/features/external-general-entities", 
                     externalEntities);
        r.setFeature("http://xml.org/sax/features/namespaces", 
                     namespaceAware);
        r.setContentHandler(ch);
        r.setDTDHandler(ch);
        r.setEntityResolver(ch);
        r.setErrorHandler(ch);
        r.setProperty("http://xml.org/sax/properties/lexical-handler",
                      ch);
        InputSource source = new InputSource(reader);
        source.setSystemId(systemId);
        r.parse(source);
      }
    catch (SAXException e)
      {
        events.add(e);
      }
    catch (IOException e)
      {
        events.add(e);
      }
    catch (ParserConfigurationException e)
      {
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    throw new IllegalArgumentException(name);
  }

  public int next()
    throws XMLStreamException
  {
    if (events.isEmpty())
      throw new XMLStreamException("EOF");
    Object event = events.removeFirst();
    if (event instanceof Exception)
      {
        Exception e = (Exception) event;
        XMLStreamException e2 = new XMLStreamException(e);
        e2.initCause(e);
        throw e2;
      }
    currentEvent = (XMLEvent) event;
    eventType = currentEvent.getEventType();
    return eventType;
  }

  public void require(int type, String namespaceURI, String localName)
    throws XMLStreamException
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  public String getElementText()
    throws XMLStreamException
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  public int nextTag()
    throws XMLStreamException
  {
    int ret;
    do
      {
        ret = next();
      }
    while (ret != XMLStreamConstants.START_ELEMENT &&
           ret != XMLStreamConstants.END_ELEMENT);
    return ret;
  }

  public boolean hasNext()
    throws XMLStreamException
  {
    return !events.isEmpty();
  }

  public void close()
    throws XMLStreamException
  {
  }

  public String getNamespaceURI(String prefix)
  {
    return namespaces.getURI(prefix);
  }

  public String getPrefix(String namespaceURI)
  {
    return namespaces.getPrefix(namespaceURI);
  }

  public Iterator getPrefixes(String namespaceURI)
  {
    LinkedList acc = new LinkedList();
    for (Enumeration e = namespaces.getPrefixes(namespaceURI);
         e.hasMoreElements(); )
      acc.add(e.nextElement());
    return acc.iterator();
  }

  public boolean isStartElement()
  {
    return eventType == START_ELEMENT;
  }

  public boolean isEndElement()
  {
    return eventType == END_ELEMENT;
  }

  public boolean isCharacters()
  {
    return eventType == CHARACTERS || eventType == CDATA;
  }

  public boolean isWhiteSpace()
  {
    return eventType == SPACE;
  }

  public String getAttributeValue(String namespaceURI, String localName)
  {
    StartElement se = (StartElement) currentEvent;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        Attribute attr = (Attribute) i.next();
        QName name = attr.getName();
        if (namespaceURI != null &&
            !namespaceURI.equals(name.getNamespaceURI()))
          continue;
        if (!localName.equals(name.getLocalPart()))
          continue;
        return attr.getValue();
      }
    return null;
  }

  public int getAttributeCount()
  {
    StartElement se = (StartElement) currentEvent;
    int count = 0;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        i.next();
        count++;
      }
    return count;
  }

  public QName getAttributeQName(int index)
  {
    StartElement se = (StartElement) currentEvent;
    int count = 0;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        Attribute attr = (Attribute) i.next();
        if (index == count)
          return attr.getName();
        count++;
      }
    return null;
  }

  public String getAttributeNamespace(int index)
  {
    QName name = getAttributeQName(index);
    return (name == null) ? null : name.getNamespaceURI();
  }

  public String getAttributeName(int index)
  {
    QName name = getAttributeQName(index);
    return (name == null) ? null : name.getLocalPart();
  }

  public String getAttributePrefix(int index)
  {
    QName name = getAttributeQName(index);
    return (name == null) ? null : name.getPrefix();
  }

  public String getAttributeType(int index)
  {
    StartElement se = (StartElement) currentEvent;
    int count = 0;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        Attribute attr = (Attribute) i.next();
        if (index == count)
          {
            QName type = attr.getDTDType();
            return (type == null) ? "CDATA" : type.toString();
          }
        count++;
      }
    return null;
  }

  public String getAttributeValue(int index)
  {
    StartElement se = (StartElement) currentEvent;
    int count = 0;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        Attribute attr = (Attribute) i.next();
        if (index == count)
          return attr.getValue();
        count++;
      }
    return null;
  }

  public boolean isAttributeSpecified(int index)
  {
    StartElement se = (StartElement) currentEvent;
    int count = 0;
    for (Iterator i = se.getAttributes(); i.hasNext(); )
      {
        Attribute attr = (Attribute) i.next();
        if (index == count)
          return attr.isSpecified();
        count++;
      }
    return false;
  }

  public int getNamespaceCount()
  {
    Iterator i = null;
    switch (eventType)
      {
      case XMLStreamConstants.START_ELEMENT:
        i = ((StartElement) currentEvent).getNamespaces();
        break;
      case XMLStreamConstants.END_ELEMENT:
        i = ((EndElement) currentEvent).getNamespaces();
        break;
      default:
        throw new IllegalStateException();
      }
    int count = 0;
    while (i.hasNext())
      {
        i.next();
        count++;
      }
    return count;
  }

  public String getNamespacePrefix(int index)
  {
    Iterator i = null;
    switch (eventType)
      {
      case XMLStreamConstants.START_ELEMENT:
        i = ((StartElement) currentEvent).getNamespaces();
        break;
      case XMLStreamConstants.END_ELEMENT:
        i = ((EndElement) currentEvent).getNamespaces();
        break;
      default:
        throw new IllegalStateException();
      }
    int count = 0;
    while (i.hasNext())
      {
        Namespace ns = (Namespace) i.next();
        if (index == count)
          return ns.getPrefix();
        count++;
      }
    return null;
  }

  public String getNamespaceURI(int index)
  {
    Iterator i = null;
    switch (eventType)
      {
      case XMLStreamConstants.START_ELEMENT:
        i = ((StartElement) currentEvent).getNamespaces();
        break;
      case XMLStreamConstants.END_ELEMENT:
        i = ((EndElement) currentEvent).getNamespaces();
        break;
      default:
        throw new IllegalStateException();
      }
    int count = 0;
    while (i.hasNext())
      {
        Namespace ns = (Namespace) i.next();
        if (index == count)
          return ns.getNamespaceURI();
        count++;
      }
    return null;
  }

  public NamespaceContext getNamespaceContext()
  {
    return this;
  }

  public int getEventType()
  {
    return eventType;
  }

  public String getText()
  {
    switch (eventType)
      {
      case XMLStreamConstants.CHARACTERS:
      case XMLStreamConstants.CDATA:
      case XMLStreamConstants.SPACE:
        return ((Characters) currentEvent).getData();
      case XMLStreamConstants.COMMENT:
        return ((Comment) currentEvent).getText();
      case XMLStreamConstants.ENTITY_REFERENCE:
        return ((EntityReference) currentEvent).getReplacementText();
      case XMLStreamConstants.DTD:
        return ((DTD) currentEvent).getDocumentTypeDeclaration();
      }
    return null;
  }

  public char[] getTextCharacters()
  {
    String text = getText();
    return (text == null) ? null : text.toCharArray();
  }

  public int getTextCharacters(int sourceStart, char[] target,
                               int targetStart, int length)
    throws XMLStreamException
  {
    char[] source = getTextCharacters();
    int len = Math.min(source.length, length);
    System.arraycopy(source, sourceStart, target, targetStart, len);
    return len;
  }

  public int getTextStart()
  {
    return 0;
  }

  public int getTextLength()
  {
    String text = getText();
    return (text == null) ? 0 : text.length();
  }

  public String getEncoding()
  {
    // XXX SAX doesn't provide this
    return null;
  }

  public boolean hasText()
  {
    return eventType == CHARACTERS || eventType == DTD ||
      eventType == SPACE || eventType == ENTITY_REFERENCE ||
      eventType == COMMENT || eventType == DTD;
  }

  public Location getLocation()
  {
    return currentEvent.getLocation();
  }

  public QName getName()
  {
    switch (eventType)
      {
      case XMLStreamConstants.START_ELEMENT:
        return ((StartElement) currentEvent).getName();
      case XMLStreamConstants.END_ELEMENT:
        return ((EndElement) currentEvent).getName();
      case XMLStreamConstants.ATTRIBUTE:
        return ((Attribute) currentEvent).getName();
      }
    return null;
  }

  public String getLocalName()
  {
    QName name = getName();
    return (name == null) ? null : name.getLocalPart();
  }

  public boolean hasName()
  {
    return getName() != null;
  }

  public String getNamespaceURI()
  {
    QName name = getName();
    return (name == null) ? null : name.getNamespaceURI();
  }

  public String getPrefix()
  {
    QName name = getName();
    return (name == null) ? null : name.getPrefix();
  }
  
  public String getVersion()
  {
    StartDocument sd = (StartDocument) currentEvent;
    return sd.getVersion();
  }

  public boolean isStandalone()
  {
    StartDocument sd = (StartDocument) currentEvent;
    return sd.isStandalone();
  }

  public boolean standaloneSet()
  {
    StartDocument sd = (StartDocument) currentEvent;
    return sd.standaloneSet();
  }
  
  public String getCharacterEncodingScheme()
  {
    StartDocument sd = (StartDocument) currentEvent;
    return sd.getCharacterEncodingScheme();
  }

  public String getPITarget()
  {
    ProcessingInstruction pi = (ProcessingInstruction) currentEvent;
    return pi.getTarget();
  }

  public String getPIData()
  {
    ProcessingInstruction pi = (ProcessingInstruction) currentEvent;
    return pi.getData();
  }

  /**
   * This class is used to construct the event series from SAX callbacks.
   */
  class CallbackHandler
    implements ContentHandler, DTDHandler, LexicalHandler,
               DeclHandler, EntityResolver, ErrorHandler
  {

    XMLReader reader;
    Locator locator;
    Location location;
    private boolean inCDATA;
    private LinkedList namespaces = new LinkedList();
    private LinkedList notations;
    private LinkedList entities;

    CallbackHandler(XMLReader reader)
    {
      this.reader = reader;
    }

    public void setDocumentLocator(Locator locator)
    {
      this.locator = locator;
      location = new LocationImpl(-1,
                                  locator.getColumnNumber(),
                                  locator.getLineNumber(),
                                  locator.getSystemId());
    }

    public void startDocument()
      throws SAXException
    {
      String version = (locator instanceof Locator2) ?
        ((Locator2) locator).getXMLVersion() : null;
      String encoding = (locator instanceof Locator2) ? 
        ((Locator2) locator).getEncoding() : null;
      boolean standalone =
        reader.getFeature("http://xml.org/sax/features/is-standalone");
      boolean standaloneDeclared = standalone;
      boolean encodingDeclared = (encoding != null);
      events.add(new StartDocumentImpl(location,
                                       location.getLocationURI(),
                                       encoding,
                                       version,
                                       standalone,
                                       standaloneDeclared,
                                       encodingDeclared));
    }

    public void endDocument()
      throws SAXException
    {
      events.add(new EndDocumentImpl(location));
    }
    
    public void startPrefixMapping(String prefix, String uri)
      throws SAXException
    {
      namespaces.add(new NamespaceImpl(location, prefix, uri));
    }

    public void endPrefixMapping(String prefix)
      throws SAXException
    {
    }

    public void startElement(String namespaceURI, String localName,
                             String qName, Attributes atts)
      throws SAXException
    {
      LinkedList ns = namespaces;
      namespaces = new LinkedList();
      int ci = qName.indexOf(':');
      String prefix = null;
      localName = qName;
      if (ci != -1)
        {
          prefix = qName.substring(0, ci);
          localName = qName.substring(ci + 1);
        }
      QName name = new QName(namespaceURI, localName, prefix);
      LinkedList attrs = new LinkedList();
      StartElementImpl se = new StartElementImpl(location, name,
                                                 attrs, ns, null);
      events.add(se);
      // Add namespaces
      //for (Iterator i = ns.iterator(); i.hasNext(); )
      //  events.add(i.next());
      // Add attributes
      int len = atts.getLength();
      for (int i = 0; i < len; i++)
        {
          String attURI = atts.getURI(i);
          String attQName = atts.getQName(i);
          String value = atts.getValue(i);
          QName type = QName.valueOf(atts.getType(i));
          boolean specified = (atts instanceof Attributes2) &&
            ((Attributes2) atts).isSpecified(i);
          ci = attQName.indexOf(':');
          String attPrefix = null;
          String attLocalName = attQName;
          if (ci != -1)
            {
              attPrefix = attQName.substring(0, ci);
              attLocalName = attQName.substring(ci + 1);
            }
          if ("xmlns".equals(attPrefix) || "xmlns".equals(attQName))
            continue;
          QName attrName = new QName(attURI, attLocalName, attPrefix);
          AttributeImpl attr = new AttributeImpl(location, attrName,
                                                 value, type, specified);
          attrs.add(attr);
          //events.add(attr);
        }
    }

    public void endElement(String namespaceURI, String localName,
                           String qName)
      throws SAXException
    {
      int ci = qName.indexOf(':');
      String prefix = null;
      localName = qName;
      if (ci != -1)
        {
          prefix = qName.substring(0, ci);
          localName = qName.substring(ci + 1);
        }
      QName name = new QName(namespaceURI, localName, prefix);
      events.add(new EndElementImpl(location, name, new LinkedList()));
      // TODO namespaces out of scope
    }

    public void characters(char[] ch, int start, int length)
      throws SAXException
    {
      boolean whitespace = isWhitespace(ch, start, length);
      events.add(new CharactersImpl(location, new String(ch, start, length),
                                    whitespace, inCDATA, false));
    }
    
    public void ignorableWhitespace(char[] ch, int start, int length)
      throws SAXException
    {
      boolean whitespace = isWhitespace(ch, start, length);
      events.add(new CharactersImpl(location, new String(ch, start, length),
                                    whitespace, inCDATA, true));
    }

    boolean isWhitespace(char[] ch, int start, int len)
    {
      int end = start + len;
      for (int i = start; i < end; i++)
        {
          char c = ch[i];
          if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
            return false;
        }
      return true;
    }

    public void processingInstruction(String target, String data)
      throws SAXException
    {
      events.add(new ProcessingInstructionImpl(location, target, data));
    }

    public void skippedEntity(String name)
      throws SAXException
    {
    }

    public void startDTD(String name, String publicId, String systemId)
      throws SAXException
    {
      notations = new LinkedList();
      entities = new LinkedList();
      events.add(new DTDImpl(location, null, null, notations, entities));
    }

    public void endDTD()
      throws SAXException
    {
    }

    public void startEntity(String name)
      throws SAXException
    {
      events.add(new StartEntityImpl(location, name));
    }

    public void endEntity(String name)
      throws SAXException
    {
      events.add(new EndEntityImpl(location, name));
    }

    public void startCDATA()
      throws SAXException
    {
      inCDATA = true;
    }

    public void endCDATA()
      throws SAXException
    {
      inCDATA = false;
    }

    public void comment(char[] ch, int start, int length)
      throws SAXException
    {
      events.add(new CommentImpl(location, new String(ch, start, length)));
    }

    public void notationDecl(String name, String publicId, String systemId)
      throws SAXException
    {
      Object n = new NotationDeclarationImpl(location, name, publicId,
                                             systemId);
      notations.add(n);
      //events.add(n);
    }

    public void unparsedEntityDecl(String name, String publicId,
                                   String systemId, String notationName)
      throws SAXException
    {
      Object e = new EntityDeclarationImpl(location, publicId, systemId,
                                           name, notationName,
                                           null, null);
      entities.add(e);
      //events.add(e);
    }

    public void elementDecl(String name, String model)
      throws SAXException
    {
    }

    public void attributeDecl(String eName, String aName, String type,
                              String valueDefault, String value)
      throws SAXException
    {
    }

    public void internalEntityDecl(String name, String value)
      throws SAXException
    {
      Object e = new EntityDeclarationImpl(location, null, null,
                                           name, null, value, null);
      entities.add(e);
      //events.add(e);
    }

    public void externalEntityDecl(String name, String publicId,
                                   String systemId)
      throws SAXException
    {
      Object e = new EntityDeclarationImpl(location, publicId, systemId,
                                           name, null, null, null);
      entities.add(e);
      //events.add(e);
    }

    public void warning(SAXParseException e)
      throws SAXException
    {
      if (reporter != null)
        {
          try
            {
              reporter.report(e.getMessage(), "warning", e, location);
            }
          catch (XMLStreamException e2)
            {
              SAXException e3 = new SAXException(e2.getMessage());
              e3.initCause(e2);
              throw e3;
            }
        }
    }
    
    public void error(SAXParseException e)
      throws SAXException
    {
      if (reporter != null)
        {
          try
            {
              reporter.report(e.getMessage(), "error", e, location);
            }
          catch (XMLStreamException e2)
            {
              SAXException e3 = new SAXException(e2.getMessage());
              e3.initCause(e2);
              throw e3;
            }
        }
    }
    
    public void fatalError(SAXParseException e)
      throws SAXException
    {
      if (reporter != null)
        {
          try
            {
              reporter.report(e.getMessage(), "fatal-error", e, location);
            }
          catch (XMLStreamException e2)
            {
              SAXException e3 = new SAXException(e2.getMessage());
              e3.initCause(e2);
              throw e3;
            }
        }
    }

    public InputSource resolveEntity(String publicId, String systemId)
      throws SAXException, IOException
    {
      if (resolver != null)
        {
          try
            {
              InputStream in = resolver.resolve(systemId);
              if (in != null)
                {
                  InputSource ret = new InputSource(in);
                  ret.setPublicId(publicId);
                  ret.setSystemId(systemId);
                  return ret;
                }
            }
          catch (XMLStreamException e)
            {
              SAXException e2 = new SAXException(e.getMessage());
              e2.initCause(e);
              throw e2;
            }
        }
      return null;
    }
    
  }
  
}

