/* SAXParser.java -- 
   Copyright (C) 2005, 2006, 2007  Free Software Foundation, Inc.

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
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLReporter;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.Attributes2;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.EntityResolver2;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.ext.Locator2;

/**
 * JAXP SAX parser using an underlying StAX parser.
 * This parser supports the following additional SAX features and
 * properties:
 * <table>
 * <tr><th colspan='4'>Features</th></tr>
 * <tr><td>http://gnu.org/sax/features/xml-base</td>
 * <td colspan='2'>read/write</td>
 * <td>Indicates or sets whether XML Base processing is enabled</td></tr>
 * <tr><th colspan='4'>Properties</th></tr>
 * <tr><td>http://gnu.org/sax/properties/base-uri</td>
 * <td>read-only</td><td>String</td>
 * <td>Returns the base URI of the current event</td></tr>
 * <tr><td>http://gnu.org/sax/properties/document-xml-encoding</td>
 * <td>read-only</td><td>String</td>
 * <td>Returns the encoding specified in the XML declaration</td></tr>
 * </table>
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class SAXParser
  extends javax.xml.parsers.SAXParser
  implements XMLReader, Attributes2, Locator2, XMLReporter, XMLResolver
{

  ContentHandler contentHandler;
  DeclHandler declHandler;
  DTDHandler dtdHandler;
  EntityResolver entityResolver;
  ErrorHandler errorHandler;
  LexicalHandler lexicalHandler;

  boolean validating = false;
  boolean namespaceAware = true;
  boolean xIncludeAware = false;
  boolean stringInterning = true;
  boolean coalescing = true;
  boolean replaceERefs = true;
  boolean externalEntities = true;
  boolean supportDTD = true;
  boolean baseAware = true;

  XMLParser parser;
  XMLStreamReader reader;
  String encoding;
  String xmlVersion;
  boolean xmlStandalone;
  String xmlEncoding;
  String baseURI;

  public SAXParser()
  {
  }

  SAXParser(boolean validating, boolean namespaceAware, boolean xIncludeAware)
  {
    this.validating = validating;
    this.namespaceAware = namespaceAware;
    this.xIncludeAware = xIncludeAware;
  }

  // -- SAXParser --
  
  public Parser getParser()
    throws SAXException
  {
    return null;
  }
  
  public XMLReader getXMLReader()
    throws SAXException
  {
    return this;
  }

  public boolean isNamespaceAware()
  {
    return namespaceAware;
  }

  public boolean isValidating()
  {
    return validating;
  }

  public void setProperty(String name, Object value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    if (parser != null)
      throw new IllegalStateException("parsing in progress");
    final String FEATURES = "http://xml.org/sax/features/";
    final String PROPERTIES = "http://xml.org/sax/properties/";
    final String GNU_FEATURES = "http://gnu.org/sax/features/";
    if ((FEATURES + "namespaces").equals(name))
      namespaceAware = Boolean.TRUE.equals(value);
    else if ((FEATURES + "namespace-prefixes").equals(name))
      {
        // NOOP
      }
    else if ((FEATURES + "string-interning").equals(name))
      stringInterning = Boolean.TRUE.equals(value);
    else if ((FEATURES + "use-attributes2").equals(name))
      {
        // NOOP
      }
    else if ((FEATURES + "validation").equals(name))
      validating = Boolean.TRUE.equals(value);
    else if ((FEATURES + "external-general-entities").equals(name))
      externalEntities = Boolean.TRUE.equals(value);
    else if ((FEATURES + "external-parameter-entities").equals(name))
      externalEntities = Boolean.TRUE.equals(value);
    else if ((PROPERTIES + "declaration-handler").equals(name))
      declHandler = (DeclHandler) value;
    else if ((PROPERTIES + "lexical-handler").equals(name))
      lexicalHandler = (LexicalHandler) value;
    else if ((GNU_FEATURES + "xml-base").equals(name))
      baseAware = Boolean.TRUE.equals(value);
    else if ((GNU_FEATURES + "coalescing").equals(name))
      coalescing = Boolean.TRUE.equals(value);
    else
      throw new SAXNotSupportedException(name);
  }

  public Object getProperty(String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    final String FEATURES = "http://xml.org/sax/features/";
    final String PROPERTIES = "http://xml.org/sax/properties/";
    final String GNU_FEATURES = "http://gnu.org/sax/features/";
    final String GNU_PROPERTIES = "http://gnu.org/sax/properties/";
    if ((GNU_FEATURES + "base-uri").equals(name))
      return baseURI;
    if ((FEATURES + "is-standalone").equals(name))
      return xmlStandalone ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "namespaces").equals(name))
      return namespaceAware ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "namespace-prefixes").equals(name))
      return Boolean.TRUE;
    if ((FEATURES + "string-interning").equals(name))
      return stringInterning ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "use-attributes2").equals(name))
      return Boolean.TRUE;
    if ((FEATURES + "use-locator2").equals(name))
      return Boolean.TRUE;
    if ((FEATURES + "use-entity-resolver2").equals(name))
      return Boolean.FALSE;
    if ((FEATURES + "validation").equals(name))
      return validating ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "external-general-entities").equals(name))
      return externalEntities ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "external-parameter-entities").equals(name))
      return externalEntities ? Boolean.TRUE : Boolean.FALSE;
    if ((FEATURES + "xml-1.1").equals(name))
      return Boolean.TRUE;
    if ((PROPERTIES + "declaration-handler").equals(name))
      return declHandler;
    if ((PROPERTIES + "document-xml-version").equals(name))
      return xmlVersion;
    if ((PROPERTIES + "lexical-handler").equals(name))
      return lexicalHandler;
    if ((GNU_FEATURES + "xml-base").equals(name))
      return baseAware ? Boolean.TRUE : Boolean.FALSE;
    if ((GNU_PROPERTIES + "document-xml-encoding").equals(name))
      return xmlEncoding;
    throw new SAXNotSupportedException(name);
  }

  public boolean isXIncludeAware()
  {
    return xIncludeAware;
  }

  public void reset()
  {
    parser = null;
    encoding = null;
    xmlVersion = null;
    xmlStandalone = false;
  }

  // -- XMLReader --

  public boolean getFeature(String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    Object ret = getProperty(name);
    if (ret instanceof Boolean)
      return ((Boolean) ret).booleanValue();
    throw new SAXNotSupportedException(name);
  }

  public void setFeature(String name, boolean value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    setProperty(name, value ? Boolean.TRUE : Boolean.FALSE);
  }

  public void setEntityResolver(EntityResolver resolver)
  {
    entityResolver = resolver;
  }

  public EntityResolver getEntityResolver()
  {
    return entityResolver;
  }

  public void setDTDHandler(DTDHandler handler)
  {
    dtdHandler = handler;
  }

  public DTDHandler getDTDHandler()
  {
    return dtdHandler;
  }

  public void setContentHandler(ContentHandler handler)
  {
    contentHandler = handler;
  }

  public ContentHandler getContentHandler()
  {
    return contentHandler;
  }

  public void setErrorHandler(ErrorHandler handler)
  {
    errorHandler = handler;
  }

  public ErrorHandler getErrorHandler()
  {
    return errorHandler;
  }

  public synchronized void parse(InputSource input)
    throws IOException, SAXException
  {
    reset();
    String systemId = input.getSystemId();
    InputStream in = input.getByteStream();
    boolean opened = false;
    if (in != null)
      parser = new XMLParser(in, systemId,
                             validating,
                             namespaceAware,
                             coalescing,
                             replaceERefs,
                             externalEntities,
                             supportDTD,
                             baseAware,
                             stringInterning,
                             true,
                             this,
                             this);
    else
      {
        Reader r = input.getCharacterStream();
        if (r != null)
          parser = new XMLParser(r, systemId,
                                 validating,
                                 namespaceAware,
                                 coalescing,
                                 replaceERefs,
                                 externalEntities,
                                 supportDTD,
                                 baseAware,
                                 stringInterning,
                                 true,
                                 this,
                                 this);
      }
    if (parser == null)
      {
        if (systemId == null)
          throw new SAXException("No stream or system ID specified");
        systemId = XMLParser.absolutize(null, systemId);
        in = new URL(systemId).openStream();
        opened = true;
        parser = new XMLParser(in, systemId,
                               validating,
                               namespaceAware,
                               coalescing,
                               replaceERefs,
                               externalEntities,
                               supportDTD,
                               baseAware,
                               stringInterning,
                               true,
                               this,
                               this);
      }
    reader = parser;
    baseURI = systemId;
    
    if (xIncludeAware)
      reader = new XIncludeFilter(parser, systemId, namespaceAware,
                                  validating, true);
    
    if (contentHandler != null)
      contentHandler.setDocumentLocator(this);
    boolean startDocumentDone = false;
    try
      {
        while (parser.hasNext())
          {
            int event = parser.next();
            if (baseAware)
              baseURI = parser.getXMLBase();
            switch (event)
              {
              case XMLStreamConstants.CHARACTERS:
                if (contentHandler != null)
                  {
                    char[] b = reader.getTextCharacters();
                    contentHandler.characters(b, 0, b.length);
                  }
                break;
              case XMLStreamConstants.SPACE:
                if (contentHandler != null)
                  {
                    char[] b = reader.getTextCharacters();
                    if (isIgnorableWhitespace(parser, b, false))
                      contentHandler.ignorableWhitespace(b, 0, b.length);
                    else
                      contentHandler.characters(b, 0, b.length);
                  }
                break;
              case XMLStreamConstants.CDATA:
                if (lexicalHandler != null)
                  lexicalHandler.startCDATA();
                if (contentHandler != null)
                  {
                    char[] b = reader.getTextCharacters();
                    if (isIgnorableWhitespace(parser, b, true))
                      contentHandler.ignorableWhitespace(b, 0, b.length);
                    else
                      contentHandler.characters(b, 0, b.length);
                  }
                if (lexicalHandler != null)
                  lexicalHandler.endCDATA();
                break;
              case XMLStreamConstants.START_ELEMENT:
                if (contentHandler != null)
                  {
                    QName name = reader.getName();
                    String uri = name.getNamespaceURI();
                    String localName = name.getLocalPart();
                    String prefix = name.getPrefix();
                    String qName = localName;
                    if (!"".equals(prefix))
                      qName = prefix + ":" + localName;
                    if (!namespaceAware)
                      {
                        uri = "";
                        localName = "";
                      }
                    else
                      {
                        int nc = reader.getNamespaceCount();
                        for (int i = 0; i < nc; i++)
                          {
                            String nsuri = reader.getNamespaceURI(i);
                            String nsprefix = reader.getNamespacePrefix(i);
                            if ("xml".equals(nsprefix))
                              continue;
                            contentHandler.startPrefixMapping(nsprefix, nsuri);
                          }
                      }
                    contentHandler.startElement(uri, localName, qName, this);
                  }
                break;
              case XMLStreamConstants.END_ELEMENT:
                if (contentHandler != null)
                  {
                    QName name = reader.getName();
                    String uri = name.getNamespaceURI();
                    String localName = name.getLocalPart();
                    String prefix = name.getPrefix();
                    String qName = localName;
                    if (!"".equals(prefix))
                      qName = prefix + ":" + localName;
                    if (!namespaceAware)
                      {
                        uri = "";
                        localName = "";
                      }
                    contentHandler.endElement(uri, localName, qName);
                    if (namespaceAware)
                      {
                        int nc = reader.getNamespaceCount();
                        for (int i = 0; i < nc; i++)
                          {
                            String nsprefix = reader.getNamespacePrefix(i);
                            if ("xml".equals(nsprefix))
                              continue;
                            contentHandler.endPrefixMapping(nsprefix);
                          }
                      }
                  }
                break;
              case XMLStreamConstants.COMMENT:
                if (lexicalHandler != null)
                  {
                    char[] b = reader.getTextCharacters();
                    lexicalHandler.comment(b, 0, b.length);
                  }
                break;
              case XMLStreamConstants.PROCESSING_INSTRUCTION:
                if (contentHandler != null)
                  {
                    String target = reader.getPITarget();
                    String data = reader.getPIData();
                    if (data == null)
                      data = "";
                    contentHandler.processingInstruction(target, data);
                  }
                break;
              case XMLParser.START_ENTITY:
                if (lexicalHandler != null)
                  {
                    String name = reader.getText();
                    lexicalHandler.startEntity(name);
                  }
                break;
              case XMLParser.END_ENTITY:
                if (lexicalHandler != null)
                  {
                    String name = reader.getText();
                    lexicalHandler.endEntity(name);
                  }
                break;
              case XMLStreamConstants.START_DOCUMENT:
                encoding = reader.getEncoding();
                xmlVersion = reader.getVersion();
                xmlStandalone = reader.isStandalone();
                xmlEncoding = reader.getCharacterEncodingScheme();
                if (contentHandler != null)
                  contentHandler.startDocument();
                startDocumentDone = true;
                break;
              case XMLStreamConstants.END_DOCUMENT:
                if (contentHandler != null)
                  contentHandler.endDocument();
                break;
              case XMLStreamConstants.DTD:
                XMLParser.Doctype doctype = parser.doctype;
                if (lexicalHandler != null)
                  {
                    String rootName = doctype.rootName;
                    String publicId = doctype.publicId;
                    String systemId2 = doctype.systemId;
                    lexicalHandler.startDTD(rootName, publicId, systemId2);
                  }
                for (Iterator i = doctype.entryIterator(); i.hasNext(); )
                  {
                    String entry = (String) i.next();
                    char c = entry.charAt(0);
                    String name = entry.substring(1);
                    if ('E' == c)
                      {
                        // Element decl
                        if (declHandler != null)
                          {
                            XMLParser.ContentModel model =
                              doctype.getElementModel(name);
                            declHandler.elementDecl(name, model.text);
                          }
                      }
                    else if ('A' == c)
                      {
                        // Attlist decl
                        if (declHandler != null)
                          {
                            for (Iterator j = doctype.attlistIterator(name);
                                 j.hasNext(); )
                              {
                                Map.Entry att = (Map.Entry) j.next();
                                String aname = (String) att.getKey();
                                XMLParser.AttributeDecl decl =
                                  (XMLParser.AttributeDecl) att.getValue();
                                String type = decl.type;
                                String value = decl.value;
                                String mode = null;
                                switch (decl.valueType)
                                  {
                                  case XMLParser.ATTRIBUTE_DEFAULT_FIXED:
                                    mode = "#FIXED";
                                    break;
                                  case XMLParser.ATTRIBUTE_DEFAULT_REQUIRED:
                                    mode = "#REQUIRED";
                                    break;
                                  case XMLParser.ATTRIBUTE_DEFAULT_IMPLIED:
                                    mode = "#IMPLIED";
                                    break;
                                  }
                                declHandler.attributeDecl(name, aname,
                                                          type, mode, value);
                              }
                          }
                      }
                    else if ('e' == c)
                      {
                        // Entity decl
                        Object entity = doctype.getEntity(name);
                        if (entity instanceof String)
                          {
                            if (declHandler != null)
                              declHandler.internalEntityDecl(name,
                                                             (String) entity);
                          }
                        else
                          {
                            XMLParser.ExternalIds ids =
                              (XMLParser.ExternalIds) entity;
                            if (ids.notationName != null)
                              {
                                if (dtdHandler != null)
                                  {
                                    String pub = ids.publicId;
                                    String url = ids.systemId;
                                    String not = ids.notationName;
                                    dtdHandler.unparsedEntityDecl(name,
                                                                  pub,
                                                                  url,
                                                                  not);
                                  }
                              }
                            else
                              {
                                if (declHandler != null)
                                  {
                                    String pub = ids.publicId;
                                    String url = ids.systemId;
                                    declHandler.externalEntityDecl(name,
                                                                   pub,
                                                                   url);
                                  }
                              }
                          }
                      }
                    else if ('n' == c)
                      {
                        // Notation decl
                        if (dtdHandler != null)
                          {
                            XMLParser.ExternalIds ids =
                              doctype.getNotation(name);
                            String pub = ids.publicId;
                            String url = ids.systemId;
                            dtdHandler.notationDecl(name, pub, url);
                          }
                      }
                    else if ('c' == c)
                      {
                        // Comment
                        if (lexicalHandler != null)
                          {
                            String comment = doctype.getComment(name);
                            char[] b = comment.toCharArray();
                            lexicalHandler.comment(b, 0, b.length);
                          }
                      }
                    else if ('p' == c)
                      {
                        // Processing instruction
                        if (contentHandler != null)
                          {
                            String[] pi = doctype.getPI(name);
                            String target = pi[0];
                            String data = pi[1];
                            if (data == null)
                              data = "";
                            contentHandler.processingInstruction(target, data);
                          }
                      }
                  }
                if (lexicalHandler != null)
                  lexicalHandler.endDTD();
              }
          }
        reset();
        if (opened)
          in.close();
      }
    catch (Exception e)
      {
        SAXParseException e2 = new SAXParseException(e.getMessage(), this);
        e2.initCause(e);
        try
          {
            if (!startDocumentDone && contentHandler != null)
              contentHandler.startDocument();
            if (errorHandler != null)
              errorHandler.fatalError(e2);
            if (contentHandler != null)
              contentHandler.endDocument();
          }
        catch (SAXException sex)
          {
            // Ignored, we will rethrow the original exception.
          }
        reset();
        if (opened)
          in.close();
        if (e instanceof SAXException)
          throw (SAXException) e;
        if (e instanceof IOException)
          throw (IOException) e;
        else
          throw e2;
      }
  }

  /**
   * Indicates whether the specified characters are ignorable whitespace.
   */
  private boolean isIgnorableWhitespace(XMLParser reader, char[] b,
                                        boolean testCharacters)
    throws XMLStreamException
  {
    XMLParser.Doctype doctype = reader.doctype;
    if (doctype == null)
      return false;
    String currentElement = reader.getCurrentElement();
    // check for xml:space
    int ac = reader.getAttributeCount();
    for (int i = 0; i < ac; i++)
      {
        QName aname = reader.getAttributeName(i);
        if ("space".equals(aname.getLocalPart()) &&
            XMLConstants.XML_NS_URI.equals(aname.getNamespaceURI()))
          {
            if ("preserve".equals(reader.getAttributeValue(i)))
              return false;
          }
      }
    XMLParser.ContentModel model = doctype.getElementModel(currentElement);
    if (model == null || model.type != XMLParser.ContentModel.ELEMENT)
      return false;
    if (model.external && xmlStandalone)
      return false;
    boolean white = true;
    if (testCharacters)
      {
        for (int i = 0; i < b.length; i++)
          {
            if (b[i] != ' ' && b[i] != '\t' && b[i] != '\n' && b[i] != '\r')
              {
                white = false;
                break;
              }
          }
      }
    return white;
  }

  public void parse(String systemId)
    throws IOException, SAXException
  {
    parse(new InputSource(systemId));
  }

  // -- Attributes2 --

  public int getIndex(String qName)
  {
    int len = reader.getAttributeCount();
    for (int i = 0; i < len; i++)
      {
        QName q = reader.getAttributeName(i);
        String localName = q.getLocalPart();
        String prefix = q.getPrefix();
        String qn = ("".equals(prefix)) ? localName : prefix + ":" + localName;
        if (qName.equals(qn))
          return i;
      }
    return -1;
  }

  public int getIndex(String uri, String localName)
  {
    int len = reader.getAttributeCount();
    for (int i = 0; i < len; i++)
      {
        QName q = reader.getAttributeName(i);
        String ln = q.getLocalPart();
        String u = q.getNamespaceURI();
        if (u == null && uri != null)
          continue;
        if (u != null && !u.equals(uri))
          continue;
        if (ln.equals(localName))
          return i;
      }
    return -1;
  }

  public int getLength()
  {
    return reader.getAttributeCount();
  }

  public String getLocalName(int index)
  {
    return reader.getAttributeLocalName(index);
  }

  public String getQName(int index)
  {
    QName q = reader.getAttributeName(index);
    String localName = q.getLocalPart();
    String prefix = q.getPrefix();
    return ("".equals(prefix)) ? localName : prefix + ":" + localName;
  }

  public String getType(int index)
  {
    String ret = reader.getAttributeType(index);
    // SAX doesn't permit ENUMERATION?
    return ("ENUMERATION".equals(ret)) ? "NMTOKEN" : ret;
  }

  public String getType(String qName)
  {
    int index = getIndex(qName);
    return (index == -1) ? null : getType(index);
  }

  public String getType(String uri, String localName)
  {
    int index = getIndex(uri, localName);
    return (index == -1) ? null : getType(index);
  }

  public String getURI(int index)
  {
    String ret = reader.getAttributeNamespace(index);
    return (ret == null) ? "" : ret;
  }

  public String getValue(int index)
  {
    return reader.getAttributeValue(index);
  }

  public String getValue(String qName)
  {
    int index = getIndex(qName);
    return (index == -1) ? null : getValue(index);
  }

  public String getValue(String uri, String localName)
  {
    int index = getIndex(uri, localName);
    return (index == -1) ? null : getValue(index);
  }

  public boolean isDeclared(int index)
  {
    return parser.isAttributeDeclared(index);
  }

  public boolean isDeclared(String qName)
  {
    int index = getIndex(qName);
    return (index == -1) ? false : isDeclared(index);
  }

  public boolean isDeclared(String uri, String localName)
  {
    int index = getIndex(uri, localName);
    return (index == -1) ? false : isDeclared(index);
  }

  public boolean isSpecified(int index)
  {
    return reader.isAttributeSpecified(index);
  }

  public boolean isSpecified(String qName)
  {
    int index = getIndex(qName);
    return (index == -1) ? false : isSpecified(index);
  }

  public boolean isSpecified(String uri, String localName)
  {
    int index = getIndex(uri, localName);
    return (index == -1) ? false : isSpecified(index);
  }

  // -- Locator2 --
  
  public int getColumnNumber()
  {
    Location l = reader.getLocation();
    return l.getColumnNumber();
  }

  public int getLineNumber()
  {
    Location l = reader.getLocation();
    return l.getLineNumber();
  }

  public String getPublicId()
  {
    Location l = reader.getLocation();
    return l.getPublicId();
  }

  public String getSystemId()
  {
    Location l = reader.getLocation();
    return l.getSystemId();
  }
  
  public String getEncoding()
  {
    return encoding;
  }

  public String getXMLVersion()
  {
    return xmlVersion;
  }

  // -- XMLResolver --
  
  public Object resolveEntity(String publicId, String systemId,
                              String baseURI, String namespace)
    throws XMLStreamException
  {
    if (entityResolver != null)
      {
        try
          {
            InputSource input =
              entityResolver.resolveEntity(publicId, systemId);
            if (input != null)
              {
                InputStream in = input.getByteStream();
                if (in == null)
                  {
                    String newSystemId = input.getSystemId();
                    if (newSystemId != null && !newSystemId.equals(systemId))
                      in = XMLParser.resolve(newSystemId);
                  }
                return in;
              }
          }
        catch (SAXException e)
          {
            XMLStreamException e2 = new XMLStreamException(e.getMessage());
            e2.initCause(e);
            throw e2;
          }
        catch (IOException e)
          {
            XMLStreamException e2 = new XMLStreamException(e.getMessage());
            e2.initCause(e);
            throw e2;
          }
      }
    return null;
  }

  public XMLEventReader resolveAsXMLEventReader(String uri)
    throws XMLStreamException
  {
    // unused
    return null;
  }

  public XMLStreamReader resolveAsXMLStreamReader(String uri)
    throws XMLStreamException
  {
    // unused
    return null;
  }

  // -- XMLReporter --

  public void report(String message, String errorType,
                     Object relatedInformation, Location location)
    throws XMLStreamException
  {
    if (errorHandler != null)
      {
        try
          {
            errorHandler.warning(new SAXParseException(message, this));
          }
        catch (SAXException e)
          {
            XMLStreamException e2 = new XMLStreamException(e.getMessage());
            e2.initCause(e);
            throw e2;
          }
      }
  }

  public static void main(String[] args)
    throws Exception
  {
    boolean validating = false;
    boolean namespaceAware = false;
    boolean xIncludeAware = false;
    boolean expectCallbackClass = false;
    String callbackClass = null;
    int pos = 0;
    while (pos < args.length && (args[pos].startsWith("-") || expectCallbackClass))
      {
        if ("-x".equals(args[pos]))
          xIncludeAware = true;
        else if ("-v".equals(args[pos]))
          validating = true;
        else if ("-n".equals(args[pos]))
          namespaceAware = true;
        else if ("-c".equals(args[pos]))
          expectCallbackClass = true;
        else if (expectCallbackClass)
          {
            callbackClass = args[pos];
            expectCallbackClass = false;
          }
        pos++;
      }
    if (pos >= args.length || expectCallbackClass)
      {
        System.out.println("Syntax: SAXParser [-n] [-v] [-x] [-c <class>] <file> [<file2> [...]]");
        System.out.println("\t-n: use namespace aware mode");
        System.out.println("\t-v: use validating parser");
        System.out.println("\t-x: use XInclude aware mode");
        System.out.println("\t-c <class>: use specified class as callback handler (must have a no-arg public constructor)");
        System.exit(2);
      }
    while (pos < args.length)
      {
        ContentHandler handler = null;
        if (callbackClass != null)
          {
            Class t = Class.forName(callbackClass);
            handler = (ContentHandler) t.newInstance();
          }
        else
          handler = new org.xml.sax.helpers.DefaultHandler();
        SAXParser parser = new SAXParser(validating, namespaceAware,
                                         xIncludeAware);
        InputSource input = new InputSource(args[pos]);
        java.io.FileReader fr = new java.io.FileReader(args[pos]);
        input.setCharacterStream(fr);
        try
          {
            XMLReader reader = parser.getXMLReader();
            reader.setContentHandler(handler);
            reader.parse(input);
          }
        finally
          {
            fr.close();
          }
        pos++;
      }
  }
  
}
