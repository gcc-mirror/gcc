/* XMLParser.java -- 
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
exception statement from your version.

Partly derived from code which carried the following notice:

  Copyright (c) 1997, 1998 by Microstar Software Ltd.

  AElfred is free for both commercial and non-commercial use and
  redistribution, provided that Microstar's copyright and disclaimer are
  retained intact.  You are free to modify AElfred for your own use and
  to redistribute AElfred with your modifications, provided that the
  modifications are clearly documented.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  merchantability or fitness for a particular purpose.  Please use it AT
  YOUR OWN RISK.
*/

package gnu.xml.stream;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLReporter;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import gnu.java.net.CRLFInputStream;
import gnu.classpath.debug.TeeInputStream;
import gnu.classpath.debug.TeeReader;

/**
 * An XML parser.
 * This parser supports the following additional StAX properties:
 * <table>
 * <tr><td>gnu.xml.stream.stringInterning</td>
 * <td>Boolean</td>
 * <td>Indicates whether markup strings will be interned</td></tr>
 * <tr><td>gnu.xml.stream.xmlBase</td>
 * <td>Boolean</td>
 * <td>Indicates whether XML Base processing will be performed</td></tr>
 * <tr><td>gnu.xml.stream.baseURI</td>
 * <td>String</td>
 * <td>Returns the base URI of the current event</td></tr>
 * </table>
 *
 * @see http://www.w3.org/TR/REC-xml/
 * @see http://www.w3.org/TR/xml11/
 * @see http://www.w3.org/TR/REC-xml-names
 * @see http://www.w3.org/TR/xml-names11
 * @see http://www.w3.org/TR/xmlbase/
 * 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLParser
  implements XMLStreamReader, NamespaceContext
{

  // -- parser state machine states --
  private static final int INIT = 0; // start state
  private static final int PROLOG = 1; // in prolog
  private static final int CONTENT = 2; // in content
  private static final int EMPTY_ELEMENT = 3; // empty element state
  private static final int MISC = 4; // in Misc (after root element)

  // -- parameters for parsing literals --
  private final static int LIT_ENTITY_REF = 2;
  private final static int LIT_NORMALIZE = 4;
  private final static int LIT_ATTRIBUTE = 8;
  private final static int LIT_DISABLE_PE = 16;
  private final static int LIT_DISABLE_CREF = 32;
  private final static int LIT_DISABLE_EREF = 64;
  private final static int LIT_PUBID = 256;

  // -- types of attribute values --
  final static int ATTRIBUTE_DEFAULT_UNDECLARED = 30;
  final static int ATTRIBUTE_DEFAULT_SPECIFIED = 31;
  final static int ATTRIBUTE_DEFAULT_IMPLIED = 32;
  final static int ATTRIBUTE_DEFAULT_REQUIRED = 33;
  final static int ATTRIBUTE_DEFAULT_FIXED = 34;

  // -- additional event types --
  final static int START_ENTITY = 50;
  final static int END_ENTITY = 51;

  /**
   * The current input.
   */
  private Input input;

  /**
   * Stack of inputs representing XML general entities.
   * The input representing the XML input stream or reader is always the
   * first element in this stack.
   */
  private LinkedList inputStack = new LinkedList();

  /**
   * Stack of start-entity events to be reported.
   */
  private LinkedList startEntityStack = new LinkedList();

  /**
   * Stack of end-entity events to be reported.
   */
  private LinkedList endEntityStack = new LinkedList();
  
  /**
   * Current parser state within the main state machine.
   */
  private int state = INIT;

  /**
   * The (type of the) current event.
   */
  private int event;

  /**
   * Whether we are looking ahead. Used by hasNext.
   */
  private boolean lookahead;

  /**
   * The element name stack. The first element in this stack will be the
   * root element.
   */
  private LinkedList stack = new LinkedList();

  /**
   * Stack of namespace contexts. These are maps specifying prefix-to-URI
   * mappings. The first element in this stack is the most recent namespace
   * context (i.e. the other way around from the element name stack).
   */
  private LinkedList namespaces = new LinkedList();

  /**
   * The base-URI stack. This holds the base URI context for each element.
   * The first element in this stack is the most recent context (i.e. the
   * other way around from the element name stack).
   */
  private LinkedList bases = new LinkedList();

  /**
   * The list of attributes for the current element, in the order defined in
   * the XML stream.
   */
  private ArrayList attrs = new ArrayList();

  /**
   * Buffer for text and character data.
   */
  private StringBuffer buf = new StringBuffer();

  /**
   * Buffer for NMTOKEN strings (markup).
   */
  private StringBuffer nmtokenBuf = new StringBuffer();

  /**
   * Buffer for string literals. (e.g. attribute values)
   */
  private StringBuffer literalBuf = new StringBuffer();

  /**
   * Temporary Unicode character buffer used during character data reads.
   */
  private int[] tmpBuf = new int[1024];
  
  /**
   * The element content model for the current element.
   */
  private ContentModel currentContentModel;

  /**
   * The validation stack. This holds lists of the elements seen for each
   * element, in order to determine whether the names and order of these
   * elements match the content model for the element. The last entry in
   * this stack represents the current element.
   */
  private LinkedList validationStack;

  /**
   * These sets contain the IDs and the IDREFs seen in the document, to
   * ensure that IDs are unique and that each IDREF refers to an ID in the
   * document.
   */
  private HashSet ids, idrefs;

  /**
   * The target and data associated with the current processing instruction
   * event.
   */
  private String piTarget, piData;

  /**
   * The XML version declared in the XML declaration.
   */
  private String xmlVersion;

  /**
   * The encoding declared in the XML declaration.
   */
  private String xmlEncoding;

  /**
   * The standalone value declared in the XML declaration.
   */
  private Boolean xmlStandalone;

  /**
   * The document type definition.
   */
  Doctype doctype;

  /**
   * State variables for determining parameter-entity expansion.
   */
  private boolean expandPE, peIsError;

  /**
   * Whether this is a validating parser.
   */
  private final boolean validating;

  /**
   * Whether strings representing markup will be interned.
   */
  private final boolean stringInterning;

  /**
   * If true, CDATA sections will be merged with adjacent text nodes into a
   * single event.
   */
  private final boolean coalescing;

  /**
   * Whether to replace general entity references with their replacement
   * text automatically during parsing.
   * Otherwise entity-reference events will be issued.
   */
  private final boolean replaceERefs;

  /**
   * Whether to support external entities.
   */
  private final boolean externalEntities;

  /**
   * Whether to support DTDs.
   */
  private final boolean supportDTD;

  /**
   * Whether to support XML namespaces. If true, namespace information will
   * be available. Otherwise namespaces will simply be reported as ordinary
   * attributes.
   */
  private final boolean namespaceAware;

  /**
   * Whether to support XML Base. If true, URIs specified in xml:base
   * attributes will be honoured when resolving external entities.
   */
  private final boolean baseAware;

  /**
   * Whether to report extended event types (START_ENTITY and END_ENTITY)
   * in addition to the standard event types. Used by the SAX parser.
   */
  private final boolean extendedEventTypes;

  /**
   * The reporter to receive parsing warnings.
   */
  final XMLReporter reporter;

  /**
   * Callback interface for resolving external entities.
   */
  final XMLResolver resolver;

  // -- Constants for testing the next kind of markup event --
  private static final String TEST_START_ELEMENT = "<";
  private static final String TEST_END_ELEMENT = "</";
  private static final String TEST_COMMENT = "<!--";
  private static final String TEST_PI = "<?";
  private static final String TEST_CDATA = "<![CDATA[";
  private static final String TEST_XML_DECL = "<?xml";
  private static final String TEST_DOCTYPE_DECL = "<!DOCTYPE";
  private static final String TEST_ELEMENT_DECL = "<!ELEMENT";
  private static final String TEST_ATTLIST_DECL = "<!ATTLIST";
  private static final String TEST_ENTITY_DECL = "<!ENTITY";
  private static final String TEST_NOTATION_DECL = "<!NOTATION";
  private static final String TEST_KET = ">";
  private static final String TEST_END_COMMENT = "--";
  private static final String TEST_END_PI = "?>";
  private static final String TEST_END_CDATA = "]]>";

  /**
   * The general entities predefined by the XML specification.
   */
  private static final LinkedHashMap PREDEFINED_ENTITIES = new LinkedHashMap();
  static
  {
    PREDEFINED_ENTITIES.put("amp", "&");
    PREDEFINED_ENTITIES.put("lt", "<");
    PREDEFINED_ENTITIES.put("gt", ">");
    PREDEFINED_ENTITIES.put("apos", "'");
    PREDEFINED_ENTITIES.put("quot", "\"");
  }

  /**
   * Creates a new XML parser for the given input stream.
   * This constructor should be used where possible, as it allows the
   * encoding of the XML data to be correctly determined from the stream.
   * @param in the input stream
   * @param systemId the URL from which the input stream was retrieved
   * (necessary if there are external entities to be resolved)
   * @param validating if the parser is to be a validating parser
   * @param namespaceAware if the parser should support XML Namespaces
   * @param coalescing if CDATA sections should be merged into adjacent text
   * nodes
   * @param replaceERefs if entity references should be automatically
   * replaced by their replacement text (otherwise they will be reported as
   * entity-reference events)
   * @param externalEntities if external entities should be loaded
   * @param supportDTD if support for the XML DTD should be enabled
   * @param baseAware if the parser should support XML Base to resolve
   * external entities
   * @param stringInterning whether strings will be interned during parsing
   * @param reporter the reporter to receive warnings during processing
   * @param resolver the callback interface used to resolve external
   * entities
   */
  public XMLParser(InputStream in, String systemId,
                   boolean validating,
                   boolean namespaceAware,
                   boolean coalescing,
                   boolean replaceERefs,
                   boolean externalEntities,
                   boolean supportDTD,
                   boolean baseAware,
                   boolean stringInterning,
                   boolean extendedEventTypes,
                   XMLReporter reporter,
                   XMLResolver resolver)
  {
    this.validating = validating;
    this.namespaceAware = namespaceAware;
    this.coalescing = coalescing;
    this.replaceERefs = replaceERefs;
    this.externalEntities = externalEntities;
    this.supportDTD = supportDTD;
    this.baseAware = baseAware;
    this.stringInterning = stringInterning;
    this.extendedEventTypes = extendedEventTypes;
    this.reporter = reporter;
    this.resolver = resolver;
    if (validating)
      {
        validationStack = new LinkedList();
        ids = new HashSet();
        idrefs = new HashSet();
      }
    String debug = System.getProperty("gnu.xml.debug.input");
    if (debug != null)
      {
        try
          {
            File file = File.createTempFile(debug, ".xml");
            in = new TeeInputStream(in, new FileOutputStream(file));
          }
        catch (IOException e)
          {
            RuntimeException e2 = new RuntimeException();
            e2.initCause(e);
            throw e2;
          }
      }
    systemId = canonicalize(systemId);
    pushInput(new Input(in, null, null, systemId, null, null, false, true));
  }

  /**
   * Creates a new XML parser for the given character stream.
   * This constructor is only available for compatibility with the JAXP
   * APIs, which permit XML to be parsed from a character stream. Because
   * the encoding specified by the character stream may conflict with that
   * specified in the XML declaration, this method should be avoided where
   * possible.
   * @param in the input stream
   * @param systemId the URL from which the input stream was retrieved
   * (necessary if there are external entities to be resolved)
   * @param validating if the parser is to be a validating parser
   * @param namespaceAware if the parser should support XML Namespaces
   * @param coalescing if CDATA sections should be merged into adjacent text
   * nodes
   * @param replaceERefs if entity references should be automatically
   * replaced by their replacement text (otherwise they will be reported as
   * entity-reference events)
   * @param externalEntities if external entities should be loaded
   * @param supportDTD if support for the XML DTD should be enabled
   * @param baseAware if the parser should support XML Base to resolve
   * external entities
   * @param stringInterning whether strings will be interned during parsing
   * @param reporter the reporter to receive warnings during processing
   * @param resolver the callback interface used to resolve external
   * entities
   */
  public XMLParser(Reader reader, String systemId,
                   boolean validating,
                   boolean namespaceAware,
                   boolean coalescing,
                   boolean replaceERefs,
                   boolean externalEntities,
                   boolean supportDTD,
                   boolean baseAware,
                   boolean stringInterning,
                   boolean extendedEventTypes,
                   XMLReporter reporter,
                   XMLResolver resolver)
  {
    this.validating = validating;
    this.namespaceAware = namespaceAware;
    this.coalescing = coalescing;
    this.replaceERefs = replaceERefs;
    this.externalEntities = externalEntities;
    this.supportDTD = supportDTD;
    this.baseAware = baseAware;
    this.stringInterning = stringInterning;
    this.extendedEventTypes = extendedEventTypes;
    this.reporter = reporter;
    this.resolver = resolver;
    if (validating)
      {
        validationStack = new LinkedList();
        ids = new HashSet();
        idrefs = new HashSet();
      }
    String debug = System.getProperty("gnu.xml.debug.input");
    if (debug != null)
      {
        try
          {
            File file = File.createTempFile(debug, ".xml");
            reader = new TeeReader(reader, new FileWriter(file));
          }
        catch (IOException e)
          {
            RuntimeException e2 = new RuntimeException();
            e2.initCause(e);
            throw e2;
          }
      }
    systemId = canonicalize(systemId);
    pushInput(new Input(null, reader, null, systemId, null, null, false, true));
  }

  // -- NamespaceContext --

  public String getNamespaceURI(String prefix)
  {
    if (XMLConstants.XML_NS_PREFIX.equals(prefix))
      return XMLConstants.XML_NS_URI;
    if (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix))
      return XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        LinkedHashMap ctx = (LinkedHashMap) i.next();
        String namespaceURI = (String) ctx.get(prefix);
        if (namespaceURI != null)
          return namespaceURI;
      }
    return null;
  }

  public String getPrefix(String namespaceURI)
  {
    if (XMLConstants.XML_NS_URI.equals(namespaceURI))
      return XMLConstants.XML_NS_PREFIX;
    if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI))
      return XMLConstants.XMLNS_ATTRIBUTE;
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        LinkedHashMap ctx = (LinkedHashMap) i.next();
        if (ctx.containsValue(namespaceURI))
          {
            for (Iterator j = ctx.entrySet().iterator(); j.hasNext(); )
              {
                Map.Entry entry = (Map.Entry) i.next();
                String uri = (String) entry.getValue();
                if (uri.equals(namespaceURI))
                  return (String) entry.getKey();
              }
          }
      }
    return null;
  }

  public Iterator getPrefixes(String namespaceURI)
  {
    if (XMLConstants.XML_NS_URI.equals(namespaceURI))
      return Collections.singleton(XMLConstants.XML_NS_PREFIX).iterator();
    if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI))
      return Collections.singleton(XMLConstants.XMLNS_ATTRIBUTE).iterator();
    LinkedList acc = new LinkedList();
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        LinkedHashMap ctx = (LinkedHashMap) i.next();
        if (ctx.containsValue(namespaceURI))
          {
            for (Iterator j = ctx.entrySet().iterator(); j.hasNext(); )
              {
                Map.Entry entry = (Map.Entry) i.next();
                String uri = (String) entry.getValue();
                if (uri.equals(namespaceURI))
                  acc.add(entry.getKey());
              }
          }
      }
    return acc.iterator();
  }

  // -- XMLStreamReader --

  public void close()
    throws XMLStreamException
  {
    stack = null;
    namespaces = null;
    bases = null;
    buf = null;
    attrs = null;
    doctype = null;

    inputStack = null;
    validationStack = null;
    ids = null;
    idrefs = null;
  }

  public NamespaceContext getNamespaceContext()
  {
    return this;
  }

  public int getAttributeCount()
  {
    return attrs.size();
  }

  public String getAttributeLocalName(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    return a.localName;
  }

  public String getAttributeNamespace(int index)
  {
    String prefix = getAttributePrefix(index);
    return getNamespaceURI(prefix);
  }

  public String getAttributePrefix(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    return a.prefix;
  }

  public QName getAttributeName(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    String namespaceURI = getNamespaceURI(a.prefix);
    return new QName(namespaceURI, a.localName, a.prefix);
  }

  public String getAttributeType(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    return a.type;
  }

  private String getAttributeType(String elementName, String attName)
  {
    if (doctype != null)
      {
        AttributeDecl att = doctype.getAttributeDecl(elementName, attName);
        if (att != null)
          return att.type;
      }
    return "CDATA";
  }

  public String getAttributeValue(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    return a.value;
  }

  public String getAttributeValue(String namespaceURI, String localName)
  {
    for (Iterator i = attrs.iterator(); i.hasNext(); )
      {
        Attribute a = (Attribute) i.next();
        if (a.localName.equals(localName))
          {
            String uri = getNamespaceURI(a.prefix);
            if ((uri == null && namespaceURI == null) ||
                (uri != null && uri.equals(namespaceURI)))
              return a.value;
          }
      }
    return null;
  }

  boolean isAttributeDeclared(int index)
  {
    if (doctype == null)
      return false;
    Attribute a = (Attribute) attrs.get(index);
    String qn = ("".equals(a.prefix)) ? a.localName :
      a.prefix + ":" + a.localName;
    String elementName = buf.toString();
    return doctype.isAttributeDeclared(elementName, qn);
  }
  
  public String getCharacterEncodingScheme()
  {
    return xmlEncoding;
  }

  public String getElementText()
    throws XMLStreamException
  {
    if (event != XMLStreamConstants.START_ELEMENT)
      throw new XMLStreamException("current event must be START_ELEMENT");
    StringBuffer elementText = new StringBuffer();
    int depth = stack.size();
    while (event != XMLStreamConstants.END_ELEMENT || stack.size() > depth)
      {
        switch (next())
          {
          case XMLStreamConstants.CHARACTERS:
          case XMLStreamConstants.SPACE:
            elementText.append(buf.toString());
          }
      }
    return elementText.toString();
  }

  public String getEncoding()
  {
    return (input.inputEncoding == null) ? "UTF-8" : input.inputEncoding;
  }

  public int getEventType()
  {
    return event;
  }

  public String getLocalName()
  {
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        String qName = buf.toString();
        int ci = qName.indexOf(':');
        return (ci == -1) ? qName : qName.substring(ci + 1);
      default:
        return null;
      }
  }

  public Location getLocation()
  {
    return input;
  }

  public QName getName()
  {
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        String qName = buf.toString();
        int ci = qName.indexOf(':');
        String localName = (ci == -1) ? qName : qName.substring(ci + 1);
        String prefix = (ci == -1) ?
          (namespaceAware ? XMLConstants.DEFAULT_NS_PREFIX : null) :
          qName.substring(0, ci);
        String namespaceURI = getNamespaceURI(prefix);
        return new QName(namespaceURI, localName, prefix);
      default:
        return null;
      }
  }

  public int getNamespaceCount()
  {
    if (!namespaceAware || namespaces.isEmpty())
      return 0;
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
        return ctx.size();
      default:
        return 0;
      }
  }

  public String getNamespacePrefix(int index)
  {
    LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
    int count = 0;
    for (Iterator i = ctx.keySet().iterator(); i.hasNext(); )
      {
        String prefix = (String) i.next();
        if (count++ == index)
          return prefix;
      }
    return null;
  }

  public String getNamespaceURI()
  {
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        String qName = buf.toString();
        int ci = qName.indexOf(':');
        if (ci == -1)
          return null;
        String prefix = qName.substring(0, ci);
        return getNamespaceURI(prefix);
      default:
        return null;
      }
  }

  public String getNamespaceURI(int index)
  {
    LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
    int count = 0;
    for (Iterator i = ctx.values().iterator(); i.hasNext(); )
      {
        String uri = (String) i.next();
        if (count++ == index)
          return uri;
      }
    return null;
  }

  public String getPIData()
  {
    return piData;
  }

  public String getPITarget()
  {
    return piTarget;
  }

  public String getPrefix()
  {
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        String qName = buf.toString();
        int ci = qName.indexOf(':');
        return (ci == -1) ?
          (namespaceAware ? XMLConstants.DEFAULT_NS_PREFIX : null) :
          qName.substring(0, ci);
      default:
        return null;
      }
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    if (name == null)
      throw new IllegalArgumentException("name is null");
    if (XMLInputFactory.ALLOCATOR.equals(name))
      return null;
    if (XMLInputFactory.IS_COALESCING.equals(name))
      return coalescing ? Boolean.TRUE : Boolean.FALSE;
    if (XMLInputFactory.IS_NAMESPACE_AWARE.equals(name))
      return namespaceAware ? Boolean.TRUE : Boolean.FALSE;
    if (XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES.equals(name))
      return replaceERefs ? Boolean.TRUE : Boolean.FALSE;
    if (XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES.equals(name))
      return externalEntities ? Boolean.TRUE : Boolean.FALSE;
    if (XMLInputFactory.IS_VALIDATING.equals(name))
      return Boolean.FALSE;
    if (XMLInputFactory.REPORTER.equals(name))
      return reporter;
    if (XMLInputFactory.RESOLVER.equals(name))
      return resolver;
    if (XMLInputFactory.SUPPORT_DTD.equals(name))
      return supportDTD ? Boolean.TRUE : Boolean.FALSE;
    if ("gnu.xml.stream.stringInterning".equals(name))
      return stringInterning ? Boolean.TRUE : Boolean.FALSE;
    if ("gnu.xml.stream.xmlBase".equals(name))
      return baseAware ? Boolean.TRUE : Boolean.FALSE;
    if ("gnu.xml.stream.baseURI".equals(name))
      return getXMLBase();
    return null;
  }

  public String getText()
  {
    return buf.toString();
  }

  public char[] getTextCharacters()
  {
    return buf.toString().toCharArray();
  }

  public int getTextCharacters(int sourceStart, char[] target,
                               int targetStart, int length)
    throws XMLStreamException
  {
    length = Math.min(sourceStart + buf.length(), length);
    int sourceEnd = sourceStart + length;
    buf.getChars(sourceStart, sourceEnd, target, targetStart);
    return length;
  }

  public int getTextLength()
  {
    return buf.length();
  }

  public int getTextStart()
  {
    return 0;
  }

  public String getVersion()
  {
    return (xmlVersion == null) ? "1.0" : xmlVersion;
  }

  public boolean hasName()
  {
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
      case XMLStreamConstants.END_ELEMENT:
        return true;
      default:
        return false;
      }
  }

  public boolean hasText()
  {
    switch (event)
      {
      case XMLStreamConstants.CHARACTERS:
      case XMLStreamConstants.SPACE:
        return true;
      default:
        return false;
      }
  }

  public boolean isAttributeSpecified(int index)
  {
    Attribute a = (Attribute) attrs.get(index);
    return a.specified;
  }

  public boolean isCharacters()
  {
    return (event == XMLStreamConstants.CHARACTERS);
  }

  public boolean isEndElement()
  {
    return (event == XMLStreamConstants.END_ELEMENT);
  }

  public boolean isStandalone()
  {
    return Boolean.TRUE.equals(xmlStandalone);
  }

  public boolean isStartElement()
  {
    return (event == XMLStreamConstants.START_ELEMENT);
  }

  public boolean isWhiteSpace()
  {
    return (event == XMLStreamConstants.SPACE);
  }

  public int nextTag()
    throws XMLStreamException
  {
    do
      {
        switch (next())
          {
          case XMLStreamConstants.START_ELEMENT:
          case XMLStreamConstants.END_ELEMENT:
          case XMLStreamConstants.CHARACTERS:
          case XMLStreamConstants.SPACE:
          case XMLStreamConstants.COMMENT:
          case XMLStreamConstants.PROCESSING_INSTRUCTION:
            break;
          default:
            throw new XMLStreamException("Unexpected event type: " + event);
          }
      }
    while (event != XMLStreamConstants.START_ELEMENT &&
           event != XMLStreamConstants.END_ELEMENT);
    return event;
  }

  public void require(int type, String namespaceURI, String localName)
    throws XMLStreamException
  {
    if (event != type)
      throw new XMLStreamException("Current event type is " + event);
    if (event == XMLStreamConstants.START_ELEMENT ||
        event == XMLStreamConstants.END_ELEMENT)
      {
        String ln = getLocalName();
        if (!ln.equals(localName))
          throw new XMLStreamException("Current local-name is " + ln);
        String uri = getNamespaceURI();
        if ((uri == null && namespaceURI != null) ||
            (uri != null && !uri.equals(namespaceURI)))
          throw new XMLStreamException("Current namespace URI is " + uri);
      }
  }

  public boolean standaloneSet()
  {
    return (xmlStandalone != null);
  }

  public boolean hasNext()
    throws XMLStreamException
  {
    if (event == XMLStreamConstants.END_DOCUMENT)
      return false;
    if (!lookahead)
      {
        next();
        lookahead = true;
      }
    return event != -1;
  }
  
  public int next()
    throws XMLStreamException
  {
    if (lookahead)
      {
        lookahead = false;
        return event;
      }
    if (event == XMLStreamConstants.END_ELEMENT)
      {
        // Pop namespace context
        if (namespaceAware && !namespaces.isEmpty())
          namespaces.removeFirst();
        // Pop base context
        if (baseAware && !bases.isEmpty())
          bases.removeFirst();
      }
    if (!startEntityStack.isEmpty())
      {
        String entityName = (String) startEntityStack.removeFirst();
        buf.setLength(0);
        buf.append(entityName);
        event = START_ENTITY;
        return extendedEventTypes ? event : next();
      }
    else if (!endEntityStack.isEmpty())
      {
        String entityName = (String) endEntityStack.removeFirst();
        buf.setLength(0);
        buf.append(entityName);
        event = END_ENTITY;
        return extendedEventTypes ? event : next();
      }
    try
      {
        if (!input.initialized)
          input.init();
        switch (state)
          {
          case CONTENT:
            if (tryRead(TEST_END_ELEMENT))
              {
                readEndElement();
                if (stack.isEmpty())
                  state = MISC;
                event = XMLStreamConstants.END_ELEMENT;
              }
            else if (tryRead(TEST_COMMENT))
              {
                readComment(false);
                event = XMLStreamConstants.COMMENT;
              }
            else if (tryRead(TEST_PI))
              {
                readPI(false);
                event = XMLStreamConstants.PROCESSING_INSTRUCTION;
              }
            else if (tryRead(TEST_CDATA))
              {
                readCDSect();
                event = XMLStreamConstants.CDATA;
              }
            else if (tryRead(TEST_START_ELEMENT))
              {
                state = readStartElement();
                event = XMLStreamConstants.START_ELEMENT;
              }
            else
              {
                // Check for character reference or predefined entity
                mark(8);
                int c = readCh();
                if (c == 0x26) // '&'
                  {
                    c = readCh();
                    if (c == 0x23) // '#'
                      {
                        reset();
                        event = readCharData(null);
                      }
                    else
                      {
                        // entity reference
                        reset();
                        readCh(); // &
                        readReference();
                        String ref = buf.toString();
                        String text = (String) PREDEFINED_ENTITIES.get(ref);
                        if (text != null)
                          {
                            event = readCharData(text);
                          }
                        else if (replaceERefs && !isUnparsedEntity(ref))
                          {
                            // this will report a start-entity event
                            boolean external = false;
                            if (doctype != null)
                              {
                                Object entity = doctype.getEntity(ref);
                                if (entity instanceof ExternalIds)
                                  external = true;
                              }
                            expandEntity(ref, false, external);
                            event = next();
                          }
                        else
                          {
                            event = XMLStreamConstants.ENTITY_REFERENCE;
                          }
                      }
                  }
                else
                  {
                    reset();
                    event = readCharData(null);
                    if (validating && doctype != null)
                      validatePCData(buf.toString());
                  }
              }
            break;
          case EMPTY_ELEMENT:
            String elementName = (String) stack.removeLast();
            buf.setLength(0);
            buf.append(elementName);
            state = stack.isEmpty() ? MISC : CONTENT;
            event = XMLStreamConstants.END_ELEMENT;
            if (validating && doctype != null)
              endElementValidationHook();
            break;
          case INIT: // XMLDecl?
            if (tryRead(TEST_XML_DECL))
              readXMLDecl();
            input.finalizeEncoding();
            event = XMLStreamConstants.START_DOCUMENT;
            state = PROLOG;
            break;
          case PROLOG: // Misc* (doctypedecl Misc*)?
            skipWhitespace();
            if (doctype == null && tryRead(TEST_DOCTYPE_DECL))
              {
                readDoctypeDecl();
                event = XMLStreamConstants.DTD;
              }
            else if (tryRead(TEST_COMMENT))
              {
                readComment(false);
                event = XMLStreamConstants.COMMENT;
              }
            else if (tryRead(TEST_PI))
              {
                readPI(false);
                event = XMLStreamConstants.PROCESSING_INSTRUCTION;
              }
            else if (tryRead(TEST_START_ELEMENT))
              {
                state = readStartElement();
                event = XMLStreamConstants.START_ELEMENT;
              }
            else
              {
                int c = readCh();
                error("no root element: U+" + Integer.toHexString(c));
              }
            break;
          case MISC: // Comment | PI | S
            skipWhitespace();
            if (tryRead(TEST_COMMENT))
              {
                readComment(false);
                event = XMLStreamConstants.COMMENT;
              }
            else if (tryRead(TEST_PI))
              {
                readPI(false);
                event = XMLStreamConstants.PROCESSING_INSTRUCTION;
              }
            else
              {
                if (event == XMLStreamConstants.END_DOCUMENT)
                  throw new NoSuchElementException();
                int c = readCh();
                if (c != -1)
                  error("Only comments and PIs may appear after " +
                        "the root element");
                event = XMLStreamConstants.END_DOCUMENT;
              }
            break;
          default:
            event = -1;
          }
        return event;
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException();
        e2.initCause(e);
        throw e2;
      }
  }

  // package private

  /**
   * Returns the current element name.
   */
  String getCurrentElement()
  {
    return (String) stack.getLast();
  }

  // private

  private void mark(int limit)
    throws IOException
  {
    input.mark(limit);
  }

  private void reset()
    throws IOException
  {
    input.reset();
  }

  private int read()
    throws IOException
  {
    return input.read();
  }

  private int read(int[] b, int off, int len)
    throws IOException
  {
    return input.read(b, off, len);
  }
  
  /**
   * Parsed character read.
   */
  private int readCh()
    throws IOException, XMLStreamException
  {
    int c = read();
    if (expandPE && c == 0x25) // '%'
      {
        if (peIsError)
          error("PE reference within decl in internal subset.");
        expandPEReference();
        return readCh();
      }
    return c;
  }

  /**
   * Reads the next character, ensuring it is the character specified.
   * @param delim the character to match
   * @exception XMLStreamException if the next character is not the
   * specified one
   */
  private void require(char delim)
    throws IOException, XMLStreamException
  {
    mark(1);
    int c = readCh();
    if (delim != c)
      {
        reset();
        error("required character (got U+" + Integer.toHexString(c) + ")",
              new Character(delim));
      }
  }

  /**
   * Reads the next few characters, ensuring they match the string specified.
   * @param delim the string to match
   * @exception XMLStreamException if the next characters do not match the
   * specified string
   */
  private void require(String delim)
    throws IOException, XMLStreamException
  {
    char[] chars = delim.toCharArray();
    int len = chars.length;
    mark(len);
    int off = 0;
    do
      {
        int l2 = read(tmpBuf, off, len - off);
        if (l2 == -1)
          {
            reset();
            error("EOF before required string", delim);
          }
        off += l2;
      }
    while (off < len);
    for (int i = 0; i < chars.length; i++)
      {
        if (chars[i] != tmpBuf[i])
          {
            reset();
            error("required string", delim);
          }
      }
  }

  /**
   * Try to read a single character. On failure, reset the stream.
   * @param delim the character to test
   * @return true if the character matched delim, false otherwise.
   */
  private boolean tryRead(char delim)
    throws IOException, XMLStreamException
  {
    mark(1);
    int c = readCh();
    if (delim != c)
      {
        reset();
        return false;
      }
    return true;
  }

  /**
   * Tries to read the specified characters.
   * If successful, the stream is positioned after the last character,
   * otherwise it is reset.
   * @param test the string to test
   * @return true if the characters matched the test string, false otherwise.
   */
  private boolean tryRead(String test)
    throws IOException
  {
    char[] chars = test.toCharArray();
    int len = chars.length;
    mark(len);
    int count = 0;
    int l2 = read(tmpBuf, 0, len);
    if (l2 == -1)
      {
        reset();
        return false;
      }
    count += l2;
    // check the characters we received first before doing additional reads
    for (int i = 0; i < count; i++)
      {
        if (chars[i] != tmpBuf[i])
          {
            reset();
            return false;
          }
      }
    while (count < len)
      {
        // force read
        int c = read();
        if (c == -1)
          {
            reset();
            return false;
          }
        tmpBuf[count] = (char) c;
        // check each character as it is read
        if (chars[count] != tmpBuf[count])
          {
            reset();
            return false;
          }
        count++;
      }
    return true;
  }

  /**
   * Reads characters until the specified test string is encountered.
   * @param delim the string delimiting the end of the characters
   */
  private void readUntil(String delim)
    throws IOException, XMLStreamException
  {
    int startLine = input.line;
    try
      {
        while (!tryRead(delim))
          {
            int c = readCh();
            if (c == -1)
              throw new EOFException();
            else if (input.xml11)
              {
                if (!isXML11Char(c) || isXML11RestrictedChar(c))
                  error("illegal XML 1.1 character",
                        "U+" + Integer.toHexString(c));
              }
            else if (!isChar(c))
              error("illegal XML character", 
                    "U+" + Integer.toHexString(c));
            buf.append(Character.toChars(c));
          }
      }
    catch (EOFException e)
      {
        error("end of input while looking for delimiter "+
              "(started on line " + startLine + ')', delim);
      }
  }

  /**
   * Reads any whitespace characters.
   * @return true if whitespace characters were read, false otherwise
   */
  private boolean tryWhitespace()
    throws IOException, XMLStreamException
  {
    boolean white;
    boolean ret = false;
    do
      {
        mark(1);
        int c = readCh();
        while (c == -1 && inputStack.size() > 1)
          {
            popInput();
            c = readCh();
          }
        white = (c == 0x20 || c == 0x09 || c == 0x0a || c == 0x0d);
        if (white)
          ret = true;
      }
    while (white);
    reset();
    return ret;
  }

  /**
   * Skip over any whitespace characters.
   */
  private void skipWhitespace()
    throws IOException, XMLStreamException
  {
    boolean white;
    do
      {
        mark(1);
        int c = readCh();
        while (c == -1 && inputStack.size() > 1)
          {
            popInput();
            c = readCh();
          }
        white = (c == 0x20 || c == 0x09 || c == 0x0a || c == 0x0d);
      }
    while (white);
    reset();
  }

  /**
   * Try to read as many whitespace characters as are available.
   * @exception XMLStreamException if no whitespace characters were seen
   */
  private void requireWhitespace()
    throws IOException, XMLStreamException
  {
    if (!tryWhitespace())
      error("whitespace required");
  }

  /**
   * Returns the current base URI for resolving external entities.
   */
  String getXMLBase()
  {
    if (baseAware)
      {
        for (Iterator i = bases.iterator(); i.hasNext(); )
          {
            String base = (String) i.next();
            if (base != null)
              return base;
          }
      }
    return input.systemId;
  }

  /**
   * Push the specified text input source.
   */
  private void pushInput(String name, String text, boolean report,
                         boolean normalize)
    throws IOException, XMLStreamException
  {
    // Check for recursion
    if (name != null && !"".equals(name))
      {
        for (Iterator i = inputStack.iterator(); i.hasNext(); )
          {
            Input ctx = (Input) i.next();
            if (name.equals(ctx.name))
              error("entities may not be self-recursive", name);
          }
      }
    else
      report = false;
    pushInput(new Input(null, new StringReader(text), input.publicId,
                        input.systemId, name, input.inputEncoding, report,
                        normalize));
  }

  /**
   * Push the specified external input source.
   */
  private void pushInput(String name, ExternalIds ids, boolean report,
                         boolean normalize)
    throws IOException, XMLStreamException
  {
    if (!externalEntities)
      return;
    String url = canonicalize(absolutize(input.systemId, ids.systemId));
    // Check for recursion
    for (Iterator i = inputStack.iterator(); i.hasNext(); )
      {
        Input ctx = (Input) i.next();
        if (url.equals(ctx.systemId))
          error("entities may not be self-recursive", url);
        if (name != null && !"".equals(name) && name.equals(ctx.name))
          error("entities may not be self-recursive", name);
      }
    if (name == null || "".equals(name))
      report = false;
    InputStream in = null;
    if (resolver != null)
      {
        Object obj = resolver.resolveEntity(ids.publicId, url, getXMLBase(),
                                            null);
        if (obj instanceof InputStream)
          in = (InputStream) obj;
      }
    if (in == null)
      in = resolve(url);
    if (in == null)
      error("unable to resolve external entity",
            (ids.systemId != null) ? ids.systemId : ids.publicId);
    pushInput(new Input(in, null, ids.publicId, url, name, null, report,
                        normalize));
    input.init();
    if (tryRead(TEST_XML_DECL))
      readTextDecl();
    input.finalizeEncoding();
  }

  /**
   * Push the specified input source (general entity) onto the input stack.
   */
  private void pushInput(Input input)
  {
    if (input.report)
      startEntityStack.addFirst(input.name);
    inputStack.addLast(input);
    if (this.input != null)
      input.xml11 = this.input.xml11;
    this.input = input;
  }

  /**
   * Returns a canonicalized version of the specified URL.
   * This is largely to work around a problem with the specification of
   * file URLs.
   */
  static String canonicalize(String url)
  {
    if (url == null)
      return null;
    if (url.startsWith("file:") && !url.startsWith("file://"))
      url = "file://" + url.substring(5);
    return url;
  }

  /**
   * "Absolutize" a URL. This resolves a relative URL into an absolute one.
   * @param base the current base URL
   * @param href the (absolute or relative) URL to resolve
   */
  public static String absolutize(String base, String href)
  {
    if (href == null)
      return null;
    int ci = href.indexOf(':');
    if (ci > 1 && isURLScheme(href.substring(0, ci)))
      {
        // href is absolute already
        return href;
      }
    if (base == null)
      base = "";
    else
      {
        int i = base.lastIndexOf('/');
        if (i != -1)
          base = base.substring(0, i + 1);
        else
          base = "";
      }
    if ("".equals(base))
      {
        // assume file URL relative to current directory
        base = System.getProperty("user.dir");
        if (base.charAt(0) == '/')
          base = base.substring(1);
        base = "file:///" + base.replace(File.separatorChar, '/');
        if (!base.endsWith("/"))
          base += "/";
      }
    // We can't use java.net.URL here to do the parsing, as it searches for
    // a protocol handler. A protocol handler may not be registered for the
    // URL scheme here. Do it manually.
    // 
    // Set aside scheme and host portion of base URL
    String basePrefix = null;
    ci = base.indexOf(':');
    if (ci > 1 && isURLScheme(base.substring(0, ci)))
      {
          if (base.length() > (ci + 3)  &&
              base.charAt(ci + 1) == '/' &&
              base.charAt(ci + 2) == '/')
            {
              int si = base.indexOf('/', ci + 3);
              if (si == -1)
                base = null;
              else
                {
                  basePrefix = base.substring(0, si);
                  base = base.substring(si);
                }
            }
          else
            base = null;
      }
    if (base == null) // unknown or malformed base URL, use href
      return href;
    if (href.startsWith("/")) // absolute href pathname
      return (basePrefix == null) ? href : basePrefix + href;
    // relative href pathname
    if (!base.endsWith("/"))
      {
        int lsi = base.lastIndexOf('/');
        if (lsi == -1)
          base = "/";
        else
          base = base.substring(0, lsi + 1);
      }
    while (href.startsWith("../") || href.startsWith("./"))
      {
        if (href.startsWith("../"))
          {
            // strip last path component from base
            int lsi = base.lastIndexOf('/', base.length() - 2);
            if (lsi > -1)
              base = base.substring(0, lsi + 1);
            href = href.substring(3); // strip ../ prefix
          }
        else
          {
            href = href.substring(2); // strip ./ prefix
          }
      }
    return (basePrefix == null) ? base + href : basePrefix + base + href;
  }

  /**
   * Indicates whether the specified characters match the scheme portion of
   * a URL.
   * @see RFC 1738 section 2.1
   */
  private static boolean isURLScheme(String text)
  {
    int len = text.length();
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        if (c == '+' || c == '.' || c == '-')
          continue;
        if (c < 65 || (c > 90 && c < 97) || c > 122)
          return false;
      }
    return true;
  }

  /**
   * Returns an input stream for the given URL.
   */
  static InputStream resolve(String url)
    throws IOException
  {
    try
      {
        return new URL(url).openStream();
      }
    catch (MalformedURLException e)
      {
        return null;
      }
    catch (IOException e)
      {
        IOException e2 = new IOException("error resolving " + url);
        e2.initCause(e);
        throw e2;
      }
  }

  /**
   * Pops the current input source (general entity) off the stack.
   */
  private void popInput()
  {
    Input old = (Input) inputStack.removeLast();
    if (old.report)
      endEntityStack.addFirst(old.name);
    input = (Input) inputStack.getLast();
  }

  /**
   * Parse an entity text declaration.
   */
  private void readTextDecl()
    throws IOException, XMLStreamException
  {
    final int flags = LIT_DISABLE_CREF | LIT_DISABLE_PE | LIT_DISABLE_EREF;
    requireWhitespace();
    if (tryRead("version"))
      {
        readEq();
        String v = readLiteral(flags, false);
        if ("1.0".equals(v))
          input.xml11 = false;
        else if ("1.1".equals(v))
          {
            Input i1 = (Input) inputStack.getFirst();
            if (!i1.xml11)
              error("external entity specifies later version number");
            input.xml11 = true;
          }
        else
          throw new XMLStreamException("illegal XML version: " + v);
        requireWhitespace();
      }
    require("encoding");
    readEq();
    String enc = readLiteral(flags, false);
    skipWhitespace();
    require("?>");
    input.setInputEncoding(enc);
  }

  /**
   * Parse the XML declaration.
   */
  private void readXMLDecl()
    throws IOException, XMLStreamException
  {
    final int flags = LIT_DISABLE_CREF | LIT_DISABLE_PE | LIT_DISABLE_EREF;
    
    requireWhitespace();
    require("version");
    readEq();
    xmlVersion = readLiteral(flags, false);
    if ("1.0".equals(xmlVersion))
      input.xml11 = false;
    else if ("1.1".equals(xmlVersion))
      input.xml11 = true;
    else
      throw new XMLStreamException("illegal XML version: " + xmlVersion);
    
    boolean white = tryWhitespace();
    
    if (tryRead("encoding"))
      {
        if (!white)
          error("whitespace required before 'encoding='");
        readEq();
        xmlEncoding = readLiteral(flags, false);
        white = tryWhitespace();
      }
    
    if (tryRead("standalone"))
      {
        if (!white)
          error("whitespace required before 'standalone='");
        readEq();
        String standalone = readLiteral(flags, false);
        if ("yes".equals(standalone))
          xmlStandalone = Boolean.TRUE;
        else if ("no".equals(standalone))
          xmlStandalone = Boolean.FALSE;
        else
          error("standalone flag must be 'yes' or 'no'", standalone);
      }

    skipWhitespace();
    require("?>");
    if (xmlEncoding != null)
      input.setInputEncoding(xmlEncoding);
  }

  /**
   * Parse the DOCTYPE declaration.
   */
  private void readDoctypeDecl()
    throws IOException, XMLStreamException
  {
    if (!supportDTD)
      error("parser was configured not to support DTDs");
    requireWhitespace();
    String rootName = readNmtoken(true);
    skipWhitespace();
    ExternalIds ids = readExternalIds(false, true);
    doctype =
      this.new Doctype(rootName, ids.publicId, ids.systemId);
    
    // Parse internal subset first
    skipWhitespace();
    if (tryRead('['))
      {
        while (true)
          {
            expandPE = true;
            skipWhitespace();
            expandPE = false;
            if (tryRead(']'))
              break;
            else
              readMarkupdecl(false);
          }
      }
    skipWhitespace();
    require('>');

    // Parse external subset
    if (ids.systemId != null && externalEntities)
      {
        pushInput("", ">", false, false);
        pushInput("[dtd]", ids, true, true);
        // loop until we get back to ">"
        while (true)
          {
            expandPE = true;
            skipWhitespace();
            expandPE = false;
            mark(1);
            int c = readCh();
            if (c == 0x3e) // '>'
              break;
            else if (c == -1)
              popInput();
            else
              {
                reset();
                expandPE = true;
                readMarkupdecl(true);
                expandPE = true;
              }
          }
        if (inputStack.size() != 2)
          error("external subset has unmatched '>'");
        popInput();
      }
    checkDoctype();
    if (validating)
      validateDoctype();

    // Make rootName available for reading
    buf.setLength(0);
    buf.append(rootName);
  }

  /**
   * Checks the well-formedness of the DTD.
   */
  private void checkDoctype()
    throws XMLStreamException
  {
    // TODO check entity recursion
  }

  /**
   * Parse the markupdecl production.
   */
  private void readMarkupdecl(boolean inExternalSubset)
    throws IOException, XMLStreamException
  {
    boolean saved = expandPE;
    mark(1);
    require('<');
    reset();
    expandPE = false;
    if (tryRead(TEST_ELEMENT_DECL))
      {
        expandPE = saved;
        readElementDecl();
      }
    else if (tryRead(TEST_ATTLIST_DECL))
      {
        expandPE = saved;
        readAttlistDecl();
      }
    else if (tryRead(TEST_ENTITY_DECL))
      {
        expandPE = saved;
        readEntityDecl(inExternalSubset);
      }
    else if (tryRead(TEST_NOTATION_DECL))
      {
        expandPE = saved;
        readNotationDecl(inExternalSubset);
      }
    else if (tryRead(TEST_PI))
      {
        readPI(true);
        expandPE = saved;
      }
    else if (tryRead(TEST_COMMENT))
      {
        readComment(true);
        expandPE = saved;
      }
    else if (tryRead("<!["))
      {
        // conditional section
        expandPE = saved;
        if (inputStack.size() < 2)
          error("conditional sections illegal in internal subset");
        skipWhitespace();
        if (tryRead("INCLUDE"))
          {
            skipWhitespace();
            require('[');
            skipWhitespace();
            while (!tryRead("]]>"))
              {
                readMarkupdecl(inExternalSubset);
                skipWhitespace();
              }
          }
        else if (tryRead("IGNORE"))
          {
            skipWhitespace();
            require('[');
            expandPE = false;
            for (int nesting = 1; nesting > 0; )
              {
                int c = readCh();
                switch (c)
                  {
                  case 0x3c: // '<'
                    if (tryRead("!["))
                      nesting++;
                    break;
                  case 0x5d: // ']'
                    if (tryRead("]>"))
                      nesting--;
                    break;
                  case -1:
                    throw new EOFException();
                  }
              }
            expandPE = saved;
          }
        else
          error("conditional section must begin with INCLUDE or IGNORE");
      }
    else
      error("expected markup declaration");
  }

  /**
   * Parse the elementdecl production.
   */
  private void readElementDecl()
    throws IOException, XMLStreamException
  {
    requireWhitespace();
    boolean saved = expandPE;
    expandPE = (inputStack.size() > 1);
    String name = readNmtoken(true);
    expandPE = saved;
    requireWhitespace();
    readContentspec(name);
    skipWhitespace();
    require('>');
  }

  /**
   * Parse the contentspec production.
   */
  private void readContentspec(String elementName)
    throws IOException, XMLStreamException
  {
    if (tryRead("EMPTY"))
      doctype.addElementDecl(elementName, "EMPTY", new EmptyContentModel());
    else if (tryRead("ANY"))
      doctype.addElementDecl(elementName, "ANY", new AnyContentModel());
    else
      {
        ContentModel model;
        StringBuffer acc = new StringBuffer();
        require('(');
        acc.append('(');
        skipWhitespace();
        if (tryRead("#PCDATA"))
          {
            // mixed content
            acc.append("#PCDATA");
            MixedContentModel mm = new MixedContentModel();
            model = mm;
            skipWhitespace();
            if (tryRead(')'))
              {
                acc.append(")");
                if (tryRead('*'))
                  {
                    mm.min = 0;
                    mm.max = -1;
                  }
              }
            else
              {
                while (!tryRead(")"))
                  {
                    require('|');
                    acc.append('|');
                    skipWhitespace();
                    String name = readNmtoken(true);
                    acc.append(name);
                    mm.addName(name);
                    skipWhitespace();
                  }
                require('*');
                acc.append(")*");
                mm.min = 0;
                mm.max = -1;
              }
          }
        else
          model = readElements(acc);
        doctype.addElementDecl(elementName, acc.toString(), model);
      }
  }

  /**
   * Parses an element content model.
   */
  private ElementContentModel readElements(StringBuffer acc)
    throws IOException, XMLStreamException
  {
    int separator;
    ElementContentModel model = new ElementContentModel();
    
    // Parse first content particle
    skipWhitespace();
    model.addContentParticle(readContentParticle(acc));
    // End or separator
    skipWhitespace();
    int c = readCh();
    switch (c)
      {
      case 0x29: // ')'
        acc.append(')');
        mark(1);
        c = readCh();
        switch (c)
          {
          case 0x3f: // '?'
            acc.append('?');
            model.min = 0;
            model.max = 1;
            break;
          case 0x2a: // '*'
            acc.append('*');
            model.min = 0;
            model.max = -1;
            break;
          case 0x2b: // '+'
            acc.append('+');
            model.min = 1;
            model.max = -1;
            break;
          default:
            reset();
          }
        return model; // done
      case 0x7c: // '|'
        model.or = true;
        // fall through
      case 0x2c: // ','
        separator = c;
        acc.append(Character.toChars(c));
        break;
      default:
        error("bad separator in content model",
              "U+" + Integer.toHexString(c));
        return model;
      }
    // Parse subsequent content particles
    while (true)
      {
        skipWhitespace();
        model.addContentParticle(readContentParticle(acc));
        skipWhitespace();
        c = readCh();
        if (c == 0x29) // ')'
          {
            acc.append(')');
            break;
          }
        else if (c != separator)
          {
            error("bad separator in content model",
                  "U+" + Integer.toHexString(c));
            return model;
          }
        else
          acc.append(c);
      }
    // Check for occurrence indicator
    mark(1);
    c = readCh();
    switch (c)
      {
      case 0x3f: // '?'
        acc.append('?');
        model.min = 0;
        model.max = 1;
        break;
      case 0x2a: // '*'
        acc.append('*');
        model.min = 0;
        model.max = -1;
        break;
      case 0x2b: // '+'
        acc.append('+');
        model.min = 1;
        model.max = -1;
        break;
      default:
        reset();
      }
    return model;
  }

  /**
   * Parse a cp production.
   */
  private ContentParticle readContentParticle(StringBuffer acc)
    throws IOException, XMLStreamException
  {
    ContentParticle cp = new ContentParticle();
    if (tryRead('('))
      {
        acc.append('(');
        cp.content = readElements(acc);
      }
    else
      {
        String name = readNmtoken(true);
        acc.append(name);
        cp.content = name;
        mark(1);
        int c = readCh();
        switch (c)
          {
          case 0x3f: // '?'
            acc.append('?');
            cp.min = 0;
            cp.max = 1;
            break;
          case 0x2a: // '*'
            acc.append('*');
            cp.min = 0;
            cp.max = -1;
            break;
          case 0x2b: // '+'
            acc.append('+');
            cp.min = 1;
            cp.max = -1;
            break;
          default:
            reset();
          }
      }
    return cp;
  }

  /**
   * Parse an attribute-list definition.
   */
  private void readAttlistDecl()
    throws IOException, XMLStreamException
  {
    requireWhitespace();
    boolean saved = expandPE;
    expandPE = (inputStack.size() > 1);
    String elementName = readNmtoken(true);
    expandPE = saved;
    boolean white = tryWhitespace();
    while (!tryRead('>'))
      {
        if (!white)
          error("whitespace required before attribute definition");
        readAttDef(elementName);
        white = tryWhitespace();
      }
  }

  /**
   * Parse a single attribute definition.
   */
  private void readAttDef(String elementName)
    throws IOException, XMLStreamException
  {
    String name = readNmtoken(true);
    requireWhitespace();
    StringBuffer acc = new StringBuffer();
    HashSet values = new HashSet();
    String type = readAttType(acc, values);
    if (validating)
      {
        if ("ID".equals(type))
          {
            // VC: One ID per Element Type
            for (Iterator i = doctype.attlistIterator(elementName);
                 i.hasNext(); )
              {
                Map.Entry entry = (Map.Entry) i.next();
                AttributeDecl decl = (AttributeDecl) entry.getValue();
                if ("ID".equals(decl.type))
                  error("element types must not have more than one ID " +
                        "attribute");
              }
          }
        else if ("NOTATION".equals(type))
          {
            // VC: One Notation Per Element Type
            for (Iterator i = doctype.attlistIterator(elementName);
                 i.hasNext(); )
              {
                Map.Entry entry = (Map.Entry) i.next();
                AttributeDecl decl = (AttributeDecl) entry.getValue();
                if ("NOTATION".equals(decl.type))
                  error("element types must not have more than one NOTATION " +
                        "attribute");
              }
            // VC: No Notation on Empty Element
            ContentModel model = doctype.getElementModel(elementName);
            if (model != null && model.type == ContentModel.EMPTY)
              error("attributes of type NOTATION must not be declared on an " +
                    "element declared EMPTY");
          }
      }
    String enumer = null;
    if ("ENUMERATION".equals(type) || "NOTATION".equals(type))
      enumer = acc.toString();
    else
      values = null;
    requireWhitespace();
    readDefault(elementName, name, type, enumer, values);
  }

  /**
   * Parse an attribute type.
   */
  private String readAttType(StringBuffer acc, HashSet values)
    throws IOException, XMLStreamException
  {
    if (tryRead('('))
      {
        readEnumeration(false, acc, values);
        return "ENUMERATION";
      }
    else
      {
        String typeString = readNmtoken(true);
        if ("NOTATION".equals(typeString))
          {
            readNotationType(acc, values);
            return typeString;
          }
        else if ("CDATA".equals(typeString) ||
                 "ID".equals(typeString) ||
                 "IDREF".equals(typeString) ||
                 "IDREFS".equals(typeString) ||
                 "ENTITY".equals(typeString) ||
                 "ENTITIES".equals(typeString) ||
                 "NMTOKEN".equals(typeString) ||
                 "NMTOKENS".equals(typeString))
          return typeString;
        else
          {
            error("illegal attribute type", typeString);
            return null;
          }
      }
  }

  /**
   * Parse an enumeration.
   */
  private void readEnumeration(boolean isNames, StringBuffer acc,
                               HashSet values)
    throws IOException, XMLStreamException
  {
    acc.append('(');
    // first token
    skipWhitespace();
    String token = readNmtoken(isNames);
    acc.append(token);
    values.add(token);
    // subsequent tokens
    skipWhitespace();
    while (!tryRead(')'))
      {
        require('|');
        acc.append('|');
        skipWhitespace();
        token = readNmtoken(isNames);
        // VC: No Duplicate Tokens
        if (validating && values.contains(token))
          error("duplicate token", token);
        acc.append(token);
        values.add(token);
        skipWhitespace();
      }
    acc.append(')');
  }

  /**
   * Parse a notation type for an attribute.
   */
  private void readNotationType(StringBuffer acc, HashSet values)
    throws IOException, XMLStreamException
  {
    requireWhitespace();
    require('(');
    readEnumeration(true, acc, values);
  }

  /**
   * Parse the default value for an attribute.
   */
  private void readDefault(String elementName, String name,
                           String type, String enumeration, HashSet values)
    throws IOException, XMLStreamException
  {
    int valueType = ATTRIBUTE_DEFAULT_SPECIFIED;
    int flags = LIT_ATTRIBUTE;
    String value = null, defaultType = null;
    boolean saved = expandPE;
    
    if (!"CDATA".equals(type))
      flags |= LIT_NORMALIZE;

    expandPE = false;
    if (tryRead('#'))
      {
        if (tryRead("FIXED"))
          {
            defaultType = "#FIXED";
            valueType = ATTRIBUTE_DEFAULT_FIXED;
            requireWhitespace();
            value = readLiteral(flags, false);
          }
        else if (tryRead("REQUIRED"))
          {
            defaultType = "#REQUIRED";
            valueType = ATTRIBUTE_DEFAULT_REQUIRED;
          }
        else if (tryRead("IMPLIED"))
          {
            defaultType = "#IMPLIED";
            valueType = ATTRIBUTE_DEFAULT_IMPLIED;
          }
        else
          error("illegal keyword for attribute default value");
      }
    else
      value = readLiteral(flags, false);
    expandPE = saved;
    if (validating)
      {
        if ("ID".equals(type))
          {
            // VC: Attribute Default Value Syntactically Correct
            if (value != null && !isNmtoken(value, true))
              error("default value must match Name production", value);
            // VC: ID Attribute Default
            if (valueType != ATTRIBUTE_DEFAULT_REQUIRED &&
                valueType != ATTRIBUTE_DEFAULT_IMPLIED)
              error("ID attributes must have a declared default of " +
                    "#IMPLIED or #REQUIRED");
          }
        else if (value != null)
          {
            // VC: Attribute Default Value Syntactically Correct
            if ("IDREF".equals(type) || "ENTITY".equals(type))
              {
                if (!isNmtoken(value, true))
                  error("default value must match Name production", value);
              }
            else if ("IDREFS".equals(type) || "ENTITIES".equals(type))
              {
                StringTokenizer st = new StringTokenizer(value);
                while (st.hasMoreTokens())
                  {
                    String token = st.nextToken();
                    if (!isNmtoken(token, true))
                      error("default value must match Name production", token);
                  }
              }
            else if ("NMTOKEN".equals(type) || "ENUMERATION".equals(type))
              {
                if (!isNmtoken(value, false))
                  error("default value must match Nmtoken production", value);
              }
            else if ("NMTOKENS".equals(type))
              {
                StringTokenizer st = new StringTokenizer(value);
                while (st.hasMoreTokens())
                  {
                    String token = st.nextToken();
                    if (!isNmtoken(token, false))
                      error("default value must match Nmtoken production",
                            token);
                  }
              }
          }
      }
    // Register attribute def
    AttributeDecl attribute =
      new AttributeDecl(type, value, valueType, enumeration, values,
                        inputStack.size() != 1);
    doctype.addAttributeDecl(elementName, name, attribute);
  }

  /**
   * Parse the EntityDecl production.
   */
  private void readEntityDecl(boolean inExternalSubset)
    throws IOException, XMLStreamException
  {
    int flags = 0;
    // Check if parameter entity
    boolean peFlag = false;
    expandPE = false;
    requireWhitespace();
    if (tryRead('%'))
      {
        peFlag = true;
        requireWhitespace();
      }
    expandPE = true;
    // Read entity name
    String name = readNmtoken(true);
    if (name.indexOf(':') != -1)
      error("illegal character ':' in entity name", name);
    if (peFlag)
      name = "%" + name;
    requireWhitespace();
    mark(1);
    int c = readCh();
    reset();
    if (c == 0x22 || c == 0x27) // " | '
      {
        // Internal entity replacement text
        String value = readLiteral(flags | LIT_DISABLE_EREF, true);
        int ai = value.indexOf('&');
        while (ai != -1)
          {
            int sci = value.indexOf(';', ai);
            if (sci == -1)
              error("malformed reference in entity value", value);
            String ref = value.substring(ai + 1, sci);
            int[] cp = UnicodeReader.toCodePointArray(ref);
            if (cp.length == 0)
              error("malformed reference in entity value", value);
            if (cp[0] == 0x23) // #
              {
                if (cp.length == 1)
                  error("malformed reference in entity value", value);
                if (cp[1] == 0x78) // 'x'
                  {
                    if (cp.length == 2)
                      error("malformed reference in entity value", value);
                    for (int i = 2; i < cp.length; i++)
                      {
                        int x = cp[i];
                        if (x < 0x30 ||
                            (x > 0x39 && x < 0x41) ||
                            (x > 0x46 && x < 0x61) ||
                            x > 0x66)
                          error("malformed character reference in entity value",
                                value);
                      }
                  }
                else
                  {
                    for (int i = 1; i < cp.length; i++)
                      {
                        int x = cp[i];
                        if (x < 0x30 || x > 0x39)
                          error("malformed character reference in entity value",
                                value);
                      }
                  }
              }
            else
              {
                if (!isNameStartCharacter(cp[0], input.xml11))
                  error("malformed reference in entity value", value);
                for (int i = 1; i < cp.length; i++)
                  {
                    if (!isNameCharacter(cp[i], input.xml11))
                      error("malformed reference in entity value", value);
                  }
              }
            ai = value.indexOf('&', sci);
          }
        doctype.addEntityDecl(name, value, inExternalSubset);
      }
    else
      {
        ExternalIds ids = readExternalIds(false, false);
        // Check for NDATA
        boolean white = tryWhitespace();
        if (!peFlag && tryRead("NDATA"))
          {
            if (!white)
              error("whitespace required before NDATA");
            requireWhitespace();
            ids.notationName = readNmtoken(true);
          }
        doctype.addEntityDecl(name, ids, inExternalSubset);
      }
    // finish
    skipWhitespace();
    require('>');
  }

  /**
   * Parse the NotationDecl production.
   */
  private void readNotationDecl(boolean inExternalSubset)
    throws IOException, XMLStreamException
  {
    requireWhitespace();
    String notationName = readNmtoken(true);
    if (notationName.indexOf(':') != -1)
      error("illegal character ':' in notation name", notationName);
    if (validating)
      {
        // VC: Unique Notation Name
        ExternalIds notation = doctype.getNotation(notationName);
        if (notation != null)
          error("duplicate notation name", notationName);
      }
    requireWhitespace();
    ExternalIds ids = readExternalIds(true, false);
    ids.notationName = notationName;
    doctype.addNotationDecl(notationName, ids, inExternalSubset);
    skipWhitespace();
    require('>');
  }

  /**
   * Returns a tuple {publicId, systemId}.
   */
  private ExternalIds readExternalIds(boolean inNotation, boolean isSubset)
    throws IOException, XMLStreamException
  {
    int c;
    int flags = LIT_DISABLE_CREF | LIT_DISABLE_PE | LIT_DISABLE_EREF;
    ExternalIds ids = new ExternalIds();
    
    if (tryRead("PUBLIC"))
      {
        requireWhitespace();
        ids.publicId = readLiteral(LIT_NORMALIZE | LIT_PUBID | flags, false);
        if (inNotation)
          {
            skipWhitespace();
            mark(1);
            c = readCh();
            reset();
            if (c == 0x22 || c == 0x27) // " | '
              {
                String href = readLiteral(flags, false);
                ids.systemId = absolutize(input.systemId, href);
              }
          }
        else
          {
            requireWhitespace();
            String href = readLiteral(flags, false);
            ids.systemId = absolutize(input.systemId, href);
          }
        // Check valid URI characters
        for (int i = 0; i < ids.publicId.length(); i++)
          {
            char d = ids.publicId.charAt(i);
            if (d >= 'a' && d <= 'z')
              continue;
            if (d >= 'A' && d <= 'Z')
              continue;
            if (" \r\n0123456789-' ()+,./:=?;!*#@$_%".indexOf(d) != -1)
              continue;
            error("illegal PUBLIC id character",
                  "U+" + Integer.toHexString(d));
          }
      }
    else if (tryRead("SYSTEM"))
      {
        requireWhitespace();
        String href = readLiteral(flags, false);
        ids.systemId = absolutize(input.systemId, href);
      }
    else if (!isSubset)
      {
        error("missing SYSTEM or PUBLIC keyword");
      }
    if (ids.systemId != null && !inNotation)
      {
        if (ids.systemId.indexOf('#') != -1)
          error("SYSTEM id has a URI fragment", ids.systemId);
      }
    return ids;
  }

  /**
   * Parse the start of an element.
   * @return the state of the parser afterwards (EMPTY_ELEMENT or CONTENT)
   */
  private int readStartElement()
    throws IOException, XMLStreamException
  {
    // Read element name
    String elementName = readNmtoken(true);
    attrs.clear();
    // Push namespace context
    if (namespaceAware)
      {
        if (elementName.charAt(0) == ':' ||
            elementName.charAt(elementName.length() - 1) == ':')
          error("not a QName", elementName);
        namespaces.addFirst(new LinkedHashMap());
      }
    // Read element content
    boolean white = tryWhitespace();
    mark(1);
    int c = readCh();
    while (c != 0x2f && c != 0x3e) // '/' | '>'
      {
        // Read attribute
        reset();
        if (!white)
          error("need whitespace between attributes");
        readAttribute(elementName);
        white = tryWhitespace();
        mark(1);
        c = readCh();
      }
    // supply defaulted attributes
    if (doctype != null)
      {
        for (Iterator i = doctype.attlistIterator(elementName); i.hasNext(); )
          {
            Map.Entry entry = (Map.Entry) i.next();
            String attName = (String) entry.getKey();
            AttributeDecl decl = (AttributeDecl) entry.getValue();
            if (validating)
              {
                switch (decl.valueType)
                  {
                  case ATTRIBUTE_DEFAULT_REQUIRED:
                    // VC: Required Attribute
                    if (decl.value == null && !attributeSpecified(attName))
                      error("value for " + attName + " attribute is required");
                    break;
                  case ATTRIBUTE_DEFAULT_FIXED:
                    // VC: Fixed Attribute Default
                    for (Iterator j = attrs.iterator(); j.hasNext(); )
                      {
                        Attribute a = (Attribute) j.next();
                        if (attName.equals(a.name) &&
                            !decl.value.equals(a.value))
                          error("value for " + attName + " attribute must be " +
                                decl.value);
                      }
                    break;
                  }
              }
            if (namespaceAware && attName.equals("xmlns"))
              {
                LinkedHashMap ctx =
                  (LinkedHashMap) namespaces.getFirst();
                if (ctx.containsKey(XMLConstants.DEFAULT_NS_PREFIX))
                  continue; // namespace was specified
              }
            else if (namespaceAware && attName.startsWith("xmlns:"))
              {
                LinkedHashMap ctx =
                  (LinkedHashMap) namespaces.getFirst();
                if (ctx.containsKey(attName.substring(6)))
                  continue; // namespace was specified
              }
            else if (attributeSpecified(attName))
              continue;
            if (decl.value == null)
              continue;
            // VC: Standalone Document Declaration
            if (validating && decl.external && xmlStandalone == Boolean.TRUE)
              error("standalone must be 'no' if attributes inherit values " +
                    "from externally declared markup declarations");
            Attribute attr =
              new Attribute(attName, decl.type, false, decl.value);
            if (namespaceAware)
              {
                if (!addNamespace(attr))
                  attrs.add(attr);
              }
            else
              attrs.add(attr);
          }
      }
    if (baseAware)
      {
        String uri = getAttributeValue(XMLConstants.XML_NS_URI, "base");
        String base = getXMLBase();
        bases.addFirst(absolutize(base, uri));
      }
    if (namespaceAware)
      {
        // check prefix bindings
        int ci = elementName.indexOf(':');
        if (ci != -1)
          {
            String prefix = elementName.substring(0, ci);
            String uri = getNamespaceURI(prefix);
            if (uri == null)
              error("unbound element prefix", prefix);
            else if (input.xml11 && "".equals(uri))
              error("XML 1.1 unbound element prefix", prefix);
          }
        for (Iterator i = attrs.iterator(); i.hasNext(); )
          {
            Attribute attr = (Attribute) i.next();
            if (attr.prefix != null &&
                !XMLConstants.XMLNS_ATTRIBUTE.equals(attr.prefix))
              {
                String uri = getNamespaceURI(attr.prefix);
                if (uri == null)
                  error("unbound attribute prefix", attr.prefix);
                else if (input.xml11 && "".equals(uri))
                  error("XML 1.1 unbound attribute prefix", attr.prefix);
              }
          }
      }
    if (validating && doctype != null)
      {
        validateStartElement(elementName);
        currentContentModel = doctype.getElementModel(elementName);
        if (currentContentModel == null)
          error("no element declaration", elementName);
        validationStack.add(new LinkedList());
      }
    // make element name available for read
    buf.setLength(0);
    buf.append(elementName);
    // push element onto stack
    stack.addLast(elementName);
    switch (c)
      {
      case 0x3e: // '>'
        return CONTENT;
      case 0x2f: // '/'
        require('>');
        return EMPTY_ELEMENT;
      }
    return -1; // to satisfy compiler
  }

  /**
   * Indicates whether the specified attribute name was specified for the
   * current element.
   */
  private boolean attributeSpecified(String attName)
  {
    for (Iterator j = attrs.iterator(); j.hasNext(); )
      {
        Attribute a = (Attribute) j.next();
        if (attName.equals(a.name))
          return true;
      }
    return false;
  }

  /**
   * Parse an attribute.
   */
  private void readAttribute(String elementName)
    throws IOException, XMLStreamException
  {
    // Read attribute name
    String attributeName = readNmtoken(true);
    String type = getAttributeType(elementName, attributeName);
    readEq();
    // Read literal
    final int flags = LIT_ATTRIBUTE |  LIT_ENTITY_REF;
    String value = (type == null || "CDATA".equals(type)) ?
      readLiteral(flags, false) : readLiteral(flags | LIT_NORMALIZE, false);
    // add attribute event
    Attribute attr = this.new Attribute(attributeName, type, true, value);
    if (namespaceAware)
      {
        if (attributeName.charAt(0) == ':' ||
            attributeName.charAt(attributeName.length() - 1) == ':')
          error("not a QName", attributeName);
        else if (attributeName.equals("xmlns"))
          {
            LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
            if (ctx.containsKey(XMLConstants.DEFAULT_NS_PREFIX))
              error("duplicate default namespace");
          }
        else if (attributeName.startsWith("xmlns:"))
          {
            LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
            if (ctx.containsKey(attributeName.substring(6)))
              error("duplicate namespace", attributeName.substring(6));
          }
        else if (attrs.contains(attr))
          error("duplicate attribute", attributeName);
      }
    else if (attrs.contains(attr))
      error("duplicate attribute", attributeName);
    if (validating && doctype != null)
      {
        // VC: Attribute Value Type
        AttributeDecl decl =
          doctype.getAttributeDecl(elementName, attributeName);
        if (decl == null)
          error("attribute must be declared", attributeName);
        if ("ENUMERATION".equals(decl.type))
          {
            // VC: Enumeration
            if (!decl.values.contains(value))
              error("value does not match enumeration " + decl.enumeration,
                    value);
          }
        else if ("ID".equals(decl.type))
          {
            // VC: ID
            if (!isNmtoken(value, true))
              error("ID values must match the Name production");
            if (ids.contains(value))
              error("Duplicate ID", value);
            ids.add(value);
          }
        else if ("IDREF".equals(decl.type) || "IDREFS".equals(decl.type))
          {
            StringTokenizer st = new StringTokenizer(value);
            while (st.hasMoreTokens())
              {
                String token = st.nextToken();
                // VC: IDREF
                if (!isNmtoken(token, true))
                  error("IDREF values must match the Name production");
                idrefs.add(token);
              }
          }
        else if ("NMTOKEN".equals(decl.type) || "NMTOKENS".equals(decl.type))
          {
            StringTokenizer st = new StringTokenizer(value);
            while (st.hasMoreTokens())
              {
                String token = st.nextToken();
                // VC: Name Token
                if (!isNmtoken(token, false))
                  error("NMTOKEN values must match the Nmtoken production");
              }
          }
        else if ("ENTITY".equals(decl.type))
          {
            // VC: Entity Name
            if (!isNmtoken(value, true))
              error("ENTITY values must match the Name production");
            Object entity = doctype.getEntity(value);
            if (entity == null || !(entity instanceof ExternalIds) ||
                ((ExternalIds) entity).notationName == null)
              error("ENTITY values must match the name of an unparsed " +
                    "entity declared in the DTD");
          }
        else if ("NOTATION".equals(decl.type))
          {
            if (!decl.values.contains(value))
              error("NOTATION values must match a declared notation name",
                    value);
            // VC: Notation Attributes
            ExternalIds notation = doctype.getNotation(value);
            if (notation == null)
              error("NOTATION values must match the name of a notation " +
                    "declared in the DTD", value);
          }
      }
    if (namespaceAware)
      {
        if (!addNamespace(attr))
          attrs.add(attr);
      }
    else
      attrs.add(attr);
  }

  /**
   * Determines whether the specified attribute is a namespace declaration,
   * and adds it to the current namespace context if so. Returns false if
   * the attribute is an ordinary attribute.
   */
  private boolean addNamespace(Attribute attr)
    throws XMLStreamException
  {
    if ("xmlns".equals(attr.name))
      {
        LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
        if (ctx.get(XMLConstants.DEFAULT_NS_PREFIX) != null)
          error("Duplicate default namespace declaration");
        if (XMLConstants.XML_NS_URI.equals(attr.value))
          error("can't bind XML namespace");
        ctx.put(XMLConstants.DEFAULT_NS_PREFIX, attr.value);
        return true;
      }
    else if ("xmlns".equals(attr.prefix))
      {
        LinkedHashMap ctx = (LinkedHashMap) namespaces.getFirst();
        if (ctx.get(attr.localName) != null)
          error("Duplicate namespace declaration for prefix",
                attr.localName);
        if (XMLConstants.XML_NS_PREFIX.equals(attr.localName))
          {
            if (!XMLConstants.XML_NS_URI.equals(attr.value))
              error("can't redeclare xml prefix");
            else
              return false; // treat as attribute
          }
        if (XMLConstants.XML_NS_URI.equals(attr.value))
          error("can't bind non-xml prefix to XML namespace");
        if (XMLConstants.XMLNS_ATTRIBUTE.equals(attr.localName))
          error("can't redeclare xmlns prefix");
        if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(attr.value))
          error("can't bind non-xmlns prefix to XML Namespace namespace");
        if ("".equals(attr.value) && !input.xml11)
          error("illegal use of 1.1-style prefix unbinding in 1.0 document");
        ctx.put(attr.localName, attr.value);
        return true;
      }
    return false;
  }

  /**
   * Parse a closing tag.
   */
  private void readEndElement()
    throws IOException, XMLStreamException
  {
    // pop element off stack
    String expected = (String) stack.removeLast();
    require(expected);
    skipWhitespace();
    require('>');
    // Make element name available
    buf.setLength(0);
    buf.append(expected);
    if (validating && doctype != null)
      endElementValidationHook();
  }

  /**
   * Validate the end of an element.
   * Called on an end-element or empty element if validating.
   */
  private void endElementValidationHook()
    throws XMLStreamException
  {
    validateEndElement();
    validationStack.removeLast();
    if (stack.isEmpty())
      currentContentModel = null;
    else
      {
        String parent = (String) stack.getLast();
        currentContentModel = doctype.getElementModel(parent);
      }
  }

  /**
   * Parse a comment.
   */
  private void readComment(boolean inDTD)
    throws IOException, XMLStreamException
  {
    boolean saved = expandPE;
    expandPE = false;
    buf.setLength(0);
    readUntil(TEST_END_COMMENT);
    require('>');
    expandPE = saved;
    if (inDTD)
      doctype.addComment(buf.toString());
  }

  /**
   * Parse a processing instruction.
   */
  private void readPI(boolean inDTD)
    throws IOException, XMLStreamException
  {
    boolean saved = expandPE;
    expandPE = false;
    piTarget = readNmtoken(true);
    if (piTarget.indexOf(':') != -1)
      error("illegal character in PI target", new Character(':'));
    if ("xml".equalsIgnoreCase(piTarget))
      error("illegal PI target", piTarget);
    if (tryRead(TEST_END_PI))
      piData = null;
    else
      {
        if (!tryWhitespace())
          error("whitespace required between PI target and data");
        buf.setLength(0);
        readUntil(TEST_END_PI);
        piData = buf.toString();
      }
    expandPE = saved;
    if (inDTD)
      doctype.addPI(piTarget, piData);
  }

  /**
   * Parse an entity reference.
   */
  private void readReference()
    throws IOException, XMLStreamException
  {
    buf.setLength(0);
    String entityName = readNmtoken(true);
    require(';');
    buf.setLength(0);
    buf.append(entityName);
  }

  /**
   * Read an CDATA section.
   */
  private void readCDSect()
    throws IOException, XMLStreamException
  {
    buf.setLength(0);
    readUntil(TEST_END_CDATA);
  }

  /**
   * Read character data.
   * @return the type of text read (CHARACTERS or SPACE)
   */
  private int readCharData(String prefix)
    throws IOException, XMLStreamException
  {
    boolean white = true;
    buf.setLength(0);
    if (prefix != null)
      buf.append(prefix);
    boolean done = false;
    boolean entities = false;
    while (!done)
      {
        // Block read
        mark(tmpBuf.length);
        int len = read(tmpBuf, 0, tmpBuf.length);
        if (len == -1)
          {
            if (inputStack.size() > 1)
              {
                popInput();
                // report end-entity
                done = true;
              }
            else
              throw new EOFException();
          }
        for (int i = 0; i < len && !done; i++)
          {
            int c = tmpBuf[i];
            switch (c)
              {
              case 0x20:
              case 0x09:
              case 0x0a:
              case 0x0d:
                buf.append(Character.toChars(c));
                break; // whitespace
              case 0x26: // '&'
                reset();
                read(tmpBuf, 0, i);
                // character reference?
                mark(3);
                c = readCh(); // &
                c = readCh();
                if (c == 0x23) // '#'
                  {
                    mark(1);
                    c = readCh();
                    boolean hex = (c == 0x78); // 'x'
                    if (!hex)
                      reset();
                    char[] ch = readCharacterRef(hex ? 16 : 10);
                    buf.append(ch, 0, ch.length);
                    for (int j = 0; j < ch.length; j++)
                      {
                        switch (ch[j])
                          {
                          case 0x20:
                          case 0x09:
                          case 0x0a:
                          case 0x0d:
                            break; // whitespace
                          default:
                            white = false;
                          }
                      }
                  }
                else
                  {
                    // entity reference
                    reset();
                    c = readCh(); // &
                    String entityName = readNmtoken(true);
                    require(';');
                    String text =
                      (String) PREDEFINED_ENTITIES.get(entityName);
                    if (text != null)
                      buf.append(text);
                    else
                      {
                        pushInput("", "&" + entityName + ";", false, false);
                        done = true;
                        break;
                      }
                  }
                // continue processing
                i = -1;
                mark(tmpBuf.length);
                len = read(tmpBuf, 0, tmpBuf.length);
                if (len == -1)
                  {
                    if (inputStack.size() > 1)
                      {
                        popInput();
                        done = true;
                      }
                    else
                      throw new EOFException();
                  }
                entities = true;
                break; // end of text sequence
              case 0x3e: // '>'
                int l = buf.length();
                if (l > 1 &&
                    buf.charAt(l - 1) == ']' &&
                    buf.charAt(l - 2) == ']')
                  error("Character data may not contain unescaped ']]>'");
                buf.append(Character.toChars(c));
                break;
              case 0x3c: // '<'
                reset();
                // read i characters
                int count = 0, remaining = i;
                do
                  {
                    int r = read(tmpBuf, 0, remaining);
                    count += r;
                    remaining -= r;
                  }
                while (count < i);
                i = len;
                if (coalescing && tryRead(TEST_CDATA))
                  readUntil(TEST_END_CDATA); // read CDATA section into buf
                else
                  done = true; // end of text sequence
                break;
              default:
                if (input.xml11)
                  {
                    if (!isXML11Char(c) || isXML11RestrictedChar(c))
                      error("illegal XML 1.1 character",
                            "U+" + Integer.toHexString(c));
                  }
                else if (!isChar(c))
                  error("illegal XML character",
                        "U+" + Integer.toHexString(c));
                white = false;
                buf.append(Character.toChars(c));
              }
          }
        // if text buffer >= 2MB, return it as a chunk
        // to avoid excessive memory use
        if (buf.length() >= 2097152)
          done = true;
      }
    if (entities)
      normalizeCRLF(buf);
    return white ? XMLStreamConstants.SPACE : XMLStreamConstants.CHARACTERS;
  }

  /**
   * Expands the specified entity.
   */
  private void expandEntity(String name, boolean inAttr, boolean normalize)
    throws IOException, XMLStreamException
  {
    if (doctype != null)
      {
        Object value = doctype.getEntity(name);
        if (value != null)
          {
            if (xmlStandalone == Boolean.TRUE)
              {
                // VC: Standalone Document Declaration
                if (doctype.isEntityExternal(name))
                  error("reference to external entity in standalone document");
                else if (value instanceof ExternalIds)
                  {
                    ExternalIds ids = (ExternalIds) value;
                    if (ids.notationName != null &&
                        doctype.isNotationExternal(ids.notationName))
                      error("reference to external notation in " +
                            "standalone document");
                  }
              }
            if (value instanceof String)
              {
                String text = (String) value;
                if (inAttr && text.indexOf('<') != -1)
                  error("< in attribute value");
                pushInput(name, text, !inAttr, normalize);
              }
            else if (inAttr)
              error("reference to external entity in attribute value", name);
            else
              pushInput(name, (ExternalIds) value, !inAttr, normalize);
            return;
          }
      }
    error("reference to undeclared entity", name);
  }

  /**
   * Indicates whether the specified entity is unparsed.
   */
  private boolean isUnparsedEntity(String name)
  {
    if (doctype != null)
      {
        Object value = doctype.getEntity(name);
        if (value != null && value instanceof ExternalIds)
          return ((ExternalIds) value).notationName != null;
      }
    return false;
  }

  /**
   * Read an equals sign.
   */
  private void readEq()
    throws IOException, XMLStreamException
  { 
    skipWhitespace();
    require('=');
    skipWhitespace();
  }

  /**
   * Character read for reading literals.
   * @param recognizePEs whether to recognize parameter-entity references
   */
  private int literalReadCh(boolean recognizePEs)
    throws IOException, XMLStreamException
  {
    int c = recognizePEs ? readCh() : read();
    while (c == -1)
      {
        if (inputStack.size() > 1)
          {
            inputStack.removeLast();
            input = (Input) inputStack.getLast();
            // Don't issue end-entity
            c = recognizePEs ? readCh() : read();
          }
        else
          throw new EOFException();
      }
    return c;
  }

  /**
   * Read a string literal.
   */
  private String readLiteral(int flags, boolean recognizePEs)
    throws IOException, XMLStreamException
  {
    boolean saved = expandPE;
    int delim = readCh();
    if (delim != 0x27 && delim != 0x22)
      error("expected '\"' or \"'\"", "U+" + Integer.toHexString(delim));
    literalBuf.setLength(0);
    if ((flags & LIT_DISABLE_PE) != 0)
      expandPE = false;
    boolean entities = false;
    int inputStackSize = inputStack.size();
    do
      {
        int c = literalReadCh(recognizePEs);
        if (c == delim && inputStackSize == inputStack.size())
          break;
        switch (c)
          {
          case 0x0a:
          case 0x0d:
            if ((flags & (LIT_ATTRIBUTE | LIT_PUBID)) != 0)
              c = 0x20; // normalize to space
            break;
          case 0x09:
            if ((flags & LIT_ATTRIBUTE) != 0)
              c = 0x20; // normalize to space
            break;
          case 0x26: // '&'
            mark(2);
            c = readCh();
            if (c == 0x23) // '#'
              {
                if ((flags & LIT_DISABLE_CREF) != 0)
                  {
                    reset();
                    c = 0x26; // '&'
                  }
                else
                  {
                    mark(1);
                    c = readCh();
                    boolean hex = (c == 0x78); // 'x'
                    if (!hex)
                      reset();
                    char[] ref = readCharacterRef(hex ? 16 : 10);
                    for (int i = 0; i < ref.length; i++)
                      literalBuf.append(ref[i]);
                    entities = true;
                    continue;
                  }
              }
            else
              {
                if ((flags & LIT_DISABLE_EREF) != 0)
                  {
                    reset();
                    c = 0x26; // '&'
                  }
                else
                  {
                    reset();
                    String entityName = readNmtoken(true);
                    require(';');
                    String text =
                      (String) PREDEFINED_ENTITIES.get(entityName);
                    if (text != null)
                      literalBuf.append(text);
                    else
                      expandEntity(entityName,
                                   (flags & LIT_ATTRIBUTE) != 0,
                                   true);
                    entities = true;
                    continue;
                  }
              }
            break;
          case 0x3c: // '<'
            if ((flags & LIT_ATTRIBUTE) != 0)
              error("attribute values may not contain '<'");
            break;
          case -1:
            if (inputStack.size() > 1)
              {
                popInput();
                continue;
              }
            throw new EOFException();
          default:
            if ((c < 0x0020 || c > 0xfffd) ||
                (c >= 0xd800 && c < 0xdc00) ||
                (input.xml11 && (c >= 0x007f) &&
                 (c <= 0x009f) && (c != 0x0085)))
              error("illegal character", "U+" + Integer.toHexString(c));
          }
        literalBuf.append(Character.toChars(c));
      }
    while (true);
    expandPE = saved;
    if (entities)
      normalizeCRLF(literalBuf);
    if ((flags & LIT_NORMALIZE) > 0)
      literalBuf = normalize(literalBuf);
    return literalBuf.toString();
  }

  /**
   * Performs attribute-value normalization of the text buffer.
   * This discards leading and trailing whitespace, and replaces sequences
   * of whitespace with a single space.
   */
  private StringBuffer normalize(StringBuffer buf)
  {
    StringBuffer acc = new StringBuffer();
    int len = buf.length();
    int avState = 0;
    for (int i = 0; i < len; i++)
      {
        char c = buf.charAt(i);
        if (c == ' ')
          avState = (avState == 0) ? 0 : 1;
        else
          {
            if (avState == 1)
              acc.append(' ');
            acc.append(c);
            avState = 2;
          }
      }
    return acc;
  }

  /**
   * Replace any CR/LF pairs in the buffer with LF.
   * This may be necessary if combinations of CR or LF were declared as
   * (character) entity references in the input.
   */
  private void normalizeCRLF(StringBuffer buf)
  {
    int len = buf.length() - 1;
    for (int i = 0; i < len; i++)
      {
        char c = buf.charAt(i);
        if (c == '\r' && buf.charAt(i + 1) == '\n')
          {
            buf.deleteCharAt(i--);
            len--;
          }
      }
  }

  /**
   * Parse and expand a parameter entity reference.
   */
  private void expandPEReference()
    throws IOException, XMLStreamException
  {
    String name = readNmtoken(true, new StringBuffer());
    require(';');
    mark(1); // ensure we don't reset to before the semicolon
    if (doctype != null)
      {
        String entityName = "%" + name;
        Object entity = doctype.getEntity(entityName);
        if (entity != null)
          {
            if (xmlStandalone == Boolean.TRUE)
              {
                if (doctype.isEntityExternal(entityName))
                  error("reference to external parameter entity in " +
                        "standalone document");
              }
            if (entity instanceof String)
              {
                pushInput(name, (String) entity, false, input.normalize);
                //pushInput(name, " " + (String) entity + " ");
              }
            else
              {
                //pushInput("", " ");
                pushInput(name, (ExternalIds) entity, false, input.normalize);
                //pushInput("", " ");
              }
          }
        else
          error("reference to undeclared parameter entity", name);
      }
    else
      error("reference to parameter entity without doctype", name);
  }

  /**
   * Parse the digits in a character reference.
   * @param base the base of the digits (10 or 16)
   */
  private char[] readCharacterRef(int base)
    throws IOException, XMLStreamException
  {
    StringBuffer b = new StringBuffer();
    for (int c = readCh(); c != 0x3b && c != -1; c = readCh())
      b.append(Character.toChars(c));
    try
      {
        int ord = Integer.parseInt(b.toString(), base);
        if (input.xml11)
          {
            if (!isXML11Char(ord))
              error("illegal XML 1.1 character reference " +
                    "U+" + Integer.toHexString(ord));
          }
        else
          {
            if ((ord < 0x20 && !(ord == 0x0a || ord == 0x09 || ord == 0x0d))
                || (ord >= 0xd800 && ord <= 0xdfff)
                || ord == 0xfffe || ord == 0xffff
                || ord > 0x0010ffff)
              error("illegal XML character reference " +
                    "U+" + Integer.toHexString(ord));
          }
        return Character.toChars(ord);
      }
    catch (NumberFormatException e)
      {
        error("illegal characters in character reference", b.toString());
        return null;
      }
  }

  /**
   * Parses an NMTOKEN or Name production.
   * @param isName if a Name, otherwise an NMTOKEN
   */
  private String readNmtoken(boolean isName)
    throws IOException, XMLStreamException
  {
    return readNmtoken(isName, nmtokenBuf);
  }
  
  /**
   * Parses an NMTOKEN or Name production using the specified buffer.
   * @param isName if a Name, otherwise an NMTOKEN
   * @param buf the character buffer to use
   */
  private String readNmtoken(boolean isName, StringBuffer buf)
    throws IOException, XMLStreamException
  {
    buf.setLength(0);
    int c = readCh();
    if (isName)
      {
        if (!isNameStartCharacter(c, input.xml11))
          error("not a name start character",
                "U+" + Integer.toHexString(c));
      }
    else
      {
        if (!isNameCharacter(c, input.xml11))
          error("not a name character",
                "U+" + Integer.toHexString(c));
      }
    buf.append(Character.toChars(c));
    do
      {
        mark(1);
        c = readCh();
        switch (c)
          {
          case 0x25: // '%'
          case 0x3c: // '<'
          case 0x3e: // '>'
          case 0x26: // '&'
          case 0x2c: // ','
          case 0x7c: // '|'
          case 0x2a: // '*'
          case 0x2b: // '+'
          case 0x3f: // '?'
          case 0x29: // ')'
          case 0x3d: // '='
          case 0x27: // '\''
          case 0x22: // '"'
          case 0x5b: // '['
          case 0x20: // ' '
          case 0x09: // '\t'
          case 0x0a: // '\n'
          case 0x0d: // '\r'
          case 0x3b: // ';'
          case 0x2f: // '/'
          case -1:
            reset();
            return intern(buf.toString());
          default:
            if (!isNameCharacter(c, input.xml11))
              error("not a name character",
                    "U+" + Integer.toHexString(c));
            else
              buf.append(Character.toChars(c));
          }
      }
    while (true);
  }

  /**
   * Indicates whether the specified Unicode character is an XML 1.1 Char.
   */
  public static boolean isXML11Char(int c)
  {
    return ((c >= 0x0001 && c <= 0xD7FF) ||
            (c >= 0xE000 && c < 0xFFFE) ||
            (c >= 0x10000 && c <= 0x10FFFF));
  }

  /**
   * Indicates whether the specified Unicode character is an XML 1.1
   * RestrictedChar.
   */
  public static boolean isXML11RestrictedChar(int c)
  {
    return ((c >= 0x0001 && c <= 0x0008) ||
            (c >= 0x000B && c <= 0x000C) ||
            (c >= 0x000E && c <= 0x001F) ||
            (c >= 0x007F && c <= 0x0084) ||
            (c >= 0x0086 && c <= 0x009F));
  }

  /**
   * Indicates whether the specified text matches the Name or Nmtoken
   * production.
   */
  private boolean isNmtoken(String text, boolean isName)
  {
    try
      {
        int[] cp = UnicodeReader.toCodePointArray(text);
        if (cp.length == 0)
          return false;
        if (isName)
          {
            if (!isNameStartCharacter(cp[0], input.xml11))
              return false;
          }
        else
          {
            if (!isNameCharacter(cp[0], input.xml11))
              return false;
          }
        for (int i = 1; i < cp.length; i++)
          {
            if (!isNameCharacter(cp[i], input.xml11))
              return false;
          }
        return true;
      }
    catch (IOException e)
      {
        return false;
      }
  }

  /**
   * Indicates whether the specified Unicode character is a Name start
   * character.
   */
  public static boolean isNameStartCharacter(int c, boolean xml11)
  {
    if (xml11)
      return ((c >= 0x0041 && c <= 0x005a) ||
              (c >= 0x0061 && c <= 0x007a) ||
              c == 0x3a |
              c == 0x5f |
              (c >= 0xC0 && c <= 0xD6) ||
              (c >= 0xD8 && c <= 0xF6) ||
              (c >= 0xF8 && c <= 0x2FF) ||
              (c >= 0x370 && c <= 0x37D) ||
              (c >= 0x37F && c <= 0x1FFF) ||
              (c >= 0x200C && c <= 0x200D) ||
              (c >= 0x2070 && c <= 0x218F) ||
              (c >= 0x2C00 && c <= 0x2FEF) ||
              (c >= 0x3001 && c <= 0xD7FF) ||
              (c >= 0xF900 && c <= 0xFDCF) ||
              (c >= 0xFDF0 && c <= 0xFFFD) ||
              (c >= 0x10000 && c <= 0xEFFFF));
    else
      return (c == 0x5f || c == 0x3a || isLetter(c));
  }

  /**
   * Indicates whether the specified Unicode character is a Name non-initial
   * character.
   */
  public static boolean isNameCharacter(int c, boolean xml11)
  {
    if (xml11)
      return ((c >= 0x0041 && c <= 0x005a) ||
              (c >= 0x0061 && c <= 0x007a) ||
              (c >= 0x0030 && c <= 0x0039) ||
              c == 0x3a |
              c == 0x5f |
              c == 0x2d |
              c == 0x2e |
              c == 0xB7 |
              (c >= 0xC0 && c <= 0xD6) ||
              (c >= 0xD8 && c <= 0xF6) ||
              (c >= 0xF8 && c <= 0x2FF) ||
              (c >= 0x300 && c <= 0x37D) ||
              (c >= 0x37F && c <= 0x1FFF) ||
              (c >= 0x200C && c <= 0x200D) ||
              (c >= 0x203F && c <= 0x2040) ||
              (c >= 0x2070 && c <= 0x218F) ||
              (c >= 0x2C00 && c <= 0x2FEF) ||
              (c >= 0x3001 && c <= 0xD7FF) ||
              (c >= 0xF900 && c <= 0xFDCF) ||
              (c >= 0xFDF0 && c <= 0xFFFD) ||
              (c >= 0x10000 && c <= 0xEFFFF));
    else
      return (c == 0x2e || c == 0x2d || c == 0x5f || c == 0x3a ||
              isLetter(c) || isDigit(c) ||
              isCombiningChar(c) || isExtender(c));
  }

  /**
   * Indicates whether the specified Unicode character matches the Letter
   * production.
   */
  public static boolean isLetter(int c)
  {
    if ((c >= 0x0041 && c <= 0x005A) ||
        (c >= 0x0061 && c <= 0x007A) ||
        (c >= 0x00C0 && c <= 0x00D6) ||
        (c >= 0x00D8 && c <= 0x00F6) ||
        (c >= 0x00F8 && c <= 0x00FF) ||
        (c >= 0x0100 && c <= 0x0131) ||
        (c >= 0x0134 && c <= 0x013E) ||
        (c >= 0x0141 && c <= 0x0148) ||
        (c >= 0x014A && c <= 0x017E) ||
        (c >= 0x0180 && c <= 0x01C3) ||
        (c >= 0x01CD && c <= 0x01F0) ||
        (c >= 0x01F4 && c <= 0x01F5) ||
        (c >= 0x01FA && c <= 0x0217) ||
        (c >= 0x0250 && c <= 0x02A8) ||
        (c >= 0x02BB && c <= 0x02C1) ||
        c == 0x0386 ||
        (c >= 0x0388 && c <= 0x038A) ||
        c == 0x038C ||
        (c >= 0x038E && c <= 0x03A1) ||
        (c >= 0x03A3 && c <= 0x03CE) ||
        (c >= 0x03D0 && c <= 0x03D6) ||
        c == 0x03DA ||
      c == 0x03DC ||
        c == 0x03DE ||
        c == 0x03E0 ||
        (c >= 0x03E2 && c <= 0x03F3) ||
        (c >= 0x0401 && c <= 0x040C) ||
        (c >= 0x040E && c <= 0x044F) ||
        (c >= 0x0451 && c <= 0x045C) ||
        (c >= 0x045E && c <= 0x0481) ||
        (c >= 0x0490 && c <= 0x04C4) ||
        (c >= 0x04C7 && c <= 0x04C8) ||
        (c >= 0x04CB && c <= 0x04CC) ||
        (c >= 0x04D0 && c <= 0x04EB) ||
        (c >= 0x04EE && c <= 0x04F5) ||
        (c >= 0x04F8 && c <= 0x04F9) ||
        (c >= 0x0531 && c <= 0x0556) ||
        c == 0x0559 ||
        (c >= 0x0561 && c <= 0x0586) ||
        (c >= 0x05D0 && c <= 0x05EA) ||
        (c >= 0x05F0 && c <= 0x05F2) ||
        (c >= 0x0621 && c <= 0x063A) ||
        (c >= 0x0641 && c <= 0x064A) ||
        (c >= 0x0671 && c <= 0x06B7) ||
        (c >= 0x06BA && c <= 0x06BE) ||
        (c >= 0x06C0 && c <= 0x06CE) ||
        (c >= 0x06D0 && c <= 0x06D3) ||
        c == 0x06D5 ||
        (c >= 0x06E5 && c <= 0x06E6) ||
        (c >= 0x0905 && c <= 0x0939) ||
        c == 0x093D ||
        (c >= 0x0958 && c <= 0x0961) ||
        (c >= 0x0985 && c <= 0x098C) ||
        (c >= 0x098F && c <= 0x0990) ||
        (c >= 0x0993 && c <= 0x09A8) ||
        (c >= 0x09AA && c <= 0x09B0) ||
        c == 0x09B2 ||
        (c >= 0x09B6 && c <= 0x09B9) ||
        (c >= 0x09DC && c <= 0x09DD) ||
        (c >= 0x09DF && c <= 0x09E1) ||
        (c >= 0x09F0 && c <= 0x09F1) ||
        (c >= 0x0A05 && c <= 0x0A0A) ||
        (c >= 0x0A0F && c <= 0x0A10) ||
        (c >= 0x0A13 && c <= 0x0A28) ||
        (c >= 0x0A2A && c <= 0x0A30) ||
        (c >= 0x0A32 && c <= 0x0A33) ||
        (c >= 0x0A35 && c <= 0x0A36) ||
        (c >= 0x0A38 && c <= 0x0A39) ||
        (c >= 0x0A59 && c <= 0x0A5C) ||
        c == 0x0A5E ||
        (c >= 0x0A72 && c <= 0x0A74) ||
        (c >= 0x0A85 && c <= 0x0A8B) ||
        c == 0x0A8D ||
        (c >= 0x0A8F && c <= 0x0A91) ||
        (c >= 0x0A93 && c <= 0x0AA8) ||
        (c >= 0x0AAA && c <= 0x0AB0) ||
        (c >= 0x0AB2 && c <= 0x0AB3) ||
        (c >= 0x0AB5 && c <= 0x0AB9) ||
        c == 0x0ABD ||
        c == 0x0AE0 ||
        (c >= 0x0B05 && c <= 0x0B0C) ||
        (c >= 0x0B0F && c <= 0x0B10) ||
        (c >= 0x0B13 && c <= 0x0B28) ||
        (c >= 0x0B2A && c <= 0x0B30) ||
        (c >= 0x0B32 && c <= 0x0B33) ||
        (c >= 0x0B36 && c <= 0x0B39) ||
        c == 0x0B3D ||
        (c >= 0x0B5C && c <= 0x0B5D) ||
        (c >= 0x0B5F && c <= 0x0B61) ||
        (c >= 0x0B85 && c <= 0x0B8A) ||
        (c >= 0x0B8E && c <= 0x0B90) ||
        (c >= 0x0B92 && c <= 0x0B95) ||
        (c >= 0x0B99 && c <= 0x0B9A) ||
        c == 0x0B9C ||
        (c >= 0x0B9E && c <= 0x0B9F) ||
        (c >= 0x0BA3 && c <= 0x0BA4) ||
        (c >= 0x0BA8 && c <= 0x0BAA) ||
        (c >= 0x0BAE && c <= 0x0BB5) ||
        (c >= 0x0BB7 && c <= 0x0BB9) ||
        (c >= 0x0C05 && c <= 0x0C0C) ||
        (c >= 0x0C0E && c <= 0x0C10) ||
        (c >= 0x0C12 && c <= 0x0C28) ||
        (c >= 0x0C2A && c <= 0x0C33) ||
        (c >= 0x0C35 && c <= 0x0C39) ||
        (c >= 0x0C60 && c <= 0x0C61) ||
        (c >= 0x0C85 && c <= 0x0C8C) ||
        (c >= 0x0C8E && c <= 0x0C90) ||
        (c >= 0x0C92 && c <= 0x0CA8) ||
        (c >= 0x0CAA && c <= 0x0CB3) ||
        (c >= 0x0CB5 && c <= 0x0CB9) ||
        c == 0x0CDE ||
        (c >= 0x0CE0 && c <= 0x0CE1) ||
        (c >= 0x0D05 && c <= 0x0D0C) ||
        (c >= 0x0D0E && c <= 0x0D10) ||
        (c >= 0x0D12 && c <= 0x0D28) ||
        (c >= 0x0D2A && c <= 0x0D39) ||
        (c >= 0x0D60 && c <= 0x0D61) ||
        (c >= 0x0E01 && c <= 0x0E2E) ||
        c == 0x0E30 ||
        (c >= 0x0E32 && c <= 0x0E33) ||
        (c >= 0x0E40 && c <= 0x0E45) ||
        (c >= 0x0E81 && c <= 0x0E82) ||
        c == 0x0E84 ||
        (c >= 0x0E87 && c <= 0x0E88) ||
        c == 0x0E8A ||
        c == 0x0E8D ||
        (c >= 0x0E94 && c <= 0x0E97) ||
        (c >= 0x0E99 && c <= 0x0E9F) ||
        (c >= 0x0EA1 && c <= 0x0EA3) ||
        c == 0x0EA5 ||
        c == 0x0EA7 ||
        (c >= 0x0EAA && c <= 0x0EAB) ||
        (c >= 0x0EAD && c <= 0x0EAE) ||
        c == 0x0EB0 ||
        (c >= 0x0EB2 && c <= 0x0EB3) ||
        c == 0x0EBD ||
        (c >= 0x0EC0 && c <= 0x0EC4) ||
        (c >= 0x0F40 && c <= 0x0F47) ||
        (c >= 0x0F49 && c <= 0x0F69) ||
        (c >= 0x10A0 && c <= 0x10C5) ||
        (c >= 0x10D0 && c <= 0x10F6) ||
        c == 0x1100 ||
        (c >= 0x1102 && c <= 0x1103) ||
        (c >= 0x1105 && c <= 0x1107) ||
        c == 0x1109 ||
        (c >= 0x110B && c <= 0x110C) ||
        (c >= 0x110E && c <= 0x1112) ||
        c == 0x113C ||
        c == 0x113E ||
        c == 0x1140 ||
        c == 0x114C ||
        c == 0x114E ||
        c == 0x1150 ||
        (c >= 0x1154 && c <= 0x1155) ||
        c == 0x1159 ||
        (c >= 0x115F && c <= 0x1161) ||
        c == 0x1163 ||
        c == 0x1165 ||
        c == 0x1167 ||
        c == 0x1169 ||
        (c >= 0x116D && c <= 0x116E) ||
        (c >= 0x1172 && c <= 0x1173) ||
        c == 0x1175 ||
        c == 0x119E ||
        c == 0x11A8 ||
        c == 0x11AB ||
        (c >= 0x11AE && c <= 0x11AF) ||
        (c >= 0x11B7 && c <= 0x11B8) ||
        c == 0x11BA ||
        (c >= 0x11BC && c <= 0x11C2) ||
        c == 0x11EB ||
        c == 0x11F0 ||
        c == 0x11F9 ||
        (c >= 0x1E00 && c <= 0x1E9B) ||
        (c >= 0x1EA0 && c <= 0x1EF9) ||
        (c >= 0x1F00 && c <= 0x1F15) ||
        (c >= 0x1F18 && c <= 0x1F1D) ||
        (c >= 0x1F20 && c <= 0x1F45) ||
        (c >= 0x1F48 && c <= 0x1F4D) ||
        (c >= 0x1F50 && c <= 0x1F57) ||
        c == 0x1F59 ||
        c == 0x1F5B ||
        c == 0x1F5D ||
        (c >= 0x1F5F && c <= 0x1F7D) ||
        (c >= 0x1F80 && c <= 0x1FB4) ||
        (c >= 0x1FB6 && c <= 0x1FBC) ||
        c == 0x1FBE ||
        (c >= 0x1FC2 && c <= 0x1FC4) ||
        (c >= 0x1FC6 && c <= 0x1FCC) ||
        (c >= 0x1FD0 && c <= 0x1FD3) ||
        (c >= 0x1FD6 && c <= 0x1FDB) ||
        (c >= 0x1FE0 && c <= 0x1FEC) ||
        (c >= 0x1FF2 && c <= 0x1FF4) ||
        (c >= 0x1FF6 && c <= 0x1FFC) ||
        c == 0x2126 ||
        (c >= 0x212A && c <= 0x212B) ||
        c == 0x212E ||
        (c >= 0x2180 && c <= 0x2182) ||
        (c >= 0x3041 && c <= 0x3094) ||
        (c >= 0x30A1 && c <= 0x30FA) ||
        (c >= 0x3105 && c <= 0x312C) ||
        (c >= 0xAC00 && c <= 0xD7A3))
        return true; // BaseChar
    if ((c >= 0x4e00 && c <= 0x9fa5) ||
        c == 0x3007 ||
        (c >= 0x3021 && c <= 0x3029))
      return true; // Ideographic
    return false;
  }

  /**
   * Indicates whether the specified Unicode character matches the Digit
   * production.
   */
  public static boolean isDigit(int c)
  {
    return ((c >= 0x0030 && c <= 0x0039) ||
            (c >= 0x0660 && c <= 0x0669) ||
            (c >= 0x06F0 && c <= 0x06F9) ||
            (c >= 0x0966 && c <= 0x096F) ||
            (c >= 0x09E6 && c <= 0x09EF) ||
            (c >= 0x0A66 && c <= 0x0A6F) ||
            (c >= 0x0AE6 && c <= 0x0AEF) ||
            (c >= 0x0B66 && c <= 0x0B6F) ||
            (c >= 0x0BE7 && c <= 0x0BEF) ||
            (c >= 0x0C66 && c <= 0x0C6F) ||
            (c >= 0x0CE6 && c <= 0x0CEF) ||
            (c >= 0x0D66 && c <= 0x0D6F) ||
            (c >= 0x0E50 && c <= 0x0E59) ||
            (c >= 0x0ED0 && c <= 0x0ED9) ||
            (c >= 0x0F20 && c <= 0x0F29));
  }

  /**
   * Indicates whether the specified Unicode character matches the
   * CombiningChar production.
   */
  public static boolean isCombiningChar(int c)
  {
    return ((c >= 0x0300 && c <= 0x0345) ||
            (c >= 0x0360 && c <= 0x0361) ||
            (c >= 0x0483 && c <= 0x0486) ||
            (c >= 0x0591 && c <= 0x05A1) ||
            (c >= 0x05A3 && c <= 0x05B9) ||
            (c >= 0x05BB && c <= 0x05BD) ||
            c == 0x05BF ||
            (c >= 0x05C1 && c <= 0x05C2) ||
            c == 0x05C4 ||
            (c >= 0x064B && c <= 0x0652) ||
            c == 0x0670 ||
            (c >= 0x06D6 && c <= 0x06DC) ||
            (c >= 0x06DD && c <= 0x06DF) ||
            (c >= 0x06E0 && c <= 0x06E4) ||
            (c >= 0x06E7 && c <= 0x06E8) ||
            (c >= 0x06EA && c <= 0x06ED) ||
            (c >= 0x0901 && c <= 0x0903) ||
            c == 0x093C ||
            (c >= 0x093E && c <= 0x094C) ||
            c == 0x094D ||
            (c >= 0x0951 && c <= 0x0954) ||
            (c >= 0x0962 && c <= 0x0963) ||
            (c >= 0x0981 && c <= 0x0983) ||
            c == 0x09BC ||
            c == 0x09BE ||
            c == 0x09BF ||
            (c >= 0x09C0 && c <= 0x09C4) ||
            (c >= 0x09C7 && c <= 0x09C8) ||
            (c >= 0x09CB && c <= 0x09CD) ||
            c == 0x09D7 ||
            (c >= 0x09E2 && c <= 0x09E3) ||
            c == 0x0A02 ||
            c == 0x0A3C ||
            c == 0x0A3E ||
            c == 0x0A3F ||
            (c >= 0x0A40 && c <= 0x0A42) ||
            (c >= 0x0A47 && c <= 0x0A48) ||
            (c >= 0x0A4B && c <= 0x0A4D) ||
            (c >= 0x0A70 && c <= 0x0A71) ||
            (c >= 0x0A81 && c <= 0x0A83) ||
            c == 0x0ABC ||
            (c >= 0x0ABE && c <= 0x0AC5) ||
            (c >= 0x0AC7 && c <= 0x0AC9) ||
            (c >= 0x0ACB && c <= 0x0ACD) ||
            (c >= 0x0B01 && c <= 0x0B03) ||
            c == 0x0B3C ||
            (c >= 0x0B3E && c <= 0x0B43) ||
            (c >= 0x0B47 && c <= 0x0B48) ||
            (c >= 0x0B4B && c <= 0x0B4D) ||
            (c >= 0x0B56 && c <= 0x0B57) ||
            (c >= 0x0B82 && c <= 0x0B83) ||
            (c >= 0x0BBE && c <= 0x0BC2) ||
            (c >= 0x0BC6 && c <= 0x0BC8) ||
            (c >= 0x0BCA && c <= 0x0BCD) ||
            c == 0x0BD7 ||
            (c >= 0x0C01 && c <= 0x0C03) ||
            (c >= 0x0C3E && c <= 0x0C44) ||
            (c >= 0x0C46 && c <= 0x0C48) ||
            (c >= 0x0C4A && c <= 0x0C4D) ||
            (c >= 0x0C55 && c <= 0x0C56) ||
            (c >= 0x0C82 && c <= 0x0C83) ||
            (c >= 0x0CBE && c <= 0x0CC4) ||
            (c >= 0x0CC6 && c <= 0x0CC8) ||
            (c >= 0x0CCA && c <= 0x0CCD) ||
            (c >= 0x0CD5 && c <= 0x0CD6) ||
            (c >= 0x0D02 && c <= 0x0D03) ||
            (c >= 0x0D3E && c <= 0x0D43) ||
            (c >= 0x0D46 && c <= 0x0D48) ||
            (c >= 0x0D4A && c <= 0x0D4D) ||
            c == 0x0D57 ||
            c == 0x0E31 ||
            (c >= 0x0E34 && c <= 0x0E3A) ||
            (c >= 0x0E47 && c <= 0x0E4E) ||
            c == 0x0EB1 ||
            (c >= 0x0EB4 && c <= 0x0EB9) ||
            (c >= 0x0EBB && c <= 0x0EBC) ||
            (c >= 0x0EC8 && c <= 0x0ECD) ||
            (c >= 0x0F18 && c <= 0x0F19) ||
            c == 0x0F35 ||
            c == 0x0F37 ||
            c == 0x0F39 ||
            c == 0x0F3E ||
            c == 0x0F3F ||
            (c >= 0x0F71 && c <= 0x0F84) ||
            (c >= 0x0F86 && c <= 0x0F8B) ||
            (c >= 0x0F90 && c <= 0x0F95) ||
            c == 0x0F97 ||
            (c >= 0x0F99 && c <= 0x0FAD) ||
            (c >= 0x0FB1 && c <= 0x0FB7) ||
            c == 0x0FB9 ||
            (c >= 0x20D0 && c <= 0x20DC) ||
            c == 0x20E1 ||
            (c >= 0x302A && c <= 0x302F) ||
            c == 0x3099 ||
            c == 0x309A);
  }

  /**
   * Indicates whether the specified Unicode character matches the Extender
   * production.
   */
  public static boolean isExtender(int c)
  {
    return (c == 0x00B7 ||
            c == 0x02D0 ||
            c == 0x02D1 ||
            c == 0x0387 ||
            c == 0x0640 ||
            c == 0x0E46 ||
            c == 0x0EC6 ||
            c == 0x3005 ||
            (c >= 0x3031 && c <= 0x3035) ||
            (c >= 0x309D && c <= 0x309E) ||
            (c >= 0x30FC && c <= 0x30FE));
  }

  /**
   * Indicates whether the specified Unicode character matches the Char
   * production.
   */
  public static boolean isChar(int c)
  {
    return (c >= 0x20 && c < 0xd800) ||
      (c >= 0xe00 && c < 0xfffe) ||
      (c >= 0x10000 && c < 0x110000) ||
      c == 0xa || c == 0x9 || c == 0xd;
  }
  
  /**
   * Interns the specified text or not, depending on the value of
   * stringInterning.
   */
  private String intern(String text)
  {
    return stringInterning ? text.intern() : text;
  }

  /**
   * Report a parsing error.
   */
  private void error(String message)
    throws XMLStreamException
  {
    error(message, null);
  }
  
  /**
   * Report a parsing error.
   */
  private void error(String message, Object info)
    throws XMLStreamException
  {
    if (info != null)
      {
        if (info instanceof String)
          message += ": \"" + ((String) info) + "\"";
        else if (info instanceof Character)
          message += ": '" + ((Character) info) + "'";
      }
    throw new XMLStreamException(message);
  }

  /**
   * Perform validation of a start-element event.
   */
  private void validateStartElement(String elementName)
    throws XMLStreamException
  {
    if (currentContentModel == null)
      {
        // root element
        // VC: Root Element Type
        if (!elementName.equals(doctype.rootName))
          error("root element name must match name in DTD");
        return;
      }
    // VC: Element Valid
    switch (currentContentModel.type)
      {
      case ContentModel.EMPTY:
        error("child element found in empty element", elementName);
        break;
      case ContentModel.ELEMENT:
        LinkedList ctx = (LinkedList) validationStack.getLast();
        ctx.add(elementName);
        break;
      case ContentModel.MIXED:
        MixedContentModel mm = (MixedContentModel) currentContentModel;
        if (!mm.containsName(elementName))
          error("illegal element for content model", elementName);
        break;
      }
  }

  /**
   * Perform validation of an end-element event.
   */
  private void validateEndElement()
    throws XMLStreamException
  {
    if (currentContentModel == null)
      {
        // root element
        // VC: IDREF
        if (!idrefs.containsAll(ids))
          error("IDREF values must match the value of some ID attribute");
        return;
      }
    // VC: Element Valid
    switch (currentContentModel.type)
      {
      case ContentModel.ELEMENT:
        LinkedList ctx = (LinkedList) validationStack.getLast();
        ElementContentModel ecm = (ElementContentModel) currentContentModel;
        validateElementContent(ecm, ctx);
        break;
      }
  }

  /**
   * Perform validation of character data.
   */
  private void validatePCData(String text)
    throws XMLStreamException
  {
    // VC: Element Valid
    switch (currentContentModel.type)
      {
      case ContentModel.EMPTY:
        error("character data found in empty element", text);
        break;
      case ContentModel.ELEMENT:
        boolean white = true;
        int len = text.length();
        for (int i = 0; i < len; i++)
          {
            char c = text.charAt(i);
            if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
              {
                white = false;
                break;
              }
          }
        if (!white)
          error("character data found in element with element content", text);
        else if (xmlStandalone == Boolean.TRUE && currentContentModel.external)
          // VC: Standalone Document Declaration
          error("whitespace in element content of externally declared " +
                "element in standalone document");
        break;
      }
  }

  /**
   * Validates the specified validation context (list of child elements)
   * against the element content model for the current element.
   */
  private void validateElementContent(ElementContentModel model,
                                      LinkedList children)
    throws XMLStreamException
  {
    // Use regular expression
    StringBuffer buf = new StringBuffer();
    for (Iterator i = children.iterator(); i.hasNext(); )
      {
        buf.append((String) i.next());
        buf.append(' ');
      }
    String c = buf.toString();
    String regex = createRegularExpression(model);
    if (!c.matches(regex))
      error("element content "+model.text+" does not match expression "+regex, c);
  }

  /**
   * Creates the regular expression used to validate an element content
   * model.
   */
  private String createRegularExpression(ElementContentModel model)
  {
    if (model.regex == null)
      {
        StringBuffer buf = new StringBuffer();
        buf.append('(');
        for (Iterator i = model.contentParticles.iterator(); i.hasNext(); )
          {
            ContentParticle cp = (ContentParticle) i.next();
            if (cp.content instanceof String)
              {
                buf.append('(');
                buf.append((String) cp.content);
                buf.append(' ');
                buf.append(')');
                if (cp.max == -1)
                  {
                    if (cp.min == 0)
                      buf.append('*');
                    else
                      buf.append('+');
                  }
                else if (cp.min == 0)
                  buf.append('?');
              }
            else
              {
                ElementContentModel ecm = (ElementContentModel) cp.content;
                buf.append(createRegularExpression(ecm));
              }
            if (model.or && i.hasNext())
              buf.append('|');
          }
        buf.append(')');
        if (model.max == -1)
          {
            if (model.min == 0)
              buf.append('*');
            else
              buf.append('+');
          }
        else if (model.min == 0)
          buf.append('?');
        model.regex = buf.toString();
      }
    return model.regex;
  }

  /**
   * Performs validation of a document type declaration event.
   */
  void validateDoctype()
    throws XMLStreamException
  {
    for (Iterator i = doctype.entityIterator(); i.hasNext(); )
      {
        Map.Entry entry = (Map.Entry) i.next();
        Object entity = entry.getValue();
        if (entity instanceof ExternalIds)
          {
            ExternalIds ids = (ExternalIds) entity;
            if (ids.notationName != null)
              {
                // VC: Notation Declared
                ExternalIds notation = doctype.getNotation(ids.notationName);
                if (notation == null)
                  error("Notation name must match the declared name of a " +
                        "notation", ids.notationName);
              }
          }
      }
  }

  /**
   * Simple test harness for reading an XML file.
   * args[0] is the filename of the XML file
   * If args[1] is "-x", enable XInclude processing
   */
  public static void main(String[] args)
    throws Exception
  {
    boolean validating = false;
    boolean namespaceAware = false;
    boolean xIncludeAware = false;
    int pos = 0;
    while (pos < args.length && args[pos].startsWith("-"))
      {
        if ("-x".equals(args[pos]))
          xIncludeAware = true;
        else if ("-v".equals(args[pos]))
          validating = true;
        else if ("-n".equals(args[pos]))
          namespaceAware = true;
        pos++;
      }
    if (pos >= args.length)
      {
        System.out.println("Syntax: XMLParser [-n] [-v] [-x] <file> [<file2> [...]]");
        System.out.println("\t-n: use namespace aware mode");
        System.out.println("\t-v: use validating parser");
        System.out.println("\t-x: use XInclude aware mode");
        System.exit(2);
      }
    while (pos < args.length)
      {
        XMLParser p = new XMLParser(new java.io.FileInputStream(args[pos]),
                                    absolutize(null, args[pos]),
                                    validating, // validating
                                    namespaceAware, // namespaceAware
                                    true, // coalescing,
                                    true, // replaceERefs
                                    true, // externalEntities
                                    true, // supportDTD
                                    true, // baseAware
                                    true, // stringInterning
                                    true, // extendedEventTypes
                                    null,
                                    null);
        XMLStreamReader reader = p;
        if (xIncludeAware)
          reader = new XIncludeFilter(p, args[pos], true, true, true);
        try
          {
            int event;
            //do
            while (reader.hasNext())
              {
                event = reader.next();
                Location loc = reader.getLocation();
                System.out.print(loc.getLineNumber() + ":" + 
                                 loc.getColumnNumber() + " ");
                switch (event)
                  {
                  case XMLStreamConstants.START_DOCUMENT:
                    System.out.println("START_DOCUMENT version=" +
                                       reader.getVersion() +
                                       " encoding=" +
                                       reader.getEncoding());
                    break;
                  case XMLStreamConstants.END_DOCUMENT:
                    System.out.println("END_DOCUMENT");
                    break;
                  case XMLStreamConstants.START_ELEMENT:
                    System.out.println("START_ELEMENT " +
                                       reader.getName());
                    int l = reader.getNamespaceCount();
                    for (int i = 0; i < l; i++)
                      System.out.println("\tnamespace " +
                                         reader.getNamespacePrefix(i) + "='" +
                                         reader.getNamespaceURI(i)+"'");
                    l = reader.getAttributeCount();
                    for (int i = 0; i < l; i++)
                      System.out.println("\tattribute " +
                                         reader.getAttributeName(i) + "='" +
                                         reader.getAttributeValue(i) + "'");
                    break;
                  case XMLStreamConstants.END_ELEMENT:
                    System.out.println("END_ELEMENT " + reader.getName());
                    break;
                  case XMLStreamConstants.CHARACTERS:
                    System.out.println("CHARACTERS '" +
                                       encodeText(reader.getText()) + "'");
                    break;
                  case XMLStreamConstants.CDATA:
                    System.out.println("CDATA '" +
                                       encodeText(reader.getText()) + "'");
                    break;
                  case XMLStreamConstants.SPACE:
                    System.out.println("SPACE '" +
                                       encodeText(reader.getText()) + "'");
                    break;
                  case XMLStreamConstants.DTD:
                    System.out.println("DTD " + reader.getText());
                    break;
                  case XMLStreamConstants.ENTITY_REFERENCE:
                    System.out.println("ENTITY_REFERENCE " + reader.getText());
                    break;
                  case XMLStreamConstants.COMMENT:
                    System.out.println("COMMENT '" +
                                       encodeText(reader.getText()) + "'");
                    break;
                  case XMLStreamConstants.PROCESSING_INSTRUCTION:
                    System.out.println("PROCESSING_INSTRUCTION " +
                                       reader.getPITarget() + " " +
                                       reader.getPIData());
                    break;
                  case START_ENTITY:
                    System.out.println("START_ENTITY " + reader.getText());
                    break;
                  case END_ENTITY:
                    System.out.println("END_ENTITY " + reader.getText());
                    break;
                  default:
                    System.out.println("Unknown event: " + event);
                  }
              }
          }
        catch (XMLStreamException e)
          {
            Location l = reader.getLocation();
            System.out.println("At line "+l.getLineNumber()+
                               ", column "+l.getColumnNumber()+
                               " of "+l.getSystemId());
            throw e;
          }
        pos++;
      }
  }

  /**
   * Escapes control characters in the specified text. For debugging.
   */
  private static String encodeText(String text)
  {
    StringBuffer b = new StringBuffer();
    int len = text.length();
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        switch (c)
          {
          case '\t':
            b.append("\\t");
            break;
          case '\n':
            b.append("\\n");
            break;
          case '\r':
            b.append("\\r");
            break;
          default:
            b.append(c);
          }
      }
    return b.toString();
  }

  /**
   * An attribute instance.
   */
  class Attribute
  {

    /**
     * Attribute name.
     */
    final String name;

    /**
     * Attribute type as declared in the DTD, or CDATA otherwise.
     */
    final String type;

    /**
     * Whether the attribute was specified or defaulted.
     */
    final boolean specified;

    /**
     * The attribute value.
     */
    final String value;

    /**
     * The namespace prefix.
     */
    final String prefix;

    /**
     * The namespace local-name.
     */
    final String localName;

    Attribute(String name, String type, boolean specified, String value)
    {
      this.name = name;
      this.type = type;
      this.specified = specified;
      this.value = value;
      int ci = name.indexOf(':');
      if (ci == -1)
        {
          prefix = null;
          localName = intern(name);
        }
      else
        {
          prefix = intern(name.substring(0, ci));
          localName = intern(name.substring(ci + 1));
        }
    }

    public boolean equals(Object other)
    {
      if (other instanceof Attribute)
        {
          Attribute a = (Attribute) other;
          if (namespaceAware)
            {
              if (!a.localName.equals(localName))
                return false;
              String auri = getNamespaceURI(a.prefix);
              String uri = getNamespaceURI(prefix);
              if (uri == null && (auri == null ||
                                  (input.xml11 && "".equals(auri))))
               return true; 
              if (uri != null)
                {
                  if ("".equals(uri) && input.xml11 && "".equals(auri))
                    return true;
                  return uri.equals(auri);
                }
              return false;
            }
          else
            return a.name.equals(name);
        }
      return false;
    }

    public String toString()
    {
      StringBuffer buf = new StringBuffer(getClass().getName());
      buf.append('[');
      buf.append("name=");
      buf.append(name);
      if (value != null)
        {
          buf.append(",value=");
          buf.append(value);
        }
      if (type != null)
        {
          buf.append(",type=");
          buf.append(type);
        }
      if (specified)
        buf.append(",specified");
      buf.append(']');
      return buf.toString();
    }
    
  }

  /**
   * Representation of a DTD.
   */
  class Doctype
  {

    /**
     * Name of the root element.
     */
    final String rootName;

    /**
     * Public ID, if any, of external subset.
     */
    final String publicId;

    /**
     * System ID (URL), if any, of external subset.
     */
    final String systemId;

    /**
     * Map of element names to content models.
     */
    private final LinkedHashMap elements = new LinkedHashMap();

    /**
     * Map of element names to maps of attribute declarations.
     */
    private final LinkedHashMap attlists = new LinkedHashMap();

    /**
     * Map of entity names to entities (String or ExternalIds).
     */
    private final LinkedHashMap entities = new LinkedHashMap();

    /**
     * Map of notation names to ExternalIds.
     */
    private final LinkedHashMap notations = new LinkedHashMap();

    /**
     * Map of anonymous keys to comments.
     */    
    private final LinkedHashMap comments = new LinkedHashMap();

    /**
     * Map of anonymous keys to processing instructions (String[2]
     * containing {target, data}).
     */
    private final LinkedHashMap pis = new LinkedHashMap();

    /**
     * List of keys to all markup entries in the DTD.
     */
    private final LinkedList entries = new LinkedList();

    /**
     * Set of the entities defined in the external subset.
     */
    private final HashSet externalEntities = new HashSet();

    /**
     * Set of the notations defined in the external subset.
     */
    private final HashSet externalNotations = new HashSet();

    /**
     * Counter for making anonymous keys.
     */
    private int anon = 1;

    /**
     * Constructor.
     */
    Doctype(String rootName, String publicId, String systemId)
    {
      this.rootName = rootName;
      this.publicId = publicId;
      this.systemId = systemId;
    }

    /**
     * Adds an element declaration.
     * @param name the element name
     * @param text the content model text
     * @param model the parsed content model
     */
    void addElementDecl(String name, String text, ContentModel model)
    {
      if (elements.containsKey(name))
        return;
      model.text = text;
      model.external = (inputStack.size() != 1);
      elements.put(name, model);
      entries.add("E" + name);
    }

    /**
     * Adds an attribute declaration.
     * @param ename the element name
     * @param aname the attribute name
     * @param decl the attribute declaration details
     */
    void addAttributeDecl(String ename, String aname, AttributeDecl decl)
    {
      LinkedHashMap attlist = (LinkedHashMap) attlists.get(ename);
      if (attlist == null)
        {
          attlist = new LinkedHashMap();
          attlists.put(ename, attlist);
        }
      else if (attlist.containsKey(aname))
        return;
      attlist.put(aname, decl);
      String key = "A" + ename;
      if (!entries.contains(key))
        entries.add(key);
    }

    /**
     * Adds an entity declaration.
     * @param name the entity name
     * @param text the entity replacement text
     * @param inExternalSubset if we are in the exernal subset
     */
    void addEntityDecl(String name, String text, boolean inExternalSubset)
    {
      if (entities.containsKey(name))
        return;
      entities.put(name, text);
      entries.add("e" + name);
      if (inExternalSubset)
        externalEntities.add(name);
    }
    
    /**
     * Adds an entity declaration.
     * @param name the entity name
     * @param ids the external IDs
     * @param inExternalSubset if we are in the exernal subset
     */
    void addEntityDecl(String name, ExternalIds ids, boolean inExternalSubset)
    {
      if (entities.containsKey(name))
        return;
      entities.put(name, ids);
      entries.add("e" + name);
      if (inExternalSubset)
        externalEntities.add(name);
    }

    /**
     * Adds a notation declaration.
     * @param name the notation name
     * @param ids the external IDs
     * @param inExternalSubset if we are in the exernal subset
     */
    void addNotationDecl(String name, ExternalIds ids, boolean inExternalSubset)
    {
      if (notations.containsKey(name))
        return;
      notations.put(name, ids);
      entries.add("n" + name);
      if (inExternalSubset)
        externalNotations.add(name);
    }

    /**
     * Adds a comment.
     */
    void addComment(String text)
    {
      String key = Integer.toString(anon++);
      comments.put(key, text);
      entries.add("c" + key);
    }

    /**
     * Adds a processing instruction.
     */
    void addPI(String target, String data)
    {
      String key = Integer.toString(anon++);
      pis.put(key, new String[] {target, data});
      entries.add("p" + key);
    }

    /**
     * Returns the content model for the specified element.
     * @param name the element name
     */
    ContentModel getElementModel(String name)
    {
      return (ContentModel) elements.get(name);
    }

    /**
     * Returns the attribute definition for the given attribute
     * @param ename the element name
     * @param aname the attribute name
     */
    AttributeDecl getAttributeDecl(String ename, String aname)
    {
      LinkedHashMap attlist = (LinkedHashMap) attlists.get(ename);
      return (attlist == null) ? null : (AttributeDecl) attlist.get(aname);
    }

    /**
     * Indicates whether the specified attribute was declared in the DTD.
     * @param ename the element name
     * @param aname the attribute name
     */
    boolean isAttributeDeclared(String ename, String aname)
    {
      LinkedHashMap attlist = (LinkedHashMap) attlists.get(ename);
      return (attlist == null) ? false : attlist.containsKey(aname);
    }

    /**
     * Returns an iterator over the entries in the attribute list for the
     * given element.
     * @param ename the element name
     */
    Iterator attlistIterator(String ename)
    {
      LinkedHashMap attlist = (LinkedHashMap) attlists.get(ename);
      return (attlist == null) ? Collections.EMPTY_LIST.iterator() :
        attlist.entrySet().iterator();
    }

    /**
     * Returns the entity (String or ExternalIds) for the given entity name.
     */
    Object getEntity(String name)
    {
      return entities.get(name);
    }

    /**
     * Indicates whether the specified entity was declared in the external
     * subset.
     */
    boolean isEntityExternal(String name)
    {
      return externalEntities.contains(name);
    }

    /**
     * Returns an iterator over the entity map entries.
     */
    Iterator entityIterator()
    {
      return entities.entrySet().iterator();
    }

    /**
     * Returns the notation IDs for the given notation name.
     */
    ExternalIds getNotation(String name)
    {
      return (ExternalIds) notations.get(name);
    }

    /**
     * Indicates whether the specified notation was declared in the external
     * subset.
     */
    boolean isNotationExternal(String name)
    {
      return externalNotations.contains(name);
    }

    /**
     * Returns the comment associated with the specified (anonymous) key.
     */
    String getComment(String key)
    {
      return (String) comments.get(key);
    }

    /**
     * Returns the processing instruction associated with the specified
     * (anonymous) key.
     */
    String[] getPI(String key)
    {
      return (String[]) pis.get(key);
    }

    /**
     * Returns an iterator over the keys of the markup entries in this DTD,
     * in the order declared.
     */
    Iterator entryIterator()
    {
      return entries.iterator();
    }
    
  }

  /**
   * Combination of an ExternalID and an optional NDataDecl.
   */
  class ExternalIds
  {

    /**
     * The public ID.
     */
    String publicId;

    /**
     * The system ID.
     */
    String systemId;

    /**
     * The notation name declared with the NDATA keyword.
     */
    String notationName;
  }

  /**
   * A content model.
   */
  abstract class ContentModel
  {
    static final int EMPTY = 0;
    static final int ANY = 1;
    static final int ELEMENT = 2;
    static final int MIXED = 3;
    
    int min;
    int max;
    final int type;
    String text;
    boolean external;

    ContentModel(int type)
    {
      this.type = type;
      min = 1;
      max = 1;
    }
    
  }

  /**
   * The EMPTY content model.
   */
  class EmptyContentModel
    extends ContentModel
  {
    
    EmptyContentModel()
    {
      super(ContentModel.EMPTY);
      min = 0;
      max = 0;
    }
    
  }

  /**
   * The ANY content model.
   */
  class AnyContentModel
    extends ContentModel
  {
    
    AnyContentModel()
    {
      super(ContentModel.ANY);
      min = 0;
      max = -1;
    }
    
  }

  /**
   * An element content model.
   */
  class ElementContentModel
    extends ContentModel
  {

    LinkedList contentParticles;
    boolean or;
    String regex; // regular expression cache
    
    ElementContentModel()
    {
      super(ContentModel.ELEMENT);
      contentParticles = new LinkedList();
    }

    void addContentParticle(ContentParticle cp)
    {
      contentParticles.add(cp);
    }
    
  }

  class ContentParticle
  {

    int min = 1;
    int max = 1;
    Object content; // Name (String) or ElementContentModel
    
  }

  /**
   * A mixed content model.
   */
  class MixedContentModel
    extends ContentModel
  {

    private HashSet names;
    
    MixedContentModel()
    {
      super(ContentModel.MIXED);
      names = new HashSet();
    }

    void addName(String name)
    {
      names.add(name);
    }

    boolean containsName(String name)
    {
      return names.contains(name);
    }
    
  }

  /**
   * An attribute definition.
   */
  class AttributeDecl
  {
    
    /**
     * The attribute type (CDATA, ID, etc).
     */
    final String type;

    /**
     * The default value.
     */
    final String value;

    /**
     * The value type (#FIXED, #IMPLIED, etc).
     */
    final int valueType;

    /**
     * The enumeration text.
     */
    final String enumeration;

    /**
     * The enumeration tokens.
     */
    final HashSet values;

    /**
     * Whether this attribute declaration occurred in the external subset.
     */
    final boolean external;

    AttributeDecl(String type, String value,
                  int valueType, String enumeration,
                  HashSet values, boolean external)
    {
      this.type = type;
      this.value = value;
      this.valueType = valueType;
      this.enumeration = enumeration;
      this.values = values;
      this.external = external;
    }
    
  }

  /**
   * An XML input source.
   */
  static class Input
    implements Location
  {
    
    int line = 1, markLine;
    int column, markColumn;
    int offset, markOffset;
    final String publicId, systemId, name;
    final boolean report; // report start- and end-entity
    final boolean normalize; // normalize CR, etc to LF
    
    InputStream in;
    Reader reader;
    UnicodeReader unicodeReader;
    boolean initialized;
    boolean encodingDetected;
    String inputEncoding;
    boolean xml11;

    Input(InputStream in, Reader reader, String publicId, String systemId,
          String name, String inputEncoding, boolean report,
          boolean normalize)
    {
      if (inputEncoding == null)
        inputEncoding = "UTF-8";
      this.inputEncoding = inputEncoding;
      this.publicId = publicId;
      this.systemId = systemId;
      this.name = name;
      this.report = report;
      this.normalize = normalize;
      if (in != null)
        {
          if (reader != null)
            throw new IllegalStateException("both byte and char streams "+
                                            "specified");
          if (normalize)
            in = new CRLFInputStream(in);
          in = new BufferedInputStream(in);
          this.in = in;
        }
      else
        {
          this.reader = normalize ? new CRLFReader(reader) : reader;
          unicodeReader = new UnicodeReader(this.reader);
        }
      initialized = false;
    }

    // -- Location --
    
    public int getCharacterOffset()
    {
      return offset;
    }
    
    public int getColumnNumber()
    {
      return column;
    }

    public int getLineNumber()
    {
      return line;
    }

    public String getPublicId()
    {
      return publicId;
    }

    public String getSystemId()
    {
      return systemId;
    }

    void init()
      throws IOException
    {
      if (initialized)
        return;
      if (in != null)
        detectEncoding();
      initialized = true;
    }

    void mark(int len)
      throws IOException
    {
      markOffset = offset;
      markLine = line;
      markColumn = column;
      if (unicodeReader != null)
        unicodeReader.mark(len);
      else
        in.mark(len);
    }

    /**
     * Character read.
     */
    int read()
      throws IOException
    {
      offset++;
      int ret = (unicodeReader != null) ? unicodeReader.read() : in.read();
      if (normalize &&
          (ret == 0x0d || (xml11 && (ret == 0x85 || ret == 0x2028))))
        {
          // Normalize CR etc to LF
          ret = 0x0a;
        }
      // Locator handling
      if (ret == 0x0a)
        {
          line++;
          column = 0;
        }
      else
        column++;
      return ret;
    }

    /**
     * Block read.
     */
    int read(int[] b, int off, int len)
      throws IOException
    {
      int ret;
      if (unicodeReader != null)
        {
          ret = unicodeReader.read(b, off, len);
        }
      else
        {
          byte[] b2 = new byte[len];
          ret = in.read(b2, 0, len);
          if (ret != -1)
            {
              String s = new String(b2, 0, ret, inputEncoding);
              int[] c = UnicodeReader.toCodePointArray(s);
              ret = c.length;
              System.arraycopy(c, 0, b, off, ret);
            }
        }
      if (ret != -1)
        {
          // Locator handling
          for (int i = 0; i < ret; i++)
            {
              int c = b[off + i];
              if (normalize &&
                  (c == 0x0d || (xml11 && (c == 0x85 || c == 0x2028))))
                {
                  // Normalize CR etc to LF
                  c = 0x0a;
                  b[off + i] = c;
                }
              if (c == 0x0a)
                {
                  line++;
                  column = 0;
                }
              else
                column++;
            }
        }
      return ret;
    }

    void reset()
      throws IOException
    {
      if (unicodeReader != null)
        unicodeReader.reset();
      else
        in.reset();
      offset = markOffset;
      line = markLine;
      column = markColumn;
    }

    // Detection of input encoding
    
    private static final int[] SIGNATURE_UCS_4_1234 =
      new int[] { 0x00, 0x00, 0x00, 0x3c };
    private static final int[] SIGNATURE_UCS_4_4321 =
      new int[] { 0x3c, 0x00, 0x00, 0x00 };
    private static final int[] SIGNATURE_UCS_4_2143 =
      new int[] { 0x00, 0x00, 0x3c, 0x00 };
    private static final int[] SIGNATURE_UCS_4_3412 =
      new int[] { 0x00, 0x3c, 0x00, 0x00 };
    private static final int[] SIGNATURE_UCS_2_12 =
      new int[] { 0xfe, 0xff };
    private static final int[] SIGNATURE_UCS_2_21 =
      new int[] { 0xff, 0xfe };
    private static final int[] SIGNATURE_UCS_2_12_NOBOM =
      new int[] { 0x00, 0x3c, 0x00, 0x3f };
    private static final int[] SIGNATURE_UCS_2_21_NOBOM =
      new int[] { 0x3c, 0x00, 0x3f, 0x00 };
    private static final int[] SIGNATURE_UTF_8 =
      new int[] { 0x3c, 0x3f, 0x78, 0x6d };
    private static final int[] SIGNATURE_UTF_8_BOM =
      new int[] { 0xef, 0xbb, 0xbf };
    
    /**
     * Detect the input encoding.
     */
    private void detectEncoding()
      throws IOException
    {
      int[] signature = new int[4];
      in.mark(4);
      for (int i = 0; i < 4; i++)
        signature[i] = in.read();
      in.reset();

      // 4-byte encodings
      if (equals(SIGNATURE_UCS_4_1234, signature))
        {
          in.read();
          in.read();
          in.read();
          in.read();
          setInputEncoding("UTF-32BE");
          encodingDetected = true;
        }
      else if (equals(SIGNATURE_UCS_4_4321, signature))
        {
          in.read();
          in.read();
          in.read();
          in.read();
          setInputEncoding("UTF-32LE");
          encodingDetected = true;
        }
      else if (equals(SIGNATURE_UCS_4_2143, signature) ||
               equals(SIGNATURE_UCS_4_3412, signature))
        throw new UnsupportedEncodingException("unsupported UCS-4 byte ordering");
      
      // 2-byte encodings
      else if (equals(SIGNATURE_UCS_2_12, signature))
        {
          in.read();
          in.read();
          setInputEncoding("UTF-16BE");
          encodingDetected = true;
        }
      else if (equals(SIGNATURE_UCS_2_21, signature))
        {
          in.read();
          in.read();
          setInputEncoding("UTF-16LE");
          encodingDetected = true;
        }
      else if (equals(SIGNATURE_UCS_2_12_NOBOM, signature))
        {
          //setInputEncoding("UTF-16BE");
          throw new UnsupportedEncodingException("no byte-order mark for UCS-2 entity");
        }
      else if (equals(SIGNATURE_UCS_2_21_NOBOM, signature))
        {
          //setInputEncoding("UTF-16LE");
          throw new UnsupportedEncodingException("no byte-order mark for UCS-2 entity");
        }
      // ASCII-derived encodings
      else if (equals(SIGNATURE_UTF_8, signature))
        {
          // UTF-8 input encoding implied, TextDecl
        }
      else if (equals(SIGNATURE_UTF_8_BOM, signature))
        {
          in.read();
          in.read();
          in.read();
          setInputEncoding("UTF-8");
          encodingDetected = true;
        }
    }

    private static boolean equals(int[] b1, int[] b2)
    {
      for (int i = 0; i < b1.length; i++)
        {
          if (b1[i] != b2[i])
            return false;
        }
      return true;
    }
    
    void setInputEncoding(String encoding)
      throws IOException
    {
      if (encoding.equals(inputEncoding))
        return;
      if ("UTF-16".equalsIgnoreCase(encoding) &&
          inputEncoding.startsWith("UTF-16"))
        return;
      if (encodingDetected)
        throw new UnsupportedEncodingException("document is not in its " +
                                               "declared encoding " +
                                               inputEncoding +
                                               ": " + encoding);
      inputEncoding = encoding;
      finalizeEncoding();
    }

    void finalizeEncoding()
      throws IOException
    {
      if (reader != null)
        return;
      reader = new BufferedReader(new InputStreamReader(in, inputEncoding));
      unicodeReader = new UnicodeReader(reader);
      mark(1);
    }

  }

}

