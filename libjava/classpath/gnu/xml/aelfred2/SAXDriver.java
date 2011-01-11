/* SAXDriver.java --
   Copyright (C) 1999,2000,2001,2004 Free Software Foundation, Inc.

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

Portions derived from code which carried the following notice:

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

package gnu.xml.aelfred2;

import java.io.*;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Locale;
import java.util.Stack;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.xml.sax.*;
import org.xml.sax.ext.*;
import org.xml.sax.helpers.NamespaceSupport;


/**
 * An enhanced SAX2 version of Microstar's &AElig;lfred XML parser.
 * The enhancements primarily relate to significant improvements in
 * conformance to the XML specification, and SAX2 support.  Performance
 * has been improved.  See the package level documentation for more
 * information.
 *
 * <table border="1" width='100%' cellpadding='3' cellspacing='0'>
 * <tr bgcolor='#ccccff'>
 *      <th><font size='+1'>Name</font></th>
 *      <th><font size='+1'>Notes</font></th></tr>
 *
 * <tr><td colspan=2><center><em>Features ... URL prefix is
 * <b>http://xml.org/sax/features/</b></em></center></td></tr>
 *
 * <tr><td>(URL)/external-general-entities</td>
 *      <td>Value defaults to <em>true</em></td></tr>
 * <tr><td>(URL)/external-parameter-entities</td>
 *      <td>Value defaults to <em>true</em></td></tr>
 * <tr><td>(URL)/is-standalone</td>
 *      <td>(PRELIMINARY) Returns true iff the document's parsing
 *      has started (some non-error event after <em>startDocument()</em>
 *      was reported) and the document's standalone flag is set.</td></tr>
 * <tr><td>(URL)/namespace-prefixes</td>
 *      <td>Value defaults to <em>false</em> (but XML 1.0 names are
 *              always reported)</td></tr>
 * <tr><td>(URL)/lexical-handler/parameter-entities</td>
 *      <td>Value is fixed at <em>true</em></td></tr>
 * <tr><td>(URL)/namespaces</td>
 *      <td>Value defaults to <em>true</em></td></tr>
 * <tr><td>(URL)/resolve-dtd-uris</td>
 *      <td>(PRELIMINARY) Value defaults to <em>true</em></td></tr>
 * <tr><td>(URL)/string-interning</td>
 *      <td>Value is fixed at <em>true</em></td></tr>
 * <tr><td>(URL)/use-attributes2</td>
 *      <td>(PRELIMINARY) Value is fixed at <em>true</em></td></tr>
 * <tr><td>(URL)/use-entity-resolver2</td>
 *      <td>(PRELIMINARY) Value defaults to <em>true</em></td></tr>
 * <tr><td>(URL)/validation</td>
 *      <td>Value is fixed at <em>false</em></td></tr>
 *
 * <tr><td colspan=2><center><em>Handler Properties ... URL prefix is
 * <b>http://xml.org/sax/properties/</b></em></center></td></tr>
 *
 * <tr><td>(URL)/declaration-handler</td>
 *      <td>A declaration handler may be provided.  </td></tr>
 * <tr><td>(URL)/lexical-handler</td>
 *      <td>A lexical handler may be provided.  </td></tr>
 * </table>
 *
 * <p>This parser currently implements the SAX1 Parser API, but
 * it may not continue to do so in the future.
 *
 * @author Written by David Megginson (version 1.2a from Microstar)
 * @author Updated by David Brownell &lt;dbrownell@users.sourceforge.net&gt;
 * @see org.xml.sax.Parser
 */
final public class SAXDriver
  implements Locator, Attributes2, XMLReader, Parser, AttributeList
{

  private final DefaultHandler2 base = new DefaultHandler2();
  private XmlParser parser;

  private EntityResolver entityResolver = base;
  private EntityResolver2 resolver2 = null;
  private ContentHandler contentHandler = base;
  private DTDHandler dtdHandler = base;
  private ErrorHandler errorHandler = base;
  private DeclHandler declHandler = base;
  private LexicalHandler lexicalHandler = base;

  private String elementName;
  private Stack entityStack;

  // one vector (of object/struct): faster, smaller
  private List attributesList;

  private boolean namespaces = true;
  private boolean xmlNames = false;
  private boolean extGE = true;
  private boolean extPE = true;
  private boolean resolveAll = true;
  private boolean useResolver2 = true;

  // package private to allow (read-only) access in XmlParser
  boolean stringInterning = true;

  private int attributeCount;
  private boolean attributes;
  private String[] nsTemp;
  private NamespaceSupport prefixStack;

  //
  // Constructor.
  //

  /**
   * Constructs a SAX Parser.
   */
  public SAXDriver()
  {
    reset();
  }

  private void reset()
  {
    elementName = null;
    entityStack = new Stack();
    attributesList = Collections.synchronizedList(new ArrayList());
    attributeCount = 0;
    attributes = false;
    nsTemp = new String[3];
    prefixStack = null;
  }


  //
  // Implementation of org.xml.sax.Parser.
  //

  /**
   * <b>SAX1</b>: Sets the locale used for diagnostics; currently,
   * only locales using the English language are supported.
   * @param locale The locale for which diagnostics will be generated
   */
  public void setLocale(Locale locale)
    throws SAXException
  {
    if ("en".equals(locale.getLanguage()))
      {
        return;
      }
    throw new SAXException ("AElfred2 only supports English locales.");
  }

  /**
   * <b>SAX2</b>: Returns the object used when resolving external
   * entities during parsing (both general and parameter entities).
   */
  public EntityResolver getEntityResolver()
  {
    return (entityResolver == base) ? null : entityResolver;
  }

  /**
   * <b>SAX1, SAX2</b>: Set the entity resolver for this parser.
   * @param handler The object to receive entity events.
   */
  public void setEntityResolver(EntityResolver resolver)
  {
    if (resolver instanceof EntityResolver2)
      {
        resolver2 = (EntityResolver2) resolver;
      }
    else
      {
        resolver2 = null;
      }
    if (resolver == null)
      {
        resolver = base;
      }
    entityResolver = resolver;
  }

  /**
   * <b>SAX2</b>: Returns the object used to process declarations related
   * to notations and unparsed entities.
   */
  public DTDHandler getDTDHandler()
  {
    return (dtdHandler == base) ? null : dtdHandler;
  }

  /**
   * <b>SAX1, SAX2</b>: Set the DTD handler for this parser.
   * @param handler The object to receive DTD events.
   */
  public void setDTDHandler(DTDHandler handler)
  {
    if (handler == null)
      {
        handler = base;
      }
    this.dtdHandler = handler;
  }


  /**
   * <b>SAX1</b>: Set the document handler for this parser.  If a
   * content handler was set, this document handler will supplant it.
   * The parser is set to report all XML 1.0 names rather than to
   * filter out "xmlns" attributes (the "namespace-prefixes" feature
   * is set to true).
   *
   * @deprecated SAX2 programs should use the XMLReader interface
   *  and a ContentHandler.
   *
   * @param handler The object to receive document events.
   */
  public void setDocumentHandler(DocumentHandler handler)
  {
    contentHandler = new Adapter(handler);
    xmlNames = true;
  }

  /**
   * <b>SAX2</b>: Returns the object used to report the logical
   * content of an XML document.
   */
  public ContentHandler getContentHandler()
  {
    return (contentHandler == base) ? null : contentHandler;
  }

  /**
   * <b>SAX2</b>: Assigns the object used to report the logical
   * content of an XML document.  If a document handler was set,
   * this content handler will supplant it (but XML 1.0 style name
   * reporting may remain enabled).
   */
  public void setContentHandler(ContentHandler handler)
  {
    if (handler == null)
      {
        handler = base;
      }
    contentHandler = handler;
  }

  /**
   * <b>SAX1, SAX2</b>: Set the error handler for this parser.
   * @param handler The object to receive error events.
   */
  public void setErrorHandler(ErrorHandler handler)
  {
    if (handler == null)
      {
        handler = base;
      }
    this.errorHandler = handler;
  }

  /**
   * <b>SAX2</b>: Returns the object used to receive callbacks for XML
   * errors of all levels (fatal, nonfatal, warning); this is never null;
   */
  public ErrorHandler getErrorHandler()
  {
    return (errorHandler == base) ? null : errorHandler;
  }

  /**
   * <b>SAX1, SAX2</b>: Auxiliary API to parse an XML document, used mostly
   * when no URI is available.
   * If you want anything useful to happen, you should set
   * at least one type of handler.
   * @param source The XML input source.  Don't set 'encoding' unless
   *  you know for a fact that it's correct.
   * @see #setEntityResolver
   * @see #setDTDHandler
   * @see #setContentHandler
   * @see #setErrorHandler
   * @exception SAXException The handlers may throw any SAXException,
   *  and the parser normally throws SAXParseException objects.
   * @exception IOException IOExceptions are normally through through
   *  the parser if there are problems reading the source document.
   */
  public void parse(InputSource source)
    throws SAXException, IOException
  {
    synchronized (base)
      {
        parser = new XmlParser();
        if (namespaces)
          {
            prefixStack = new NamespaceSupport();
          }
        else if (!xmlNames)
          {
            throw new IllegalStateException();
          }
        parser.setHandler(this);

        try
          {
            Reader r = source.getCharacterStream();
            InputStream in = source.getByteStream();

            parser.doParse(source.getSystemId(),
                           source.getPublicId(),
                           r,
                           in,
                           source.getEncoding());
          }
        catch (SAXException e)
          {
            throw e;
          }
        catch (IOException e)
          {
            throw e;
          }
        catch (RuntimeException e)
          {
            throw e;
          }
        catch (Exception e)
          {
            throw new SAXParseException(e.getMessage(), this, e);
          }
        finally
          {
            contentHandler.endDocument();
            reset();
          }
      }
  }

  /**
   * <b>SAX1, SAX2</b>: Preferred API to parse an XML document, using a
   * system identifier (URI).
   */
  public void parse(String systemId)
    throws SAXException, IOException
  {
    parse(new InputSource(systemId));
  }

  //
  // Implementation of SAX2 "XMLReader" interface
  //
  static final String FEATURE = "http://xml.org/sax/features/";
  static final String PROPERTY = "http://xml.org/sax/properties/";

  /**
   * <b>SAX2</b>: Tells the value of the specified feature flag.
   *
   * @exception SAXNotRecognizedException thrown if the feature flag
   *  is neither built in, nor yet assigned.
   */
  public boolean getFeature(String featureId)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    if ((FEATURE + "validation").equals(featureId))
      {
        return false;
      }

    // external entities (both types) are optionally included
    if ((FEATURE + "external-general-entities").equals(featureId))
      {
        return extGE;
      }
    if ((FEATURE + "external-parameter-entities").equals(featureId))
      {
        return extPE;
      }

    // element/attribute names are as written in document; no mangling
    if ((FEATURE + "namespace-prefixes").equals(featureId))
      {
        return xmlNames;
      }

    // report element/attribute namespaces?
    if ((FEATURE + "namespaces").equals(featureId))
      {
        return namespaces;
      }

    // all PEs and GEs are reported
    if ((FEATURE + "lexical-handler/parameter-entities").equals(featureId))
      {
        return true;
      }

    // default is true
    if ((FEATURE + "string-interning").equals(featureId))
      {
        return stringInterning;
      }

    // EXTENSIONS 1.1

    // always returns isSpecified info
    if ((FEATURE + "use-attributes2").equals(featureId))
      {
        return true;
      }

    // meaningful between startDocument/endDocument
    if ((FEATURE + "is-standalone").equals(featureId))
      {
        if (parser == null)
          {
            throw new SAXNotSupportedException(featureId);
          }
        return parser.isStandalone();
      }

    // optionally don't absolutize URIs in declarations
    if ((FEATURE + "resolve-dtd-uris").equals(featureId))
      {
        return resolveAll;
      }

    // optionally use resolver2 interface methods, if possible
    if ((FEATURE + "use-entity-resolver2").equals(featureId))
      {
        return useResolver2;
      }

    throw new SAXNotRecognizedException(featureId);
  }

  // package private
  DeclHandler getDeclHandler()
  {
    return declHandler;
  }

  // package private
  boolean resolveURIs()
  {
    return resolveAll;
  }

  /**
   * <b>SAX2</b>:  Returns the specified property.
   *
   * @exception SAXNotRecognizedException thrown if the property value
   *  is neither built in, nor yet stored.
   */
  public Object getProperty(String propertyId)
    throws SAXNotRecognizedException
  {
    if ((PROPERTY + "declaration-handler").equals(propertyId))
      {
        return (declHandler == base) ? null : declHandler;
      }

    if ((PROPERTY + "lexical-handler").equals(propertyId))
      {
        return (lexicalHandler == base) ? null : lexicalHandler;
      }

    // unknown properties
    throw new SAXNotRecognizedException(propertyId);
  }

  /**
   * <b>SAX2</b>:  Sets the state of feature flags in this parser.  Some
   * built-in feature flags are mutable.
   */
  public void setFeature(String featureId, boolean value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    boolean state;

    // Features with a defined value, we just change it if we can.
    state = getFeature (featureId);

    if (state == value)
      {
        return;
      }
    if (parser != null)
      {
        throw new SAXNotSupportedException("not while parsing");
      }

    if ((FEATURE + "namespace-prefixes").equals(featureId))
      {
        // in this implementation, this only affects xmlns reporting
        xmlNames = value;
        // forcibly prevent illegal parser state
        if (!xmlNames)
          {
            namespaces = true;
          }
        return;
      }

    if ((FEATURE + "namespaces").equals(featureId))
      {
        namespaces = value;
        // forcibly prevent illegal parser state
        if (!namespaces)
          {
            xmlNames = true;
          }
        return;
      }

    if ((FEATURE + "external-general-entities").equals(featureId))
      {
        extGE = value;
        return;
      }
    if ((FEATURE + "external-parameter-entities").equals(featureId))
      {
        extPE = value;
        return;
      }
    if ((FEATURE + "resolve-dtd-uris").equals(featureId))
      {
        resolveAll = value;
        return;
      }

    if ((FEATURE + "use-entity-resolver2").equals(featureId))
      {
        useResolver2 = value;
        return;
      }

    throw new SAXNotRecognizedException(featureId);
  }

  /**
   * <b>SAX2</b>:  Assigns the specified property.  Like SAX1 handlers,
   * these may be changed at any time.
   */
  public void setProperty(String propertyId, Object value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    // see if the property is recognized
    getProperty(propertyId);

    // Properties with a defined value, we just change it if we can.

    if ((PROPERTY + "declaration-handler").equals(propertyId))
      {
        if (value == null)
          {
            declHandler = base;
          }
        else if (!(value instanceof DeclHandler))
          {
            throw new SAXNotSupportedException(propertyId);
          }
        else
          {
            declHandler = (DeclHandler) value;
          }
        return ;
      }

    if ((PROPERTY + "lexical-handler").equals(propertyId))
      {
        if (value == null)
          {
            lexicalHandler = base;
          }
        else if (!(value instanceof LexicalHandler))
          {
            throw new SAXNotSupportedException(propertyId);
          }
        else
          {
            lexicalHandler = (LexicalHandler) value;
          }
        return;
      }

    throw new SAXNotSupportedException(propertyId);
  }

  //
  // This is where the driver receives XmlParser callbacks and translates
  // them into SAX callbacks.  Some more callbacks have been added for
  // SAX2 support.
  //

  void startDocument()
    throws SAXException
  {
    contentHandler.setDocumentLocator(this);
    contentHandler.startDocument();
    attributesList.clear();
  }

  void skippedEntity(String name)
    throws SAXException
  {
    contentHandler.skippedEntity(name);
  }

  InputSource getExternalSubset(String name, String baseURI)
    throws SAXException, IOException
  {
    if (resolver2 == null || !useResolver2 || !extPE)
      {
        return null;
      }
    return resolver2.getExternalSubset(name, baseURI);
  }

  InputSource resolveEntity(boolean isPE, String name,
                            InputSource in, String baseURI)
    throws SAXException, IOException
  {
    InputSource  source;

    // external entities might be skipped
    if (isPE && !extPE)
      {
        return null;
      }
    if (!isPE && !extGE)
      {
        return null;
      }

    // ... or not
    lexicalHandler.startEntity(name);
    if (resolver2 != null && useResolver2)
      {
        source = resolver2.resolveEntity(name, in.getPublicId(),
                                         baseURI, in.getSystemId());
        if (source == null)
          {
            in.setSystemId(absolutize(baseURI,
                                      in.getSystemId(), false));
            source = in;
          }
      }
    else
      {
        in.setSystemId(absolutize(baseURI,
                                  in.getSystemId(),
                                  entityResolver != base));
        source = entityResolver.resolveEntity(in.getPublicId(),
                                              in.getSystemId());
        if (source == null)
          {
            source = in;
          }
      }
    startExternalEntity(name, source.getSystemId(), true);
    return source;
  }

  // absolutize a system ID relative to the specified base URI
  // (temporarily) package-visible for external entity decls
  String absolutize(String baseURI, String systemId, boolean nice)
    throws MalformedURLException, SAXException
  {
    // FIXME normalize system IDs -- when?
    // - Convert to UTF-8
    // - Map reserved and non-ASCII characters to %HH

    try
      {
        if (baseURI == null)
          {
            if (XmlParser.uriWarnings)
              {
                warn ("No base URI; hope this SYSTEM id is absolute: "
                      + systemId);
              }
            return new URL(systemId).toString();
          }
        else
          {
            return new URL(new URL(baseURI), systemId).toString();
          }
      }
    catch (MalformedURLException e)
      {
        // Let unknown URI schemes pass through unless we need
        // the JVM to map them to i/o streams for us...
        if (!nice)
          {
            throw e;
          }

        // sometimes sysids for notations or unparsed entities
        // aren't really URIs...
        warn("Can't absolutize SYSTEM id: " + e.getMessage());
        return systemId;
      }
  }

  void startExternalEntity(String name, String systemId, boolean stackOnly)
    throws SAXException
  {
    // The following warning was deleted because the application has the
    // option of not setting systemId. Sun's JAXP or Xerces seems to
    // ignore this case.
    /*
       if (systemId == null)
       warn ("URI was not reported to parser for entity " + name);
     */
    if (!stackOnly)  // spliced [dtd] needs startEntity
      {
        lexicalHandler.startEntity(name);
      }
    entityStack.push(systemId);
  }

  void endExternalEntity(String name)
    throws SAXException
  {
    if (!"[document]".equals(name))
      {
        lexicalHandler.endEntity(name);
      }
    entityStack.pop();
  }

  void startInternalEntity(String name)
    throws SAXException
  {
    lexicalHandler.startEntity(name);
  }

  void endInternalEntity(String name)
    throws SAXException
  {
    lexicalHandler.endEntity(name);
  }

  void doctypeDecl(String name, String publicId, String systemId)
    throws SAXException
  {
    lexicalHandler.startDTD(name, publicId, systemId);

    // ... the "name" is a declaration and should be given
    // to the DeclHandler (but sax2 doesn't).

    // the IDs for the external subset are lexical details,
    // as are the contents of the internal subset; but sax2
    // doesn't provide the internal subset "pre-parse"
  }

  void notationDecl(String name, String publicId, String systemId,
                    String baseUri)
    throws SAXException
  {
    try
      {
        dtdHandler.notationDecl(name, publicId,
                                (resolveAll && systemId != null)
                                ? absolutize(baseUri, systemId, true)
                                : systemId);
      }
    catch (IOException e)
      {
        // "can't happen"
        throw new SAXParseException(e.getMessage(), this, e);
      }
  }

  void unparsedEntityDecl(String name, String publicId, String systemId,
                          String baseUri, String notation)
    throws SAXException
  {
    try
      {
        dtdHandler.unparsedEntityDecl(name, publicId,
                                      resolveAll
                                      ? absolutize(baseUri, systemId, true)
                                      : systemId,
                                      notation);
      }
    catch (IOException e)
      {
        // "can't happen"
        throw new SAXParseException(e.getMessage(), this, e);
      }
  }

  void endDoctype()
    throws SAXException
  {
    lexicalHandler.endDTD();
  }

  private void declarePrefix(String prefix, String uri)
    throws SAXException
  {
    int index = uri.indexOf(':');

    // many versions of nwalsh docbook stylesheets
    // have bogus URLs; so this can't be an error...
    if (index < 1 && uri.length() != 0)
      {
        warn("relative URI for namespace: " + uri);
      }

    // FIXME:  char [0] must be ascii alpha; chars [1..index]
    // must be ascii alphanumeric or in "+-." [RFC 2396]

    //Namespace Constraints
    //name for xml prefix must be http://www.w3.org/XML/1998/namespace
    boolean prefixEquality = prefix.equals("xml");
    boolean uriEquality = uri.equals("http://www.w3.org/XML/1998/namespace");
    if ((prefixEquality || uriEquality) && !(prefixEquality && uriEquality))
      {
        fatal("xml is by definition bound to the namespace name " +
              "http://www.w3.org/XML/1998/namespace");
      }

    //xmlns prefix declaration is illegal but xml prefix declaration is llegal...
    if (prefixEquality && uriEquality)
      {
        return;
      }

    //name for xmlns prefix must be http://www.w3.org/2000/xmlns/
    prefixEquality = prefix.equals("xmlns");
    uriEquality = uri.equals("http://www.w3.org/2000/xmlns/");
    if ((prefixEquality || uriEquality) && !(prefixEquality && uriEquality))
      {
        fatal("http://www.w3.org/2000/xmlns/ is by definition bound" +
              " to prefix xmlns");
      }

    //even if the uri is http://www.w3.org/2000/xmlns/
    // it is illegal to declare it
    if (prefixEquality && uriEquality)
      {
        fatal ("declaring the xmlns prefix is illegal");
      }

    uri = uri.intern();
    prefixStack.declarePrefix(prefix, uri);
    contentHandler.startPrefixMapping(prefix, uri);
  }

  void attribute(String qname, String value, boolean isSpecified)
    throws SAXException
  {
    if (!attributes)
      {
        attributes = true;
        if (namespaces)
          {
            prefixStack.pushContext();
          }
      }

    // process namespace decls immediately;
    // then maybe forget this as an attribute
    if (namespaces)
      {
        int index;

        // default NS declaration?
        if (stringInterning)
          {
            if ("xmlns" == qname)
              {
                declarePrefix("", value);
                if (!xmlNames)
                  {
                    return;
                  }
              }
            // NS prefix declaration?
            else if ((index = qname.indexOf(':')) == 5
                     && qname.startsWith("xmlns"))
              {
                String prefix = qname.substring(6);

                if (prefix.equals(""))
                  {
                    fatal("missing prefix " +
                          "in namespace declaration attribute");
                  }
                if (value.length() == 0)
                  {
                    verror("missing URI in namespace declaration attribute: "
                           + qname);
                  }
                else
                  {
                    declarePrefix(prefix, value);
                  }
                if (!xmlNames)
                  {
                    return;
                  }
              }
          }
        else
          {
            if ("xmlns".equals(qname))
              {
                declarePrefix("", value);
                if (!xmlNames)
                  {
                    return;
                  }
              }
            // NS prefix declaration?
            else if ((index = qname.indexOf(':')) == 5
                     && qname.startsWith("xmlns"))
              {
                String prefix = qname.substring(6);

                if (value.length() == 0)
                  {
                    verror("missing URI in namespace decl attribute: "
                           + qname);
                  }
                else
                  {
                    declarePrefix(prefix, value);
                  }
                if (!xmlNames)
                  {
                    return;
                  }
              }
          }
      }
    // remember this attribute ...
    attributeCount++;

    // attribute type comes from querying parser's DTD records
    attributesList.add(new Attribute(qname, value, isSpecified));

  }

  void startElement(String elname)
    throws SAXException
  {
    ContentHandler handler = contentHandler;

    //
    // NOTE:  this implementation of namespace support adds something
    // like six percent to parsing CPU time, in a large (~50 MB)
    // document that doesn't use namespaces at all.  (Measured by PC
    // sampling, with a bug where endElement processing was omitted.)
    // [Measurement referred to older implementation, older JVM ...]
    //
    // It ought to become notably faster in such cases.  Most
    // costs are the prefix stack calling Hashtable.get() (2%),
    // String.hashCode() (1.5%) and about 1.3% each for pushing
    // the context, and two chunks of name processing.
    //

    if (!attributes)
      {
        if (namespaces)
          {
            prefixStack.pushContext();
          }
      }
    else if (namespaces)
      {

        // now we can patch up namespace refs; we saw all the
        // declarations, so now we'll do the Right Thing
        Iterator itt = attributesList.iterator();
        while (itt.hasNext())
          {
            Attribute attribute = (Attribute) itt.next();
            String qname = attribute.name;
            int index;

            // default NS declaration?
            if (stringInterning)
              {
                if ("xmlns" == qname)
                  {
                    continue;
                  }
              }
            else
              {
                if ("xmlns".equals(qname))
                  {
                    continue;
                  }
              }
            //Illegal in the new Namespaces Draft
            //should it be only in 1.1 docs??
            if (qname.equals (":"))
              {
                fatal("namespace names consisting of a single colon " +
                      "character are invalid");
              }
            index = qname.indexOf(':');

            // NS prefix declaration?
            if (index == 5 && qname.startsWith("xmlns"))
              {
                continue;
              }

            // it's not a NS decl; patch namespace info items
            if (prefixStack.processName(qname, nsTemp, true) == null)
              {
                fatal("undeclared attribute prefix in: " + qname);
              }
            else
              {
                attribute.nameSpace = nsTemp[0];
                attribute.localName = nsTemp[1];
              }
          }
      }

    // save element name so attribute callbacks work
    elementName = elname;
    if (namespaces)
      {
        if (prefixStack.processName(elname, nsTemp, false) == null)
          {
            fatal("undeclared element prefix in: " + elname);
            nsTemp[0] = nsTemp[1] = "";
          }
        handler.startElement(nsTemp[0], nsTemp[1], elname, this);
      }
    else
      {
        handler.startElement("", "", elname, this);
      }
    // elementName = null;

    // elements with no attributes are pretty common!
    if (attributes)
      {
        attributesList.clear();
        attributeCount = 0;
        attributes = false;
      }
  }

  void endElement(String elname)
    throws SAXException
  {
    ContentHandler handler = contentHandler;

    if (!namespaces)
      {
        handler.endElement("", "", elname);
        return;
      }
    prefixStack.processName(elname, nsTemp, false);
    handler.endElement(nsTemp[0], nsTemp[1], elname);

    Enumeration prefixes = prefixStack.getDeclaredPrefixes();

    while (prefixes.hasMoreElements())
      {
        handler.endPrefixMapping((String) prefixes.nextElement());
      }
    prefixStack.popContext();
  }

  void startCDATA()
    throws SAXException
  {
    lexicalHandler.startCDATA();
  }

  void charData(char[] ch, int start, int length)
    throws SAXException
  {
    contentHandler.characters(ch, start, length);
  }

  void endCDATA()
    throws SAXException
  {
    lexicalHandler.endCDATA();
  }

  void ignorableWhitespace(char[] ch, int start, int length)
    throws SAXException
  {
    contentHandler.ignorableWhitespace(ch, start, length);
  }

  void processingInstruction(String target, String data)
    throws SAXException
  {
    contentHandler.processingInstruction(target, data);
  }

  void comment(char[] ch, int start, int length)
    throws SAXException
  {
    if (lexicalHandler != base)
      {
        lexicalHandler.comment(ch, start, length);
      }
  }

  void fatal(String message)
    throws SAXException
  {
    SAXParseException fatal;

    fatal = new SAXParseException(message, this);
    errorHandler.fatalError(fatal);

    // Even if the application can continue ... we can't!
    throw fatal;
  }

  // We can safely report a few validity errors that
  // make layered SAX2 DTD validation more conformant
  void verror(String message)
    throws SAXException
  {
    SAXParseException err;

    err = new SAXParseException(message, this);
    errorHandler.error(err);
  }

  void warn(String message)
    throws SAXException
  {
    SAXParseException err;

    err = new SAXParseException(message, this);
    errorHandler.warning(err);
  }

  //
  // Implementation of org.xml.sax.Attributes.
  //

  /**
   * <b>SAX1 AttributeList, SAX2 Attributes</b> method
   * (don't invoke on parser);
   */
  public int getLength()
  {
    return attributesList.size();
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public String getURI(int index)
  {
    if (index < 0 || index >= attributesList.size())
      {
        return null;
      }
    return ((Attribute) attributesList.get(index)).nameSpace;
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public String getLocalName(int index)
  {
    if (index < 0 || index >= attributesList.size())
      {
        return null;
      }
    Attribute attr = (Attribute) attributesList.get(index);
    // FIXME attr.localName is sometimes null, why?
    if (namespaces && attr.localName == null)
      {
        // XXX fix this here for now
        int ci = attr.name.indexOf(':');
        attr.localName = (ci == -1) ? attr.name :
          attr.name.substring(ci + 1);
      }
    return (attr.localName == null) ? "" : attr.localName;
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public String getQName(int index)
  {
    if (index < 0 || index >= attributesList.size())
      {
      return null;
      }
    Attribute attr = (Attribute) attributesList.get(index);
    return (attr.name == null) ? "" : attr.name;
  }

  /**
   * <b>SAX1 AttributeList</b> method (don't invoke on parser);
   */
  public String getName(int index)
  {
    return getQName(index);
  }

  /**
   * <b>SAX1 AttributeList, SAX2 Attributes</b> method
   * (don't invoke on parser);
   */
  public String getType(int index)
  {
    if (index < 0 || index >= attributesList.size())
      {
        return null;
      }
    String type = parser.getAttributeType(elementName, getQName(index));
    if (type == null)
      {
        return "CDATA";
      }
    // ... use DeclHandler.attributeDecl to see enumerations
    if (type == "ENUMERATION")
      {
        return "NMTOKEN";
      }
    return type;
  }

  /**
   * <b>SAX1 AttributeList, SAX2 Attributes</b> method
   * (don't invoke on parser);
   */
  public String getValue(int index)
  {
    if (index < 0 || index >= attributesList.size())
      {
        return null;
      }
    return ((Attribute) attributesList.get(index)).value;
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public int getIndex(String uri, String local)
    {
      int length = getLength();

      for (int i = 0; i < length; i++)
        {
          if (!getURI(i).equals(uri))
            {
              continue;
            }
          if (getLocalName(i).equals(local))
            {
              return i;
            }
        }
      return -1;
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public int getIndex(String xmlName)
  {
    int length = getLength();

    for (int i = 0; i < length; i++)
      {
        if (getQName(i).equals(xmlName))
          {
            return i;
          }
      }
    return -1;
  }

  /**
   * <b>SAX2 Attributes</b> method (don't invoke on parser);
   */
  public String getType(String uri, String local)
  {
    int index = getIndex(uri, local);

    if (index < 0)
      {
        return null;
      }
    return getType(index);
  }

  /**
   * <b>SAX1 AttributeList, SAX2 Attributes</b> method
   * (don't invoke on parser);
   */
  public String getType(String xmlName)
  {
    int index = getIndex(xmlName);

    if (index < 0)
      {
        return null;
      }
    return getType(index);
  }

  /**
   * <b>SAX Attributes</b> method (don't invoke on parser);
   */
  public String getValue(String uri, String local)
  {
    int index = getIndex(uri, local);

    if (index < 0)
      {
        return null;
      }
    return getValue(index);
  }

  /**
   * <b>SAX1 AttributeList, SAX2 Attributes</b> method
   * (don't invoke on parser);
   */
  public String getValue(String xmlName)
  {
    int index = getIndex(xmlName);

    if (index < 0)
      {
        return null;
      }
    return getValue(index);
  }

  //
  // Implementation of org.xml.sax.ext.Attributes2
  //

  /** @return false unless the attribute was declared in the DTD.
   * @throws java.lang.ArrayIndexOutOfBoundsException
   *   When the supplied index does not identify an attribute.
   */
  public boolean isDeclared(int index)
  {
    if (index < 0 || index >= attributeCount)
      {
        throw new ArrayIndexOutOfBoundsException();
      }
    String type = parser.getAttributeType(elementName, getQName(index));
    return (type != null);
  }

  /** @return false unless the attribute was declared in the DTD.
   * @throws java.lang.IllegalArgumentException
   *   When the supplied names do not identify an attribute.
   */
  public boolean isDeclared(String qName)
  {
    int index = getIndex(qName);
    if (index < 0)
      {
        throw new IllegalArgumentException();
      }
    String type = parser.getAttributeType(elementName, qName);
    return (type != null);
  }

  /** @return false unless the attribute was declared in the DTD.
   * @throws java.lang.IllegalArgumentException
   *   When the supplied names do not identify an attribute.
   */
  public boolean isDeclared(String uri, String localName)
  {
    int index = getIndex(uri, localName);
    return isDeclared(index);
  }

  /**
   * <b>SAX-ext Attributes2</b> method (don't invoke on parser);
   */
  public boolean isSpecified(int index)
  {
    return ((Attribute) attributesList.get(index)).specified;
  }

  /**
   * <b>SAX-ext Attributes2</b> method (don't invoke on parser);
   */
  public boolean isSpecified(String uri, String local)
  {
    int index = getIndex (uri, local);
    return isSpecified(index);
  }

  /**
   * <b>SAX-ext Attributes2</b> method (don't invoke on parser);
   */
  public boolean isSpecified(String xmlName)
  {
    int index = getIndex (xmlName);
    return isSpecified(index);
  }

  //
  // Implementation of org.xml.sax.Locator.
  //

  /**
   * <b>SAX Locator</b> method (don't invoke on parser);
   */
  public String getPublicId()
  {
    return null;   // FIXME track public IDs too
  }

  /**
   * <b>SAX Locator</b> method (don't invoke on parser);
   */
  public String getSystemId()
  {
    if (entityStack.empty())
      {
        return null;
      }
    else
      {
        return (String) entityStack.peek();
      }
  }

  /**
   * <b>SAX Locator</b> method (don't invoke on parser);
   */
  public int getLineNumber()
  {
    return parser.getLineNumber();
  }

  /**
   * <b>SAX Locator</b> method (don't invoke on parser);
   */
  public int getColumnNumber()
  {
    return parser.getColumnNumber();
  }

  // adapter between SAX2 content handler and SAX1 document handler callbacks
  private static class Adapter
    implements ContentHandler
  {

    private DocumentHandler docHandler;

    Adapter(DocumentHandler dh)
    {
      docHandler = dh;
    }

    public void setDocumentLocator(Locator l)
    {
      docHandler.setDocumentLocator(l);
    }

    public void startDocument()
      throws SAXException
    {
      docHandler.startDocument();
    }

    public void processingInstruction(String target, String data)
      throws SAXException
    {
      docHandler.processingInstruction(target, data);
    }

    public void startPrefixMapping(String prefix, String uri)
    {
      /* ignored */
    }

    public void startElement(String namespace,
                             String local,
                             String name,
                             Attributes attrs)
      throws SAXException
    {
      docHandler.startElement(name, (AttributeList) attrs);
    }

    public void characters(char[] buf, int offset, int len)
      throws SAXException
    {
      docHandler.characters(buf, offset, len);
    }

    public void ignorableWhitespace(char[] buf, int offset, int len)
      throws SAXException
    {
      docHandler.ignorableWhitespace(buf, offset, len);
    }

    public void skippedEntity(String name)
    {
      /* ignored */
    }

    public void endElement(String u, String l, String name)
      throws SAXException
    {
      docHandler.endElement(name);
    }

    public void endPrefixMapping(String prefix)
    {
      /* ignored */
    }

    public void endDocument()
      throws SAXException
    {
      docHandler.endDocument();
    }
  }

  private static class Attribute
  {

    String name;
    String value;
    String nameSpace;
    String localName;
    boolean specified;

    Attribute(String name, String value, boolean specified)
    {
      this.name = name;
      this.value = value;
      this.nameSpace = "";
      this.specified = specified;
    }

  }

}
