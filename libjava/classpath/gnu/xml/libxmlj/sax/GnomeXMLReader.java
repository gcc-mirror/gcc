/* GnomeXMLReader.java - 
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

package gnu.xml.libxmlj.sax;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.LexicalHandler;

import gnu.xml.libxmlj.util.NamedInputStream;
import gnu.xml.libxmlj.util.StandaloneLocator;
import gnu.xml.libxmlj.util.XMLJ;

/**
 * A SAX2 parser that uses libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class GnomeXMLReader
implements XMLReader
{

  static
  {
    XMLJ.init ();
  }

  private static final String FEATURES_PREFIX =
    "http://xml.org/sax/features/";
  
  private static final List RECOGNIZED_FEATURES =
    Arrays.asList (new String[]
                   {
                   "external-general-entities",
                   "external-parameter-entities",
                   "is-standalone",
                   "lexical-handler/parameter-entities",
                   "namespaces",
                   "namespace-prefixes",
                   "resolve-dtd-uris",
                   "string-interning",
                   "use-attributes2",
                   "use-locator2",
                   "use-entity-resolver2",
                   "validation"
                   });
  
  private static final String PROPERTIES_PREFIX =
    "http://xml.org/sax/properties/";
  
  private static final List RECOGNIZED_PROPERTIES =
    Arrays.asList (new String[]
                   {
                   "declaration-handler",
                   "dom-node",
                   "lexical-handler",
                   "xml-string"
                   });

  // Features

  private transient boolean standalone;
  private boolean namespaces;
  private boolean namespacePrefixes;
  private boolean validation;

  // Callback handlers

  private ContentHandler contentHandler;

  private DTDHandler dtdHandler;

  private EntityResolver entityResolver;

  private ErrorHandler errorHandler;

  private DeclHandler declarationHandler;

  private LexicalHandler lexicalHandler;

  private GnomeLocator locator;

  // Namespace helper for handling callbacks
  private transient Namespaces ns;

  // If true, do not invoke callback methods except endDocument
  private transient boolean seenFatalError;

  private transient boolean seenStartDocument;

  private transient String base;

  public GnomeXMLReader ()
  {
    this (true, true);
  }

  public GnomeXMLReader (boolean namespaces, boolean validation)
  {
    this.namespaces = namespaces;
    this.validation = validation;
    ns = new Namespaces ();
  }

  public ContentHandler getContentHandler ()
  {
    return contentHandler;
  }

  public void setContentHandler (ContentHandler handler)
  {
    contentHandler = handler;
  }

  public DTDHandler getDTDHandler ()
  {
    return dtdHandler;
  }

  public void setDTDHandler (DTDHandler handler)
  {
    dtdHandler = handler;
  }

  public EntityResolver getEntityResolver ()
  {
    return entityResolver;
  }

  public void setEntityResolver (EntityResolver resolver)
  {
    entityResolver = resolver;
  }

  public ErrorHandler getErrorHandler ()
  {
    return errorHandler;
  }

  public void setErrorHandler (ErrorHandler handler)
  {
    errorHandler = handler;
  }

  // Features

  public boolean getFeature (String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    checkFeatureName (name);
    String key = name.substring (FEATURES_PREFIX.length ());
    if ("external-general-entities".equals (key))
      {
        return validation; // TODO check this
      }
    else if ("external-parameter-entities".equals (key))
      {
        return validation; // TODO check this
      }
    else if ("is-standalone".equals (key))
      {
        return standalone;
      }
    else if ("namespaces".equals (key))
      {
        return namespaces;
      }
    else if ("namespace-prefixes".equals (key))
      {
        return namespacePrefixes;
      }
    else if ("resolve-dtd-uris".equals (key))
      {
        return true;
      }
    else if ("validation".equals (key))
      {
        return validation;
      }
    else
      {
        return false;
      }
  }

  public void setFeature (String name, boolean value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    checkFeatureName (name);
    String key = name.substring (FEATURES_PREFIX.length ());
    if ("namespaces".equals (key))
      {
        namespaces = value;
      }
    else if ("namespace-prefixes".equals (key))
      {
        namespacePrefixes = value;
      }
    else if ("validation".equals (key))
      {
        validation = value;
      }
  }

  /**
   * Check that the specified feature name is recognized.
   */
  static void checkFeatureName (String name)
    throws SAXNotRecognizedException
  {
    if (name == null || !name.startsWith (FEATURES_PREFIX))
      {
        throw new SAXNotRecognizedException (name);
      }
    String key = name.substring (FEATURES_PREFIX.length ());
    if (!RECOGNIZED_FEATURES.contains (key))
      {
        throw new SAXNotRecognizedException (name);
      }
  }

  // Properties

  public Object getProperty (String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    checkPropertyName (name);
    String key = name.substring (PROPERTIES_PREFIX.length ());
    if ("declaration-handler".equals (key))
      {
        return getDeclarationHandler ();
      }
    else if ("lexical-handler".equals (key))
      {
        return getLexicalHandler ();
      }
    else
      {
        throw new SAXNotSupportedException (name);
      }
  }

  public void setProperty (String name, Object value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    checkPropertyName (name);
    String key = name.substring (PROPERTIES_PREFIX.length ());
    if ("declaration-handler".equals (key))
      {
        setDeclarationHandler ((DeclHandler) value);
      }
    else if ("lexical-handler".equals (key))
      {
        setLexicalHandler ((LexicalHandler) value);
      }
  }

  public DeclHandler getDeclarationHandler ()
  {
    return declarationHandler;
  }

  public void setDeclarationHandler (DeclHandler declarationHandler)
  {
    this.declarationHandler = declarationHandler;
  }

  public LexicalHandler getLexicalHandler ()
  {
    return lexicalHandler;
  }

  public void setLexicalHandler (LexicalHandler lexicalHandler)
  {
    this.lexicalHandler = lexicalHandler;
  }

  /**
   * Check that the specified property name is recognized.
   */
  static void checkPropertyName (String name)
    throws SAXNotRecognizedException
  {
    if (!name.startsWith (PROPERTIES_PREFIX))
      {
        throw new SAXNotRecognizedException (name);
      }
    String key = name.substring (PROPERTIES_PREFIX.length ());
    if (!RECOGNIZED_PROPERTIES.contains (key))
      {
        throw new SAXNotRecognizedException (name);
      }
  }

  // Parse

  public void parse (String systemId)
    throws IOException, SAXException
  {
    URL url = null;
    try
      {
        url = new URL (systemId);
      }
    catch (MalformedURLException e)
      {
        File file = new File(systemId);
        if (!file.exists ())
          {
            throw new FileNotFoundException (systemId);
          }
        String path  = file.getAbsolutePath();
        if (File.separatorChar != '/')
          {
            path = path.replace (File.separatorChar, '/');
          }
        if (!path.startsWith ("/"))
          {
            path = "/" + path;
          }
        if (!path.endsWith ("/") && file.isDirectory ())
          {
            path = path + "/";
          }
        url = new URL ("file:" + path);
      }
    InputSource source = new InputSource(url.toString ());
    source.setByteStream (url.openStream ());
    parse (source);
  }

  public synchronized void parse (InputSource input)
    throws IOException, SAXException
  {
    NamedInputStream in = XMLJ.getInputStream (input);
    byte[] detectBuffer = in.getDetectBuffer ();
    String publicId = input.getPublicId ();
    String systemId = input.getSystemId ();
    base = XMLJ.getBaseURI (systemId);
    // Reset state
    standalone = false;
    seenFatalError = false;
    seenStartDocument = false;
    if (systemId != null)
      {
        int lsi = systemId.lastIndexOf ('/');
        if (lsi != -1)
          {
            base = systemId.substring (0, lsi + 1);
          }
      }
    // Handle zero-length document
    if (detectBuffer == null)
      {
        startDocument (true);
        fatalError ("No document element", 0, 0, publicId, systemId);
        endDocument ();
        return;
      }
    // Parse
    parseStream(in,
                detectBuffer,
                publicId,
                systemId,
                base,
                validation,
                contentHandler != null,
                dtdHandler != null,
                entityResolver != null,
                errorHandler != null,
                declarationHandler != null,
                lexicalHandler != null);
    in.close ();
  }

  native void parseStream (InputStream in,
                           byte[] detectBuffer,
                           String publicId,
                           String systemId,
                           String base,
                           boolean validate,
                           boolean contentHandler,
                           boolean dtdHandler,
                           boolean entityResolver,
                           boolean errorHandler,
                           boolean declarationHandler,
                           boolean lexicalHandler)
    throws IOException, SAXException;

  String getURI (String prefix)
  {
    if (!namespaces)
      {
        return null;
      }
    return ns.getURI (prefix);
  }

  // Callbacks from libxmlj

  private void startDTD (String name, String publicId, String systemId)
    throws SAXException
  {
    if (seenFatalError || lexicalHandler == null)
      {
        return;
      }
    try
      {
        systemId = XMLJ.getAbsoluteURI (base, systemId);
        lexicalHandler.startDTD (name, publicId, systemId);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void externalEntityDecl (String name, String publicId,
                                   String systemId)
    throws SAXException
  {
    if (seenFatalError || declarationHandler == null)
      {
        return;
      }
    try
      {
        systemId = XMLJ.getAbsoluteURI (base, systemId);
        declarationHandler.externalEntityDecl (name, publicId, systemId);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void internalEntityDecl (String name, String value)
    throws SAXException
  {
    if (seenFatalError || declarationHandler == null)
      {
        return;
      }
    try
      {
        declarationHandler.internalEntityDecl (name, value);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private InputStream resolveEntity (String publicId, String systemId)
    throws SAXException, IOException
  {
    if (entityResolver == null)
      {
        return null;
      }
    try
      {
        systemId = XMLJ.getAbsoluteURI (base, systemId);
        InputSource source = entityResolver.resolveEntity (publicId, systemId);
        return (source == null) ? null : XMLJ.getInputStream (source);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void notationDecl (String name, String publicId, String systemId)
    throws SAXException
  {
    if (seenFatalError || dtdHandler == null)
      {
        return;
      }
    try
      {
        systemId = XMLJ.getAbsoluteURI (base, systemId);
        dtdHandler.notationDecl (name, publicId, systemId);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void attributeDecl (String eName, String aName, String type,
                              String mode, String value)
    throws SAXException
  {
    if (seenFatalError || declarationHandler == null)
      {
        return;
      }
    try
      {
        declarationHandler.attributeDecl (eName, aName, type, mode, value);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void elementDecl (String name, String model)
    throws SAXException
  {
    if (seenFatalError || declarationHandler == null)
      {
        return;
      }
    try
      {
        declarationHandler.elementDecl (name, model);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void unparsedEntityDecl (String name, String publicId,
                                   String systemId, String notationName)
    throws SAXException
  {
    if (seenFatalError || dtdHandler == null)
      {
        return;
      }
    try
      {
        systemId = XMLJ.getAbsoluteURI (base, systemId);
        dtdHandler.unparsedEntityDecl (name, publicId, systemId,
                                       notationName);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void setDocumentLocator (Object ctx, Object loc)
  {
    locator = new GnomeLocator (ctx, loc);
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    try
      {
        contentHandler.setDocumentLocator (locator);
      }
    catch (Exception e)
      {
      }
  }
  
  private void startDocument (boolean standalone)
    throws SAXException
  {
    this.standalone = standalone;
    seenStartDocument = true;
    if (contentHandler == null)
      {
        return;
      }
    try
      {
        contentHandler.startDocument ();
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void endDocument ()
    throws SAXException
  {
    if (contentHandler == null)
      {
        return;
      }
    try
      {
        contentHandler.endDocument();
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void startElement(String name, String[] attrs)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    try
      {
        XMLName xName = new XMLName (this, name);
        if (namespaces)
          {
            // Handle defined namespaces
            ns.push ();
            int len = (attrs == null) ? 0 : attrs.length;
            if (len > 0)
              {
                ArrayList filtered = new ArrayList (len);
                for (int i = 0; i < len; i += 2)
                  {
                    String attName = attrs[i];
                    String attValue = attrs[i + 1];
                    if (attName.equals ("xmlns"))
                      {
                        startPrefixMapping ("", attValue);
                      }
                    else if (attName.startsWith ("xmlns:"))
                      {
                        startPrefixMapping (attName.substring (6), attValue);
                      }
                    else
                      {
                        filtered.add (attName);
                        filtered.add (attValue);
                      }
                  }
                // Remove xmlns attributes
                attrs = new String[filtered.size ()];
                filtered.toArray (attrs);
              }
          }
        // Construct attributes
        Attributes atts = new StringArrayAttributes (this, attrs);
        contentHandler.startElement (xName.uri, xName.localName, xName.qName,
                                     atts);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void endElement (String name)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    try
      {
        XMLName xName = new XMLName (this, name);
        String uri = (xName.uri == null) ? "" : xName.uri;
        contentHandler.endElement (uri, xName.localName, xName.qName);
        // Handle undefining namespaces
        if (namespaces)
          {
            for (Iterator i = ns.currentPrefixes (); i.hasNext (); )
              {
                endPrefixMapping ((String) i.next ());
              }
            ns.pop (); // releases current depth
          }
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void startPrefixMapping (String prefix, String uri)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    ns.define (prefix, uri);
    contentHandler.startPrefixMapping (prefix, uri);
  }

  private void endPrefixMapping (String prefix)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    contentHandler.endPrefixMapping (prefix);
  }

  private void characters (String text)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null || text == null)
      {
        return;
      }
    try
      {
        char[] ch = text.toCharArray ();
        contentHandler.characters (ch, 0, ch.length);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void ignorableWhitespace (String text)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null || text == null)
      {
        return;
      }
    try
      {
        char[] ch = text.toCharArray ();
        contentHandler.ignorableWhitespace (ch, 0, ch.length);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void processingInstruction (String target, String data)
    throws SAXException
  {
    if (seenFatalError || contentHandler == null)
      {
        return;
      }
    try
      {
        if (data == null)
          {
            data = "";
          }
        contentHandler.processingInstruction (target, data);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void comment (String text)
    throws SAXException
  {
    if (seenFatalError || lexicalHandler == null || text == null)
      {
        return;
      }
    try
      {
        char[] ch = text.toCharArray ();
        lexicalHandler.comment (ch, 0, ch.length);
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void cdataBlock (String text)
    throws SAXException
  {
    if (seenFatalError || text == null)
      {
        return;
      }
    try
      {
        if (lexicalHandler == null)
          {
            characters(text);
          }
        else
          {
            lexicalHandler.startCDATA();
            characters(text);
            lexicalHandler.endCDATA();
          }
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void warning (String message,
                        int lineNumber, int columnNumber,
                        String publicId, String systemId)
    throws SAXException
  {
    if (seenFatalError || errorHandler == null)
      {
        return;
      }
    try
      {
        Locator l = new StandaloneLocator (lineNumber, columnNumber,
                                           publicId, systemId);
        errorHandler.warning (new SAXParseException (message, l));
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void error (String message,
                      int lineNumber, int columnNumber,
                      String publicId, String systemId)
    throws SAXException
  {
    if (seenFatalError || errorHandler == null)
      {
        return;
      }
    try
      {
        Locator l = new StandaloneLocator (lineNumber, columnNumber,
                                           publicId, systemId);
        errorHandler.error (new SAXParseException (message, l));
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

  private void fatalError (String message,
                           int lineNumber, int columnNumber,
                           String publicId, String systemId)
    throws SAXException
  {
    if (seenFatalError || errorHandler == null)
      {
        return;
      }
    try
      {
        if (!seenStartDocument)
          {
            startDocument (false);
          }
        seenFatalError = true;
        Locator l = new StandaloneLocator (lineNumber, columnNumber,
                                           publicId, systemId);
        errorHandler.fatalError (new SAXParseException (message, l));
      }
    catch (Exception e)
      {
        if (e instanceof SAXException)
          {
            throw (SAXException) e;
          }
        else
          {
            throw new SAXException (e);
          }
      }
  }

}
