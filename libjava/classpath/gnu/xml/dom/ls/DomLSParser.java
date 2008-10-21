/* DomLSParser.java -- 
   Copyright (C) 1999,2000,2001,2007 Free Software Foundation, Inc.

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

package gnu.xml.dom.ls;

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.w3c.dom.Document;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMStringList;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSException;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSParser;
import org.w3c.dom.ls.LSParserFilter;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import gnu.xml.dom.DomDocument;
import gnu.xml.dom.DomDOMException;

/**
 * Parser implementation for GNU DOM.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomLSParser
  implements LSParser, DOMConfiguration, DOMStringList, ErrorHandler
{

  private static final List SUPPORTED_PARAMETERS
    = Arrays.asList(new String[] { "cdata-sections",
                    "comments",
                    "element-content-whitespace",
                    "namespaces",
                    "expand-entity-references",
                    "coalescing",
                    "validating",
                    "xinclude-aware",
                    "entity-resolver",
                    "error-handler" });

  private LSParserFilter filter;
  private final boolean async;
  private String schemaType;
  private SAXEventSink eventSink;
  private SAXParserFactory factory;
  private XMLReader reader;

  private boolean namespaceAware = true;
  private boolean ignoreWhitespace;
  private boolean expandEntityReferences;
  private boolean ignoreComments;
  private boolean coalescing;
  private boolean validating;
  private boolean xIncludeAware;
  private EntityResolver entityResolver;
  private ErrorHandler errorHandler;

  public DomLSParser(short mode, String schemaType)
    throws DOMException
  {
    switch (mode)
      {
      case DOMImplementationLS.MODE_ASYNCHRONOUS:
        async = true;
        break;
      case DOMImplementationLS.MODE_SYNCHRONOUS:
        async = false;
        break;
      default:
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
      }
    // TODO schemaType
    this.schemaType = schemaType;
    factory = SAXParserFactory.newInstance();
  }

  // -- LSParser --
  
  public DOMConfiguration getDomConfig()
  {
    return this;
  }
  
  public LSParserFilter getFilter()
  {
    return filter;
  }

  public void setFilter(LSParserFilter filter)
  {
    this.filter = filter;
  }

  public boolean getAsync()
  {
    return async;
  }

  public boolean getBusy()
  {
    return eventSink != null;
  }

  public Document parse(LSInput input)
    throws DOMException, LSException
  {
    if (async)
      {
        return doParse(input);
      }
    else
      {
        synchronized (this)
          {
            return doParse(input);
          }
      }
  }

  public Document parseURI(String uri)
    throws DOMException, LSException
  {
    LSInput input = new DomLSInput();
    input.setSystemId(uri);
    return parse(input);
  }

  public Node parseWithContext(LSInput input, Node context, short action)
    throws DOMException, LSException
  {
    Document doc = (context.getNodeType() == Node.DOCUMENT_NODE) ?
      (Document) context : context.getOwnerDocument();
    input.setBaseURI(doc.getDocumentURI());
    // TODO use namespaces defined on context node
    Document ret = parse(input);
    Node root = ret.getDocumentElement();
    root = doc.adoptNode(root);
    switch (action)
      {
      case ACTION_APPEND_AS_CHILDREN:
        context.appendChild(root);
        break;
      case ACTION_REPLACE_CHILDREN:
        Node c1 = context.getFirstChild();
        while (c1 != null)
          {
            Node next = c1.getNextSibling();
            context.removeChild(c1);
            c1 = next;
          }
        context.appendChild(root);
        break;
      case ACTION_INSERT_BEFORE:
        Node p1 = context.getParentNode();
        p1.insertBefore(root, context);
        break;
      case ACTION_INSERT_AFTER:
        Node p2 = context.getParentNode();
        Node r1 = context.getNextSibling();
        if (r1 == null)
          {
            p2.appendChild(root);
          }
        else
          {
            p2.insertBefore(root, r1);
          }
        break;
      case ACTION_REPLACE:
        Node p3 = context.getParentNode();
        Node r2 = context.getNextSibling();
        p3.removeChild(context);
        if (r2 == null)
          {
            p3.appendChild(root);
          }
        else
          {
            p3.insertBefore(root, r2);
          }
        break;
      }
    return root;
  }

  public void abort()
  {
    if (eventSink != null)
      {
        eventSink.interrupt();
      }
  }

  private Document doParse(LSInput input)
    throws DOMException, LSException
  {
    // create event sink
    if (eventSink != null)
      {
        throw new LSException(LSException.PARSE_ERR, "parse in progress");
      }
    InputSource source = getInputSource(input);
    eventSink = (filter == null) ? new SAXEventSink() :
      new FilteredSAXEventSink(filter);
    // configure sink
    eventSink.setNamespaceAware(namespaceAware);
    eventSink.ignoreWhitespace = ignoreWhitespace;
    eventSink.expandEntityReferences = expandEntityReferences;
    eventSink.ignoreComments = ignoreComments;
    eventSink.coalescing = coalescing;
    // get and configure reader
    XMLReader reader = getXMLReader();
    eventSink.reader = reader;
    try
      {
        reader.setContentHandler(eventSink);
        reader.setDTDHandler(eventSink);
        reader.setProperty("http://xml.org/sax/properties/lexical-handler",
                           eventSink);
        reader.setProperty("http://xml.org/sax/properties/declaration-handler",
                           eventSink);
        reader.setFeature("http://xml.org/sax/features/namespaces",
                          namespaceAware);
        reader.setFeature("http://xml.org/sax/features/namespace-prefixes",
                          true);
        reader.setFeature("http://xml.org/sax/features/validation",
                          validating);
        try
          {
            reader.setFeature("http://gnu.org/sax/features/coalescing",
                              coalescing);
          }
        catch (SAXNotRecognizedException e)
          {
            // ignore
          } 
        try
          {
            reader.setFeature("http://xml.org/sax/features/use-attributes2",
                              true);
          }
        catch (SAXNotRecognizedException e)
          {
            // ignore
          }
        try
          {
            reader.setFeature("http://xml.org/sax/features/external-general-entities",
                              true);
          }
        catch (SAXNotRecognizedException e)
          {
            // ignore
          }
        reader.setEntityResolver(entityResolver);
        reader.setErrorHandler(errorHandler);
        // parse
        reader.parse(source);
      }
    catch (DOMException e)
      {
        reader = null;
        eventSink = null;
        throw e;
      }
    catch (SAXException e)
      {
        reader = null;
        eventSink = null;
        throw new DomLSException(LSException.PARSE_ERR, e);
      }
    catch (IOException e)
      {
        reader = null;
        eventSink = null;
        throw new DomLSException(LSException.PARSE_ERR, e);
      }
    // return document
    Document ret = eventSink.doc;
    String systemId = input.getSystemId();
    if (systemId != null && ret instanceof DomDocument)
      {
        ((DomDocument) ret).setDocumentURI(systemId);
      }
    eventSink = null;
    return ret;
  }

  private XMLReader getXMLReader()
    throws LSException
  {
    if (reader == null)
      {
        factory.setNamespaceAware(namespaceAware);
        factory.setValidating(validating);
        factory.setXIncludeAware(xIncludeAware);
        try
          {
            SAXParser parser = factory.newSAXParser();
            reader = parser.getXMLReader();
          }
        catch (ParserConfigurationException e)
          {
            throw new DomLSException(LSException.PARSE_ERR, e);
          }
        catch (SAXException e)
          {
            throw new DomLSException(LSException.PARSE_ERR, e);
          }
      }
    return reader;
  }

  private InputSource getInputSource(LSInput input)
    throws LSException
  {
    InputSource source = null;
    String systemId = input.getSystemId();
    InputStream in = input.getByteStream();
    if (in != null)
      {
        source = new InputSource(in);
        source.setSystemId(systemId);
      }
    if (source == null)
      {
        URL url = null;
        String base = input.getBaseURI();
        try
          {
            try
              {
                URL baseURL = (base == null) ? null : new URL(base);
                url = (baseURL == null) ? new URL(systemId) :
                  new URL(baseURL, systemId);
              }
            catch (MalformedURLException e)
              {
                File baseFile = (base == null) ? null : new File(base);
                url = (baseFile == null) ? new File(systemId).toURL() :
                  new File(baseFile, systemId).toURL();
              }
            in = url.openStream();
            systemId = url.toString();
            source = new InputSource(in);
            source.setSystemId(systemId);
          }
        catch (IOException e)
          {
            throw new DomLSException(LSException.PARSE_ERR, e);
          }
      }
    return source;
  }

  // -- DOMConfiguration --

  public void setParameter(String name, Object value)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("cdata-sections".equals(name))
      {
        coalescing = !((Boolean) value).booleanValue();
      }
    else if ("comments".equals(name))
      {
        ignoreComments = !((Boolean) value).booleanValue();
      }
    else if ("element-content-whitespace".equals(name))
      {
        ignoreWhitespace = !((Boolean) value).booleanValue();
      }
    else if ("namespaces".equals(name))
      {
        namespaceAware = ((Boolean) value).booleanValue();
      }
    else if ("expand-entity-references".equals(name))
      {
        expandEntityReferences = ((Boolean) value).booleanValue();
      }
    else if ("coalescing".equals(name))
      {
        coalescing = ((Boolean) value).booleanValue();
      }
    else if ("validating".equals(name))
      {
        validating = ((Boolean) value).booleanValue();
      }
    else if ("xinclude-aware".equals(name))
      {
        xIncludeAware = ((Boolean) value).booleanValue();
      }
    else if ("entity-resolver".equals(name))
      {
        entityResolver = (EntityResolver) value;
      }
    else if ("error-handler".equals(name))
      {
        errorHandler = (ErrorHandler) value;
      }
    else
      {
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
      }
    // invalidate reader, a new one will be created
    reader = null;
  }

  public Object getParameter(String name)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("cdata-sections".equals(name))
      {
        return coalescing ? Boolean.FALSE : Boolean.TRUE;
      }
    else if ("comments".equals(name))
      {
        return ignoreComments ? Boolean.FALSE : Boolean.TRUE;
      }
    else if ("element-content-whitespace".equals(name))
      {
        return ignoreWhitespace ? Boolean.FALSE : Boolean.TRUE;
      }
    else if ("namespaces".equals(name))
      {
        return namespaceAware ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("expand-entity-references".equals(name))
      {
        return expandEntityReferences ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("coalescing".equals(name))
      {
        return coalescing ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("validating".equals(name))
      {
        return validating ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("xinclude-aware".equals(name))
      {
        return xIncludeAware ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("entity-resolver".equals(name))
      {
        return entityResolver;
      }
    else if ("error-handler".equals(name))
      {
        return errorHandler;
      }
    else
      {
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
      }
  }

  public boolean canSetParameter(String name, Object value)
  {
    return contains(name);
  }

  public DOMStringList getParameterNames()
  {
    return this;
  }

  // -- DOMStringList --

  public String item(int i)
  {
    return (String) SUPPORTED_PARAMETERS.get(i);
  }

  public int getLength()
  {
    return SUPPORTED_PARAMETERS.size();
  }

  public boolean contains(String str)
  {
    return SUPPORTED_PARAMETERS.contains(str);
  }

  // -- ErrorHandler --

  public void warning(SAXParseException e)
    throws SAXException
  {
    if (errorHandler != null)
      {
        errorHandler.warning(e);
      }
  }

  public void error(SAXParseException e)
    throws SAXException
  {
    if (errorHandler != null)
      {
        errorHandler.error(e);
      }
  }

  public void fatalError(SAXParseException e)
    throws SAXException
  {
    if (errorHandler != null)
      {
        errorHandler.fatalError(e);
      }
    abort();
  }
  
}

