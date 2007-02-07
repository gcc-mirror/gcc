/* XSLURIResolver.java -- 
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

package gnu.xml.transform;

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.URIResolver;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import gnu.xml.dom.DomDocument;
import gnu.xml.dom.ls.SAXEventSink;
import gnu.xml.dom.ls.ReaderInputStream;

/**
 * URI resolver for XSLT.
 * This resolver parses external entities into DOMSources. It
 * maintains a cache of URIs to DOMSources to avoid expensive re-parsing.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class XSLURIResolver
  implements URIResolver
{

  Map lastModifiedCache = new HashMap();
  Map nodeCache = new HashMap();
  DocumentBuilder builder;
  URIResolver userResolver;
  ErrorListener userListener;

  void setUserResolver(URIResolver userResolver)
  {
    this.userResolver = userResolver;
  }

  void setUserListener(ErrorListener userListener)
  {
    this.userListener = userListener;
  }

  /**
   * Clear the cache.
   */
  void flush()
  {
    lastModifiedCache.clear();
    nodeCache.clear();
  }

  public Source resolve(String href, String base)
    throws TransformerException
  {
    Source source = null;
    if (userResolver != null)
      {
        source = userResolver.resolve(base, href);
      }
    return resolveDOM(source, href, base);
  }

  DOMSource resolveDOM(Source source, String base, String href)
    throws TransformerException
  {
    if (source != null && source instanceof DOMSource)
      {
        return (DOMSource) source;
      }
    String systemId = (source == null) ? null : source.getSystemId();
    long lastModified = 0L, lastLastModified = 0L;

    try
      {
        Node node = null;
        InputStream in = null;
        if (source != null && source instanceof StreamSource)
          {
            StreamSource ss = (StreamSource) source;
            in = ss.getInputStream();
            if (in == null)
              {
                Reader reader = ss.getReader();
                if (reader != null)
                  {
                    in = new ReaderInputStream(reader);
                  }
              }
          }
        else if (source != null && source instanceof SAXSource)
          {
            SAXSource ss = (SAXSource) source;
            InputSource input = ss.getInputSource();
            if (input != null)
              {
                if (systemId == null)
                  systemId = input.getSystemId();
                XMLReader reader = ss.getXMLReader();
                if (reader != null)
                  return parse(input, reader);
              }
          }
        if (in == null)
          {
            URL url = resolveURL(systemId, base, href);
            if (url != null)
              {
                systemId = url.toString();
                node = (Node) nodeCache.get(systemId);
                // Is the resource up to date?
                URLConnection conn = url.openConnection();
                Long llm = (Long) lastModifiedCache.get(systemId);
                if (llm != null)
                  {
                    lastLastModified = llm.longValue();
                    conn.setIfModifiedSince(lastLastModified);
                  }
                conn.connect();
                lastModified = conn.getLastModified();
                if (node != null && 
                    lastModified > 0L &&
                    lastModified <= lastLastModified)
                  {
                    // Resource unchanged
                    return new DOMSource(node, systemId);
                  }
                else
                  {
                    // Resource new or modified
                    in = conn.getInputStream();
                    nodeCache.put(systemId, node);
                    lastModifiedCache.put(systemId, new Long(lastModified));
                  }
              }
            else
              {
                throw new TransformerException("can't resolve URL: " +
                                               systemId);
              }
          }
        InputSource input = new InputSource(in);
        input.setSystemId(systemId);
        DocumentBuilder builder = getDocumentBuilder();
        node = builder.parse(input);
        return new DOMSource(node, systemId);
      }
    catch (IOException e)
      {
        throw new TransformerException(e);
      }
    catch (SAXException e)
      {
        throw new TransformerException(e);
      }
  }

  URL resolveURL(String systemId, String base, String href)
    throws IOException
  {
    URL url = null;
    try
      {
        if (systemId != null)
          {
            try
              {
                url = new URL(systemId);
              }
            catch (MalformedURLException e)
              {
                // Try building from base + href
              }
          }
        if (url == null)
          {
            if (base != null)
              {
                URL baseURL = new URL(base);
                url = new URL(baseURL, href);
              }
            else if (href != null)
              {
                url = new URL(href);
              }
            else
              {
                // See below
                throw new MalformedURLException(systemId);
              }
          }
        return url;
      }
    catch (MalformedURLException e)
      {
        // Fall back to local filesystem
        File file = null;
        if (href == null)
          {
            href = systemId;
          }
        if (base != null)
          {
            int lsi = base.lastIndexOf(File.separatorChar);
            if (lsi != -1 && lsi < base.length() - 1)
              {
                base = base.substring(0, lsi);
              }
            File baseFile = new File(base);
            file = new File(baseFile, href);
          }
        else if (href != null)
          {
            file = new File(href);
          }
        return (file == null) ? null : file.toURL();
      }
  }
  
  DocumentBuilder getDocumentBuilder()
    throws TransformerException
  {
    try
      {
        if (builder == null)
          {
            DocumentBuilderFactory factory =
              DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            factory.setExpandEntityReferences(true);
            builder = factory.newDocumentBuilder();
          }
        if (userResolver != null)
          {
            builder.setEntityResolver(new URIResolverEntityResolver(userResolver));
          }
        if (userListener != null)
          {
            builder.setErrorHandler(new ErrorListenerErrorHandler(userListener));
          }
        return builder;
      }
    catch (Exception e)
      {
        throw new TransformerException(e);
      }
  }

  DOMSource parse(InputSource source, XMLReader reader)
    throws SAXException, IOException
  {
    SAXEventSink eventSink = new SAXEventSink();
    eventSink.setReader(reader);
    reader.setContentHandler(eventSink);
    reader.setDTDHandler(eventSink);
    reader.setProperty("http://xml.org/sax/properties/lexical-handler",
            eventSink);
    reader.setProperty("http://xml.org/sax/properties/declaration-handler",
            eventSink);
    // XXX entityResolver
    // XXX errorHandler
    reader.parse(source);
    Document doc = eventSink.getDocument();
    String systemId = source.getSystemId();
    if (systemId != null && doc instanceof DomDocument)
      ((DomDocument) doc).setDocumentURI(systemId);
    return new DOMSource(doc, systemId);
  }
  
}

