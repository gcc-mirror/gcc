/* TransformerFactoryImpl.java -- 
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

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.URIResolver;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import gnu.xml.dom.DomDocument;

/**
 * GNU transformer factory implementation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class TransformerFactoryImpl
  extends TransformerFactory
{

  final XPathFactory xpathFactory;
	final XSLURIResolver resolver;
  ErrorListener userListener;
  URIResolver userResolver;

  public TransformerFactoryImpl()
  {
    xpathFactory = new gnu.xml.xpath.XPathFactoryImpl();
    resolver = new XSLURIResolver();
  }

  public Transformer newTransformer(Source source)
    throws TransformerConfigurationException
  {
    Stylesheet stylesheet = newStylesheet(source, 0, null);
    Properties outputProperties =
      new TransformerOutputProperties(stylesheet);
    TransformerImpl transformer =
      new TransformerImpl(this, stylesheet, outputProperties);
    stylesheet.transformer = transformer;
    return transformer;
  }

  public Transformer newTransformer()
    throws TransformerConfigurationException
  {
    return new TransformerImpl(this, null, new Properties());
  }

  public Templates newTemplates(Source source)
    throws TransformerConfigurationException
  {
    Stylesheet stylesheet = newStylesheet(source, 0, null);
    return new TemplatesImpl(this, stylesheet);
  }

  Stylesheet newStylesheet(Source source, int precedence, Stylesheet parent)
    throws TransformerConfigurationException
  {
    Document doc = null;
    String systemId = null;
    if (source != null)
      {
        try
          {
            DOMSource ds;
            synchronized (resolver)
              {
                resolver.setUserResolver(userResolver);
                resolver.setUserListener(userListener);
                ds = resolver.resolveDOM(source, null, null);
              }
            Node node = ds.getNode();
            if (node == null)
              {
                throw new TransformerConfigurationException("no source document");
              }
            doc = (node instanceof Document) ? (Document) node :
              node.getOwnerDocument();
            systemId = ds.getSystemId();
          }
        catch (TransformerException e)
          {
            throw new TransformerConfigurationException(e);
          }
      }
    return new Stylesheet(this, parent, doc, systemId, precedence);
  }
  
  public Source getAssociatedStylesheet(Source source,
                                        String media,
                                        String title,
                                        String charset)
    throws TransformerConfigurationException
  {
    try
      {
        DOMSource ds;
        synchronized (resolver)
          {
            resolver.setUserResolver(userResolver);
            resolver.setUserListener(userListener);
            ds = resolver.resolveDOM(source, null, null);
          }
        Node node = ds.getNode();
        if (node == null)
          {
            throw new TransformerConfigurationException("no source document");
          }
        Document doc = (node instanceof Document) ? (Document) node :
          node.getOwnerDocument();
        LinkedList matches = new LinkedList();
        for (node = doc.getFirstChild();
             node != null;
             node = node.getNextSibling())
          {
            if (node.getNodeType() == Node.PROCESSING_INSTRUCTION_NODE &&
                "xml-stylesheet".equals(node.getNodeName()))
              {
                Map params = parseParameters(node.getNodeValue());
                if (media != null && !media.equals(params.get("media")))
                  {
                    continue;
                  }
                if (title != null && !title.equals(params.get("title")))
                  {
                    continue;
                  }
                if (charset != null && !charset.equals(params.get("charset")))
                  {
                    continue;
                  }
                String href = (String) params.get("href");
                URL url = resolver.resolveURL(null, node.getBaseURI(), href);
                matches.add(url);
              }
          }
        switch (matches.size())
          {
          case 0:
            return null;
          case 1:
            return new StreamSource(((URL) matches.getFirst()).toString());
          default:
            // Create a source representing a stylesheet with a list of
            // imports
            DomDocument ssDoc = new DomDocument();
            ssDoc.setBuilding(true);
            // Create document element
            Node root =
              ssDoc.createElementNS(Stylesheet.XSL_NS, "stylesheet");
            Node version =
              ssDoc.createAttributeNS(null, "version");
            version.setNodeValue("1.0");
            root.getAttributes().setNamedItemNS(version);
            ssDoc.appendChild(root);
            // Create xsl:import for each URL
            for (Iterator i = matches.iterator(); i.hasNext(); )
              {
                URL url = (URL) i.next();
                Node imp =
                  ssDoc.createElementNS(Stylesheet.XSL_NS, "import");
                Node href =
                  ssDoc.createAttributeNS(null, "href");
                href.setNodeValue(url.toString());
                imp.getAttributes().setNamedItemNS(href);
                root.appendChild(imp);
              }
            ssDoc.setBuilding(false);
            return new DOMSource(ssDoc);
          }
      }
    catch (IOException e)
      {
        throw new TransformerConfigurationException(e);
      }
    catch (TransformerException e)
      {
        throw new TransformerConfigurationException(e);
      }
  }

  Map parseParameters(String data)
  {
    Map ret = new LinkedHashMap();
    int len = data.length();
    String key = null;
    int start = 0;
    char quoteChar = '\u0000';
    for (int i = 0; i < len; i++)
      {
        char c = data.charAt(i);
        if (quoteChar == '\u0000' && c == ' ')
          {
            if (key == null && start < i)
              {
                key = data.substring(start, i);
              }
            else
              {
                String val = unquote(data.substring(start, i).trim());
                ret.put(key, val);
                key = null;
              }
            start = i + 1;
          }
        else if (c == '"')
          {
            quoteChar = (quoteChar == c) ? '\u0000' : c;
          }
        else if (c == '\'')
          {
            quoteChar = (quoteChar == c) ? '\u0000' : c;
          }
      }
    if (start < len && key != null)
      {
        String val = unquote(data.substring(start, len).trim());
        ret.put(key, val);
      }
    return ret;
  }

  String unquote(String text)
  {
    int end = text.length() - 1;
    if (text.charAt(0) == '\'' && text.charAt(end) == '\'')
      {
        return text.substring(1, end);
      }
    if (text.charAt(0) == '"' && text.charAt(end) == '"')
      {
        return text.substring(1, end);
      }
    return text;
  }

  public void setURIResolver(URIResolver resolver)
  {
    userResolver = resolver;
  }

  public URIResolver getURIResolver()
  {
    return userResolver;
  }

  public void setFeature(String name, boolean value)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException("not supported");
  }

  public boolean getFeature(String name)
  {
    if (SAXSource.FEATURE.equals(name) ||
        SAXResult.FEATURE.equals(name) ||
        StreamSource.FEATURE.equals(name) ||
        StreamResult.FEATURE.equals(name) ||
        DOMSource.FEATURE.equals(name) ||
        DOMResult.FEATURE.equals(name))
      {
        return true;
      }
    return false;
  }

  public void setAttribute(String name, Object value)
    throws IllegalArgumentException
  {
    throw new IllegalArgumentException("not supported");
  }

  public Object getAttribute(String name)
    throws IllegalArgumentException
  {
    throw new IllegalArgumentException("not supported");
  }

  public void setErrorListener(ErrorListener listener)
    throws IllegalArgumentException
  {
    userListener = listener;
  }

  public ErrorListener getErrorListener()
  {
    return userListener;
  }
  
}
