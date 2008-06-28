/* XIncludeFilter.java -- 
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
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.util.ReaderDelegate;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.traversal.DocumentTraversal;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.TreeWalker;
import org.w3c.dom.xpath.XPathEvaluator;
import org.w3c.dom.xpath.XPathNSResolver;
import org.w3c.dom.xpath.XPathResult;
import org.xml.sax.SAXException;

/**
 * StAX filter for performing XInclude processing.
 *
 * @see http://www.w3.org/TR/xinclude/
 * @see http://www.w3.org/TR/xptr-framework/
 * @see http://www.w3.org/TR/xptr-element/
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class XIncludeFilter
  extends ReaderDelegate
{

  static final String XINCLUDE_NS_URI = "http://www.w3.org/2001/XInclude";
  static final int SHOW_FLAGS =
    NodeFilter.SHOW_CDATA_SECTION |
    NodeFilter.SHOW_COMMENT |
    NodeFilter.SHOW_ELEMENT |
    NodeFilter.SHOW_ENTITY_REFERENCE |
    NodeFilter.SHOW_PROCESSING_INSTRUCTION |
    NodeFilter.SHOW_TEXT;

  final String systemId;
  final boolean namespaceAware;
  final boolean validating;
  final boolean expandERefs;
  String href;
  int event;
  boolean included;
  XPathResult result;
  int snapshotIndex;
  Node current;
  TreeWalker walker;
  HashSet seen = new HashSet();
  boolean backtracking;
  boolean lookahead;
  
  Reader includedText;
  char[] buf;
  int len = -1;
  boolean inInclude, inFallback, seenFallback;

  DocumentBuilder builder;

  XIncludeFilter(XMLStreamReader reader, String systemId,
                 boolean namespaceAware, boolean validating,
                 boolean expandERefs)
  {
    super(reader);
    this.systemId = XMLParser.absolutize(null, systemId);
    this.namespaceAware = namespaceAware;
    this.validating = validating;
    this.expandERefs = expandERefs;
  }

  public int getAttributeCount()
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        return (attrs == null) ? 0 : attrs.getLength();
      }
    return super.getAttributeCount();
  }

  public String getAttributeLocalName(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.item(index);
        return attr.getLocalName();
      }
    return super.getAttributeLocalName(index);
  }

  public String getAttributeNamespace(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.item(index);
        return attr.getNamespaceURI();
      }
    return super.getAttributeNamespace(index);
  }

  public String getAttributePrefix(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.item(index);
        return attr.getPrefix();
      }
    return super.getAttributePrefix(index);
  }

  public QName getAttributeName(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.item(index);
        String localName = attr.getLocalName();
        String uri = attr.getNamespaceURI();
        String prefix = attr.getPrefix();
        return new QName(uri, localName, prefix);
      }
    return super.getAttributeName(index);
  }

  public String getAttributeType(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Attr attr = (Attr) attrs.item(index);
        TypeInfo ti = attr.getSchemaTypeInfo();
        return (ti == null) ? "CDATA" : ti.getTypeName();
      }
    return super.getAttributeType(index);
  }

  public boolean isAttributeSpecified(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
          return false;
        Attr attr = (Attr) attrs.item(index);
        return attr.getSpecified();
      }
    return super.isAttributeSpecified(index);
  }

  public String getAttributeValue(int index)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.item(index);
        return attr.getNodeValue();
      }
    return super.getAttributeValue(index);
  }

  public String getAttributeValue(String uri, String localName)
  {
    if (current != null)
      {
        NamedNodeMap attrs = current.getAttributes();
        if (attrs == null)
         return null;
        Node attr = attrs.getNamedItemNS(uri, localName);
        return (attr == null) ? null : attr.getNodeValue();
      }
    return super.getAttributeValue(uri, localName);
  }

  public String getElementText()
    throws XMLStreamException
  {
    if (current != null)
      return current.getTextContent();
    return super.getElementText();
  }

  public int getEventType()
  {
    return event;
  }

  public String getLocalName()
  {
    if (current != null)
      return current.getLocalName();
    return super.getLocalName();
  }

  public QName getName()
  {
    if (current != null)
      {
        String localName = current.getLocalName();
        String uri = current.getNamespaceURI();
        String prefix = current.getPrefix();
        return new QName(uri, localName, prefix);
      }
    return super.getName();
  }

  public String getNamespaceURI()
  {
    if (current != null)
      return current.getNamespaceURI();
    return super.getNamespaceURI();
  }

  // TODO namespaces

  public String getPIData()
  {
    if (current != null)
      return ((ProcessingInstruction) current).getData();
    return super.getPIData();
  }

  public String getPITarget()
  {
    if (current != null)
      return ((ProcessingInstruction) current).getTarget();
    return super.getPITarget();
  }

  public String getPrefix()
  {
    if (current != null)
      return current.getPrefix();
    return super.getPrefix();
  }

  public String getText()
  {
    if (current != null)
      return current.getNodeValue();
    if (walker != null)
      {
        Node n = walker.getCurrentNode();
        if (n != null)
          return n.getTextContent();
      }
    if (buf != null)
      return new String(buf, 0, len);
    return super.getText();
  }

  public char[] getTextCharacters()
  {
    if (current != null)
      {
        buf = current.getNodeValue().toCharArray();
        len = buf.length;
      }
    if (buf != null)
      return buf;
    return super.getTextCharacters();
  }

  public int getTextCharacters(int sourceStart, char[] target,
                               int targetStart, int length)
    throws XMLStreamException
  {
    if (current != null)
      {
        buf = current.getNodeValue().toCharArray();
        len = buf.length;
      }
    if (buf != null)
      {
        int max = Math.min(len - sourceStart, length);
        if (max > 0)
          System.arraycopy(buf, sourceStart, target, targetStart, max);
        return max;
      }
    return super.getTextCharacters(sourceStart, target, targetStart, length);
  }

  public int getTextLength()
  {
    if (current != null)
      {
        buf = current.getNodeValue().toCharArray();
        len = buf.length;
      }
    if (buf != null)
      return len;
    return super.getTextLength();
  }

  public int getTextStart()
  {
    if (current != null)
      {
        buf = current.getNodeValue().toCharArray();
        len = buf.length;
      }
    if (buf != null)
      return 0;
    return super.getTextStart();
  }

  public boolean hasNext()
    throws XMLStreamException
  {
    if (!lookahead)
      {
        try
          {
            next();
          }
        catch (NoSuchElementException e)
          {
            event = -1;
          }
        lookahead = true;
      }
    return (event != -1);
  }

  public int next()
    throws XMLStreamException
  {
    if (lookahead)
      {
        lookahead = false;
        return event;
      }
    buf = null;
    len = 0;
    if (walker != null)
      {
        Node c = walker.getCurrentNode();
        Node n = null;
        if (c.getNodeType() == Node.ELEMENT_NODE)
          {
            boolean isStartElement = !seen.contains(c);
            if (isStartElement)
              {
                seen.add(c);
                current = c;
                event = XMLStreamConstants.START_ELEMENT;
                return event;
              }
            else if (backtracking)
              {
                n = walker.nextSibling();
                if (n != null)
                  backtracking = false;
              }
            else
              {
                n = walker.firstChild();
                if (n == null)
                  n = walker.nextSibling();
              }
          }
        else
          {
            n = walker.firstChild();
            if (n == null)
              n = walker.nextSibling();
          }
        if (n == null)
          {
            current = walker.parentNode();
            if (current != null && current.getNodeType() == Node.ELEMENT_NODE)
              {
                // end-element
                backtracking = true;
                event = XMLStreamConstants.END_ELEMENT;
                return event;
              }
            else
              {
                walker = null;
                current = null;
              }
          }
        else
          {
            current = n;
            switch (n.getNodeType())
              {
              case Node.ELEMENT_NODE:
                return next();
              case Node.TEXT_NODE:
                String text = n.getNodeValue();
                buf = text.toCharArray();
                len = buf.length;
                event = isSpace(buf, len) ?
                  XMLStreamConstants.SPACE :
                  XMLStreamConstants.CHARACTERS;
                return event;
              case Node.CDATA_SECTION_NODE:
                event = XMLStreamConstants.CDATA;
                return event;
              case Node.COMMENT_NODE:
                event = XMLStreamConstants.COMMENT;
                return event;
              case Node.PROCESSING_INSTRUCTION_NODE:
                event = XMLStreamConstants.PROCESSING_INSTRUCTION;
                return event;
              case Node.ENTITY_REFERENCE_NODE:
                event = XMLStreamConstants.ENTITY_REFERENCE;
                return event;
              default:
                throw new IllegalStateException();
              }
          }
      }
    if (result != null)
      {
        switch (result.getResultType())
          {
          case XPathResult.BOOLEAN_TYPE:
            boolean bval = result.getBooleanValue();
            String btext = bval ? "true" : "false";
            buf = btext.toCharArray();
            len = buf.length;
            result = null;
            event = XMLStreamConstants.CHARACTERS;
            return event;
          case XPathResult.NUMBER_TYPE:
            double nval = result.getNumberValue();
            String ntext = Double.toString(nval);
            buf = ntext.toCharArray();
            len = buf.length;
            result = null;
            event = XMLStreamConstants.CHARACTERS;
            return event;
          case XPathResult.STRING_TYPE:
            String stext = result.getStringValue();
            buf = stext.toCharArray();
            len = buf.length;
            result = null;
            event = isSpace(buf, len) ?
              XMLStreamConstants.SPACE :
              XMLStreamConstants.CHARACTERS;
            return event;
          case XPathResult.ANY_UNORDERED_NODE_TYPE:
          case XPathResult.FIRST_ORDERED_NODE_TYPE:
            Node n1 = result.getSingleNodeValue();
            Document d1 = getDocument(n1);
            walker = getDocumentTraversal(d1)
              .createTreeWalker(n1, SHOW_FLAGS, null, expandERefs);
            result = null;
            return next();
          case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
          case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
            Node n2 = result.iterateNext();
            if (n2 == null)
              {
                result = null;
                return next();
              }
            Document d2 = getDocument(n2);
            walker = getDocumentTraversal(d2)
              .createTreeWalker(n2, SHOW_FLAGS, null, expandERefs);
            return next();
          case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
          case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
            Node n3 = result.snapshotItem(snapshotIndex++);
            if (n3 == null)
              {
                result = null;
                return next();
              }
            Document d3 = getDocument(n3);
            walker = getDocumentTraversal(d3)
              .createTreeWalker(n3, SHOW_FLAGS, null, expandERefs);
            return next();
          default:
            throw new IllegalStateException();
          }
      }
    if (includedText != null)
      {
        // fill buffer
        if (buf == null)
          buf = new char[2048];
        try
          {
            len = includedText.read(buf, 0, buf.length);
            if (len == -1)
              {
                includedText = null;
                buf = null;
                return next();
              }
            // chars or space?
            return (event = isSpace(buf, len) ?
                    XMLStreamConstants.SPACE :
                    XMLStreamConstants.CHARACTERS);
          }
        catch (IOException e)
          {
            XMLStreamException e2 = new XMLStreamException(e.getMessage());
            e2.initCause(e);
            throw e2;
          }
      }
    event = super.next();
    switch (event)
      {
      case XMLStreamConstants.START_ELEMENT:
        String uri = getNamespaceURI();
        if (XINCLUDE_NS_URI.equals(uri))
          {
            String localName = getLocalName();
            if ("include".equals(localName))
              {
                href = getAttributeValue(null, "href");
                String parse = getAttributeValue(null, "parse");
                String xpointer = getAttributeValue(null, "xpointer");
                String encoding = getAttributeValue(null, "encoding");
                String accept = getAttributeValue(null, "accept");
                String acceptLanguage = getAttributeValue(null,
                                                          "accept-language");
                if (includeResource(href, parse, xpointer, encoding,
                                    accept, acceptLanguage))
                  {
                    // Skip to xi:include end-element event
                    int depth = 0;
                    while (depth >= 0)
                      {
                        event = super.next();
                        switch (event)
                          {
                          case XMLStreamConstants.START_ELEMENT:
                            depth++;
                            break;
                          case XMLStreamConstants.END_ELEMENT:
                            depth--;
                          }
                      }
                  }
                else
                  inInclude = true;
              }
            else if (inInclude && "fallback".equals(localName))
              {
                if (!seenFallback)
                  inFallback = seenFallback = true;
                else
                  throw new XMLStreamException("duplicate xi:fallback element");
              }
            else if (inInclude)
              {
                throw new XMLStreamException("illegal xi element '" +
                                             localName + "'");
              }
            return next();
          }
        break;
      case XMLStreamConstants.END_ELEMENT:
        String uri2 = getNamespaceURI();
        if (XINCLUDE_NS_URI.equals(uri2))
          {
            String localName = getLocalName();
            if ("include".equals(localName))
              {
                if (!seenFallback && included)
                  {
                    String msg = "Unable to read " + href +
                      " and no xi:fallback element present";
                    throw new XMLStreamException(msg);
                  }
                included = false;
                href = null;
                inInclude = inFallback = seenFallback = false;
              }
            else if ("fallback".equals(localName))
              inFallback = false;
            return next();
          }
        break;
      }
    if (inInclude && !inFallback)
      return next();
    return event;
  }

  boolean isSpace(char[] text, int len)
  {
    boolean space = true;
    for (int i = 0; i < len; i++)
      {
        char c = text[i];
        if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
          {
            space = false;
            break;
          }
      }
    return space;
  }

  String getBaseURI()
  {
    String base = (String) getParent().getProperty("gnu.xml.stream.baseURI");
    return (base == null) ? systemId : base;
  }
  
  boolean includeResource(String href, String parse, String xpointer,
                          String encoding, String accept,
                          String acceptLanguage)
  {
    included = false;
    try
      {
        if (xpointer != null)
          throw new XMLStreamException("xpointer attribute not yet supported");
        String base = getBaseURI();
        if (href == null || "".equals(href))
          href = base;
        else
          href = XMLParser.absolutize(base, href);
        if (parse == null || "xml".equals(parse))
          {
            seen.clear();
            result = null;
            snapshotIndex = 0;
            walker = null;
            current = null;
            backtracking = false;
            
            URLConnection connection = getURLConnection(href, accept,
                                                        acceptLanguage);
            InputStream in = connection.getInputStream();
            Document doc = getDocumentBuilder().parse(in, href);
            DocumentTraversal dt = getDocumentTraversal(doc);
            if (xpointer == null)
              {
                result = null;
                Node item = doc.getDocumentElement();
                walker = dt.createTreeWalker(item, SHOW_FLAGS, null,
                                             expandERefs);
              }
            else
              {
                result = null;
                snapshotIndex = 0;
                walker = null;
                // shorthand or scheme-based?
                int lpi = xpointer.indexOf('(');
                int rpi = xpointer.indexOf(')', lpi);
                if (lpi != -1 && rpi != -1)
                  {
                    String scheme = xpointer.substring(0, lpi);
                    if ("element".equals(scheme))
                      {
                        // element() scheme
                        String elementSchemeData =
                          xpointer.substring(lpi + 1, rpi);
                        Node item = doc;
                        int si = elementSchemeData.indexOf('/');
                        if (si == -1)
                          {
                            if (elementSchemeData.length() > 0)
                              item = doc.getElementById(elementSchemeData);
                          }
                        else
                          {
                            if (si > 0)
                              {
                                String context =
                                  elementSchemeData.substring(0, si);
                                item = doc.getElementById(context);
                                elementSchemeData =
                                  elementSchemeData.substring(si + 1);
                              }
                            StringTokenizer st =
                              new StringTokenizer(elementSchemeData, "/");
                            while (st.hasMoreTokens() && item != null)
                              {
                                int n = Integer.parseInt(st.nextToken());
                                Node ctx = item.getFirstChild();
                                int count = 1;
                                while (ctx != null && count++ < n)
                                  ctx = ctx.getNextSibling();
                                item = ctx;
                              }
                          }
                        walker = dt.createTreeWalker(item, SHOW_FLAGS, null,
                                                     expandERefs);
                        included = true;
                      }
                    else if ("xpointer".equals(scheme))
                      {
                        xpointer = xpointer.substring(lpi + 1, rpi);
                        XPathEvaluator eval = getXPathEvaluator(doc);
                        XPathNSResolver resolver = eval.createNSResolver(doc);
                        result =
                          (XPathResult) eval.evaluate(xpointer, doc,
                                                      resolver,
                                                      XPathResult.ANY_TYPE,
                                                      null);
                        // TODO xpointer() scheme functions
                        included = true;
                      }
                    else
                      {
                        String msg = "Unknown XPointer scheme: " + scheme;
                        throw new XMLStreamException(msg);
                      }
                  }
                else
                  {
                    Node item = doc.getElementById(xpointer);
                    walker = dt.createTreeWalker(item, SHOW_FLAGS, null,
                                                 expandERefs);
                    included = true;
                  }
              }
          }
        else if ("text".equals(parse))
          {
            URLConnection connection = getURLConnection(href, accept,
                                                        acceptLanguage);
            InputStream in = connection.getInputStream();
            if (encoding == null)
              {
                encoding = connection.getContentEncoding();
                if (encoding == null)
                  {
                    String contentType = connection.getContentType();
                    if (contentType != null)
                      encoding = getParameter(contentType, "charset");
                  }
              }
            if (encoding == null)
              includedText = new InputStreamReader(in, "UTF-8");
            else
              includedText = new InputStreamReader(in, encoding);
            included = true;
          }
        else
          throw new XMLStreamException("value of 'parse' attribute must be "+
                                       "'xml' or 'text'");
        return true;
      }
    catch (IOException e)
      {
        return false;
      }
    catch (XMLStreamException e)
      {
        return false;
      }
    catch (SAXException e)
      {
        return false;
      }
  }

  URLConnection getURLConnection(String href, String accept,
                                 String acceptLanguage)
    throws IOException
  {
    URL url = new URL(href);
    URLConnection connection = url.openConnection();
    if (connection instanceof HttpURLConnection)
      {
        HttpURLConnection http = (HttpURLConnection) connection;
        http.setInstanceFollowRedirects(true);
        if (accept != null)
          http.setRequestProperty("Accept", accept);
        if (acceptLanguage != null)
          http.setRequestProperty("Accept-Language", acceptLanguage);
      }
    return connection;
  }

  Document getDocument(Node node)
  {
    if (node.getNodeType() == Node.DOCUMENT_NODE)
      return (Document) node;
    return node.getOwnerDocument();
  }

  DocumentBuilder getDocumentBuilder()
    throws XMLStreamException
  {
    if (builder == null)
      {
        try
          {
            DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
            f.setXIncludeAware(true);
            f.setNamespaceAware(namespaceAware);
            f.setValidating(validating);
            builder = f.newDocumentBuilder();
          }
        catch (ParserConfigurationException e)
          {
            XMLStreamException e2 = new XMLStreamException(e.getMessage());
            e2.initCause(e);
            throw e2;
          }
      }
    builder.reset();
    return builder;
  }

  DocumentTraversal getDocumentTraversal(Document doc)
    throws XMLStreamException
  {
    DOMImplementation dom = doc.getImplementation();
    if (!dom.hasFeature("Traversal", "2.0"))
      throw new XMLStreamException("Traversal not supported");
    return (DocumentTraversal) doc;
  }

  XPathEvaluator getXPathEvaluator(Document doc)
    throws XMLStreamException
  {
    DOMImplementation dom = doc.getImplementation();
    if (!dom.hasFeature("XPath", "3.0"))
      throw new XMLStreamException("XPath not supported");
    return (XPathEvaluator) doc;
  }

  static String getParameter(String contentType, String name)
  {
    StringTokenizer st = new StringTokenizer(contentType, " ;");
    if (st.hasMoreTokens())
      st.nextToken();
    while (st.hasMoreTokens())
      {
        String token = st.nextToken();
        int ei = token.indexOf('=');
        if (ei != -1)
          {
            String key = token.substring(0, ei);
            if (key.equals(name))
              {
                String value = token.substring(ei + 1);
                int len = value.length();
                if (len > 1 &&
                    value.charAt(0) == '"' &&
                    value.charAt(len - 1) == '"')
                  value = value.substring(1, len - 1);
                else if (len > 1 &&
                         value.charAt(0) == '\'' &&
                         value.charAt(len - 1) == '\'')
                  value = value.substring(1, len - 1);
                return value;
              }
          }
      }
    return null;
  }
  
}
