/* TransformerImpl.java -- 
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.UnknownServiceException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.URIResolver;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import gnu.xml.dom.DomDoctype;
import gnu.xml.dom.DomDocument;
import gnu.xml.dom.ls.WriterOutputStream;

/**
 * The transformation process for a given stylesheet.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class TransformerImpl
  extends Transformer
{

  final TransformerFactoryImpl factory;
  final Stylesheet stylesheet;
  URIResolver uriResolver;
  ErrorListener errorListener;
  Properties outputProperties;

  TransformerImpl(TransformerFactoryImpl factory,
                  Stylesheet stylesheet,
                  Properties outputProperties)
    throws TransformerConfigurationException
  {
    this.factory = factory;
    uriResolver = factory.userResolver;
    errorListener = factory.userListener;
    this.stylesheet = stylesheet;
    this.outputProperties = outputProperties;
    if (stylesheet != null)
      {
        // Set up parameter context for this transformer
        stylesheet.bindings.push(Bindings.PARAM);
      }
  }

  public void transform(Source xmlSource, Result outputTarget)
    throws TransformerException
  {
    // Get the source tree
    DOMSource source;
    synchronized (factory.resolver)
      {
        factory.resolver.setUserResolver(uriResolver);
        factory.resolver.setUserListener(errorListener);
        source = factory.resolver.resolveDOM(xmlSource, null, null);
      }
    Node context = source.getNode();
    Document doc = (context instanceof Document) ? (Document) context :
      context.getOwnerDocument();
    if (doc instanceof DomDocument)
      {
        // Suppress mutation events
        ((DomDocument) doc).setBuilding(true);
        // TODO find a better/more generic way of doing this than
        // casting
      }
    // Get the result tree
    Node parent = null, nextSibling = null;
    if (outputTarget instanceof DOMResult)
      {
        DOMResult dr = (DOMResult) outputTarget;
        parent = dr.getNode();
        nextSibling = dr.getNextSibling();

        Document rdoc = (parent instanceof Document) ? (Document) parent :
          parent.getOwnerDocument();
        if (rdoc instanceof DomDocument)
          {
            // Suppress mutation events and allow multiple root elements
            DomDocument drdoc = (DomDocument) rdoc;
            drdoc.setBuilding(true);
            drdoc.setCheckWellformedness(false);
            // TODO find a better/more generic way of doing this than
            // casting
          }
      }
    boolean created = false;
    // Transformation
    if (stylesheet != null)
      {
        if (parent == null)
          {
            // Create a new document to hold the result
            DomDocument resultDoc = new DomDocument();
            resultDoc.setBuilding(true);
            resultDoc.setCheckWellformedness(false);
            parent = resultDoc;
            created = true;
          }
        // Make a copy of the source node, and strip it
        context = context.cloneNode(true);
        strip(context);
        // XSLT transformation
        try
          {
            // Set output properties in the underlying stylesheet
            ((TransformerOutputProperties) outputProperties).apply();
            stylesheet.initTopLevelVariables(context);
            TemplateNode t = stylesheet.getTemplate(null, context, false);
            if (t != null)
              {
                stylesheet.current = context;
                t.apply(stylesheet, null, context, 1, 1, parent, nextSibling);
              }
          }
        catch (TransformerException e)
          {
            // Done transforming, reset document
            if (doc instanceof DomDocument)
              {
                ((DomDocument) doc).setBuilding(false);
              }
            throw e;
          }
      }
    else
      {
        // Identity transform
        Node clone = context.cloneNode(true);
        if (context.getNodeType() != Node.DOCUMENT_NODE)
          {
            Document resultDoc;
            if (parent == null)
              {
                // Create a new document to hold the result
                DomDocument rd = new DomDocument();
                rd.setBuilding(true);
                rd.setCheckWellformedness(false);
                parent = resultDoc = rd;
                created = true;
              }
            else
              {
                resultDoc = (parent instanceof Document) ?
                  (Document) parent :
                  parent.getOwnerDocument();
              }
            Document sourceDoc = context.getOwnerDocument();
            if (sourceDoc != resultDoc)
              {
                clone = resultDoc.adoptNode(clone);
              }
            if (nextSibling != null)
              {
                parent.insertBefore(clone, nextSibling);
              }
            else
              {
                parent.appendChild(clone);
              }
          }
        else
          {
            // Cannot append document to another tree
            parent = clone;
            created = true;
          }
      }
    String method = outputProperties.getProperty(OutputKeys.METHOD);
    int outputMethod = "html".equals(method) ? Stylesheet.OUTPUT_HTML :
      "text".equals(method) ? Stylesheet.OUTPUT_TEXT :
      Stylesheet.OUTPUT_XML;
    String encoding = outputProperties.getProperty(OutputKeys.ENCODING);
    String publicId = outputProperties.getProperty(OutputKeys.DOCTYPE_PUBLIC);
    String systemId = outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM);
    String version = outputProperties.getProperty(OutputKeys.VERSION);
    boolean omitXmlDeclaration = 
      "yes".equals(outputProperties.getProperty(OutputKeys.OMIT_XML_DECLARATION));
    boolean standalone = 
      "yes".equals(outputProperties.getProperty(OutputKeys.STANDALONE));
    String mediaType = outputProperties.getProperty(OutputKeys.MEDIA_TYPE);
    String cdataSectionElements =
      outputProperties.getProperty(OutputKeys.CDATA_SECTION_ELEMENTS);
    boolean indent =
      "yes".equals(outputProperties.getProperty(OutputKeys.INDENT));
    if (created)
      {
        // Discover document element
        DomDocument resultDoc = (DomDocument) parent;
        Node root = resultDoc.getDocumentElement();
        // Add doctype if specified
        if ((publicId != null || systemId != null) &&
            root != null)
          {
            // We must know the name of the root element to
            // create the document type
            resultDoc.appendChild(new DomDoctype(resultDoc,
                                                 root.getNodeName(),
                                                 publicId,
                                                 systemId));
          }
        resultDoc.setBuilding(false);
        resultDoc.setCheckWellformedness(true);
      }
    else if (publicId != null || systemId != null)
      {
        switch (parent.getNodeType())
          {
          case Node.DOCUMENT_NODE:
          case Node.DOCUMENT_FRAGMENT_NODE:
            Document resultDoc = (parent instanceof Document) ?
              (Document) parent :
              parent.getOwnerDocument();
            DOMImplementation impl = resultDoc.getImplementation();
            DocumentType doctype =
              impl.createDocumentType(resultDoc.getNodeName(),
                                      publicId,
                                      systemId);
            // Try to insert doctype before first element
            Node ctx = parent.getFirstChild();
            for (; ctx != null &&
                 ctx.getNodeType() != Node.ELEMENT_NODE;
                 ctx = ctx.getNextSibling())
              {
              }
            if (ctx != null)
              {
                parent.insertBefore(doctype, ctx);
              }
            else
              {
                parent.appendChild(doctype);
              }
          }
      }
    if (version != null)
      {
        parent.setUserData("version", version, stylesheet);
      }
    if (omitXmlDeclaration)
      {
        parent.setUserData("omit-xml-declaration", "yes", stylesheet);
      }
    if (standalone)
      {
        parent.setUserData("standalone", "yes", stylesheet);
      }
    if (mediaType != null)
      {
        parent.setUserData("media-type", mediaType, stylesheet);
      }
    if (cdataSectionElements != null)
      {
        List list = new LinkedList();
        StringTokenizer st = new StringTokenizer(cdataSectionElements);
        while (st.hasMoreTokens())
          {
            String name = st.nextToken();
            String localName = name;
            String uri = null;
            String prefix = null;
            int ci = name.indexOf(':');
            if (ci != -1)
              {
                // Use namespaces defined on xsl:output node to resolve
                // namespaces for QName
                prefix = name.substring(0, ci);
                localName = name.substring(ci + 1);
                uri = stylesheet.output.lookupNamespaceURI(prefix);
              }
            list.add(new QName(uri, localName, prefix));
          }
        if (!list.isEmpty())
          {
            Document resultDoc = (parent instanceof Document) ?
              (Document) parent :
              parent.getOwnerDocument();
            convertCdataSectionElements(resultDoc, parent, list);
          }
      }
    if (indent)
      {
        parent.normalize();
        strip(parent);
        Document resultDoc = (parent instanceof Document) ?
          (Document) parent :
          parent.getOwnerDocument();
        reindent(resultDoc, parent, 0);
      }
    // Render result to the target device
    if (outputTarget instanceof DOMResult)
      {
        if (created)
          {
            DOMResult dr = (DOMResult) outputTarget;
            dr.setNode(parent);
            dr.setNextSibling(null);
          }
      }
    else if (outputTarget instanceof StreamResult)
      {
        StreamResult sr = (StreamResult) outputTarget;
        IOException ex = null;
        try
          {
            writeStreamResult(parent, sr, outputMethod, encoding);
          }
        catch (UnsupportedEncodingException e)
          {
            try
              {
                writeStreamResult(parent, sr, outputMethod, "UTF-8");
              }
            catch (IOException e2)
              {
                ex = e2;
              }
          }
        catch (IOException e)
          {
            ex = e;
          }
        if (ex != null)
          {
            if (errorListener != null)
              {
                errorListener.error(new TransformerException(ex));
              }
            else
              {
                ex.printStackTrace(System.err);
              }
          }
      }
    else if (outputTarget instanceof SAXResult)
      {
        SAXResult sr = (SAXResult) outputTarget;
        try
          {
            ContentHandler ch = sr.getHandler();
            LexicalHandler lh = sr.getLexicalHandler();
            if (lh == null && ch instanceof LexicalHandler)
              {
                lh = (LexicalHandler) ch;
              }
            SAXSerializer serializer = new SAXSerializer();
            serializer.serialize(parent, ch, lh);
          }
        catch (SAXException e)
          {
            if (errorListener != null)
              {
                errorListener.error(new TransformerException(e));
              }
            else
              {
                e.printStackTrace(System.err);
              }
          }
      }
  }

  /**
   * Strip whitespace from the source tree.
   */
  void strip(Node node)
    throws TransformerConfigurationException
  {
    short nt = node.getNodeType();
    if (nt == Node.ENTITY_REFERENCE_NODE)
      {
        // Replace entity reference with its content
        Node parent = node.getParentNode();
        Node child = node.getFirstChild();
        if (child != null)
          {
            strip(child);
          }
        while (child != null)
          {
            Node next = child.getNextSibling();
            node.removeChild(child);
            parent.insertBefore(child, node);
            child = next;
          }
        parent.removeChild(node);
      }
    if (nt == Node.TEXT_NODE || nt == Node.CDATA_SECTION_NODE)
      {
        if (!stylesheet.isPreserved((Text) node))
          {
            node.getParentNode().removeChild(node);
          }
        else
          {
            String text = node.getNodeValue();
            String stripped = text.trim();
            if (!text.equals(stripped))
              {
                node.setNodeValue(stripped);
              }
          }
      }
    else
      {
        for (Node child = node.getFirstChild(); child != null;
             child = child.getNextSibling())
          {
            strip(child);
          }
      }
  }

  /**
   * Obtain a suitable output stream for writing the result to,
   * and use the StreamSerializer to write the result tree to the stream.
   */
  void writeStreamResult(Node node, StreamResult sr, int outputMethod,
                         String encoding)
    throws IOException
  {
    OutputStream out = null;
    try
      {
        out = sr.getOutputStream();
        if (out == null)
          {
            Writer writer = sr.getWriter();
            if (writer != null)
              {
                out = new WriterOutputStream(writer);
              }
          }
        if (out == null)
          {
            String systemId = sr.getSystemId();
            try
              {
                URL url = new URL(systemId);
                URLConnection connection = url.openConnection();
                // We need to call setDoInput(false), because our
                // implementation of the file protocol allows writing
                // (unlike Sun), but it will fail with a FileNotFoundException
                // if we also open the connection for input and the output
                // file doesn't yet exist.
                connection.setDoInput(false);
                connection.setDoOutput(true);
                out = connection.getOutputStream();
              }
            catch (MalformedURLException e)
              {
                out = new FileOutputStream(systemId);
              }
            catch (UnknownServiceException e)
              {
                URL url = new URL(systemId);
                out = new FileOutputStream(url.getPath());
              }
          }
        out = new BufferedOutputStream(out);
        StreamSerializer serializer =
          new StreamSerializer(outputMethod, encoding, null);
        if (stylesheet != null)
          {
            Collection celem = stylesheet.outputCdataSectionElements;
            serializer.setCdataSectionElements(celem);
          }
        serializer.serialize(node, out);
        out.flush();
      }
    finally
      {
        try
          {
            if (out != null)
              {
                out.close();
              }
          }
        catch (IOException e)
          {
          }
      }
  }

  void copyChildren(Document dstDoc, Node src, Node dst)
  {
    Node srcChild = src.getFirstChild();
    while (srcChild != null)
      {
        Node dstChild = dstDoc.adoptNode(srcChild);
        dst.appendChild(dstChild);
        srcChild = srcChild.getNextSibling();
      }
  }

  public void setParameter(String name, Object value)
  {
    if (stylesheet != null)
      {
        stylesheet.bindings.set(new QName(null, name), value, Bindings.PARAM);
      }
  }

  public Object getParameter(String name)
  {
    if (stylesheet != null)
      {
        return stylesheet.bindings.get(new QName(null, name), null, 1, 1);
      }
    return null;
  }

  public void clearParameters()
  {
    if (stylesheet != null)
      {
        stylesheet.bindings.pop(Bindings.PARAM);
        stylesheet.bindings.push(Bindings.PARAM);
      }
  }

  public void setURIResolver(URIResolver resolver)
  {
    uriResolver = resolver;
  }

  public URIResolver getURIResolver()
  {
    return uriResolver;
  }

  public void setOutputProperties(Properties oformat)
    throws IllegalArgumentException
  {
    if (oformat == null)
      {
        outputProperties.clear();
      }
    else
      {
        outputProperties.putAll(oformat);
      }
  }

  public Properties getOutputProperties()
  {
    return (Properties) outputProperties.clone();
  }

  public void setOutputProperty(String name, String value)
    throws IllegalArgumentException
  {
    outputProperties.put(name, value);
  }

  public String getOutputProperty(String name)
    throws IllegalArgumentException
  {
    return outputProperties.getProperty(name);
  }

  public void setErrorListener(ErrorListener listener)
  {
    errorListener = listener;
  }

  public ErrorListener getErrorListener()
  {
    return errorListener;
  }

  static final String INDENT_WHITESPACE = "  ";

  /*
   * Apply indent formatting to the given tree.
   */
  void reindent(Document doc, Node node, int offset)
  {
    if (node.hasChildNodes())
      {
        boolean markupContent = false;
        boolean textContent = false;
        List children = new LinkedList();
        Node ctx = node.getFirstChild();
        while (ctx != null)
          {
            switch (ctx.getNodeType())
              {
              case Node.ELEMENT_NODE:
              case Node.PROCESSING_INSTRUCTION_NODE:
              case Node.DOCUMENT_TYPE_NODE:
                markupContent = true;
                break;
              case Node.TEXT_NODE:
              case Node.CDATA_SECTION_NODE:
              case Node.ENTITY_REFERENCE_NODE:
              case Node.COMMENT_NODE:
                textContent = true;
                break;
              }
            children.add(ctx);
            ctx = ctx.getNextSibling();
          }
        if (markupContent)
          {
            if (textContent)
              {
                // XXX handle mixed content differently?
              }
            int nodeType = node.getNodeType();
            if (nodeType == Node.DOCUMENT_NODE)
              {
                for (Iterator i = children.iterator(); i.hasNext(); )
                  {
                    ctx = (Node) i.next();
                    reindent(doc, ctx, offset + 1);
                  }
              }
            else
              {
                StringBuffer buf = new StringBuffer();
                buf.append('\n');
                for (int i = 0; i < offset + 1; i++)
                  {
                    buf.append(INDENT_WHITESPACE);
                  }
                String ws = buf.toString();
                for (Iterator i = children.iterator(); i.hasNext(); )
                  {
                    ctx = (Node) i.next();
                    node.insertBefore(doc.createTextNode(ws), ctx);
                    reindent(doc, ctx, offset + 1);
                  }
                buf = new StringBuffer();
                buf.append('\n');
                ws = buf.toString();
                for (int i = 0; i < offset; i++)
                  {
                    buf.append(INDENT_WHITESPACE);
                  }
                node.appendChild(doc.createTextNode(ws));
              }
          }
      }
  }

  /**
   * Converts the text node children of any cdata-section-elements in the
   * tree to CDATA section nodes.
   */
  void convertCdataSectionElements(Document doc, Node node, List list)
  {
    if (node.getNodeType() == Node.ELEMENT_NODE)
      {
        boolean match = false;
        for (Iterator i = list.iterator(); i.hasNext(); )
          {
            QName qname = (QName) i.next();
            if (match(qname, node))
              {
                match = true;
                break;
              }
          }
        if (match)
          {
            Node ctx = node.getFirstChild();
            while (ctx != null)
              {
                if (ctx.getNodeType() == Node.TEXT_NODE)
                  {
                    Node cdata = doc.createCDATASection(ctx.getNodeValue());
                    node.replaceChild(cdata, ctx);
                    ctx = cdata;
                  }
                ctx = ctx.getNextSibling();
              }
          }
      }
    Node ctx = node.getFirstChild();
    while (ctx != null)
      {
        if (ctx.hasChildNodes())
          {
            convertCdataSectionElements(doc, ctx, list);
          }
        ctx = ctx.getNextSibling();
      }
  }

  boolean match(QName qname, Node node)
  {
    String ln1 = qname.getLocalPart();
    String ln2 = node.getLocalName();
    if (ln2 == null)
      {
        return ln1.equals(node.getNodeName());
      }
    else
      {
        String uri1 = qname.getNamespaceURI();
        String uri2 = node.getNamespaceURI();
        return (uri1.equals(uri2) && ln1.equals(ln2));
      }
  }

}
