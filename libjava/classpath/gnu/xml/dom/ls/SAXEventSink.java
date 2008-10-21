/* SAXEventSink.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import javax.xml.XMLConstants;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Entity;
import org.w3c.dom.EntityReference;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.Attributes2;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.ext.Locator2;
import gnu.xml.dom.DomAttr;
import gnu.xml.dom.DomDocument;
import gnu.xml.dom.DomDoctype;
import gnu.xml.dom.DomNode;

/**
 * A SAX content and lexical handler used to construct a DOM document.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class SAXEventSink
  implements ContentHandler, LexicalHandler, DTDHandler, DeclHandler
{

  private static final String XMLNS_URI = XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
  private static final String XMLNS_PREFIX = XMLConstants.XMLNS_ATTRIBUTE;
  private static final HashSet PREDEFINED_ENTITIES = new HashSet();
  static
  {
    PREDEFINED_ENTITIES.add("amp");
    PREDEFINED_ENTITIES.add("lt");
    PREDEFINED_ENTITIES.add("gt");
    PREDEFINED_ENTITIES.add("quot");
    PREDEFINED_ENTITIES.add("apos");
  }

  private boolean namespaceAware;
  boolean ignoreWhitespace;
  boolean expandEntityReferences;
  boolean ignoreComments;
  boolean coalescing;

  XMLReader reader; // reference back to the parser to get features
  
  DomDocument doc; // document being constructed
  Node ctx; // current context (parent node)
  LinkedList entityCtx; // entity context
  List pending; // namespace nodes waiting for a declaring element
  Locator locator;
  boolean inCDATA;
  boolean inDTD;
  boolean interrupted;

  void interrupt()
  {
    interrupted = true;
  }

  public Document getDocument()
  {
    return doc;
  }

  public void setReader(XMLReader reader)
  {
    this.reader = reader;
  }

  // -- ContentHandler2 --
  
  public void setDocumentLocator(Locator locator)
  {
    this.locator = locator;
  }

  public void setNamespaceAware(boolean namespaceAware)
  {
    this.namespaceAware = namespaceAware;
  }

  public void startDocument()
    throws SAXException
  {
    if (namespaceAware)
      {
        pending = new LinkedList();
      }
    doc = new DomDocument();
    doc.setStrictErrorChecking(false);
    doc.setBuilding(true);
    doc.setDefaultAttributes(false);
    ctx = doc;

    final String FEATURES = "http://xml.org/sax/features/";
    final String PROPERTIES = "http://xml.org/sax/properties/";
    final String GNU_PROPERTIES = "http://gnu.org/sax/properties/";

    if (reader != null)
      {
        boolean standalone = reader.getFeature(FEATURES + "is-standalone");
        doc.setXmlStandalone(standalone);
        try
          {
            String version = (String) reader.getProperty(PROPERTIES +
                    "document-xml-version");
            doc.setXmlVersion(version);
          }
        catch (SAXNotRecognizedException e)
          {
          }
        catch (SAXNotSupportedException e)
          {
          }
        try
          {
              String encoding = (String) reader.getProperty(GNU_PROPERTIES +
                      "document-xml-encoding");
              doc.setXmlEncoding(encoding);
          }
        catch (SAXNotRecognizedException e)
          {
          }
        catch (SAXNotSupportedException e)
          {
          }
      }
    if (locator != null && locator instanceof Locator2)
      {
        String encoding = ((Locator2) locator).getEncoding();
        doc.setInputEncoding(encoding);
      }
  }

  public void endDocument()
    throws SAXException
  {
    doc.setStrictErrorChecking(true);
    doc.setBuilding(false);
    doc.setDefaultAttributes(true);
    DomDoctype doctype = (DomDoctype) doc.getDoctype();
    if (doctype != null)
      {
        doctype.makeReadonly();
      }
    ctx = null;
    locator = null;
  }

  public void startPrefixMapping(String prefix, String uri)
    throws SAXException
  {
    if (namespaceAware)
      {
        String nsName = (prefix != null && prefix.length() > 0) ?
          XMLNS_PREFIX + ":" + prefix : XMLNS_PREFIX;
        DomAttr ns = (DomAttr) doc.createAttributeNS(XMLNS_URI, nsName);
        ns.setNodeValue(uri);
        if (ctx.getNodeType() == Node.ATTRIBUTE_NODE)
          {
            // Add to owner element
            Node target = ((Attr) ctx).getOwnerElement();
            target.getAttributes().setNamedItemNS(ns);
          }
        else
          {
            // Add to pending list; namespace node will be inserted when
            // element is seen
            pending.add(ns);
          }
      }
  }

  public void endPrefixMapping(String prefix)
    throws SAXException
  {
  }

  public void startElement(String uri, String localName, String qName,
                           Attributes atts)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Element element = createElement(uri, localName, qName, atts);
    // add element to context
    ctx.appendChild(element);
    ctx = element;
  }

  protected Element createElement(String uri, String localName, String qName,
                                  Attributes atts)
    throws SAXException
  {
    // create element node
    Element element = namespaceAware ?
      doc.createElementNS(uri, qName) :
      doc.createElement(qName);
    NamedNodeMap attrs = element.getAttributes();
    if (namespaceAware && !pending.isEmpty())
      {
        // add pending namespace nodes
        for (Iterator i = pending.iterator(); i.hasNext(); )
          {
            Node ns = (Node) i.next();
            attrs.setNamedItemNS(ns);
          }
        pending.clear();
      }
    // add attributes
    int len = atts.getLength();
    for (int i = 0; i < len; i++)
      {
        // create attribute
        Attr attr = createAttr(atts, i);
        if (attr != null)
          {
            // add attribute to element
            if (namespaceAware)
              {
                attrs.setNamedItemNS(attr);
              }
            else
              {
                attrs.setNamedItem(attr);
              }
          }
      }
    return element;
  }

  protected Attr createAttr(Attributes atts, int index)
  {
    DomAttr attr;
    if (namespaceAware)
      {
        String a_uri = atts.getURI(index);
        String a_qName = atts.getQName(index);
        attr = (DomAttr) doc.createAttributeNS(a_uri, a_qName);
      }
    else
      {
        String a_qName = atts.getQName(index);
        attr = (DomAttr) doc.createAttribute(a_qName);
      }
    attr.setNodeValue(atts.getValue(index));
    if (atts instanceof Attributes2)
      {
        Attributes2 atts2 = (Attributes2) atts;
        // TODO attr.setDeclared(atts2.isDeclared(index));
        attr.setSpecified(atts2.isSpecified(index));
      }
    return attr;
  }

  public void endElement(String uri, String localName, String qName)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (namespaceAware)
      {
        pending.clear();
      }
    ctx = ctx.getParentNode();
  }

  public void characters(char[] c, int off, int len)
    throws SAXException
  {
    if (interrupted || len < 1)
      {
        return;
      }
    ctx.appendChild(createText(c, off, len));
  }

  protected Text createText(char[] c, int off, int len)
    throws SAXException
  {
    Text text = (inCDATA && !coalescing) ?
      doc.createCDATASection(new String(c, off, len)) :
      doc.createTextNode(new String(c, off, len));
    return text;
  }

  public void ignorableWhitespace(char[] c, int off, int len)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!ignoreWhitespace)
      {
        characters(c, off, len);
      }
  }

  public void processingInstruction(String target, String data)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Node pi = createProcessingInstruction(target, data);
    ctx.appendChild(pi);
  }

  protected Node createProcessingInstruction(String target, String data)
  {
    return doc.createProcessingInstruction(target, data);
  }

  public void skippedEntity(String name)
    throws SAXException
  {
    // This callback is totally pointless
  }

  // -- LexicalHandler --
  
  public void startDTD(String name, String publicId, String systemId)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Node doctype = createDocumentType(name, publicId, systemId);
    doc.appendChild(doctype);
    ctx = doctype;
    inDTD = true;
  }

  protected Node createDocumentType(String name, String publicId,
                                    String systemId)
  {
    return new DomDoctype(doc, name, publicId, systemId);
  }

  public void endDTD()
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    inDTD = false;
    ctx = ctx.getParentNode();
  }

  public void startEntity(String name)
    throws SAXException
  {
    if (interrupted)
      return;
    DocumentType doctype = doc.getDoctype();
    if (doctype == null)
      {
        throw new SAXException("SAX parser error: " +
                               "reference to entity in undeclared doctype");
      }
    if ("[dtd]".equals(name) || name.charAt(0) == '%')
      return;
    if (PREDEFINED_ENTITIES.contains(name))
      return;
    // Get entity
    NamedNodeMap entities = doctype.getEntities();
    Entity entity = (Entity) entities.getNamedItem(name);
    if (entity == null)
      {
        throw new SAXException("SAX parser error: " +
                               "reference to undeclared entity: " + name);
      }
    EntityReference ref = doc.createEntityReference(name);
    // DomDocument populates with the entity replacement text, remove this
    Node child = ref.getFirstChild();
    while (child != null)
      {
        Node nextChild = child.getNextSibling();
        ref.removeChild(child);
        child = nextChild;
      }
    ctx.appendChild(ref);
    ctx = ref;
  }

  public void endEntity(String name)
    throws SAXException
  {
    if (interrupted)
      return;
    if ("[dtd]".equals(name) || name.charAt(0) == '%')
      return;
    if (PREDEFINED_ENTITIES.contains(name))
      return;
    // Get entity reference
    EntityReference ref = (EntityReference) ctx;
    if (!ref.getNodeName().equals(name))
      throw new SAXException("expecting end of "+ref.getNodeName()+" entity");
    ctx = ctx.getParentNode();
    if (ref instanceof DomNode)
      ((DomNode) ref).makeReadonly();
    if (expandEntityReferences)
      {
        // Move entity content from reference node onto context
        Node child = ref.getFirstChild();
        while (child != null)
          {
            Node nextChild = child.getNextSibling();
            ctx.appendChild(child);
            child = nextChild;
          }
        ctx.removeChild(ref);
      }
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

  public void comment(char[] c, int off, int len)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Node comment = createComment(c, off, len);
    ctx.appendChild(comment);
  }

  protected Node createComment(char[] c, int off, int len)
  {
    return doc.createComment(new String(c, off, len));
  }

  // -- DTDHandler --

  public void notationDecl(String name, String publicId, String systemId)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("notation decl outside DTD");
    DomDoctype doctype = (DomDoctype) ctx;
    doctype.declareNotation(name, publicId, systemId);
  }

  public void unparsedEntityDecl(String name, String publicId, String systemId,
                                 String notationName)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("unparsed entity decl outside DTD");
    DomDoctype doctype = (DomDoctype) ctx;
    Entity entity = doctype.declareEntity(name, publicId, systemId,
                                          notationName);
  }

  // -- DeclHandler --
  
  public void elementDecl(String name, String model)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("element decl outside DTD");
    // Ignore fake element declarations generated by ValidationConsumer.
    // If an element is not really declared in the DTD it will not be
    // declared in the document model.
    if (!(ctx instanceof DomDoctype))
      {
        return;
      }
    DomDoctype doctype = (DomDoctype) ctx;
    doctype.elementDecl(name, model);
  }

  public void attributeDecl(String eName, String aName, String type,
                            String mode, String value)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("attribute decl outside DTD");
    DomDoctype doctype = (DomDoctype) ctx;
    doctype.attributeDecl(eName, aName, type, mode, value);
  }

  public void internalEntityDecl(String name, String value)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("internal entity decl outside DTD");
    DomDoctype doctype = (DomDoctype) ctx;
    Entity entity = doctype.declareEntity(name, null, null, null);
    if (entity != null)
      {
        Node text = doc.createTextNode(value);
        entity.appendChild(text);
      }
  }

  public void externalEntityDecl(String name, String publicId, String systemId)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    if (!inDTD)
      throw new SAXException("external entity decl outside DTD");
    DomDoctype doctype = (DomDoctype) ctx;
    Entity entity = doctype.declareEntity(name, publicId, systemId, null);
  }
  
}

