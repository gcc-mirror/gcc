/* DomDocument.java -- 
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
exception statement from your version. */

package gnu.xml.dom;

import java.util.Iterator;
import javax.xml.XMLConstants;

import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Entity;
import org.w3c.dom.EntityReference;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Notation;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;
import org.w3c.dom.UserDataHandler;
import org.w3c.dom.traversal.DocumentTraversal;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.NodeIterator;
import org.w3c.dom.traversal.TreeWalker;
import org.w3c.dom.xpath.XPathEvaluator;
import org.w3c.dom.xpath.XPathException;
import org.w3c.dom.xpath.XPathExpression;
import org.w3c.dom.xpath.XPathNSResolver;

/**
 * <p> "Document" and "DocumentTraversal" implementation.
 *
 * <p> Note that when this checks names for legality, it uses an
 * approximation of the XML rules, not the real ones.  Specifically,
 * it uses Unicode rules, with sufficient tweaks to pass a majority
 * of basic XML conformance tests.  (The huge XML character tables are
 * hairy to implement.)
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomDocument
  extends DomNode
  implements Document, DocumentTraversal, XPathEvaluator
{

  private final DOMImplementation implementation;
  private boolean checkingCharacters = true;
  boolean checkingWellformedness = true;

  boolean building; // if true, skip mutation events in the tree
  
  DomDocumentConfiguration config;

  String inputEncoding;
  String encoding;
  String version = "1.0";
  boolean standalone;
  String systemId;
  
  /**
   * Constructs a Document node, associating it with an instance
   * of the DomImpl class.
   *
   * <p> Note that this constructor disables character checking.
   * It is normally used when connecting a DOM to an XML parser,
   * and duplicating such checks is undesirable.  When used for
   * purposes other than connecting to a parser, you should
   * re-enable that checking.
   *
   * @see #setCheckingCharacters
   */
  public DomDocument()
  {
    this(new DomImpl());
  }
  
  /**
   * Constructs a Document node, associating it with the specified
   * implementation.  This should only be used in conjunction with
   * a specialized implementation; it will normally be called by
   * that implementation.
   *
   * @see DomImpl
   * @see #setCheckingCharacters
   */
  protected DomDocument(DOMImplementation impl)
  {
    super(DOCUMENT_NODE, null);
    implementation = impl;
  }

  /**
   * Sets the <code>building</code> flag.
   * Mutation events in the document are not reported.
   */
  public void setBuilding(boolean flag)
  {
    building = flag;
  }

  /**
   * Sets whether to check for document well-formedness.
   * If true, an exception will be raised if a second doctype or root
   * element node is added to the document.
   */
  public void setCheckWellformedness(boolean flag)
  {
    checkingWellformedness = flag;
  }

  /**
   * <b>DOM L1</b>
   * Returns the constant "#document".
   */
  final public String getNodeName()
  {
    return "#document";
  }

  /**
   * <b>DOM L1</b>
   * Returns the document's root element, or null.
   */
  final public Element getDocumentElement()
  {
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        if (ctx.nodeType == ELEMENT_NODE)
          {
            return (Element) ctx;
          }
      }
    return null;
  }

  /**
   * <b>DOM L1</b>
   * Returns the document's DocumentType, or null.
   */
  final public DocumentType getDoctype()
  {
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
      if (ctx.nodeType == DOCUMENT_TYPE_NODE)
          {
            return (DocumentType) ctx;
          }
      }
    return null;
  }

  /**
   * <b>DOM L1</b>
   * Returns the document's DOMImplementation.
   */
  final public DOMImplementation getImplementation()
  {
    return implementation;
  }

  /**
   * <b>DOM L1 (relocated in DOM L2)</b>
   * Returns the element with the specified "ID" attribute, or null.
   *
   * <p>Returns null unless {@link Consumer} was used to populate internal
   * DTD declaration information, using package-private APIs.  If that
   * internal DTD information is available, the document may be searched for
   * the element with that ID.
   */
  public Element getElementById(String id)
  {
    DomDoctype doctype = (DomDoctype) getDoctype();
    
    if (doctype == null || !doctype.hasIds()
        || id == null || id.length() == 0)
      {
        return null;
      }
    
    // yes, this is linear in size of document.
    // it'd be easy enough to maintain a hashtable.
    Node current = getDocumentElement();
    Node temp;
    
    if (current == null)
      {
        return null;
      }
    while (current != this)
      {
        // done?
        if (current.getNodeType() == ELEMENT_NODE)
          {
            DomElement element = (DomElement) current;
            DTDElementTypeInfo info =
              doctype.getElementTypeInfo(current.getNodeName());
            if (info != null &&
                id.equals(element.getAttribute(info.idAttrName)))
              {
                return element;
              }
            else if (element.userIdAttrs != null)
              {
                for (Iterator i = element.userIdAttrs.iterator();
                     i.hasNext(); )
                  {
                    Node idAttr = (Node) i.next();
                    if (id.equals(idAttr.getNodeValue()))
                      {
                        return element;
                      }
                  }
              }
          }
        
        // descend?
        if (current.hasChildNodes())
          {
            current = current.getFirstChild();
            continue;
          }
        
        // lateral?
        temp = current.getNextSibling();
        if (temp != null)
          {
            current = temp;
            continue;
          }
        
        // back up ... 
        do
          {
            temp = current.getParentNode();
            if (temp == null)
              {
                return null;
              }
            current = temp;
            temp = current.getNextSibling();
          }
        while (temp == null);
        current = temp;
      }
    return null;
  }

  private void checkNewChild(Node newChild)
  {
    if (newChild.getNodeType() == ELEMENT_NODE
        && getDocumentElement() != null)
      {
        throw new DomDOMException(DOMException.HIERARCHY_REQUEST_ERR,
                                  "document element already present: " +
                                  getDocumentElement(), newChild, 0);
      }
    if (newChild.getNodeType() == DOCUMENT_TYPE_NODE
        && getDoctype() != null)
      {
        throw new DomDOMException(DOMException.HIERARCHY_REQUEST_ERR,
                                  "document type already present: " +
                                  getDoctype(), newChild, 0);
      }
  }

  /**
   * <b>DOM L1</b>
   * Appends the specified node to this node's list of children,
   * enforcing the constraints that there be only one root element
   * and one document type child.
   */
  public Node appendChild(Node newChild)
  {
    if (checkingWellformedness)
      {
        checkNewChild(newChild);
      }
    return super.appendChild(newChild);
  }

  /**
   * <b>DOM L1</b>
   * Inserts the specified node in this node's list of children,
   * enforcing the constraints that there be only one root element
   * and one document type child.
   */
  public Node insertBefore(Node newChild, Node refChild)
  {
    if (checkingWellformedness)
      {
        checkNewChild(newChild);
      }
    return super.insertBefore(newChild, refChild);
  }

  /**
   * <b>DOM L1</b>
   * Replaces the specified node in this node's list of children,
   * enforcing the constraints that there be only one root element
   * and one document type child.
   */
  public Node replaceChild(Node newChild, Node refChild)
  {
    if (checkingWellformedness &&
        ((newChild.getNodeType() == ELEMENT_NODE &&
          refChild.getNodeType() != ELEMENT_NODE) ||
         (newChild.getNodeType() == DOCUMENT_TYPE_NODE &&
          refChild.getNodeType() != DOCUMENT_TYPE_NODE)))
      {
        checkNewChild(newChild);
      }
    return super.replaceChild(newChild, refChild);
  }
 
  // NOTE:  DOM can't really tell when the name of an entity,
  // notation, or PI must follow the namespace rules (excluding
  // colons) instead of the XML rules (which allow them without
  // much restriction).  That's an API issue.  verifyXmlName
  // aims to enforce the XML rules, not the namespace rules.
  
  /**
   * Throws a DOM exception if the specified name is not a legal XML 1.0
   * Name.
   * @deprecated This method is deprecated and may be removed in future
   * versions of GNU JAXP
   */
  public static void verifyXmlName(String name)
  {
    // XXX why is this public?
    checkName(name, false);
  }

  static void checkName(String name, boolean xml11)
  {
    if (name == null)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR, name, null, 0);
      }
    int len = name.length();
    if (len == 0)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR, name, null, 0);
      }

    // dog: rewritten to use the rules for XML 1.0 and 1.1
    
    // Name start character
    char c = name.charAt(0);
    if (xml11)
      {
        // XML 1.1
        if ((c < 0x0041 || c > 0x005a) &&
            (c < 0x0061 || c > 0x007a) &&
            c != ':' && c != '_' &&
            (c < 0x00c0 || c > 0x00d6) &&
            (c < 0x00d8 || c > 0x00f6) &&
            (c < 0x00f8 || c > 0x02ff) &&
            (c < 0x0370 || c > 0x037d) &&
            (c < 0x037f || c > 0x1fff) &&
            (c < 0x200c || c > 0x200d) &&
            (c < 0x2070 || c > 0x218f) &&
            (c < 0x2c00 || c > 0x2fef) &&
            (c < 0x3001 || c > 0xd7ff) &&
            (c < 0xf900 || c > 0xfdcf) &&
            (c < 0xfdf0 || c > 0xfffd) &&
            (c < 0x10000 || c > 0xeffff))
          {
            throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                      name, null, c);
          }
      }
    else
      {
        // XML 1.0
        int type = Character.getType(c);
        switch (type)
          {
          case Character.LOWERCASE_LETTER: // Ll
          case Character.UPPERCASE_LETTER: // Lu
          case Character.OTHER_LETTER: // Lo
          case Character.TITLECASE_LETTER: // Lt
          case Character.LETTER_NUMBER: // Nl
            if ((c > 0xf900 && c < 0xfffe) ||
                (c >= 0x20dd && c <= 0x20e0))
              {
                // Compatibility area and Unicode 2.0 exclusions
                throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                          name, null, c);
              }
            break;
          default:
            if (c != ':' && c != '_' && (c < 0x02bb || c > 0x02c1) &&
                c != 0x0559 && c != 0x06e5 && c != 0x06e6)
              {
                throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                          name, null, c);
              }
          }
      }

    // Subsequent characters
    for (int i = 1; i < len; i++)
      {
        c = name.charAt(i);
        if (xml11)
          {
            // XML 1.1
            if ((c < 0x0041 || c > 0x005a) &&
                (c < 0x0061 || c > 0x007a) &&
                (c < 0x0030 || c > 0x0039) &&
                c != ':' && c != '_' && c != '-' && c != '.' &&
                (c < 0x00c0 || c > 0x00d6) &&
                (c < 0x00d8 || c > 0x00f6) &&
                (c < 0x00f8 || c > 0x02ff) &&
                (c < 0x0370 || c > 0x037d) &&
                (c < 0x037f || c > 0x1fff) &&
                (c < 0x200c || c > 0x200d) &&
                (c < 0x2070 || c > 0x218f) &&
                (c < 0x2c00 || c > 0x2fef) &&
                (c < 0x3001 || c > 0xd7ff) &&
                (c < 0xf900 || c > 0xfdcf) &&
                (c < 0xfdf0 || c > 0xfffd) &&
                (c < 0x10000 || c > 0xeffff) &&
                c != 0x00b7 &&
                (c < 0x0300 || c > 0x036f) &&
                (c < 0x203f || c > 0x2040))
              {
                throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR, name,
                                          null, c);
              }
          }
        else
          {
            // XML 1.0
            int type = Character.getType(c);
            switch (type)
              {
              case Character.LOWERCASE_LETTER: // Ll
              case Character.UPPERCASE_LETTER: // Lu
              case Character.DECIMAL_DIGIT_NUMBER: // Nd
              case Character.OTHER_LETTER: // Lo
              case Character.TITLECASE_LETTER: // Lt
              case Character.LETTER_NUMBER: // Nl
              case Character.COMBINING_SPACING_MARK: // Mc
              case Character.ENCLOSING_MARK: // Me
              case Character.NON_SPACING_MARK: // Mn
              case Character.MODIFIER_LETTER: // Lm
                if ((c > 0xf900 && c < 0xfffe) ||
                    (c >= 0x20dd && c <= 0x20e0))
                  {
                    // Compatibility area and Unicode 2.0 exclusions
                    throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                              name, null, c);
                  }
                break;
              default:
                if (c != '-' && c != '.' && c != ':' && c != '_' &&
                    c != 0x0387 && (c < 0x02bb || c > 0x02c1) &&
                    c != 0x0559 && c != 0x06e5 && c != 0x06e6 && c != 0x00b7)
                  {
                    throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                              name, null, c);
                  }
              }
          }
      }

    // FIXME characters with a font or compatibility decomposition (i.e.
    // those with a "compatibility formatting tag" in field 5 of the
    // database -- marked by field 5 beginning with a "<") are not allowed.
  }

  // package private
  static void checkNCName(String name, boolean xml11)
  {
    checkName(name, xml11);
    int len = name.length();
    int index = name.indexOf(':');
    if (index != -1)
      {
        if (index == 0 || index == (len - 1) ||
            name.lastIndexOf(':') != index)
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      name, null, 0);
          }
      }
  }

  // package private
  static void checkChar(String value, boolean xml11)
  {
    char[] chars = value.toCharArray();
    checkChar(chars, 0, chars.length, xml11);
  }
  
  static void checkChar(char[] buf, int off, int len, boolean xml11)
  {
    for (int i = 0; i < len; i++)
      {
        char c = buf[i];
        
        // assume surrogate pairing checks out OK, for simplicity
        if ((c >= 0x0020 && c <= 0xd7ff) ||
            (c == 0x000a || c == 0x000d || c == 0x0009) ||
            (c >= 0xe000 && c <= 0xfffd) ||
            (c >= 0x10000 && c <= 0x10ffff))
          {
            continue;
          }
        if (xml11)
          {
            if ((c >= 0x0001 && c <= 0x001f) ||
                (c >= 0x007f && c <= 0x0084) ||
                (c >= 0x0086 && c <= 0x009f))
              {
                continue;
              }
          }
        throw new DomDOMException(DOMException.INVALID_CHARACTER_ERR,
                                  new String(buf, off, len), null, c);
      }
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created element with the specified name.
   */
  public Element createElement(String name)
  {
    Element element;
    
    if (checkingCharacters)
      {
        checkName(name, "1.1".equals(version));
      }
    if (name.startsWith("xml:"))
      {
        element = createElementNS(null, name);
      }
    else
      {
        DomElement domElement = new DomElement(this, null, name);
        domElement.localName = null;
        element = domElement;
      }
    defaultAttributes(element, name);
    return element;
  }

  /**
   * <b>DOM L2</b>
   * Returns a newly created element with the specified name
   * and namespace information.
   */
  public Element createElementNS(String namespaceURI, String name)
  {
    if (checkingCharacters)
      {
        checkNCName(name, "1.1".equals(version));
      }
    
    if ("".equals(namespaceURI))
      {
        namespaceURI = null;
      }
    if (name.startsWith("xml:"))
      {
        if (namespaceURI != null
            && !XMLConstants.XML_NS_URI.equals(namespaceURI))
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      "xml namespace is always " +
                                      XMLConstants.XML_NS_URI, this, 0);
          }
        namespaceURI = XMLConstants.XML_NS_URI;
      }
    else if (XMLConstants.XMLNS_ATTRIBUTE.equals(name) ||
             name.startsWith("xmlns:"))
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "xmlns is reserved", this, 0);
      }
    else if (namespaceURI == null && name.indexOf(':') != -1)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "prefixed name '" + name +
                                  "' needs a URI", this, 0);
      }
    
    Element  element = new DomElement(this, namespaceURI, name);
    defaultAttributes(element, name);
    return element;
  }
  
  private void defaultAttributes(Element element, String name)
  {
    DomDoctype doctype = (DomDoctype) getDoctype();
    if (doctype == null)
      {
        return;
      }

    // default any attributes that need it
    DTDElementTypeInfo info = doctype.getElementTypeInfo(name);
    if (info != null)
      {
        for (Iterator i = info.attributes(); i != null && i.hasNext(); )
          {
            DTDAttributeTypeInfo attr = (DTDAttributeTypeInfo) i.next();
            DomAttr node = (DomAttr) createAttribute(attr.name);
            
            String value = attr.value;
            if (value == null)
              {
                value = "";
              }
            node.setValue(value);
            node.setSpecified(false);
            element.setAttributeNode(node);
          }
      }
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created document fragment.
   */
  public DocumentFragment createDocumentFragment()
  {
    return new DomDocumentFragment(this);
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created text node with the specified value.
   */
  public Text createTextNode(String value)
  {
    if (checkingCharacters)
      {
        checkChar(value, "1.1".equals(version));
      }
    return new DomText(this, value);
  }

  /**
   * Returns a newly created text node with the specified value.
   */
  public Text createTextNode(char[] buf, int off, int len)
  {
    if (checkingCharacters)
      {
        checkChar(buf, off, len, "1.1".equals(version));
      }
    return new DomText(this, buf, off, len);
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created comment node with the specified value.
   */
  public Comment createComment(String value)
  {
    if (checkingCharacters)
      {
        checkChar(value, "1.1".equals(version));
      }
    return new DomComment(this, value);
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created CDATA section node with the specified value.
   */
  public CDATASection createCDATASection(String value)
  {
    if (checkingCharacters)
      {
        checkChar(value, "1.1".equals(version));
      }
    return new DomCDATASection(this, value);
  }

  /**
   * Returns a newly created CDATA section node with the specified value.
   */
  public CDATASection createCDATASection(char[] buf, int off, int len)
  {
    if (checkingCharacters)
      {
        checkChar(buf, off, len, "1.1".equals(version));
      }
    return new DomCDATASection(this, buf, off, len);
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created processing instruction.
   */
  public ProcessingInstruction createProcessingInstruction(String target,
                                                           String data)
  {
    if (checkingCharacters)
      {
        boolean xml11 = "1.1".equals(version);
        checkName(target, xml11);
        if ("xml".equalsIgnoreCase(target))
          {
            throw new DomDOMException(DOMException.SYNTAX_ERR,
                                      "illegal PI target name",
                                      this, 0);
          }
        checkChar(data, xml11);
      }
    return new DomProcessingInstruction(this, target, data);
  }

  /**
   * <b>DOM L1</b>
   * Returns a newly created attribute with the specified name.
   */
  public Attr createAttribute(String name)
  {
    if (checkingCharacters)
      {
        checkName(name, "1.1".equals(version));
      }
    if (name.startsWith("xml:"))
      {
        return createAttributeNS(XMLConstants.XML_NS_URI, name);
      }
    else if (XMLConstants.XMLNS_ATTRIBUTE.equals(name) ||
             name.startsWith("xmlns:"))
      {
        return createAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, name);
      }
    else
      {
        DomAttr ret = new DomAttr(this, null, name);
        ret.localName = null;
        return ret;
      }
  }

  /**
   * <b>DOM L2</b>
   * Returns a newly created attribute with the specified name
   * and namespace information.
   */
  public Attr createAttributeNS(String namespaceURI, String name)
  {
    if (checkingCharacters)
      {
        checkNCName(name, "1.1".equals(version));
      }
    
    if ("".equals(namespaceURI))
      {
        namespaceURI = null;
      }
    if (name.startsWith ("xml:"))
      {
        if (namespaceURI == null)
          {
            namespaceURI = XMLConstants.XML_NS_URI;
          }
        else if (!XMLConstants.XML_NS_URI.equals(namespaceURI))
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      "xml namespace is always " +
                                      XMLConstants.XML_NS_URI,
                                      this, 0);
          }
      }
    else if (XMLConstants.XMLNS_ATTRIBUTE.equals(name) ||
             name.startsWith("xmlns:"))
      {
        if (namespaceURI == null)
          {
            namespaceURI = XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
          }
        else if (!XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI))
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      "xmlns namespace must be " +
                                      XMLConstants.XMLNS_ATTRIBUTE_NS_URI,
                                      this, 0);
          }
      }
    else if (namespaceURI == null && name.indexOf(':') != -1)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                        "prefixed name needs a URI: " + name, this, 0);
      }
    return new DomAttr(this, namespaceURI, name);
  }
  
  /**
   * <b>DOM L1</b>
   * Returns a newly created reference to the specified entity.
   * The caller should populate this with the appropriate children
   * and then mark it as readonly.
   *
   * @see DomNode#makeReadonly
   */
  public EntityReference createEntityReference(String name)
  {
    DomEntityReference ret = new DomEntityReference(this, name);
    DocumentType doctype = getDoctype();
    if (doctype != null)
      {
        DomEntity ent = (DomEntity) doctype.getEntities().getNamedItem(name);
        if (ent != null)
          {
            for (DomNode ctx = ent.first; ctx != null; ctx = ctx.next)
              {
                ret.appendChild(ctx.cloneNode(true));
              }
          }
      }
    ret.makeReadonly();
    return ret;
  }

  /**
   * <b>DOM L2</b>
   * Makes a copy of the specified node, with all nodes "owned" by
   * this document and with children optionally copied.  This type
   * of standard utility has become, well, a standard utility.
   *
   * <p> Note that EntityReference nodes created through this method (either
   * directly, or recursively) never have children, and that there is no
   * portable way to associate them with such children.
   *
   * <p> Note also that there is no requirement that the specified node
   * be associated with a different document.  This differs from the
   * <em>cloneNode</em> operation in that the node itself is not given
   * an opportunity to participate, so that any information managed
   * by node subclasses will be lost.
   */
  public Node importNode(Node src, boolean deep)
  {
    Node dst = null;
    switch (src.getNodeType())
      {
      case TEXT_NODE:
        dst = createTextNode(src.getNodeValue());
        break;
      case CDATA_SECTION_NODE:
        dst = createCDATASection(src.getNodeValue());
        break;
      case COMMENT_NODE:
        dst = createComment(src.getNodeValue());
        break;
      case PROCESSING_INSTRUCTION_NODE:
        dst = createProcessingInstruction(src.getNodeName(),
                                          src.getNodeValue());
        break;
      case NOTATION_NODE:
        // NOTE:  There's no standard way to create
        // these, or add them to a doctype.  Useless.
        Notation notation = (Notation) src;
        dst = new DomNotation(this, notation.getNodeName(),
                              notation.getPublicId(),
                              notation.getSystemId());
        break;
      case ENTITY_NODE:
        // NOTE:  There's no standard way to create
        // these, or add them to a doctype.  Useless.
        Entity entity = (Entity) src;
        dst = new DomEntity(this, entity.getNodeName(),
                            entity.getPublicId(),
                            entity.getSystemId(),
                            entity.getNotationName());
        if (deep)
          {
            for (Node ctx = src.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                dst.appendChild(importNode(ctx, deep));
              }
          }
        break;
      case ENTITY_REFERENCE_NODE:
        dst = createEntityReference(src.getNodeName());
        break;
      case DOCUMENT_FRAGMENT_NODE:
        dst = new DomDocumentFragment(this);
        if (deep)
          {
            for (Node ctx = src.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                dst.appendChild(importNode(ctx, deep));
              }
          }
        break;
      case ATTRIBUTE_NODE:
        String attr_nsuri = src.getNamespaceURI();
        if (attr_nsuri != null)
          {
            dst = createAttributeNS(attr_nsuri, src.getNodeName());
          }
        else
          {
            dst = createAttribute(src.getNodeName());
          }
        // this is _always_ done regardless of "deep" setting
        for (Node ctx = src.getFirstChild(); ctx != null;
             ctx = ctx.getNextSibling())
          {
            dst.appendChild(importNode(ctx, false));
          }
        break;
      case ELEMENT_NODE:
        String elem_nsuri = src.getNamespaceURI();
        if (elem_nsuri != null)
          {
            dst = createElementNS(elem_nsuri, src.getNodeName());
          }
        else
          {
            dst = createElement(src.getNodeName());
          }
        NamedNodeMap srcAttrs = src.getAttributes();
        NamedNodeMap dstAttrs = dst.getAttributes();
        int len = srcAttrs.getLength();
        for (int i = 0; i < len; i++)
          {
            Attr a = (Attr) srcAttrs.item(i);
            Attr dflt;
            
            // maybe update defaulted attributes
            dflt = (Attr) dstAttrs.getNamedItem(a.getNodeName());
            if (dflt != null)
              {
                String newval = a.getNodeValue();
                if (!dflt.getNodeValue().equals(newval)
                    || a.getSpecified () == true)
                  {
                    dflt.setNodeValue (newval);
                  }
                continue;
              }
            
            dstAttrs.setNamedItem((Attr) importNode(a, false));
          }
        if (deep)
          {
            for (Node ctx = src.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                dst.appendChild(importNode(ctx, true));
              }
          }
        break;
        // can't import document or doctype nodes
      case DOCUMENT_NODE:
      case DOCUMENT_TYPE_NODE:
        // FALLTHROUGH
        // can't import unrecognized or nonstandard nodes
      default:
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR, null, src, 0);
      }
    
    // FIXME cleanup a bit -- for deep copies, copy those
    // children in one place, here (code sharing is healthy)

    if (src instanceof DomNode)
      {
        ((DomNode) src).notifyUserDataHandlers(UserDataHandler.NODE_IMPORTED,
                                               src, dst);
      }
    return dst;
  }

  /**
   * <b>DOM L2 (Traversal)</b>
   * Returns a newly created node iterator.  Don't forget to detach
   * this iterator when you're done using it!
   *
   * @see DomIterator
   */
  public NodeIterator createNodeIterator(Node root,
                                         int whatToShow,
                                         NodeFilter filter,
                                         boolean expandEntities)
  {
    return new DomNodeIterator(root, whatToShow, filter, expandEntities,
                               false);
  }

  public TreeWalker createTreeWalker(Node root,
                                     int whatToShow,
                                     NodeFilter filter,
                                     boolean expandEntities)
  {
    return new DomNodeIterator(root, whatToShow, filter, expandEntities,
                               true);
  }

  // DOM Level 3 methods
  
  /**
   * DOM L3
   */
  public String getInputEncoding()
  {
    return inputEncoding;
  }

  public void setInputEncoding(String inputEncoding)
  {
    this.inputEncoding = inputEncoding;
  }
  
  /**
   * DOM L3
   */
  public String getXmlEncoding()
  {
    return encoding;
  }
  
  public void setXmlEncoding(String encoding)
  {
    this.encoding = encoding;
  }
  
  public boolean getXmlStandalone()
  {
    return standalone;
  }

  public void setXmlStandalone(boolean xmlStandalone)
  {
    standalone = xmlStandalone;
  }

  public String getXmlVersion()
  {
    return version;
  }

  public void setXmlVersion(String xmlVersion)
  {
    if (xmlVersion == null)
      {
        xmlVersion = "1.0";
      }
    if ("1.0".equals(xmlVersion) ||
        "1.1".equals(xmlVersion))
      {
        version = xmlVersion;
      }
    else
      {
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
      }
  }

  public boolean getStrictErrorChecking()
  {
    return checkingCharacters;
  }

  public void setStrictErrorChecking(boolean strictErrorChecking)
  {
    checkingCharacters = strictErrorChecking;
  }

  public String lookupPrefix(String namespaceURI)
  {
    Node root = getDocumentElement();
    return (root == null) ? null : root.lookupPrefix(namespaceURI);
  }

  public boolean isDefaultNamespace(String namespaceURI)
  {
    Node root = getDocumentElement();
    return (root == null) ? false : root.isDefaultNamespace(namespaceURI);
  }

  public String lookupNamespaceURI(String prefix)
  {
    Node root = getDocumentElement();
    return (root == null) ? null : root.lookupNamespaceURI(prefix);
  }

  public String getBaseURI()
  {
    return getDocumentURI();
    /*
    Node root = getDocumentElement();
    if (root != null)
      {
        NamedNodeMap attrs = root.getAttributes();
        Node xmlBase = attrs.getNamedItemNS(XMLConstants.XML_NS_URI, "base");
        if (xmlBase != null)
          {
            return xmlBase.getNodeValue();
          }
      }
    return systemId;
    */
  }
  
  public String getDocumentURI()
  {
    return systemId;
  }

  public void setDocumentURI(String documentURI)
  {
    systemId = documentURI;
  }

  public Node adoptNode(Node source)
  {
    int sourceNodeType = source.getNodeType();
    switch (sourceNodeType)
      {
      case DOCUMENT_NODE:
      case DOCUMENT_TYPE_NODE:
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
      case ENTITY_NODE:
      case NOTATION_NODE:
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    if (source instanceof DomNode)
      {
        // GNU native
        DomNode src = (DomNode) source;
        DomNode dst = src;
        if (dst.parent != null)
          {
            dst = (DomNode) dst.cloneNode(true);
          }
        dst.setOwner(this);
        src.notifyUserDataHandlers(UserDataHandler.NODE_ADOPTED, src, dst);
        return dst;
      }
    else
      {
        // Some other implementation
        Node dst = null;
        switch (sourceNodeType)
          {
          case Node.ATTRIBUTE_NODE:
              {
                Attr src = (Attr) source;
                String nodeName = src.getNodeName();
                String localName = src.getLocalName();
                String namespaceUri = src.getNamespaceURI();
                dst = (localName == null) ?
                  createAttribute(nodeName) :
                  createAttributeNS(namespaceUri, nodeName);
                adoptChildren(src, dst);
                break;
              }
          case Node.CDATA_SECTION_NODE:
              {
                CDATASection src = (CDATASection) source;
                dst = createCDATASection(src.getData());
                break;
              }
          case Node.COMMENT_NODE:
              {
                Comment src = (Comment) source;
                dst = createComment(src.getData());
                break;
              }
          case Node.DOCUMENT_FRAGMENT_NODE:
              {
                DocumentFragment src = (DocumentFragment) source;
                dst = createDocumentFragment();
                adoptChildren(src, dst);
                break;
              }
          case Node.ELEMENT_NODE:
              {
                Element src = (Element) source;
                String nodeName = src.getNodeName();
                String localName = src.getLocalName();
                String namespaceUri = src.getNamespaceURI();
                dst = (localName == null) ?
                  createElement(nodeName) :
                  createElementNS(namespaceUri, nodeName);
                adoptAttributes(src, dst);
                adoptChildren(src, dst);
                break;
              }
          case Node.ENTITY_REFERENCE_NODE:
              {
                EntityReference src = (EntityReference) source;
                dst = createEntityReference(src.getNodeName());
                adoptChildren(src, dst);
                break;
              }
          case Node.PROCESSING_INSTRUCTION_NODE:
              {
                ProcessingInstruction src = (ProcessingInstruction) source;
                dst = createProcessingInstruction(src.getTarget(),
                                                  src.getData());
                break;
              }
          case Node.TEXT_NODE:
              {
                Text src = (Text) source;
                dst = createTextNode(src.getData());
                break;
              }
          }
        return dst;
      }
  }

  void adoptChildren(Node src, Node dst)
  {
    Node node = src.getFirstChild();
    while (node != null)
      {
        Node next = node.getNextSibling();
        dst.appendChild(adoptNode(node));
        node = next;
      }
  }

  void adoptAttributes(Node src, Node dst)
  {
    NamedNodeMap srcAttrs = src.getAttributes();
    NamedNodeMap dstAttrs = dst.getAttributes();
    int len = srcAttrs.getLength();
    for (int i = 0; i < len; i++)
      {
        Node node = srcAttrs.item(i);
        String localName = node.getLocalName();
        if (localName == null)
          {
            dstAttrs.setNamedItem(adoptNode(node));
          }
        else
          {
            dstAttrs.setNamedItemNS(adoptNode(node));
          }
      }
  }

  public DOMConfiguration getDomConfig()
  {
    if (config == null)
      {
        config = new DomDocumentConfiguration();
      }
    return config;
  }

  public void normalizeDocument()
  {
    boolean save = building;
    building = true;
    normalizeNode(this);
    building = save;
  }

  void normalizeNode(DomNode node)
  {
    node.normalize();
    if (config != null)
      {
        switch (node.nodeType)
          {
          case CDATA_SECTION_NODE:
            if (!config.cdataSections)
              {
                // replace CDATA section with text node
                Text text = createTextNode(node.getNodeValue());
                node.parent.insertBefore(text, node);
                node.parent.removeChild(node);
                // merge adjacent text nodes
                String data = text.getWholeText();
                node = (DomNode) text.replaceWholeText(data);
              }
            else if (config.splitCdataSections)
              {
                String value = node.getNodeValue();
                int i = value.indexOf("]]>");
                while (i != -1)
                  {
                    Node node2 = createCDATASection(value.substring(0, i));
                    node.parent.insertBefore(node2, node);
                    value = value.substring(i + 3);
                    node.setNodeValue(value);
                    i = value.indexOf("]]>");
                  }
              }
            break;
          case COMMENT_NODE:
            if (!config.comments)
              {
                node.parent.removeChild(node);
              }
            break;
          case TEXT_NODE:
            if (!config.elementContentWhitespace &&
                ((Text) node).isElementContentWhitespace())
              {
                node.parent.removeChild(node);
              }
            break;
          case ENTITY_REFERENCE_NODE:
            if (!config.entities)
              {
                for (DomNode ctx = node.first; ctx != null; )
                  {
                    DomNode ctxNext = ctx.next;
                    node.parent.insertBefore(ctx, node);
                    ctx = ctxNext;
                  }
                node.parent.removeChild(node);
              }
            break;
          case ELEMENT_NODE:
            if (!config.namespaceDeclarations)
              {
                DomNamedNodeMap attrs =
                  (DomNamedNodeMap) node.getAttributes();
                boolean aro = attrs.readonly;
                attrs.readonly = false; // Ensure we can delete if necessary
                int len = attrs.getLength();
                for (int i = 0; i < len; i++)
                  {
                    Node attr = attrs.item(i);
                    String namespace = attr.getNamespaceURI();
                    if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespace))
                      {
                        attrs.removeNamedItemNS(namespace,
                                                attr.getLocalName());
                        i--;
                        len--;
                      }
                  }
                attrs.readonly = aro;
              }
            break;
          }
      }
    for (DomNode ctx = node.first; ctx != null; )
      {
        DomNode ctxNext = ctx.next;
        normalizeNode(ctx);
        ctx = ctxNext;
      }
  }
  
  public Node renameNode(Node n, String namespaceURI, String qualifiedName)
    throws DOMException
  {
    if (n instanceof DomNsNode)
      {
        DomNsNode src = (DomNsNode) n;
        if (src == null)
          {
            throw new DomDOMException(DOMException.NOT_FOUND_ERR);
          }
        if (src.owner != this)
          {
            throw new DomDOMException(DOMException.WRONG_DOCUMENT_ERR,
                                      null, src, 0);
          }
        boolean xml11 = "1.1".equals(version);
        checkName(qualifiedName, xml11);
        int ci = qualifiedName.indexOf(':');
        if ("".equals(namespaceURI))
          {
            namespaceURI = null;
          }
        if (namespaceURI != null)
          {
            checkNCName(qualifiedName, xml11);
            String prefix = (ci == -1) ? "" :
              qualifiedName.substring(0, ci);
            if (XMLConstants.XML_NS_PREFIX.equals(prefix) &&
                !XMLConstants.XML_NS_URI.equals(namespaceURI))
              {
                throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                "xml namespace must be " +
                                XMLConstants.XML_NS_URI, src, 0);
              }
            else if (src.nodeType == ATTRIBUTE_NODE &&
                     (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix) ||
                      XMLConstants.XMLNS_ATTRIBUTE.equals(qualifiedName)) &&
                     !XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI))
              {
                throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                "xmlns namespace must be " +
                                XMLConstants.XMLNS_ATTRIBUTE_NS_URI, src, 0);
              }
            if (XMLConstants.XML_NS_URI.equals(namespaceURI) &&
                !XMLConstants.XML_NS_PREFIX.equals(prefix))
              {
                throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                "xml namespace must be " +
                                XMLConstants.XML_NS_URI, src, 0);
              }
            else if (src.nodeType == ATTRIBUTE_NODE &&
                     XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceURI) &&
                     !(XMLConstants.XMLNS_ATTRIBUTE.equals(prefix) ||
                       XMLConstants.XMLNS_ATTRIBUTE.equals(qualifiedName)))
              {
                throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                "xmlns namespace must be " +
                                XMLConstants.XMLNS_ATTRIBUTE_NS_URI, src, 0);
              }
                
          }
        src.setNodeName(qualifiedName);
        src.setNamespaceURI(namespaceURI);
        src.notifyUserDataHandlers(UserDataHandler.NODE_RENAMED, src, src);
        // TODO MutationNameEvents
        // DOMElementNameChanged or DOMAttributeNameChanged
        return src;
      }
    throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR, null, n, 0);
  }

  // -- XPathEvaluator --
  
  public XPathExpression createExpression(String expression,
                                          XPathNSResolver resolver)
    throws XPathException, DOMException
  {
    return new DomXPathExpression(this, expression, resolver);
  }
  
  public XPathNSResolver createNSResolver(Node nodeResolver)
  {
    return new DomXPathNSResolver(nodeResolver);
  }
    
  public Object evaluate(String expression,
                         Node contextNode,
                         XPathNSResolver resolver,
                         short type,
                         Object result)
    throws XPathException, DOMException
  {
    XPathExpression xpe =
      new DomXPathExpression(this, expression, resolver);
    return xpe.evaluate(contextNode, type, result);
  }

}

