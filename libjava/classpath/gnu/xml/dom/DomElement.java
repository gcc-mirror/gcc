/* DomElement.java -- 
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

import java.util.HashSet;
import java.util.Set;
import javax.xml.XMLConstants;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.TypeInfo;

/**
 * <p> "Element" implementation.
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomElement
  extends DomNsNode
  implements Element
{

  /**
   * User-defined ID attributes.
   * Used by DomAttr.isId and DomDocument.getElementById
   */
  Set userIdAttrs;

  // Attributes are VERY expensive in DOM, and not just for
  // this implementation.  Avoid creating them.
  private DomNamedNodeMap attributes;

  // xml:space cache
  String xmlSpace = "";

  /**
   * Constructs an Element node associated with the specified document.
   *
   * <p>This constructor should only be invoked by a Document as part
   * of its createElement functionality, or through a subclass which is
   * similarly used in a "Sub-DOM" style layer.
   *
   * @param owner The document with which this node is associated
   * @param namespaceURI Combined with the local part of the name,
   *	this is used to uniquely identify a type of element
   * @param name Name of this element, which may include a prefix
   */
  protected DomElement(DomDocument owner, String namespaceURI, String name)
  {
    super(ELEMENT_NODE, owner, namespaceURI, name);
  }

  /**
   * <b>DOM L1</b>
   * Returns the element's attributes
   */
  public NamedNodeMap getAttributes()
  {
    if (attributes == null)
      {
        attributes = new DomNamedNodeMap(this, Node.ATTRIBUTE_NODE);
      }
    return attributes;
  }

  /**
   * <b>DOM L2></b>
   * Returns true iff this is an element node with attributes.
   */
  public boolean hasAttributes()
  {
    return attributes != null && attributes.length != 0;
  }

  /**
   * Shallow clone of the element, except that associated
   * attributes are (deep) cloned.
   */
  public Object clone()
  {
    DomElement node = (DomElement) super.clone();

    if (attributes != null)
      {
        node.attributes = new DomNamedNodeMap(node, Node.ATTRIBUTE_NODE);
        for (DomNode ctx = attributes.first; ctx != null; ctx = ctx.next)
          {
            node.attributes.setNamedItemNS(ctx.cloneNode(true));
          }
      }
    return node;
  }

  void setOwner(DomDocument doc)
  {
    if (attributes != null)
      {
        for (DomNode ctx = attributes.first; ctx != null; ctx = ctx.next)
          {
            ctx.setOwner(doc);
          }
      }
    super.setOwner(doc);
  }

  /**
   * Marks this element, its children, and its associated attributes as
   * readonly.
   */
  public void makeReadonly()
  {
    super.makeReadonly();
    if (attributes != null)
      {
        attributes.makeReadonly();
      }
  }

  /**
   * <b>DOM L1</b>
   * Returns the element name (same as getNodeName).
   */
  final public String getTagName()
  {
    return getNodeName();
  }

  /**
   * <b>DOM L1</b>
   * Returns the value of the specified attribute, or an
   * empty string.
   */
  public String getAttribute(String name)
  {
    if ("xml:space" == name) // NB only works on interned string
      {
        // Use cached value
        return xmlSpace;
      }
    Attr attr = getAttributeNode(name);
    return (attr == null) ? "" : attr.getValue();
  }

  /**
   * <b>DOM L2</b>
   * Returns true if the element has an attribute with the
   * specified name (specified or DTD defaulted).
   */
  public boolean hasAttribute(String name)
  {
    return getAttributeNode(name) != null;
  }

  /**
   * <b>DOM L2</b>
   * Returns true if the element has an attribute with the
   * specified name (specified or DTD defaulted).
   */
  public boolean hasAttributeNS(String namespaceURI, String local)
  {
    return getAttributeNodeNS(namespaceURI, local) != null;
  }

  /**
   * <b>DOM L2</b>
   * Returns the value of the specified attribute, or an
   * empty string.
   */
  public String getAttributeNS(String namespaceURI, String local)
  {
    Attr attr = getAttributeNodeNS(namespaceURI, local);
    return (attr == null) ? "" : attr.getValue();
  }

  /**
   * <b>DOM L1</b>
   * Returns the appropriate attribute node; the name is the
   * nodeName property of the attribute.
   */
  public Attr getAttributeNode(String name)
  {
    return (attributes == null) ? null :
      (Attr) attributes.getNamedItem(name);
  }

  /**
   * <b>DOM L2</b>
   * Returns the appropriate attribute node; the name combines
   * the namespace name and the local part.
   */
  public Attr getAttributeNodeNS(String namespace, String localPart)
  {
    return (attributes == null) ? null :
      (Attr) attributes.getNamedItemNS(namespace, localPart);
  }

  /**
   * <b>DOM L1</b>
   * Modifies an existing attribute to have the specified value,
   * or creates a new one with that value.  The name used is the
   * nodeName value. 
   */
  public void setAttribute(String name, String value)
  {
    Attr attr = getAttributeNode(name);
    if (attr != null)
      {
        attr.setNodeValue(value);
        ((DomAttr) attr).setSpecified(true);
        return;
      }
    attr = owner.createAttribute(name);
    attr.setNodeValue(value);
    setAttributeNode(attr);
  }

  /**
   * <b>DOM L2</b>
   * Modifies an existing attribute to have the specified value,
   * or creates a new one with that value.
   */
  public void setAttributeNS(String uri, String aname, String value)
  {
    if (("xmlns".equals (aname) || aname.startsWith ("xmlns:"))
        && !XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals (uri))
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                        "setting xmlns attribute to illegal value", this, 0);
      }

    Attr attr = getAttributeNodeNS(uri, aname);
    if (attr != null)
      {
        attr.setNodeValue(value);
        return;
      }
    attr = owner.createAttributeNS(uri, aname);
    attr.setNodeValue(value);
    setAttributeNodeNS(attr);
  }

  /**
   * <b>DOM L1</b>
   * Stores the specified attribute, optionally overwriting any
   * existing one with that name.
   */
  public Attr setAttributeNode(Attr attr)
  {
    return (Attr) getAttributes().setNamedItem(attr);
  }

  /**
   * <b>DOM L2</b>
   * Stores the specified attribute, optionally overwriting any
   * existing one with that name.
   */
  public Attr setAttributeNodeNS(Attr attr)
  {
    return (Attr) getAttributes().setNamedItemNS(attr);
  }

  /**
   * <b>DOM L1</b>
   * Removes the appropriate attribute node.
   * If there is no such node, this is (bizarrely enough) a NOP so you
   * won't see exceptions if your code deletes non-existent attributes.
   *
   * <p>Note that since there is no portable way for DOM to record
   * DTD information, default values for attributes will never be
   * provided automatically.
   */
  public void removeAttribute(String name)
  {
    if (attributes == null)
      {
        return;
      }

    try
      {
        attributes.removeNamedItem(name);
      }
    catch (DomDOMException e)
      {
        if (e.code != DOMException.NOT_FOUND_ERR)
          {
            throw e;
          }
      }
  }

  /**
   * <b>DOM L1</b>
   * Removes the appropriate attribute node; the name is the
   * nodeName property of the attribute.
   *
   * <p>Note that since there is no portable way for DOM to record
   * DTD information, default values for attributes will never be
   * provided automatically.
   */
  public Attr removeAttributeNode(Attr node)
  {
    if (attributes == null)
      {
        throw new DomDOMException(DOMException.NOT_FOUND_ERR, null, node, 0);
      }
    return (Attr) attributes.removeNamedItem(node.getNodeName());
  }

  /**
   * <b>DOM L2</b>
   * Removes the appropriate attribute node; the name combines
   * the namespace name and the local part.
   *
   * <p>Note that since there is no portable way for DOM to record
   * DTD information, default values for attributes will never be
   * provided automatically.
   */
  public void removeAttributeNS(String namespace, String localPart)
  {
    if (attributes == null)
      {
        throw new DomDOMException(DOMException.NOT_FOUND_ERR, localPart, null, 0);
      }
    attributes.removeNamedItemNS (namespace, localPart);
  }

  // DOM Level 3 methods

  public String lookupPrefix(String namespaceURI)
  {
    if (namespaceURI == null)
      {
        return null;
      }
    String namespace = getNamespaceURI();
    if (namespace != null && namespace.equals(namespaceURI))
      {
        return getPrefix();
      }
    if (attributes != null)
      {
        for (DomNode ctx = attributes.first; ctx != null; ctx = ctx.next)
          {
            if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI
                .equals(ctx.getNamespaceURI()))
              {
                String value = ctx.getNodeValue();
                if (value.equals(namespaceURI))
                  {
                    return ctx.getLocalName();
                  }
              }
          }
      }
    return super.lookupPrefix(namespaceURI);
  }

  public boolean isDefaultNamespace(String namespaceURI)
  {
    String namespace = getNamespaceURI();
    if (namespace != null && namespace.equals(namespaceURI))
      {
        return getPrefix() == null;
      }
    if (attributes != null)
      {
        for (DomNode ctx = attributes.first; ctx != null; ctx = ctx.next)
          {
            if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI
                .equals(ctx.getNamespaceURI()))
              {
                String qName = ctx.getNodeName();
                return (XMLConstants.XMLNS_ATTRIBUTE.equals(qName));
              }
          }
      }
    return super.isDefaultNamespace(namespaceURI);
  }

  public String lookupNamespaceURI(String prefix)
  {
    String namespace = getNamespaceURI();
    if (namespace != null && equal(prefix, getPrefix()))
      {
        return namespace;
      }
    if (attributes != null)
      {
        for (DomNode ctx = attributes.first; ctx != null; ctx = ctx.next)
          {
            if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI
                .equals(ctx.getNamespaceURI()))
              {
                if (prefix == null)
                  {
                    if (XMLConstants.XMLNS_ATTRIBUTE.equals(ctx.getNodeName()))
                      {
                        return ctx.getNodeValue();
                      }
                  }
                else
                  {
                    if (prefix.equals(ctx.getLocalName()))
                      {
                        return ctx.getNodeValue();
                      }
                  }
              }
          }
      }
    return super.lookupNamespaceURI(prefix);
  }
  
  public String getBaseURI()
  {
    if (attributes != null)
      {
        Node xmlBase =
          attributes.getNamedItemNS(XMLConstants.XML_NS_URI, "base");
        if (xmlBase != null)
          {
            return xmlBase.getNodeValue();
          }
      }
    return super.getBaseURI();
  }
  
  public TypeInfo getSchemaTypeInfo()
  {
    // DTD implementation
    DomDoctype doctype = (DomDoctype) owner.getDoctype();
    if (doctype != null)
      {
        return doctype.getElementTypeInfo(getNodeName());
      }
    // TODO XML Schema implementation
    return null;
  }

  public void setIdAttribute(String name, boolean isId)
  {
    NamedNodeMap attrs = getAttributes();
    Attr attr = (Attr) attrs.getNamedItem(name);
    setIdAttributeNode(attr, isId);
  }
  
  public void setIdAttributeNode(Attr attr, boolean isId)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    if (attr == null || attr.getOwnerElement() != this)
      {
        throw new DomDOMException(DOMException.NOT_FOUND_ERR);
      }
    if (isId)
      {
        if (userIdAttrs == null)
          {
            userIdAttrs = new HashSet();
          }
        userIdAttrs.add(attr);
      }
    else if (userIdAttrs != null)
      {
        userIdAttrs.remove(attr);
        if (userIdAttrs.isEmpty())
          {
            userIdAttrs = null;
          }
      }
  }

  public void setIdAttributeNS(String namespaceURI, String localName,
                               boolean isId)
  {
    NamedNodeMap attrs = getAttributes();
    Attr attr = (Attr) attrs.getNamedItemNS(namespaceURI, localName);
    setIdAttributeNode(attr, isId);
  }

  public boolean isEqualNode(Node arg)
  {
    if (!super.isEqualNode(arg))
      return false;
    getAttributes();
    NamedNodeMap argAttrs = arg.getAttributes();
    int len = argAttrs.getLength();
    if (argAttrs == null || (len != attributes.length))
      return false;
    for (int i = 0; i < len; i++)
      {
        Node argCtx = argAttrs.item(i);
        // Don't compare namespace nodes
        if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI
            .equals(argCtx.getNamespaceURI()))
          continue;
        // Find corresponding attribute node
        DomNode ctx = attributes.first;
        for (; ctx != null; ctx = ctx.next)
          {
            if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI
                .equals(ctx.getNamespaceURI()))
              continue;
            if (!ctx.isEqualNode(argCtx))
              continue;
            break;
          }
        if (ctx == null)
          return false; // not found
      }
    return true;
  }
  
}
