/* IIOAttr.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.imageio.metadata;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.UserDataHandler;

/**
 * Simple Attr node for metadata trees
 * 
 * @author jlquinn
 */
class IIOAttr implements Attr
{
  String name;
  String value;
  IIOMetadataNode owner;
  
  public IIOAttr(String name, String value, IIOMetadataNode owner)
  {
    this.name = name;
    this.value = value;
    this.owner = owner;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Attr#getName()
   */
  public String getName()
  {
    return name;
  }

  public TypeInfo getSchemaTypeInfo()
  {
    throw new Error("not implemented");
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Attr#getSpecified()
   */
  public boolean getSpecified()
  {
    // I don't think there can be default attrs in metadata
    return true;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Attr#getValue()
   */
  public String getValue()
  {
    return value;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Attr#setValue(java.lang.String)
   */
  public void setValue(String value) throws DOMException
  {
    this.value = value;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Attr#getOwnerElement()
   */
  public Element getOwnerElement()
  {
    return owner;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeName()
   */
  public String getNodeName()
  {
    return name;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeValue()
   */
  public String getNodeValue() throws DOMException
  {
    return value;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#setNodeValue(java.lang.String)
   */
  public void setNodeValue(String nodeValue) throws DOMException
  {
    this.value = nodeValue;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeType()
   */
  public short getNodeType()
  {
    return ATTRIBUTE_NODE;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getParentNode()
   */
  public Node getParentNode()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getChildNodes()
   */
  public NodeList getChildNodes()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getFirstChild()
   */
  public Node getFirstChild()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getLastChild()
   */
  public Node getLastChild()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getPreviousSibling()
   */
  public Node getPreviousSibling()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNextSibling()
   */
  public Node getNextSibling()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getAttributes()
   */
  public NamedNodeMap getAttributes()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getOwnerDocument()
   */
  public Document getOwnerDocument()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#insertBefore(org.w3c.dom.Node, org.w3c.dom.Node)
   */
  public Node insertBefore(Node newChild, Node refChild) throws DOMException
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#replaceChild(org.w3c.dom.Node, org.w3c.dom.Node)
   */
  public Node replaceChild(Node newChild, Node oldChild) throws DOMException
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#removeChild(org.w3c.dom.Node)
   */
  public Node removeChild(Node oldChild) throws DOMException
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#appendChild(org.w3c.dom.Node)
   */
  public Node appendChild(Node newChild) throws DOMException
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#hasChildNodes()
   */
  public boolean hasChildNodes()
  {
    return false;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#cloneNode(boolean)
   */
  public Node cloneNode(boolean deep)
  {
    return new IIOAttr(name, value, owner);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#normalize()
   */
  public void normalize()
  {
  }

  public boolean isDefaultNamespace(String namespaceURI)
  {
    throw new Error("not implemented");
  }
  
  /* (non-Javadoc)
   * @see org.w3c.dom.Node#isSupported(java.lang.String, java.lang.String)
   */
  public boolean isSupported(String feature, String version)
  {
    return false;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNamespaceURI()
   */
  public String getNamespaceURI()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getPrefix()
   */
  public String getPrefix()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#setPrefix(java.lang.String)
   */
  public void setPrefix(String prefix) throws DOMException
  {
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getLocalName()
   */
  public String getLocalName()
  {
    return name;
  }

  public Object getUserData(String key)
  {
    throw new Error("not implemented");
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#hasAttributes()
   */
  public boolean hasAttributes()
  {
    return false;
  }

  public boolean isId()
  {
    throw new Error("not implemented");
  }

  public String lookupNamespaceURI(String prefix)
  {
    throw new Error("not implemented");
  }

  public String lookupPrefix(String namespaceURI)
  {
    throw new Error("not implemented");
  }
  
  public Object setUserData(String key, Object data, UserDataHandler handler)
  {
    throw new Error("not implemented");
  }

  public String getBaseURI()
  {
    throw new Error("not implemented");
  }

  public String getTextContent()
  {
    throw new Error("not implemented");
  }

  public void setTextContent(String textContent)
  {
    throw new Error("not implemented");
  }

  public short compareDocumentPosition(Node other)
    throws DOMException
  {
    throw new Error("not implemented");
  }

  public Object getFeature(String feature, String version)
  {
    throw new Error("not implemented");
  }
  
  public boolean isEqualNode(Node other)
  {
    throw new Error("not implemented");
  }
  
  public boolean isSameNode(Node other)
  {
    throw new Error("not implemented");
  }
}
