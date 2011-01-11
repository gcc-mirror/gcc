/* IIOMetadataNode.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio.metadata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.UserDataHandler;
import javax.imageio.metadata.IIOMetadataFormatImpl.IIOMetadataNodeAttr;

public class IIOMetadataNode
  implements Element, NodeList
{
  private String name;
  private HashMap attrs = new HashMap();
  private List children = new ArrayList();
  private IIOMetadataNode parent;
  private Object obj;

  /**
   * Simple NamedNodeMap class for IIOMetadataNode.
   *
   * @author jlquinn
   */
  private class IIONamedNodeMap implements NamedNodeMap
  {
    HashMap attrs;

    /**
     * @param attrs
     * @param node
     */
    public IIONamedNodeMap(HashMap attrs)
    {
      this.attrs = attrs;
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#getNamedItem(java.lang.String)
     */
    public Node getNamedItem(String name)
    {
      return (Node)attrs.get(name);
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#setNamedItem(org.w3c.dom.Node)
     */
    public Node setNamedItem(Node arg) throws DOMException
    {
      if (arg instanceof IIOMetadataNodeAttr)
        {
          IIOMetadataNodeAttr attr = (IIOMetadataNodeAttr) arg;
          // The only code that can successfully do this is in this package.
          if (attr.owner != null)
            throw new DOMException(DOMException.INUSE_ATTRIBUTE_ERR, "");
          return (Node)attrs.put(attr.name, attr);
        }
      // Anything else gets treated as an invalid op.
      throw new DOMException(DOMException.HIERARCHY_REQUEST_ERR, "");
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#removeNamedItem(java.lang.String)
     */
    public Node removeNamedItem(String name) throws DOMException
    {
      return (Node)attrs.remove(name);
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#item(int)
     */
    public Node item(int index)
    {
      return (Node)attrs.values().toArray()[index];
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#getLength()
     */
    public int getLength()
    {
      return attrs.size();
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#getNamedItemNS(java.lang.String, java.lang.String)
     */
    public Node getNamedItemNS(String namespaceURI, String localName)
    {
      return getNamedItem(localName);
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#setNamedItemNS(org.w3c.dom.Node)
     */
    public Node setNamedItemNS(Node arg) throws DOMException
    {
      return setNamedItem(arg);
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NamedNodeMap#removeNamedItemNS(java.lang.String, java.lang.String)
     */
    public Node removeNamedItemNS(String namespaceURI, String localName)
      throws DOMException
    {
      return removeNamedItem(localName);
    }
  }

  /**
   * Simple NodeList implementation for IIOMetadataNode.
   *
   * @author jlquinn
   *
   */
  private class IIONodeList implements NodeList
  {
    List children = new ArrayList();

    /* (non-Javadoc)
     * @see org.w3c.dom.NodeList#item(int)
     */
    public Node item(int index)
    {
      return (index < children.size()) ? (Node)children.get(index) : null;
    }

    /* (non-Javadoc)
     * @see org.w3c.dom.NodeList#getLength()
     */
    public int getLength()
    {
      return children.size();
    }
  }

  public IIOMetadataNode()
  {
    // Do nothing here.
  }

  public IIOMetadataNode(String nodename)
  {
    name = nodename;
  }

  public Object getUserObject()
  {
    return obj;
  }

  public void setUserObject(Object o)
  {
    obj = o;
  }

  public short compareDocumentPosition(Node other)
    throws DOMException
  {
    return Element.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getAttribute(java.lang.String)
   */
  public String getAttribute(String name)
  {
    Attr anode = (Attr) attrs.get(name);
    return anode != null ? anode.getValue() : null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getAttributeNode(java.lang.String)
   */
  public Attr getAttributeNode(String name)
  {
    String val = getAttribute(name);
    if (val != null)
      return new IIOMetadataNodeAttr(this, name, val);
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getAttributeNodeNS(java.lang.String, java.lang.String)
   */
  public Attr getAttributeNodeNS(String namespaceURI, String localName)
  {
    return getAttributeNode(localName);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getAttributeNS(java.lang.String, java.lang.String)
   */
  public String getAttributeNS(String namespaceURI, String localName)
  {
    return getAttribute(localName);
  }

  public String getBaseURI()
  {
    return null;
  }

  // Recursive function for assembling a node list.
  private void getElementsRecurse(IIONodeList list, String name)
  {
    for (int i=0; i < children.size(); i++)
    {
      if (((Node)children.get(i)).getNodeName().equals(name))
        list.children.add(children.get(i));
      getElementsRecurse(list, name);
    }
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getElementsByTagName(java.lang.String)
   */
  public NodeList getElementsByTagName(String name)
  {
    IIONodeList list = new IIONodeList();
    getElementsRecurse(list, name);
    return list;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getElementsByTagNameNS(java.lang.String, java.lang.String)
   */
  public NodeList getElementsByTagNameNS(String namespaceURI, String localName)
  {
    IIONodeList list = new IIONodeList();
    getElementsRecurse(list, name);
    return list;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#getTagName()
   */
  public String getTagName()
  {
    return name;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#hasAttribute(java.lang.String)
   */
  public boolean hasAttribute(String name)
  {
    return attrs.containsKey(name);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#hasAttributeNS(java.lang.String, java.lang.String)
   */
  public boolean hasAttributeNS(String namespaceURI, String localName)
  {
    return attrs.containsKey(localName);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#removeAttribute(java.lang.String)
   */
  public void removeAttribute(String name)
  {
    attrs.remove(name);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#removeAttributeNode(org.w3c.dom.Attr)
   */
  public Attr removeAttributeNode(Attr oldAttr)
  {
    return (Attr)attrs.remove(oldAttr.getName());
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#removeAttributeNS(java.lang.String, java.lang.String)
   */
  public void removeAttributeNS(String namespaceURI, String localName)
  {
    removeAttribute(localName);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#setAttribute(java.lang.String, java.lang.String)
   */
  public void setAttribute(String name, String value)
  {
    Attr attr = getAttributeNode(name);
    if (attr != null)
      attr.setValue(value);
    else
      attrs.put(name, new IIOMetadataNodeAttr(this, name, value));
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#setAttributeNode(org.w3c.dom.Attr)
   */
  public Attr setAttributeNode(Attr newAttr)
  {
    return (Attr)attrs.put(newAttr.getName(), newAttr);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#setAttributeNodeNS(org.w3c.dom.Attr)
   */
  public Attr setAttributeNodeNS(Attr newAttr)
  {
    return (Attr)attrs.put(newAttr.getName(), newAttr);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Element#setAttributeNS(java.lang.String, java.lang.String, java.lang.String)
   */
  public void setAttributeNS(String namespaceURI, String qualifiedName, String value)
  {
    setAttribute(qualifiedName, value);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.NodeList#getLength()
   */
  public int getLength()
  {
    return children.size();
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.NodeList#item(int)
   */
  public Node item(int index)
  {
    if (index < children.size())
      return (Node)children.get(index);
    else
      return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#appendChild(org.w3c.dom.Node)
   */
  public Node appendChild(Node newChild)
  {
    if (newChild == null)
      throw new IllegalArgumentException("Child node is null");

    IIOMetadataNode child = (IIOMetadataNode) newChild;

    children.add(child);
    child.parent = this;
    return this;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#cloneNode(boolean)
   */
  public Node cloneNode(boolean deep)
  {
    IIOMetadataNode newnode = new IIOMetadataNode(name);
    newnode.parent = null;
    newnode.obj = obj;
    if (deep)
    {
      for (int i=0; i < children.size(); i++)
        newnode.children.add(((Node)children.get(i)).cloneNode(deep));
    }

    // clone attrs
    for (Iterator it = attrs.values().iterator(); it.hasNext();)
    {
      IIOMetadataNodeAttr attr = (IIOMetadataNodeAttr)it.next();
      newnode.attrs.put(attr.name, attr.cloneNode(deep));
      attr.owner = newnode;
    }

    return newnode;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getAttributes()
   */
  public NamedNodeMap getAttributes()
  {
    return new IIONamedNodeMap(attrs);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getChildNodes()
   */
  public NodeList getChildNodes()
  {
    return this;
  }

  public Object getFeature(String feature, String version)
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getFirstChild()
   */
  public Node getFirstChild()
  {
    return (children.size() > 0) ? (Node)children.get(0) : null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getLastChild()
   */
  public Node getLastChild()
  {
    return (children.size() > 0) ? (Node)children.get(children.size() - 1)
           : null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getLocalName()
   */
  public String getLocalName()
  {
    return name;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNamespaceURI()
   */
  public String getNamespaceURI()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNextSibling()
   */
  public Node getNextSibling()
  {
    // If this op needs to be faster, add links to prev and next nodes.
    if (parent == null) return null;
    int idx = parent.children.indexOf(this);
    return (idx == parent.children.size() - 1) ? null
        : (Node)parent.children.get(idx + 1);
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeName()
   */
  public String getNodeName()
  {
    return name;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeType()
   */
  public short getNodeType()
  {
    return ELEMENT_NODE;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getNodeValue()
   */
  public String getNodeValue()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getOwnerDocument()
   */
  public Document getOwnerDocument()
  {
    // IOMetadataNodes have no owner
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getParentNode()
   */
  public Node getParentNode()
  {
    return parent;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getPrefix()
   */
  public String getPrefix()
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#getPreviousSibling()
   */
  public Node getPreviousSibling()
  {
    // If this op needs to be faster, add links to prev and next nodes.
    if (parent == null) return null;
    int idx = parent.children.indexOf(this);
    return (idx == 0) ? null
        : (Node)parent.children.get(idx - 1);
  }

  public TypeInfo getSchemaTypeInfo()
  {
    return null;
  }

  public String getTextContent()
    throws DOMException
  {
    return null;
  }

  public Object getUserData(String key)
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#hasAttributes()
   */
  public boolean hasAttributes()
  {
    return !attrs.isEmpty();
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#hasChildNodes()
   */
  public boolean hasChildNodes()
  {
    return !children.isEmpty();
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#insertBefore(org.w3c.dom.Node, org.w3c.dom.Node)
   */
  public Node insertBefore(Node newChild, Node refChild)
  {
    if (newChild == null)
      throw new IllegalArgumentException();

    int idx = children.indexOf(refChild);
    if (idx == -1)
      children.add(newChild);
    else
      children.add(idx, newChild);
    ((IIOMetadataNode)newChild).parent = this;

    return newChild;
  }

  public boolean isDefaultNamespace(String namespaceURI)
  {
    return true;
  }

  public boolean isEqualNode(Node arg)
  {
    return true;
  }

  public boolean isSameNode(Node other)
  {
    return this == other;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#isSupported(java.lang.String, java.lang.String)
   */
  public boolean isSupported(String feature, String version)
  {
    // No DOM features are supported
    return false;
  }

  public String lookupNamespaceURI(String prefix)
  {
    return null;
  }

  public String lookupPrefix(String namespaceURI)
  {
    return null;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#normalize()
   */
  public void normalize()
  {
    // No text nodes so no action
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#removeChild(org.w3c.dom.Node)
   */
  public Node removeChild(Node oldChild)
  {
    if (oldChild == null)
      throw new IllegalArgumentException();
    children.remove(oldChild);
    ((IIOMetadataNode)oldChild).parent = null;

    return oldChild;
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#replaceChild(org.w3c.dom.Node, org.w3c.dom.Node)
   */
  public Node replaceChild(Node newChild, Node oldChild)
  {
    if (newChild == null)
      throw new IllegalArgumentException();
    children.set(children.indexOf(oldChild), newChild);
    ((IIOMetadataNode)oldChild).parent = null;
    return oldChild;
  }

  public void setIdAttribute(String name, boolean isId)
    throws DOMException
  {
  }

  public void setIdAttributeNode(Attr idAttr, boolean isId)
    throws DOMException
  {
  }

  public void setIdAttributeNS(String namespaceURI, String localName, boolean isId)
    throws DOMException
  {
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#setNodeValue(java.lang.String)
   */
  public void setNodeValue(String nodeValue) throws DOMException
  {
  }

  /* (non-Javadoc)
   * @see org.w3c.dom.Node#setPrefix(java.lang.String)
   */
  public void setPrefix(String prefix)
  {
  }

  public void setTextContent(String textContent)
    throws DOMException
  {
  }

  public Object setUserData(String key, Object data, UserDataHandler handler)
  {
    return null;
  }
}
