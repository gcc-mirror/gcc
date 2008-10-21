/* GnomeNode.java - 
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

package gnu.xml.libxmlj.dom;

import gnu.java.lang.CPStringBuilder;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.w3c.dom.UserDataHandler;

import gnu.xml.libxmlj.util.StandaloneDocumentType;

/**
 * A DOM node implemented in libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class GnomeNode
  implements Node, Comparable
{

  /**
   * Maps document pointers to a map of node pointers to node instances.
   */
  static Map instances;

  /**
   * Retrieves the node instance for the specified node pointer.
   * This creates a new instance and adds it to the cache if required.
   * @param doc the document pointer
   * @param node the node pointer
   * @param type the node type
   */
  static GnomeNode newInstance(final Object doc, final Object node,
                               final int type)
  {
    if (doc == null)
      {
        throw new NullPointerException("doc");
      }
    if (node == null)
      {
        throw new NullPointerException("node");
      }
    if (instances == null)
      {
        instances = new HashMap();
      }
    Map docNodes = (Map) instances.get(doc);
    if (docNodes == null)
      {
        docNodes = new HashMap(1024); // TODO review optimal initial capacity
        instances.put(doc, docNodes);
      }
    GnomeNode nodeInstance = (GnomeNode) docNodes.get(node);
    if (nodeInstance != null)
      {
        return nodeInstance; // Return cached version
      }
    switch (type)
      {
      case ELEMENT_NODE:
        nodeInstance = new GnomeElement(node);
        break;
      case ATTRIBUTE_NODE:
        nodeInstance = new GnomeAttr(node);
        break;
      case TEXT_NODE:
        nodeInstance = new GnomeText(node);
        break;
      case CDATA_SECTION_NODE:
        nodeInstance = new GnomeCDATASection(node);
        break;
      case ENTITY_REFERENCE_NODE:
        nodeInstance = new GnomeEntityReference(node);
        break;
      case ENTITY_NODE:
        nodeInstance = new GnomeEntity(node);
        break;
      case PROCESSING_INSTRUCTION_NODE:
        nodeInstance = new GnomeProcessingInstruction(node);
        break;
      case COMMENT_NODE:
        nodeInstance = new GnomeComment(node);
        break;
      case DOCUMENT_NODE:
        nodeInstance = new GnomeDocument(node);
        break;
      case DOCUMENT_TYPE_NODE:
        nodeInstance = new GnomeDocumentType(node);
        break;
      case DOCUMENT_FRAGMENT_NODE:
        nodeInstance = new GnomeDocumentFragment(node);
        break;
      case NOTATION_NODE:
        nodeInstance = new GnomeNotation(node);
        break;
      default:
        throw new IllegalArgumentException("Unknown node type: " + type);
      }
    docNodes.put(node, nodeInstance);
    return nodeInstance;
  }
  
  /**
   * Frees the specified document.
   * This removes all its nodes from the cache.
   */
  static void freeDocument(final Object doc)
  {
    if (instances == null || doc == null)
      {
        return;
      }
    instances.remove(doc);
    //System.out.println("Freed "+instances.remove(doc));
  }
  
  /**
   * xmlNodePtr
   */
  final Object id;

  Map userData;
  Map userDataHandlers;

  GnomeNode(final Object id)
  {
    this.id = id;
  }

  public native String getNodeName();

  public native String getNodeValue()
    throws DOMException;

  public native void setNodeValue(String nodeValue)
    throws DOMException;

  public native short getNodeType();

  public native Node getParentNode();

  public NodeList getChildNodes()
  {
    return new GnomeNodeList(id);
  }

  public native Node getFirstChild();

  public native Node getLastChild();

  public native Node getPreviousSibling();

  public native Node getNextSibling();

  public NamedNodeMap getAttributes()
  {
    return new GnomeNamedNodeMap(id, 0);
  }

  public native Document getOwnerDocument();

  public Node insertBefore(Node newChild, Node refChild)
    throws DOMException
  {
    if (newChild instanceof StandaloneDocumentType)
      {
        DocumentType dt = (DocumentType) newChild;
        newChild = ((GnomeDocument) getOwnerDocument())
          .createDocumentType(dt.getName(), dt.getPublicId(),
                              dt.getSystemId());
      }
    if (newChild == null)
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, null);
      }
    if (!(newChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.WRONG_DOCUMENT_ERR, null);
      }
    if (refChild == null || !(refChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, null);
      }
    return xmljInsertBefore(newChild, refChild);
  }

  private native Node xmljInsertBefore(Node newChild, Node refChild)
    throws DOMException;

  public Node replaceChild(Node newChild, Node oldChild)
    throws DOMException
  {
    if (newChild instanceof StandaloneDocumentType)
      {
        DocumentType dt = (DocumentType) newChild;
        newChild = ((GnomeDocument) getOwnerDocument())
          .createDocumentType(dt.getName(), dt.getPublicId(),
                              dt.getSystemId());
      }
    if (newChild == null)
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, null);
      }
    if (!(newChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.WRONG_DOCUMENT_ERR, newChild.toString());
      }
    if (oldChild == null || !(oldChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, null);
      }
    return xmljReplaceChild(newChild, oldChild);
  }

  private native Node xmljReplaceChild(Node newChild, Node oldChild)
    throws DOMException;

  public Node removeChild(Node oldChild)
    throws DOMException
  {
    if (!(oldChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.WRONG_DOCUMENT_ERR, null);
      }
    return xmljRemoveChild(oldChild);
  }

  private native Node xmljRemoveChild(Node oldChild)
    throws DOMException;

  public Node appendChild(Node newChild)
    throws DOMException
  {
    if (newChild instanceof StandaloneDocumentType)
      {
        DocumentType dt = (DocumentType) newChild;
        newChild = ((GnomeDocument) getOwnerDocument())
          .createDocumentType(dt.getName(), dt.getPublicId(),
                              dt.getSystemId());
      }
    if (!(newChild instanceof GnomeNode))
      {
        throw new GnomeDOMException(DOMException.WRONG_DOCUMENT_ERR, null);
      }
    return xmljAppendChild(newChild);
  }

  private native Node xmljAppendChild(Node newChild)
    throws DOMException;

  public native boolean hasChildNodes();

  public Node cloneNode(boolean deep)
  {
    Node ret = xmljCloneNode(deep);
    notifyUserDataHandlers(UserDataHandler.NODE_CLONED, this, ret);
    return ret;
  }
  
  private native Node xmljCloneNode(boolean deep);

  public native void normalize();

  public boolean isSupported(String feature, String version)
  {
    return getOwnerDocument().getImplementation()
      .hasFeature(feature, version);
  }

  public native String getNamespaceURI();

  public native String getPrefix();

  public native void setPrefix(String prefix)
    throws DOMException;

  public native String getLocalName();

  public native boolean hasAttributes();

  public int hashCode()
  {
    return id.hashCode();
  }

  public boolean equals(Object other)
  {
    if (other == this)
      {
        return true;
      }
    return (other instanceof GnomeNode &&
            ((GnomeNode) other).id == id);
  }

  // DOM Level 3 methods

  public native String getBaseURI();

  public short compareDocumentPosition(Node other)
    throws DOMException
  {
    return (short) compareTo(other);
  }

  public final int compareTo(Object other)
  {
    if (other instanceof GnomeNode)
      {
        return xmljCompareTo(other);
      }
    return 0;
  }

  private native int xmljCompareTo(Object other);
  
  public String getTextContent()
    throws DOMException
  {
    switch (getNodeType())
      {
      case ELEMENT_NODE:
      case ATTRIBUTE_NODE:
      case ENTITY_NODE:
      case ENTITY_REFERENCE_NODE:
      case DOCUMENT_FRAGMENT_NODE:
        CPStringBuilder buffer = new CPStringBuilder();
        NodeList children = getChildNodes();
        int len = children.getLength();
        for (int i = 0; i < len; i++)
          {
            Node child = children.item(i);
            String textContent = child.getTextContent();
            if (textContent != null)
              {
                buffer.append(textContent);
              }
          }
        return buffer.toString();
      case TEXT_NODE:
      case CDATA_SECTION_NODE:
      case COMMENT_NODE:
      case PROCESSING_INSTRUCTION_NODE:
        return getNodeValue();
      default:
        return null;
      }
  }
  
  public void setTextContent(String textContent)
    throws DOMException
  {
    switch (getNodeType())
      {
      case ENTITY_REFERENCE_NODE:
        // entity references are read only
        throw new GnomeDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                                    null);
      case ELEMENT_NODE:
      case ATTRIBUTE_NODE:
      case ENTITY_NODE:
      case DOCUMENT_FRAGMENT_NODE:
        NodeList children = getChildNodes();
        int len = children.getLength();
        for (int i = 0; i < len; i++)
          {
            Node child = children.item(i);
            removeChild(child);
          }
        if (textContent != null)
          {
            Text text = getOwnerDocument().createTextNode(textContent);
            appendChild(text);
          }
        break;
      case TEXT_NODE:
      case CDATA_SECTION_NODE:
      case COMMENT_NODE:
      case PROCESSING_INSTRUCTION_NODE:
        setNodeValue(textContent);
        break;
      }
  }
  
  public boolean isSameNode(Node other)
  {
    return equals(other);
  }
  
  public native String lookupPrefix(String namespaceURI);
  
  public native boolean isDefaultNamespace(String namespaceURI);
  
  public native String lookupNamespaceURI(String prefix);
  
  public native boolean isEqualNode(Node arg);
  
  public Object getFeature(String feature, String version)
  {
    return getOwnerDocument().getImplementation()
      .getFeature(feature, version);
  }

  public Object setUserData(String key, Object data, UserDataHandler handler)
  {
    // TODO handler
    if (userData == null)
      {
        userData = new HashMap();
      }
    if (handler != null)
      {
        if (userDataHandlers == null)
          {
            userDataHandlers = new HashMap();
          }
        userDataHandlers.put(key, handler);
      }
    return userData.put(key, data);
  }

  public Object getUserData(String key)
  {
    if (userData == null)
      {
        return null;
      }
    return userData.get(key);
  }

  void notifyUserDataHandlers(short op, Node src, Node dst)
  {
    if (userDataHandlers != null)
      {
        for (Iterator i = userDataHandlers.entrySet().iterator(); i.hasNext(); )
          {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            UserDataHandler handler = (UserDataHandler) entry.getValue();
            Object data = userData.get(key);
            handler.handle(op, key, data, src, dst);
          }
      }
  }

  public String toString()
  {
    CPStringBuilder buffer = new CPStringBuilder(getClass().getName());
    buffer.append("[nodeName=");
    buffer.append(getNodeName());
    buffer.append("]");
    return buffer.toString();
  }

}
