/* DomHTMLCollection.java -- 
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.xml.dom.html2;

import gnu.xml.dom.DomDOMException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.html2.HTMLCollection;
import org.w3c.dom.html2.HTMLOptionsCollection;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.NodeIterator;

/**
 * An HTML element collection.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class DomHTMLCollection
  implements HTMLCollection, HTMLOptionsCollection, NodeList, NodeFilter
{

  final DomHTMLDocument doc;
  final Node root;
  List nodeNames;
  List attributeNames;
  List results;

  DomHTMLCollection(DomHTMLDocument doc, Node root)
  {
    this.doc = doc;
    this.root = root;
  }

  // -- Node name and attribute filtering --

  void addNodeName(String name)
  {
    if (nodeNames == null)
      {
        nodeNames = new LinkedList();
      }
    nodeNames.add(name);
  }

  void addAttributeName(String name)
  {
    if (attributeNames == null)
      {
        attributeNames = new LinkedList();
      }
    attributeNames.add(name);
  }

  public short acceptNode(Node n)
  {
    if (n.getNodeType() != Node.ELEMENT_NODE)
      {
        return NodeFilter.FILTER_SKIP;
      }
    String localName = n.getLocalName();
    if (localName == null)
      {
        localName = n.getNodeName();
      }
    if (nodeNames != null && !acceptName(localName))
      {
        return NodeFilter.FILTER_SKIP;
      }
    if (attributeNames != null && !acceptAttributes(n.getAttributes()))
      {
        return NodeFilter.FILTER_SKIP;
      }
    return NodeFilter.FILTER_ACCEPT;
  }

  private boolean acceptName(String name)
  {
    for (Iterator i = nodeNames.iterator(); i.hasNext(); )
      {
        String nodeName = (String) i.next();
        if (nodeName.equalsIgnoreCase(name))
          {
            return true;
          }
      }
    return false;
  }

  private boolean acceptAttributes(NamedNodeMap attrs)
  {
    for (Iterator i = attributeNames.iterator(); i.hasNext(); )
      {
        String attributeName = (String) i.next();
        Node attr = getNamedItem(attrs, attributeName);
        if (attr != null)
          {
            // Check that attribute has a non-null value
            String nodeValue = attr.getNodeValue();
            if (nodeValue != null && nodeValue.length() > 0)
              {
                return true;
              }
          }
      }
    return false;
  }

  /**
   * Case-insensitive version of getNamedItem.
   */
  private Node getNamedItem(NamedNodeMap attrs, String name)
  {
    int len = attrs.getLength();
    for (int i = 0; i < len; i++)
      {
        Node attr = attrs.item(i);
        String attrName = attr.getLocalName();
        if (attrName == null)
          {
            attrName = attr.getNodeName();
          }
        if (name.equalsIgnoreCase(attrName))
          {
            return attr;
          }
      }
    return null;
  }

  // -- Perform query --

  void evaluate()
  {
    NodeIterator i = doc.createNodeIterator(root, NodeFilter.SHOW_ELEMENT,
                                            this, true);
    results = new ArrayList();
    for (Node node = i.nextNode(); node != null; node = i.nextNode())
      {
        results.add(node);
      }
  }

  // -- HTMLCollection/NodeList interface --

  public int getLength()
  {
    return results.size();
  }

  public void setLength(int length)
  {
    throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR);
  }

  public Node item(int index)
  {
    return (Node) results.get(index);
  }

  public Node namedItem(String name)
  {
    boolean xhtml = false; // FIXME detect XHTML document
    for (Iterator i = results.iterator(); i.hasNext(); )
      {
        Node node = (Node) i.next();
        NamedNodeMap attrs = node.getAttributes();
        Node attr = getNamedItem(attrs, "id");
        if (name.equals(attr.getTextContent()))
          {
            return node;
          }
        if (!xhtml)
          {
            attr = getNamedItem(attrs, "name");
            if (name.equals(attr.getTextContent()))
              {
                return node;
              }
          }
      }
    return null;
  }
  
}

