/* DomNodeIterator.java -- 
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
package gnu.xml.dom;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.NodeIterator;
import org.w3c.dom.traversal.TreeWalker;

/**
 * Node iterator and tree walker.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomNodeIterator
  implements NodeIterator, TreeWalker
{

  Node root;
  final int whatToShow;
  final NodeFilter filter;
  final boolean entityReferenceExpansion;
  final boolean walk;
  Node current;

  public DomNodeIterator(Node root, int whatToShow, NodeFilter filter,
                         boolean entityReferenceExpansion, boolean walk)
  {
    if (root == null)
      {
        throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "null root");
      }
    this.root = root;
    this.whatToShow = whatToShow;
    this.filter = filter;
    this.entityReferenceExpansion = entityReferenceExpansion;
    this.walk = walk;
    current = root;
  }

  public Node getRoot()
  {
    return root;
  }

  public int getWhatToShow()
  {
    return whatToShow;
  }

  public NodeFilter getFilter()
  {
    return filter;
  }

  public boolean getExpandEntityReferences()
  {
    return entityReferenceExpansion;
  }

  public Node nextNode()
    throws DOMException
  {
    if (root == null)
      {
        throw new DOMException(DOMException.INVALID_STATE_ERR, "null root");
      }
    Node ret;
    do
      {
        if (current.equals(root))
          {
            ret = root.getFirstChild();
          }
        else if (walk)
          {
            ret = current.getFirstChild();
            if (ret == null)
              {
                ret = current.getNextSibling();
              }
            if (ret == null)
              {
                Node tmp = current;
                ret = tmp.getParentNode();
                while (!ret.equals(root) && tmp.equals(ret.getLastChild()))
                  {
                    tmp = ret;
                    ret = tmp.getParentNode();
                  }
                if (ret.equals(root))
                  {
                    ret = null;
                  }
                else
                  {
                    ret = ret.getNextSibling();
                  }
              }
          }
        else
          {
            ret = current.getNextSibling();
          }
      }
    while (!accept(ret));
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node previousNode()
    throws DOMException
  {
    if (root == null)
      {
        throw new DOMException(DOMException.INVALID_STATE_ERR, "null root");
      }
    Node ret;
    do
      {
        if (current.equals(root))
          {
            ret = current.getLastChild();
          }
        else if (walk)
          {
            ret = current.getLastChild();
            if (ret == null)
              {
                ret = current.getPreviousSibling();
              }
            if (ret == null)
              {
                Node tmp = current;
                ret = tmp.getParentNode();
                while (!ret.equals(root) && tmp.equals(ret.getFirstChild()))
                  {
                    tmp = ret;
                    ret = tmp.getParentNode();
                  }
                if (ret.equals(root))
                  {
                    ret = null;
                  }
                else
                  {
                    ret = ret.getPreviousSibling();
                  }
              }
          }
        else
          {
            ret = current.getPreviousSibling();
          }
      }
    while (!accept(ret));
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node getCurrentNode()
  {
    return current;
  }

  public void setCurrentNode(Node current)
    throws DOMException
  {
    if (current == null)
      {
        throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "null root");
      }
    this.current = current;
  }

  public Node parentNode()
  {
    Node ret = current.getParentNode();
    if (!accept (ret))
      {
        ret = null;
      }
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node firstChild ()
  {
    Node ret = current.getFirstChild();
    while (!accept(ret))
      {
        ret = ret.getNextSibling();
      }
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node lastChild()
  {
    Node ret = current.getLastChild();
    while (!accept(ret))
      {
        ret = ret.getPreviousSibling();
      }
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node previousSibling()
  {
    Node ret = current.getPreviousSibling();
    while (!accept(ret))
      {
        ret = ret.getPreviousSibling();
      }
    current = (ret == null) ? current : ret;
    return ret;
  }

  public Node nextSibling()
  {
    Node ret = current.getNextSibling();
    while (!accept(ret))
      {
        ret = ret.getNextSibling();
      }
    current = (ret == null) ? current : ret;
    return ret;
  }

  public void detach()
  {
    root = null;
  }

  boolean accept(Node node)
  {
    if (node == null)
      {
        return true;
      }
    boolean ret;
    switch (node.getNodeType())
      {
      case Node.ATTRIBUTE_NODE:
        ret = (whatToShow & NodeFilter.SHOW_ATTRIBUTE) != 0;
        break;
      case Node.CDATA_SECTION_NODE:
        ret = (whatToShow & NodeFilter.SHOW_CDATA_SECTION) != 0;
        break;
      case Node.COMMENT_NODE:
        ret = (whatToShow & NodeFilter.SHOW_COMMENT) != 0;
        break;
      case Node.DOCUMENT_NODE:
        ret = (whatToShow & NodeFilter.SHOW_DOCUMENT) != 0;
        break;
      case Node.DOCUMENT_FRAGMENT_NODE:
        ret = (whatToShow & NodeFilter.SHOW_DOCUMENT_FRAGMENT) != 0;
        break;
      case Node.DOCUMENT_TYPE_NODE:
        ret = (whatToShow & NodeFilter.SHOW_DOCUMENT_TYPE) != 0;
        break;
      case Node.ELEMENT_NODE:
        ret = (whatToShow & NodeFilter.SHOW_ELEMENT) != 0;
        break;
      case Node.ENTITY_NODE:
        ret = (whatToShow & NodeFilter.SHOW_ENTITY) != 0;
        break;
      case Node.ENTITY_REFERENCE_NODE:
        ret = (whatToShow & NodeFilter.SHOW_ENTITY_REFERENCE) != 0;
        ret = ret && entityReferenceExpansion;
        break;
      case Node.NOTATION_NODE:
        ret = (whatToShow & NodeFilter.SHOW_NOTATION) != 0;
        break;
      case Node.PROCESSING_INSTRUCTION_NODE:
        ret = (whatToShow & NodeFilter.SHOW_PROCESSING_INSTRUCTION) != 0;
        break;
      case Node.TEXT_NODE:
        ret = (whatToShow & NodeFilter.SHOW_TEXT) != 0;
        break;
      default:
        ret = true;
      }
    if (ret && filter != null)
      {
        ret = (filter.acceptNode(node) == NodeFilter.FILTER_ACCEPT);
      }
    return ret;
  }

}
