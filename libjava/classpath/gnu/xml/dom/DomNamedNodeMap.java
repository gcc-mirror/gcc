/* DomNamedNodeMap.java -- 
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

import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * <p> "NamedNodeMap" implementation. </p>
 * Used mostly to hold element attributes, but sometimes also
 * to list notations or entities.
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomNamedNodeMap
  implements NamedNodeMap
{

  final DomNode owner;
  final short type;
  
  DomNode first;
  int length;
  boolean readonly;
  
  // package private
  DomNamedNodeMap(DomNode owner, short type)
  {
    this.owner = owner;
    this.type = type;
  }

  /**
   * Exposes the internal "readonly" flag.  In DOM, all NamedNodeMap
   * objects found in a DocumentType object are read-only (after
   * they are fully constructed), and those holding attributes of
   * a readonly element will also be readonly.
   */
  public final boolean isReadonly()
  {
    return readonly;
  }  
    
  /**
   * Sets the internal "readonly" flag so the node and its
   * children can't be changed.
   */
  public void makeReadonly()
  {
    readonly = true;
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        ctx.makeReadonly();
      }
  }
  
  /**
   * <b>DOM L1</b>
   * Returns the named item from the map, or null; names are just
   * the nodeName property.
   */
  public Node getNamedItem(String name)
  {
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        if (ctx.getNodeName().equals(name))
          {
            return ctx;
          }
      }
    return null;
  }

  /**
   * <b>DOM L2</b>
   * Returns the named item from the map, or null; names are the
   * localName and namespaceURI properties, ignoring any prefix.
   */
  public Node getNamedItemNS(String namespaceURI, String localName)
  {
    if ("".equals(namespaceURI))
      {
        namespaceURI = null;
      }
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        String name = ctx.getLocalName();
        if ((localName == null && name == null) ||
            (localName != null && localName.equals(name)))
          {
            String uri = ctx.getNamespaceURI();
            if ("".equals(uri))
              {
                uri = null;
              }
            if ((namespaceURI == null && uri == null) ||
                (namespaceURI != null && namespaceURI.equals(uri)))
              {
                return ctx;
              }
          }
      }
    return null;
  }

  /**
   * <b>DOM L1</b>
   * Stores the named item into the map, optionally overwriting
   * any existing node with that name.  The name used is just
   * the nodeName attribute.
   */
  public Node setNamedItem(Node arg)
  {
    return setNamedItem(arg, false, false);
  }

  /**
   * <b>DOM L2</b>
   * Stores the named item into the map, optionally overwriting
   * any existing node with that fully qualified name.  The name
   * used incorporates the localName and namespaceURI properties,
   * and ignores any prefix.
   */
  public Node setNamedItemNS(Node arg)
  {
    return setNamedItem(arg, true, false);
  }

  Node setNamedItem(Node arg, boolean ns, boolean cloning)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }

    DomNode node = (DomNode) arg;
    if (!cloning && node.owner != owner.owner)
      {
        throw new DomDOMException(DOMException.WRONG_DOCUMENT_ERR);
      }
    if (node.nodeType != type)
      {
        throw new DomDOMException(DOMException.HIERARCHY_REQUEST_ERR);
      }
    if (node.nodeType == Node.ATTRIBUTE_NODE)
      {
        DomNode element = node.parent;
        if (element != null && element != owner)
          {
            throw new DomDOMException(DOMException.INUSE_ATTRIBUTE_ERR);
          }
        node.parent = owner;
        node.depth = owner.depth + 1;
      }
    
    String nodeName = node.getNodeName();
    String localName = ns ? node.getLocalName() : null;
    String namespaceURI = ns ? node.getNamespaceURI() : null;
    if ("".equals(namespaceURI))
      {
        namespaceURI = null;
      }
    
    // maybe attribute ADDITION events (?)
    DomNode last = null;
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        boolean test = false;
        if (ns)
          {
            String tln = ctx.getLocalName();
            if (tln == null)
              {
                tln = ctx.getNodeName();
              }
            if (tln.equals(localName))
              {
                String tu = ctx.getNamespaceURI();
                if ((tu == null && namespaceURI == null) ||
                    (tu != null && tu.equals(namespaceURI)))
                  {
                    test = true;
                  }
              }
          }
        else
          {
            test = ctx.getNodeName().equals(nodeName);
          }
        if (test)
          {
            // replace
            node.previous = ctx.previous;
            node.next = ctx.next;
            if (ctx.previous != null)
              {
                ctx.previous.next = node;
              }
            if (ctx.next != null)
              {
                ctx.next.previous = node;
              }
            if (first == ctx)
              {
                first = node;
              }
            reparent(node, nodeName, ctx.index);
            ctx.parent = null;
            ctx.next = null;
            ctx.previous = null;
            ctx.setDepth(0);
            ctx.index = 0;
            return ctx;
          }
        last = ctx;
      }
    // append
    if (last != null)
      {
        last.next = node;
        node.previous = last;
      }
    else
      {
        first = node;
      }
    length++;
    reparent(node, nodeName, 0);
    return null;
  }

  void reparent(DomNode node, String nodeName, int i)
  {
    node.parent = owner;
    node.setDepth(owner.depth + 1);
    // index renumbering
    for (DomNode ctx = node; ctx != null; ctx = ctx.next)
      {
        ctx.index = i++;
      }
    // cache xml:space
    boolean xmlSpace = "xml:space".equals(nodeName);
    if (xmlSpace && owner instanceof DomElement)
      {
        ((DomElement) owner).xmlSpace = node.getNodeValue();
      }
  }

  /**
   * <b>DOM L1</b>
   * Removes the named item from the map, or reports an exception;
   * names are just the nodeName property.
   */
  public Node removeNamedItem(String name)
  {
    return removeNamedItem(null, name, false);
  }

  /**
   * <b>DOM L2</b>
   * Removes the named item from the map, or reports an exception;
   * names are the localName and namespaceURI properties.
   */
  public Node removeNamedItemNS(String namespaceURI, String localName)
  {
    return removeNamedItem(namespaceURI, localName, true);
  }

  Node removeNamedItem(String uri, String name, boolean ns)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }

    // report attribute REMOVAL event?

    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        boolean test = false;
        String nodeName = ctx.getNodeName();
        if (ns)
          {
            String tln = ctx.getLocalName();
            if (name != null && name.equals(tln))
              {
                String tu = ctx.getNamespaceURI();
                if ((tu == null && uri == null) ||
                    (tu != null && tu.equals(uri)))
                  {
                    test = true;
                  }
              }
          }
        else
          {
            test = nodeName.equals(name);
          }
        if (test)
          {
            // uncache xml:space
            boolean xmlSpace = "xml:space".equals(nodeName);
            if (xmlSpace && owner instanceof DomElement)
              {
                ((DomElement) owner).xmlSpace = "";
              }
            // is this a default attribute?
            if (ctx.nodeType == Node.ATTRIBUTE_NODE)
              {
                String def = getDefaultValue(ctx.getNodeName());
                if (def != null)
                  {
                    ctx.setNodeValue(def);
                    ((DomAttr) ctx).setSpecified(false);
                    return null;
                  }
              }
            // remove
            if (ctx == first)
              {
                first = ctx.next;
              }
            if (ctx.previous != null)
              {
                ctx.previous.next = ctx.next;
              }
            if (ctx.next != null)
              {
                ctx.next.previous = ctx.previous;
              }
            length--;
            ctx.previous = null;
            ctx.next = null;
            ctx.parent = null;
            ctx.setDepth(0);
            ctx.index = 0;
            return ctx;
          }
      }    
    throw new DomDOMException(DOMException.NOT_FOUND_ERR);
  }
  
  String getDefaultValue(String name)
  {
    DomDoctype doctype = (DomDoctype) owner.owner.getDoctype();
    if (doctype == null)
      {
        return null;
      }
    DTDAttributeTypeInfo info =
      doctype.getAttributeTypeInfo(owner.getNodeName(), name);
    if (info == null)
      {
        return null;
      }
    return info.value;
  }

  /**
   * <b>DOM L1</b>
   * Returns the indexed item from the map, or null.
   */
  public Node item(int index)
  {
    DomNode ctx = first;
    int count = 0;
    while (ctx != null && count < index)
      {
        ctx = ctx.next;
        count++;
      }
    return ctx;
  }

  /**
   * <b>DOM L1</b>
   * Returns the length of the map.
   */
  public int getLength()
  {
    return length;
  }
  
}
