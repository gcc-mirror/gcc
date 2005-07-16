/* IIONamedNodeMap.java --
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

import java.util.HashMap;

import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * Simple NamedNodeMap class for IIOMetadataNode.
 *
 * @author jlquinn
 */
class IIONamedNodeMap implements NamedNodeMap
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
    if (arg instanceof IIOAttr)
    {
      IIOAttr attr = (IIOAttr) arg;
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
