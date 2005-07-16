/* DomNsNode.java -- 
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

import javax.xml.XMLConstants;
import org.w3c.dom.DOMException;

/**
 * <p> Abstract implemention of namespace support.  This facilitates
 * sharing code for attribute and element nodes.
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class DomNsNode
  extends DomNode
{
  
  private String name;
  private String namespace;
  private String prefix;
  String localName;
  
  /**
   * Constructs a node associated with the specified document, and
   * with the specified namespace information.
   *
   * @param owner The document with which this entity is associated
   * @param namespaceURI Combined with the local part of the name,
   *	this identifies a type of element or attribute; may be null.
   *  If this is the empty string, it is reassigned as null so that
   *  applications only need to test that case.
   * @param name Name of this node, which may include a prefix
   */
  // package private
  DomNsNode(short nodeType, DomDocument owner, String namespaceURI, String name)
  {
    super(nodeType, owner);
    setNodeName(name);
    setNamespaceURI(namespaceURI);
  }

  /**
   * <b>DOM L1</b>
   * Returns the node's name, including any namespace prefix.
   */
  public final String getNodeName()
  {
    return name;
  }

  final void setNodeName(String name)
  {
    this.name = name.intern();
    int index = name.indexOf(':');
    if (index == -1)
      {
        prefix = null;
        localName = this.name;
      }
    else
      {
        prefix = name.substring(0, index).intern();
        localName = name.substring(index + 1).intern();
      }
  }
  
  /**
   * <b>DOM L2</b>
   * Returns the node's namespace URI
   * <em>or null</em> if the node name is not namespace scoped.
   */
  public final String getNamespaceURI()
  {
    return namespace;
  }

  final void setNamespaceURI(String namespaceURI)
  {
    if ("".equals(namespaceURI))
      {
        namespaceURI = null;
      }
    namespace = (namespaceURI == null) ? null : namespaceURI.intern();
  }
  
  /**
   * <b>DOM L2</b>
   * Returns any prefix part of the node's name (before any colon).
   */
  public final String getPrefix()
  {
    return prefix;
  }

  /**
   * <b>DOM L2</b>
   * Assigns the prefix part of the node's name (before any colon).
   */
  public final void setPrefix(String prefix)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }

    if (prefix == null)
      {
        name = localName;
        return;
      }
    else if (namespace == null)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "can't set prefix, node has no namespace URI",
                                  this, 0);
      }

    DomDocument.checkName(prefix, "1.1".equals(owner.getXmlVersion()));
    if (prefix.indexOf (':') != -1)
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "illegal prefix " + prefix, this, 0);
      }

    if (XMLConstants.XML_NS_PREFIX.equals(prefix)
        && !XMLConstants.XML_NS_URI.equals(namespace))
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "xml namespace is always " +
                                  XMLConstants.XML_NS_URI, this, 0);
      }

    if (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix))
      {
        if (namespace != null || getNodeType() != ATTRIBUTE_NODE)
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      "xmlns attribute prefix is reserved",
                                      this, 0);
          }
      }
    else if (getNodeType () == ATTRIBUTE_NODE
             && (XMLConstants.XMLNS_ATTRIBUTE.equals(name) ||
                 name.startsWith("xmlns:")))
      {
        throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                  "namespace declarations can't change names",
                                  this, 0);
      }

    this.prefix = prefix.intern();
  }

  /**
   * <b>DOM L2</b>
   * Returns the local part of the node's name (after any colon).
   */
  public final String getLocalName()
  {
    return localName;
  }
  
}

