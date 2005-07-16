/* GnomeElement.java - 
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

import java.util.HashSet;
import java.util.Set;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.TypeInfo;

/**
 * A DOM element node implemented in libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class GnomeElement
  extends GnomeNode
  implements Element
{

  /**
   * User-defined ID attributes.
   */
  Set userIdAttrs;

  GnomeElement(Object id)
  {
    super(id);
  }
  
  public String getTagName()
  {
    return getNodeName();
  }

  public native String getAttribute(String name);

  public native void setAttribute(String name, String value)
    throws DOMException;
  
  public void removeAttribute(String name)
    throws DOMException
  {
    Attr attr = getAttributeNode(name);
    if (attr != null)
      {
        removeAttributeNode(attr);
      }
  }

  public native Attr getAttributeNode(String name);
  
  public native Attr setAttributeNode(Attr newAttr)
    throws DOMException;

  public native Attr removeAttributeNode(Attr oldAttr)
    throws DOMException;

  public native NodeList getElementsByTagName(String name);
  
  public native String getAttributeNS(String namespaceURI, String localName);
  
  public native void setAttributeNS(String namespaceURI, String
                                    qualifiedName, String value)
    throws DOMException;

  public void removeAttributeNS(String namespaceURI, String localName)
    throws DOMException
  {
    Attr attr = getAttributeNodeNS(namespaceURI, localName);
    if (attr != null)
      {
        removeAttributeNode(attr);
      }
  }
  
  public native Attr getAttributeNodeNS(String namespaceURI,
                                        String localName);

  public native Attr setAttributeNodeNS(Attr newAttr)
    throws DOMException;

  public native NodeList getElementsByTagNameNS(String namespaceURI,
                                                String localName);
  
  public native boolean hasAttribute(String name);

  public native boolean hasAttributeNS(String namespaceURI,
                                       String localName);

  // DOM Level 3 methods

  public TypeInfo getSchemaTypeInfo()
  {
    return new GnomeTypeInfo(id);
  }

  public void setIdAttribute(String name, boolean isId)
  {
    Attr attr = getAttributeNode(name);
    setIdAttributeNode(attr, isId);
  }

  public void setIdAttributeNode(Attr attr, boolean isId)
  {
    if (attr == null)// FIXME || !attr.getOwnerElement().equals(this))
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, null);
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
    Attr attr = getAttributeNodeNS(namespaceURI, localName);
    setIdAttributeNode(attr, isId);
  }

  public String toString()
  {
    StringBuffer buffer = new StringBuffer(getClass().getName());
    buffer.append("[tagName=");
    buffer.append(getTagName());
    buffer.append("]");
    return buffer.toString();
  }

}
