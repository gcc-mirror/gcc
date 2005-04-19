/* DomHTMLElement.java -- 
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

package gnu.xml.dom.html2;

import gnu.xml.dom.DomDOMException;
import gnu.xml.dom.DomElement;
import gnu.xml.dom.DomEvent;
import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.events.UIEvent;
import org.w3c.dom.html2.HTMLElement;

/**
 * Abstract implementation of an HTML element node.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class DomHTMLElement
  extends DomElement
  implements HTMLElement
{

  protected DomHTMLElement(DomHTMLDocument owner, String namespaceURI,
                           String name)
  {
    super(owner, namespaceURI, name);
  }

  /**
   * Returns the value of the specified attribute.
   * The attribute name is case insensitive.
   */
  protected String getHTMLAttribute(String name)
  {
    if (hasAttributes())
      {
        NamedNodeMap attrs = getAttributes();
        int len = attrs.getLength();
        for (int i = 0; i < len; i++)
          {
            Node attr = attrs.item(i);
            String attrName = attr.getLocalName();
            if (attrName.equalsIgnoreCase(name))
              {
                return attr.getNodeValue();
              }
          }
      }
    return "";
  }

  protected int getIntHTMLAttribute(String name)
  {
    String value = getHTMLAttribute(name);
    if (value == null)
      {
        return -1;
      }
    try
      {
        return Integer.parseInt(value);
      }
    catch (NumberFormatException e)
      {
        return -1;
      }
  }

  protected boolean getBooleanHTMLAttribute(String name)
  {
    String value = getHTMLAttribute(name);
    return value != null;
  }

  /**
   * Sets the value of the specified attribute.
   * The attribute name is case insensitive.
   */
  protected void setHTMLAttribute(String name, String value)
  {
    Node attr;
    NamedNodeMap attrs = getAttributes();
    int len = attrs.getLength();
    for (int i = 0; i < len; i++)
      {
        attr = attrs.item(i);
        String attrName = attr.getLocalName();
        if (attrName.equalsIgnoreCase(name))
          {
            if (value != null)
              {
                attr.setNodeValue(value);
              }
            else
              {
                attrs.removeNamedItem(attr.getNodeName());
              }
            return;
          }
      }
    if (value != null)
      {
        // Create a new attribute
        DomHTMLDocument doc = (DomHTMLDocument) getOwnerDocument();
        // XXX namespace URI for attribute?
        attr = doc.createAttribute(name);
        attr.setNodeValue(value);
      }
  }

  protected void setIntHTMLAttribute(String name, int value)
  {
    setHTMLAttribute(name, Integer.toString(value));
  }

  protected void setBooleanHTMLAttribute(String name, boolean value)
  {
    setHTMLAttribute(name, value ? name : null);
  }

  /**
   * Returns the first parent element with the specified name.
   */
  protected Node getParentElement(String name)
  {
    for (Node parent = getParentNode(); parent != null;
         parent = parent.getParentNode())
      {
        if (name.equalsIgnoreCase(parent.getLocalName()))
          {
            return parent;
          }
      }
    return null;
  }

  /**
   * Returns the first child element with the specified name.
   */
  protected Node getChildElement(String name)
  {
    for (Node child = getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        if (name.equalsIgnoreCase(child.getLocalName()))
          {
            return child;
          }
      }
    return null;
  }

  /**
   * Returns the index of this element among elements of the same name,
   * relative to its parent.
   */
  protected int getIndex()
  {
    int index = 0;
    Node parent = getParentNode();
    if (parent != null)
      {
        for (Node ctx = parent.getFirstChild(); ctx != null;
             ctx = ctx.getNextSibling())
          {
            if (ctx == this)
              {
                return index;
              }
            index++;
          }
      }
    throw new DomDOMException(DOMException.NOT_FOUND_ERR);
  }

  protected void dispatchUIEvent(String name)
  {
    UIEvent event = new DomEvent.DomUIEvent(name);
    dispatchEvent(event);
  }

  public String getId()
  {
    return getHTMLAttribute("id");
  }

  public void setId(String id)
  {
    setHTMLAttribute("id", id);
  }
  
  public String getTitle()
  {
    return getHTMLAttribute("title");
  }

  public void setTitle(String title)
  {
    setHTMLAttribute("title", title);
  }
  
  public String getLang()
  {
    return getHTMLAttribute("lang");
  }

  public void setLang(String lang)
  {
    setHTMLAttribute("lang", lang);
  }
  
  public String getDir()
  {
    return getHTMLAttribute("dir");
  }

  public void setDir(String dir)
  {
    setHTMLAttribute("dir", dir);
  }
  
  public String getClassName()
  {
    return getHTMLAttribute("class");
  }

  public void setClassName(String className)
  {
    setHTMLAttribute("class", className);
  }
  
}

