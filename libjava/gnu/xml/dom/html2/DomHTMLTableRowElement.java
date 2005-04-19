/* DomHTMLTableRowElement.java -- 
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
import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.html2.HTMLCollection;
import org.w3c.dom.html2.HTMLElement;
import org.w3c.dom.html2.HTMLTableRowElement;

/**
 * An HTML 'TR' element node.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomHTMLTableRowElement
  extends DomHTMLElement
  implements HTMLTableRowElement
{

  protected DomHTMLTableRowElement(DomHTMLDocument owner,
                                   String namespaceURI,
                                   String name)
  {
    super(owner, namespaceURI, name);
  }

  public int getRowIndex()
  {
    return getIndex();
  }

  public int getSectionRowIndex()
  {
    int index = 0;
    DomHTMLElement parent = (DomHTMLElement) getParentElement("table");
    if (parent != null)
      {
        Node thead = parent.getChildElement("thead");
        if (thead != null)
          {
            for (Node ctx = thead.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                if (ctx == this)
                  {
                    return index;
                  }
                index++;
              }
          }
        Node tbody = parent.getChildElement("tbody");
        if (tbody != null)
          {
            for (Node ctx = tbody.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                if (ctx == this)
                  {
                    return index;
                  }
                index++;
              }
          }
        Node tfoot = parent.getChildElement("tfoot");
        if (tfoot != null)
          {
            for (Node ctx = tfoot.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                if (ctx == this)
                  {
                    return index;
                  }
                index++;
              }
          }
      }
    throw new DomDOMException(DOMException.NOT_FOUND_ERR);
  }

  public HTMLCollection getCells()
  {
    DomHTMLCollection ret =
      new DomHTMLCollection((DomHTMLDocument) getOwnerDocument(), this);
    ret.addNodeName("th");
    ret.addNodeName("td");
    ret.evaluate();
    return ret;
  }

  public String getAlign()
  {
    return getHTMLAttribute("align");
  }

  public void setAlign(String align)
  {
    setHTMLAttribute("align", align);
  }
  
  public String getBgColor()
  {
    return getHTMLAttribute("bgcolor");
  }

  public void setBgColor(String bgColor)
  {
    setHTMLAttribute("bgcolor", bgColor);
  }
  
  public String getCh()
  {
    return getHTMLAttribute("char");
  }

  public void setCh(String ch)
  {
    setHTMLAttribute("char", ch);
  }
  
  public String getChOff()
  {
    return getHTMLAttribute("charoff");
  }

  public void setChOff(String chOff)
  {
    setHTMLAttribute("charoff", chOff);
  }
  
  public String getVAlign()
  {
    return getHTMLAttribute("valign");
  }

  public void setVAlign(String vAlign)
  {
    setHTMLAttribute("valign", vAlign);
  }

  public HTMLElement insertCell(int index)
  {
    Node ref = getCell(index);
    Node cell = getOwnerDocument().createElement("td");
    if (ref == null)
      {
        appendChild(cell);
      }
    else
      {
        insertBefore(cell, ref);
      }
    return (HTMLElement) cell;
  }

  public void deleteCell(int index)
  {
    Node ref = getCell(index);
    if (ref == null)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
    removeChild(ref);
  }
  
  Node getCell(final int index)
  { 
    int i = 0;
    for (Node ctx = getFirstChild(); ctx != null;
         ctx = ctx.getNextSibling())
      {
        String name = ctx.getLocalName();
        if (!"td".equalsIgnoreCase(name) &&
            !"th".equalsIgnoreCase(name))
          {
            continue;
          }
        if (index == i)
          {
            return ctx;
          }
        i++;
      }
    return null;
  }
  
}

