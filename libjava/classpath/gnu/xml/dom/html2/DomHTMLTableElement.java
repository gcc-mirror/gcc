/* DomHTMLTableElement.java --
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
import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.html2.HTMLCollection;
import org.w3c.dom.html2.HTMLElement;
import org.w3c.dom.html2.HTMLTableCaptionElement;
import org.w3c.dom.html2.HTMLTableElement;
import org.w3c.dom.html2.HTMLTableSectionElement;

/**
 * An HTML 'TABLE' element node.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomHTMLTableElement
  extends DomHTMLElement
  implements HTMLTableElement
{

  protected DomHTMLTableElement(DomHTMLDocument owner, String namespaceURI,
                                String name)
  {
    super(owner, namespaceURI, name);
  }

  public HTMLTableCaptionElement getCaption()
  {
    return (HTMLTableCaptionElement) getChildElement("caption");
  }

  public void setCaption(HTMLTableCaptionElement caption)
  {
    HTMLTableCaptionElement ref = getCaption();
    if (ref == null)
      {
        appendChild(caption);
      }
    else
      {
        replaceChild(caption, ref);
      }
  }

  public HTMLTableSectionElement getTHead()
  {
    return (HTMLTableSectionElement) getChildElement("thead");
  }

  public void setTHead(HTMLTableSectionElement tHead)
  {
    HTMLTableSectionElement ref = getTHead();
    if (ref == null)
      {
        appendChild(tHead);
      }
    else
      {
        replaceChild(tHead, ref);
      }
  }

  public HTMLTableSectionElement getTFoot()
  {
    return (HTMLTableSectionElement) getChildElement("tfoot");
  }

  public void setTFoot(HTMLTableSectionElement tFoot)
  {
    HTMLTableSectionElement ref = getTFoot();
    if (ref == null)
      {
        appendChild(tFoot);
      }
    else
      {
        replaceChild(tFoot, ref);
      }
  }

  public HTMLCollection getRows()
  {
    DomHTMLCollection ret =
      new DomHTMLCollection((DomHTMLDocument) getOwnerDocument(), this);
    ret.addNodeName("tr");
    ret.evaluate();
    return ret;
  }

  public HTMLCollection getTBodies()
  {
    DomHTMLCollection ret =
      new DomHTMLCollection((DomHTMLDocument) getOwnerDocument(), this);
    ret.addNodeName("tbody");
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

  public String getBorder()
  {
    return getHTMLAttribute("border");
  }

  public void setBorder(String border)
  {
    setHTMLAttribute("border", border);
  }

  public String getCellPadding()
  {
    return getHTMLAttribute("cellpadding");
  }

  public void setCellPadding(String cellPadding)
  {
    setHTMLAttribute("cellpadding", cellPadding);
  }

  public String getCellSpacing()
  {
    return getHTMLAttribute("cellspacing");
  }

  public void setCellSpacing(String cellSpacing)
  {
    setHTMLAttribute("cellspacing", cellSpacing);
  }

  public String getFrame()
  {
    return getHTMLAttribute("frame");
  }

  public void setFrame(String frame)
  {
    setHTMLAttribute("frame", frame);
  }

  public String getRules()
  {
    return getHTMLAttribute("rules");
  }

  public void setRules(String rules)
  {
    setHTMLAttribute("rules", rules);
  }

  public String getSummary()
  {
    return getHTMLAttribute("summary");
  }

  public void setSummary(String summary)
  {
    setHTMLAttribute("summary", summary);
  }

  public String getWidth()
  {
    return getHTMLAttribute("width");
  }

  public void setWidth(String width)
  {
    setHTMLAttribute("width", width);
  }

  public HTMLElement createTHead()
  {
    HTMLTableSectionElement ref = getTHead();
    if (ref == null)
      {
        return (HTMLElement) getOwnerDocument().createElement("thead");
      }
    else
      {
        return ref;
      }
  }

  public void deleteTHead()
  {
    HTMLTableSectionElement ref = getTHead();
    if (ref != null)
      {
        removeChild(ref);
      }
  }

  public HTMLElement createTFoot()
  {
    HTMLTableSectionElement ref = getTFoot();
    if (ref == null)
      {
        return (HTMLElement) getOwnerDocument().createElement("tfoot");
      }
    else
      {
        return ref;
      }
  }

  public void deleteTFoot()
  {
    HTMLTableSectionElement ref = getTFoot();
    if (ref != null)
      {
        removeChild(ref);
      }
  }

  public HTMLElement createCaption()
  {
    HTMLTableCaptionElement ref = getCaption();
    if (ref == null)
      {
        return (HTMLElement) getOwnerDocument().createElement("caption");
      }
    else
      {
        return ref;
      }
  }

  public void deleteCaption()
  {
    HTMLTableCaptionElement ref = getCaption();
    if (ref != null)
      {
        removeChild(ref);
      }
  }

  public HTMLElement insertRow(int index)
  {
    Node ref = getRow(index);
    Node row = getOwnerDocument().createElement("tr");
    if (ref == null)
      {
        Node tbody = getChildElement("tbody");
        if (tbody == null)
          {
            tbody = getOwnerDocument().createElement("tfoot");
            appendChild(tbody);
          }
        tbody.appendChild(row);
      }
    else
      {
        ref.getParentNode().insertBefore(row, ref);
      }
    return (HTMLElement) row;
  }

  public void deleteRow(int index)
  {
    Node ref = getRow(index);
    if (ref == null)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
    ref.getParentNode().removeChild(ref);
  }

  Node getRow(final int index)
  {
    int i = 0;
    Node thead = getChildElement("thead");
    if (thead != null)
      {
        for (Node ctx = thead.getFirstChild(); ctx != null;
             ctx = ctx.getNextSibling())
          {
            String ctxName = ctx.getLocalName();
            if (ctxName == null)
              {
                ctxName = ctx.getNodeName();
              }
            if (!"tr".equalsIgnoreCase(ctxName))
              {
                continue;
              }
            if (index == i)
              {
                return ctx;
              }
            i++;
          }
      }
    Node tbody = getChildElement("tbody");
    if (tbody == null)
      {
        tbody = this;
      }
    for (Node ctx = tbody.getFirstChild(); ctx != null;
         ctx = ctx.getNextSibling())
      {
        String ctxName = ctx.getLocalName();
        if (ctxName == null)
          {
            ctxName = ctx.getNodeName();
          }
        if (!"tr".equalsIgnoreCase(ctxName))
          {
            continue;
          }
        if (index == i)
          {
            return ctx;
          }
        i++;
      }
    Node tfoot = getChildElement("tfoot");
    if (tfoot != null)
      {
        for (Node ctx = tfoot.getFirstChild(); ctx != null;
             ctx = ctx.getNextSibling())
          {
            String ctxName = ctx.getLocalName();
            if (ctxName == null)
              {
                ctxName = ctx.getNodeName();
              }
            if (!"tr".equalsIgnoreCase(ctxName))
              {
                continue;
              }
            if (index == i)
              {
                return ctx;
              }
            i++;
          }
      }
    return null;
  }

}
