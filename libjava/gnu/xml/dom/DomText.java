/* DomText.java -- 
   Copyright (C) 1999, 2000, 2001, 2004 Free Software Foundation, Inc.

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

package gnu.xml.dom;

import org.w3c.dom.DOMException;
import org.w3c.dom.Text;

/**
 * <p> "Text" implementation.  </p>
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomText
  extends DomCharacterData
  implements Text
{
  
  // NOTE:  deleted unused per-instance "isIgnorable"
  // support to reclaim its space.
  
  /**
   * Constructs a text node associated with the specified
   * document and holding the specified data.
   *
   * <p>This constructor should only be invoked by a Document object
   * as part of its createTextNode functionality, or through a subclass
   * which is similarly used in a "Sub-DOM" style layer. 
   */
  protected DomText(DomDocument owner, String value)
  {
    super(TEXT_NODE, owner, value);
  }

  protected DomText(DomDocument owner, char[] buf, int off, int len)
  {
    super(TEXT_NODE, owner, buf, off, len);
  }

  // Used by DomCDATA
  DomText(short nodeType, DomDocument owner, String value)
  {
    super(nodeType, owner, value);
  }

  DomText(short nodeType, DomDocument owner, char[] buf, int off, int len)
  {
    super(nodeType, owner, buf, off, len);
  }

  /**
   * <b>DOM L1</b>
   * Returns the string "#text".
   */
  // can't be 'final' with CDATA subclassing
  public String getNodeName()
  {
    return "#text";
  }

  /**
   * <b>DOM L1</b>
   * Splits this text node in two parts at the offset, returning
   * the new text node (the sibling with the second part).
   */
  public Text splitText(int offset)
  {
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    try
      {
        String text = getNodeValue();
        String before = text.substring(0, offset);
        String after = text.substring(offset);
        Text next;
        
        if (getNodeType() == TEXT_NODE)
          {
            next = owner.createTextNode(after);
          }
        else // CDATA_SECTION_NODE
          {
            next = owner.createCDATASection(after);
          }
        
        if (this.next != null)
          {
            parent.insertBefore(next, this.next);
          }
        else
          {
            parent.appendChild(next);
          }
        setNodeValue(before);
        return next;
        
      }
    catch (IndexOutOfBoundsException x)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
  }
    
  // DOM Level 3
  
  public boolean isElementContentWhitespace()
  {
    if (parent != null)
      {
        DomDoctype doctype = (DomDoctype) owner.getDoctype();
        if (doctype != null)
          {
            DTDElementTypeInfo info =
              doctype.getElementTypeInfo(parent.getNodeName());
            if (info != null)
              {
                if (info.model == null && info.model.indexOf("#PCDATA") != -1)
                  {
                    return false;
                  }
                return getNodeValue().trim().length() == 0;
              }
          }
      }
    return false;
  }

  public String getWholeText()
  {
    DomNode ref = this;
    DomNode ctx;
    for (ctx = previous; ctx != null &&
         (ctx.nodeType == TEXT_NODE || ctx.nodeType == CDATA_SECTION_NODE);
         ctx = ctx.previous)
      {
        ref = ctx;
      }
    StringBuffer buf = new StringBuffer(ref.getNodeValue());
    for (ctx = ref.next; ctx != null &&
         (ctx.nodeType == TEXT_NODE || ctx.nodeType == CDATA_SECTION_NODE);
         ctx = ctx.next)
      {
        buf.append(ctx.getNodeValue());
      }
    return buf.toString ();
  }

  public Text replaceWholeText(String content)
    throws DOMException
  {
    boolean isEmpty = (content == null || content.length () == 0);
    if (!isEmpty)
      {
        setNodeValue(content);
      }
    
    DomNode ref = this;
    DomNode ctx;
    for (ctx = previous; ctx != null &&
         (ctx.nodeType == TEXT_NODE || ctx.nodeType == CDATA_SECTION_NODE);
         ctx = ctx.previous)
      {
        ref = ctx;
      }
    ctx = ref.next;
    if ((isEmpty || ref != this) && parent != null)
      {
        parent.removeChild(ref);
      }
    for (; ctx != null &&
         (ctx.nodeType == TEXT_NODE || ctx.nodeType == CDATA_SECTION_NODE);
         ctx = ref)
      {
        ref = ctx.next;
        if ((isEmpty || ctx != this) && parent != null)
          {
            parent.removeChild(ctx);
          }
      }
    return (isEmpty) ? null : this;
  }
    
}
