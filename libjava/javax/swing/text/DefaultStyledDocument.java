/* DefaultStyledDocument.java --
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


package javax.swing.text;

import java.awt.Color;
import java.awt.Font;
import java.io.Serializable;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class DefaultStyledDocument extends AbstractDocument
  implements StyledDocument
{
  public class ElementBuffer
    implements Serializable
  {
    private Element root;
    
    public ElementBuffer(Element root)
    {
      this.root = root;
    }

    public Element getRootElement()
    {
      return root;
    }
  }
  
  public static final int BUFFER_SIZE_DEFAULT = 4096;

  protected DefaultStyledDocument.ElementBuffer buffer;
  
  public DefaultStyledDocument()
  {
    this(new GapContent(BUFFER_SIZE_DEFAULT), new StyleContext());
  }

  public DefaultStyledDocument(StyleContext context)
  {
    this(new GapContent(BUFFER_SIZE_DEFAULT), context);
  }

  public DefaultStyledDocument(AbstractDocument.Content content,
			       StyleContext context)
  {
    super(content, context);
    buffer = new ElementBuffer(createDefaultRoot());
    setLogicalStyle(0, context.getStyle(StyleContext.DEFAULT_STYLE));
  }

  public Style addStyle(String nm, Style parent)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.addStyle(nm, parent);
  }
  
  protected AbstractDocument.AbstractElement createDefaultRoot()
  {
    Element[] tmp;
    BranchElement section = new BranchElement(null, null);
    
    BranchElement paragraph = new BranchElement(section, null);
    tmp = new Element[1];
    tmp[0] = paragraph;
    section.replace(0, 0, tmp);

    LeafElement leaf = new LeafElement(paragraph, null, 0, 1);
    tmp = new Element[1];
    tmp[0] = leaf;
    paragraph.replace(0, 0, tmp);
    
    return section;
  }
  
  public Element getCharacterElement(int position)
  {
    Element element = getDefaultRootElement();

    while (! element.isLeaf())
      {
	int index = element.getElementIndex(position);
	element = element.getElement(index);
      }
    
    return element;
  }
  
  public Color getBackground(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getBackground(attributes);
  }
  
  public Element getDefaultRootElement()
  {
    return buffer.getRootElement();
  }
  
  public Font getFont(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getFont(attributes);
  }
  
  public Color getForeground(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getForeground(attributes);
  }
  
  public Style getLogicalStyle(int position)
  {
    Element paragraph = getParagraphElement(position);
    AttributeSet attributes = paragraph.getAttributes();
    return (Style) attributes.getResolveParent();
  }
  
  public Element getParagraphElement(int position)
  {
    Element element = getCharacterElement(position);
    return element.getParentElement();
  }

  public Style getStyle(String nm)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getStyle(nm);
  }

  public void removeStyle(String nm)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    context.removeStyle(nm);
  }

  public void setCharacterAttributes(int offset, int length,
				     AttributeSet attributes,
				     boolean replace)
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
  }
  
  public void setLogicalStyle(int position, Style style)
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
  }

  public void setParagraphAttributes(int offset, int length,
				     AttributeSet attributes,
				     boolean replace)
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
  }
}
