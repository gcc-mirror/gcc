/* DefaultHighlighter.java --
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;

import javax.swing.plaf.TextUI;

public class DefaultHighlighter extends LayeredHighlighter
{
  public static class DefaultHighlightPainter
    extends LayerPainter
  {
    private Color color;
    
    public DefaultHighlightPainter(Color c)
    {
      super();
      color = c;
    }

    public Color getColor()
    {
      return color;
    }

    private void paintHighlight(Graphics g, Rectangle rect)
    {
      g.fillRect(rect.x, rect.y, rect.width, rect.height);
    }
    
    public void paint(Graphics g, int p0, int p1, Shape bounds,
		      JTextComponent c)
    {
      Rectangle r0 = null;
      Rectangle r1 = null;
      Rectangle rect = bounds.getBounds();
      
      try
	{
	  r0 = c.modelToView(p0);
	  r1 = c.modelToView(p1);
	}
      catch (BadLocationException e)
        {
	  // This should never occur.
          return;
	}
      
      if (r0 == null || r1 == null)
	return;

      if (color == null)
	g.setColor(c.getSelectionColor());
      else
	g.setColor(color);

      // Check if only one line to highlight.
      if (r0.y == r1.y)
	{
	  r0.width = r1.x - r0.x;
	  paintHighlight(g, r0);
	  return;
	}
      
      // First line, from p0 to end-of-line.
      r0.width = rect.x + rect.width - r0.x;
      paintHighlight(g, r0);
      
      // FIXME: All the full lines in between, if any (assumes that all lines
      // have the same height -- not a good assumption with JEditorPane/JTextPane).
      r0.y += r0.height;
      r0.x = rect.x;
      r0.width = rect.width;
      
      while (r0.y < r1.y)
	{
	  paintHighlight(g, r0);
	  r0.y += r0.height;
	}

      // Last line, from beginning-of-line to p1.
      // The "-1" is neccessary else we would paint one pixel column more
      // than in the case where the selection is only on one line. 
      r0.width = r1.x + r1.width - 1;
      paintHighlight(g, r0);
    }

    public Shape paintLayer(Graphics g, int p0, int p1, Shape bounds,
			    JTextComponent c, View view)
    {
      throw new InternalError();
    }
  }
  
  private class HighlightEntry implements Highlighter.Highlight
  {
    int p0;
    int p1;
    Highlighter.HighlightPainter painter;

    public HighlightEntry(int p0, int p1, Highlighter.HighlightPainter painter)
    {
      this.p0 = p0;
      this.p1 = p1;
      this.painter = painter;
    }

    public int getStartOffset()
    {
      return p0;
    }

    public int getEndOffset()
    {
      return p1;
    }

    public Highlighter.HighlightPainter getPainter()
    {
      return painter;
    }
  }

  /**
   * @specnote final as of 1.4
   */
  public static final LayeredHighlighter.LayerPainter DefaultPainter =
    new DefaultHighlightPainter(null);
  
  private JTextComponent textComponent;
  private ArrayList highlights = new ArrayList();
  private boolean drawsLayeredHighlights = true;
  
  public DefaultHighlighter()
  {
    // Nothing to do here.
  }

  public boolean getDrawsLayeredHighlights()
  {
    return drawsLayeredHighlights;
  }

  public void setDrawsLayeredHighlights(boolean newValue)
  {
    drawsLayeredHighlights = newValue;
  }
  
  private void checkPositions(int p0, int p1)
    throws BadLocationException
  {
    if (p0 < 0)
      throw new BadLocationException("DefaultHighlighter", p0);
    
    if (p1 < p0)
      throw new BadLocationException("DefaultHighlighter", p1);
  }

  public void install(JTextComponent c)
  {
    textComponent = c;
    removeAllHighlights();
  }

  public void deinstall(JTextComponent c)
  {
    textComponent = null;
  }

  public Object addHighlight(int p0, int p1, Highlighter.HighlightPainter painter)
    throws BadLocationException
  {
    checkPositions(p0, p1);
    HighlightEntry entry = new HighlightEntry(p0, p1, painter);
    highlights.add(entry);
    
    textComponent.getUI().damageRange(textComponent, p0, p1);
    
    return entry;
  }

  public void removeHighlight(Object tag)
  {
    highlights.remove(tag);

    HighlightEntry entry = (HighlightEntry) tag;
    textComponent.getUI().damageRange(textComponent,
                                      entry.p0,
                                      entry.p1);
  }

  public void removeAllHighlights()
  {
    highlights.clear();
  }

  public Highlighter.Highlight[] getHighlights()
  {
    return (Highlighter.Highlight[]) 
      highlights.toArray(new Highlighter.Highlight[highlights.size()]);
  }

  public void changeHighlight(Object tag, int n0, int n1)
    throws BadLocationException
  {
    int o0, o1;
    
    checkPositions(n0, n1);
    HighlightEntry entry = (HighlightEntry) tag;
    o0 = entry.p0;
    o1 = entry.p1;
    
    // Prevent useless write & repaint operations.
    if (o0 == n0 && o1 == n1)
      return;
    
    entry.p0 = n0;
    entry.p1 = n1;
    
    TextUI ui = textComponent.getUI();
    
    // Special situation where the old area has to be cleared simply.
    if (n0 == n1)
      ui.damageRange(textComponent, o0, o1);
    // Calculates the areas where a change is really neccessary
    else if ((o1 > n0 && o1 <= n1)
        || (n1 > o0 && n1 <= o1))
      {
        // [fds, fde) - the first damage region
        // [sds, sde] - the second damage region
        int fds, sds;
        int fde, sde;

        // Calculate first damaged region.
        if(o0 < n0)
          {
            // Damaged region will be cleared as
            // the old highlight region starts first.
            fds = o0;
            fde = n0;
          }
        else
          {
            // Damaged region will be painted as
            // the new highlight region starts first.
            fds = n0;
            fde = o0;
          }

        if (o1 < n1)
          {
            // Final region will be painted as the
            // old highlight region finishes first
            sds = o1;
            sde = n1;
          }
        else
          {
            // Final region will be cleared as the
            // new highlight region finishes first.
            sds = n1;
            sde = o1;
          }
        
        // If there is no undamaged region in between
        // call damageRange only once.
        if (fde == sds)
          ui.damageRange(textComponent, fds, sde);
        else
          {
            if (fds != fde)
              ui.damageRange(textComponent, fds, fde);
        
            if (sds != sde)
              ui.damageRange(textComponent, sds, sde);
          }
      }
    else
      {
        // The two regions do not overlap. So mark
        // both areas as damaged.
        ui.damageRange(textComponent, o0, o1);
        ui.damageRange(textComponent, n0, n1);
      }
    
  }

  public void paintLayeredHighlights(Graphics g, int p0, int p1,
                                     Shape viewBounds, JTextComponent editor,
                                     View view)
  {
    // TODO: Implement this properly.
  }

  public void paint(Graphics g)
  {
    int size = highlights.size();
    
    // Check if there are any highlights.
    if (size == 0)
      return;

    // Prepares the rectangle of the inner drawing area.
    Insets insets = textComponent.getInsets();
    Shape bounds =
      new Rectangle(insets.left,
                    insets.top,
                    textComponent.getWidth() - insets.left - insets.right,
                    textComponent.getHeight() - insets.top - insets.bottom);
    
    for (int index = 0; index < size; ++index)
      {
	HighlightEntry entry = (HighlightEntry) highlights.get(index);
	entry.painter.paint(g, entry.p0, entry.p1, bounds, textComponent);
      }
  }
}
