/* DefaultHighlighter.java --
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


package javax.swing.text;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Vector;

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

      while (r0.y < r1.y)
	{
	  paintHighlight(g, r0);
	  r0.y += r0.height;
	}

      // Last line, from beginnin-of-line to p1.
      paintHighlight(g, r1);
    }

    public Shape paintLayer(Graphics g, int p0, int p1, Shape bounds,
			    JTextComponent c, View view)
    {
      throw new InternalError();
    }
  }
  
  private class HighlightEntry
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

    public int getStartPosition()
    {
      return p0;
    }

    public int getEndPosition()
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
  private Vector highlights = new Vector();
  private boolean drawsLayeredHighlights = true;
  
  public DefaultHighlighter()
  {
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
    return entry;
  }

  public void removeHighlight(Object tag)
  {
    highlights.remove(tag);
  }

  public void removeAllHighlights()
  {
    highlights.clear();
  }

  public Highlighter.Highlight[] getHighlights()
  {
    return null;
  }

  public void changeHighlight(Object tag, int p0, int p1)
    throws BadLocationException
  {
    checkPositions(p0, p1);
    HighlightEntry entry = (HighlightEntry) tag;
    entry.p0 = p0;
    entry.p1 = p1;
  }

  public void paintLayeredHighlights(Graphics g, int p0, int p1,
                                     Shape viewBounds, JTextComponent editor,
                                     View view)
  {
  }

  public void paint(Graphics g)
  {
    // Check if there are any highlights.
    if (highlights.size() == 0)
      return;
    
    Shape bounds = textComponent.getBounds();
    
    for (int index = 0; index < highlights.size(); ++index)
      {
	HighlightEntry entry = (HighlightEntry) highlights.get(index);
	entry.painter.paint(g, entry.p0, entry.p1, bounds, textComponent);
      }
  }
}
