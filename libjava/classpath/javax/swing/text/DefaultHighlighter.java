/* DefaultHighlighter.java -- The default highlight for Swing
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
import java.util.Iterator;

import javax.swing.SwingUtilities;
import javax.swing.plaf.TextUI;

/**
 * The default highlight for Swing text components. It highlights text
 * by filling the background with a rectangle. 
 */
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

    public void paint(Graphics g, int p0, int p1, Shape bounds,
		      JTextComponent t)
    {
      if (p0 == p1)
        return;
      
      Rectangle rect = bounds.getBounds();

      Color col = getColor();
      if (col == null)
        col = t.getSelectionColor();
      g.setColor(col);

      TextUI ui = t.getUI();
      
      try
        {

          Rectangle l0 = ui.modelToView(t, p0, null);
          Rectangle l1 = ui.modelToView(t, p1, null);
        
          // Note: The computed locations may lie outside of the allocation
          // area if the text is scrolled.

          if (l0.y == l1.y)
          {
            SwingUtilities.computeUnion(l0.x, l0.y, l0.width, l0.height, l1);

            // Paint only inside the allocation area.
            SwingUtilities.computeIntersection(rect.x, rect.y, rect.width,
					       rect.height, l1);
        
            g.fillRect(l1.x, l1.y, l1.width, l1.height);
          }
        else
          {
            // 1. The line of p0 is painted from the position of p0
            // to the right border.
            // 2. All lines between the ones where p0 and p1 lie on
            // are completely highlighted. The allocation area is used to find
            // out the bounds.
            // 3. The final line is painted from the left border to the
            // position of p1.

	    int firstLineWidth = rect.x + rect.width - l0.x;
	    g.fillRect(l0.x, l0.y, firstLineWidth, l0.height);
	    if (l0.y + l0.height != l1.y)
	      {
		g.fillRect(rect.x, l0.y + l0.height, rect.width,
			   l1.y - l0.y - l0.height);
	      }
	    g.fillRect(rect.x, l1.y, l1.x - rect.x, l1.height);
	  }
      }
    catch (BadLocationException ex)
      {
	// Can't render. Comment out for debugging.
	// ex.printStackTrace();
      }
    }

    public Shape paintLayer(Graphics g, int p0, int p1, Shape bounds,
			    JTextComponent c, View view)
    {
      Color col = getColor();
      if (col == null)
	col = c.getSelectionColor();
      g.setColor(col);

      Rectangle rect = null;
      if (p0 == view.getStartOffset() && p1 == view.getEndOffset())
	{
	  // Paint complete bounds region.
          rect = bounds instanceof Rectangle ? (Rectangle) bounds
	                                     : bounds.getBounds();
	}
      else
	{
	  // Only partly inside the view.
          try
	    {
              Shape s = view.modelToView(p0, Position.Bias.Forward,
					 p1, Position.Bias.Backward,
					 bounds);
	      rect = s instanceof Rectangle ? (Rectangle) s : s.getBounds();
	    }
	  catch (BadLocationException ex)
	    {
	      // Can't render the highlight.
	    }
	}

      if (rect != null)
	{
          g.fillRect(rect.x, rect.y, rect.width, rect.height);
	}
      return rect;
    }
  }
  
  private class HighlightEntry implements Highlighter.Highlight
  {
    Position p0;
    Position p1;
    Highlighter.HighlightPainter painter;

    public HighlightEntry(Position p0, Position p1,
                          Highlighter.HighlightPainter painter)
    {
      this.p0 = p0;
      this.p1 = p1;
      this.painter = painter;
    }

    public int getStartOffset()
    {
      return p0.getOffset();
    }

    public int getEndOffset()
    {
      return p1.getOffset();
    }

    public Highlighter.HighlightPainter getPainter()
    {
      return painter;
    }
  }

  /**
   * A HighlightEntry that is used for LayerPainter painters. In addition
   * to the info maintained by the HighlightEntry, this class maintains
   * a painting rectangle. This is used as repaint region when the
   * highlight changes and the text component needs repainting.
   */
  private class LayerHighlightEntry
    extends HighlightEntry
  {

    /**
     * The paint rectangle.
     */
    Rectangle paintRect = new Rectangle();

    LayerHighlightEntry(Position p0, Position p1,
                        Highlighter.HighlightPainter p)
    {
      super(p0, p1, p);
    }

    /**
     * Paints the highlight by calling the LayerPainter. This
     * restricts the area to be painted by startOffset and endOffset
     * and manages the paint rectangle.
     */
    void paintLayeredHighlight(Graphics g, int p0, int p1, Shape bounds,
			       JTextComponent tc, View view)
    {
      p0 = Math.max(getStartOffset(), p0);
      p1 = Math.min(getEndOffset(), p1);

      Highlighter.HighlightPainter painter = getPainter();
      if (painter instanceof LayerPainter)
	{
	  LayerPainter layerPainter = (LayerPainter) painter;
	  Shape area = layerPainter.paintLayer(g, p0, p1, bounds, tc, view);
	  Rectangle rect;
	  if (area instanceof Rectangle && paintRect != null)
	    rect = (Rectangle) area;
	  else
	    rect = area.getBounds();

	  if (paintRect.width == 0 || paintRect.height == 0)
	    paintRect = rect.getBounds();
	  else
	    paintRect = SwingUtilities.computeUnion(rect.x, rect.y, rect.width,
						    rect.height, paintRect);
	}
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

  public Object addHighlight(int p0, int p1,
                             Highlighter.HighlightPainter painter)
    throws BadLocationException
  {
    checkPositions(p0, p1);
    HighlightEntry entry;
    Document doc = textComponent.getDocument();
    Position pos0 = doc.createPosition(p0);
    Position pos1 = doc.createPosition(p1);
    if (getDrawsLayeredHighlights() && painter instanceof LayerPainter)
      entry = new LayerHighlightEntry(pos0, pos1, painter);
    else
      entry = new HighlightEntry(pos0, pos1, painter);
    highlights.add(entry);
    
    textComponent.getUI().damageRange(textComponent, p0, p1);
    
    return entry;
  }

  public void removeHighlight(Object tag)
  {
    HighlightEntry entry = (HighlightEntry) tag;
    if (entry instanceof LayerHighlightEntry)
      {
	LayerHighlightEntry lEntry = (LayerHighlightEntry) entry;
	Rectangle paintRect = lEntry.paintRect;
	textComponent.repaint(paintRect.x, paintRect.y, paintRect.width,
			      paintRect.height);
      }
    else
      {
	textComponent.getUI().damageRange(textComponent,
					  entry.getStartOffset(),
					  entry.getEndOffset());
      }
    highlights.remove(tag);

  }

  public void removeAllHighlights()
  {
    // Repaint damaged region.
    int minX = 0;
    int maxX = 0;
    int minY = 0;
    int maxY = 0;
    int p0 = -1;
    int p1 = -1;
    for (Iterator i = highlights.iterator(); i.hasNext();)
      {
        HighlightEntry e = (HighlightEntry) i.next();
        if (e instanceof LayerHighlightEntry)
          {
            LayerHighlightEntry le = (LayerHighlightEntry) e;
            Rectangle r = le.paintRect;
            minX = Math.min(r.x, minX);
            maxX = Math.max(r.x + r.width, maxX);
            minY = Math.min(r.y, minY);
            maxY = Math.max(r.y + r.height, maxY);
          }
        else
          {
            if (p0 == -1 || p1 == -1)
              {
                p0 = e.getStartOffset();
                p1 = e.getEndOffset();
              }
            else
              {
                p0 = Math.min(p0, e.getStartOffset());
                p1 = Math.max(p1, e.getEndOffset());
              }
          }
        if (minX != maxX && minY != maxY)
          textComponent.repaint(minX, minY, maxX - minX, maxY - minY);
        if (p0 != -1 && p1 != -1)
          {
            TextUI ui = textComponent.getUI();
            ui.damageRange(textComponent, p0, p1);
          }
        
      }
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
    Document doc = textComponent.getDocument();
    TextUI ui = textComponent.getUI();
    if (tag instanceof LayerHighlightEntry)
      {
        LayerHighlightEntry le = (LayerHighlightEntry) tag;
        Rectangle r = le.paintRect;
        if (r.width > 0 && r.height > 0)
          textComponent.repaint(r.x, r.y, r.width, r.height);
        r.width = 0;
        r.height = 0;
        le.p0 = doc.createPosition(n0);
        le.p1 = doc.createPosition(n1);
        ui.damageRange(textComponent, Math.min(n0, n1), Math.max(n0, n1));
      }
    else if (tag instanceof HighlightEntry)
      {
        HighlightEntry e = (HighlightEntry) tag;
        int p0 = e.getStartOffset();
        int p1 = e.getEndOffset();
        if (p0 == n0)
          {
            ui.damageRange(textComponent, Math.min(p1, n1),
                           Math.max(p1, n1));
          }
        else if (n1 == p1)
          {
            ui.damageRange(textComponent, Math.min(p0, n0),
                           Math.max(p0, n0));
          }
        else
          {
            ui.damageRange(textComponent, p0, p1);
            ui.damageRange(textComponent, n0, n1);
          }
        e.p0 = doc.createPosition(n0);
        e.p1 = doc.createPosition(n1);
      }
  }

  public void paintLayeredHighlights(Graphics g, int p0, int p1,
                                     Shape viewBounds, JTextComponent editor,
                                     View view)
  {
    for (Iterator i = highlights.iterator(); i.hasNext();)
      {
	Object o = i.next();
	if (o instanceof LayerHighlightEntry)
	  {
	    LayerHighlightEntry entry = (LayerHighlightEntry) o;
	    int start = entry.getStartOffset();
	    int end = entry.getEndOffset();
	    if ((p0 < start && p1 > start) || (p0 >= start && p0 < end))
	      entry.paintLayeredHighlight(g, p0, p1, viewBounds, editor, view);
	  }
      }
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
	if (! (entry instanceof LayerHighlightEntry))
          entry.painter.paint(g, entry.getStartOffset(), entry.getEndOffset(),
                              bounds, textComponent);
      }
  }
}
