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

import java.awt.Graphics;
import java.awt.Shape;
import java.util.Vector;

import javax.swing.text.JTextComponent;
import javax.swing.text.View;


public class DefaultHighlighter extends LayeredHighlighter
{
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

  private JTextComponent textComponent;
  private Vector highlights = new Vector();
  
  public DefaultHighlighter()
  {
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
  }
}
