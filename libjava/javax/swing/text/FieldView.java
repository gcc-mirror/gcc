/* FieldView.java -- 
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

import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Shape;

public class FieldView extends PlainView
{
  public FieldView(Element elem)
  {
    super(elem);
  }

  protected FontMetrics getFontMetrics()
  {
    Component container = getContainer();
    return container.getFontMetrics(container.getFont());
  }

  public float getPreferredSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException();

    FontMetrics fm = getFontMetrics();

    if (axis == Y_AXIS)
      return fm.getHeight();

    String text;
    Element elem = getElement();

    try
      {
	text = elem.getDocument().getText(elem.getStartOffset(),
					  elem.getEndOffset());
      }
    catch (BadLocationException e)
      {
	// This should never happen.
	text = "";
      }
    
    return fm.stringWidth(text);
  }

  public int getResizeWeight(int axis)
  {
    return axis = axis == X_AXIS ? 1 : 0;
  }
  
  public Shape modelToView(int pos, Shape a, Position.Bias bias)
    throws BadLocationException
  {
    return super.modelToView(pos, a, bias);
  }
  
  public void paint(Graphics g, Shape s)
  {
    super.paint(g, s);
  }
}
