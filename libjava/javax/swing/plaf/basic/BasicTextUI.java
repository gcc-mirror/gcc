/* BasicTextUI.java
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TextUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;


public abstract class BasicTextUI extends TextUI
  implements ViewFactory
{
  public static class BasicCaret extends DefaultCaret
    implements UIResource
  {
    public BasicCaret()
    {
    }
  }

  public static class BasicHighlighter extends DefaultHighlighter
    implements UIResource
  {
    public BasicHighlighter()
    {
    }
  }

  private class RootView extends View
  {
    private JTextComponent textComponent;
    private View view;
    
    public RootView(JTextComponent parent)
      {
          super(null);
      textComponent = parent;
      }

    public void setView(View v)
      {
          if (view != null)
	view.setParent(null);
      
      if (v != null)
	v.setParent(null);

      view = v;
    }

    public Container getContainer()
              {
      return textComponent;
    }

    public float getPreferredSpan(int axis)
    {
      if (view != null)
	return view.getPreferredSpan(axis);

      return Integer.MAX_VALUE;
              }

    public void paint(Graphics g, Shape s)
    {
      System.out.println("Michael: BasicTextUI.RootView.paint");
      
      if (view != null)
	view.paint(g, s);
      }
  }
  
  RootView rootView;
  JTextComponent textComponent;
  int gap = 3;
  EditorKit kit = new DefaultEditorKit();

  public BasicTextUI()
  {
  }

  protected Caret createCaret()
  {
    return new BasicCaret();
  }

  protected Highlighter createHighlighter()
  {
    return new BasicHighlighter();
  }
  
  protected final JTextComponent getComponent()
  {
    return textComponent;
  }

  public void installUI(final JComponent c)
  {
    super.installUI(c);
    c.setOpaque(true);

    textComponent = (JTextComponent) c;

    Document doc = textComponent.getDocument();
    if (doc == null)
      {
	doc = new PlainDocument();
	textComponent.setDocument(doc);
  }

    rootView = new RootView(textComponent);
    setView(create(doc.getDefaultRootElement()));
    
    installDefaults();
    installListeners();
    installKeyboardActions();
  }

  protected void installDefaults()
  {
  }

  protected void installListeners()
  {
  }

  protected void installKeyboardActions()
  {
  }
  
  public void uninstallUI(final JComponent c)
  {
    super.uninstallUI(c);
    rootView = null;

    uninstallDefaults();
    uninstallListeners();
    uninstallKeyboardActions();
  }

  protected void uninstallDefaults()
  {
  }

  protected void uninstallListeners()
  {
  }

  protected void uninstallKeyboardActions()
  {
  }
  
  protected abstract String getPropertyPrefix();

  public Dimension getPreferredSize(JComponent c)
  {
    View v = getRootView(textComponent);

    float w = v.getPreferredSpan(View.X_AXIS);
    float h = v.getPreferredSpan(View.Y_AXIS);

    return new Dimension((int) w, (int) h);
  }

  public final void paint(Graphics g, JComponent c)
  {
    paintSafely(g);
  }

  protected void paintSafely(Graphics g)
  {
    Caret caret = textComponent.getCaret();
    Highlighter highlighter = textComponent.getHighlighter();
    
    if (textComponent.isOpaque())
      paintBackground(g);
    
    rootView.paint(g, getVisibleEditorRect());

    if (highlighter != null)
      highlighter.paint(g);

    if (caret != null)
      caret.paint(g);
  }

  protected void paintBackground(Graphics g)
  {
    g.setColor(Color.WHITE); // FIXME: set background color
    g.fillRect(0, 0, textComponent.getWidth(), textComponent.getHeight());
  }

  public void damageRange(JTextComponent t, int p0, int p1)
  {
    damageRange(t, p0, p1, null, null);
  }

  public void damageRange(JTextComponent t, int p0, int p1,
                          Position.Bias firstBias, Position.Bias secondBias)
  {
  }

  public EditorKit getEditorKit(JTextComponent t)
  {
    return kit;
  }

  public int getNextVisualPositionFrom(JTextComponent t, int pos,
                                       Position.Bias b, int direction,
                                       Position.Bias[] biasRet)
    throws BadLocationException
  {
    return 0;
  }

  public View getRootView(JTextComponent t)
  {
    return rootView;
  }

  public Rectangle modelToView(JTextComponent t, int pos)
    throws BadLocationException
  {
    return modelToView(t, pos, null);
  }

  public Rectangle modelToView(JTextComponent t, int pos, Position.Bias bias)
    throws BadLocationException
  {
    return null;
  }

  public int viewToModel(JTextComponent t, Point pt)
  {
    return viewToModel(t, pt, null);
  }

  public int viewToModel(JTextComponent t, Point pt, Position.Bias[] biasReturn)
  {
    return 0;
  }

  public View create(Element elem)
  {
    // subclasses have to implement this to get this functionality
    return null;
  }

  public View create(Element elem, int p0, int p1)
  {
    // subclasses have to implement this to get this functionality
    return null;
  }
  
  protected Rectangle getVisibleEditorRect()
  {
    int width = textComponent.getWidth();
    int height = textComponent.getHeight();

    if (width <= 0 || height <= 0)
      return null;
	
    Insets insets = textComponent.getInsets();
    return new Rectangle(insets.left, insets.top,
			 width - insets.left + insets.right,
			 height - insets.top + insets.bottom);
  }

  protected final void setView(View view)
  {
    rootView.setView(view);
    view.setParent(rootView);
  }
}
