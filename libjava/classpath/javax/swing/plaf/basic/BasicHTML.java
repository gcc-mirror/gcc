/* BasicHTML.java -- Provides HTML support to ComponentUI implementations
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.io.IOException;
import java.io.StringReader;

import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

/**
 * Provides support for HTML rendering to {@link javax.swing.plaf.ComponentUI}
 * implementations.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class BasicHTML
{

  /**
   * This class serves as the root view for HTML rendering components.
   * Its purpose and implementation is similar to the BasicTextUI.RootView
   * class, only that is implements some stuff differently due to the nature
   * of not beeing inside a JTextComponent.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private static class HTMLRootView extends View
  {
    /**
     * The real root view.
     */
    private View view;

    /**
     * The component on which to render the view.
     */
    private JComponent component;

    /**
     * The EditorKit.
     */
    private EditorKit editorKit;

    /**
     * The document to use.
     */
    private Document document;

    /**
     * Creates a new RootView.
     */
    public HTMLRootView(JComponent c, View view, EditorKit kit, Document doc)
    {
      super(null);
      component = c;
      editorKit = kit;
      document = doc;
      setView(view);
      setSize(view.getPreferredSpan(X_AXIS), view.getPreferredSpan(Y_AXIS));
    }

    /**
     * Returns the ViewFactory for this RootView. If the current EditorKit
     * provides a ViewFactory, this is used. Otherwise the TextUI itself
     * is returned as a ViewFactory.
     *
     * @return the ViewFactory for this RootView
     */
    public ViewFactory getViewFactory()
    {
      return editorKit.getViewFactory();
    }

    /**
     * Indicates that the preferences of one of the child view has changed.
     * This calls revalidate on the text component.
     *
     * @param v the child view which's preference has changed
     * @param width <code>true</code> if the width preference has changed
     * @param height <code>true</code> if the height preference has changed
     */
    public void preferenceChanged(View v, boolean width, boolean height)
    {
      component.revalidate();
    }

    /**
     * Sets the real root view.
     *
     * @param v the root view to set
     */
    public void setView(View v)
    {
      if (view != null)
        view.setParent(null);
      
      if (v != null)
        v.setParent(this);

      view = v;
    }

    /**
     * Overridden to forward to real view.
     */
    public void setSize(float w, float h)
    {
      view.setSize(w, h);
    }

    /**
     * Returns the real root view, regardless of the index.
     *
     * @param index not used here
     *
     * @return the real root view, regardless of the index.
     */
    public View getView(int index)
    {
      return view;
    }

    /**
     * Returns <code>1</code> since the RootView always contains one
     * child, that is the real root of the View hierarchy.
     *
     * @return <code>1</code> since the RootView always contains one
     *         child, that is the real root of the View hierarchy
     */
    public int getViewCount()
    {
      int count = 0;
      if (view != null)
        count = 1;
      return count;
    }

    /**
     * Returns the <code>Container</code> that contains this view. This
     * normally will be the text component that is managed by this TextUI.
     *
     * @return the <code>Container</code> that contains this view
     */
    public Container getContainer()
    {
      return component;
    }

    /**
     * Returns the preferred span along the specified <code>axis</code>.
     * This is delegated to the real root view.
     *
     * @param axis the axis for which the preferred span is queried
     *
     * @return the preferred span along the axis
     */
    public float getPreferredSpan(int axis)
    {
      if (view != null)
        return view.getPreferredSpan(axis);

      return Integer.MAX_VALUE;
    }

    /**
     * Paints the view. This is delegated to the real root view.
     *
     * @param g the <code>Graphics</code> context to paint to
     * @param s the allocation for the View
     */
    public void paint(Graphics g, Shape s)
    {
      if (view != null)
        {
          Rectangle b = s.getBounds();
          view.setSize(b.width, b.height);
          view.paint(g, s);
        }
    }


    /**
     * Maps a position in the document into the coordinate space of the View.
     * The output rectangle usually reflects the font height but has a width
     * of zero.
     *
     * This is delegated to the real root view.
     *
     * @param position the position of the character in the model
     * @param a the area that is occupied by the view
     * @param bias either {@link Position.Bias#Forward} or
     *        {@link Position.Bias#Backward} depending on the preferred
     *        direction bias. If <code>null</code> this defaults to
     *        <code>Position.Bias.Forward</code>
     *
     * @return a rectangle that gives the location of the document position
     *         inside the view coordinate space
     *
     * @throws BadLocationException if <code>pos</code> is invalid
     * @throws IllegalArgumentException if b is not one of the above listed
     *         valid values
     */
    public Shape modelToView(int position, Shape a, Position.Bias bias)
      throws BadLocationException
    {
      return view.modelToView(position, a, bias);
    }

    /**
     * Maps coordinates from the <code>View</code>'s space into a position
     * in the document model.
     *
     * @param x the x coordinate in the view space
     * @param y the y coordinate in the view space
     * @param a the allocation of this <code>View</code>
     * @param b the bias to use
     *
     * @return the position in the document that corresponds to the screen
     *         coordinates <code>x, y</code>
     */
    public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
    {
      return view.viewToModel(x, y, a, b);
    }

    /**
     * Notification about text insertions. These are forwarded to the
     * real root view.
     *
     * @param ev the DocumentEvent describing the change
     * @param shape the current allocation of the view's display
     * @param vf the ViewFactory to use for creating new Views
     */
    public void insertUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
    {
      view.insertUpdate(ev, shape, vf);
    }

    /**
     * Notification about text removals. These are forwarded to the
     * real root view.
     *
     * @param ev the DocumentEvent describing the change
     * @param shape the current allocation of the view's display
     * @param vf the ViewFactory to use for creating new Views
     */
    public void removeUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
    {
      view.removeUpdate(ev, shape, vf);
    }

    /**
     * Notification about text changes. These are forwarded to the
     * real root view.
     *
     * @param ev the DocumentEvent describing the change
     * @param shape the current allocation of the view's display
     * @param vf the ViewFactory to use for creating new Views
     */
    public void changedUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
    {
      view.changedUpdate(ev, shape, vf);
    }

    /**
     * Returns the document position that is (visually) nearest to the given
     * document position <code>pos</code> in the given direction <code>d</code>.
     *
     * @param pos the document position
     * @param b the bias for <code>pos</code>
     * @param a the allocation for the view
     * @param d the direction, must be either {@link SwingConstants#NORTH},
     *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
     *        {@link SwingConstants#EAST}
     * @param biasRet an array of {@link Position.Bias} that can hold at least
     *        one element, which is filled with the bias of the return position
     *        on method exit
     *
     * @return the document position that is (visually) nearest to the given
     *         document position <code>pos</code> in the given direction
     *         <code>d</code>
     *
     * @throws BadLocationException if <code>pos</code> is not a valid offset in
     *         the document model
     */
    public int getNextVisualPositionFrom(int pos, Position.Bias b, Shape a,
                                         int d, Position.Bias[] biasRet)
      throws BadLocationException
    {
      return view.getNextVisualPositionFrom(pos, b, a, d, biasRet);
    }

    public int getStartOffset()
    {
      return 0;
    }

    public int getEndOffset()
    {
      return getDocument().getLength();
    }

    public Document getDocument()
    {
      return document;
    }

    /**
     * Overridden to return null, as a RootView has no attributes on its own.
     */
    public AttributeSet getAttributes()
    {
      return null;
    }

    /**
     * Overridden to provide an element for the view.
     */
    public Element getElement()
    {
      return view.getElement();
    }
  }

  /**
   * The key that is used to store a HTML view in a JComponent's client
   * properties.
   */
  public static final String propertyKey = "html";

  /**
   * The key that is used to store the document base in a JComponent's client
   * properties. The document base is used to resolve relative references
   * in HTML.
   */
  public static final String documentBaseKey = "html.base";

  /**
   * Creates a new instance of BasicHTML. This should not be necessary since
   * all methods in this class are static.
   */
  public BasicHTML()
  {
    // Nothing to do here.
  }

  /**
   * Creates a {@link View} instance that can be used by the component
   * <code>c</code> to render the HTML string <code>html</code>.
   *
   * @param c the component that needs to render the HTML string
   * @param html the HTML string to be rendered
   *
   * @return a view that can render the HTML string
   */
  public static View createHTMLView(JComponent c, String html)
  {
    // TODO: This might be wrong. Lets see if it turns out good when
    // the javax.swing.text.html package is in a good shape.
    HTMLDocument doc = new HTMLDocument();
    HTMLEditorKit kit = new HTMLEditorKit();
    StringReader reader = new StringReader(html);
    try
      {
        kit.read(reader, doc, 0);
      }
    catch (IOException ex)
      {
        AssertionError err = new AssertionError("unexpected IOException");
        err.initCause(ex);
        throw err;
      }
    catch (BadLocationException ex)
      {
        AssertionError err =
          new AssertionError("unexpected BadLocationException");
        err.initCause(ex);
        throw err;
      }
    ViewFactory vf = kit.getViewFactory();
    Element root = doc.getDefaultRootElement();
    View view = vf.create(root);
    HTMLRootView rootView = new HTMLRootView(c, view, kit, doc);
    return rootView;
  }

  /**
   * Returns <code>true</code> if <code>s</code> is HTML, <code>false</code>
   * otherwise.
   *
   * @param s the string to test
   *
   * @return <code>true</code> if <code>s</code> is HTML, <code>false</code>
   *         otherwise
   */
  public static boolean isHTMLString(String s)
  {
    // We consider a string to be HTML if it contains both the '<' and '>'
    // character at least once.
    return (s != null) && s.contains("<") && s.contains(">");
  }

  /**
   * Stores a HTML renderer in <code>c</code>'s client property if
   * <code>text</code> is HTML, otherwise it clears the corresponding client
   * property. This is useful for {@link javax.swing.plaf.ComponentUI}
   * implementations that are shared between it's components.
   *
   * @param c the component to update the renderer for
   * @param text the string to be rendered
   */
  public static void updateRenderer(JComponent c, String text)
  {
    if (isHTMLString(text))
      c.putClientProperty(propertyKey, createHTMLView(c, text));
    else
      c.putClientProperty(propertyKey, null);
  }
}
