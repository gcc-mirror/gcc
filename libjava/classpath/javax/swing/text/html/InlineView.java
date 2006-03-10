/* InlineView.java -- Renders HTML content
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


package javax.swing.text.html;

import java.awt.Shape;

import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.LabelView;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * Renders HTML content (identified by {@link HTML.Tag#CONTENT}). This is
 * basically a {@link LabelView} that is adjusted to understand styles defined
 * by stylesheets.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class InlineView
  extends LabelView
{

  /**
   * Creates a new <code>InlineView</code> that renders the specified element.
   *
   * @param element the element for this view
   */
  public InlineView(Element element)
  {
    super(element);
  }

  /**
   * Receives notification that something was inserted into the document in
   * a location that this view is responsible for.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void insertUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    // FIXME: What to do here?
    super.insertUpdate(e, a, f);
  }

  /**
   * Receives notification that something was removed from the document in
   * a location that this view is responsible for.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void removeUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    // FIXME: What to do here?
    super.removeUpdate(e, a, f);
  }

  /**
   * Receives notification that attributes have changed in the document in
   * a location that this view is responsible for. This calls
   * {@link #setPropertiesFromAttributes}.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void changedUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    super.changedUpdate(e, a, f);
    setPropertiesFromAttributes();
  }

  /**
   * Returns the attributes that are used for rendering. This is implemented
   * to multiplex the attributes specified in the model with a stylesheet.
   *
   * @return the attributes that are used for rendering
   */
  public AttributeSet getAttributes()
  {
    // FIXME: Implement this.
    return super.getAttributes();
  }

  
  public int getBreakWeight(int axis, float pos, float len)
  {
    // FIXME: Implement this.
    return super.getBreakWeight(axis, pos, len);
  }

  public View breakView(int axis, int offset, float pos, float len)
  {
    // FIXME: Implement this.
    return super.breakView(axis, offset, pos, len);
  }

  protected void setPropertiesFromAttributes()
  {
    // FIXME: Implement this.
    super.setPropertiesFromAttributes();
  }

  /**
   * Returns the stylesheet used by this view. This returns the stylesheet
   * of the <code>HTMLDocument</code> that is rendered by this view.
   *
   * @return the stylesheet used by this view
   */
  protected StyleSheet getStyleSheet()
  {
    Document doc = getDocument();
    StyleSheet styleSheet = null;
    if (doc instanceof HTMLDocument)
      styleSheet = ((HTMLDocument) doc).getStyleSheet();
    return styleSheet;
  }
}
