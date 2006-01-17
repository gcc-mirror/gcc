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

import java.io.IOException;
import java.io.StringReader;

import javax.swing.JComponent;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
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
    return view;
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
    return s.contains("<") && s.contains(">");
  }

  /**
   * Stores a HTML renderer in <code>c</code>'s client property if
   * <code>text</code> is HTML, otherwise it clears the corresponding client
   * property. This is useful for {@link java.swing.plaf.ComponentUI}
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
