/* JEditorPane.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;

import javax.accessibility.AccessibleContext;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;


public class JEditorPane extends JTextComponent
{
  private static final long serialVersionUID = 3140472492599046285L;
  
  URL page_url;
  EditorKit kit;
  String ctype = "text/plain";
  boolean focus_root;
  boolean manages_focus;

  public JEditorPane()
  {
  }

  public JEditorPane(String url) throws IOException
  {
    setPage(url);
  }

  public JEditorPane(String type, String text)
  {
    ctype = text;
    setText(text);
  }

  public JEditorPane(URL url) throws IOException
  {
    setPage(url);
  }

  protected EditorKit createDefaultEditorKit()
  {
    return new DefaultEditorKit();
  }

  public static EditorKit createEditorKitForContentType(String type)
  {
    return new DefaultEditorKit();
  }

  /**
   * Sends a given <code>HyperlinkEvent</code> to all registered listeners.
   *
   * @param event the event to send
   */
  public void fireHyperlinkUpdate(HyperlinkEvent event)
  {
    HyperlinkListener[] listeners = getHyperlinkListeners();

    for (int index = 0; index < listeners.length; ++index)
       listeners[index].hyperlinkUpdate(event);
  }

  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  public String getContentType()
  {
    return ctype;
  }

  public EditorKit getEditorKit()
  {
    return kit;
  }

  public static String getEditorKitClassNameForContentType(String type)
  {
    return "text/plain";
  }

  public EditorKit getEditorKitForContentType(String type)
  {
    return kit;
  }

  /**
   * Returns the preferred size for the JEditorPane.  
   */
  public Dimension getPreferredSize()
  {
    return super.getPreferredSize();
  }

  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  public URL getPage()
  {
    return page_url;
  }

  protected InputStream getStream(URL page)
  {
    try
      {
	return page.openStream();
      }
    catch (Exception e)
      {
	System.out.println("Hhmmm, failed to open stream: " + e);
      }
    return null;
  }

  public String getText()
  {
    return super.getText();
  }

  public String getUIClassID()
  {
    return "EditorPaneUI";
  }

  public boolean isFocusCycleRoot()
  {
    return focus_root;
  }

  public boolean isManagingFocus()
  {
    return manages_focus;
  }

  protected String paramString()
  {
    return "JEditorPane";
  }

  /**
   * Overridden to handle processing of tab/shift tab. 
   */
  protected void processComponentKeyEvent(KeyEvent e)
  {
  }

  /**
   * Make sure that TAB and Shift-TAB events get consumed,
   * so that awt doesn't attempt focus traversal.  
   */
  protected void processKeyEvent(KeyEvent e)
  {
  }

  /**
   * This method initializes from a stream. 
   */
  public void read(InputStream in, Object desc)
  {
  }

  /**
   * Establishes the default bindings of type to classname. 
   */
  public static void registerEditorKitForContentType(String type,
                                                     String classname)
  {
  }

  /**
   * Establishes the default bindings of type to classname.  
   */
  public static void registerEditorKitForContentType(String type,
                                                     String classname,
                                                     ClassLoader loader)
  {
  }

  /**
   * Replaces the currently selected content with new content represented
   * by the given string.
   */
  public void replaceSelection(String content)
  {
  }

  /**
   * Scrolls the view to the given reference location (that is, the value
   * returned by the UL.getRef method for the URL being displayed).
   */
  public void scrollToReference(String reference)
  {
  }

  public void setContentType(String type)
  {
    ctype = type;
    invalidate();
    repaint();
  }

  public void setEditorKit(EditorKit kit)
  {
    this.kit = kit;
    invalidate();
    repaint();
  }

  public void setEditorKitForContentType(String type, EditorKit k)
  {
    ctype = type;
    setEditorKit(k);
  }

  /**
   * Sets the current URL being displayed.  
   */
  public void setPage(String url) throws IOException
  {
  }

  /**
   * Sets the current URL being displayed.  
   */
  public void setPage(URL page) throws IOException
  {
  }

  public void setText(String t)
  {
    super.setText(t);
  }

  /**
   * Add a <code>HyperlinkListener</code> object to this editor pane.
   *
   * @param listener the listener to add
   */
  public void addHyperlinkListener(HyperlinkListener listener)
  {
    listenerList.add(HyperlinkListener.class, listener);
  }

  /**
   * Removes a <code>HyperlinkListener</code> object to this editor pane.
   *
   * @param listener the listener to remove
   */
  public void removeHyperlinkListener(HyperlinkListener listener)
  {
    listenerList.remove(HyperlinkListener.class, listener);
  }

  /**
   * Returns all added <code>HyperlinkListener</code> objects.
   *
   * @return array of listeners
   *
   * @since 1.4
   */
  public HyperlinkListener[] getHyperlinkListeners()
  {
    return (HyperlinkListener[]) getListeners(HyperlinkListener.class);
  }
}
