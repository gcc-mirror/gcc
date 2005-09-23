/* JEditorPane.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Dimension;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.accessibility.AccessibleContext;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;

/**
 * A powerful text editor component that can handle different types of
 * content.
 *
 * The JEditorPane text component is driven by an instance of
 * {@link EditorKit}. The editor kit is responsible for providing
 * a default {@link Document} implementation, a mechanism for loading
 * and saving documents of its supported content type and providing
 * a set of {@link Action}s for manipulating the content.
 *
 * By default the following content types are supported:
 * <ul>
 * <li><code>text/plain</code>: Plain text, handled by
 *   {@link javax.swing.text.DefaultEditorKit}.</li>
 * <li><code>text/html</code>: HTML 4.0 styled text, handled by
 *   {@link javax.swing.text.html.HTMLEditorKit}.</li>
 * <li><code>text/rtf</code>: RTF text, handled by
 *   {@link javax.swing.text.rtf.RTFEditorKit}.</li>
 * </ul>
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public class JEditorPane extends JTextComponent
{
  private static final long serialVersionUID = 3140472492599046285L;
  
  private URL page;
  private EditorKit editorKit;
  
  boolean focus_root;

  public JEditorPane()
  {
    setEditorKit(createDefaultEditorKit());
  }

  public JEditorPane(String url) throws IOException
  {
    this(new URL(url));
  }

  public JEditorPane(String type, String text)
  {
    setEditorKit(createEditorKitForContentType(type));
    setText(text);
  }

  public JEditorPane(URL url) throws IOException
  {
    this();
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

  public final String getContentType()
  {
    return getEditorKit().getContentType();
  }

  /**
   * Returns the EditorKit. If there is no EditorKit set this method
   * calls createDefaultEditorKit() and setEditorKit() first.
   */
  public EditorKit getEditorKit()
  {
    if (editorKit == null)
      setEditorKit(createDefaultEditorKit());
    return editorKit;
  }

  public static String getEditorKitClassNameForContentType(String type)
  {
    return "text/plain";
  }

  public EditorKit getEditorKitForContentType(String type)
  {
    return editorKit;
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
    return page;
  }

  protected InputStream getStream(URL page)
    throws IOException
  {
    return page.openStream();
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

  protected String paramString()
  {
    return "JEditorPane";
  }

  /**
   * This method initializes from a stream. 
   */
  public void read(InputStream in, Object desc)
    throws IOException
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

  public final void setContentType(String type)
  {
    if (editorKit != null
	&& editorKit.getContentType().equals(type))
      return;
    	      
    EditorKit kit = getEditorKitForContentType(type);
	    	
    if (kit != null)
      setEditorKit(kit);
  }

  public void setEditorKit(EditorKit newValue)
  {
    if (editorKit == newValue)
      return;
    	
    if (editorKit != null)
      editorKit.deinstall(this);
	    	    
    EditorKit oldValue = editorKit;
    editorKit = newValue;
			    	
    if (editorKit != null)
      {
	editorKit.install(this);
	setDocument(editorKit.createDefaultDocument());
      }
				    	    
    firePropertyChange("editorKit", oldValue, newValue);
    invalidate();
    repaint();
  }

  public void setEditorKitForContentType(String type, EditorKit k)
  {
    // FIXME: editorKitCache.put(type, kit);
  }

  /**
   * Sets the current URL being displayed.  
   */
  public void setPage(String url) throws IOException
  {
    setPage(new URL(url));
  }

  /**
   * Sets the current URL being displayed.  
   */
  public void setPage(URL page) throws IOException
  {
    if (page == null)
      throw new IOException("invalid url");

    try
      {
	this.page = page;
	getEditorKit().read(page.openStream(), getDocument(), 0);
      }
    catch (BadLocationException e)
      {
	// Ignored. '0' is always a valid offset.
      }
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
