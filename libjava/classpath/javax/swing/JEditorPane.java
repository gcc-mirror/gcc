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
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleHyperlink;
import javax.accessibility.AccessibleHypertext;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.WrappedPlainView;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

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
  /**
   * Provides accessibility support for <code>JEditorPane</code>.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  protected class AccessibleJEditorPane extends AccessibleJTextComponent
  {

    /**
     * Creates a new <code>AccessibleJEditorPane</code> object.
     */
    protected AccessibleJEditorPane()
    {
      super();
    }

    /**
     * Returns a description of this <code>AccessibleJEditorPane</code>. If
     * this property is not set, then this returns the content-type of the
     * editor pane.
     *
     * @return a description of this AccessibleJEditorPane
     */
    public String getAccessibleDescription()
    {
      String descr = super.getAccessibleDescription(); 
      if (descr == null)
        return getContentType();
      else
        return descr;
    }

    /**
     * Returns the accessible state of this <code>AccessibleJEditorPane</code>.
     *
     * @return  the accessible state of this <code>AccessibleJEditorPane</code>
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet state = super.getAccessibleStateSet();
      // TODO: Figure out what state must be added here to the super's state.
      return state;
    }
  }

  /**
   * Provides accessibility support for <code>JEditorPane</code>s, when the
   * editor kit is an instance of {@link HTMLEditorKit}.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  protected class AccessibleJEditorPaneHTML extends AccessibleJEditorPane
  {
    /**
     * Returns the accessible text of the <code>JEditorPane</code>. This will
     * be an instance of
     * {@link JEditorPaneAccessibleHypertextSupport}.
     *
     * @return the accessible text of the <code>JEditorPane</code>
     */
    public AccessibleText getAccessibleText()
    {
      return new JEditorPaneAccessibleHypertextSupport();
    }
  }

  /**
   * This is the accessible text that is returned by
   * {@link AccessibleJEditorPaneHTML#getAccessibleText()}.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  protected class JEditorPaneAccessibleHypertextSupport
    extends AccessibleJEditorPane implements AccessibleHypertext
  {

    /**
     * The accessible representation of a HTML link. 
     *
     * @author Roman Kennke (kennke@aicas.com)
     */
    public class HTMLLink extends AccessibleHyperlink
    {

      /**
       * The element in the document that represents the link.
       */
      Element element;

      /**
       * Creates a new <code>HTMLLink</code>.
       *
       * @param el the link element
       */
      public HTMLLink(Element el)
      {
        this.element = el;
      }

      /**
       * Returns <code>true</code> if this <code>HTMLLink</code> is still
       * valid. A <code>HTMLLink</code> can become invalid when the document
       * changes.
       *
       * @return <code>true</code> if this <code>HTMLLink</code> is still
       *         valid
       */
      public boolean isValid()
      {
        // I test here if the element at our element's start offset is the
        // same as the element in the document at this offset. If this is true,
        // I consider the link valid, if not, then this link no longer
        // represented by this HTMLLink and therefor invalid.
        HTMLDocument doc = (HTMLDocument) getDocument();
        return doc.getCharacterElement(element.getStartOffset()) == element;
      }

      /**
       * Returns the number of AccessibleActions in this link object. In
       * general, link have 1 AccessibleAction associated with them. There are
       * special cases where links can have multiple actions associated, like
       * in image maps.
       * 
       * @return the number of AccessibleActions in this link object
       */
      public int getAccessibleActionCount()
      {
        // TODO: Implement the special cases.
        return 1;
      }

      /**
       * Performs the specified action on the link object. This ususally means
       * activating the link.
       *
       * @return <code>true</code> if the action has been performed
       *         successfully, <code>false</code> otherwise
       */
      public boolean doAccessibleAction(int i)
      {
        String href = (String) element.getAttributes().getAttribute("href");
        HTMLDocument doc = (HTMLDocument) getDocument();
        try
          {
            URL url = new URL(doc.getBase(), href);
            setPage(url);
            String desc = doc.getText(element.getStartOffset(),
                            element.getEndOffset() - element.getStartOffset());
            HyperlinkEvent ev =
              new HyperlinkEvent(JEditorPane.this,
                                 HyperlinkEvent.EventType.ACTIVATED, url, desc,
                                 element);
            fireHyperlinkUpdate(ev);
            return true;
          }
        catch (Exception ex)
          {
            return false;
          }
      }

      /**
       * Returns the description of the action at action index <code>i</code>.
       * This method returns the text within the element associated with this
       * link.
       *
       * @param i the action index
       *
       * @return the description of the action at action index <code>i</code>
       */
      public String getAccessibleActionDescription(int i)
      {
        HTMLDocument doc = (HTMLDocument) getDocument();
        try
          {
            return doc.getText(element.getStartOffset(),
                            element.getEndOffset() - element.getStartOffset());
          }
        catch (BadLocationException ex)
          {
            throw (AssertionError)
            new AssertionError("BadLocationException must not be thrown "
                               + "here.")
              .initCause(ex);
          }
      }

      /**
       * Returns an {@link URL} object, that represents the action at action
       * index <code>i</code>.
       *
       * @param i the action index
       *
       * @return an {@link URL} object, that represents the action at action
       *         index <code>i</code>
       */
      public Object getAccessibleActionObject(int i)
      {
        String href = (String) element.getAttributes().getAttribute("href");
        HTMLDocument doc = (HTMLDocument) getDocument();
        try
          {
            URL url = new URL(doc.getBase(), href);
            return url;
          }
        catch (MalformedURLException ex)
          {
            return null;
          }
      }

      /**
       * Returns an object that represents the link anchor. For examples, if
       * the link encloses a string, then a <code>String</code> object is
       * returned, if the link encloses an &lt;img&gt; tag, then an
       * <code>ImageIcon</code> object is returned.
       *
       * @return an object that represents the link anchor
       */
      public Object getAccessibleActionAnchor(int i)
      {
        // TODO: This is only the String case. Implement all cases.
        return getAccessibleActionDescription(i);
      }

      /**
       * Returns the start index of the hyperlink element.
       *
       * @return the start index of the hyperlink element
       */
      public int getStartIndex()
      {
        return element.getStartOffset();
      }

      /**
       * Returns the end index of the hyperlink element.
       *
       * @return the end index of the hyperlink element
       */
      public int getEndIndex()
      {
        return element.getEndOffset();
      }
      
    }

    /**
     * Returns the number of hyperlinks in the document.
     *
     * @return the number of hyperlinks in the document
     */
    public int getLinkCount()
    {
      HTMLDocument doc = (HTMLDocument) getDocument();
      HTMLDocument.Iterator linkIter = doc.getIterator(HTML.Tag.A);
      int count = 0;
      while (linkIter.isValid())
        {
          count++;
          linkIter.next();
        }
      return count;
    }

    /**
     * Returns the <code>i</code>-th hyperlink in the document or
     * <code>null</code> if there is no hyperlink with the specified index.
     *
     * @param i the index of the hyperlink to return
     *
     * @return the <code>i</code>-th hyperlink in the document or
     *         <code>null</code> if there is no hyperlink with the specified
     *         index
     */
    public AccessibleHyperlink getLink(int i)
    {
      HTMLDocument doc = (HTMLDocument) getDocument();
      HTMLDocument.Iterator linkIter = doc.getIterator(HTML.Tag.A);
      int count = 0;
      while (linkIter.isValid())
        {
          count++;
          if (count == i)
            break;
          linkIter.next();
        }
      if (linkIter.isValid())
        {
          int offset = linkIter.getStartOffset();
          // TODO: I fetch the element for the link via getCharacterElement().
          // I am not sure that this is correct, maybe we must use
          // getParagraphElement()?
          Element el = doc.getCharacterElement(offset);
          HTMLLink link = new HTMLLink(el);
          return link;
        }
      else
        return null;
    }

    /**
     * Returns the index of the link element at the character position
     * <code>c</code> within the document, or <code>-1</code> if there is no
     * link at the specified position.
     *
     * @param c the character index from which to fetch the link index
     *
     * @return the index of the link element at the character position
     *         <code>c</code> within the document, or <code>-1</code> if there
     *         is no link at the specified position
     */
    public int getLinkIndex(int c)
    {
      HTMLDocument doc = (HTMLDocument) getDocument();
      HTMLDocument.Iterator linkIter = doc.getIterator(HTML.Tag.A);
      int count = 0;
      while (linkIter.isValid())
        {
          if (linkIter.getStartOffset() <= c && linkIter.getEndOffset() > c)
            break;
          count++;
          linkIter.next();
        }
      if (linkIter.isValid())
        return count;
      else
        return -1;
    }

    /**
     * Returns the link text of the link at index <code>i</code>, or
     * <code>null</code>, if there is no link at the specified position.
     *
     * @param i the index of the link
     *
     * @return  the link text of the link at index <code>i</code>, or
     *          <code>null</code>, if there is no link at the specified
     *          position
     */
    public String getLinkText(int i)
    {
      HTMLDocument doc = (HTMLDocument) getDocument();
      HTMLDocument.Iterator linkIter = doc.getIterator(HTML.Tag.A);
      int count = 0;
      while (linkIter.isValid())
        {
          count++;
          if (count == i)
            break;
          linkIter.next();
        }
      if (linkIter.isValid())
        {
          int offset = linkIter.getStartOffset();
          // TODO: I fetch the element for the link via getCharacterElement().
          // I am not sure that this is correct, maybe we must use
          // getParagraphElement()?
          Element el = doc.getCharacterElement(offset);
          try
            {
              String text = doc.getText(el.getStartOffset(),
                                      el.getEndOffset() - el.getStartOffset());
              return text;
            }
          catch (BadLocationException ex)
            {
              throw (AssertionError)
                new AssertionError("BadLocationException must not be thrown "
                                   + "here.")
                  .initCause(ex);
            }
        }
      else
        return null;
    }
  }

  /**
   * An EditorKit used for plain text. This is the default editor kit for
   * JEditorPanes.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private static class PlainEditorKit extends DefaultEditorKit
  {

    /**
     * Returns a ViewFactory that supplies WrappedPlainViews.
     */
    public ViewFactory getViewFactory()
    {
      return new ViewFactory()
      {
        public View create(Element el)
        {
          return new WrappedPlainView(el);
        }
      };
    }
  }

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
    return new PlainEditorKit();
  }

  public static EditorKit createEditorKitForContentType(String type)
  {
    return new PlainEditorKit();
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

  /**
   * Returns the accessible context associated with this editor pane.
   *
   * @return the accessible context associated with this editor pane
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      {
        if (getEditorKit() instanceof HTMLEditorKit)
          accessibleContext = new AccessibleJEditorPaneHTML();
        else
          accessibleContext = new AccessibleJEditorPane();
      }
    return accessibleContext;
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
  /*  Container parent = getParent();
    return (parent instanceof JViewport &&
        parent.isValid());*/
    return isValid();
  }

  public boolean getScrollableTracksViewportWidth()
  {
    /*Container parent = getParent();
    return (parent instanceof JViewport &&
        parent.isValid());*/
    return isValid();
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
  public void read(InputStream in, Object desc) throws IOException
  {
    EditorKit kit = getEditorKit();
    if (kit instanceof HTMLEditorKit && desc instanceof HTMLDocument)
      {
        Document doc = (Document) desc;
        try
          {
            kit.read(in, doc, 0);
          }
        catch (BadLocationException ex)
          {
            assert false : "BadLocationException must not be thrown here.";
          }
      }
    else
      {
        Reader inRead = new InputStreamReader(in);
        super.read(inRead, desc);
      }
  }

  /**
   * Establishes the default bindings of type to classname. 
   */
  public static void registerEditorKitForContentType(String type,
                                                     String classname)
  {
    // TODO: Implement this properly.
  }

  /**
   * Establishes the default bindings of type to classname.
   */
  public static void registerEditorKitForContentType(String type,
                                                     String classname,
                                                     ClassLoader loader)
  {
    // TODO: Implement this properly.
  }

  /**
   * Replaces the currently selected content with new content represented
   * by the given string.
   */
  public void replaceSelection(String content)
  {
    // TODO: Implement this properly.
  }

  /**
   * Scrolls the view to the given reference location (that is, the value
   * returned by the UL.getRef method for the URL being displayed).
   */
  public void scrollToReference(String reference)
  {
    // TODO: Implement this properly.
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
    // Reset the accessibleContext since this depends on the editorKit.
    accessibleContext = null;
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
