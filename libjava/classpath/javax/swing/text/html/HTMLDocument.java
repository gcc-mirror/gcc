/* HTMLDocument.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import gnu.classpath.NotImplementedException;

import java.io.IOException;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.DefaultButtonModel;
import javax.swing.JEditorPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.ElementIterator;
import javax.swing.text.GapContent;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.PlainDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTML.Tag;

/**
 * Represents the HTML document that is constructed by defining the text and
 * other components (images, buttons, etc) in HTML language. This class can
 * becomes the default document for {@link JEditorPane} after setting its
 * content type to "text/html". HTML document also serves as an intermediate
 * data structure when it is needed to parse HTML and then obtain the content of
 * the certain types of tags. This class also has methods for modifying the HTML
 * content.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 * @author Anthony Balkissoon (abalkiss@redhat.com)
 * @author Lillian Angel (langel@redhat.com)
 */
public class HTMLDocument extends DefaultStyledDocument
{
  /** A key for document properies.  The value for the key is
   * a Vector of Strings of comments not found in the body.
   */  
  public static final String AdditionalComments = "AdditionalComments";
  URL baseURL = null;
  boolean preservesUnknownTags = true;
  int tokenThreshold = Integer.MAX_VALUE;
  HTMLEditorKit.Parser parser;

  /**
   * Indicates whether this document is inside a frame or not.
   */
  private boolean frameDocument;

  /**
   * Package private to avoid accessor methods.
   */
  String baseTarget;

  /**
   * Constructs an HTML document using the default buffer size and a default
   * StyleSheet.
   */
  public HTMLDocument()
  {
    this(new GapContent(BUFFER_SIZE_DEFAULT), new StyleSheet());
  }
  
  /**
   * Constructs an HTML document with the default content storage 
   * implementation and the specified style/attribute storage mechanism.
   * 
   * @param styles - the style sheet
   */
  public HTMLDocument(StyleSheet styles)
  {
   this(new GapContent(BUFFER_SIZE_DEFAULT), styles);
  }
  
  /**
   * Constructs an HTML document with the given content storage implementation 
   * and the given style/attribute storage mechanism.
   * 
   * @param c - the document's content
   * @param styles - the style sheet
   */
  public HTMLDocument(AbstractDocument.Content c, StyleSheet styles)
  {
    super(c, styles);
  }
  
  /**
   * Gets the style sheet with the document display rules (CSS) that were specified 
   * in the HTML document.
   * 
   * @return - the style sheet
   */
  public StyleSheet getStyleSheet()
  {
    return (StyleSheet) getAttributeContext();
  }
  
  /**
   * This method creates a root element for the new document.
   * 
   * @return the new default root
   */
  protected AbstractElement createDefaultRoot()
  {
    AbstractDocument.AttributeContext ctx = getAttributeContext();

    // Create html element.
    AttributeSet atts = ctx.getEmptySet();
    atts = ctx.addAttribute(atts, StyleConstants.NameAttribute, HTML.Tag.HTML);
    BranchElement html = (BranchElement) createBranchElement(null, atts);

    // Create body element.
    atts = ctx.getEmptySet();
    atts = ctx.addAttribute(atts, StyleConstants.NameAttribute, HTML.Tag.BODY);
    BranchElement body = (BranchElement) createBranchElement(html, atts);
    html.replace(0, 0, new Element[] { body });

    // Create p element.
    atts = ctx.getEmptySet();
    atts = ctx.addAttribute(atts, StyleConstants.NameAttribute, HTML.Tag.P);
    BranchElement p = (BranchElement) createBranchElement(body, atts);
    body.replace(0, 0, new Element[] { p });

    // Create an empty leaf element.
    atts = ctx.getEmptySet();
    atts = ctx.addAttribute(atts, StyleConstants.NameAttribute,
                            HTML.Tag.CONTENT);
    Element leaf = createLeafElement(p, atts, 0, 1);
    p.replace(0, 0, new Element[]{ leaf });

    return html;
  }
  
  /**
   * This method returns an HTMLDocument.RunElement object attached to
   * parent representing a run of text from p0 to p1. The run has 
   * attributes described by a.
   * 
   * @param parent - the parent element
   * @param a - the attributes for the element
   * @param p0 - the beginning of the range >= 0
   * @param p1 - the end of the range >= p0
   *
   * @return the new element
   */
  protected Element createLeafElement(Element parent, AttributeSet a, int p0,
                                      int p1)
  {
    return new RunElement(parent, a, p0, p1);
  }

  /**
   * This method returns an HTMLDocument.BlockElement object representing the
   * attribute set a and attached to parent.
   * 
   * @param parent - the parent element
   * @param a - the attributes for the element
   *
   * @return the new element
   */
  protected Element createBranchElement(Element parent, AttributeSet a)
  {
    return new BlockElement(parent, a);
  }
  
  /**
   * Returns the parser used by this HTMLDocument to insert HTML.
   * 
   * @return the parser used by this HTMLDocument to insert HTML.
   */
  public HTMLEditorKit.Parser getParser()
  {
    return parser; 
  }
  
  /**
   * Sets the parser used by this HTMLDocument to insert HTML.
   * 
   * @param p the parser to use
   */
  public void setParser (HTMLEditorKit.Parser p)
  {
    parser = p;
  }
  /**
   * Sets the number of tokens to buffer before trying to display the
   * Document.
   * 
   * @param n the number of tokens to buffer
   */
  public void setTokenThreshold (int n)
  {
    tokenThreshold = n;
  }
  
  /**
   * Returns the number of tokens that are buffered before the document
   * is rendered.
   * 
   * @return the number of tokens buffered
   */
  public int getTokenThreshold ()
  {
    return tokenThreshold;
  }
  
  /**
   * Returns the location against which to resolve relative URLs.
   * This is the document's URL if the document was loaded from a URL.
   * If a <code>base</code> tag is found, it will be used.
   * @return the base URL
   */
  public URL getBase()
  {
    return baseURL;
  }
  
  /**
   * Sets the location against which to resolve relative URLs.
   * @param u the new base URL
   */
  public void setBase(URL u)
  {
    baseURL = u;
    getStyleSheet().setBase(u);
  }
  
  /**
   * Returns whether or not the parser preserves unknown HTML tags.
   * @return true if the parser preserves unknown tags
   */
  public boolean getPreservesUnknownTags()
  {
    return preservesUnknownTags;
  }
  
  /**
   * Sets the behaviour of the parser when it encounters unknown HTML tags.
   * @param preservesTags true if the parser should preserve unknown tags.
   */
  public void setPreservesUnknownTags(boolean preservesTags)
  {
    preservesUnknownTags = preservesTags;
  }
  
  /**
   * An iterator to iterate through LeafElements in the document.
   */
  class LeafIterator extends Iterator
  {
    HTML.Tag tag;
    HTMLDocument doc;
    ElementIterator it;

    public LeafIterator (HTML.Tag t, HTMLDocument d)
    {
      doc = d;
      tag = t;
      it = new ElementIterator(doc);
    }
    
    /**
     * Return the attributes for the tag associated with this iteartor
     * @return the AttributeSet
     */
    public AttributeSet getAttributes()
    {
      if (it.current() != null)
        return it.current().getAttributes();
      return null;
    }

    /**
     * Get the end of the range for the current occurrence of the tag
     * being defined and having the same attributes.
     * @return the end of the range
     */
    public int getEndOffset()
    {
      if (it.current() != null)
        return it.current().getEndOffset();
      return -1;
    }

    /**
     * Get the start of the range for the current occurrence of the tag
     * being defined and having the same attributes.
     * @return the start of the range (-1 if it can't be found).
     */

    public int getStartOffset()
    {
      if (it.current() != null)
        return it.current().getStartOffset();
      return -1;
    }

    /**
     * Advance the iterator to the next LeafElement .
     */
    public void next()
    {
      it.next();
      while (it.current()!= null && !it.current().isLeaf())
        it.next();
    }

    /**
     * Indicates whether or not the iterator currently represents an occurrence
     * of the tag.
     * @return true if the iterator currently represents an occurrence of the
     * tag.
     */
    public boolean isValid()
    {
      return it.current() != null;
    }

    /**
     * Type of tag for this iterator.
     */
    public Tag getTag()
    {
      return tag;
    }

  }

  public void processHTMLFrameHyperlinkEvent(HTMLFrameHyperlinkEvent event)
  {
    String target = event.getTarget();
    Element el = event.getSourceElement();
    URL url = event.getURL();
    if (target.equals("_self"))
      {
        updateFrame(el, url);
      }
    else if (target.equals("_parent"))
      {
        updateFrameSet(el.getParentElement(), url);
      }
    else
      {
        Element targetFrame = findFrame(target);
        if (targetFrame != null)
          updateFrame(targetFrame, url);
      }
  }

  /**
   * Finds the named frame inside this document.
   *
   * @param target the name to look for
   *
   * @return the frame if there is a matching frame, <code>null</code>
   *         otherwise
   */
  private Element findFrame(String target)
  {
    ElementIterator i = new ElementIterator(this);
    Element next = null;
    while ((next = i.next()) != null)
      {
        AttributeSet atts = next.getAttributes();
        if (atts.getAttribute(StyleConstants.NameAttribute) == HTML.Tag.FRAME)
          {
            String name = (String) atts.getAttribute(HTML.Attribute.NAME);
            if (name != null && name.equals(target))
              break;
          }
      }
    return next;
  }

  /**
   * Updates the frame that is represented by the specified element to
   * refer to the specified URL.
   *
   * @param el the element
   * @param url the new url
   */
  private void updateFrame(Element el, URL url)
  {
    try
      {
        writeLock();
        DefaultDocumentEvent ev =
          new DefaultDocumentEvent(el.getStartOffset(), 1,
                                   DocumentEvent.EventType.CHANGE);
        AttributeSet elAtts = el.getAttributes();
        AttributeSet copy = elAtts.copyAttributes();
        MutableAttributeSet matts = (MutableAttributeSet) elAtts;
        ev.addEdit(new AttributeUndoableEdit(el, copy, false));
        matts.removeAttribute(HTML.Attribute.SRC);
        matts.addAttribute(HTML.Attribute.SRC, url.toString());
        ev.end();
        fireChangedUpdate(ev);
        fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
      }
    finally
      {
        writeUnlock();
      }
  }

  /**
   * Updates the frameset that is represented by the specified element
   * to create a frame that refers to the specified URL.
   *
   * @param el the element
   * @param url the url
   */
  private void updateFrameSet(Element el, URL url)
  {
    int start = el.getStartOffset();
    int end = el.getEndOffset();
    
    StringBuilder html = new StringBuilder();
    html.append("<frame");
    if (url != null)
      {
        html.append(" src=\"");
        html.append(url.toString());
        html.append("\"");
      }
    html.append('>');
    if (getParser() == null)
      setParser(new HTMLEditorKit().getParser());
    try
      {
        setOuterHTML(el, html.toString());
      }
    catch (BadLocationException ex)
      {
        ex.printStackTrace();
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }
  }

  /**
   * Gets an iterator for the given HTML.Tag.
   * @param t the requested HTML.Tag
   * @return the Iterator
   */
  public HTMLDocument.Iterator getIterator (HTML.Tag t)
  {
    return new HTMLDocument.LeafIterator(t, this);
  }
  
  /**
   * An iterator over a particular type of tag.
   */
  public abstract static class Iterator
  {
    /**
     * Return the attribute set for this tag.
     * @return the <code>AttributeSet</code> (null if none found).
     */
    public abstract AttributeSet getAttributes();
    
    /**
     * Get the end of the range for the current occurrence of the tag
     * being defined and having the same attributes.
     * @return the end of the range
     */
    public abstract int getEndOffset();
    
    /**
     * Get the start of the range for the current occurrence of the tag
     * being defined and having the same attributes.
     * @return the start of the range (-1 if it can't be found).
     */
    public abstract int getStartOffset();
    
    /**
     * Move the iterator forward.
     */
    public abstract void next();
    
    /**
     * Indicates whether or not the iterator currently represents an occurrence
     * of the tag.
     * @return true if the iterator currently represents an occurrence of the
     * tag.
     */
    public abstract boolean isValid();
    
    /**
     * Type of tag this iterator represents.
     * @return the tag.
     */
    public abstract HTML.Tag getTag();
  }
  
  public class BlockElement extends AbstractDocument.BranchElement
  {
    public BlockElement (Element parent, AttributeSet a)
    {
      super(parent, a);
    }
    
    /**
     * Gets the resolving parent.  Since HTML attributes are not 
     * inherited at the model level, this returns null.
     */
    public AttributeSet getResolveParent()
    {
      return null;
    }
    
    /**
     * Gets the name of the element.
     * 
     * @return the name of the element if it exists, null otherwise.
     */
    public String getName()
    {
      Object tag = getAttribute(StyleConstants.NameAttribute);
      String name = null;
      if (tag != null)
        name = tag.toString();
      if (name == null)
        name = super.getName();
      return name;
    }
  }

  /**
   * RunElement represents a section of text that has a set of 
   * HTML character level attributes assigned to it.
   */
  public class RunElement extends AbstractDocument.LeafElement
  {
    
    /**
     * Constructs an element that has no children. It represents content
     * within the document.
     * 
     * @param parent - parent of this
     * @param a - elements attributes
     * @param start - the start offset >= 0
     * @param end - the end offset 
     */
    public RunElement(Element parent, AttributeSet a, int start, int end)
    {
      super(parent, a, start, end);
    }
    
    /**
     * Gets the name of the element.
     * 
     * @return the name of the element if it exists, null otherwise.
     */
    public String getName()
    {
      Object tag = getAttribute(StyleConstants.NameAttribute);
      String name = null;
      if (tag != null)
        name = tag.toString();
      if (name == null)
        name = super.getName();
      return name;
    }
    
    /**
     * Gets the resolving parent. HTML attributes do not inherit at the
     * model level, so this method returns null.
     * 
     * @return null
     */
    public AttributeSet getResolveParent()
    {
      return null;
    }
  }
  
  /**
   * A reader to load an HTMLDocument with HTML structure.
   * 
   * @author Anthony Balkissoon abalkiss at redhat dot com
   */
  public class HTMLReader extends HTMLEditorKit.ParserCallback
  {
    /**
     * The maximum token threshold. We don't grow it larger than this.
     */
    private static final int MAX_THRESHOLD = 10000;

    /**
     * The threshold growth factor.
     */
    private static final int GROW_THRESHOLD = 5;

    /**
     * Holds the current character attribute set *
     */
    protected MutableAttributeSet charAttr = new SimpleAttributeSet();
    
    protected Vector<ElementSpec> parseBuffer = new Vector<ElementSpec>();   

    /**
     * The parse stack. It holds the current element tree path.
     */
    private Stack<HTML.Tag> parseStack = new Stack<HTML.Tag>();

    /** 
     * A stack for character attribute sets *
     */
    Stack charAttrStack = new Stack();
   
    /** A mapping between HTML.Tag objects and the actions that handle them **/
    HashMap tagToAction;
    
    /** Tells us whether we've received the '</html>' tag yet **/
    boolean endHTMLEncountered = false;
    
    /** 
     * Related to the constructor with explicit insertTag 
     */
    int popDepth;
    
    /**
     * Related to the constructor with explicit insertTag
     */    
    int pushDepth;
    
    /** 
     * Related to the constructor with explicit insertTag
     */    
    int offset;
    
    /**
     * The tag (inclusve), after that the insertion should start.
     */
    HTML.Tag insertTag;
    
    /**
     * This variable becomes true after the insert tag has been encountered.
     */
    boolean insertTagEncountered;

    
    /** A temporary variable that helps with the printing out of debug information **/
    boolean debug = false;

    /**
     * This is true when we are inside a pre tag.
     */
    boolean inPreTag = false;

    /**
     * This is true when we are inside a style tag. This will add text
     * content inside this style tag beeing parsed as CSS.
     *
     * This is package private to avoid accessor methods.
     */
    boolean inStyleTag = false;

    /**
     * This is true when we are inside a &lt;textarea&gt; tag. Any text
     * content will then be added to the text area.
     *
     * This is package private to avoid accessor methods.
     */
    boolean inTextArea = false;

    /**
     * This contains all stylesheets that are somehow read, either
     * via embedded style tags, or via linked stylesheets. The
     * elements will be String objects containing a stylesheet each.
     */
    ArrayList styles;

    /**
     * The document model for a textarea.
     *
     * This is package private to avoid accessor methods.
     */
    ResetablePlainDocument textAreaDocument;

    /**
     * The current model of a select tag. Can be a ComboBoxModel or a
     * ListModel depending on the type of the select box.
     */
    Object selectModel;

    /**
     * The current option beeing read.
     */
    Option option;

    /**
     * The current number of options in the current select model.
     */
    int numOptions;

    /**
     * The current button groups mappings.
     */
    HashMap buttonGroups;

    /**
     * The token threshold. This gets increased while loading.
     */
    private int threshold;

    public class TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.  By default this does nothing.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // Nothing to do here.
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.  By default does nothing.
       */
      public void end(HTML.Tag t)
      {
        // Nothing to do here.
      }
    }

    public class BlockAction extends TagAction
    {      
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // Tell the parse buffer to open a new block for this tag.
        blockOpen(t, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // Tell the parse buffer to close this block.
        blockClose(t);
      }
    }
    
    public class CharacterAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // Put the old attribute set on the stack.
        pushCharacterStyle();

        // Initialize with link pseudo class.
        if (t == HTML.Tag.A)
          a.addAttribute(HTML.Attribute.PSEUDO_CLASS, "link");

        // Just add the attributes in <code>a</code>.
        charAttr.addAttribute(t, a.copyAttributes());
      }

      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        popCharacterStyle();
      } 
    }

    /**
     * Processes elements that make up forms: &lt;input&gt;, &lt;textarea&gt;,
     * &lt;select&gt; and &lt;option&gt;.
     */
    public class FormAction extends SpecialAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        if (t == HTML.Tag.INPUT)
          {
            String type = (String) a.getAttribute(HTML.Attribute.TYPE);
            if (type == null)
              {
                type = "text"; // Default to 'text' when nothing was specified.
                a.addAttribute(HTML.Attribute.TYPE, type);
              }
            setModel(type, a);
          }
        else if (t == HTML.Tag.TEXTAREA)
          {
            inTextArea = true;
            textAreaDocument = new ResetablePlainDocument();
            a.addAttribute(StyleConstants.ModelAttribute, textAreaDocument);
          }
        else if (t == HTML.Tag.SELECT)
          {
            int size = HTML.getIntegerAttributeValue(a, HTML.Attribute.SIZE,
                                                     1);
            boolean multi = a.getAttribute(HTML.Attribute.MULTIPLE) != null;
            if (size > 1 || multi)
              {
                SelectListModel m = new SelectListModel();
                if (multi)
                  m.getSelectionModel().setSelectionMode(ListSelectionModel
                                                 .MULTIPLE_INTERVAL_SELECTION);
                selectModel = m;
              }
            else
              {
                selectModel = new SelectComboBoxModel();
              }
            a.addAttribute(StyleConstants.ModelAttribute, selectModel);
          }
        if (t == HTML.Tag.OPTION)
          {
            option = new Option(a);
            if (selectModel instanceof SelectListModel)
              {
                SelectListModel m = (SelectListModel) selectModel;
                m.addElement(option);
                if (option.isSelected())
                  {
                    m.getSelectionModel().addSelectionInterval(numOptions,
                                                               numOptions);
                    m.addInitialSelection(numOptions);
                  }
              }
            else if (selectModel instanceof SelectComboBoxModel)
              {
                SelectComboBoxModel m = (SelectComboBoxModel) selectModel;
                m.addElement(option);
                if (option.isSelected())
                  {
                    m.setSelectedItem(option);
                    m.setInitialSelection(option);
                  }
              }
            numOptions++;
          }
        else
          {
            // Build the element.
            super.start(t, a);
          }
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        if (t == HTML.Tag.OPTION)
          {
            option = null;
          }
        else
          {
            if (t == HTML.Tag.TEXTAREA)
              {
                inTextArea = false;
              }
            else if (t == HTML.Tag.SELECT)
              {
                selectModel = null;
                numOptions = 0;
              }
            // Finish the element.
            super.end(t);
          }
      }

      private void setModel(String type, MutableAttributeSet attrs)
      {
        if (type.equals("submit") || type.equals("reset")
            || type.equals("image"))
          {
            // Create button.
            attrs.addAttribute(StyleConstants.ModelAttribute,
                               new DefaultButtonModel());
          }
        else if (type.equals("text") || type.equals("password"))
          {
            String text = (String) attrs.getAttribute(HTML.Attribute.VALUE);
            ResetablePlainDocument doc = new ResetablePlainDocument();
            if (text != null)
              {
                doc.setInitialText(text);
                try
                  {
                    doc.insertString(0, text, null);
                  }
                catch (BadLocationException ex)
                  {
                    // Shouldn't happen.
                    assert false;
                  }
              }
            attrs.addAttribute(StyleConstants.ModelAttribute, doc);
          }
        else if (type.equals("file"))
          {
            attrs.addAttribute(StyleConstants.ModelAttribute,
                               new PlainDocument());
          }
        else if (type.equals("checkbox") || type.equals("radio"))
          {
            ResetableToggleButtonModel model =
              new ResetableToggleButtonModel();
            if (attrs.getAttribute(HTML.Attribute.SELECTED) != null)
              {
                model.setSelected(true);
                model.setInitial(true);
              }
            if (type.equals("radio"))
              {
                String name = (String) attrs.getAttribute(HTML.Attribute.NAME);
                if (name != null)
                  {
                    if (buttonGroups == null)
                      buttonGroups = new HashMap();
                    ButtonGroup group = (ButtonGroup) buttonGroups.get(name);
                    if (group == null)
                      {
                        group = new ButtonGroup();
                        buttonGroups.put(name, group);
                      }
                    model.setGroup(group);
                  }
              }
            attrs.addAttribute(StyleConstants.ModelAttribute, model);
          }
      }
    }

    /**
     * Called for form tags.
     */
    class FormTagAction
      extends BlockAction
    {
      /**
       * Clears the button group mapping.
       */
      public void end(HTML.Tag t)
      {
        super.end(t);
        buttonGroups = null;
      } 
    }

    /**
     * This action indicates that the content between starting and closing HTML
     * elements (like script - /script) should not be visible. The content is
     * still inserted and can be accessed when iterating the HTML document. The
     * parser will only fire
     * {@link javax.swing.text.html.HTMLEditorKit.ParserCallback#handleText} for
     * the hidden tags, regardless from that html tags the hidden section may
     * contain.
     */
    public class HiddenAction
        extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        blockOpen(t, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        blockClose(t);
      } 
    }

    /**
     * Handles &lt;isindex&gt; tags.
     */
    public class IsindexAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        blockOpen(HTML.Tag.IMPLIED, new SimpleAttributeSet());
        addSpecialElement(t, a);
        blockClose(HTML.Tag.IMPLIED);
      }
    }
    
    public class ParagraphAction extends BlockAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        super.start(t, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        super.end(t);
      } 
    }

    /**
     * This action is performed when a &lt;pre&gt; tag is parsed.
     */
    public class PreAction extends BlockAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        inPreTag = true;
        blockOpen(t, a);
        a.addAttribute(CSS.Attribute.WHITE_SPACE, "pre");
        blockOpen(HTML.Tag.IMPLIED, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        blockClose(HTML.Tag.IMPLIED);
        inPreTag = false;
        blockClose(t);
      } 
    }
    
    /**
     * Inserts the elements that are represented by ths single tag with 
     * attributes (only). The closing tag, even if present, mut follow
     * immediately after the starting tag without providing any additional
     * information. Hence the {@link TagAction#end} method need not be
     * overridden and still does nothing.
     */
    public class SpecialAction extends TagAction
    {
      /**
       * The functionality is delegated to {@link HTMLReader#addSpecialElement}
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        addSpecialElement(t, a);
      }
    }
    
    class AreaAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
        throws NotImplementedException
      {
        // FIXME: Implement.
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
        throws NotImplementedException
      {
        // FIXME: Implement.
      } 
    }

    /**
     * Converts HTML tags to CSS attributes.
     */
    class ConvertAction
      extends TagAction
    {

      public void start(HTML.Tag tag, MutableAttributeSet atts)
      {
        pushCharacterStyle();
        charAttr.addAttribute(tag, atts.copyAttributes());
        StyleSheet styleSheet = getStyleSheet();
        // TODO: Add other tags here.
        if (tag == HTML.Tag.FONT)
          {
            String color = (String) atts.getAttribute(HTML.Attribute.COLOR);
            if (color != null)
              styleSheet.addCSSAttribute(charAttr, CSS.Attribute.COLOR, color);
            String face = (String) atts.getAttribute(HTML.Attribute.FACE);
            if (face != null)
              styleSheet.addCSSAttribute(charAttr, CSS.Attribute.FONT_FAMILY,
                                         face);
            String size = (String) atts.getAttribute(HTML.Attribute.SIZE);
            if (size != null)
              styleSheet.addCSSAttribute(charAttr, CSS.Attribute.FONT_SIZE,
                                         size);
          }
      }

      public void end(HTML.Tag tag)
      {
        popCharacterStyle();
      }
    }

    class BaseAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        baseTarget = (String) a.getAttribute(HTML.Attribute.TARGET);
      }
    }
    
    class HeadAction extends BlockAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
        throws NotImplementedException
      {
        // FIXME: Implement.
        super.start(t, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // We read in all the stylesheets that are embedded or referenced
        // inside the header.
        if (styles != null)
          {
            int numStyles = styles.size();
            for (int i = 0; i < numStyles; i++)
              {
                String style = (String) styles.get(i);
                getStyleSheet().addRule(style);
              }
          }
        super.end(t);
      }
    }
    
    class LinkAction extends HiddenAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        super.start(t, a);
        String type = (String) a.getAttribute(HTML.Attribute.TYPE);
        if (type == null)
          type = "text/css";
        if (type.equals("text/css"))
          {
            String rel = (String) a.getAttribute(HTML.Attribute.REL);
            String media = (String) a.getAttribute(HTML.Attribute.MEDIA);
            String title = (String) a.getAttribute(HTML.Attribute.TITLE);
            if (media == null)
              media = "all";
            else
              media = media.toLowerCase();
            if (rel != null)
              {
                rel = rel.toLowerCase();
                if ((media.indexOf("all") != -1
                     || media.indexOf("screen") != -1)
                    && (rel.equals("stylesheet")))
                  {
                    String href = (String) a.getAttribute(HTML.Attribute.HREF);
                    URL url = null;
                    try
                      {
                        url = new URL(baseURL, href);
                      }
                    catch (MalformedURLException ex)
                      {
                        try
                          {
                            url = new URL(href);
                          }
                        catch (MalformedURLException ex2)
                          {
                            url = null;
                          }
                      }
                    if (url != null)
                      {
                        try
                          {
                            getStyleSheet().importStyleSheet(url);
                          }
                        catch (Exception ex)
                          {
                            // Don't let exceptions and runtime exceptions
                            // in CSS parsing disprupt the HTML parsing
                            // process. But inform the user/developer
                            // on the console about it.
                            ex.printStackTrace();
                          }
                      }
                  }                  
              }
          }
      }
      
    }
    
    class MapAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
        throws NotImplementedException
      {
        // FIXME: Implement.
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
        throws NotImplementedException
      {
        // FIXME: Implement.
      } 
    }
    
    class MetaAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
        throws NotImplementedException
      {
        // FIXME: Implement.
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
        throws NotImplementedException
      {
        // FIXME: Implement.
      } 
    }

    class StyleAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        inStyleTag = true;
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        inStyleTag = false;
      } 
    }
    
    class TitleAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
        throws NotImplementedException
      {
        // FIXME: Implement.
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
        throws NotImplementedException
      {
        // FIXME: Implement.
      } 
    }    
    
    public HTMLReader(int offset)
    {
      this (offset, 0, 0, null);
    }
    
    public HTMLReader(int offset, int popDepth, int pushDepth,
                      HTML.Tag insertTag)
    {
      this.insertTag = insertTag;
      this.offset = offset;
      this.popDepth = popDepth;
      this.pushDepth = pushDepth;
      threshold = getTokenThreshold();
      initTags();
    }
    
    void initTags()
    {
      tagToAction = new HashMap(72);
      CharacterAction characterAction = new CharacterAction();
      HiddenAction hiddenAction = new HiddenAction();
      AreaAction areaAction = new AreaAction();
      BaseAction baseAction = new BaseAction();
      BlockAction blockAction = new BlockAction();
      SpecialAction specialAction = new SpecialAction();
      ParagraphAction paragraphAction = new ParagraphAction();
      HeadAction headAction = new HeadAction();
      FormAction formAction = new FormAction();
      IsindexAction isindexAction = new IsindexAction();
      LinkAction linkAction = new LinkAction();
      MapAction mapAction = new MapAction();
      PreAction preAction = new PreAction();
      MetaAction metaAction = new MetaAction();
      StyleAction styleAction = new StyleAction();
      TitleAction titleAction = new TitleAction();
      
      ConvertAction convertAction = new ConvertAction();
      tagToAction.put(HTML.Tag.A, characterAction);
      tagToAction.put(HTML.Tag.ADDRESS, characterAction);
      tagToAction.put(HTML.Tag.APPLET, hiddenAction);
      tagToAction.put(HTML.Tag.AREA, areaAction);
      tagToAction.put(HTML.Tag.B, characterAction);
      tagToAction.put(HTML.Tag.BASE, baseAction);
      tagToAction.put(HTML.Tag.BASEFONT, characterAction);
      tagToAction.put(HTML.Tag.BIG, characterAction);
      tagToAction.put(HTML.Tag.BLOCKQUOTE, blockAction);
      tagToAction.put(HTML.Tag.BODY, blockAction);
      tagToAction.put(HTML.Tag.BR, specialAction);
      tagToAction.put(HTML.Tag.CAPTION, blockAction);
      tagToAction.put(HTML.Tag.CENTER, blockAction);
      tagToAction.put(HTML.Tag.CITE, characterAction);
      tagToAction.put(HTML.Tag.CODE, characterAction);
      tagToAction.put(HTML.Tag.DD, blockAction);
      tagToAction.put(HTML.Tag.DFN, characterAction);
      tagToAction.put(HTML.Tag.DIR, blockAction);
      tagToAction.put(HTML.Tag.DIV, blockAction);
      tagToAction.put(HTML.Tag.DL, blockAction);
      tagToAction.put(HTML.Tag.DT, paragraphAction);
      tagToAction.put(HTML.Tag.EM, characterAction);
      tagToAction.put(HTML.Tag.FONT, convertAction);
      tagToAction.put(HTML.Tag.FORM, new FormTagAction());
      tagToAction.put(HTML.Tag.FRAME, specialAction);
      tagToAction.put(HTML.Tag.FRAMESET, blockAction);
      tagToAction.put(HTML.Tag.H1, paragraphAction);
      tagToAction.put(HTML.Tag.H2, paragraphAction);
      tagToAction.put(HTML.Tag.H3, paragraphAction);
      tagToAction.put(HTML.Tag.H4, paragraphAction);
      tagToAction.put(HTML.Tag.H5, paragraphAction);
      tagToAction.put(HTML.Tag.H6, paragraphAction);
      tagToAction.put(HTML.Tag.HEAD, headAction);
      tagToAction.put(HTML.Tag.HR, specialAction);
      tagToAction.put(HTML.Tag.HTML, blockAction);
      tagToAction.put(HTML.Tag.I, characterAction);
      tagToAction.put(HTML.Tag.IMG, specialAction);
      tagToAction.put(HTML.Tag.INPUT, formAction);
      tagToAction.put(HTML.Tag.ISINDEX, isindexAction);
      tagToAction.put(HTML.Tag.KBD, characterAction);
      tagToAction.put(HTML.Tag.LI, blockAction);
      tagToAction.put(HTML.Tag.LINK, linkAction);
      tagToAction.put(HTML.Tag.MAP, mapAction);
      tagToAction.put(HTML.Tag.MENU, blockAction);
      tagToAction.put(HTML.Tag.META, metaAction);
      tagToAction.put(HTML.Tag.NOFRAMES, blockAction);
      tagToAction.put(HTML.Tag.OBJECT, specialAction);
      tagToAction.put(HTML.Tag.OL, blockAction);
      tagToAction.put(HTML.Tag.OPTION, formAction);
      tagToAction.put(HTML.Tag.P, paragraphAction);
      tagToAction.put(HTML.Tag.PARAM, hiddenAction);
      tagToAction.put(HTML.Tag.PRE, preAction);
      tagToAction.put(HTML.Tag.SAMP, characterAction);
      tagToAction.put(HTML.Tag.SCRIPT, hiddenAction);
      tagToAction.put(HTML.Tag.SELECT, formAction);
      tagToAction.put(HTML.Tag.SMALL, characterAction);
      tagToAction.put(HTML.Tag.STRIKE, characterAction);
      tagToAction.put(HTML.Tag.S, characterAction);      
      tagToAction.put(HTML.Tag.STRONG, characterAction);
      tagToAction.put(HTML.Tag.STYLE, styleAction);
      tagToAction.put(HTML.Tag.SUB, characterAction);
      tagToAction.put(HTML.Tag.SUP, characterAction);
      tagToAction.put(HTML.Tag.TABLE, blockAction);
      tagToAction.put(HTML.Tag.TD, blockAction);
      tagToAction.put(HTML.Tag.TEXTAREA, formAction);
      tagToAction.put(HTML.Tag.TH, blockAction);
      tagToAction.put(HTML.Tag.TITLE, titleAction);
      tagToAction.put(HTML.Tag.TR, blockAction);
      tagToAction.put(HTML.Tag.TT, characterAction);
      tagToAction.put(HTML.Tag.U, characterAction);
      tagToAction.put(HTML.Tag.UL, blockAction);
      tagToAction.put(HTML.Tag.VAR, characterAction);
    }
    
    /**
     * Pushes the current character style onto the stack.
     *
     */
    protected void pushCharacterStyle()
    {
      charAttrStack.push(charAttr.copyAttributes());
    }
    
    /**
     * Pops a character style off of the stack and uses it as the 
     * current character style.
     *
     */
    protected void popCharacterStyle()
    {
      if (!charAttrStack.isEmpty())
        charAttr = (MutableAttributeSet) charAttrStack.pop();
    }
    
    /**
     * Registers a given tag with a given Action.  All of the well-known tags
     * are registered by default, but this method can change their behaviour
     * or add support for custom or currently unsupported tags.
     * 
     * @param t the Tag to register
     * @param a the Action for the Tag
     */
    protected void registerTag(HTML.Tag t, HTMLDocument.HTMLReader.TagAction a)
    {
      tagToAction.put (t, a);
    }
    
    /**
     * This is the last method called on the HTMLReader, allowing any pending
     * changes to be flushed to the HTMLDocument.
     */
    public void flush() throws BadLocationException
    {
      flushImpl();
    }

    /**
     * Flushes the buffer and handle partial inserts.
     *
     */
    private void flushImpl()
      throws BadLocationException
    {
      int oldLen = getLength();
      int size = parseBuffer.size();
      ElementSpec[] elems = new ElementSpec[size];
      parseBuffer.copyInto(elems);
      if (oldLen == 0)
        create(elems);
      else
        insert(offset, elems);
      parseBuffer.removeAllElements();
      offset += getLength() - oldLen;
    }

    /**
     * This method is called by the parser to indicate a block of 
     * text was encountered.  Should insert the text appropriately.
     * 
     * @param data the text that was inserted
     * @param pos the position at which the text was inserted
     */
    public void handleText(char[] data, int pos)
    {
      if (shouldInsert() && data != null && data.length > 0)
        {
          if (inTextArea)
            textAreaContent(data);
          else if (inPreTag)
            preContent(data);
          else if (option != null)
            option.setLabel(new String(data));
          else if (inStyleTag)
            {
              if (styles == null)
                styles = new ArrayList();
              styles.add(new String(data));
            }
          else
            addContent(data, 0, data.length);
            
        }
    }
    
    /**
     * Checks if the HTML tag should be inserted. The tags before insert tag (if
     * specified) are not inserted. Also, the tags after the end of the html are
     * not inserted.
     * 
     * @return true if the tag should be inserted, false otherwise.
     */
    private boolean shouldInsert()
    {
      return ! endHTMLEncountered
             && (insertTagEncountered || insertTag == null);
    }
    
    /**
     * This method is called by the parser and should route the call to the
     * proper handler for the tag.
     * 
     * @param t the HTML.Tag
     * @param a the attribute set
     * @param pos the position at which the tag was encountered
     */
    public void handleStartTag(HTML.Tag t, MutableAttributeSet a, int pos)
    {
      if (t == insertTag)
        insertTagEncountered = true;

      if (shouldInsert())
        {
          TagAction action = (TagAction) tagToAction.get(t);
          if (action != null)
            action.start(t, a);
        }
    }
    
    /**
     * This method called by parser to handle a comment block.
     * 
     * @param data the comment
     * @param pos the position at which the comment was encountered
     */
    public void handleComment(char[] data, int pos)
    {
      if (shouldInsert())
        {
          TagAction action = (TagAction) tagToAction.get(HTML.Tag.COMMENT);
          if (action != null)
            {
              action.start(HTML.Tag.COMMENT, new SimpleAttributeSet());
              action.end(HTML.Tag.COMMENT);
            }
        }
    }
    
    /**
     * This method is called by the parser and should route the call to the
     * proper handler for the tag.
     * 
     * @param t the HTML.Tag
     * @param pos the position at which the tag was encountered
     */
    public void handleEndTag(HTML.Tag t, int pos)
    {
      if (shouldInsert())
        {
          // If this is the </html> tag we need to stop calling the Actions
          if (t == HTML.Tag.HTML)
            endHTMLEncountered = true;

          TagAction action = (TagAction) tagToAction.get(t);
          if (action != null)
            action.end(t);
        }
    }
    
    /**
     * This is a callback from the parser that should be routed to the
     * appropriate handler for the tag.
     * 
     * @param t the HTML.Tag that was encountered
     * @param a the attribute set
     * @param pos the position at which the tag was encountered
     */
    public void handleSimpleTag(HTML.Tag t, MutableAttributeSet a, int pos)
    {
      if (t == insertTag)
        insertTagEncountered = true;

      if (shouldInsert())
        {
          TagAction action = (TagAction) tagToAction.get(t);
          if (action != null)
            {
              action.start(t, a);
              action.end(t);
            }
        }
    }
    
    /**
     * This is invoked after the stream has been parsed but before it has been
     * flushed.
     * 
     * @param eol one of \n, \r, or \r\n, whichever was encountered the most in 
     * parsing the stream
     * @since 1.3
     */
    public void handleEndOfLineString(String eol)
    {
      // FIXME: Implement.
    }
    
    /**
     * Adds the given text to the textarea document.  Called only when we are
     * within a textarea.  
     * 
     * @param data the text to add to the textarea
     */
    protected void textAreaContent(char[] data)
    {
      try
        {
          int offset = textAreaDocument.getLength();
          String text = new String(data);
          textAreaDocument.setInitialText(text);
          textAreaDocument.insertString(offset, text, null);
        }
      catch (BadLocationException ex)
        {
          // Must not happen as we insert at a model location that we
          // got from the document itself.
          assert false;
        }
    }
    
    /**
     * Adds the given text that was encountered in a <PRE> element.
     * This adds synthesized lines to hold the text runs.
     *
     * @param data the text
     */
    protected void preContent(char[] data)
    {
      int start = 0;
      for (int i = 0; i < data.length; i++)
        {
          if (data[i] == '\n')
            {
              addContent(data, start, i - start + 1);
              blockClose(HTML.Tag.IMPLIED);
              MutableAttributeSet atts = new SimpleAttributeSet();
              atts.addAttribute(CSS.Attribute.WHITE_SPACE, "pre");
              blockOpen(HTML.Tag.IMPLIED, atts);
              start = i + 1;
            }
        }
      if (start < data.length)
        {
          // Add remaining last line.
          addContent(data, start, data.length - start);
        }
    }
    
    /**
     * Instructs the parse buffer to create a block element with the given 
     * attributes.
     * 
     * @param t the tag that requires opening a new block
     * @param attr the attribute set for the new block
     */
    protected void blockOpen(HTML.Tag t, MutableAttributeSet attr)
    {
      if (inImpliedParagraph())
        blockClose(HTML.Tag.IMPLIED);

      // Push the new tag on top of the stack.
      parseStack.push(t);

      DefaultStyledDocument.ElementSpec element;

      AbstractDocument.AttributeContext ctx = getAttributeContext();
      AttributeSet copy = attr.copyAttributes();
      copy = ctx.addAttribute(copy, StyleConstants.NameAttribute, t);
      element = new DefaultStyledDocument.ElementSpec(copy,
                               DefaultStyledDocument.ElementSpec.StartTagType);
      parseBuffer.addElement(element);
    }

    /**
     * Returns true when we are currently inside a paragraph, either
     * a real one or an implied, false otherwise.
     *
     * @return
     */
    private boolean inParagraph()
    {
      boolean inParagraph = false;
      if (! parseStack.isEmpty())
        {
          HTML.Tag top = parseStack.peek();
          inParagraph = top == HTML.Tag.P || top == HTML.Tag.IMPLIED;
        }
      return inParagraph;
    }

    private boolean inImpliedParagraph()
    {
      boolean inParagraph = false;
      if (! parseStack.isEmpty())
        {
          HTML.Tag top = parseStack.peek();
          inParagraph = top == HTML.Tag.IMPLIED;
        }
      return inParagraph;
    }

    /**
     * Instructs the parse buffer to close the block element associated with 
     * the given HTML.Tag
     * 
     * @param t the HTML.Tag that is closing its block
     */
    protected void blockClose(HTML.Tag t)
    {
      DefaultStyledDocument.ElementSpec element;

      if (inImpliedParagraph() && t != HTML.Tag.IMPLIED)
        blockClose(HTML.Tag.IMPLIED);

      // Pull the token from the stack.
      if (! parseStack.isEmpty()) // Just to be sure.
        parseStack.pop();

      // If the previous tag is a start tag then we insert a synthetic
      // content tag.
      DefaultStyledDocument.ElementSpec prev;
      prev = parseBuffer.size() > 0 ? (DefaultStyledDocument.ElementSpec)
                                parseBuffer.get(parseBuffer.size() - 1) : null;
      if (prev != null &&
          prev.getType() == DefaultStyledDocument.ElementSpec.StartTagType)
        {
          addContent(new char[]{' '}, 0, 1);
        }

      element = new DefaultStyledDocument.ElementSpec(null,
				DefaultStyledDocument.ElementSpec.EndTagType);
      parseBuffer.addElement(element);
    }
    
    /**
     * Adds text to the appropriate context using the current character
     * attribute set.
     * 
     * @param data the text to add
     * @param offs the offset at which to add it
     * @param length the length of the text to add
     */
    protected void addContent(char[] data, int offs, int length)
    {
      addContent(data, offs, length, true);
    }
    
    /**
     * Adds text to the appropriate context using the current character
     * attribute set, and possibly generating an IMPLIED Tag if necessary.
     * 
     * @param data the text to add
     * @param offs the offset at which to add it
     * @param length the length of the text to add
     * @param generateImpliedPIfNecessary whether or not we should generate
     * an HTML.Tag.IMPLIED tag if necessary
     */
    protected void addContent(char[] data, int offs, int length,
                              boolean generateImpliedPIfNecessary)
    {
      if (generateImpliedPIfNecessary && ! inParagraph())
        {
          blockOpen(HTML.Tag.IMPLIED, new SimpleAttributeSet());
        }

      AbstractDocument.AttributeContext ctx = getAttributeContext();
      DefaultStyledDocument.ElementSpec element;
      AttributeSet attributes = null;

      // Copy the attribute set, don't use the same object because 
      // it may change
      if (charAttr != null)
        attributes = charAttr.copyAttributes();
      else
        attributes = ctx.getEmptySet();
      attributes = ctx.addAttribute(attributes, StyleConstants.NameAttribute,
                                    HTML.Tag.CONTENT);
      element = new DefaultStyledDocument.ElementSpec(attributes,
                                DefaultStyledDocument.ElementSpec.ContentType,
                                data, offs, length);
      
      // Add the element to the buffer
      parseBuffer.addElement(element);

      if (parseBuffer.size() > threshold)
        {
          if (threshold <= MAX_THRESHOLD)
            threshold *= GROW_THRESHOLD;
          try
            {
              flushImpl();
            }
          catch (BadLocationException ble)
            {
              // TODO: what to do here?
            }
        }
    }
    
    /**
     * Adds content that is specified in the attribute set.
     * 
     * @param t the HTML.Tag
     * @param a the attribute set specifying the special content
     */
    protected void addSpecialElement(HTML.Tag t, MutableAttributeSet a)
    {
      if (t != HTML.Tag.FRAME && ! inParagraph())
        {
          blockOpen(HTML.Tag.IMPLIED, new SimpleAttributeSet());
        }

      a.addAttribute(StyleConstants.NameAttribute, t);
      
      // The two spaces are required because some special elements like HR
      // must be broken. At least two characters are needed to break into the
      // two parts.
      DefaultStyledDocument.ElementSpec spec =
        new DefaultStyledDocument.ElementSpec(a.copyAttributes(),
	  DefaultStyledDocument.ElementSpec.ContentType, 
          new char[] {' '}, 0, 1 );
      parseBuffer.add(spec);
    }
    
  }
  
  /**
   * Gets the reader for the parser to use when loading the document with HTML. 
   * 
   * @param pos - the starting position
   * @return - the reader
   */
  public HTMLEditorKit.ParserCallback getReader(int pos)
  {
    return new HTMLReader(pos);
  }  
  
  /**
   * Gets the reader for the parser to use when loading the document with HTML. 
   * 
   * @param pos - the starting position
   * @param popDepth - the number of EndTagTypes to generate before inserting
   * @param pushDepth - the number of StartTagTypes with a direction 
   * of JoinNextDirection that should be generated before inserting, 
   * but after the end tags have been generated.
   * @param insertTag - the first tag to start inserting into document
   * @return - the reader
   */
  public HTMLEditorKit.ParserCallback getReader(int pos,
                                                int popDepth,
                                                int pushDepth,
                                                HTML.Tag insertTag)
  {
    return new HTMLReader(pos, popDepth, pushDepth, insertTag);
  }
  
  /**
   * Gets the reader for the parser to use when inserting the HTML fragment into
   * the document. Checks if the parser is present, sets the parent in the
   * element stack and removes any actions for BODY (it can be only one body in
   * a HTMLDocument).
   * 
   * @param pos - the starting position
   * @param popDepth - the number of EndTagTypes to generate before inserting
   * @param pushDepth - the number of StartTagTypes with a direction of
   *          JoinNextDirection that should be generated before inserting, but
   *          after the end tags have been generated.
   * @param insertTag - the first tag to start inserting into document
   * @param parent the element that will be the parent in the document. HTML
   *          parsing includes checks for the parent, so it must be available.
   * @return - the reader
   * @throws IllegalStateException if the parsert is not set.
   */
  public HTMLEditorKit.ParserCallback getInsertingReader(int pos, int popDepth,
                                                         int pushDepth,
                                                         HTML.Tag insertTag,
                                                         final Element parent)
      throws IllegalStateException
  {
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");

    HTMLReader reader = new HTMLReader(pos, popDepth, pushDepth, insertTag)
    {
      /**
       * Ignore BODY.
       */
      public void handleStartTag(HTML.Tag t, MutableAttributeSet a, int pos)
      {
        if (t != HTML.Tag.BODY)
          super.handleStartTag(t, a, pos);
      }

      /**
       * Ignore BODY.
       */
      public void handleEndTag(HTML.Tag t, int pos)
      {
        if (t != HTML.Tag.BODY)
          super.handleEndTag(t, pos);
      }
    };
      
    return reader;
  }   
  
  /**
   * Gets the child element that contains the attribute with the value or null.
   * Not thread-safe.
   * 
   * @param e - the element to begin search at
   * @param attribute - the desired attribute
   * @param value - the desired value
   * @return the element found with the attribute and value specified or null if
   *         it is not found.
   */
  public Element getElement(Element e, Object attribute, Object value)
  {
    if (e != null)
      {
        if (e.getAttributes().containsAttribute(attribute, value))
          return e;
        
        int count = e.getElementCount();
        for (int j = 0; j < count; j++)
          {
            Element child = e.getElement(j);
            if (child.getAttributes().containsAttribute(attribute, value))
              return child;
            
            Element grandChild = getElement(child, attribute, value);
            if (grandChild != null)
              return grandChild;
          }
      }
    return null;
  }
  
  /**
   * Returns the element that has the given id Attribute (for instance, &lt;p id
   * ='my paragraph &gt;'). If it is not found, null is returned. The HTML tag,
   * having this attribute, is not checked by this method and can be any. The
   * method is not thread-safe.
   * 
   * @param attrId - the value of the attribute id to look for
   * @return the element that has the given id.
   */
  public Element getElement(String attrId)
  {
    return getElement(getDefaultRootElement(), HTML.Attribute.ID,
                      attrId);
  }
  
  /**
   * Replaces the children of the given element with the contents of
   * the string. The document must have an HTMLEditorKit.Parser set.
   * This will be seen as at least two events, n inserts followed by a remove.
   * 
   * @param elem - the brance element whose children will be replaced
   * @param htmlText - the string to be parsed and assigned to element.
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalArgumentException - if elem is a leaf 
   * @throws IllegalStateException - if an HTMLEditorKit.Parser has not been set
   */
  public void setInnerHTML(Element elem, String htmlText) 
    throws BadLocationException, IOException
  {
    if (elem.isLeaf())
      throw new IllegalArgumentException("Element is a leaf");
    
    int start = elem.getStartOffset();
    int end = elem.getEndOffset();

    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      end, 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);
    
    // Remove the previous content
    remove(start, end - start);
  }
  
  /**
   * Replaces the given element in the parent with the string. When replacing a
   * leaf, this will attempt to make sure there is a newline present if one is
   * needed. This may result in an additional element being inserted. This will
   * be seen as at least two events, n inserts followed by a remove. The
   * HTMLEditorKit.Parser must be set.
   * 
   * @param elem - the branch element whose parent will be replaced
   * @param htmlText - the string to be parsed and assigned to elem
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalStateException - if parser is not set
   */
public void setOuterHTML(Element elem, String htmlText)
      throws BadLocationException, IOException
  {
    // Remove the current element:
    int start = elem.getStartOffset();
    int end = elem.getEndOffset();

    remove(start, end-start);
       
    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      start, 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);
  }
  
  /**
   * Inserts the string before the start of the given element. The parser must
   * be set.
   * 
   * @param elem - the element to be the root for the new text.
   * @param htmlText - the string to be parsed and assigned to elem
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalStateException - if parser has not been set
   */
  public void insertBeforeStart(Element elem, String htmlText)
      throws BadLocationException, IOException
  {
    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      elem.getStartOffset(), 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);
  }
  
  /**
   * Inserts the string at the end of the element. If elem's children are
   * leaves, and the character at elem.getEndOffset() - 1 is a newline, then it
   * will be inserted before the newline. The parser must be set.
   * 
   * @param elem - the element to be the root for the new text
   * @param htmlText - the text to insert
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalStateException - if parser is not set
   */
  public void insertBeforeEnd(Element elem, String htmlText)
      throws BadLocationException, IOException
  {
    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      elem.getEndOffset(), 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);

  }
  
  /**
   * Inserts the string after the end of the given element.
   * The parser must be set.
   * 
   * @param elem - the element to be the root for the new text
   * @param htmlText - the text to insert
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalStateException - if parser is not set
   */
  public void insertAfterEnd(Element elem, String htmlText)
      throws BadLocationException, IOException
  {
    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      elem.getEndOffset(), 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);
  }
  
  /**
   * Inserts the string at the start of the element.
   * The parser must be set.
   * 
   * @param elem - the element to be the root for the new text
   * @param htmlText - the text to insert
   * @throws BadLocationException
   * @throws IOException
   * @throws IllegalStateException - if parser is not set
   */
  public void insertAfterStart(Element elem, String htmlText)
      throws BadLocationException, IOException
  {
    HTMLEditorKit.ParserCallback reader = getInsertingReader(
      elem.getStartOffset(), 0, 0, HTML.Tag.BODY, elem);

    // TODO charset
    getParser().parse(new StringReader(htmlText), reader, true);
  }

  /**
   * Overridden to tag content with the synthetic HTML.Tag.CONTENT
   * tag.
   */
  protected void insertUpdate(DefaultDocumentEvent evt, AttributeSet att)
  {
    if (att == null)
      {
        SimpleAttributeSet sas = new SimpleAttributeSet();
        sas.addAttribute(StyleConstants.NameAttribute, HTML.Tag.CONTENT);
        att = sas;
      }
    super.insertUpdate(evt, att);
  }

  /**
   * Returns <code>true</code> when this document is inside a frame,
   * <code>false</code> otherwise.
   *
   * @return <code>true</code> when this document is inside a frame,
   *         <code>false</code> otherwise
   */
  boolean isFrameDocument()
  {
    return frameDocument;
  }

  /**
   * Set <code>true</code> when this document is inside a frame,
   * <code>false</code> otherwise.
   *
   * @param frameDoc <code>true</code> when this document is inside a frame,
   *                 <code>false</code> otherwise
   */
  void setFrameDocument(boolean frameDoc)
  {
    frameDocument = frameDoc;
  }

  /**
   * Returns the target that is specified in the base tag, if this is the case.
   *
   * @return the target that is specified in the base tag, if this is the case
   */
  String getBaseTarget()
  {
    return baseTarget;
  }

  /**
   * Updates the A tag's pseudo class value in response to a hyperlink
   * action.
   *
   * @param el the corresponding element
   * @param value the new value
   */
  void updateSpecialClass(Element el, HTML.Attribute cl, String value)
  {
    try
    {
      writeLock();
      DefaultDocumentEvent ev =
        new DefaultDocumentEvent(el.getStartOffset(), 1,
                                 DocumentEvent.EventType.CHANGE);
      AttributeSet elAtts = el.getAttributes();
      AttributeSet anchorAtts = (AttributeSet) elAtts.getAttribute(HTML.Tag.A);
      if (anchorAtts != null)
        {
          AttributeSet copy = elAtts.copyAttributes();
          StyleSheet ss = getStyleSheet();
          if (value != null)
            {
              anchorAtts = ss.addAttribute(anchorAtts, cl, value);
            }
          else
            {
              anchorAtts = ss.removeAttribute(anchorAtts, cl);
            }
          MutableAttributeSet matts = (MutableAttributeSet) elAtts;
          ev.addEdit(new AttributeUndoableEdit(el, copy, false));
          matts.removeAttribute(HTML.Tag.A);
          matts.addAttribute(HTML.Tag.A, anchorAtts);
          ev.end();
          fireChangedUpdate(ev);
          fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
        }
    }
  finally
    {
      writeUnlock();
    }
  }

}
