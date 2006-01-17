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

import java.net.URL;

import java.io.IOException;

import java.util.HashMap;
import java.util.Stack;
import java.util.Vector;

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
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTML.Tag;

/**
 * TODO: Add more comments here 
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
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
  StyleSheet styleSheet;
  AbstractDocument.Content content;
  
  /**
   * Constructs an HTML document using the default buffer size and a default
   * StyleSheet.
   */
  public HTMLDocument()
  {
    this(null);
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
    this.content = c;
    if (styles == null)
      {
        styles = new StyleSheet();
        styles.importStyleSheet(getClass().getResource(HTMLEditorKit.
                                                       DEFAULT_CSS));
      }
    this.styleSheet = styles;
  }
  
  /**
   * Gets the style sheet with the document display rules (CSS) that were specified 
   * in the HTML document.
   * 
   * @return - the style sheet
   */
  public StyleSheet getStyleSheet()
  {
    return styleSheet;
  }
  
  /**
   * Replaces the contents of the document with the given element specifications.
   * This is called before insert if the loading is done in bursts. This is the
   * only method called if loading the document entirely in one burst.
   * 
   * @param data - the date that replaces the content of the document
   */
  protected void create(DefaultStyledDocument.ElementSpec[] data)
  {
    // FIXME: Not implemented
    System.out.println("create not implemented");
    super.create(data);
  }
  
  /**
   * This method creates a root element for the new document.
   * 
   * @return the new default root
   */
  protected AbstractDocument.AbstractElement createDefaultRoot()
  {
    // FIXME: Not implemented
    System.out.println("createDefaultRoot not implemented");
    return super.createDefaultRoot();
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
   * @return the new element
   */
  protected Element createLeafElement(Element parent, AttributeSet a, int p0,
                                      int p1)
  {
    // FIXME: Not implemented
    System.out.println("createLeafElement not implemented");
    return super.createLeafElement(parent, a, p0, p1);
  }

  /** This method returns an HTMLDocument.BlockElement object representing the
   * attribute set a and attached to parent.
   * 
   * @param parent - the parent element
   * @param a - the attributes for the element
   * @return the new element
   */
  protected Element createBranchElement(Element parent, AttributeSet a)
  {
    // FIXME: Not implemented
    System.out.println("createBranchElement not implemented");
    return super.createBranchElement(parent, a);
  }
  
  /**
   * Inserts new elements in bulk. This is how elements get created in the
   * document. The parsing determines what structure is needed and creates the
   * specification as a set of tokens that describe the edit while leaving the
   * document free of a write-lock. This method can then be called in bursts by
   * the reader to acquire a write-lock for a shorter duration (i.e. while the
   * document is actually being altered). 
   * 
   * @param offset - the starting offset 
   * @param data - the element data
   * @throws BadLocationException - if the given position does not
   * represent a valid location in the associated document.
   */
  protected void insert(int offset, DefaultStyledDocument.ElementSpec[] data)
    throws BadLocationException
    {
      super.insert(offset, data);
    }
  
  /**
   * Updates document structure as a result of text insertion. This will happen
   * within a write lock. This implementation simply parses the inserted content
   * for line breaks and builds up a set of instructions for the element buffer.
   * 
   * @param chng - a description of the document change
   * @param attr - the attributes
   */
  protected void insertUpdate(AbstractDocument.DefaultDocumentEvent chng, 
                              AttributeSet attr)
  {
    // FIXME: Not implemented
    System.out.println("insertUpdate not implemented");
    super.insertUpdate(chng, attr);    
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
    styleSheet.setBase(u);
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
    // TODO: Implement this properly.
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
      super (parent, a);
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
      return (String) getAttribute(StyleConstants.NameAttribute); 
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
      return (String) getAttribute(StyleConstants.NameAttribute);      
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
    /** Holds the current character attribute set **/
    protected MutableAttributeSet charAttr = new SimpleAttributeSet();
    
    protected Vector parseBuffer = new Vector();
    
    /** A stack for character attribute sets **/
    Stack charAttrStack = new Stack();
    
    /** A mapping between HTML.Tag objects and the actions that handle them **/
    HashMap tagToAction;
    
    /** Tells us whether we've received the '</html>' tag yet **/
    boolean endHTMLEncountered = false;
    
    /** Variables related to the constructor with explicit insertTag **/
    int popDepth, pushDepth, offset;
    HTML.Tag insertTag;
    boolean insertTagEncountered = false;
    
    /** A temporary variable that helps with the printing out of debug information **/
    boolean debug = false;
    
    void print (String line)
    {
      if (debug)
        System.out.println (line);
    }
    
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
        
        // And create the new one by adding the attributes in <code>a</code>.
        if (a != null)
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
    
    public class FormAction extends SpecialAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("FormAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("FormAction.end not implemented");
      } 
    }
    
    public class HiddenAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("HiddenAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("HiddenAction.end not implemented");
      } 
    }
    
    public class IsindexAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("IsindexAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("IsindexAction.end not implemented");
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
        // FIXME: Implement.
        print ("ParagraphAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("ParagraphAction.end not implemented");
      } 
    }
    
    public class PreAction extends BlockAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("PreAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("PreAction.end not implemented");
      } 
    }
    
    public class SpecialAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("SpecialAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("SpecialAction.end not implemented");
      }                
    }
    
    class AreaAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("AreaAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("AreaAction.end not implemented");
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
        // FIXME: Implement.
        print ("BaseAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("BaseAction.end not implemented");
      } 
    }
    
    class HeadAction extends BlockAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("HeadAction.start not implemented: "+t);
        super.start(t, a);
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("HeadAction.end not implemented: "+t);
        super.end(t);
      } 
    }
    
    class LinkAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("LinkAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("LinkAction.end not implemented");
      } 
    }
    
    class MapAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("MapAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("MapAction.end not implemented");
      } 
    }
    
    class MetaAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("MetaAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("MetaAction.end not implemented");
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
        // FIXME: Implement.
        print ("StyleAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("StyleAction.end not implemented");
      } 
    }
    
    class TitleAction extends TagAction
    {
      /**
       * This method is called when a start tag is seen for one of the types
       * of tags associated with this Action.
       */
      public void start(HTML.Tag t, MutableAttributeSet a)
      {
        // FIXME: Implement.
        print ("TitleAction.start not implemented");
      }
      
      /**
       * Called when an end tag is seen for one of the types of tags associated
       * with this Action.
       */
      public void end(HTML.Tag t)
      {
        // FIXME: Implement.
        print ("TitleAction.end not implemented");
      } 
    }    
    
    public HTMLReader(int offset)
    {
      this (offset, 0, 0, null);
    }
    
    public HTMLReader(int offset, int popDepth, int pushDepth,
                      HTML.Tag insertTag)
    {
      print ("HTMLReader created with pop: "+popDepth
                          + " push: "+pushDepth + " offset: "+offset
                          + " tag: "+insertTag);
      this.insertTag = insertTag;
      this.offset = offset;
      this.popDepth = popDepth;
      this.pushDepth = pushDepth;
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
      tagToAction.put(HTML.Tag.FONT, characterAction);
      tagToAction.put(HTML.Tag.FORM, blockAction);
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
      charAttrStack.push(charAttr);
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
      DefaultStyledDocument.ElementSpec[] elements;
      elements = new DefaultStyledDocument.ElementSpec[parseBuffer.size()];
      parseBuffer.copyInto(elements);
      parseBuffer.removeAllElements();
      insert(offset, elements);
      offset += HTMLDocument.this.getLength() - offset;
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
      if (data != null && data.length > 0)
        addContent(data, 0, data.length);
    }
    
    /**
     * This method is called by the parser and should route the call to 
     * the proper handler for the tag.
     * 
     * @param t the HTML.Tag
     * @param a the attribute set
     * @param pos the position at which the tag was encountered
     */
    public void handleStartTag(HTML.Tag t, MutableAttributeSet a, int pos)
    {
      // Don't call the Action if we've already seen </html>.
      if (endHTMLEncountered)
        return;
        
      TagAction action = (TagAction) tagToAction.get(t);
      if (action != null)
        action.start(t, a);      
    }
    
    /**
     * This method called by parser to handle a comment block.
     * 
     * @param data the comment
     * @param pos the position at which the comment was encountered
     */
    public void handleComment(char[] data, int pos)
    {
      // Don't call the Action if we've already seen </html>.
      if (endHTMLEncountered)
        return;
      
      TagAction action = (TagAction) tagToAction.get(HTML.Tag.COMMENT);
      if (action != null)
        {
          action.start(HTML.Tag.COMMENT, new SimpleAttributeSet());
          action.end (HTML.Tag.COMMENT);
        }
    }
    
    /**
     * This method is called by the parser and should route the call to 
     * the proper handler for the tag.
     * 
     * @param t the HTML.Tag
     * @param pos the position at which the tag was encountered
     */
    public void handleEndTag(HTML.Tag t, int pos)
    {
      // Don't call the Action if we've already seen </html>.
      if (endHTMLEncountered)
        return;
      
      // If this is the </html> tag we need to stop calling the Actions
      if (t == HTML.Tag.HTML)
        endHTMLEncountered = true;
      
      TagAction action = (TagAction) tagToAction.get(t);
      if (action != null)
        action.end(t);
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
      // Don't call the Action if we've already seen </html>.
      if (endHTMLEncountered)
        return;
      
      TagAction action = (TagAction) tagToAction.get (t);
      if (action != null)
        {
          action.start(t, a);
          action.end(t);
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
      print ("HTMLReader.handleEndOfLineString not implemented yet");
    }
    
    /**
     * Adds the given text to the textarea document.  Called only when we are
     * within a textarea.  
     * 
     * @param data the text to add to the textarea
     */
    protected void textAreaContent(char[] data)
    {
      // FIXME: Implement.
      print ("HTMLReader.textAreaContent not implemented yet");
    }
    
    /**
     * Adds the given text that was encountered in a <PRE> element.
     * 
     * @param data the text
     */
    protected void preContent(char[] data)
    {
      // FIXME: Implement
      print ("HTMLReader.preContent not implemented yet");
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
      printBuffer();
      DefaultStyledDocument.ElementSpec element;
      element = new DefaultStyledDocument.ElementSpec(attr.copyAttributes(),
			DefaultStyledDocument.ElementSpec.StartTagType);
      parseBuffer.addElement(element);
      printBuffer();
    }
    
    /**
     * Instructs the parse buffer to close the block element associated with 
     * the given HTML.Tag
     * 
     * @param t the HTML.Tag that is closing its block
     */
    protected void blockClose(HTML.Tag t)
    {
      printBuffer();
      DefaultStyledDocument.ElementSpec element;
      element = new DefaultStyledDocument.ElementSpec(null,
				DefaultStyledDocument.ElementSpec.EndTagType);
      parseBuffer.addElement(element);
      printBuffer();
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
      // Copy the attribute set, don't use the same object because 
      // it may change
      AttributeSet attributes = null;
      if (charAttr != null)
        attributes = charAttr.copyAttributes();

      DefaultStyledDocument.ElementSpec element;
      element = new DefaultStyledDocument.ElementSpec(attributes,
			      DefaultStyledDocument.ElementSpec.ContentType,
                              data, offs, length);
      
      printBuffer();
      // Add the element to the buffer
      parseBuffer.addElement(element);
      printBuffer();

      if (parseBuffer.size() > HTMLDocument.this.getTokenThreshold())
        {
          try
            {
              flush();
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
      // FIXME: Implement
      print ("HTMLReader.addSpecialElement not implemented yet");
    }
    
    void printBuffer()
    {      
      print ("\n*********BUFFER**********");
      for (int i = 0; i < parseBuffer.size(); i ++)
        print ("  "+parseBuffer.get(i));
      print ("***************************");
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
   * Gets the child element that contains the attribute with the value or null.
   * Not thread-safe.
   * 
   * @param e - the element to begin search at
   * @param attribute - the desired attribute
   * @param value - the desired value
   * @return the element found with the attribute and value specified or null
   * if it is not found.
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
   * Returns the element that has the given id Attribute. If it is not found, 
   * null is returned. This method works on an Attribute, not a character tag.
   * This is not thread-safe.
   * 
   * @param attrId - the Attribute id to look for
   * @return the element that has the given id.
   */
  public Element getElement(String attrId)
  {
    Element root = getDefaultRootElement();
    return getElement(root, HTML.getAttributeKey(attrId) , attrId);
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
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");
    // FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
    System.out.println("setInnerHTML not implemented");
  }
  
  /**
   * Replaces the given element in the parent with the string. When replacing
   * a leaf, this will attempt to make sure there is a newline present if one is
   * needed. This may result in an additional element being inserted.
   * This will be seen as at least two events, n inserts followed by a remove.
   * The HTMLEditorKit.Parser must be set.
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
      if (parser == null)
        throw new IllegalStateException("Parser has not been set");
      // FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
      System.out.println("setOuterHTML not implemented");
    }
  
  /**
   * Inserts the string before the start of the given element.
   * The parser must be set.
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
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");
    //  FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
    System.out.println("insertBeforeStart not implemented");
  }
  
  /**
   * Inserts the string at the end of the element. If elem's children
   * are leaves, and the character at elem.getEndOffset() - 1 is a newline, 
   * then it will be inserted before the newline. The parser must be set.
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
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");
    //  FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
    System.out.println("insertBeforeEnd not implemented");
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
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");
    //  FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
    System.out.println("insertAfterEnd not implemented");
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
    if (parser == null)
      throw new IllegalStateException("Parser has not been set");
    //  FIXME: Not implemented fully, use InsertHTML* in HTMLEditorKit?
    System.out.println("insertAfterStart not implemented");
  }
  
  /**
   * This method sets the attributes associated with the paragraph containing
   * offset. If replace is false, s is merged with existing attributes. The
   * length argument determines how many characters are affected by the new
   * attributes. This is often the entire paragraph.
   * 
   * @param offset -
   *          the offset into the paragraph (must be at least 0)
   * @param length -
   *          the number of characters affected (must be at least 0)
   * @param s -
   *          the attributes
   * @param replace -
   *          whether to replace existing attributes, or merge them
   */
  public void setParagraphAttributes(int offset, int length, AttributeSet s,
                                     boolean replace)
  {
    //  FIXME: Not implemented.
    System.out.println("setParagraphAttributes not implemented");
    super.setParagraphAttributes(offset, length, s, replace);
  }
  
  /**
   * This method flags a change in the document.
   * 
   *  @param e - the Document event
   */
  protected void fireChangedUpdate(DocumentEvent e)
  {
    //  FIXME: Not implemented.
    System.out.println("fireChangedUpdate not implemented");
    super.fireChangedUpdate(e);    
  }

  /**
   * This method fires an event intended to be caught by Undo listeners. It
   * simply calls the super version inherited from DefaultStyledDocument. With
   * this method, an HTML editor could easily provide undo support.
   * 
   * @param e - the UndoableEditEvent
   */
  protected void fireUndoableEditUpdate(UndoableEditEvent e)
  {
    super.fireUndoableEditUpdate(e);
  }
}
