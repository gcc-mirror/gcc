/* HTMLEditorKit.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.Cursor;

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.Writer;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;

import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.ParagraphView;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.TextAction;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.parser.ParserDelegator;

/**
 * @author Lillian Angel (langel at redhat dot com)
 */
public class HTMLEditorKit
  extends StyledEditorKit
  implements Serializable, Cloneable, Accessible
{
  
  /**
   * Fires the hyperlink events on the associated component
   * when needed.
   */
  public static class LinkController
    extends MouseAdapter
    implements MouseMotionListener, Serializable
    {
      
      /**
       * Constructor
       */
      public LinkController() 
      {
        super();
      }
      
      /**
       * Dispatched when the mouse is clicked. If the component
       * is read-only, then the clicked event is used to drive an
       * attempt to follow the reference specified by a link
       * 
       * @param e - the mouse event
       */
      public void mouseClicked(MouseEvent e)
      {
        /*
         These MouseInputAdapter methods generate mouse appropriate events around
         hyperlinks (entering, exiting, and activating).
         */
        // FIXME: Not implemented.
      }
      
      /**
       * Dispatched when the mouse is dragged on a component.
       * 
       * @param e - the mouse event.
       */
      public void mouseDragged(MouseEvent e)
      {
        /*
        These MouseInputAdapter methods generate mouse appropriate events around
        hyperlinks (entering, exiting, and activating).
        */
        // FIXME: Not implemented.     
      }
      
      /**
       * Dispatched when the mouse cursor has moved into the component.
       * 
       * @param e - the mouse event.
       */
      public void mouseMoved(MouseEvent e)
      {
        /*
        These MouseInputAdapter methods generate mouse appropriate events around
        hyperlinks (entering, exiting, and activating).
        */
        // FIXME: Not implemented.
      }
      
      /**
       * If the given position represents a link, then linkActivated is called
       * on the JEditorPane. Implemented to forward to the method with the same
       * name, but pos == editor == -1.
       * 
       * @param pos - the position
       * @param editor - the editor pane
       */
      protected void activateLink(int pos,
                                  JEditorPane editor)
      {
        /*
          This method creates and fires a HyperlinkEvent if the document is an
          instance of HTMLDocument and the href tag of the link is not null.
         */
        // FIXME: Not implemented.
      }
    }
  
  /**
   * This class is used to insert a string of HTML into an existing
   * document. At least 2 HTML.Tags need to be supplied. The first Tag (parentTag)
   * identifies the parent in the document to add the elements to. The second, (addTag), 
   * identifies that the first tag should be added to the document as seen in the string.
   * The parser will generate all appropriate (opening/closing tags_ even if they are not
   * in the HTML string passed in.
   */
  public static class InsertHTMLTextAction
    extends HTMLTextAction
    {
      
      /**
       * Tag in HTML to start adding tags from.
       */
      protected HTML.Tag addTag;
      
      /**
       * Alternate tag in HTML to start adding tags from if parentTag is
       * not found and alternateParentTag is not found.
       */      
      protected HTML.Tag alternateAddTag;
      
      /**
       * Alternate tag to check if parentTag is not found.
       */
      protected HTML.Tag alternateParentTag;
      
      /**
       * HTML to insert.
       */
      protected String html;
      
      /**
       * Tag to check for in the document.
       */
      protected HTML.Tag parentTag;
      
      /**
       * Initializes all fields.
       * 
       * @param name - the name of the document.
       * @param html - the html to insert
       * @param parentTag - the parent tag to check for
       * @param addTag - the tag to start adding from
       */
      public InsertHTMLTextAction(String name, String html, 
                                  HTML.Tag parentTag, HTML.Tag addTag)
      {
        this(name, html, parentTag, addTag, null, null);
      }
      
      /**
       * Initializes all fields and calls super
       * 
       * @param name - the name of the document.
       * @param html - the html to insert
       * @param parentTag - the parent tag to check for
       * @param addTag - the tag to start adding from
       * @param alternateParentTag - the alternate parent tag
       * @param alternateAddTag - the alternate add tag
       */
      public InsertHTMLTextAction(String name, String html, HTML.Tag parentTag, 
                                  HTML.Tag addTag, HTML.Tag alternateParentTag, 
                                  HTML.Tag alternateAddTag) 
      {
        super(name);
        // Fields are for easy access when the action is applied to an actual
        // document.
        this.html = html;
        this.parentTag = parentTag;
        this.addTag = addTag;
        this.alternateParentTag = alternateParentTag;
        this.alternateAddTag = alternateAddTag;
      }
      
      /**
       * HTMLEditorKit.insertHTML is called. If an exception is
       * thrown, it is wrapped in a RuntimeException and thrown.
       * 
       * @param editor - the editor to use to get the editorkit
       * @param doc -
       *          the Document to insert the HTML into.
       * @param offset -
       *          where to begin inserting the HTML.
       * @param html -
       *          the String to insert
       * @param popDepth -
       *          the number of ElementSpec.EndTagTypes to generate before
       *          inserting
       * @param pushDepth -
       *          the number of ElementSpec.StartTagTypes with a direction of
       *          ElementSpec.JoinNextDirection that should be generated before
       * @param addTag -
       *          the first tag to start inserting into document
       */
      protected void insertHTML(JEditorPane editor, HTMLDocument doc, int offset,
                              String html, int popDepth, int pushDepth,
                              HTML.Tag addTag)
      {
        try
          {
            super.getHTMLEditorKit(editor).insertHTML(doc, offset, html,
                                                      popDepth, pushDepth, addTag);
          }
        catch (IOException e)
          {
            throw (RuntimeException) new RuntimeException("Parser is null.").initCause(e);
          }
        catch (BadLocationException ex)
          {
            throw (RuntimeException) new RuntimeException("BadLocationException: "
                                              + offset).initCause(ex);
          }
      }
      
      /**
       * Invoked when inserting at a boundary. Determines the number of pops,
       * and then the number of pushes that need to be performed. The it calls
       * insertHTML.
       * 
       * @param editor -
       *          the editor to use to get the editorkit
       * @param doc -
       *          the Document to insert the HTML into.
       * @param offset -
       *          where to begin inserting the HTML.
       * @param insertElement -
       *          the element to insert
       * @param html -
       *          the html to insert
       * @param parentTag -
       *          the parent tag
       * @param addTag -
       *          the first tag
       */
      protected void insertAtBoundary(JEditorPane editor,
                                      HTMLDocument doc, int offset,
                                      Element insertElement,
                                      String html, HTML.Tag parentTag,
                                      HTML.Tag addTag)
      {
        /*
        As its name implies, this protected method is used when HTML is inserted at a
        boundary. (A boundary in this case is an offset in doc that exactly matches the
        beginning offset of the parentTag.) It performs the extra work required to keep
        the tag stack in shape and then calls insertHTML(). The editor and doc argu-
        ments are the editor pane and document where the HTML should go. The offset
        argument represents the cursor location or selection start in doc. The insert-
        Element and parentTag arguments are used to calculate the proper number of
        tag pops and pushes before inserting the HTML (via html and addTag, which are
        passed directly to insertHTML()).
        */
        // FIXME: not implemented
      }
      
      /**
       * Invoked when inserting at a boundary. Determines the number of pops, 
       * and then the number of pushes that need to be performed. The it calls
       * insertHTML.
       * 
       * @param editor - the editor to use to get the editorkit
       * @param doc -
       *          the Document to insert the HTML into.
       * @param offset -
       *          where to begin inserting the HTML.
       * @param insertElement - the element to insert
       * @param html - the html to insert
       * @param parentTag - the parent tag
       * @param addTag - the first tag
       * 
       * @deprecated as of v1.3, use insertAtBoundary
       */
      protected void insertAtBoundry(JEditorPane editor,
                                     HTMLDocument doc,
                                     int offset, Element insertElement,
                                     String html, HTML.Tag parentTag,
                                     HTML.Tag addTag)
      {
        insertAtBoundary(editor, doc, offset, insertElement,
                         html, parentTag, addTag);
      }
      
      /**
       * Inserts the HTML.
       * 
       * @param ae - the action performed
       */
      public void actionPerformed(ActionEvent ae)
      {
        Object source = ae.getSource();
        if (source instanceof JEditorPane)
          {
            JEditorPane pane = ((JEditorPane) source);
            Document d = pane.getDocument();
            if (d instanceof HTMLDocument)
              insertHTML(pane, (HTMLDocument) d, 0, html, 0, 0, addTag);
            // FIXME: is this correct parameters?
          }
        // FIXME: else not implemented
      }
  }
  
  /**
   * Abstract Action class that helps inserting HTML into an existing document.
   */
  public abstract static class HTMLTextAction
    extends StyledEditorKit.StyledTextAction
    {
      
      /**
       * Constructor
       */
      public HTMLTextAction(String name) 
      {
        super(name);
      }
      
      /**
       * Gets the HTMLDocument from the JEditorPane.
       * 
       * @param e - the editor pane
       * @return the html document.
       */
      protected HTMLDocument getHTMLDocument(JEditorPane e)
      {
        Document d = e.getDocument();
        if (d instanceof HTMLDocument)
          return (HTMLDocument) d;
        throw new IllegalArgumentException("Document is not a HTMLDocument.");
      }
      
      /**
       * Gets the HTMLEditorKit
       *  
       * @param e - the JEditorPane to get the HTMLEditorKit from.
       * @return the HTMLEditorKit
       */
      protected HTMLEditorKit getHTMLEditorKit(JEditorPane e) 
      {
        EditorKit d = e.getEditorKit();
        if (d instanceof HTMLEditorKit)
          return (HTMLEditorKit) d;
        throw new IllegalArgumentException("EditorKit is not a HTMLEditorKit.");
      }
      
      /**
       * Returns an array of Elements that contain the offset.
       * The first elements corresponds to the roots of the doc.
       * 
       * @param doc - the document to get the Elements from.
       * @param offset - the offset the Elements must contain
       * @return an array of all the elements containing the offset.
       */
      protected Element[] getElementsAt(HTMLDocument doc,
                                        int offset)
      {
        return getElementsAt(doc.getDefaultRootElement(), offset, 0);
      }
      
      /**
       * Helper function to get all elements using recursion.
       */
      private Element[] getElementsAt(Element root, int offset, int depth)
      {
        Element[] elements = null;
        if (root != null)
          {
            if (root.isLeaf())
              {
                elements = new Element[depth + 1];
                elements[depth] = root;
                return elements;
              }
            elements = getElementsAt(root.getElement(root.getElementIndex(offset)),
                                     offset, depth + 1);
            elements[depth] = root;
          }
        return elements;
      }
      
      /**
       * Returns the number of elements, starting at the deepest point, needed
       * to get an element representing tag. -1 if no elements are found, 0 if
       * the parent of the leaf at offset represents the tag.
       * 
       * @param doc -
       *          the document to search
       * @param offset -
       *          the offset to check
       * @param tag -
       *          the tag to look for
       * @return - the number of elements needed to get an element representing
       *         tag.
       */
      protected int elementCountToTag(HTMLDocument doc,
                                      int offset, HTML.Tag tag)
      {
        Element root = doc.getDefaultRootElement();
        int num = -1;
        Element next = root.getElement(root.getElementIndex(offset));
        
        while (!next.isLeaf())
          {
            num++;
            if (next.getAttributes().
                getAttribute(StyleConstants.NameAttribute).equals(tag))
              return num;
            next = next.getElement(next.getElementIndex(offset));
          }
        return num;
      }
      
      /**
       * Gets the deepest element at offset with the
       * matching tag.
       * 
       * @param doc - the document to search
       * @param offset - the offset to check for
       * @param tag - the tag to match
       * @return - the element that is found, null if not found.
       */
      protected Element findElementMatchingTag(HTMLDocument doc,
                                               int offset, HTML.Tag tag)
      {
        Element element = doc.getDefaultRootElement();
        Element tagElement = null;
        
        while (element != null)
          {
            Object otag = element.getAttributes().getAttribute(
                                     StyleConstants.NameAttribute);
            if (otag instanceof HTML.Tag && otag.equals(tag))
              tagElement = element;
            element = element.getElement(element.getElementIndex(offset));
          }
        
        return tagElement;
      }
    }
  
  /**
   * A {@link ViewFactory} that is able to create {@link View}s for
   * the <code>Element</code>s that are supported.
   */
  public static class HTMLFactory
    implements ViewFactory
  {
    
    /**
     * Constructor
     */
    public HTMLFactory()
    {
      // Do Nothing here.
    }
    
    /**
     * Creates a {@link View} for the specified <code>Element</code>.
     *
     * @param element the <code>Element</code> to create a <code>View</code>
     *        for
     * @return the <code>View</code> for the specified <code>Element</code>
     *         or <code>null</code> if the type of <code>element</code> is
     *         not supported
     */
    public View create(Element element)
    {
      View view = null;
      Object attr = element.getAttributes().getAttribute(
                                StyleConstants.NameAttribute);
      if (attr instanceof HTML.Tag)
        {
          HTML.Tag tag = (HTML.Tag) attr;

          if (tag.equals(HTML.Tag.IMPLIED) || tag.equals(HTML.Tag.P)
              || tag.equals(HTML.Tag.H1) || tag.equals(HTML.Tag.H2)
              || tag.equals(HTML.Tag.H3) || tag.equals(HTML.Tag.H4)
              || tag.equals(HTML.Tag.H5) || tag.equals(HTML.Tag.H6)
              || tag.equals(HTML.Tag.DT))
            view = new ParagraphView(element);
          else if (tag.equals(HTML.Tag.LI) || tag.equals(HTML.Tag.DL)
                   || tag.equals(HTML.Tag.DD) || tag.equals(HTML.Tag.BODY)
                   || tag.equals(HTML.Tag.HTML) || tag.equals(HTML.Tag.CENTER)
                   || tag.equals(HTML.Tag.DIV)
                   || tag.equals(HTML.Tag.BLOCKQUOTE)
                   || tag.equals(HTML.Tag.PRE))
            view = new BlockView(element, View.Y_AXIS);
          
          // FIXME: Uncomment when the views have been implemented
         /* else if (tag.equals(HTML.Tag.CONTENT))
            view = new InlineView(element); 
          else if (tag.equals(HTML.Tag.MENU) || tag.equals(HTML.Tag.DIR)
                   || tag.equals(HTML.Tag.UL) || tag.equals(HTML.Tag.OL))
            view = new ListView(element);
          else if (tag.equals(HTML.Tag.IMG))
            view = new ImageView(element);
          else if (tag.equals(HTML.Tag.HR))
            view = new HRuleView(element);
          else if (tag.equals(HTML.Tag.BR))
            view = new BRView(element);
          else if (tag.equals(HTML.Tag.TABLE))
            view = new TableView(element);
          else if (tag.equals(HTML.Tag.INPUT) || tag.equals(HTML.Tag.SELECT)
                   || tag.equals(HTML.Tag.TEXTAREA))
            view = new FormView(element);
          else if (tag.equals(HTML.Tag.OBJECT))
            view = new ObjectView(element);
          else if (tag.equals(HTML.Tag.FRAMESET))
            view = new FrameSetView(element);
          else if (tag.equals(HTML.Tag.FRAME))
            view = new FrameView(element); */
        }      
      
      if (view == null)
        {
          String name = element.getName();
          if (name.equals(AbstractDocument.ContentElementName))
            view = new LabelView(element);
          else if (name.equals(AbstractDocument.ParagraphElementName))
            view = new ParagraphView(element);
          else if (name.equals(AbstractDocument.SectionElementName))
            view = new BoxView(element, View.Y_AXIS);
          else if (name.equals(StyleConstants.ComponentElementName))
            view = new ComponentView(element);
          else if (name.equals(StyleConstants.IconElementName))
            view = new IconView(element);
        }
      return view;
    }
  }
  
  /**
   * The abstract HTML parser declaration.
   */
  public abstract static class Parser
  {
    /**
     * Parse the HTML text, calling various methods of the provided callback
     * in response to the occurence of the corresponding HTML constructions.
     * @param reader The reader to read the source HTML from.
     * @param callback The callback to receive information about the parsed
     * HTML structures
     * @param ignoreCharSet If true, the parser ignores all charset information
     * that may be present in HTML documents.
     * @throws IOException, normally if the reader throws one.
     */
    public abstract void parse(Reader reader, ParserCallback callback,
                               boolean ignoreCharSet) throws IOException;
  }

  /**
   * The "hook" that receives all information about the HTML document
   * structure while parsing it. The methods are invoked by parser
   * and should be normally overridden.
   */
  public static class ParserCallback
  {
    /**
     * If the tag does not occurs in the html stream directly, but
     * is supposed by parser, the tag attribute set contains this additional
     * attribute, having value Boolean.True.
     */
    public static final Object IMPLIED = "_implied_";

    /**
     * Constructor
     */
    public ParserCallback()
    {
      // Nothing to do here.
    }
    
    /**
     * The parser calls this method after it finishes parsing the document.
     */
    public void flush() throws BadLocationException
    {
      // Nothing to do here.
    }

    /**
     * Handle HTML comment, present in the given position.
     * @param comment the comment
     * @position the position of the comment in the text being parsed.
     */
    public void handleComment(char[] comment, int position)
    {
      // Nothing to do here.
    }

    /**
     * Notifies about the character sequences, used to separate lines in
     * this document. The parser calls this method after it finishes
     * parsing the document, but before flush().
     * @param end_of_line The "end of line sequence", one of: \r or \n or \r\n.
     */
    public void handleEndOfLineString(String end_of_line)
    {
      // Nothing to do here.
    }

    /**
     * The method is called when the HTML closing tag ((like &lt;/table&gt;)
     * is found or if the parser concludes that the one should be present
     * in the current position.
     * @param tag The tag being handled
     * @param position the tag position in the text being parsed.
     */
    public void handleEndTag(HTML.Tag tag, int position)
    {
      // Nothing to do here.
    }

    /**
     * Handle the error.
     * @param message The message, explaining the error.
     * @param position The starting position of the fragment that has caused
     * the error in the html document being parsed.
     */
    public void handleError(String message, int position)
    {
      // Nothing to do here.
    }

    /**
     * Handle the tag with no content, like &lt;br&gt;. The method is
     * called for the elements that, in accordance with the current DTD,
     * has an empty content.
     * @param tag The tag being handled.
     * @param position The tag position in the text being parsed.
     */
    public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                                int position)
    {
      // Nothing to do here.
    }

    /**
     * The method is called when the HTML opening tag ((like &lt;table&gt;)
     * is found or if the parser concludes that the one should be present
     * in the current position.
     * @param tag The tag being handled
     * @param position The tag position in the text being parsed
     */
    public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                               int position)
    {
      // Nothing to do here.
    }

    /**
     * Handle the text section.
     * @param text A section text.
     * @param position The text position in the HTML document text being parsed.
     */
    public void handleText(char[] text, int position)
    {
      // Nothing to do here.
    }
  }

  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 8751997116710384592L;

  /**
   * Default cascading stylesheed file ("default.css").
   */
  public static final String DEFAULT_CSS = "default.css";

  /**
   * The <b>bold</b> action identifier.
   */
  public static final String BOLD_ACTION = "html-bold-action";

  /**
   * The <i>italic</i> action identifier.
   */
  public static final String ITALIC_ACTION = "html-italic-action";

  /**
   * The <font color="#FF0000">color</font> action indentifier
   * (passing the color as an argument).
   */
  public static final String COLOR_ACTION = "html-color-action";

  /**
   * The <font size="+1">increase</font> font action identifier.
   */
  public static final String FONT_CHANGE_BIGGER = "html-font-bigger";

  /**
   * The <font size="-1">decrease</font> font action identifier.
   */
  public static final String FONT_CHANGE_SMALLER = "html-font-smaller";

  /**
   * Align images at the bottom.
   */
  public static final String IMG_ALIGN_BOTTOM = "html-image-align-bottom";

  /**
   * Align images at the middle.
   */
  public static final String IMG_ALIGN_MIDDLE = "html-image-align-middle";

  /**
   * Align images at the top.
   */
  public static final String IMG_ALIGN_TOP = "html-image-align-top";

  /**
   * Align images at the border.
   */
  public static final String IMG_BORDER = "html-image-border";

  /**
   * The "logical style" action identifier, passing that style as parameter.
   */
  public static final String LOGICAL_STYLE_ACTION = "html-logical-style-action";

  /**
   * The "ident paragraph left" action.
   */
  public static final String PARA_INDENT_LEFT = "html-para-indent-left";

  /**
   * The "ident paragraph right" action.
   */
  public static final String PARA_INDENT_RIGHT = "html-para-indent-right";
  
  /**
   * Actions for HTML 
   */
  private static final Action[] defaultActions = {
    // FIXME: Add default actions for html
  };
  
  /**
   * The current style sheet.
   */
  StyleSheet styleSheet;
  
  /**
   * The ViewFactory for HTMLFactory.
   */
  HTMLFactory viewFactory;
  
  /**
   * The Cursor for links.
   */
  Cursor linkCursor;
  
  /**
   * The default cursor.
   */
  Cursor defaultCursor;
  
  /**
   * The parser.
   */
  Parser parser;
  
  /**
   * The mouse listener used for links.
   */
  LinkController mouseListener;
  
  /**
   * Style context for this editor.
   */
  StyleContext styleContext;
  
  /** The content type */
  String contentType = "text/html";
  
  /** The input attributes defined by default.css */
  MutableAttributeSet inputAttributes;
  
  /** The editor pane used. */
  JEditorPane editorPane;
    
  /**
   * Constructs an HTMLEditorKit, creates a StyleContext, and loads the style sheet.
   */
  public HTMLEditorKit()
  {
    super();    
    styleContext = new StyleContext();
    styleSheet = new StyleSheet();
    styleSheet.importStyleSheet(getClass().getResource(DEFAULT_CSS));
    // FIXME: Set inputAttributes with default.css    
  }
  
  /**
   * Gets a factory suitable for producing views of any 
   * models that are produced by this kit.
   * 
   * @return the view factory suitable for producing views.
   */
  public ViewFactory getViewFactory()
  {
    if (viewFactory == null)
      viewFactory = new HTMLFactory();
    return viewFactory;
  }
  
  /**
   * Create a text storage model for this type of editor.
   *
   * @return the model
   */
  public Document createDefaultDocument()
  {
    HTMLDocument document = new HTMLDocument(getStyleSheet());
    document.setParser(getParser());
    return document;
  }

  /**
   * Get the parser that this editor kit uses for reading HTML streams. This
   * method can be overridden to use the alternative parser.
   * 
   * @return the HTML parser (by default, {@link ParserDelegator}).
   */
  protected Parser getParser()
  {
    if (parser == null)
      parser = new ParserDelegator();
    return parser;
  }
  
  /**
   * Inserts HTML into an existing document.
   * 
   * @param doc - the Document to insert the HTML into.
   * @param offset - where to begin inserting the HTML.
   * @param html - the String to insert
   * @param popDepth - the number of ElementSpec.EndTagTypes 
   * to generate before inserting
   * @param pushDepth - the number of ElementSpec.StartTagTypes 
   * with a direction of ElementSpec.JoinNextDirection that 
   * should be generated before
   * @param insertTag - the first tag to start inserting into document
   * @throws IOException - on any I/O error
   * @throws BadLocationException - if pos represents an invalid location
   * within the document
   */
  public void insertHTML(HTMLDocument doc, int offset, String html,
                         int popDepth, int pushDepth, HTML.Tag insertTag)
      throws BadLocationException, IOException
  {
    Parser parser = getParser();
    if (offset < 0 || offset > doc.getLength())
      throw new BadLocationException("Bad location", offset);
    if (parser == null)
      throw new IOException("Parser is null.");

    ParserCallback pc = ((HTMLDocument) doc).getReader
                          (offset, popDepth, pushDepth, insertTag);

    // FIXME: What should ignoreCharSet be set to?
    
    // parser.parse inserts html into the buffer
    parser.parse(new StringReader(html), pc, false);
    pc.flush();
  }
  
  /**
   * Inserts content from the given stream. Inserting HTML into a non-empty 
   * document must be inside the body Element, if you do not insert into 
   * the body an exception will be thrown. When inserting into a non-empty 
   * document all tags outside of the body (head, title) will be dropped.
   * 
   * @param in - the stream to read from
   * @param doc - the destination for the insertion
   * @param pos - the location in the document to place the content
   * @throws IOException - on any I/O error
   * @throws BadLocationException - if pos represents an invalid location
   * within the document
   */
  public void read(Reader in, Document doc, int pos) throws IOException,
      BadLocationException
  {
    if (doc instanceof HTMLDocument)
      {
        Parser parser = getParser();
        if (pos < 0 || pos > doc.getLength())
          throw new BadLocationException("Bad location", pos);
        if (parser == null)
          throw new IOException("Parser is null.");
        
        HTMLDocument hd = ((HTMLDocument) doc);
        hd.setBase(editorPane.getPage());
        ParserCallback pc = hd.getReader(pos);
        
        // FIXME: What should ignoreCharSet be set to?
        
        // parser.parse inserts html into the buffer
        parser.parse(in, pc, false);
        pc.flush();
      }
    else
      // read in DefaultEditorKit is called.
      // the string is inserted in the document as usual.
      super.read(in, doc, pos);
  }
  
  /**
   * Writes content from a document to the given stream in 
   * an appropriate format.
   * 
   * @param out - the stream to write to
   * @param doc - the source for the write
   * @param pos - the location in the document to get the content.
   * @param len - the amount to write out
   * @throws IOException - on any I/O error
   * @throws BadLocationException - if pos represents an invalid location
   * within the document
   */
  public void write(Writer out, Document doc, int pos, int len)
      throws IOException, BadLocationException
  {
    if (doc instanceof HTMLDocument)
      {
        // FIXME: Not implemented. Use HTMLWriter.
        out.write(doc.getText(pos, len));
      }
    else
      super.write(out, doc, pos, len);
  }
  
  /**
   * Gets the content type that the kit supports.
   * This kit supports the type text/html.
   * 
   * @returns the content type supported.
   */
  public String getContentType()
  {
    return contentType;
  } 
  
  /**
   * Creates a copy of the editor kit.
   * 
   * @return a copy of this.
   */
  public Object clone()
  {
    // FIXME: Need to clone all fields
    return (HTMLEditorKit) super.clone();
  }
  
  /**
   * Copies the key/values in elements AttributeSet into set. 
   * This does not copy component, icon, or element names attributes.
   * This is called anytime the caret moves over a different location. 
   * 
   * @param element - the element to create the input attributes for.
   * @param set - the set to copy the values into.
   */
  protected void createInputAttributes(Element element,
                                       MutableAttributeSet set)
  {
    set.removeAttributes(set);
    set.addAttributes(element.getAttributes());
    // FIXME: Not fully implemented.
  }
  
  /**
   * Called when this is installed into the JEditorPane.
   * 
   * @param c - the JEditorPane installed into.
   */
  public void install(JEditorPane c)
  {
    super.install(c);
    mouseListener = new LinkController();
    c.addMouseListener(mouseListener);
    editorPane = c;
    // FIXME: need to set up hyperlinklistener object
  }
  
  /**
   * Called when the this is removed from the JEditorPane.
   * It unregisters any listeners.
   * 
   * @param c - the JEditorPane being removed from.
   */
  public void deinstall(JEditorPane c)
  {
    super.deinstall(c);
    c.removeMouseListener(mouseListener);
    mouseListener = null;
    editorPane = null;
  }
  
  /**
   * Gets the AccessibleContext associated with this.
   * 
   * @return the AccessibleContext for this.
   */
  public AccessibleContext getAccessibleContext()
  {
    // FIXME: Should return an instance of 
    // javax.swing.text.html.AccessibleHTML$RootHTMLAccessibleContext
    // Not implemented yet.
    return null;
  }
  
  /**
   * Gets the action list. This list is supported by the superclass
   * augmented by the collection of actions defined locally for style
   * operations.
   * 
   * @return an array of all the actions
   */
  public Action[] getActions()
  {
    return TextAction.augmentList(super.getActions(), defaultActions);
  }
  
  /**
   * Returns the default cursor.
   * 
   * @return the default cursor
   */
  public Cursor getDefaultCursor()
  {
    if (defaultCursor == null)
      defaultCursor = Cursor.getDefaultCursor();
    return defaultCursor;
  }
  
  /**
   * Returns the cursor for links.
   * 
   * @return the cursor for links.
   */
  public Cursor getLinkCursor()
  {
    if (linkCursor == null)
      linkCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
    return linkCursor;
  }
  
  /**
   * Sets the Cursor for links.
   * 
   * @param cursor - the new cursor for links.
   */
  public void setLinkCursor(Cursor cursor)
  {
    linkCursor = cursor;
  }
  
  /**
   * Sets the default cursor.
   * 
   * @param cursor - the new default cursor.
   */
  public void setDefaultCursor(Cursor cursor)
  {
    defaultCursor = cursor;
  }
  
  /**
   * Gets the input attributes used for the styled editing actions.
   * 
   * @return the attribute set
   */
  public MutableAttributeSet getInputAttributes()
  {
    return inputAttributes;
  }
  
  /**
   * Get the set of styles currently being used to render the HTML elements. 
   * By default the resource specified by DEFAULT_CSS gets loaded, and is 
   * shared by all HTMLEditorKit instances.
   * 
   * @return the style sheet.
   */
  public StyleSheet getStyleSheet()
  {
    if (styleSheet == null)
      {
        styleSheet = new StyleSheet();
        styleSheet.importStyleSheet(getClass().getResource(DEFAULT_CSS));
      }
    return styleSheet;
  }
  
  /**
   * Set the set of styles to be used to render the various HTML elements. 
   * These styles are specified in terms of CSS specifications. Each document 
   * produced by the kit will have a copy of the sheet which it can add the 
   * document specific styles to. By default, the StyleSheet specified is shared 
   * by all HTMLEditorKit instances. 
   * 
   * @param s - the new style sheet
   */
  public void setStyleSheet(StyleSheet s)
  {
    styleSheet = s;
  }
}
