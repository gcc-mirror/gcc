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

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;

import javax.swing.text.BadLocationException;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyledEditorKit;

/**
 * This class is NOT implemented. This file currently holds only
 * declarations of the two enclosing classes, necessary for testing
 * the implemented javax.swing.text.html.parser package.
 *
 * @author No authorship is taken, implement the class and be!
 * TODO: replace this header after implementing the class.
 */
public class HTMLEditorKit
  extends StyledEditorKit
  implements Serializable, Cloneable
{
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
                               boolean ignoreCharSet
                              )
                        throws IOException;
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
     * The parser calls this method after it finishes parsing the document.
     */
    public void flush()
               throws BadLocationException
    {
    }

    /**
     * Handle HTML comment, present in the given position.
     * @param comment the comment
     * @position the position of the comment in the text being parsed.
     */
    public void handleComment(char[] comment, int position)
    {
    }

    /**
     * Notifies about the character sequences, used to separate lines in
     * this document. The parser calls this method after it finishes
     * parsing the document, but before flush().
     * @param end_of_line The "end of line sequence", one of: \r or \n or \r\n.
     */
    public void handleEndOfLineString(String end_of_line)
    {
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
    }

    /**
     * Handle the error.
     * @param message The message, explaining the error.
     * @param position The starting position of the fragment that has caused
     * the error in the html document being parsed.
     */
    public void handleError(String message, int position)
    {
    }

    /**
     * Handle the tag with no content, like &lt;br&gt;. The method is
     * called for the elements that, in accordance with the current DTD,
     * has an empty content.
     * @param tag The tag being handled.
     * @param position The tag position in the text being parsed.
     */
    public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                                int position
                               )
    {
    }

    /**
     * The method is called when the HTML opening tag ((like &lt;table&gt;)
     * is found or if the parser concludes that the one should be present
     * in the current position.
     * @param tag The tag being handled
     * @param position The tag position in the text being parsed
     */
    public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                               int position
                              )
    {
    }

    /**
     * Handle the text section.
     * @param text A section text.
     * @param position The text position in the HTML document text being parsed.
     */
    public void handleText(char[] text, int position)
    {
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
}