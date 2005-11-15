/* Parser.java -- HTML parser
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


package javax.swing.text.html.parser;

import java.io.IOException;
import java.io.Reader;

import javax.swing.text.ChangedCharSetException;
import javax.swing.text.SimpleAttributeSet;

/*
 * FOR DEVELOPERS: To avoid regression, please run the package test
 * textsuite/javax.swing.text.html.parser/AllParserTests after your
 * modifications.
 */

/**
 * <p>A simple error-tolerant HTML parser that uses a DTD document
 * to access data on the possible tokens, arguments and syntax.</p>
 * <p> The parser reads an HTML content from a Reader and calls various
 * notifying methods (which should be overridden in a subclass)
 * when tags or data are encountered.</p>
 * <p>Some HTML elements need no opening or closing tags. The
 * task of this parser is to invoke the tag handling methods also when
 * the tags are not explicitly specified and must be supposed using
 * information, stored in the DTD.
 * For  example, parsing the document
 * <p>&lt;table&gt;&lt;tr&gt;&lt;td&gt;a&lt;td&gt;b&lt;td&gt;c&lt;/tr&gt; <br>
 * will invoke exactly the handling methods exactly in the same order
 * (and with the same parameters) as if parsing the document: <br>
 * <em>&lt;html&gt;&lt;head&gt;&lt;/head&gt;&lt;body&gt;&lt;table&gt;&lt;
 * tbody&gt;</em>&lt;tr&gt;&lt;td&gt;a<em>&lt;/td&gt;</em>&lt;td&gt;b<em>
 * &lt;/td&gt;</em>&lt;td&gt;c<em>&lt;/td&gt;&lt;/tr&gt;</em>&lt;
 * <em>/tbody&gt;&lt;/table&gt;&lt;/body&gt;&lt;/html&gt;</em></p>
 * (supposed tags are given in italics). The parser also supports
 * obsolete elements of HTML syntax.<p>
 * </p>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Parser 
  implements DTDConstants
{
  /**
   * The document template description that will be used to parse the documents.
   */
  protected DTD dtd;

  /**
   * The value of this field determines whether or not the Parser will be
   * strict in enforcing SGML compatibility. The default value is false,
   * stating that the parser should do everything to parse and get at least
   * some information even from the incorrectly written HTML input.
   */
  protected boolean strict;

  /**
   * The package level reference to the working HTML parser in this
   * implementation.
   */
  final gnu.javax.swing.text.html.parser.support.Parser gnu;

  /**
   * Creates a new parser that uses the given DTD to access data on the
   * possible tokens, arguments and syntax. There is no single - step way
   * to get a default DTD; you must either refer to the implementation -
   * specific packages, write your own DTD or obtain the working instance
   * of parser in other way, for example, by calling
   * {@link javax.swing.text.html.HTMLEditorKit#getParser() }.
   * @param a_dtd A DTD to use.
   */
  public Parser(DTD a_dtd)
  {
    dtd = a_dtd;

    final Parser j = this;

    gnu =
      new gnu.javax.swing.text.html.parser.support.Parser(dtd)
        {
          protected final void handleComment(char[] comment)
          {
            j.handleComment(comment);
          }

          protected final void handleEOFInComment()
          {
            j.handleEOFInComment();
          }

          protected final void handleEmptyTag(TagElement tag)
            throws javax.swing.text.ChangedCharSetException
          {
            j.handleEmptyTag(tag);
          }

          protected final void handleStartTag(TagElement tag)
          {
            j.handleStartTag(tag);
          }

          protected final void handleEndTag(TagElement tag)
          {
            j.handleEndTag(tag);
          }

          protected final void handleError(int line, String message)
          {
            j.handleError(line, message);
          }

          protected final void handleText(char[] text)
          {
            j.handleText(text);
          }

          protected final void handleTitle(char[] title)
          {
            j.handleTitle(title);
          }

          protected final void markFirstTime(Element element)
          {
            j.markFirstTime(element);
          }

          protected final void startTag(TagElement tag)
            throws ChangedCharSetException
          {
            j.startTag(tag);
          }

          protected final void endTag(boolean omitted)
          {
            j.endTag(omitted);
          }

          protected TagElement makeTag(Element element)
          {
            return j.makeTag(element);
          }

          protected TagElement makeTag(Element element, boolean isSupposed)
          {
            return j.makeTag(element, isSupposed);
          }
        };
  }

  /**
   * Parse the HTML text, calling various methods in response to the
   * occurence of the corresponding HTML constructions.
   * @param reader The reader to read the source HTML from.
   * @throws IOException If the reader throws one.
   */
  public synchronized void parse(Reader reader)
    throws IOException
  {
    gnu.parse(reader);
  }

  /**
   * Parses DTD markup declaration. Currently returns without action.
   * @return null.
   * @throws java.io.IOException
   */
  public String parseDTDMarkup()
    throws IOException
  {
    return gnu.parseDTDMarkup();
  }

  /**
   * Parse DTD document declarations. Currently only parses the document
   * type declaration markup.
   * @param strBuff
   * @return true if this is a valid DTD markup declaration.
   * @throws IOException
   */
  protected boolean parseMarkupDeclarations(StringBuffer strBuff)
    throws IOException
  {
    return gnu.parseMarkupDeclarations(strBuff);
  }

  /**
   * Get the attributes of the current tag.
   * @return The attribute set, representing the attributes of the current tag.
   */
  protected SimpleAttributeSet getAttributes()
  {
    return gnu.getAttributes();
  }

  /**
   * Get the number of the document line being parsed.
   * @return The current line.
   */
  protected int getCurrentLine()
  {
    return gnu.hTag.where.beginLine;
  }

  /**
   * Get the current position in the document being parsed.
   * @return The current position.
   */
  protected int getCurrentPos()
  {
    return gnu.hTag.where.startPosition;
  }

  /**
   * The method is called when the HTML end (closing) tag is found or if
   * the parser concludes that the one should be present in the
   * current position. The method is called immediatly
   * before calling the handleEndTag().
   * @param omitted True if the tag is no actually present in the document,
   * but is supposed by the parser (like &lt;/html&gt; at the end of the
   * document).
   */
  protected void endTag(boolean omitted)
  {
    // This default implementation does nothing.
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * finally delegates the call to handleError, also providing the number of the
   * current line.
   */
  protected void error(String msg)
  {
    gnu.error(msg);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * finally delegates the call to error (msg+": '"+invalid+"'").
   */
  protected void error(String msg, String invalid)
  {
    gnu.error(msg, invalid);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * finally delegates the call to error (parm1+" "+ parm2+" "+ parm3).
   */
  protected void error(String parm1, String parm2, String parm3)
  {
    gnu.error(parm1, parm2, parm3);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * finally delegates the call to error
   * (parm1+" "+ parm2+" "+ parm3+" "+ parm4).
   */
  protected void error(String parm1, String parm2, String parm3, String parm4)
  {
    gnu.error(parm1, parm2, parm3, parm4);
  }

  /**
   * In this implementation, this is never called and returns without action.
   */
  protected void flushAttributes()
  {
    gnu.flushAttributes();
  }

  /**
   * Handle HTML comment. The default method returns without action.
   * @param comment The comment being handled
   */
  protected void handleComment(char[] comment)
  {
    // This default implementation does nothing.
  }

  /**
   * This is additionally called in when the HTML content terminates
   * without closing the HTML comment. This can only happen if the
   * HTML document contains errors (for example, the closing --;gt is
   * missing. The default method calls the error handler.
   */
  protected void handleEOFInComment()
  {
    gnu.error("Unclosed comment");
  }

  /**
   * Handle the tag with no content, like &lt;br&gt;. The method is
   * called for the elements that, in accordance with the current DTD,
   * has an empty content.
   * @param tag The tag being handled.
   * @throws javax.swing.text.ChangedCharSetException
   */
  protected void handleEmptyTag(TagElement tag)
    throws ChangedCharSetException
  {
    // This default implementation does nothing.
  }

  /**
   * The method is called when the HTML closing tag ((like &lt;/table&gt;)
   * is found or if the parser concludes that the one should be present
   * in the current position.
   * @param tag The tag being handled
   */
  protected void handleEndTag(TagElement tag)
  {
    // This default implementation does nothing.
  }

  /* Handle error that has occured in the given line. */
  protected void handleError(int line, String message)
  {
    // This default implementation does nothing.
  }

  /**
   * The method is called when the HTML opening tag ((like &lt;table&gt;)
   * is found or if the parser concludes that the one should be present
   * in the current position.
   * @param tag The tag being handled
   */
  protected void handleStartTag(TagElement tag)
  {
    // This default implementation does nothing.
  }

  /**
   * Handle the text section.
   * <p> For non-preformatted section, the parser replaces
   * \t, \r and \n by spaces and then multiple spaces
   * by a single space. Additionaly, all whitespace around
   * tags is discarded.
   * </p>
   * <p> For pre-formatted text (inside TEXAREA and PRE), the parser preserves
   * all tabs and spaces, but removes <b>one</b>  bounding \r, \n or \r\n,
   * if it is present. Additionally, it replaces each occurence of \r or \r\n
   * by a single \n.</p>
   *
   * @param text A section text.
   */
  protected void handleText(char[] text)
  {
    // This default implementation does nothing.
  }

  /**
   * Handle HTML &lt;title&gt; tag. This method is invoked when
   * both title starting and closing tags are already behind.
   * The passed argument contains the concatenation of all
   * title text sections.
   * @param title The title text.
   */
  protected void handleTitle(char[] title)
  {
    // This default implementation does nothing.
  }

  /**
   * Constructs the tag from the given element. In this implementation,
   * this is defined, but never called.
   * @param element the base element of the tag.
   * @return the tag
   */
  protected TagElement makeTag(Element element)
  {
    return makeTag(element, false);
  }

  /**
   * Constructs the tag from the given element.
   * @param element the tag base {@link javax.swing.text.html.parser.Element}
   * @param isSupposed true if the tag is not actually present in the
   * html input, but the parser supposes that it should to occur in
   * the current location.
   * @return the tag
   */
  protected TagElement makeTag(Element element, boolean isSupposed)
  {
    return new TagElement(element, isSupposed);
  }

  /**
   * This is called when the tag, representing the given element,
   * occurs first time in the document.
   * @param element
   */
  protected void markFirstTime(Element element)
  {
    // This default implementation does nothing.
  }

  /**
   * The method is called when the HTML opening tag ((like &lt;table&gt;)
   * is found or if the parser concludes that the one should be present
   * in the current position. The method is called immediately before
   * calling the handleStartTag.
   * @param tag The tag
   */
  protected void startTag(TagElement tag)
    throws ChangedCharSetException
  {
    // This default implementation does nothing.
  }
}
