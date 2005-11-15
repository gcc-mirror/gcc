/* DocumentParser.java -- A parser for HTML documents.
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

import gnu.javax.swing.text.html.parser.htmlAttributeSet;
import javax.swing.text.html.parser.Parser;

import java.io.IOException;
import java.io.Reader;

import javax.swing.text.BadLocationException;
import javax.swing.text.html.HTMLEditorKit;

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
 * In this implementation, DocumentParser is directly derived from its
 * ancestor without changes of functionality.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DocumentParser
  extends Parser
  implements DTDConstants
{
  /**
   * The enclosed working parser class.
   */
  private class gnuParser
    extends gnu.javax.swing.text.html.parser.support.Parser
  {
    private gnuParser(DTD d)
    {
      super(d);
    }

    protected final void handleComment(char[] comment)
    {
      parser.handleComment(comment);
      callBack.handleComment(comment, hTag.where.startPosition);
    }

    protected final void handleEmptyTag(TagElement tag)
      throws javax.swing.text.ChangedCharSetException
    {
      parser.handleEmptyTag(tag);
      callBack.handleSimpleTag(tag.getHTMLTag(), getAttributes(),
                               hTag.where.startPosition
                              );
    }

    protected final void handleEndTag(TagElement tag)
    {
      parser.handleEndTag(tag);
      callBack.handleEndTag(tag.getHTMLTag(), hTag.where.startPosition);
    }

    protected final void handleError(int line, String message)
    {
      parser.handleError(line, message);
      callBack.handleError(message, hTag.where.startPosition);
    }

    protected final void handleStartTag(TagElement tag)
    {
      parser.handleStartTag(tag);
      htmlAttributeSet attributes = gnu.getAttributes();

      if (tag.fictional())
        attributes.addAttribute(HTMLEditorKit.ParserCallback.IMPLIED,
                                Boolean.TRUE
                               );

      callBack.handleStartTag(tag.getHTMLTag(), attributes,
                              hTag.where.startPosition
                             );
    }

    protected final void handleText(char[] text)
    {
      parser.handleText(text);
      callBack.handleText(text, hTag.where.startPosition);
    }

    DTD getDTD()
    {
      return dtd;
    }
  }

  /**
   * This field is used to access the identically named
   * methods of the outer class.
   * This is package-private to avoid an accessor method.
   */
  DocumentParser parser = this;

  /**
   * The callback.
   * This is package-private to avoid an accessor method.
   */
  HTMLEditorKit.ParserCallback callBack;

  /**
   * The reference to the working class of HTML parser that is
   * actually used to parse the document.
   * This is package-private to avoid an accessor method.
   */
  gnuParser gnu;

  /**
   * Creates a new parser that uses the given DTD to access data on the
   * possible tokens, arguments and syntax. There is no single - step way
   * to get a default DTD; you must either refer to the implementation -
   * specific packages, write your own DTD or obtain the working instance
   * of parser in other way, for example, by calling
   * {@link javax.swing.text.html.HTMLEditorKit#getParser()}.
   *
   * @param a_dtd a DTD to use.
   */
  public DocumentParser(DTD a_dtd)
  {
    super(a_dtd);
    gnu = new gnuParser(a_dtd);
  }

  /**
   * Parses the HTML document, calling methods of the provided
   * callback. This method must be multithread - safe.
   * @param reader The reader to read the HTML document from
   * @param aCallback The callback that is notifyed about the presence
   * of HTML elements in the document.
   * @param ignoreCharSet If thrue, any charset changes during parsing
   * are ignored.
   * @throws java.io.IOException
   */
  public void parse(Reader reader, HTMLEditorKit.ParserCallback aCallback,
                    boolean ignoreCharSet
                   )
             throws IOException
  {
    callBack = aCallback;
    gnu.parse(reader);

    callBack.handleEndOfLineString(gnu.getEndOfLineSequence());
    try
      {
        callBack.flush();
      }
    catch (BadLocationException ex)
      {
        // Convert this into the supported type of exception.
        throw new IOException(ex.getMessage());
      }
  }

  /**
   * Handle HTML comment. The default method returns without action.
   * @param comment the comment being handled
   */
  protected void handleComment(char[] comment)
  {
    // This default implementation does nothing.
  }

  /**
   * Handle the tag with no content, like &lt;br&gt;. The method is
   * called for the elements that, in accordance with the current DTD,
   * has an empty content.
   * @param tag the tag being handled.
   * @throws javax.swing.text.ChangedCharSetException
   */
  protected void handleEmptyTag(TagElement tag)
                         throws javax.swing.text.ChangedCharSetException
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
   * @param text a section text.
   */
  protected void handleText(char[] text)
  {
    // This default implementation does nothing.
  }
}
