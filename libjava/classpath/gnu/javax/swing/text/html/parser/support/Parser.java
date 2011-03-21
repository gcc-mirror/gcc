/* Parser.java -- HTML parser.
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


package gnu.javax.swing.text.html.parser.support;

import gnu.java.lang.CPStringBuilder;

import gnu.javax.swing.text.html.parser.htmlAttributeSet;
import gnu.javax.swing.text.html.parser.htmlValidator;
import gnu.javax.swing.text.html.parser.support.low.Constants;
import gnu.javax.swing.text.html.parser.support.low.ParseException;
import gnu.javax.swing.text.html.parser.support.low.ReaderTokenizer;
import gnu.javax.swing.text.html.parser.support.low.Token;
import gnu.javax.swing.text.html.parser.support.low.node;
import gnu.javax.swing.text.html.parser.support.low.pattern;

import java.io.IOException;
import java.io.Reader;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.text.ChangedCharSetException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.DTDConstants;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.Entity;
import javax.swing.text.html.parser.TagElement;

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
  extends ReaderTokenizer
  implements DTDConstants
{
  /**
   * The current html tag.
   */
  public Token hTag = new Token();

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
   * This fields has positive values in preformatted tags.
   */
  protected int preformatted = 0;

  /**
   * The set of the document tags. This field is used for supporting
   * markFirstTime().
   */
  private Set documentTags =
    new TreeSet(new Comparator()
      {
        public int compare(Object a, Object b)
        {
          return ((String) a).compareToIgnoreCase((String) b);
        }
      }
               );

  /**
  * The buffer to collect the incremental output like text or coment.
  */
  private final StringBuffer buffer = new StringBuffer();

  /**
   * The buffer to store the document title.
   */
  private final StringBuffer title = new StringBuffer();

  /**
   * The current token.
   */
  private Token t;

  /**
   * True means that the 'title' tag of this document has
   * already been handled.
   */
  private boolean titleHandled;

  /**
   * True means that the 'title' tag is currently open and all
   * text is also added to the title buffer.
   */
  private boolean titleOpen;

  /**
   * The attributes of the current HTML element.
   * Package-private to avoid an accessor method.
   */
  htmlAttributeSet attributes =
    htmlAttributeSet.EMPTY_HTML_ATTRIBUTE_SET;

  /**
   * The validator, controlling the forcible closing of the tags that
   * (in accordance to dtd) are not allowed in the current context.
   */
  private htmlValidator validator;

  /**
   * Provides the default values for parameters in the case when these
   * values are defined in the DTD.
   */
  private parameterDefaulter defaulter;

  /**
   * The text pre-processor for handling line ends and tabs.
   */
  private textPreProcessor textProcessor = new textPreProcessor();

  /**
   * Creates a new Parser that uses the given
   * {@link javax.swing.text.html.parser.DTD }. The only standard way
   * to get an instance of DTD is to construct it manually, filling in
   * all required fields.
   * @param a_dtd The DTD to use. The parser behaviour after passing null
   * as an argument is not documented and may vary between implementations.
   */
  public Parser(DTD a_dtd)
  {
    if (a_dtd == null)
      dtd = gnu.javax.swing.text.html.parser.HTML_401F.getInstance();
    else
      dtd = a_dtd;

    defaulter = new parameterDefaulter(dtd);

    validator =
      new htmlValidator(dtd)
        {
          /**
           * Handles the error message. This method must be overridden to pass
           * the message where required.
           * @param msg The message text.
           */
          protected void s_error(String msg)
          {
            error(msg);
          }

          /**
           * The method is called when the tag validator decides to close the
           * tag on its own initiative. After reaching the end of stream,
           * The tag validator closes all unclosed elements that are required
           * to have the end (closing) tag.
           *
           * @param tElement The tag being fictionally (forcibly) closed.
           */
          protected void handleSupposedEndTag(Element tElement)
          {
            // The tag is cloned as the original tElement is the
            // element from the starting tag - may be accidently used
            // somewhere else.
            TagElement tag = makeTag(tElement, true);
            _handleEndTag_remaining(tag);
          }

          /**
           * The method is called when the the tag validator decides to open
           * the new tag on its own initiative. The tags, opened in this
           * way, are HTML, HEAD and BODY. The attribute set is temporary
           * assigned to the empty one, the previous value is
           * restored before return.
           *
           * @param tElement The tag being fictionally (forcibly) closed.
           */
          protected void handleSupposedStartTag(Element tElement)
          {
            TagElement tag = makeTag(tElement, true);
            htmlAttributeSet were = attributes;
            attributes = htmlAttributeSet.EMPTY_HTML_ATTRIBUTE_SET;
            _handleStartTag(tag);
            attributes = were;
          }
        };
  }

  /**
   * Get the attributes of the current tag.
   * @return The attribute set, representing the attributes of the current tag.
   */
  public SimpleAttributeSet getAttributes()
  {
    return new SimpleAttributeSet(attributes);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * delegates the call to handleError, also providing the current line.
   */
  public void error(String msg)
  {
    error(msg, getTokenAhead());
  }

  public void error(String msg, Token atToken)
  {
    if (atToken != null)
      handleError(atToken.where.beginLine,
                  msg + ": line " + atToken.where.beginLine +
                  ", absolute pos " + atToken.where.startPosition
                 );
    else
      handleError(0, msg);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * delegates the call to error (parm1+": '"+parm2+"'").
   */
  public void error(String msg, String invalid)
  {
    error(msg + ": '" + invalid + "'");
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * delegates the call to error (parm1+" "+ parm2+" "+ parm3).
   */
  public void error(String parm1, String parm2, String parm3)
  {
    error(parm1 + " " + parm2 + " " + parm3);
  }

  /**
   * Invokes the error handler. The default method in this implementation
   * delegates the call to error (parm1+" "+ parm2+" "+ parm3+" "+ parm4).
   */
  public void error(String parm1, String parm2, String parm3, String parm4)
  {
    error(parm1 + " " + parm2 + " " + parm3 + " " + parm4);
  }

  public void flushAttributes()
  {
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
    reset(reader);
    restart();
    try
      {
        parseDocument();
        validator.closeAll();
      }
    catch (ParseException ex)
      {
        if (ex != null)
          {
            error("Unable to continue parsing the document", ex.getMessage());

            Throwable cause = ex.getCause();
            if (cause instanceof IOException)
              throw (IOException) cause;
          }
      }
  }

  /**
   * Parses DTD markup declaration. Currently returns null without action.
   * @return null.
   * @throws IOException
   */
  public String parseDTDMarkup()
                        throws IOException
  {
    return null;
  }

  /**
   * Parse SGML insertion ( &lt;! ... &gt; ). When the
   * the SGML insertion is found, this method is called, passing
   * SGML in the string buffer as a parameter. The default method
   * returns false without action and can be overridden to
   * implement user - defined SGML support.
   * <p>
   * If you need more information about SGML insertions in HTML documents,
   * the author suggests to read SGML tutorial on
   * {@link http://www.w3.org/TR/WD-html40-970708/intro/sgmltut.html}.
   * We also recommend Goldfarb C.F (1991) <i>The SGML Handbook</i>,
   * Oxford University Press, 688 p, ISBN: 0198537379.
   * </p>
   * @param strBuff
   * @return true if this is a valid DTD markup declaration.
   * @throws IOException
   */
  public boolean parseMarkupDeclarations(StringBuffer strBuff)
                                  throws IOException
  {
    return false;
  }

  /**
   * Get the first line of the last parsed token.
   */
  protected int getCurrentLine()
  {
    return hTag.where.beginLine;
  }

  /**
   * Read parseable character data, add to buffer.
   * @param clearBuffer If true, buffer if filled by CDATA section,
   * otherwise the section is appended to the existing content of the
   * buffer.
   *
   * @throws ParseException
   */
  protected void CDATA(boolean clearBuffer)
                throws ParseException
  {
    Token start = hTag = getTokenAhead();

    if (clearBuffer)
      buffer.setLength(0);

    // Handle expected EOF.
    if (start.kind == EOF)
      return;

    read:
    while (true)
      {
        t = getTokenAhead();
        if (t.kind == EOF)
          {
            error("unexpected eof", t);
            break read;
          }
        else if (t.kind == BEGIN)
          break read;
        else if (t.kind == Constants.ENTITY)
          {
            resolveAndAppendEntity(t);
            getNextToken();
          }
        else
          {
            append(t);
            getNextToken();
          }
      }
    hTag = new Token(start, getTokenAhead(0));
    if (buffer.length() != 0)
      _handleText();
  }

  /**
  * Process Comment. This method skips till --> without
  * taking SGML constructs into consideration.  The supported SGML
  * constructs are handled separately.
  */
  protected void Comment()
                  throws ParseException
  {
    buffer.setLength(0);

    Token start = hTag = mustBe(BEGIN);
    optional(WS);
    mustBe(EXCLAMATION);
    optional(WS);
    mustBe(DOUBLE_DASH);

    Token t;
    Token last;

    comment:
    while (true)
      {
        t = getTokenAhead();
        if (t.kind == EOF)
          {
            handleEOFInComment();
            last = t;
            break comment;
          }
        else if (COMMENT_END.matches(this))
          {
            mustBe(DOUBLE_DASH);
            optional(WS);
            last = mustBe(END);
            break comment;
          }
        else if (COMMENT_TRIPLEDASH_END.matches(this))
          {
            mustBe(DOUBLE_DASH);
            t = mustBe(NUMTOKEN);
            if (t.getImage().equals("-"))
              {
                append(t);
                last = mustBe(END);
                break comment;
              }
            else
              {
                buffer.append("--");
                append(t);
                t = getTokenAhead();
              }
          }
        else
        /* The lllll-- can match as NUMTOKEN */
        if ((t.getImage().endsWith("--")) &&
            (
              getTokenAhead(1).kind == END ||
              (getTokenAhead(1).kind == WS && getTokenAhead(2).kind == END)
            )
           )
          {
            buffer.append(t.getImage().substring(0, t.getImage().length() - 2));

            /* Skip the closing > that we have already checked. */
            last = mustBe(t.kind);
            break comment;
          }
        else
          append(t);
        mustBe(t.kind);
      }
    hTag = new Token(start, last);

    // Consume any whitespace immediately following a comment.
    optional(WS);
    handleComment();
  }

  /**
  * Read a script. The text, returned without any changes,
  * is terminated only by the closing tag SCRIPT.
  */
  protected void Script()
                 throws ParseException
  {
    Token name;

    Token start = hTag = mustBe(BEGIN);
    optional(WS);

    name = mustBe(SCRIPT);

    optional(WS);

    restOfTag(false, name, start);

    buffer.setLength(0);

    while (!SCRIPT_CLOSE.matches(this))
      {
        append(getNextToken());
      }

    consume(SCRIPT_CLOSE);

    _handleText();

    endTag(false);
    _handleEndTag(makeTagElement(name.getImage(), false));
  }

  /**
  * Process SGML insertion that is not a comment.
  */
  protected void Sgml()
               throws ParseException
  {
    if (COMMENT_OPEN.matches(this))
      Comment();
    else // skip till ">"
      {
        Token start = hTag = mustBe(BEGIN);
        optional(WS);
        mustBe(EXCLAMATION);

        buffer.setLength(0);
        read:
        while (true)
          {
            t = getNextToken();
            if (t.kind == Constants.ENTITY)
              {
                resolveAndAppendEntity(t);
              }
            else if (t.kind == EOF)
              {
                error("unexpected eof", t);
                break read;
              }
            else if (t.kind == END)
              break read;
            else
              append(t);
          }

        try
          {
            parseMarkupDeclarations(buffer);
          }
        catch (IOException ex)
          {
            error("Unable to parse SGML insertion: '" + buffer + "'",
                  new Token(start, t)
                 );
          }
      }
    // Consume any whitespace that follows the Sgml insertion.
    optional(WS);
  }

  /**
  * Read a style definition. The text, returned without any changes,
  * is terminated only by the closing tag STYLE.
  */
  protected void Style()
                throws ParseException
  {
    Token name;

    Token start = hTag = mustBe(BEGIN);
    optional(WS);

    name = mustBe(STYLE);

    optional(WS);

    restOfTag(false, name, start);

    buffer.setLength(0);

    while (!STYLE_CLOSE.matches(this))
      {
        append(getNextToken());
      }

    consume(STYLE_CLOSE);

    _handleText();

    endTag(false);
    _handleEndTag(makeTagElement(name.getImage(), false));
  }

  /**
   * Read a html tag.
   */
  protected void Tag()
              throws ParseException
  {
    mark(true);

    boolean closing = false;
    Token name;
    Token start = hTag = mustBe(BEGIN);

    optional(WS);
    name = getNextToken();
    optional(WS);

    if (name.kind == SLASH)
      {
        closing = true;
        name = getNextToken();
      }

    restOfTag(closing, name, start);
  }

  /**
   * A hook, for operations, preceeding call to handleText.
   * Handle text in a string buffer.
   * In non - preformatted mode, all line breaks immediately following the
   * start tag and immediately before an end tag is discarded,
   * \r, \n and \t are replaced by spaces, multiple space are replaced
   * by the single one and the result is  moved into array,
   * passing it  to handleText().
   */
  protected void _handleText()
  {
    char[] text;

    if (preformatted > 0)
      text = textProcessor.preprocessPreformatted(buffer);
    else
      text = textProcessor.preprocess(buffer);

    if (text != null && text.length > 0
        // According to the specs we need to discard whitespace immediately
        // before a closing tag.
        && (text.length > 1 || text[0] != ' ' || ! TAG_CLOSE.matches(this)))
      {
        TagElement pcdata = new TagElement(dtd.getElement("#pcdata"));
        attributes = htmlAttributeSet.EMPTY_HTML_ATTRIBUTE_SET;
        _handleEmptyTag(pcdata);

        handleText(text);
        if (titleOpen)
          title.append(text);
      }
  }

  /**
   * Add the image of this token to the buffer.
   * @param t A token to append.
   */
  protected final void append(Token t)
  {
    if (t.kind != EOF)
      t.appendTo(buffer);
  }

  /**
   * Consume pattern that must match.
   * @param p A pattern to consume.
   */
  protected final void consume(pattern p)
  {
    node n;
    for (int i = 0; i < p.nodes.length; i++)
      {
        n = p.nodes [ i ];
        if (n.optional)
          optional(n.kind);
        else
          mustBe(n.kind);
      }
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
  }

  /**
   * Handle HTML comment. The default method returns without action.
   * @param comment
   */
  protected void handleComment(char[] comment)
  {
  }

  /**
   * This is additionally called in when the HTML content terminates
   * without closing the HTML comment. This can only happen if the
   * HTML document contains errors (for example, the closing --;gt is
   * missing.
   */
  protected void handleEOFInComment()
  {
    error("Unclosed comment");
  }

  /**
   * Handle the tag with no content, like &lt;br&gt;. The method is
   * called for the elements that, in accordance with the current DTD,
   * has an empty content.
   * @param tag The tag being handled.
   * @throws javax.swing.text.ChangedCharSetException
   */
  protected void handleEmptyTag(TagElement tag)
                         throws javax.swing.text.ChangedCharSetException
  {
  }

  /**
   * The method is called when the HTML closing tag ((like &lt;/table&gt;)
   * is found or if the parser concludes that the one should be present
   * in the current position.
   * @param tag The tag
   */
  protected void handleEndTag(TagElement tag)
  {
  }

  /* Handle error that has occured in the given line. */
  protected void handleError(int line, String message)
  {
  }

  /**
   * The method is called when the HTML opening tag ((like &lt;table&gt;)
   * is found or if the parser concludes that the one should be present
   * in the current position.
   * @param tag The tag
   */
  protected void handleStartTag(TagElement tag)
  {
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
  }

  /**
   * Constructs the tag from the given element. In this implementation,
   * this is defined, but never called.
   * @return the tag
   */
  protected TagElement makeTag(Element element)
  {
    return makeTag(element, false);
  }

  /**
   * Constructs the tag from the given element.
   * @param the tag base {@link javax.swing.text.html.parser.Element}
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
  }

  /**
   * Consume the token that was checked before and hence MUST be present.
   * @param kind The kind of token to consume.
   */
  protected Token mustBe(int kind)
  {
    if (getTokenAhead().kind == kind)
      return getNextToken();
    else
      {
        String ei = "";
        if (kind < 1000)
          ei = " ('" + (char) kind + "') ";
        throw new AssertionError("The token of kind " + kind + ei +
                                 " MUST be here,"
                                );
      }
  }

  /**
   * Handle attribute without value. The default method uses
   * the only allowed attribute value from DTD.
   * If the attribute is unknown or allows several values,
   * the HTML.NULL_ATTRIBUTE_VALUE is used. The attribute with
   * this value is added to the attribute set.
   * @param element The name of element.
   * @param attribute The name of attribute without value.
   */
  protected void noValueAttribute(String element, String attribute)
  {
    Object value = HTML.NULL_ATTRIBUTE_VALUE;

    Element e = dtd.elementHash.get(element.toLowerCase());
    if (e != null)
      {
        AttributeList attr = e.getAttribute(attribute);
        if (attr != null)
          {
            Vector values = attr.values;
            if (values != null && values.size() == 1)
              value = values.get(0);
          }
      }
    attributes.addAttribute(attribute, value);
  }

  /**
   * Consume the optional token, if present.
   * @param kind The kind of token to consume.
   */
  protected Token optional(int kind)
  {
    if (getTokenAhead().kind == kind)
      return getNextToken();
    else
      return null;
  }

  /** Parse the html document. */
  protected void parseDocument()
                        throws ParseException
  {
    // Read up any initial whitespace.
    optional(WS);
    while (getTokenAhead().kind != EOF)
      {
        advanced = false;
        if (TAG.matches(this))
          Tag();
        else if (COMMENT_OPEN.matches(this))
          Comment();
        else if (STYLE_OPEN.matches(this))
          Style();
        else if (SCRIPT_OPEN.matches(this))
          Script();
        else if (SGML.matches(this))
          Sgml();
        else
          CDATA(true);

        // Surely HTML error, treat as a text.
        if (!advanced)
          {
            Token wrong = getNextToken();
            error("unexpected '" + wrong.getImage() + "'", wrong);
            buffer.setLength(0);
            buffer.append(wrong.getImage());
            _handleText();
          }
      }
  }

  /**
   * Read the element attributes, adding them into attribute set.
   * @param element The element name (needed to access attribute
   * information in dtd).
   */
  protected void readAttributes(String element)
  {
    Token name;
    Token value;
    Token next;
    String attrValue;

    attributes = new htmlAttributeSet();

    optional(WS);

    attributeReading:
      while (getTokenAhead().kind == NUMTOKEN)
      {
        name = getNextToken();
        optional(WS);

        next = getTokenAhead();
        if (next.kind == EQ)
          {
            mustBe(EQ);
            optional(WS);

            next = getNextToken();

            switch (next.kind)
              {
              case QUOT:

                // read "quoted" attribute.
                buffer.setLength(0);
                readTillTokenE(QUOT);
                attrValue = buffer.toString();
                break;

              case AP:

                // read 'quoted' attribute.
                buffer.setLength(0);
                readTillTokenE(AP);
                attrValue = buffer.toString();
                break;

              // read unquoted attribute.
              case NUMTOKEN:
                value = next;
                optional(WS);

                // Check maybe the opening quote is missing.
                next = getTokenAhead();
                if (bQUOTING.get(next.kind))
                  {
                    hTag = next;
                    error("The value without opening quote is closed with '"
                          + next.getImage() + "'");
                    attrValue = value.getImage();
                  }
                else if (next.kind == SLASH || next.kind == OTHER)
                // The slash and other characters (like %) in this context is
                // treated as the ordinary
                // character, not as a token. The character may be part of
                // the unquoted URL.
                  {
                    CPStringBuilder image = new CPStringBuilder(value.getImage());
                    while (next.kind == NUMTOKEN || next.kind == SLASH
                           || next.kind == OTHER)
                      {
                        image.append(getNextToken().getImage());
                        next = getTokenAhead();
                      }
                    attrValue = image.toString();
                  }
                else
                  attrValue = value.getImage();
                break;

              case SLASH:
                value = next;
                optional(WS);

                // Check maybe the opening quote is missing.
                next = getTokenAhead();
                if (bQUOTING.get(next.kind))
                  {
                    hTag = next;
                    error("The value without opening quote is closed with '"
                          + next.getImage() + "'");
                    attrValue = value.getImage();
                  }
                else if (next.kind == NUMTOKEN || next.kind == SLASH)
                // The slash in this context is treated as the ordinary
                // character, not as a token. The slash may be part of
                // the unquoted URL.
                  {
                    CPStringBuilder image = new CPStringBuilder(value.getImage());
                    while (next.kind == NUMTOKEN || next.kind == SLASH)
                      {
                        image.append(getNextToken().getImage());
                        next = getTokenAhead();
                      }
                    attrValue = image.toString();
                  }
                else
                  attrValue = value.getImage();
                break;
              default:
                break attributeReading;
              }
            attributes.addAttribute(name.getImage(), attrValue);
            optional(WS);
          }
        else
          // The '=' is missing: attribute without value.
          {
            noValueAttribute(element, name.getImage());
          }
      }
  }

  /**
   * Return string, corresponding the given named entity. The name is passed
   * with the preceeding &, but without the ending semicolon.
   */
  protected String resolveNamedEntity(final String a_tag)
  {
    // Discard &
    if (!a_tag.startsWith("&"))
      throw new AssertionError("Named entity " + a_tag +
                               " must start witn '&'."
                              );

    String tag = a_tag.substring(1);

    try
      {
        Entity entity = dtd.getEntity(tag);
        if (entity != null)
          return entity.getString();

        entity = dtd.getEntity(tag.toLowerCase());

        if (entity != null)
          {
            error("The name of this entity should be in lowercase", a_tag);
            return entity.getString();
          }
      }
    catch (IndexOutOfBoundsException ibx)
      {
        /* The error will be reported. */
      }

    error("Unknown named entity", a_tag);
    return a_tag;
  }

  /**
   * Return char, corresponding the given numeric entity.
   * The name is passed with the preceeding &#, but without
   * the ending semicolon.
   */
  protected char resolveNumericEntity(final String a_tag)
  {
    // Discard &#
    if (!a_tag.startsWith("&#"))
      throw new AssertionError("Numeric entity " + a_tag +
                               " must start witn '&#'."
                              );

    String tag = a_tag.substring(2);

    try
      {
        // Determine the encoding type:
        char cx = tag.charAt(0);
        if (cx == 'x' || cx == 'X') // Hexadecimal &#Xnnn;

          return (char) Integer.parseInt(tag.substring(1), 16);

        return (char) Integer.parseInt(tag);
      }

    /* The error will be reported. */
    catch (NumberFormatException nex)
      {
      }
    catch (IndexOutOfBoundsException ix)
      {
      }

    error("Invalid numeric entity", a_tag);
    return '?';
  }

  /**
   * Reset all fields into the intial default state, preparing the
   * parset for parsing the next document.
   */
  protected void restart()
  {
    documentTags.clear();
    titleHandled = false;
    titleOpen = false;
    buffer.setLength(0);
    title.setLength(0);
    validator.restart();
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
  }

  /**
   * Handle a complete element, when the tag content is already present in the
   * buffer and both starting and heading tags behind. This is called
   * in the case when the tag text must not be parsed for the nested
   * elements (elements STYLE and SCRIPT).
   */
  private void _handleCompleteElement(TagElement tag)
  {
    _handleStartTag(tag);

    // Suppress inclusion of the SCRIPT ans STYLE texts into the title.
    HTML.Tag h = tag.getHTMLTag();
    if (h == HTML.Tag.SCRIPT || h == HTML.Tag.STYLE)
      {
        boolean tmp = titleOpen;
        titleOpen = false;
        _handleText();
        titleOpen = tmp;
      }
    else
      _handleText();

    _handleEndTag(tag);
  }

  /**
   * A hooks for operations, preceeding call to handleEmptyTag().
   * Handle the tag with no content, like &lt;br&gt;. As no any
   * nested tags are expected, the tag validator is not involved.
   * @param tag The tag being handled.
   */
  private void _handleEmptyTag(TagElement tag)
  {
    try
      {
        validator.validateTag(tag, attributes);
        handleEmptyTag(tag);
        HTML.Tag h = tag.getHTMLTag();
        // When a block tag is closed, consume whitespace that follows after
        // it.
        // For some unknown reason a FRAME tag is not treated as block element.
        // However in this case it should be treated as such.
        if (isBlock(h))
          optional(WS);
      }
    catch (ChangedCharSetException ex)
      {
        error("Changed charset exception:", ex.getMessage());
      }
  }

  /**
   * A hooks for operations, preceeding call to handleEndTag().
   * The method is called when the HTML closing tag
   * is found. Calls handleTitle after closing the 'title' tag.
   * @param tag The tag
   */
  private void _handleEndTag(TagElement tag)
  {
    if (validator.closeTag(tag))
       _handleEndTag_remaining(tag);
  }

  /**
   * Actions that are also required if the closing action was
   * initiated by the tag validator.
   * Package-private to avoid an accessor method.
   */
  void _handleEndTag_remaining(TagElement tag)
  {
    HTML.Tag h = tag.getHTMLTag();

    handleEndTag(tag);
    endTag(tag.fictional());

    if (h.isPreformatted())
      preformatted--;
    if (preformatted < 0)
      preformatted = 0;

    // When a block tag is closed, consume whitespace that follows after
    // it.
    if (isBlock(h))
      optional(WS);

    if (h == HTML.Tag.TITLE)
      {
        titleOpen = false;
        titleHandled = true;

        char[] a = new char[ title.length() ];
        title.getChars(0, a.length, a, 0);
        handleTitle(a);
      }
  }

  /**
   * A hooks for operations, preceeding call to handleStartTag().
   * The method is called when the HTML opening tag ((like &lt;table&gt;)
   * is found.
   * Package-private to avoid an accessor method.
   * @param tag The tag
   */
  void _handleStartTag(TagElement tag)
  {
    validator.openTag(tag, attributes);
    startingTag(tag);
    handleStartTag(tag);

    HTML.Tag h = tag.getHTMLTag();

    if (isBlock(h))
      optional(WS);

    if (h.isPreformatted())
      preformatted++;

    if (h == HTML.Tag.TITLE)
      {
        if (titleHandled)
          error("Repetetive <TITLE> tag");
        titleOpen = true;
        titleHandled = false;
      }
  }

  /**
   * Resume parsing after heavy errors in HTML tag structure.
   * @throws ParseException
   */
  private void forciblyCloseTheTag()
                            throws ParseException
  {
    int closeAt = 0;
    buffer.setLength(0);

    ahead:
    for (int i = 1; i < 100; i++)
      {
        t = getTokenAhead(i - 1);
        if (t.kind == EOF || t.kind == BEGIN)
          break ahead;
        if (t.kind == END)
          {
            /* Closing '>' found. */
            closeAt = i;
            break ahead;
          }
      }
    if (closeAt > 0)
      {
        buffer.append("Ignoring '");
        for (int i = 1; i <= closeAt; i++)
          {
            t = getNextToken();
            append(t);
          }
        buffer.append('\'');
        error(buffer.toString());
      }
  }

  /**
   * Handle comment in string buffer. You can avoid allocating a char
   * array each time by processing your comment directly here.
   */
  private void handleComment()
  {
    char[] a = new char[ buffer.length() ];
    buffer.getChars(0, a.length, a, 0);
    handleComment(a);
  }

  private TagElement makeTagElement(String name, boolean isSupposed)
  {
    Element e = dtd.elementHash.get(name.toLowerCase());
    if (e == null)
      {
        error("Unknown tag <" + name + ">");
        e = dtd.getElement(name);
        e.name = name.toUpperCase();
        e.index = -1;
      }

    if (!documentTags.contains(e.name))
      {
        markFirstTime(e);
        documentTags.add(e.name);
      }

    return makeTag(e, isSupposed);
  }

  /**
   * Read till the given token, resolving entities. Consume the given
   * token without adding it to buffer.
   * @param till The token to read till
   * @throws ParseException
   */
  private void readTillTokenE(int till)
                       throws ParseException
  {
    buffer.setLength(0);
    read:
    while (true)
      {
        t = getNextToken();
        if (t.kind == Constants.ENTITY)
          {
            resolveAndAppendEntity(t);
          }
        else if (t.kind == EOF)
          {
            error("unexpected eof", t);
            break read;
          }
        else if (t.kind == till)
          break read;
        else if (t.kind == WS)
          {
            // Processing whitespace in accordance with CDATA rules:
            String s = t.getImage();
            char c;
            for (int i = 0; i < s.length(); i++)
              {
                c = s.charAt(i);
                if (c == '\r')
                  buffer.append(' '); // CR replaced by space
                else if (c == '\n')
                  { /* LF ignored */ }
                else if (c == '\t')
                  buffer.append(' '); // Tab replaced by space
                else
                  buffer.append(c);
              }
          }
        else
          append(t);
      }
  }

  /**
   * Resolve the entity and append it to the end of buffer.
   * @param entity
   */
  private void resolveAndAppendEntity(Token entity)
  {
    switch (entity.category)
      {
        case ENTITY_NAMED :
          buffer.append(resolveNamedEntity(entity.getImage()));
          break;

        case ENTITY_NUMERIC :
          buffer.append(resolveNumericEntity(entity.getImage()));
          break;

        default :
          throw new AssertionError("Invalid entity category " +
                                   entity.category
                                  );
      }
  }

  /**
   * Handle the remaining of HTML tags. This is a common end for
   * TAG, SCRIPT and STYLE.
   * @param closing True for closing tags ( &lt;/TAG&gt; ).
   * @param name Name of element
   * @param start Token where element has started
   * @throws ParseException
   */
  private void restOfTag(boolean closing, Token name, Token start)
                  throws ParseException
  {
    boolean end = false;
    Token next;

    optional(WS);

    readAttributes(name.getImage());

    optional(WS);

    next = getTokenAhead();
    if (next.kind == END)
      {
        mustBe(END);
        end = true;
      }

    hTag = new Token(start, next);

    if (!end)
      {
        // The tag body contains errors. If additionally the tag
        // name is not valid, this construction is treated as text.
        if (dtd.elementHash.get(name.getImage().toLowerCase()) == null &&
            backupMode
           )
          {
            error("Errors in tag body and unknown tag name. " +
                  "Treating the tag as a text."
                 );
            reset();

            hTag = mustBe(BEGIN);
            buffer.setLength(0);
            buffer.append(hTag.getImage());
            CDATA(false);
            return;
          }
        else
          {
            error("Forcibly closing invalid parameter list");
            forciblyCloseTheTag();
          }
      }

    if (closing)
      {
        endTag(false);
        _handleEndTag(makeTagElement(name.getImage(), false));
      }
    else
      {
        TagElement te = makeTagElement(name.getImage(), false);
        if (te.getElement().type == DTDConstants.EMPTY)
          _handleEmptyTag(te);
        else
          {
            // According to the specs we need to consume whitespace following
            // immediately after a opening tag.
            optional(WS);
            _handleStartTag(te);
          }
      }
  }

  /**
   * This should fire additional actions in response to the
   * ChangedCharSetException.  The current implementation
   * does nothing.
   * @param tag
   */
  private void startingTag(TagElement tag)
  {
    try
      {
        startTag(tag);
      }
    catch (ChangedCharSetException cax)
      {
        error("Invalid change of charset");
      }
  }

  private void ws_error()
  {
    error("Whitespace here is not permitted");
  }

  /**
   * Returns true when the specified tag should be considered a block tag
   * wrt whitespace handling. We need this special handling, since there
   * are a couple of tags that we must treat as block tags but which aren't
   * officially block tags.
   *
   * @param tag the tag to check
   * @return true when the specified tag should be considered a block tag
   *         wrt whitespace handling
   */
  private boolean isBlock(HTML.Tag tag)
  {
    return tag.isBlock() || tag == HTML.Tag.STYLE || tag == HTML.Tag.FRAME;
  }
}
