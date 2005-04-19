/* RTFParser.java --
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


package javax.swing.text.rtf;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**
 * Parses an RTF file into a {@link Document}. The parser utilizes
 * {@link RTFScanner}.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
class RTFParser
{

  /**
   * Our scanner.
   */
  private RTFScanner scanner;

  /**
   * The document into which we parse.
   */
  private Document doc;

  /**
   * The current position.
   */
  private int pos;

  /**
   * Constructs a new RTFParser for the specified document and position,
   * without initializing the scanner. This is only used internally.
   *
   * @param doc the {@link Document} into which we should parse
   * @param pos the position to start
   */
  private RTFParser(Document doc, int pos)
  {
    this.doc = doc;
    this.pos = pos;
  }

  /**
   * Constructs a new RTFParser for the specified <code>stream</code>.
   *
   * @param stream the stream from which we parse
   * @param doc the {@link Document} into which we should parse
   * @param pos the position to start
   */
  public RTFParser(InputStream stream, Document doc, int pos)
  {
    this(doc, pos);
    scanner = new RTFScanner(stream);
  }

  /**
   * Constructs a new RTFParser for the specified <code>reader</code>.
   *
   * @param reader the reader from which we parse
   * @param doc the {@link Document} into which we should parse
   * @param pos the position to start
   */
  public RTFParser(Reader reader, Document doc, int pos)
  {
    this(doc, pos);
    scanner = new RTFScanner(reader);
  }

  /**
   * Returns the {@link Document} in which we parsed the RTF data.
   *
   * @return the {@link Document} in which we parsed the RTF data
   */
  public Document getDocument()
  {
    return doc;
  }

  /**
   * Starts the parsing process.
   */
  public void parse()
    throws IOException, BadLocationException
  {
    parseFile();
  }

  /**
   * The parse rules for &lt;file&gt;.
   */
  private void parseFile()
    throws IOException, BadLocationException
  {
    Token t1 = scanner.readToken();
    if (t1.type != Token.LCURLY)
      throw new RTFParseException("expected left curly braces");

    parseHeader();
    parseDocument();
  
    Token t2 = scanner.readToken();
    if (t2.type != Token.RCURLY)
      throw new RTFParseException("expected right curly braces");

  }

  /**
   * The parse rules for &lt;header&gt;.
   *
   * TODO: implement this properly
   */
  private void parseHeader()
  //throws IOException, BadLocationException
  {
    // TODO add parse rules here
  }


  /**
   * The parse rules for &lt;document&gt;.
   *
   * TODO: implement this properly
   */
  private void parseDocument()
    throws IOException, BadLocationException
  {
    //  !!! TODO !!!
    // This simply emits every TEXT Token as text to the document
    // which is plain stupid

    boolean eof = false;

    do {
      Token token = scanner.readToken();
      switch (token.type)
        {
        case Token.TEXT:
          TextToken textToken = (TextToken) token;
          doc.insertString(pos, textToken.text, null);
          pos += textToken.text.length();
          break;
        case Token.EOF:
          eof = true;
          break;
        default:
          // FIXME
          break;
        }
    } while (!eof);

  }

}
