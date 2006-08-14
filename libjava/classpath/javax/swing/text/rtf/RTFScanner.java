/* RTFScanner.java --
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


package javax.swing.text.rtf;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

/**
 * Provides a scanner that scans an {@link InputStream} for tokens of the
 * RTF syntax.
 *
 * This scanner is based upon the RTF specification 1.6
 * available at:
 * 
 * <a
 * href="http://msdn.microsoft.com/library/en-us/dnrtfspec/html/rtfspec.asp">
 * RTF specification at MSDN</a>
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
class RTFScanner
{

  /**
   * The reader from which we read the RTF data.
   */
  private Reader in;

  /**
   * This is used to constuct strings from the read in chars.
   */
  private StringBuffer buffer;

  /**
   * Lookahead token.
   */
  private Token lastToken;

  /**
   * Constructs a new RTFScanner without initializing the {@link Reader}.
   */
  private RTFScanner()
  {
    buffer = new StringBuffer();
  }

  /**
   * Constructs a new RTFScanner for the given {@link InputStream}.
   * The stream is wrapped into an {@link InputStreamReader} and if it's
   * not yet buffered then the Reader is wrapped in a {@link BufferedReader}
   *
   * @param stream the {@link InputStream} to read RTF data from
   */
  public RTFScanner(InputStream stream)
  {
    this();
    InputStreamReader reader = new InputStreamReader(stream);
    in = new BufferedReader(reader);
  }

  /**
   * Constructs a new RTFScanner for the given {@link Reader}.
   *
   * If the reader is not an instance of {@link BufferedReader} then it
   * is wrapped into a BufferedReader.
   *
   * @param reader the {@link BufferedReader} to read RTF data from
   */
  public RTFScanner(Reader reader)
  {
    this();
    if (reader instanceof BufferedReader)
      {
        in = reader;
      }
    else
      {
        in = new BufferedReader(reader);
      }
  }

  /**
   * Reads in the next {@link Token} from the stream.
   *
   * @return the read {@link Token}
   *
   * @throws IOException if the underlying stream has problems
   */
  private Token readTokenImpl()
    throws IOException
  {
    Token token = null;

    int c = in.read();
    switch(c)
      {
      case -1:
        token = new Token(Token.EOF);
        break;

      case '{':
        token = new Token(Token.LCURLY);
        break;

      case '}':
        token = new Token(Token.RCURLY);
        break;

      case '\\':
        buffer.delete(0, buffer.length());
        buffer.append((char) c);
        token = readControlWord();
        break;

      default:
        buffer.delete(0, buffer.length());
        buffer.append((char) c);
        token = readText();
        break;
      }

    return token;
  }

  Token peekToken()
    throws IOException
  {
    lastToken = readTokenImpl();
    return lastToken;
  }

  Token readToken()
    throws IOException
  {
    Token token;
    if (lastToken != null)
      {
        token = lastToken;
        lastToken = null;
      }
    else
      token = readTokenImpl();
    return token;
  }

  /**
   * Reads in a control word and optional parameter.
   *
   * @return the read in control word as {@link ControlWordToken}
   *
   * @throws IOException if the underlying stream has problems
   */
  private Token readControlWord()
    throws IOException
  {
    // this flag indicates if we are still reading the name or are already
    // in the parameter
    boolean readingName = true;
    String name = null;
    String param = null;

    while (true)
      {
        in.mark(1);
        int c = in.read();

        // check for 'a'..'z'
        if (readingName && (c >= 'a') && (c <= 'z'))
          {
            buffer.append((char) c);
          }
        else if ((c >= '0') && (c <= '9'))
          {
            // if the last char was in the name, then finish reading the name
            if (readingName)
              {
                name = buffer.toString();
                buffer.delete(0, buffer.length());
                readingName = false;
              }
            buffer.append((char) c);
          }
        else
          {
            // if we were in the name, then finish this
            if (readingName)
              {
                name = buffer.toString();
              }
            // otherwise finish the parameter
            else
              {
                param = buffer.toString();
              }

            // clear up
            buffer.delete(0, buffer.length());
            // reset input buffer to last char
            in.reset();
            // break while loop
            break;
          }
      }

    ControlWordToken token = null;

    if (param == null)
      token = new ControlWordToken(name);
    else
      token =new ControlWordToken(name, Integer.parseInt(param));

    return token;

  }

  /**
   * Reads in a block of text.
   *
   * @return the token for the text
   */
  private Token readText()
    throws IOException
  {

    boolean readingText = true;
    while (readingText)
      {
        in.mark(1);
        int c = in.read();
        switch(c)
          {
          case '\\':
          case '{':
          case '}':
          case -1:
            readingText = false;
            in.reset();
            break;

          default:
            buffer.append((char) c);
            break;
          }

      }

    String text = buffer.toString();
    Token token = new TextToken(text);

    buffer.delete(0, buffer.length());

    return token;

  }
}
