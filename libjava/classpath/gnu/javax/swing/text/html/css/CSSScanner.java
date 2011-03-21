/* CSSScanner.java -- A parser for CSS stylesheets
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.css;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

/**
 * A tokenizer for CSS stylesheets. This is based on the scanner definition
 * from:
 *
 * http://www.w3.org/TR/CSS21/syndata.html#tokenization
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
// TODO: Maybe implement more restrictive scanner:
// http://www.w3.org/TR/CSS21/grammar.html#q2
class CSSScanner
{

  // The tokens. This list is taken from:
  // http://www.w3.org/TR/CSS21/syndata.html#tokenization
  static final int IDENT = 1;
  static final int ATKEYWORD = 2;
  static final int STRING = 3;
  static final int INVALID = 4;
  static final int HASH = 5;
  static final int NUMBER = 6;
  static final int PERCENTAGE = 7;
  static final int DIMENSION = 8;
  static final int URI = 9;
  static final int UNICODE_RANGE = 10;
  static final int CDO = 11;
  static final int CDC = 12;
  static final int SEMICOLON = 13;
  static final int CURLY_LEFT = 14;
  static final int CURLY_RIGHT = 15;
  static final int PAREN_LEFT = 16;
  static final int PAREN_RIGHT = 17;
  static final int BRACE_LEFT = 16;
  static final int BRACE_RIGHT = 17;
  static final int S = 18;
  static final int COMMENT = 19;
  static final int FUNCTION = 20;
  static final int INCLUDES = 21;
  static final int DASHMATCH = 22;
  static final int DELIM = 23;

  // Additional tokens defined for convenience.
  static final int EOF = -1;

  /**
   * The input source.
   */
  private Reader in;

  /**
   * The parse buffer.
   */
  char[] parseBuffer;

  /**
   * The end index in the parseBuffer of the current token.
   */
  int tokenEnd;

  /**
   * The lookahead 'buffer'.
   */
  private int[] lookahead;

  CSSScanner(Reader r)
  {
    lookahead = new int[2];
    lookahead[0] = -1;
    lookahead[1] = -1;
    parseBuffer = new char[2048];
    in = r;
  }

  /**
   * Fetches the next token. The actual character data is in the parseBuffer
   * afterwards with the tokenStart at index 0 and the tokenEnd field
   * pointing to the end of the token.
   *
   * @return the next token
   */
  int nextToken()
    throws IOException
  {
    tokenEnd = 0;
    int token = -1;
    int next = read();
    if (next != -1)
      {
        switch (next)
        {
          case ';':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = SEMICOLON;
            break;
          case '{':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = CURLY_LEFT;
            break;
          case '}':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = CURLY_RIGHT;
            break;
          case '(':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = PAREN_LEFT;
            break;
          case ')':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = PAREN_RIGHT;
            break;
          case '[':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = BRACE_LEFT;
            break;
          case ']':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            token = BRACE_RIGHT;
            break;
          case '@':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            readIdent();
            token = ATKEYWORD;
            break;
          case '#':
            parseBuffer[0] = (char) next;
            tokenEnd = 1;
            readName();
            token = HASH;
            break;
          case '\'':
          case '"':
            lookahead[0] = next;
            readString();
            token = STRING;
            break;
          case ' ':
          case '\t':
          case '\r':
          case '\n':
          case '\f':
            lookahead[0] = next;
            readWhitespace();
            token = S;
            break;
            // FIXME: Detecting an URI involves several characters lookahead.
//          case 'u':
//            lookahead[0] = ch;
//            readURI();
//            token = URI;
//            break;
          case '<':
            parseBuffer[0] = (char) next;
            parseBuffer[1] = (char) read();
            parseBuffer[2] = (char) read();
            parseBuffer[3] = (char) read();
            if (parseBuffer[1] == '!' && parseBuffer[2] == '-'
              && parseBuffer[3] == '-')
              {
                token = CDO;
                tokenEnd = 4;
              }
            else
              throw new CSSLexicalException("expected CDO token");
            break;
          case '/':
            lookahead[0] = next;
            readComment();
            token = COMMENT;
            break;
          case '~':
            parseBuffer[0] = (char) next;
            parseBuffer[1] = (char) read();
            if (parseBuffer[1] == '=')
              token = INCLUDES;
            else
              throw new CSSLexicalException("expected INCLUDES token");
            break;
          case '|':
            parseBuffer[0] = (char) next;
            parseBuffer[1] = (char) read();
            if (parseBuffer[1] == '=')
              token = DASHMATCH;
            else
              throw new CSSLexicalException("expected DASHMATCH token");
            break;
          case '-':
            int ch2 = read();
            if (ch2 == '-')
              {
                int ch3 = read();
                if (ch3 == '>')
                  {
                    parseBuffer[0] = (char) next;
                    parseBuffer[1] = (char) ch2;
                    parseBuffer[2] = (char) ch3;
                    tokenEnd = 3;
                    token = CDC;
                  }
                else
                  throw new CSSLexicalException("expected CDC token");
              }
            else
              {
                lookahead[0] = next;
                lookahead[1] = ch2;
                readIdent();
                int ch3 = read();
                if (ch3 == -1 || ch3 != '(')
                  {
                    lookahead[0] = ch3;
                    token = IDENT;
                  }
                else
                  {
                    parseBuffer[tokenEnd] = (char) ch3;
                    tokenEnd++;
                    token = FUNCTION;
                  }
              }
            break;
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            lookahead[0] = next;
            readNum();
            int ch3 = read();
            if (ch3 == '%')
              {
                parseBuffer[tokenEnd] = (char) ch3;
                tokenEnd++;
                token = PERCENTAGE;
              }
            else if (ch3 == -1 || (! (ch3 == '_'
                                      || (ch3 >= 'a' && ch3 <= 'z')
                                      || (ch3 >= 'A' && ch3 <= 'Z')
                                      || ch3 == '\\' || ch3 > 177)))
              {
                lookahead[0] = ch3;
                token = NUMBER;
              }
            else
              {
                lookahead[0] = ch3;
                readIdent();
                token = DIMENSION;
              }
            break;
          default:
            // Handle IDENT that don't begin with '-'.
            if (next == '_' || (next >= 'a' && next <= 'z')
                || (next >= 'A' && next <= 'Z') || next == '\\' || next > 177)
              {
                lookahead[0] = next;
                readIdent();
                int ch4 = read();
                if (ch4 == -1 || ch4 != '(')
                  {
                    lookahead[0] = ch4;
                    token = IDENT;
                  }
                else
                  {
                    parseBuffer[tokenEnd] = (char) ch4;
                    tokenEnd++;
                    token = FUNCTION;
                  }
              }
            else
              {
                parseBuffer[0] = (char) next;
                tokenEnd = 1;
                token = DELIM;
              }
          break;
        }
      }
    return token;
  }

  String currentTokenString()
  {
    return new String(parseBuffer, 0, tokenEnd);
  }

  /**
   * Reads one character from the input stream or from the lookahead
   * buffer, if it contains one character.
   *
   * @return the next character
   *
   * @throws IOException if problems occur on the input source
   */
  private int read()
    throws IOException
  {
    int ret;
    if (lookahead[0] != -1)
      {
        ret = lookahead[0];
        lookahead[0] = -1;
      }
    else if (lookahead[1] != -1)
      {
        ret = lookahead[1];
        lookahead[1] = -1;
      }
    else
      {
        ret = in.read();
      }
    return ret;
  }

  /**
   * Reads and identifier.
   *
   * @throws IOException if something goes wrong in the input source or if
   *         the lexical analyser fails to read an identifier
   */
  private void readIdent()
    throws IOException
  {
    int ch1 = read();
    // Read possibly leading '-'.
    if (ch1 == '-')
      {
        parseBuffer[tokenEnd] = (char) ch1;
        tokenEnd++;
        ch1 = read();
      }
    // What follows must be '_' or a-z or A-Z or nonascii (>177) or an
    // escape.
    if (ch1 == '_' || (ch1 >= 'a' && ch1 <= 'z')
        || (ch1 >= 'A' && ch1 <= 'Z') || ch1 > 177)
      {
        parseBuffer[tokenEnd] = (char) ch1;
        tokenEnd++;
      }
    else if (ch1 == '\\')
      {
        // Try to read an escape.
        lookahead[0] = ch1;
        readEscape();
      }
    else
      throw new CSSLexicalException("First character of identifier incorrect");

    // Read any number of [_a-zA-Z0-9-] chars.
    int ch = read();
    while (ch != -1 && (ch == '_' || ch == '-' || (ch >= 'a' && ch <= 'z')
           || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        ch = read();
      }

    // Push back last read character since it doesn't belong to the IDENT.
    lookahead[0] = ch;
  }

  /**
   * Reads an escape.
   *
   * @throws IOException if something goes wrong in the input source or if
   *         the lexical analyser fails to read an escape
   */
  private void readEscape()
    throws IOException
  {
    int ch = read();
    if (ch != -1 && ch == '\\')
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        ch = read();
        if ((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f'))
          {
            // Read unicode escape.
            // Zero to five 0-9a-f chars can follow.
            int hexcount = 0;
            ch = read();
            while (((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f'))
                   && hexcount < 5)
              {
                parseBuffer[tokenEnd] = (char) ch;
                tokenEnd++;
                hexcount++;
                ch = read();
              }
            // Now we can have a \r\n or any whitespace character following.
            if (ch == '\r')
              {
                parseBuffer[tokenEnd] = (char) ch;
                tokenEnd++;
                ch = read();
                if (ch == '\n')
                  {
                    parseBuffer[tokenEnd] = (char) ch;
                    tokenEnd++;
                  }
                else
                  {
                    lookahead[0] = ch;
                  }
              }
            else if (ch == ' ' || ch == '\n' || ch == '\f' || ch == '\t')
              {
                parseBuffer[tokenEnd] = (char) ch;
                tokenEnd++;
              }
            else
              {
                lookahead[0] = ch;
              }
          }
        else if (ch != '\n' && ch != '\r' && ch != '\f')
          {
            parseBuffer[tokenEnd] = (char) ch;
            tokenEnd++;
          }
        else
          throw new CSSLexicalException("Can't read escape");
      }
    else
      throw new CSSLexicalException("Escape must start with '\\'");

  }

  private void readName()
    throws IOException
  {
    // Read first name character.
    int ch = read();
    if (ch != -1 && (ch == '_' || ch == '-' || (ch >= 'a' && ch <= 'z')
           || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
      }
    else
      throw new CSSLexicalException("Invalid name");

    // Read any number (at least one) of [_a-zA-Z0-9-] chars.
    ch = read();
    while (ch != -1 && (ch == '_' || ch == '-' || (ch >= 'a' && ch <= 'z')
           || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        ch = read();
      }

    // Push back last read character since it doesn't belong to the IDENT.
    lookahead[0] = ch;
  }

  /**
   * Reads in a string.
   *
   * @throws IOException
   */
  private void readString()
    throws IOException
  {
    int ch1 = read();
    if (ch1 != -1 && (ch1 == '\'' || ch1 == '\"'))
      {
        parseBuffer[tokenEnd] = (char) ch1;
        tokenEnd++;

        // Read any number of chars until we hit another chc1 char.
        // Reject newlines, except if prefixed with \.
        int ch = read();
        while (ch != -1 && ch != ch1)
          {
            // Every non-newline and non-\ char should be ok.
            if (ch != '\n' && ch != '\r' && ch != '\f' && ch != '\\')
              {
                parseBuffer[tokenEnd] = (char) ch;
                tokenEnd++;
              }
            // Ok when followed by newline or as part of escape.
            else if (ch == '\\')
              {
                int ch2 = read();
                if (ch2 == '\n' || ch2 == '\r')
                  {
                    parseBuffer[tokenEnd] = (char) ch;
                    parseBuffer[tokenEnd + 1] = (char) ch2;
                    tokenEnd += 2;
                  }
                else
                  {
                    // Try to parse an escape.
                    lookahead[0] = ch;
                    lookahead[1] = ch2;
                    readEscape();
                  }
              }
            else
              throw new CSSLexicalException("Invalid string");

            ch = read();
          }
        if (ch != -1)
          {
            // Push the final char on the buffer.
            parseBuffer[tokenEnd] = (char) ch;
            tokenEnd++;
          }
        else
          throw new CSSLexicalException("Unterminated string");
      }
    else
      throw new CSSLexicalException("Invalid string");
  }

  /**
   * Reads a chunk of whitespace.
   *
   * @throws IOException
   */
  private void readWhitespace()
    throws IOException
  {
    int ch = read();
    while (ch != -1 && (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
           || ch == '\f'))
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        ch = read();
      }
    // Push back last character read.
    lookahead[0] = ch;

  }

  private void readURI()
    throws IOException
  {
    // FIXME: Implement.
  }

  /**
   * Reads a comment block.
   *
   * @throws IOException
   */
  private void readComment()
    throws IOException
  {
    // First we need a / and a *
    int ch = read();
    if (ch != -1 && ch == '/')
      {
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        ch = read();
        if (ch != -1 && ch == '*')
          {
            parseBuffer[tokenEnd] = (char) ch;
            tokenEnd++;
            ch = read();
            parseBuffer[tokenEnd] = (char) ch;
            tokenEnd++;
            boolean finished = false;
            int lastChar = ch;
            ch = read();
            while (! finished && ch != -1)
              {
                if (lastChar == '*' && ch == '/')
                  finished = true;
                parseBuffer[tokenEnd] = (char) ch;
                tokenEnd++;
                lastChar = ch;
                ch = read();
              }
          }
      }
    if (ch == -1)
      throw new CSSLexicalException("Unterminated comment");

    // Push back last character read.
    lookahead[0] = ch;
  }

  /**
   * Reads a number.
   *
   * @throws IOException
   */
  private void readNum()
    throws IOException
  {
    boolean hadDot = false;
    // First char must be number or .
    int ch = read();
    if (ch != -1 && ((ch >= '0' && ch <= '9') || ch == '.'))
      {
        if (ch == '.')
          hadDot = true;
        parseBuffer[tokenEnd] = (char) ch;
        tokenEnd++;
        // Now read in any number of digits afterwards, and maybe one dot,
        // if we hadn't one already.
        ch = read();
        while (ch != -1 && ((ch >= '0' && ch <= '9')
                            || (ch == '.' && ! hadDot)))
          {
            if (ch == '.')
              hadDot = true;
            parseBuffer[tokenEnd] = (char) ch;
            tokenEnd++;
            ch = read();
          }
      }
    else
      throw new CSSLexicalException("Invalid number");

    // Check if we haven't accidentally finished with a dot.
    if (parseBuffer[tokenEnd - 1] == '.')
      throw new CSSLexicalException("Invalid number");

    // Push back last character read.
    lookahead[0] = ch;
  }

  /**
   * For testing, we read in the default.css in javax/swing/text/html
   *
   * @param args
   */
  public static void main(String[] args)
  {
    try
      {
        String name = "/javax/swing/text/html/default.css";
        InputStream in = CSSScanner.class.getResourceAsStream(name);
        BufferedInputStream bin = new BufferedInputStream(in);
        InputStreamReader r = new InputStreamReader(bin);
        CSSScanner s = new CSSScanner(r);
        int token;
        do
          {
            token = s.nextToken();
            System.out.println("token: " + token + ": "
                               + s.currentTokenString());
          } while (token != -1);
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }
  }
}
