/* CSSParser.java -- A parser for CSS stylesheets
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
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.StringTokenizer;

/**
 * A parser for CSS stylesheets.
 *
 * This parser is based on the simple CSS grammar describe in
 *
 * http://www.w3.org/TR/CSS21/syndata.html .
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
// TODO: Maybe use more restrictive grammar:
// http://www.w3.org/TR/CSS21/grammar.html#q1
public class CSSParser
{

  /**
   * The scanner used to read the input streams into more usable tokens.
   */
  private CSSScanner scanner;

  /**
   * The parser callback.
   */
  private CSSParserCallback callback;

  /**
   * One lookahead token.
   */
  private int lookahead;

  /**
   * The parse error.
   */
  private String error;

  /**
   * Creates a new CSSParser that parses the specified input.
   *
   * @param in the source to parse
   */
  public CSSParser(Reader in, CSSParserCallback cb)
  {
    scanner = new CSSScanner(in);
    callback = cb;
    lookahead = -1;
  }

  /**
   * Parses the input source specified in the constructor.
   *
   * @throws IOException if an IO or parse error occurs
   */
  public void parse()
    throws IOException
  {
    boolean success = parseStylesheet();
    if (! success)
      {
        throw new CSSParserException(error);
      }
  }

  /**
   * Parses a stylesheet.
   *
   * @return <code>true</code> if the stylesheet could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseStylesheet()
    throws IOException
  {
    int token = peekToken();
    while (token != CSSScanner.EOF && (token == CSSScanner.CDC
           || token == CSSScanner.CDO || token == CSSScanner.S
           || parseStatement()))
      {
        if (token == CSSScanner.CDC || token == CSSScanner.CDO
            || token == CSSScanner.S)
          readToken();
        token = peekToken();
      }
    // Last token must be EOF for valid stylesheets, I'd think.
    return token == CSSScanner.EOF;
  }

  /**
   * Parses a CSS statement.
   * @return <code>true</code> if the stylesheet could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseStatement()
    throws IOException
  {
    return parseRuleset() || parseAtRule();
  }

  /**
   * Parses a CSS rule set.
   *
   * @return <code>true</code> if the ruleset could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseRuleset()
    throws IOException
  {
    StringBuilder selector = new StringBuilder();
    parseSelector(selector);
    StringTokenizer selSplitter =
      new StringTokenizer(selector.toString(), ",");
    Selector[] sels = new Selector[selSplitter.countTokens()];
    for (int i = 0; selSplitter.hasMoreTokens(); i++)
      {
        String sel = selSplitter.nextToken().trim();
        sels[i] = new Selector(sel);
      }
    callback.startStatement(sels);
    // Read any number of whitespace.
    int token;
    do
      {
        token = readToken();
      } while (token == CSSScanner.S);
    boolean ret = true;

    if (token == CSSScanner.CURLY_LEFT)
      {
        // Read any number of whitespace.
        do
          {
            token = readToken();
          } while (token == CSSScanner.S);
        lookahead = token;

        // Maybe read declaration.
        boolean decl = parseDeclaration();
        token = peekToken();
        while (token == CSSScanner.SEMICOLON)
          {
            readToken(); // Read the semicolon.
            // Read any number of whitespace.
            do
              {
                token = readToken();
              } while (token == CSSScanner.S);
            lookahead = token;

            // Maybe read declaration.
            parseDeclaration();
            token = peekToken();
          }
        if (token != CSSScanner.CURLY_RIGHT)
          {
            error = "Expected right curly brace";
            ret = false;
          }
        else
          {
            readToken();
            // Read any number of whitespace.
            do
              {
                token = readToken();
              } while (token == CSSScanner.S);
            lookahead = token;
            callback.endStatement();
          }
      }
    else
      {
        ret = false;
        error = "Expected left curly brace";
      }
    return ret;
  }

  /**
   * Parses a CSS declaration.
   *
   * @return <code>true</code> if the ruleset could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseDeclaration()
   throws IOException
  {
    // Maybe fetch one DELIM.
    int token = readToken();
    if (token == CSSScanner.DELIM)
      token = readToken();

    boolean ret = true;

    // Parse property
    String property = null;
    if (token == CSSScanner.IDENT)
      {
        property = new String(scanner.parseBuffer, 0, scanner.tokenEnd);
        // Read any number of whitespace.
        do
          {
            token = readToken();
          } while (token == CSSScanner.S);

        // Read ':'.
        if (token == CSSScanner.DELIM && scanner.parseBuffer[0] == ':')
          {
            // Read any number of whitespace.
            do
              {
                token = readToken();
              } while (token == CSSScanner.S);
            lookahead = token;

            StringBuilder value = new StringBuilder();
            if (parseValue(value))
              {
                callback.declaration(property, value.toString().trim());
              }
            else
              {
                ret = false;
                error = "Error while reading the property value";
              }
          }
        else
          {
            ret = false;
            error = "Expected colon to separate property and value";
          }

      }
    else
      {
        lookahead = token;
        ret = false;
        error = "Expected IDENT token for property";
      }
    return ret;
  }

  /**
   * Parses a property value.
   *
   * @param s the string builder to read the value into
   *
   * @return <code>true</code> if the ruleset could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseValue(StringBuilder s)
    throws IOException
  {
    // FIXME: Handle block and ATKEYWORD.
    boolean success = parseAny(s);
    while (parseAny(s))
      ;

    return success;
  }

  /**
   * Parses a selector.
   *
   * @param sel the string buffer to put the selector into
   *
   * @return <code>true</code> if the ruleset could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseSelector(StringBuilder sel)
    throws IOException
  {
    // At least one any needs to be parsed.
    boolean ret = parseAny(sel);
    if (ret)
      {
        while (parseAny(sel))
          ;
      }
    return ret;
  }

  /**
   * Parses the any rule. If s is not null, then the contents of the
   * tokens is appended verbatim.
   *
   * @param s the string builder to append to
   *
   * @return <code>true</code> if the ruleset could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseAny(StringBuilder s)
    throws IOException
  {
    int token = peekToken();
    boolean ret = false;
    if (token == CSSScanner.IDENT || token == CSSScanner.NUMBER
        || token == CSSScanner.PERCENTAGE || token == CSSScanner.DIMENSION
        || token == CSSScanner.STRING || token == CSSScanner.DELIM
        || token == CSSScanner.URI || token == CSSScanner.HASH
        || token == CSSScanner.UNICODE_RANGE || token == CSSScanner.INCLUDES
        || token == CSSScanner.DASHMATCH)
      {
        if (s != null)
          s.append(scanner.parseBuffer, 0, scanner.tokenEnd);
        readToken();
        ret = true;
      }
    else if (token == CSSScanner.FUNCTION)
      System.err.println("Implement parseAny for FUNCTION");
    else if (token == CSSScanner.PAREN_LEFT)
      System.err.println("Implement parseAny for (");
    else if (token == CSSScanner.BRACE_LEFT)
      System.err.println("Implement parseAny for [");

    // Parse any following whitespace too.
    token = peekToken();
    while (token == CSSScanner.S)
      {
        if (s != null)
          s.append(scanner.parseBuffer, 0, scanner.tokenEnd);
        readToken();
        token = peekToken();
      }
    return ret;
  }

  /**
   * Parses a CSS at-rule.
   *
   * @return <code>true</code> if the at-rule could be parsed successfully,
   *         <code>false</code> otherwise
   *
   * @throws IOException if an IO or parse error occurs
   */
  private boolean parseAtRule()
    throws IOException
  {
    // FIXME: Implement.
    return false;
  }

  /**
   * Reads the next token, and skips the comments.
   *
   * @return the next non-comment token
   */
  private int readToken()
    throws IOException
  {
    int token;
    if (lookahead == -1)
      {
        do
          {
            token = scanner.nextToken();
          } while (token == CSSScanner.COMMENT);
      }
    else
      {
        token = lookahead;
        lookahead = -1;
      }
    return token;
  }

  /**
   * Returns the next token to be read, without really reading it. The next
   * call to readToken() will return the same token again.
   *
   * @return the next token to be read, without really reading it
   */
  private int peekToken()
    throws IOException
  {
    int token;
    if (lookahead == -1)
      {
        do
          {
            token = scanner.nextToken();
          } while (token == CSSScanner.COMMENT);
        lookahead = token;
      }
    else
      token = lookahead;
    return token;
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
        InputStream in;
        if (args.length > 0)
          {
            File file = new File(args[0]);
            in = new FileInputStream(file);
          }
        else
          {
            String name = "/javax/swing/text/html/default.css";
            in = CSSScanner.class.getResourceAsStream(name);
          }
        BufferedInputStream bin = new BufferedInputStream(in);
        InputStreamReader r = new InputStreamReader(bin);
        CSSParserCallback cb = new CSSParserCallback()
        {
          public void startStatement(Selector[] selector)
          {
            System.out.print("startStatement: ");
            for (int i = 0; i < selector.length; i++)
              {
                System.out.print(selector[i]);
                if (i < selector.length - 1)
                  System.out.print(',');
                else
                  System.out.println();
              }
          }
          public void endStatement()
          {
            System.out.println("endStatement");
          }
          public void declaration(String property, String value)
          {
            System.out.println("declaration: " + property + ", " + value);
          }
        };
        CSSParser p = new CSSParser(r, cb);
        p.parse();
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }
  }

}
