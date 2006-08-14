/* ConfigFileTokenizer.java -- JAAS Login Configuration default syntax tokenizer
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


package gnu.javax.security.auth.login;

import gnu.java.security.Configuration;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.logging.Logger;

/**
 * A UTF-8 friendly, JAAS Login Module Configuration file tokenizer written in
 * the deault syntax. This class emulates, to a certain extent, the behavior of
 * a {@link java.io.StreamTokenizer} instance <code>st</code>, when set as
 * follows:
 * 
 *  <pre>
 *  st.resetSyntax();
 *  st.lowerCaseMode(false);
 *  st.slashSlashComments(true);
 *  st.slashStarComments(true);
 *  st.eolIsSignificant(false);
 *  st.wordChars('_', '_');
 *  st.wordChars('$', '$');
 *  st.wordChars('A', 'Z');
 *  st.wordChars('a', 'z');
 *  st.wordChars('0', '9');
 *  st.wordChars('.', '.');
 *  st.whitespaceChars(' ', ' ');
 *  st.whitespaceChars('\t', '\t');
 *  st.whitespaceChars('\f', '\f');
 *  st.whitespaceChars('\r', '\r');
 *  st.whitespaceChars('\n', '\n');
 *  st.quoteChar('"');
 *  st.quoteChar('\'');
 *  </pre>
 *
 * <p>The most important (negative) difference with a
 * {@link java.io.StreamTokenizer} is that this tokenizer does not properly
 * handle C++ and Java // style comments in the middle of the line. It only
 * ignores them if/when found at the start of the line.</p>  
 */
public class ConfigFileTokenizer
{
  private static final Logger log = Logger.getLogger(ConfigFileParser.class.getName());
  /** A constant indicating that the end of the stream has been read. */
  public static final int TT_EOF = -1;
  /** A constant indicating that a word token has been read. */
  public static final int TT_WORD = -3;
  /** A constant indicating that no tokens have been read yet. */
  private static final int TT_NONE = -4;

  public String sval;
  public int ttype;

  private BufferedReader br;
  boolean initialised;
  private StringBuffer sb;
  private int sbNdx;

  // Constructor(s)
  // --------------------------------------------------------------------------

  /** Trivial constructor. */
  ConfigFileTokenizer(Reader r)
  {
    super();

    br = r instanceof BufferedReader ? (BufferedReader) r : new BufferedReader(r);
    initialised = false;
  }

  // Class methods
  // --------------------------------------------------------------------------

  // Instance methods
  // --------------------------------------------------------------------------

  public int nextToken() throws IOException
  {
    if (!initialised)
      init();

    if (sbNdx >= sb.length())
      return TT_EOF;

    skipWhitespace();

    if (sbNdx >= sb.length())
      return TT_EOF;

    int endNdx;
    if (Character.isJavaIdentifierPart(sb.charAt(sbNdx)))
      {
        endNdx = sbNdx + 1;
        while (Character.isJavaIdentifierPart(sb.charAt(endNdx))
            || sb.charAt(endNdx) == '.')
          endNdx++;

        ttype = TT_WORD;
        sval = sb.substring(sbNdx, endNdx);
        sbNdx = endNdx;
        return ttype;
      }

    int c = sb.charAt(sbNdx);
    if (c == '{' || c == '}' || c == ';' || c == '=')
      {
        ttype = c;
        sbNdx++;
        return ttype;
      }

    if (c == '"' || c == '\'')
      {
        ttype = c;
        String quote = sb.substring(sbNdx, sbNdx + 1);
        int i = sbNdx + 1;
        while (true)
          {
            // find a candidate
            endNdx = sb.indexOf(quote, i);
            if (endNdx == -1)
              abort("Missing closing quote: " + quote);

            // found one; is it escaped?
            if (sb.charAt(endNdx - 1) != '\\')
              break;

            i++;
            continue;
          }

        endNdx++;
        sval = sb.substring(sbNdx, endNdx);
        sbNdx = endNdx;
        return ttype;
      }

    abort("Unknown character: " + sb.charAt(sbNdx));
    return Integer.MIN_VALUE;
  }

  public void pushBack()
  {
    sbNdx -= ttype != TT_WORD ? 1 : sval.length();
  }

  private void init() throws IOException
  {
    sb = new StringBuffer();
    String line;
    while ((line = br.readLine()) != null)
      {
        line = line.trim();
        if (line.length() == 0)
          continue;

        if (line.startsWith("#") || line.startsWith("//"))
          continue;

        sb.append(line).append(" ");
      }

    sbNdx = 0;
    sval = null;
    ttype = TT_NONE;

    initialised = true;
  }

  private void skipWhitespace() throws IOException
  {
    int endNdx;
    while (sbNdx < sb.length())
      if (Character.isWhitespace(sb.charAt(sbNdx)))
        {
          sbNdx++;
          while (sbNdx < sb.length() && Character.isWhitespace(sb.charAt(sbNdx)))
            sbNdx++;

          continue;
        }
      else if (sb.charAt(sbNdx) == '/' && sb.charAt(sbNdx + 1) == '*')
        {
          endNdx = sb.indexOf("*/", sbNdx + 2);
          if (endNdx == -1)
            abort("Missing closing */ sequence");

          sbNdx = endNdx + 2;
          continue;
        }
      else
        break;
  }

  private void abort(String m) throws IOException
  {
    if (Configuration.DEBUG)
      {
        log.fine(m);
        log.fine("sb = " + sb);
        log.fine("sbNdx = " + sbNdx);
      }
    throw new IOException(m);
  }
}
