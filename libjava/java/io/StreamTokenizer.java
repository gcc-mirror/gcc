/* StreamTokenizer.java -- parses streams of characters into tokens
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

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

package java.io;

/**
 * This class parses streams of characters into tokens.  There are a
 * million-zillion flags that can be set to control the parsing, as 
 * described under the various method headings.
 *
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 25, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class StreamTokenizer
{
  /** A constant indicating that the end of the stream has been read. */
  public static final int TT_EOF = -1;

  /** A constant indicating that the end of the line has been read. */
  public static final int TT_EOL = '\n';

  /** A constant indicating that a number token has been read. */
  public static final int TT_NUMBER = -2;

  /** A constant indicating that a word token has been read. */
  public static final int TT_WORD = -3;

  /** A constant indicating that no tokens have been read yet. */
  private static final int TT_NONE = -4;

  /**
   * Contains the type of the token read resulting from a call to nextToken
   * The rules are as follows:
   * <ul>
   * <li>For a token consisting of a single ordinary character, this is the 
   *     value of that character.
   * <li>For a quoted string, this is the value of the quote character
   * <li>For a word, this is TT_WORD
   * <li>For a number, this is TT_NUMBER
   * <li>For the end of the line, this is TT_EOL
   * <li>For the end of the stream, this is TT_EOF
   * </ul>
   */
  public int ttype = TT_NONE;

  /** The String associated with word and string tokens. */
  public String sval;

  /** The numeric value associated with number tokens. */
  public double nval;

  /* Indicates whether end-of-line is recognized as a token. */
  private boolean eolSignificant = false;

  /* Indicates whether word tokens are automatically made lower case. */
  private boolean lowerCase = false;

  /* Indicates whether C++ style comments are recognized and skipped. */
  private boolean slashSlash = false;

  /* Indicates whether C style comments are recognized and skipped. */
  private boolean slashStar = false;

  /* Attribute tables of each byte from 0x00 to 0xFF. */
  private boolean[] whitespace = new boolean[256];
  private boolean[] alphabetic = new boolean[256];
  private boolean[] numeric = new boolean[256];
  private boolean[] quote = new boolean[256];
  private boolean[] comment = new boolean[256];

  /* The Reader associated with this class. */
  private PushbackReader in;

  /* Indicates if a token has been pushed back. */
  private boolean pushedBack = false;

  /* Contains the current line number of the reader. */
  private int lineNumber = 1;

  /**
   * This method reads bytes from an <code>InputStream</code> and tokenizes
   * them.  For details on how this method operates by default, see
   * <code>StreamTokenizer(Reader)</code>.
   *
   * @param in The <code>InputStream</code> to read from
   *
   * @deprecated Since JDK 1.1.
   */
  public StreamTokenizer(InputStream is)
  {
    this(new InputStreamReader(is));
  }

  /**
   * This method initializes a new <code>StreamTokenizer</code> to read 
   * characters from a <code>Reader</code> and parse them.  The char values
   * have their hight bits masked so that the value is treated a character
   * in the range of 0x0000 to 0x00FF.
   * <p>
   * This constructor sets up the parsing table to parse the stream in the
   * following manner:
   * <ul>
   * <li>The values 'A' through 'Z', 'a' through 'z' and 0xA0 through 0xFF
   *     are initialized as alphabetic
   * <li>The values 0x00 through 0x20 are initialized as whitespace
   * <li>The values '\'' and '"' are initialized as quote characters
   * <li>'/' is a comment character
   * <li>Numbers will be parsed
   * <li>EOL is not treated as significant
   * <li>C  and C++ (//) comments are not recognized
   * </ul>
   *
   * @param in The <code>Reader</code> to read chars from
   */
  public StreamTokenizer(Reader r)
  {
    in = new PushbackReader(r);

    whitespaceChars(0x00, 0x20);
    wordChars('A', 'Z');
    wordChars('a', 'z');
    wordChars(0xA0, 0xFF);
    commentChar('/');
    quoteChar('\'');
    quoteChar('"');
    parseNumbers();
  }

  /**
   * This method sets the comment attribute on the specified
   * character.  Other attributes for the character are cleared.
   *
   * @param c The character to set the comment attribute for, passed as an int
   */
  public void commentChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      {
	comment[ch] = true;
	whitespace[ch] = false;
	alphabetic[ch] = false;
	numeric[ch] = false;
	quote[ch] = false;
      }
  }

  /**
   * This method sets a flag that indicates whether or not the end of line
   * sequence terminates and is a token.  The defaults to <code>false</code>
   *
   * @param flag <code>true</code> if EOF is significant, <code>false</code>
   *             otherwise
   */
  public void eolIsSignificant(boolean flag)
  {
    eolSignificant = flag;
  }

  /**
   * This method returns the current line number.  Note that if the 
   * <code>pushBack()</code> method is called, it has no effect on the
   * line number returned by this method.
   *
   * @return The current line number
   */
  public int lineno()
  {
    return lineNumber;
  }

  /**
   * This method sets a flag that indicates whether or not alphabetic
   * tokens that are returned should be converted to lower case.
   * 
   * @param flag <code>true</code> to convert to lower case,
   *             <code>false</code> otherwise
   */
  public void lowerCaseMode(boolean flag)
  {
    lowerCase = flag;
  }

  private boolean isWhitespace(int ch)
  {
    return (ch >= 0 && ch <= 255 && whitespace[ch]);
  }

  private boolean isAlphabetic(int ch)
  {
    return ((ch > 255) || (ch >= 0 && alphabetic[ch]));
  }

  private boolean isNumeric(int ch)
  {
    return (ch >= 0 && ch <= 255 && numeric[ch]);
  }

  private boolean isQuote(int ch)
  {
    return (ch >= 0 && ch <= 255 && quote[ch]);
  }

  private boolean isComment(int ch)
  {
    return (ch >= 0 && ch <= 255 && comment[ch]);
  }

  /**
   * This method reads the next token from the stream.  It sets the 
   * <code>ttype</code> variable to the appropriate token type and 
   * returns it.  It also can set <code>sval</code> or <code>nval</code>
   * as described below.  The parsing strategy is as follows:
   * <ul>
   * <li>Skip any whitespace characters.
   * <li>If a numeric character is encountered, attempt to parse a numeric
   * value.  Leading '-' characters indicate a numeric only if followed by
   * another non-'-' numeric.  The value of the numeric token is terminated
   * by either the first non-numeric encountered, or the second occurrence of
   * '-' or '.'.  The token type returned is TT_NUMBER and <code>nval</code>
   * is set to the value parsed.
   * <li>If an alphabetic character is parsed, all subsequent characters
   * are read until the first non-alphabetic or non-numeric character is
   * encountered.  The token type returned is TT_WORD and the value parsed
   * is stored in <code>sval</code>.  If lower case mode is set, the token
   * stored in <code>sval</code> is converted to lower case.  The end of line
   * sequence terminates a word only if EOL signficance has been turned on.
   * The start of a comment also terminates a word.  Any character with a 
   * non-alphabetic and non-numeric attribute (such as white space, a quote,
   * or a commet) are treated as non-alphabetic and terminate the word.
   * <li>If a comment character is parsed, then all remaining characters on
   * the current line are skipped and another token is parsed.  Any EOL or
   * EOF's encountered are not discarded, but rather terminate the comment.
   * <li>If a quote character is parsed, then all characters up to the 
   * second occurrence of the same quote character are parsed into a
   * <code>String</code>.  This <code>String</code> is stored as
   * <code>sval</code>, but is not converted to lower case, even if lower case
   * mode is enabled.  The token type returned is the value of the quote
   * character encountered.  Any escape sequences
   * (\b (backspace), \t (HTAB), \n (linefeed), \f (form feed), \r
   * (carriage return), \" (double quote), \' (single quote), \\
   * (backslash), \XXX (octal esacpe)) are converted to the appropriate
   * char values.  Invalid esacape sequences are left in untranslated.  
   * Unicode characters like ('\ u0000') are not recognized. 
   * <li>If the C++ comment sequence "//" is encountered, and the parser
   * is configured to handle that sequence, then the remainder of the line
   * is skipped and another token is read exactly as if a character with
   * the comment attribute was encountered.
   * <li>If the C comment sequence "/*" is encountered, and the parser
   * is configured to handle that sequence, then all characters up to and
   * including the comment terminator sequence are discarded and another
   * token is parsed.
   * <li>If all cases above are not met, then the character is an ordinary
   * character that is parsed as a token by itself.  The char encountered
   * is returned as the token type.
   * </ul>
   *
   * @return The token type
   * @exception IOException If an I/O error occurs
   */
  public int nextToken() throws IOException
  {
    if (pushedBack)
      {
	pushedBack = false;
	if (ttype != TT_NONE)
	  return ttype;
      }

    sval = null;
    int ch;

    // Skip whitespace.  Deal with EOL along the way.
    while (isWhitespace(ch = in.read()))
      if (ch == '\n' || ch == '\r')
	{
	  lineNumber++;

	  // Throw away \n if in combination with \r.
	  if (ch == '\r' && (ch = in.read()) != '\n')
	    {
	      if (ch != TT_EOF)
		in.unread(ch);
	    }
	  if (eolSignificant)
	    return (ttype = TT_EOL);
	}

    if (ch == '/')
      if ((ch = in.read()) == '/' && slashSlash)
	{
	  while ((ch = in.read()) != '\n' && ch != '\r' && ch != TT_EOF)
	    ;
	  if (ch != TT_EOF)
	    in.unread(ch);
	  return nextToken(); // Recursive, but not too deep in normal cases
	}
      else if (ch == '*' && slashStar) 
	{
	  while (true)
	    {
	      ch = in.read();
	      if (ch == '*')
		{
		  if ((ch = in.read()) == '/')
		    break;
		  else if (ch != TT_EOF)
		    in.unread(ch);
		}
	      else if (ch == '\n' || ch == '\r')
		{
		  lineNumber++;
		  if (ch == '\r' && (ch = in.read()) != '\n')
		    {
		      if (ch != TT_EOF)
			in.unread(ch);
		    }
		}
	      else if (ch == TT_EOF)
		{
		  break;
		}
	    }
	  return nextToken(); // Recursive, but not too deep in normal cases
	}
      else
	{
	  if (ch != TT_EOF)
	    in.unread(ch);
	  ch = '/';
	}

    if (ch == TT_EOF)
      ttype = TT_EOF;
    else if (isNumeric(ch))
      {
	boolean isNegative = false;
	if (ch == '-')
	  {
	    // Read ahead to see if this is an ordinary '-' rather than numeric.
	    ch = in.read();
	    if (isNumeric(ch) && ch != '-')
	      {
		isNegative = true;
	      }
	    else
	      {
		if (ch != TT_EOF)
		  in.unread(ch);
		return (ttype = '-');
	      }
	  }

	StringBuffer tokbuf = new StringBuffer();
	tokbuf.append((char) ch);

	int decCount = 0;
	while (isNumeric(ch = in.read()) && ch != '-')
	  if (ch == '.' && decCount++ > 0)
	    break;
	  else
	    tokbuf.append((char) ch);

	if (ch != TT_EOF)
	  in.unread(ch);
	ttype = TT_NUMBER;
	try
	  {
	    nval = Double.valueOf(tokbuf.toString()).doubleValue();
	  }
	catch (NumberFormatException _)
	  {
	    nval = 0.0;
	  }
	if (isNegative)
	  nval = -nval;
      }
    else if (isAlphabetic(ch))
      {
	StringBuffer tokbuf = new StringBuffer();
	tokbuf.append((char) ch);
	while (isAlphabetic(ch = in.read()) || isNumeric(ch))
	  tokbuf.append((char) ch);
	if (ch != TT_EOF)
	  in.unread(ch);
	ttype = TT_WORD;
	sval = tokbuf.toString();
	if (lowerCase)
	  sval = sval.toLowerCase();
      }
    else if (isComment(ch))
      {
	while ((ch = in.read()) != '\n' && ch != '\r' && ch != TT_EOF)
	  ;
	if (ch != TT_EOF)
	  in.unread(ch);
	return nextToken();	// Recursive, but not too deep in normal cases.
      }
    else if (isQuote(ch))
      {
	ttype = ch;
	StringBuffer tokbuf = new StringBuffer();
	while ((ch = in.read()) != ttype && ch != '\n' && ch != '\r' &&
	       ch != TT_EOF)
	  {
	    if (ch == '\\')
	      switch (ch = in.read())
		{
		  case 'a':	ch = 0x7;
		    break;
		  case 'b':	ch = '\b';
		    break;
		  case 'f':	ch = 0xC;
		    break;
		  case 'n':	ch = '\n';
		    break;
		  case 'r':	ch = '\r';
		    break;
		  case 't':	ch = '\t';
		    break;
		  case 'v':	ch = 0xB;
		    break;
		  case '\n':    ch = '\n';
		    break;
                  case '\r':    ch = '\r';
		    break;
		  case '\"':
		  case '\'':
		  case '\\':
		    break;
		  default:
		    int ch1, nextch;
		    if ((nextch = ch1 = ch) >= '0' && ch <= '7')
		      {
		        ch -= '0';
		        if ((nextch = in.read()) >= '0' && nextch <= '7')
			  {
			    ch = ch * 8 + nextch - '0';
			    if ((nextch = in.read()) >= '0' && nextch <= '7' &&
				ch1 >= '0' && ch1 <= '3')
			      {
				ch = ch * 8 + nextch - '0';
				nextch = in.read();
			      }
			  }
		      }

		    if (nextch != TT_EOF)
		      in.unread(nextch);
		}

	    tokbuf.append((char) ch);
	  }

	// Throw away matching quote char.
	if (ch != ttype && ch != TT_EOF)
	  in.unread(ch);

	sval = tokbuf.toString();
      }
    else
      {
	ttype = ch;
      }

    return ttype;
  }

  private void resetChar(int ch)
  {
    whitespace[ch] = alphabetic[ch] = numeric[ch] = quote[ch] = comment[ch] =
      false;
  }

  /**
   * This method makes the specified character an ordinary character.  This
   * means that none of the attributes (whitespace, alphabetic, numeric,
   * quote, or comment) will be set on this character.  This character will
   * parse as its own token.
   *
   * @param c The character to make ordinary, passed as an int
   */
  public void ordinaryChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      resetChar(ch);
  }

  /**
   * This method makes all the characters in the specified range, range
   * terminators included, ordinary.  This means the none of the attributes
   * (whitespace, alphabetic, numeric, quote, or comment) will be set on
   * any of the characters in the range.  This makes each character in this
   * range parse as its own token.
   *
   * @param low The low end of the range of values to set the whitespace
   *            attribute for
   * @param high The high end of the range of values to set the whitespace
   *            attribute for
   */
  public void ordinaryChars(int low, int hi)
  {
    if (low < 0)
      low = 0;
    if (hi > 255)
      hi = 255;
    for (int i = low; i <= hi; i++)
      resetChar(i);
  }

  /**
   * This method sets the numeric attribute on the characters '0' - '9' and
   * the characters '.' and '-'.
   */
  public void parseNumbers()
  {
    for (int i = 0; i <= 9; i++)
      numeric['0' + i] = true;

    numeric['.'] = true;
    numeric['-'] = true;
  }

  /**
   * Puts the current token back into the StreamTokenizer so
   * <code>nextToken</code> will return the same value on the next call.
   * May cause the lineno method to return an incorrect value
   * if lineno is called before the next call to nextToken.
   */
  public void pushBack()
  {
    pushedBack = true;
  }

  /**
   * This method sets the quote attribute on the specified character.
   * Other attributes for the character are cleared.
   *
   * @param c The character to set the quote attribute for, passed as an int.
   */
  public void quoteChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      {
	quote[ch] = true;
	comment[ch] = false;
	whitespace[ch] = false;
	alphabetic[ch] = false;
	numeric[ch] = false;
      }
  }

  /**
   * This method removes all attributes (whitespace, alphabetic, numeric,
   * quote, and comment) from all characters.  It is equivalent to calling
   * <code>ordinaryChars(0x00, 0xFF)</code>.
   *
   * @see #ordinaryChars(int, int)
   */
  public void resetSyntax()
  {
    ordinaryChars(0x00, 0xFF);
  }

  /**
   * This method sets a flag that indicates whether or not "C++" language style
   * comments ("//" comments through EOL ) are handled by the parser.
   * If this is <code>true</code> commented out sequences are skipped and
   * ignored by the parser.  This defaults to <code>false</code>.
   *
   * @param flag <code>true</code> to recognized and handle "C++" style
   *             comments, <code>false</code> otherwise
   */
  public void slashSlashComments(boolean flag)
  {
    slashSlash = flag;
  }

  /**
   * This method sets a flag that indicates whether or not "C" language style
   * comments (with nesting not allowed) are handled by the parser.
   * If this is <code>true</code> commented out sequences are skipped and
   * ignored by the parser.  This defaults to <code>false</code>.
   *
   * @param flag <code>true</code> to recognized and handle "C" style comments,
   *             <code>false</code> otherwise
   */
  public void slashStarComments(boolean flag)
  {
    slashStar = flag;
  }

  /**
   * This method returns the current token value as a <code>String</code> in
   * the form "Token[x], line n", where 'n' is the current line numbers and
   * 'x' is determined as follows.
   * <p>
   * <ul>
   * <li>If no token has been read, then 'x' is "NOTHING" and 'n' is 0
   * <li>If <code>ttype</code> is TT_EOF, then 'x' is "EOF"
   * <li>If <code>ttype</code> is TT_EOL, then 'x' is "EOL"
   * <li>If <code>ttype</code> is TT_WORD, then 'x' is <code>sval</code>
   * <li>If <code>ttype</code> is TT_NUMBER, then 'x' is "n=strnval" where
   * 'strnval' is <code>String.valueOf(nval)</code>.
   * <li>If <code>ttype</code> is a quote character, then 'x' is
   * <code>sval</code>
   * <li>For all other cases, 'x' is <code>ttype</code>
   * </ul>
   */
  public String toString()
  {
    String tempstr;
    if (ttype == TT_EOF)
      tempstr = "EOF";
    else if (ttype == TT_EOL)
      tempstr = "EOL";
    else if (ttype == TT_WORD)
      tempstr = sval;
    else if (ttype == TT_NUMBER)
      tempstr = "n=" + nval;
    else if (ttype == TT_NONE)
      tempstr = "NOTHING";
    else // must be an ordinary char.
      tempstr = "\'" + (char) ttype + "\'";

    return "Token[" + tempstr + "], line " + lineno();
  }

  /**
   * This method sets the whitespace attribute for all characters in the
   * specified range, range terminators included.
   *
   * @param low The low end of the range of values to set the whitespace
   *            attribute for
   * @param high The high end of the range of values to set the whitespace
   *             attribute for
   */
  public void whitespaceChars(int low, int hi)
  {
    if (low < 0)
      low = 0;
    if (hi > 255)
      hi = 255;
    for (int i = low; i <= hi; i++)
      {
	resetChar(i);
	whitespace[i] = true;
      }
  }

  /**
   * This method sets the alphabetic attribute for all characters in the
   * specified range, range terminators included.
   *
   * @param low The low end of the range of values to set the alphabetic
   *            attribute for
   * @param high The high end of the range of values to set the alphabetic
   *             attribute for
   */
  public void wordChars(int low, int hi)
  {
    if (low < 0)
      low = 0;
    if (hi > 255)
      hi = 255;
    for (int i = low; i <= hi; i++)
      alphabetic[i] = true;
  }
}
