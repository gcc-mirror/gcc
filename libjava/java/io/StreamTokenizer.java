/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
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
  /* A constant indicating that the end of the stream has been read. */
  public static final int TT_EOF = -1;

  /* A constant indicating that the end of the line has been read. */
  public static final int TT_EOL = '\n';

  /* A constant indicating that a number token has been read. */
  public static final int TT_NUMBER = -2;

  /* A constant indicating that a word token has been read. */
  public static final int TT_WORD = -3;

  /* Contains the type of the token read resulting from a call to nextToken. */
  public int ttype;

  /* The String associated with word and string tokens. */
  public String sval;

  /* The numeric value associated with number tokens. */
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
  private boolean[] whitespace;
  private boolean[] alphabetic;
  private boolean[] numeric;
  private boolean[] quote;
  private boolean[] comment;

  /* The Reader associated with this class. */
  private PushbackReader in;

  /* Indicates if a token has been pushed back. */
  private boolean pushedBack = false;

  /* Contains the current line number of the reader. */
  private int lineNumber = 1;

  // Deprecated in JDK 1.1.
  public StreamTokenizer(InputStream is)
  {
    this(new InputStreamReader(is));
  }

  public StreamTokenizer(Reader r)
  {
    in = new PushbackReader(r);

    whitespace = new boolean[256];
    alphabetic = new boolean[256];
    numeric = new boolean[256];
    quote = new boolean[256];
    comment = new boolean[256];
    for (int i = 0; i < 256; i++)
      resetChar(i);

    whitespaceChars(0x00, 0x20);
    wordChars('A', 'Z');
    wordChars('a', 'z');
    wordChars(0xA0, 0xFF);
    commentChar('/');
    quoteChar('\'');
    quoteChar('"');
    parseNumbers();
  }

  public void commentChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      comment[ch] = true;
  }

  public void eolIsSignificant(boolean flag)
  {
    eolSignificant = flag;
  }

  public int lineno()
  {
    return lineNumber;
  }

  public void lowerCaseMode(boolean flag)
  {
    lowerCase = flag;
  }

  private boolean isWhitespace(int ch)
  {
    if (ch >= 0 && ch <= 255)
      return whitespace[ch];

    return false;
  }

  private boolean isAlphabetic(int ch)
  {
    if (ch >= 0 && ch <= 255)
      return alphabetic[ch];
    else if (ch > 255)
      return true;

    return false;
  }

  private boolean isNumeric(int ch)
  {
    if (ch >= 0 && ch <= 255)
      return numeric[ch];

    return false;
  }

  private boolean isQuote(int ch)
  {
    if (ch >= 0 && ch <= 255)
      return quote[ch];

    return false;
  }

  private boolean isComment(int ch)
  {
    if (ch >= 0 && ch <= 255)
      return comment[ch];

    return false;
  }

  public int nextToken() throws IOException
  {
    if (pushedBack)
      {
	pushedBack = false;
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

    if (ch == TT_EOF)
      ttype = TT_EOF;
    else if (isNumeric(ch))
      {
	if (ch == '-')
	  {
	    // Read ahead to see if this is an ordinary '-' rather than numeric.
	    ch = in.read();
	    if (ch != TT_EOF)
	      in.unread(ch);
	    if (isNumeric(ch) && ch != '-')
	      ch = '-';
	    else
	      return (ttype = '-');
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
	nval = Double.valueOf(tokbuf.toString()).doubleValue();
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
	  sval.toLowerCase();
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

	ttype = ch;
      }

    return ttype;
  }

  private void resetChar(int ch)
  {
    whitespace[ch] = alphabetic[ch] = numeric[ch] = quote[ch] = comment[ch] =
      false;
  }

  public void ordinaryChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      resetChar(ch);
  }

  public void ordinaryChars(int low, int hi)
  {
    if (low < 0)
      low = 0;
    if (hi > 255)
      hi = 255;
    for (int i = low; i <= hi; i++)
      resetChar(i);
  }

  public void parseNumbers()
  {
    for (int i = 0; i <= 9; i++)
      numeric['0' + i] = true;

    numeric['.'] = true;
    numeric['-'] = true;
  }

  public void pushBack()
  {
    // pushBack may cause the lineno method to return an incorrect value
    // if lineno is called before the next call to nextToken.
    pushedBack = true;
  }

  public void quoteChar(int ch)
  {
    if (ch >= 0 && ch <= 255)
      quote[ch] = true;
  }

  public void resetSyntax()
  {
    ordinaryChars(0x00, 0xFF);
  }

  public void slashSlashComments(boolean flag)
  {
    slashSlash = flag;
  }

  public void slashStarComments(boolean flag)
  {
    slashStar = flag;
  }

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
      tempstr = "n=" + Double.toString(nval);
    else // must be an ordinary char.
      tempstr = "\'" + (new Character((char) ttype)).toString() + "\'";

    return "Token[" + tempstr + "], line " + Integer.toString(lineno());
  }

  public void whitespaceChars(int low, int hi)
  {
    if (low < 0)
      low = 0;
    if (hi > 255)
      hi = 255;
    for (int i = low; i <= hi; i++)
      whitespace[i] = true;
  }

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
