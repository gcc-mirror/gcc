/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 24, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

public class StringTokenizer implements Enumeration
{
  /* String to be parsed */
  private String inputString;

  /* String to be parsed put into a char array for efficient access */
  private char[] chArray;

  /* Set of delimiter characters for separating tokens */
  private String delimiters;

  /* Whether delimiters in this instance are treated as tokens themselves */
  private boolean returnDelimiters;

  /* Index into the input string to start parsing for the next token */
  private int inputStringIndex;

  public StringTokenizer(String str)
  {
    this(str, " \t\n\r", false);
  }

  public StringTokenizer(String str, String delims)
  {
    this(str, delims, false);
  }

  public StringTokenizer(String str, String delims, boolean retDelim)
  {
    inputString = str;
    delimiters = delims;
    returnDelimiters = retDelim;
    inputStringIndex = 0;

    // Work on a copy of the remaining string in a char array
    // to gain efficiency of using primitives
    chArray = new char[inputString.length()];
    inputString.getChars(0, inputString.length(), chArray, 0);
  }

  public int countTokens()
  {
    int count = 0;
    int delimiterCount = 0;
    boolean tokenFound = false;		// Set when a non-delimiter is found
    int offset = inputStringIndex;

    // Note for efficiency, we count up the delimiters rather than check
    // returnDelimiters every time we encounter one.  That way, we can
    // just do the conditional once at the end of the method
    while (offset < chArray.length)
      {
	if (isDelimiter(chArray[offset++]))
	  {
	    if (tokenFound)
	      {
		// Got to the end of a token
	        count++;
	        tokenFound = false;
	      }

	    delimiterCount++;		// Increment for this delimiter
	  }
	else
	  {
	    tokenFound = true;

	    // Get to the end of the token
	    while (offset < chArray.length && !isDelimiter(chArray[offset]))
	      offset++;
	  }
      }

    // Make sure to count the last token 
    if (tokenFound)
      count++;

    // if counting delmiters add them into the token count
    return returnDelimiters ? count + delimiterCount : count;
  }

  public boolean hasMoreElements()
  {
    return hasMoreTokens();
  }

  public boolean hasMoreTokens()
  {
    int offset = inputStringIndex;

    while (offset < chArray.length)
      if (!isDelimiter(chArray[offset++]) || returnDelimiters)
	{
	  // update the current position with the start of the next token
	  inputStringIndex = --offset;

	  return true;
	}

    return false;
  }

  public Object nextElement()
  {
    return nextToken();
  }

  public String nextToken()
  {
    int offset = inputStringIndex;
    int startSubstr = -1;

    // Make sure we have more chars left to parse
    // and then find the start of the next token
    while (offset < chArray.length && startSubstr < 0)
      {
	// Find the start of the token; skipping initial delimiters
	if (!isDelimiter(chArray[offset++]))
	  startSubstr = offset - 1;
	else if (returnDelimiters)
	  {
	    // The single char delimiter is treated as a token
	    inputStringIndex = offset;		// update the current position

	    return inputString.substring(offset - 1, inputStringIndex);
	  }
      }

    // Now look for the end of the token
    while (offset < chArray.length)
      {
	if (isDelimiter(chArray[offset++]))
	  {
	    // Found the end of token
            inputStringIndex = offset - 1;	// update the current position

            return inputString.substring(startSubstr, inputStringIndex);
	  }
      }

    // Got to the end of the string without finding the start of a token
    if (startSubstr < 0)
      throw new NoSuchElementException();

    // Got to the end of the string before a delimiter
    inputStringIndex = offset;		// update the current position

    return inputString.substring(startSubstr, inputStringIndex);
  }

  public String nextToken(String delims)
  {
    // First replace with new set of delimiters
    delimiters = delims;

    return nextToken();
  }

  // This private method could be inlined but the other methods are
  // more readable this way, so we'll take the hit on efficiency.
  private boolean isDelimiter(char ch)
  {
    return delimiters.indexOf(ch) >= 0;
  }
}
