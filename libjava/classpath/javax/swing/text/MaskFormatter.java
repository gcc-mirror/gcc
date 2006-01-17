/* MaskFormatter.java -- 
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


package javax.swing.text;

import java.text.ParseException;

import javax.swing.JFormattedTextField;

/**
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 */
public class MaskFormatter extends DefaultFormatter
{
  // The declaration of the valid mask characters
  private static final char NUM_CHAR = '#';
  private static final char ESCAPE_CHAR = '\'';
  private static final char UPPERCASE_CHAR = 'U';
  private static final char LOWERCASE_CHAR = 'L';
  private static final char ALPHANUM_CHAR = 'A';
  private static final char LETTER_CHAR = '?';
  private static final char ANYTHING_CHAR = '*';
  private static final char HEX_CHAR = 'H';
  
  /** The mask for this MaskFormatter **/
  private String mask;
  
  /** 
   * A String made up of the characters that are not valid for input for 
   * this MaskFormatter. 
   */
  private String invalidChars;
  
  /** 
   * A String made up of the characters that are valid for input for 
   * this MaskFormatter. 
   */
  private String validChars;
  
  /** A String used in place of missing chracters if the value does not 
   * completely fill in the spaces in the mask.
   */
  private String placeHolder;
  
  /** A character used in place of missing characters if the value does 
   * not completely fill in the spaces in the mask.
   */
  private char placeHolderChar = ' ';
  
  /**
   * Whether or not stringToValue should return literal characters in the mask.
   */
  private boolean valueContainsLiteralCharacters = true;
  
  /** A String used for easy access to valid HEX characters **/
  private static String hexString = "0123456789abcdefABCDEF";
  
  /** An int to hold the length of the mask, accounting for escaped characters **/
  int maskLength = 0;
  
  public MaskFormatter ()
  {
    // Override super's default behaviour, in MaskFormatter the default
    // is not to allow invalid values
    setAllowsInvalid(false);
  }
  
  /**
   * Creates a MaskFormatter with the specified mask.
   * @specnote doesn't actually throw a ParseException although it 
   * is declared to do so
   * @param mask
   * @throws java.text.ParseException
   */
  public MaskFormatter (String mask) throws java.text.ParseException
  {
    // Override super's default behaviour, in MaskFormatter the default
    // is not to allow invalid values
    setAllowsInvalid(false);
    setMask (mask);
  }
  
  /**
   * Returns the mask used in this MaskFormatter.
   * @return the mask used in this MaskFormatter.
   */
  public String getMask()
  {
    return mask;
  }
  
  /**
   * Returns a String containing the characters that are not valid for input
   * for this MaskFormatter.
   * @return a String containing the invalid characters.
   */
  public String getInvalidCharacters()
  {
    return invalidChars;
  }
  
  /**
   * Sets characters that are not valid for input. If
   * <code>invalidCharacters</code> is non-null then no characters contained
   * in it will be allowed to be input.
   * 
   * @param invalidCharacters the String specifying invalid characters.
   */
  public void setInvalidCharacters (String invalidCharacters)
  {
    this.invalidChars = invalidCharacters;
  }
  
  /**
   * Returns a String containing the characters that are valid for input
   * for this MaskFormatter.
   * @return a String containing the valid characters.
   */
  public String getValidCharacters()
  {
    return validChars;
  }
  
  /**
   * Sets characters that are valid for input. If
   * <code>validCharacters</code> is non-null then no characters that are
   * not contained in it will be allowed to be input.
   * 
   * @param validCharacters the String specifying valid characters.
   */
  public void setValidCharacters (String validCharacters)
  {
    this.validChars = validCharacters;
  }

  /**
   * Returns the place holder String that is used in place of missing 
   * characters when the value doesn't completely fill in the spaces
   * in the mask.
   * @return the place holder String.
   */
  public String getPlaceholder()
  {
    return placeHolder;
  }
  
  /**
   * Sets the string to use if the value does not completely fill in the mask.
   * If this is null, the place holder character will be used instead.
   * @param placeholder the String to use if the value doesn't completely 
   * fill in the mask.
   */
  public void setPlaceholder (String placeholder)
  {
    this.placeHolder = placeholder;
  }
  
  /**
   * Returns the character used in place of missing characters when the
   * value doesn't completely fill the mask.
   * @return the place holder character
   */
  public char getPlaceholderCharacter()
  {
    return placeHolderChar;
  }
  
  /**
   * Sets the char  to use if the value does not completely fill in the mask.
   * This is only used if the place holder String has not been set or does 
   * not completely fill in the mask.
   * @param placeholder the char to use if the value doesn't completely 
   * fill in the mask.
   */
  public void setPlaceholderCharacter (char placeholder)
  {
    this.placeHolderChar = placeholder;
  }
  
  /**
   * Returns true if stringToValue should return the literal 
   * characters in the mask.
   * @return true if stringToValue should return the literal 
   * characters in the mask
   */
  public boolean getValueContainsLiteralCharacters()
  {
    return valueContainsLiteralCharacters;
  }
  
  /**
   * Determines whether stringToValue will return literal characters or not.
   * @param containsLiteralChars if true, stringToValue will return the 
   * literal characters in the mask, otherwise it will not.
   */
  public void setValueContainsLiteralCharacters (boolean containsLiteralChars)
  {
    this.valueContainsLiteralCharacters = containsLiteralChars;
  }
  
  /**
   * Sets the mask for this MaskFormatter.  
   * @specnote doesn't actually throw a ParseException even though it is
   * declared to do so
   * @param mask the new mask for this MaskFormatter
   * @throws ParseException if <code>mask</code> is not valid.
   */
  public void setMask (String mask) throws ParseException
  {
    this.mask = mask;

    // Update the cached maskLength.
    int end = mask.length() - 1;
    maskLength = 0;    
    for (int i = 0; i <= end; i++)
      {
        // Handle escape characters properly - they don't add to the maskLength
        // but 2 escape characters in a row is really one escape character and
        // one literal single quote, so that does add 1 to the maskLength.
        if (mask.charAt(i) == '\'')
          {            
            // Escape characters at the end of the mask don't do anything.
            if (i != end)
              maskLength++;
            i++;
          }
        else
          maskLength++;
      }
  }
  
  /**
   * Installs this MaskFormatter on the JFormattedTextField.
   * Invokes valueToString to convert the current value from the 
   * JFormattedTextField to a String, then installs the Actions from
   * getActions, the DocumentFilter from getDocumentFilter, and the 
   * NavigationFilter from getNavigationFilter.
   * 
   * If valueToString throws a ParseException, this method sets the text
   * to an empty String and marks the JFormattedTextField as invalid.
   */
  public void install (JFormattedTextField ftf)
  {
    super.install(ftf);
    if (ftf != null)
      {
        try
        {
          valueToString(ftf.getValue());
        }
        catch (ParseException pe)
        {
          // Set the text to an empty String and mark the JFormattedTextField
          // as invalid.
          ftf.setText("");
          setEditValid(false);
        }
      }
  }
  
  /**
   * Parses the text using the mask, valid characters, and invalid characters
   * to determine the appropriate Object to return.  This strips the literal
   * characters if necessary and invokes super.stringToValue.  If the paramter
   * is invalid for the current mask and valid/invalid character sets this 
   * method will throw a ParseException.
   * 
   * @param value the String to parse
   * @throws ParseException if value doesn't match the mask and valid/invalid
   * character sets
   */
  public Object stringToValue (String value) throws ParseException
  {
    int vLength = value.length();
    
    // For value to be a valid it must be the same length as the mask
    // note this doesn't take into account symbols that occupy more than 
    // one character, this is something we may possibly need to fix.
    if (maskLength != vLength)
      throw new ParseException ("stringToValue passed invalid value", vLength);
    
    // Check if the value is valid according to the mask and valid/invalid 
    // sets.
    try
    {
      convertValue(value, false);      
    }
    catch (ParseException pe)
    {
      throw new ParseException("stringToValue passed invalid value",
                                 pe.getErrorOffset());
    }
    
    if (!getValueContainsLiteralCharacters())
      value = stripLiterals(value);
    return super.stringToValue(value);
  }
  
  /**
   * Strips the literal characters from the given String.
   * @param value the String to strip
   * @return the stripped String
   */
  String stripLiterals(String value)
  {
    StringBuffer result = new StringBuffer();
    for (int i = 0; i < value.length(); i++)
      {
        // Only append the characters that don't correspond to literal
        // characters in the mask.
        switch (mask.charAt(i))
          {
          case NUM_CHAR:
          case UPPERCASE_CHAR:
          case LOWERCASE_CHAR:
          case ALPHANUM_CHAR:
          case LETTER_CHAR:
          case HEX_CHAR:
          case ANYTHING_CHAR:
            result.append(value.charAt(i));
            break;
          default:
          }
      }
    return result.toString();
  }
  
  /**
   * Returns a String representation of the Object value based on the mask.
   * 
   * @param value the value to convert
   * @throws ParseException if value is invalid for this mask and valid/invalid
   * character sets
   */
  public String valueToString (Object value) throws ParseException
  {
    String result = super.valueToString(value);
    int rLength = result.length();
    
    // If value is longer than the mask, truncate it.  Note we may need to 
    // account for symbols that are more than one character long.
    if (rLength > maskLength)
      result = result.substring(0, maskLength);
    
    // Verify the validity and convert to upper/lowercase as needed.
    result = convertValue(result, true);        
    if (rLength < maskLength)
      return pad(result, rLength);    
    return result;
  }
  
  /**
   * This method takes in a String and runs it through the mask to make
   * sure that it is valid.  If <code>convert</code> is true, it also
   * converts letters to upper/lowercase as required by the mask.
   * @param value the String to convert
   * @param convert true if we should convert letters to upper/lowercase
   * @return the converted String
   * @throws ParseException if the given String isn't valid for the mask
   */
  String convertValue(String value, boolean convert) throws ParseException
  {
    StringBuffer result = new StringBuffer(value);
    char markChar;
    char resultChar;
    boolean literal;

    // this boolean is specifically to avoid calling the isCharValid method
    // when neither invalidChars or validChars has been set
    boolean checkCharSets = (invalidChars != null || validChars != null);

    for (int i = 0, j = 0; i < value.length(); i++, j++)
      {
        literal = false;
        resultChar = result.charAt(i);
        // This switch block on the mask character checks that the character 
        // within <code>value</code> at that point is valid according to the
        // mask and also converts to upper/lowercase as needed.
        switch (mask.charAt(j))
          {
          case NUM_CHAR:
            if (!Character.isDigit(resultChar))
              throw new ParseException("Number expected", i);
            break;
          case UPPERCASE_CHAR:
            if (!Character.isLetter(resultChar))
              throw new ParseException("Letter expected", i);
            if (convert)
              result.setCharAt(i, Character.toUpperCase(resultChar));
            break;
          case LOWERCASE_CHAR:
            if (!Character.isLetter(resultChar))
              throw new ParseException("Letter expected", i);
            if (convert)
              result.setCharAt(i, Character.toLowerCase(resultChar));
            break;
          case ALPHANUM_CHAR:
            if (!Character.isLetterOrDigit(resultChar))
              throw new ParseException("Letter or number expected", i);
            break;
          case LETTER_CHAR:
            if (!Character.isLetter(resultChar))
              throw new ParseException("Letter expected", i);
            break;
          case HEX_CHAR:
            if (hexString.indexOf(resultChar) == -1)
              throw new ParseException("Hexadecimal character expected", i);
            break;
          case ANYTHING_CHAR:
            break;
          case ESCAPE_CHAR:
            // Escape character, check the next character to make sure that 
            // the literals match
            j++;
            literal = true;
            if (resultChar != mask.charAt(j))
              throw new ParseException ("Invalid character: "+resultChar, i);
            break;
          default:
            literal = true;
            if (!getValueContainsLiteralCharacters() && convert)
              throw new ParseException ("Invalid character: "+resultChar, i);
            else if (resultChar != mask.charAt(j))
              throw new ParseException ("Invalid character: "+resultChar, i);
          }
        // If necessary, check if the character is valid.
        if (!literal && checkCharSets && !isCharValid(resultChar))
          throw new ParseException("invalid character: "+resultChar, i);

      }
    return result.toString();
  }
  
  /**
   * Convenience method used by many other methods to check if a character is 
   * valid according to the mask, the validChars, and the invalidChars.  To
   * be valid a character must:
   * 1. be allowed by the mask
   * 2. be present in any non-null validChars String
   * 3. not be present in any non-null invalidChars String
   * @param testChar the character to test
   * @return true if the character is valid
   */
  boolean isCharValid(char testChar)
  {
    char lower = Character.toLowerCase(testChar);
    char upper = Character.toUpperCase(testChar);
    // If validChars isn't null, the character must appear in it.
    if (validChars != null)
      if (validChars.indexOf(lower) == -1 && validChars.indexOf(upper) == -1)
        return false;
    // If invalidChars isn't null, the character must not appear in it.
    if (invalidChars != null)
      if (invalidChars.indexOf(lower) != -1
          || invalidChars.indexOf(upper) != -1)
        return false;
    return true;
  }
  
  /**
   * Pads the value with literals, the placeholder String and/or placeholder
   * character as appropriate.
   * @param value the value to pad
   * @param currLength the current length of the value
   * @return the padded String
   */
  String pad (String value, int currLength)
  {
    StringBuffer result = new StringBuffer(value);
    int index = currLength;
    while (result.length() < maskLength)
      {
        // The character used to pad may be a literal, a character from the 
        // place holder string, or the place holder character.  getPadCharAt
        // will find the proper one for us.
        result.append (getPadCharAt(index));
        index++;
      }
    return result.toString();
  }

  /**
   * Returns the character with which to pad the value at the given index
   * position.  If the mask has a literal at this position, this is returned
   * otherwise if the place holder string is initialized and is longer than 
   * <code>i</code> characters then the character at position <code>i</code>
   * from this String is returned.  Else, the place holder character is 
   * returned.
   * @param i the index at which we want to pad the value
   * @return the character with which we should pad the value
   */
  char getPadCharAt(int i)
  {
    boolean escaped = false;
    int target = i;
    char maskChar;
    int holderLength = placeHolder == null ? -1 : placeHolder.length();
    // We must iterate through the mask from the beginning, because the given
    // index doesn't account for escaped characters.  For example, with the 
    // mask "1A'A''A1" index 2 refers to the literalized A, not to the 
    // single quotation.
    for (int n = 0; n < mask.length(); n++)
      {
        maskChar = mask.charAt(n);
        if (maskChar == ESCAPE_CHAR && !escaped)
          {
            target++;
            escaped = true;
          }
        else if (escaped == true)
          {
            // Check if target == n which means we've come to the character
            // we want to return and since it is a literal (because escaped 
            // is true), we return it.
            if (target == n)
              return maskChar;
            escaped = false;
          }
        if (target == n)
          {
            // We've come to the character we want to return.  It wasn't
            // escaped so if it isn't a literal we should return either
            // the character from place holder string or the place holder
            // character, depending on whether or not the place holder
            // string is long enough.
            switch (maskChar)
            {
            case NUM_CHAR:
            case UPPERCASE_CHAR:
            case LOWERCASE_CHAR:
            case ALPHANUM_CHAR:
            case LETTER_CHAR:
            case HEX_CHAR:
            case ANYTHING_CHAR:
              if (holderLength > i)
                return placeHolder.charAt(i);
              else
                return placeHolderChar;
            default:
              return maskChar;
            }
          }
      }
    // This shouldn't happen
    throw new AssertionError("MaskFormatter.getMaskCharAt failed");
  }
}
