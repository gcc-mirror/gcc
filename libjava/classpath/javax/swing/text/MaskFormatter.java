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

import gnu.java.lang.CPStringBuilder;

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
    this();
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
    return super.stringToValue(convertStringToValue(value));
  }
  
  private String convertStringToValue(String value)
    throws ParseException
  {
    CPStringBuilder result = new CPStringBuilder();
    char valueChar;
    boolean isPlaceHolder;

    int length = mask.length();
    for (int i = 0, j = 0; j < length; j++)
      {
        char maskChar = mask.charAt(j);

        if (i < value.length())
          {
            isPlaceHolder = false;
            valueChar = value.charAt(i);
            if (maskChar != ESCAPE_CHAR && maskChar != valueChar)
              {
                if (invalidChars != null
                    && invalidChars.indexOf(valueChar) != -1)
                  throw new ParseException("Invalid character: " + valueChar, i);
                if (validChars != null
                    && validChars.indexOf(valueChar) == -1)
                  throw new ParseException("Invalid character: " + valueChar, i);
              }
          }
        else if (placeHolder != null && i < placeHolder.length())
          {
            isPlaceHolder = true;
            valueChar = placeHolder.charAt(i);
          }
        else
          {
            isPlaceHolder = true;
            valueChar = placeHolderChar;
          }

        // This switch block on the mask character checks that the character 
        // within <code>value</code> at that point is valid according to the
        // mask and also converts to upper/lowercase as needed.
        switch (maskChar)
          {
          case NUM_CHAR:
            if (! Character.isDigit(valueChar))
              throw new ParseException("Number expected: " + valueChar, i);
            result.append(valueChar);
            i++;
            break;
          case UPPERCASE_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(Character.toUpperCase(valueChar));
            i++;
            break;
          case LOWERCASE_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(Character.toLowerCase(valueChar));
            i++;
            break;
          case ALPHANUM_CHAR:
            if (! Character.isLetterOrDigit(valueChar))
              throw new ParseException("Letter or number expected", i);
            result.append(valueChar);
            i++;
            break;
          case LETTER_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(valueChar);
            i++;
            break;
          case HEX_CHAR:
            if (hexString.indexOf(valueChar) == -1 && ! isPlaceHolder)
              throw new ParseException("Hexadecimal character expected", i);
            result.append(valueChar);
            i++;
            break;
          case ANYTHING_CHAR:
            result.append(valueChar);
            i++;
            break;
          case ESCAPE_CHAR:
            // Escape character, check the next character to make sure that 
            // the literals match
            j++;
            if (j < length)
              {
                maskChar = mask.charAt(j);
                if (! isPlaceHolder && getValueContainsLiteralCharacters()
                    && valueChar != maskChar)
                  throw new ParseException ("Invalid character: "+ valueChar, i);
                if (getValueContainsLiteralCharacters())
                  {
                    result.append(maskChar);
                  }
                i++;
              }
            else if (! isPlaceHolder)
              throw new ParseException("Bad match at trailing escape: ", i);
            break;
          default:
            if (! isPlaceHolder && getValueContainsLiteralCharacters()
                && valueChar != maskChar)
              throw new ParseException ("Invalid character: "+ valueChar, i);
            if (getValueContainsLiteralCharacters())
              {
                result.append(maskChar);
              }
            i++;
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
  public String valueToString(Object value) throws ParseException
  {
    String string = value != null ? value.toString() : "";
    return convertValueToString(string);
  }
  
  /**
   * This method takes in a String and runs it through the mask to make
   * sure that it is valid.  If <code>convert</code> is true, it also
   * converts letters to upper/lowercase as required by the mask.
   * @param value the String to convert
   * @return the converted String
   * @throws ParseException if the given String isn't valid for the mask
   */
  private String convertValueToString(String value)
    throws ParseException
  {
    CPStringBuilder result = new CPStringBuilder();
    char valueChar;
    boolean isPlaceHolder;

    int length = mask.length();
    for (int i = 0, j = 0; j < length; j++)
      {
        char maskChar = mask.charAt(j);
        if (i < value.length())
          {
            isPlaceHolder = false;
            valueChar = value.charAt(i);
            if (maskChar != ESCAPE_CHAR && valueChar != maskChar)
              {
                if (invalidChars != null
                    && invalidChars.indexOf(valueChar) != -1)
                  throw new ParseException("Invalid character: " + valueChar,
                                           i);
                if (validChars != null && validChars.indexOf(valueChar) == -1)
                  throw new ParseException("Invalid character: " + valueChar +" maskChar: " + maskChar,
                                           i);
              }
          }
        else if (placeHolder != null && i < placeHolder.length())
          {
            isPlaceHolder = true;
            valueChar = placeHolder.charAt(i);
          }
        else
          {
            isPlaceHolder = true;
            valueChar = placeHolderChar;
          }

        // This switch block on the mask character checks that the character 
        // within <code>value</code> at that point is valid according to the
        // mask and also converts to upper/lowercase as needed.
        switch (maskChar)
          {
          case NUM_CHAR:
            if ( ! isPlaceHolder && ! Character.isDigit(valueChar))
              throw new ParseException("Number expected: " + valueChar, i);
            result.append(valueChar);
            i++;
            break;
          case UPPERCASE_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(Character.toUpperCase(valueChar));
            i++;
            break;
          case LOWERCASE_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(Character.toLowerCase(valueChar));
            i++;
            break;
          case ALPHANUM_CHAR:
            if (! Character.isLetterOrDigit(valueChar))
              throw new ParseException("Letter or number expected", i);
            result.append(valueChar);
            i++;
            break;
          case LETTER_CHAR:
            if (! Character.isLetter(valueChar))
              throw new ParseException("Letter expected", i);
            result.append(valueChar);
            i++;
            break;
          case HEX_CHAR:
            if (hexString.indexOf(valueChar) == -1 && ! isPlaceHolder)
              throw new ParseException("Hexadecimal character expected", i);
            result.append(valueChar);
            i++;
            break;
          case ANYTHING_CHAR:
            result.append(valueChar);
            i++;
            break;
          case ESCAPE_CHAR:
            // Escape character, check the next character to make sure that 
            // the literals match
            j++;
            if (j < length)
              {
                maskChar = mask.charAt(j);
                if (! isPlaceHolder && getValueContainsLiteralCharacters()
                    && valueChar != maskChar)
                  throw new ParseException ("Invalid character: "+ valueChar, i);
                if (getValueContainsLiteralCharacters())
                  i++;
                result.append(maskChar);
              }
            break;
          default:
            if (! isPlaceHolder && getValueContainsLiteralCharacters()
                && valueChar != maskChar)
              throw new ParseException ("Invalid character: "+ valueChar, i);
            if (getValueContainsLiteralCharacters())
              i++;
            result.append(maskChar);
          }
      }
    return result.toString();
  }

}
