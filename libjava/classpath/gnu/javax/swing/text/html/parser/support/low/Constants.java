/* Constants.java --
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


package gnu.javax.swing.text.html.parser.support.low;

import java.util.BitSet;

/**
 * The parser constants and operations, directly related to the parser
 * constants.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Constants
{
  /* Single character tokens are reflected into they ASCII codes. */

  /**
   * Start of HTML token.
   */
  public static final int BEGIN = '<';

  /**
   * End of HTML token.
   */
  public static final int END = '>';

  /**
   * Exclamation (indicates SGML or comment).
   */
  public static final int EXCLAMATION = '!';

  /**
   * Slash (indicates closing tag).
   */
  public static final int SLASH = '/';

  /**
   * Equals sign.
   */
  public static final int EQ = '=';

  /**
   * Quoting sign.
   */
  public static final int AP = '\'';

  /**
   * Quoting sign.
   */
  public static final int QUOT = '"';

  /* The numbers of other tokens start outside the ascii space. */
  /* String tokens */

  /**
   * Double dash (--)
   */
  public static final int DOUBLE_DASH = 1000;

  /**
   * The STYLE tag (needs special handling).
   */
  public static final int STYLE = 1001;

  /**
   * The SCRIPT tag (needs special handling).
   */
  public static final int SCRIPT = 1002;

  /* Pattern tokens */

  /**
   * HTML whitespace.
   */
  public static final int WS = 1003;

  /**
   * Named or numeric entity,
   */
  public static final int ENTITY = 1004;

  /**
   * Sequence of valid name characters (can start from digit).
   */
  public static final int NUMTOKEN = 1005;

  /* Complex tokens */

  /**
   * Comment opening sequence.
   */
  public static final pattern COMMENT_OPEN =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(EXCLAMATION),
                  new node(WS, true), new node(DOUBLE_DASH),
                }
               );

  /**
   * Comment closing sequence
   */
  public static final pattern COMMENT_END =
    new pattern(new node[]
                {
                  new node(DOUBLE_DASH), new node(WS, true), new node(END)
                }
               );

  /**
   * Special case ---> (also is treated as end of comment).
   */
  public static final pattern COMMENT_TRIPLEDASH_END =
    new pattern(new node[]
                {
                  new node(DOUBLE_DASH), new node(NUMTOKEN), new node(END)
                }
               );

  /**
   * STYLE element heading pattern.
   */
  public static final pattern STYLE_OPEN =
    new pattern(new node[] { new node(BEGIN), new node(WS, true), new node(STYLE) });

  /**
   * SCRIPT element heading pattern.
   */
  public static final pattern SCRIPT_OPEN =
    new pattern(new node[] { new node(BEGIN), new node(WS, true), new node(SCRIPT) });

  /**
   * SGML element heading pattern.
   */
  public static final pattern SGML =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(EXCLAMATION)
                }
               );

  /**
   * SCRIPT element closing pattern.
   */
  public static final pattern SCRIPT_CLOSE =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(SLASH),
                  new node(WS, true), new node(SCRIPT), new node(WS, true),
                  new node(END)
                }
               );

  /**
   * STYLE element closing pattern.
   */
  public static final pattern STYLE_CLOSE =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(SLASH),
                  new node(WS, true), new node(STYLE), new node(WS, true),
                  new node(END)
                }
               );

  /**
   * Ordinary HTML tag heading pattern.
   */
  public static final pattern TAG =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(SLASH, true),
                  new node(WS, true), new node(NUMTOKEN)
                }
               );

  /**
   * Ordinary HTML tag closing pattern.
   */
  public static final pattern TAG_CLOSE =
    new pattern(new node[]
                {
                  new node(BEGIN), new node(WS, true), new node(SLASH),
                  new node(WS, true), new node(NUMTOKEN)
                }
               );

  /* Special tokens */

  /**
   * All other tokens.
   */
  public static final int OTHER = 1999;

  /**
   * The UNICODE "end of text" control code
   */
  static final char ETX = 3;

  /**
   * End of file.
   */
  public static final int EOF = ETX;

  /* Character categories */

  /**
   * All single char tokens.
   */
  public static final BitSet bSINGLE_CHAR_TOKEN = new BitSet();

  /**
   * Non letters and non numbers, allowed in HTML names.
   */
  public static final BitSet bSPECIAL = new BitSet();

  /**
   * All letters, used in HTML names.
   */
  public static final BitSet bLETTER = new BitSet();

  /**
   * Digits.
   */
  public static final BitSet bDIGIT = new BitSet();

  /**
   * Both line breaks.
   */
  public static final BitSet bLINEBREAK = new BitSet();

  /**
   * All whitespace.
   */
  public static final BitSet bWHITESPACE = new BitSet();

  /**
   * Both quoting characters.
   */
  public static final BitSet bQUOTING = new BitSet();

  /**
   * Valid name characters.
   */
  public static final BitSet bNAME = new BitSet();

  /* Entity subcategories */

  /**
   * Named entity.
   */
  public static final int ENTITY_NAMED = 1;

  /**
   * Numeric entity.
   */
  public static final int ENTITY_NUMERIC = 2;

  static
  {
    bQUOTING.set(AP);
    bQUOTING.set(QUOT);

    bSINGLE_CHAR_TOKEN.set(BEGIN);
    bSINGLE_CHAR_TOKEN.set(END);
    bSINGLE_CHAR_TOKEN.set(EXCLAMATION);
    bSINGLE_CHAR_TOKEN.set(SLASH);
    bSINGLE_CHAR_TOKEN.set(EQ);
    bSINGLE_CHAR_TOKEN.set(EOF);

    bSINGLE_CHAR_TOKEN.or(bQUOTING);

    bLINEBREAK.set('\r');
    bLINEBREAK.set('\n');

    bWHITESPACE.set(' ');
    bWHITESPACE.set('\t');
    bWHITESPACE.set(0xC);
    bWHITESPACE.or(bLINEBREAK);

    for (char i = '0'; i <= '9'; i++)
      {
        bDIGIT.set(i);
      }

    for (char i = 'a'; i <= 'z'; i++)
      {
        bLETTER.set(i);
      }

    for (char i = 'A'; i <= 'Z'; i++)
      {
        bLETTER.set(i);
      }

    bSPECIAL.set('-');
    bSPECIAL.set('_');
    bSPECIAL.set(':');
    bSPECIAL.set('.');

    bNAME.or(bLETTER);
    bNAME.or(bDIGIT);
    bNAME.or(bSPECIAL);
  }

  /**
   * Verifies if one of the tokens matches the end of string
   * buffer. The last character in the string buffer is the
   * "future character", some tokens needs to verify it the
   * token does not continue "towards the future". If the token
   * matches, it matches till "pre-last" character in the buffer.
   * @param b
   * @return
   */
  public Token endMatches(Buffer b)
  {
    if (b.length() < 2)
      return null;

    int p = b.length() - 2;

    if (b.length() > 2 && b.charAt(p) == '-' && b.charAt(p - 1) == '-')
      return new Token(DOUBLE_DASH, "--", b.getLocation(p - 1, p + 1));

    char last = b.charAt(p);

    if (bSINGLE_CHAR_TOKEN.get(last))
      return new Token(last, last, b.getLocation(p, p + 1));

    char future = b.charAt(p + 1);

    // Check for numtokens, script and style:
    if (bNAME.get(last) && !bNAME.get(future))
      {
        // Scan the history up:
        int u = p - 1;
        while (u >= 0 && bNAME.get(b.charAt(u)))
          u--;
        u++;

        char[] token = new char[ p - u + 1 ];

        // Found a numtoken
        b.getChars(u, p + 1, token, 0);

        // Verify for the built-in tokens:
        String e = new String(token);

        // found the entity reference
        if (u > 0 && b.charAt(u - 1) == '&')
          {
            // The subsequent semicolon may be the part of the token
            // as well. The semicolon must be ignored. This must be
            // handled elsewhere.
            return new Token(ENTITY, ENTITY_NAMED, "&" + e,
                             b.getLocation(u - 1, p + 1)
                            );
          }

        // found the numeric entity reference
        if (u > 1 && b.charAt(u - 1) == '#' && b.charAt(u - 2) == '&')
          {
            // The subsequent semicolon may be the part of the token
            // as well. The semicolon must be ignored. This must be
            // handled elsewhere.
            return new Token(ENTITY, ENTITY_NUMERIC, "&#" + e,
                             b.getLocation(u - 2, p + 2)
                            );
          }

        Location le = b.getLocation(u, p + 1);

        if (e.equalsIgnoreCase("SCRIPT"))
          return new Token(SCRIPT, e, le);
        else if (e.equalsIgnoreCase("STYLE"))
          return new Token(STYLE, e, le);
        else
          return new Token(NUMTOKEN, e, le);
      }

    // Check for whitespace
    if (bWHITESPACE.get(last) && !bWHITESPACE.get(future))
      {
        // Scan the history up:
        int u = p - 1;
        while (u >= 0 && bWHITESPACE.get(b.charAt(u)))
          u--;
        u++;

        char[] token = new char[ p - u + 1 ];
        b.getChars(u, p + 1, token, 0);

        return new Token(WS, new String(token), b.getLocation(u, p + 1));
      }

    return null;
  }
}
