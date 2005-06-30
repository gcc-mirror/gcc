/* Token.java --
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

/**
 * A token.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Token
{
  /**
   * The place of this token in the document.
   */
  public Location where;

  /**
   * The additional category of token.
   */
  public int category;

  /**
   * An integer that describes the kind of this token.
   */
  public int kind;

  /**
   * The string image of the token, null if the char image must be used.
   */
  private String stringImage;

  /**
   * The char image of the token.
   */
  private char charImage;

  /**
   * Creates a new token with fields, initialized to the default values.
   */
  public Token()
  {
  }

  /**
   * Creates a new token of the given kind.
   */
  public Token(int _kind, Location _where)
  {
    kind = _kind;
    where = _where;
  }

  /**
   * Creates a new token of the given kind and given single char image.
   */
  public Token(int _kind, char _image, Location _where)
  {
    kind = _kind;
    charImage = _image;
    where = _where;
  }

  /**
   * Creates a new token of the given kind and given string image.
   */
  public Token(int _kind, String _image, Location _where)
  {
    kind = _kind;
    stringImage = _image;
    where = _where;
  }

  /**
   * Creates a new token of the given kind, category and given string image.
   */
  public Token(int _kind, int _category, String _image, Location _where)
  {
    kind = _kind;
    category = _category;
    stringImage = _image;
    where = _where;
  }

  /**
   * Creates a new token, where location fields are set as for token,
   * spanning over two provided tokens and any tokens between them.
   * The image field is initialized to null, the kind field is set to -1.
   */
  public Token(Token fromInclusive, Token toInclusive)
  {
    where = new Location();
    where.beginLine = fromInclusive.where.beginLine;
    where.startPosition = fromInclusive.where.startPosition;

    where.endLine = toInclusive.where.endLine;
    where.endPosition = toInclusive.where.endPosition;
  }

  public String getImage()
  {
    if (kind == 3)
      return "#";
    if (stringImage == null)
      {
        if (charImage == 0)
          return null;
        stringImage = new String(new char[] { charImage });
      }
    return stringImage;
  }

  /**
   * Append the token image to the given string buffer.
   * This may be more effective that buffer.append(this.getImage()).
   * @param buffer A buffer to append.
   */
  public void appendTo(StringBuffer buffer)
  {
    if (charImage == 0)
      buffer.append(getImage());
    else
      buffer.append(charImage);
  }

  /**
   * Returns the string image or, if null, the bounding positions.
   */
  public String toString()
  {
    return getImage() != null ? kind + "'" + getImage()
           : "<line " + where.beginLine + ", abs pos " + where.startPosition +
           ".." + where.endPosition + ">";
  }
}
