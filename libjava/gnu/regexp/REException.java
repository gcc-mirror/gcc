/* gnu/regexp/REException.java
   Copyright (C) 1998-2001, 2004 Free Software Foundation, Inc.

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

package gnu.regexp;

import java.text.MessageFormat;

/**
 * This is the regular expression exception class.  An exception of this type
 * defines the three attributes:
 * <OL>
 * <LI> A descriptive message of the error.
 * <LI> An integral type code equivalent to one of the statically
 *      defined symbols listed below.
 * <LI> The approximate position in the input string where the error
 *      occurred.
 * </OL>
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 */

public class REException extends Exception {
  private int type;
  private int pos;

  // Error conditions from GNU regcomp(3) manual

  /**
   * Error flag.
   * Invalid use of repetition operators such  as  using
   * `*' as the first character.
   */
  public static final int REG_BADRPT  =  1;

  /**
   * Error flag.
   * Invalid use of back reference operator.
   */
  public static final int REG_BADBR   =  2;

  /**
   * Error flag.
   * Un-matched brace interval operators.
   */
  public static final int REG_EBRACE  =  3;

  /**
   * Error flag.
   * Un-matched bracket list operators.
   */
  public static final int REG_EBRACK  =  4;

  /**
   * Error flag.
   * Invalid  use  of the range operator, eg. the ending
   * point of the range occurs  prior  to  the  starting
   * point.
   */
  public static final int REG_ERANGE  =  5;

  /**
   * Error flag.
   * Unknown character class name. <B>Not implemented</B>.
   */
  public static final int REG_ECTYPE  =  6;

  /**
   * Error flag.
   * Un-matched parenthesis group operators.
   */
  public static final int REG_EPAREN  =  7;

  /**
   * Error flag.
   * Invalid back reference to a subexpression.
   */
  public static final int REG_ESUBREG =  8;

  /**
   * Error flag.
   * Non specific error. <B>Not implemented</B>.
   */
  public static final int REG_EEND    =  9;

  /**
   * Error flag.
   * Invalid escape sequence. <B>Not implemented</B>.
   */
  public static final int REG_ESCAPE  = 10;

  /**
   * Error flag.
   * Invalid  use  of pattern operators such as group or list.
   */
  public static final int REG_BADPAT  = 11;

  /**
   * Error flag.
   * Compiled  regular  expression  requires  a  pattern
   * buffer larger than 64Kb. <B>Not implemented</B>.
   */
  public static final int REG_ESIZE   = 12;

  /**
   * Error flag.
   * The regex routines ran out of memory. <B>Not implemented</B>.
   */
  public static final int REG_ESPACE  = 13;

  REException(String msg, int type, int position) { 
    super(msg); 
    this.type = type;
    this.pos = position;
  }

  /**
   * Returns the type of the exception, one of the constants listed above.
   */

  public int getType() {
    return type;
  }

  /**
   * Returns the position, relative to the string or character array being
   * compiled, where the error occurred.  This position is generally the point
   * where the error was detected, not necessarily the starting index of
   * a bad subexpression.
   */
  public int getPosition() {
    return pos;
  }

  /**
   * Reports the descriptive message associated with this exception
   * as well as its index position in the string or character array
   * being compiled.
   */
  public String getMessage() {
    Object[] args = {new Integer(pos)};
    StringBuffer sb = new StringBuffer();
    String prefix = RE.getLocalizedMessage("error.prefix");
    sb.append(MessageFormat.format(prefix, args));
    sb.append('\n');
    sb.append(super.getMessage());
    return sb.toString();
  }
}
