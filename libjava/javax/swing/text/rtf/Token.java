/* Token.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package javax.swing.text.rtf;

/**
 * Represents a simple token that the RTFScanner can read. A simple
 * only has a type (like LCURLY or RCURLY). More complex tokens may
 * attach data to the token.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
class Token
{

  /**
   * This special type inidicates the end of the input stream.
   */
  public static final int EOF = -1;

  /**
   * A left curly brace '{'.
   */
  public static final int LCURLY = 1;

  /**
   * A right curly brace '}'.
   */
  public static final int RCURLY = 2;

  /**
   * A control word like '\rtf1'. Tokens with this type are represented
   * through the subclass {@link ControlWordToken}.
   */
  public static final int CONTROL_WORD = 3;

  /**
   * A token that contains text. This is represented through the subclass
   * {@link TextToken}.
   */
  public static final int TEXT = 4;


  /** The token type. */
  public int type;

  /**
   * Constructs a new Token with the specified type.
   *
   * @param type the Token type
   */
  public Token(int type)
  {
    this.type = type;
  }
}
