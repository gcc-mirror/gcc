/* Facet.java -- 
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.xml.validation.datatype;

/**
 * An XML Schema constraining facet.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class Facet
{

  public static final int LENGTH = 1;
  public static final int MIN_LENGTH = 2;
  public static final int MAX_LENGTH = 3;
  public static final int PATTERN = 4;
  public static final int ENUMERATION = 5;
  public static final int WHITESPACE = 6;
  public static final int MAX_INCLUSIVE = 7;
  public static final int MAX_EXCLUSIVE = 8;
  public static final int MIN_EXCLUSIVE = 9;
  public static final int MIN_INCLUSIVE = 10;
  public static final int TOTAL_DIGITS = 11;
  public static final int FRACTION_DIGITS = 12;

  /**
   * The type of this facet.
   */
  public final int type;

  /**
   * Optional annotation.
   */
  public Annotation annotation;

  protected Facet(int type, Annotation annotation)
  {
    this.type = type;
    this.annotation = annotation;
  }
  
}

