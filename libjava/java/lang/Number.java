/* java.lang.Number
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.lang;

import java.io.Serializable;

/**
 ** Number is a generic superclass of all the numeric classes, namely
 ** <code>Byte</code>, <code>Short</code>, <code>Integer</code>,
 ** <code>Long</code>, <code>Float</code>, and <code>Double</code>.
 **
 ** It provides ways to convert from any one value to any other.
 **
 ** @author Paul Fisher
 ** @author John Keiser
 ** @author Warren Levy
 ** @since JDK1.0
 **/
public abstract class Number implements Serializable
{
  /** Return the value of this <code>Number</code> as a <code>byte</code>.
   ** @return the value of this <code>Number</code> as a <code>byte</code>.
   **/
  public byte byteValue()
  {
    return (byte) intValue();
  }

  /** Return the value of this <code>Number</code> as a <code>short</code>.
   ** @return the value of this <code>Number</code> as a <code>short</code>.
   **/
  public short shortValue()
  {
    return (short) intValue();
  }

  /** Return the value of this <code>Number</code> as an <code>int</code>.
   ** @return the value of this <code>Number</code> as an <code>int</code>.
   **/
  public abstract int intValue();

  /** Return the value of this <code>Number</code> as a <code>long</code>.
   ** @return the value of this <code>Number</code> as a <code>long</code>.
   **/
  public abstract long longValue();

  /** Return the value of this <code>Number</code> as a <code>float</code>.
   ** @return the value of this <code>Number</code> as a <code>float</code>.
   **/
  public abstract float floatValue();

  /** Return the value of this <code>Number</code> as a <code>float</code>.
   ** @return the value of this <code>Number</code> as a <code>float</code>.
   **/
  public abstract double doubleValue();

  private static final long serialVersionUID = -8742448824652078965L;
}
