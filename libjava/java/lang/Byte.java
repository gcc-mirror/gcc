/* Byte.java -- object wrapper for byte
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


package java.lang;

/**
 * Instances of class <code>Byte</code> represent primitive <code>byte</code>
 * values.
 *
 * Additionally, this class provides various helper functions and variables
 * useful to bytes.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Per Bothner
 * @since JDK 1.0
 */
public final class Byte extends Number implements Comparable 
{
  static final long serialVersionUID = -7183698231559129828L;

  /**
   * The minimum value a <code>byte</code> can represent is -128.
   */
  public static final byte MIN_VALUE = -128;

  /**
   * The maximum value a <code>byte</code> can represent is 127.
   */
  public static final byte MAX_VALUE = 127;

  /**
   * The primitive type <code>byte</code> is represented by this 
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('B');

  /**
   * The immutable value of this Byte.
   */
  private final byte value;

  /**
   * Create a <code>Byte</code> object representing the value of the 
   * <code>byte</code> argument.
   *
   * @param value the value to use
   */     
  public Byte(byte value) 
  {
    this.value = value;
  }

  /**
   * Create a <code>Byte</code> object representing the value specified 
   * by the <code>String</code> argument.
   *
   * @param s the string to convert.
   */
  public Byte(String s) throws NumberFormatException 
  {
    value = parseByte(s, 10);
  }

  /**
   * Return a hashcode representing this Object.
   *
   * <code>Byte</code>'s hash code is calculated by simply returning its
   * value.
   *
   * @return this Object's hash code.
   */
  public int hashCode() 
  {
    return value;
  }

  /**
   * Returns <code>true</code> if <code>obj</code> is an instance of
   * <code>Byte</code> and represents the same byte value.
   * @return whether these Objects are semantically equal.
   */    
  public boolean equals(Object obj) 
  {
    return ((obj instanceof Byte) && (value == ((Byte)obj).byteValue()));
  }

  /**
   * Converts the <code>byte</code> to a <code>String</code> and assumes
   * a radix of 10.
   * @param i the <code>byte</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */    
  public static String toString(byte i) 
  {
    return Integer.toString ((int) i);
  }

  /**
   * Converts the <code>Byte</code> value to a <code>String</code> and
   * assumes a radix of 10.
   * @return the <code>String</code> representation of this <code>Byte</code>.
   */    
  public String toString() 
  {
    return Integer.toString ((int) value);
  }
    
  /**
   * Creates a new <code>Byte</code> object using the <code>String</code>,
   * assuming a radix of 10.
   * @param s the <code>String</code> to convert.
   * @return the new <code>Byte</code>.
   * @see #Byte(java.lang.String)
   * @see #parseByte(java.lang.String)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>byte</code>.
   */
  public static Byte valueOf(String s) throws NumberFormatException 
  {
    return new Byte(parseByte(s));
  }

  /**
   * Creates a new <code>Byte</code> object using the <code>String</code>
   * and specified radix (base).
   * @param s the <code>String</code> to convert.
   * @param radix the radix (base) to convert with.
   * @return the new <code>Byte</code>.
   * @see #parseByte(java.lang.String,int)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>byte</code>.
   */
  public static Byte valueOf(String s, int radix) 
    throws NumberFormatException 
  {
    return new Byte(parseByte(s, radix));
  }

  /**
   * Converts the specified <code>String</code> into a <code>byte</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>byte</code> value of the <code>String</code>
   *         argument.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>byte</code>.
   */
  public static byte parseByte(String s) throws NumberFormatException 
  {
    return parseByte(s, 10);
  }

  /**
   * Converts the specified <code>String</code> into a <code>byte</code>
   * using the specified radix (base).
   *
   * @param str the <code>String</code> to convert
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> argument converted to </code>byte</code>.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>byte</code>.    
   */
  public static byte parseByte(String str, int radix) 
    throws NumberFormatException 
  {
    int i = Integer.parseInt(str, radix);
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return (byte) i;
  }

  /**
   * Convert the specified <code>String</code> into a <code>Byte</code>.
   * The <code>String</code> may represent decimal, hexadecimal, or 
   * octal numbers.
   *
   * The <code>String</code> argument is interpreted based on the leading
   * characters.  Depending on what the String begins with, the base will be
   * interpreted differently:
   *
   * <table>
   * <tr><th>Leading<br>Characters</th><th>Base</th></tr>
   * <tr><td>#</td><td>16</td></tr>
   * <tr><td>0x</td><td>16</td></tr>
   * <tr><td>0X</td><td>16</td></tr>
   * <tr><td>0</td><td>8</td></tr>
   * <tr><td>Anything<br>Else</td><td>10</td></tr>
   * </table>
   *
   * @param str the <code>String</code> to interpret.
   * @return the value of the String as a <code>Byte</code>.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>byte</code>.    
   */
  public static Byte decode(String str) throws NumberFormatException 
  {
    int i = (Integer.decode(str)).intValue();
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return new Byte((byte) i);
  }
    
  /** Return the value of this <code>Byte</code> as an <code>short</code>.
   ** @return the value of this <code>Byte</code> as an <code>short</code>.
   **/
  public byte byteValue()
  {
    return value;
  }

  /** Return the value of this <code>Byte</code> as an <code>short</code>.
   ** @return the value of this <code>Byte</code> as an <code>short</code>.
   **/
  public short shortValue()
  {
    return value;
  }

  /** Return the value of this <code>Byte</code> as an <code>int</code>.
   ** @return the value of this <code>Byte</code> as an <code>int</code>.
   **/
  public int intValue()
  {
    return value;
  }

  /** Return the value of this <code>Byte</code> as a <code>long</code>.
   ** @return the value of this <code>Byte</code> as a <code>long</code>.
   **/
  public long longValue()
  {
    return value;
  }

  /** Return the value of this <code>Byte</code> as a <code>float</code>.
   ** @return the value of this <code>Byte</code> as a <code>float</code>.
   **/
  public float floatValue()
  {
    return value;
  }

  /** Return the value of this <code>Byte</code> as a <code>double</code>.
   ** @return the value of this <code>Byte</code> as a <code>double</code>.
   **/
  public double doubleValue()
  {
    return value;
  }
    
  /**
   * Compare two Bytes numerically by comparing their
   * <code>byte</code> values.
   * @return a positive value if this <code>Byte</code> is greater
   * in value than the argument <code>Byte</code>; a negative value
   * if this <code>Byte</code> is smaller in value than the argument
   * <code>Byte</code>; and <code>0</code>, zero, if this
   * <code>Byte</code> is equal in value to the argument
   * <code>Byte</code>.  
   */
  public int compareTo(Byte b)
  {
    return (int)(value - b.byteValue());
  }
    
  /**
   * Behaves like <code>compareTo(java.lang.Byte)</code> unless the Object
   * is not a <code>Byte</code>.  Then it throws a 
   * <code>ClassCastException</code>.
   * @exception ClassCastException if the argument is not a
   * <code>Byte</code>.  
   */
  public int compareTo(Object o)
  {
    return compareTo((Byte)o);
  }
}
