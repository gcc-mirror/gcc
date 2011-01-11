/* TypeSignature.java -- Class used to compute type signatures
   Copyright (C) 1998, 2000, 2002 Free Software Foundation, Inc.

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


package gnu.java.lang.reflect;

import gnu.java.lang.CPStringBuilder;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;

/**
 * This class provides static methods that can be used to compute
 * type-signatures of <code>Class</code>s or <code>Member</code>s.
 * More specific methods are also provided for computing the
 * type-signature of <code>Constructor</code>s and
 * <code>Method</code>s.  Methods are also provided to go in the
 * reverse direction.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 */
public class TypeSignature
{
  /**
   * Returns a <code>String</code> representing the type-encoding of a class.
   * The .class file format has different encodings for classes, depending
   * on whether it must be disambiguated from primitive types or not; hence
   * the descriptor parameter to choose between them. If you are planning
   * on decoding primitive types along with classes, then descriptor should
   * be true for correct results. Type-encodings are computed as follows:
   *
   * <pre>
   * boolean -> "Z"
   * byte    -> "B"
   * char    -> "C"
   * double  -> "D"
   * float   -> "F"
   * int     -> "I"
   * long    -> "J"
   * short   -> "S"
   * void    -> "V"
   * arrays  -> "[" + descriptor format of component type
   * object  -> class format: fully qualified name with '.' replaced by '/'
   *            descriptor format: "L" + class format + ";"
   * </pre>
   *
   * @param type the class name to encode
   * @param descriptor true to return objects in descriptor format
   * @return the class name, as it appears in bytecode constant pools
   * @see #getClassForEncoding(String)
   */
  public static String getEncodingOfClass(String type, boolean descriptor)
  {
    if (! descriptor || type.charAt(0) == '[')
      return type.replace('.', '/');
    if (type.equals("boolean"))
      return "Z";
    if (type.equals("byte"))
      return "B";
    if (type.equals("short"))
      return "S";
    if (type.equals("char"))
      return "C";
    if (type.equals("int"))
      return "I";
    if (type.equals("long"))
      return "J";
    if (type.equals("float"))
      return "F";
    if (type.equals("double"))
      return "D";
    if (type.equals("void"))
      return "V";
    return 'L' + type.replace('.', '/') + ';';
  }

  /**
   * Gets the descriptor encoding for a class.
   *
   * @param clazz the class to encode
   * @param descriptor true to return objects in descriptor format
   * @return the class name, as it appears in bytecode constant pools
   * @see #getEncodingOfClass(String, boolean)
   */
  public static String getEncodingOfClass(Class clazz, boolean descriptor)
  {
    return getEncodingOfClass(clazz.getName(), descriptor);
  }

  /**
   * Gets the descriptor encoding for a class.
   *
   * @param clazz the class to encode
   * @return the class name, as it appears in bytecode constant pools
   * @see #getEncodingOfClass(String, boolean)
   */
  public static String getEncodingOfClass(Class clazz)
  {
    return getEncodingOfClass(clazz.getName(), true);
  }


  /**
   * This function is the inverse of <code>getEncodingOfClass</code>. This
   * accepts both object and descriptor formats, but must know which style
   * of string is being passed in (usually, descriptor should be true). In
   * descriptor format, "I" is treated as int.class, in object format, it
   * is treated as a class named I in the unnamed package. This method is
   * strictly equivalent to {@link #getClassForEncoding(java.lang.String, boolean, java.lang.ClassLoader)}
   * with a class loader equal to <code>null</code>. In that case, it
   * uses the default class loader on the calling stack.
   *
   * @param type_code the class name to decode
   * @param descriptor if the string is in descriptor format
   * @return the corresponding Class object
   * @throws ClassNotFoundException if the class cannot be located
   * @see #getEncodingOfClass(Class, boolean)
   */
  public static Class getClassForEncoding(String type_code, boolean descriptor)
    throws ClassNotFoundException
  {
    return getClassForEncoding(type_code, descriptor, null);
  }

  /**
   * This function is the inverse of <code>getEncodingOfClass</code>. This
   * accepts both object and descriptor formats, but must know which style
   * of string is being passed in (usually, descriptor should be true). In
   * descriptor format, "I" is treated as int.class, in object format, it
   * is treated as a class named I in the unnamed package.
   *
   * @param type_code The class name to decode.
   * @param descriptor If the string is in descriptor format.
   * @param loader The class loader when resolving generic object name. If
   * <code>loader</code> is null then it uses the default class loader on the
   * calling stack.
   * @return the corresponding Class object.
   * @throws ClassNotFoundException if the class cannot be located.
   * @see #getEncodingOfClass(Class, boolean)
   * @see #getClassForEncoding(String, boolean)
   */
  public static Class getClassForEncoding(String type_code, boolean descriptor,
                                          ClassLoader loader)
    throws ClassNotFoundException
  {
    if (descriptor)
      {
        switch (type_code.charAt(0))
          {
          case 'B':
            return byte.class;
          case 'C':
            return char.class;
          case 'D':
            return double.class;
          case 'F':
            return float.class;
          case 'I':
            return int.class;
          case 'J':
            return long.class;
          case 'S':
            return short.class;
          case 'V':
            return void.class;
          case 'Z':
            return boolean.class;
          default:
            throw new ClassNotFoundException("Invalid class name: "
                                             + type_code);
          case 'L':
            type_code = type_code.substring(1, type_code.length() - 1);
            // Fallthrough.
          case '[':
          }
      }
    return Class.forName(type_code.replace('/', '.'), true, loader);
  }

  /**
   * Gets the Class object for a type name.
   *
   * @param type_code the class name to decode
   * @return the corresponding Class object
   * @throws ClassNotFoundException if the class cannot be located
   * @see #getClassForEncoding(String, boolean)
   */
  public static Class getClassForEncoding(String type_code)
    throws ClassNotFoundException
  {
    return getClassForEncoding(type_code, true);
  }

  /**
   * Returns a <code>String</code> representing the type-encoding of a
   * method.  The type-encoding of a method is:
   *
   * "(" + parameter type descriptors + ")" + return type descriptor
   *
   * XXX This could be faster if it were implemented natively.
   *
   * @param m the method to encode
   * @return the encoding
   */
  public static String getEncodingOfMethod(Method m)
  {
    Class[] paramTypes = m.getParameterTypes();
    CPStringBuilder buf = new CPStringBuilder("(");
    for (int i = 0; i < paramTypes.length; i++)
      buf.append(getEncodingOfClass(paramTypes[i].getName(), true));
    buf.append(')').append(getEncodingOfClass(m.getReturnType().getName(),
                                              true));
    return buf.toString();
  }

  /**
   * Returns a <code>String</code> representing the type-encoding of a
   * constructor. The type-encoding of a method is:
   *
   * "(" + parameter type descriptors + ")V"
   *
   * XXX This could be faster if it were implemented natively.
   *
   * @param c the constructor to encode
   * @return the encoding
   */
  public static String getEncodingOfConstructor(Constructor c)
  {
    Class[] paramTypes = c.getParameterTypes();
    CPStringBuilder buf = new CPStringBuilder("(");
    for (int i = 0; i < paramTypes.length; i++)
      buf.append(getEncodingOfClass(paramTypes[i].getName(), true));
    buf.append(")V");
    return buf.toString();
  }

  /**
   * Returns a <code>String</code> representing the type-encoding of a
   * class member. This appropriately handles Constructors, Methods, and
   * Fields.
   *
   * @param mem the member to encode
   * @return the encoding
   */
  public static String getEncodingOfMember(Member mem)
  {
    if (mem instanceof Constructor)
      return getEncodingOfConstructor((Constructor) mem);
    if (mem instanceof Method)
      return getEncodingOfMethod((Method) mem);
    else // Field
      return getEncodingOfClass(((Field) mem).getType().getName(), true);
  }
} // class TypeSignature
