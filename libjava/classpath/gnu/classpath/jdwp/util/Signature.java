/* Signature.java -- utility class to compute class and method signatures
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.util;

import gnu.java.lang.CPStringBuilder;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * A class to compute class and method signatures.
 *
 * @author Tom Tromey  (tromey@redhat.com)
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class Signature
{
  /**
   * Computes the class signature, i.e., java.lang.String.class
   * returns "Ljava/lang/String;".
   *
   * @param theClass  the class for which to compute the signature
   * @return          the class's type signature
   */
  public static String computeClassSignature (Class theClass)
  {
    CPStringBuilder sb = new CPStringBuilder ();
    _addToSignature (sb, theClass);
    return sb.toString ();
  }

  /**
   * Computes the field signature which is just the class signature of the
   * field's type, ie a Field of type java.lang.String this will return
   * "Ljava/lang/String;".
   *
   * @param field  the field for which to compute the signature
   * @return       the field's type signature
   */
  public static String computeFieldSignature (Field field)
  {
    return computeClassSignature (field.getType());
  }

  /**
   * Computes the method signature, i.e., java.lang.String.split (String, int)
   * returns "(Ljava/lang/String;I)[Ljava/lang/String;"
   *
   * @param method  the method for which to compute the signature
   * @return        the method's type signature
   */
  public static String computeMethodSignature (Method method)
  {
    return _computeSignature (method.getReturnType (),
			      method.getParameterTypes ());
  }

  private static String _computeSignature (Class returnType,
					   Class[] paramTypes)
  {
    CPStringBuilder sb = new CPStringBuilder ("(");
    if (paramTypes != null)
      {
	for (int i = 0; i < paramTypes.length; ++i)
	  _addToSignature (sb, paramTypes[i]);
      }
    sb.append (")");
    _addToSignature (sb, returnType);
    return sb.toString();
  }

  private static void _addToSignature (CPStringBuilder sb, Class k)
  {
    // For some reason there's no easy way to get the signature of a
    // class.
    if (k.isPrimitive ())
      {
        if (k == void.class)
          sb.append('V');
        else if (k == boolean.class)
          sb.append('Z');
        else if (k == byte.class)
          sb.append('B');
        else if (k == char.class)
          sb.append('C');
        else if (k == short.class)
          sb.append('S');
        else if (k == int.class)
          sb.append('I');
        else if (k == float.class)
          sb.append('F');
        else if (k == double.class)
          sb.append('D');
        else if (k == long.class)
          sb.append('J');
        return;
      }
    
    String name = k.getName ();
    int len = name.length ();
    sb.ensureCapacity (len);
    if (! k.isArray ())
      sb.append('L');
    for (int i = 0; i < len; ++i)
      {
	char c = name.charAt (i);
	if (c == '.')
	  c = '/';
	sb.append (c);
      }
    if (! k.isArray ())
      sb.append(';');
  }
}
