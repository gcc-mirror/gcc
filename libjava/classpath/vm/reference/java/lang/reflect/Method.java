/* java.lang.reflect.Method - reflection of Java methods
   Copyright (C) 1998, 2001, 2002 Free Software Foundation, Inc.

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


package java.lang.reflect;

import java.util.Arrays;

/**
 * The Method class represents a member method of a class. It also allows
 * dynamic invocation, via reflection. This works for both static and
 * instance methods. Invocation on Method objects knows how to do
 * widening conversions, but throws {@link IllegalArgumentException} if
 * a narrowing conversion would be necessary. You can query for information
 * on this Method regardless of location, but invocation access may be limited
 * by Java language access controls. If you can't do it in the compiler, you
 * can't normally do it here either.<p>
 *
 * <B>Note:</B> This class returns and accepts types as Classes, even
 * primitive types; there are Class types defined that represent each
 * different primitive type.  They are <code>java.lang.Boolean.TYPE,
 * java.lang.Byte.TYPE,</code>, also available as <code>boolean.class,
 * byte.class</code>, etc.  These are not to be confused with the
 * classes <code>java.lang.Boolean, java.lang.Byte</code>, etc., which are
 * real classes.<p>
 *
 * Also note that this is not a serializable class.  It is entirely feasible
 * to make it serializable using the Externalizable interface, but this is
 * on Sun, not me.
 *
 * @author John Keiser
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Member
 * @see Class
 * @see java.lang.Class#getMethod(String,Object[])
 * @see java.lang.Class#getDeclaredMethod(String,Object[])
 * @see java.lang.Class#getMethods()
 * @see java.lang.Class#getDeclaredMethods()
 * @since 1.1
 * @status updated to 1.4
 */
public final class Method
extends AccessibleObject implements Member
{
  Class declaringClass;
  String name;
  int slot;

  /**
   * This class is uninstantiable.
   */
  private Method(Class declaringClass, String name, int slot)
  {
    this.declaringClass = declaringClass;
    this.name = name;
    this.slot = slot;
  }

  /**
   * Gets the class that declared this method, or the class where this method
   * is a non-inherited member.
   * @return the class that declared this member
   */
  public Class getDeclaringClass()
  {
    return declaringClass;
  }

  /**
   * Gets the name of this method.
   * @return the name of this method
   */
  public String getName()
  {
    return name;
  }

  /**
   * Gets the modifiers this method uses.  Use the <code>Modifier</code>
   * class to interpret the values.  A method can only have a subset of the
   * following modifiers: public, private, protected, abstract, static,
   * final, synchronized, native, and strictfp.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  public native int getModifiers();

  /**
   * Gets the return type of this method.
   * @return the type of this method
   */
  public native Class getReturnType();

  /**
   * Get the parameter list for this method, in declaration order. If the
   * method takes no parameters, returns a 0-length array (not null).
   *
   * @return a list of the types of the method's parameters
   */
  public native Class[] getParameterTypes();

  /**
   * Get the exception types this method says it throws, in no particular
   * order. If the method has no throws clause, returns a 0-length array
   * (not null).
   *
   * @return a list of the types in the method's throws clause
   */
  public native Class[] getExceptionTypes();

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Methods are semantically equivalent if they have the same declaring
   * class, name, parameter list, and return type.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not
   */
  public boolean equals(Object o)
  {
      // Implementation note:
      // The following is a correct but possibly slow implementation.
      //
      // This class has a private field 'slot' that could be used by
      // the VM implementation to "link" a particular method to a Class.
      // In that case equals could be simply implemented as:
      //
      // if (o instanceof Method)
      // {
      //    Method m = (Method)o;
      //    return m.declaringClass == this.declaringClass
      //           && m.slot == this.slot;
      // }
      // return false;
      //
      // If a VM uses the Method class as their native/internal representation
      // then just using the following would be optimal:
      //
      // return this == o;
      //
    if (!(o instanceof Method))
      return false;
    Method that = (Method)o;
    if (this.getDeclaringClass() != that.getDeclaringClass())
      return false;
    if (!this.getName().equals(that.getName()))
      return false;
    if (this.getReturnType() != that.getReturnType())
      return false;
    if (!Arrays.equals(this.getParameterTypes(), that.getParameterTypes()))
      return false;
    return true;
  }

  /**
   * Get the hash code for the Method. The Method hash code is the hash code
   * of its name XOR'd with the hash code of its class name.
   *
   * @return the hash code for the object
   */
  public int hashCode()
  {
    return getDeclaringClass().getName().hashCode() ^ getName().hashCode();
  }

  /**
   * Get a String representation of the Method. A Method's String
   * representation is "&lt;modifiers&gt; &lt;returntype&gt;
   * &lt;methodname&gt;(&lt;paramtypes&gt;) throws &lt;exceptions&gt;", where
   * everything after ')' is omitted if there are no exceptions.<br> Example:
   * <code>public static int run(java.lang.Runnable,int)</code>
   *
   * @return the String representation of the Method
   */
  public String toString()
  {
    // 128 is a reasonable buffer initial size for constructor
    StringBuffer sb = new StringBuffer(128);
    Modifier.toString(getModifiers(), sb).append(' ');
    sb.append(getUserTypeName(getReturnType().getName())).append(' ');
    sb.append(getDeclaringClass().getName()).append('.');
    sb.append(getName()).append('(');
    Class[] c = getParameterTypes();
    if (c.length > 0)
      {
        sb.append(getUserTypeName(c[0].getName()));
        for (int i = 1; i < c.length; i++)
          sb.append(',').append(getUserTypeName(c[i].getName()));
      }
    sb.append(')');
    c = getExceptionTypes();
    if (c.length > 0)
      {
        sb.append(" throws ").append(c[0].getName());
        for (int i = 1; i < c.length; i++)
          sb.append(',').append(c[i].getName());
      }
    return sb.toString();
  }

  private static String getUserTypeName(String typeSpec)
  {
    int pos = 0;
    String typeName = "";
    String arrayPart = "";

    while (typeSpec.charAt(pos) == '[')
      {
	arrayPart += "[]";
	++pos;
      }

    switch (typeSpec.charAt(pos))
      {
      case 'Z':
	typeName = "boolean";
	break;
      case 'B':
	typeName = "byte";
	break;
      case 'C':
	typeName = "char";
	break;
      case 'D':
	typeName = "double";
	break;
      case 'F':
	typeName = "float";
	break;
      case 'I':
	typeName = "int";
	break;
      case 'J':
	typeName = "long";
	break;
      case 'S':
	typeName = "short";
	break;
      case 'L':
	typeName = typeSpec.substring(pos + 1, typeSpec.length() - 1);
	break;
      default:
	typeName = typeSpec;
	break;
      }

    return typeName + arrayPart;
  }

  /**
   * Invoke the method. Arguments are automatically unwrapped and widened,
   * and the result is automatically wrapped, if needed.<p>
   *
   * If the method is static, <code>o</code> will be ignored. Otherwise,
   * the method uses dynamic lookup as described in JLS 15.12.4.4. You cannot
   * mimic the behavior of nonvirtual lookup (as in super.foo()). This means
   * you will get a <code>NullPointerException</code> if <code>o</code> is
   * null, and an <code>IllegalArgumentException</code> if it is incompatible
   * with the declaring class of the method. If the method takes 0 arguments,
   * you may use null or a 0-length array for <code>args</code>.<p>
   *
   * Next, if this Method enforces access control, your runtime context is
   * evaluated, and you may have an <code>IllegalAccessException</code> if
   * you could not acces this method in similar compiled code. If the method
   * is static, and its class is uninitialized, you trigger class
   * initialization, which may end in a
   * <code>ExceptionInInitializerError</code>.<p>
   *
   * Finally, the method is invoked. If it completes normally, the return value
   * will be null for a void method, a wrapped object for a primitive return
   * method, or the actual return of an Object method. If it completes
   * abruptly, the exception is wrapped in an
   * <code>InvocationTargetException</code>.
   *
   * @param o the object to invoke the method on
   * @param args the arguments to the method
   * @return the return value of the method, wrapped in the appropriate
   *         wrapper if it is primitive
   * @throws IllegalAccessException if the method could not normally be called
   *         by the Java code (i.e. it is not public)
   * @throws IllegalArgumentException if the number of arguments is incorrect;
   *         if the arguments types are wrong even with a widening conversion;
   *         or if <code>o</code> is not an instance of the class or interface
   *         declaring this method
   * @throws InvocationTargetException if the method throws an exception
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static method triggered
   *         class initialization, which then failed
   */
  public Object invoke(Object o, Object[] args)
    throws IllegalAccessException, InvocationTargetException
  {
    return invokeNative(o, args, declaringClass, slot);
  }

  /*
   * NATIVE HELPERS
   */

  private native Object invokeNative(Object o, Object[] args,
                                     Class declaringClass, int slot)
    throws IllegalAccessException, InvocationTargetException;
}
