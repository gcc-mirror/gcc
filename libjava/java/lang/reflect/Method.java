// Method.java - Represent method of class or interface.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;

import gnu.gcj.RawData;

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
 * @author Tom Tromey <tromey@redhat.com>
 * @see Member
 * @see Class
 * @see java.lang.Class#getMethod(String,Object[])
 * @see java.lang.Class#getDeclaredMethod(String,Object[])
 * @see java.lang.Class#getMethods()
 * @see java.lang.Class#getDeclaredMethods()
 * @since 1.1
 * @status updated to 1.4
 */
public final class Method extends AccessibleObject implements Member
{
  /**
   * This class is uninstantiable.
   */
  private Method ()
  {
  }

  /**
   * Gets the class that declared this method, or the class where this method
   * is a non-inherited member.
   * @return the class that declared this member
   */
  public Class getDeclaringClass ()
  {
    return declaringClass;
  }

  /**
   * Gets the name of this method.
   * @return the name of this method
   */
  public native String getName ();

  /**
   * Gets the modifiers this method uses.  Use the <code>Modifier</code>
   * class to interpret the values.  A method can only have a subset of the
   * following modifiers: public, private, protected, abstract, static,
   * final, synchronized, native, and strictfp.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  public native int getModifiers ();

  /**
   * Gets the return type of this method.
   * @return the type of this method
   */
  public Class getReturnType ()
  {
    if (return_type == null)
      getType();
    return return_type;
  }

  /**
   * Get the parameter list for this method, in declaration order. If the
   * method takes no parameters, returns a 0-length array (not null).
   *
   * @return a list of the types of the method's parameters
   */
  public Class[] getParameterTypes ()
  {
    if (parameter_types == null)
      getType();
    return (Class[]) parameter_types.clone();
  }

  /**
   * Get the exception types this method says it throws, in no particular
   * order. If the method has no throws clause, returns a 0-length array
   * (not null).
   *
   * @return a list of the types in the method's throws clause
   */
  public Class[] getExceptionTypes ()
  {
    if (exception_types == null)
      getType();
    return (Class[]) exception_types.clone();
  }

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Methods are semantically equivalent if they have the same declaring
   * class, name, and parameter list.  This ignores different exception
   * clauses or return types.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Method))
      return false;
    Method m = (Method) obj;
    return declaringClass == m.declaringClass && offset == m.offset;
  }

  /**
   * Get the hash code for the Method.
   *
   * @return the hash code for the object
   */
  public int hashCode ()
  {
    // FIXME.
    return getName().hashCode() + declaringClass.getName().hashCode();
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
  public String toString ()
  {
    if (parameter_types == null)
      getType ();

    StringBuffer b = new StringBuffer ();
    int mods = getModifiers();
    if (mods != 0)
      {
	Modifier.toString(mods, b);
	b.append(" ");
      }
    appendClassName (b, return_type);
    b.append(" ");
    appendClassName (b, declaringClass);
    b.append(".");
    b.append(getName());
    b.append("(");
    for (int i = 0; i < parameter_types.length; ++i)
      {
	appendClassName (b, parameter_types[i]);
	if (i < parameter_types.length - 1)
	  b.append(",");
      }
    b.append(")");
    if (exception_types.length > 0)
      {
	b.append(" throws ");
	for (int i = 0; i < exception_types.length; ++i)
	  {
	    appendClassName (b, exception_types[i]);
	    if (i < exception_types.length - 1)
	      b.append(",");
	  }
      }
    return b.toString();
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
  public native Object invoke (Object obj, Object[] args)
    throws IllegalAccessException, IllegalArgumentException,
    InvocationTargetException;

  private native void getType ();

  // Append a class name to a string buffer.  We try to print the
  // fully-qualified name, the way that a Java programmer would expect
  // it to be written.  Weirdly, Class has no appropriate method for
  // this.
  static void appendClassName (StringBuffer buf, Class k)
  {
    if (k.isArray ())
      {
	appendClassName (buf, k.getComponentType ());
	buf.append ("[]");
      }
    else
      {
	// This is correct for primitive and reference types.  Really
	// we'd like `Main$Inner' to be printed as `Main.Inner', I
	// think, but that is a pain.
	buf.append (k.getName ());
      }
  }

  // Declaring class.
  private Class declaringClass;

  // Exception types.
  private Class[] exception_types;
  // Name cache.  (Initially null.)
  private String name;
  // Parameter types.
  private Class[] parameter_types;
  // Return type.
  private Class return_type;

  // Offset in bytes from the start of declaringClass's methods array.
  private int offset;
}
