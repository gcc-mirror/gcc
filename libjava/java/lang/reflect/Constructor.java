// Constructor.java - Represents a constructor for a class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;

/**
 * The Constructor class represents a constructor of a class. It also allows
 * dynamic creation of an object, via reflection. Invocation on Constructor
 * objects knows how to do widening conversions, but throws
 * {@link IllegalArgumentException} if a narrowing conversion would be
 * necessary. You can query for information on this Constructor regardless
 * of location, but construction access may be limited by Java language
 * access controls. If you can't do it in the compiler, you can't normally
 * do it here either.<p>
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
 * @see java.lang.Class#getConstructor(Object[])
 * @see java.lang.Class#getDeclaredConstructor(Object[])
 * @see java.lang.Class#getConstructors()
 * @see java.lang.Class#getDeclaredConstructors()
 * @since 1.1
 * @status updated to 1.4
 */
public final class Constructor extends AccessibleObject implements Member
{
  /**
   * This class is uninstantiable except from native code.
   */
  private Constructor ()
  {
  }

  /**
   * Gets the class that declared this constructor.
   * @return the class that declared this member
   */
  public Class getDeclaringClass ()
  {
    return declaringClass;
  }

  /**
   * Gets the name of this constructor (the non-qualified name of the class
   * it was declared in).
   * @return the name of this constructor
   */
  public String getName ()
  {
    return declaringClass.getName();
  }

  /**
   * Gets the modifiers this constructor uses.  Use the <code>Modifier</code>
   * class to interpret the values. A constructor can only have a subset of the
   * following modifiers: public, private, protected.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  public native int getModifiers ();

  /**
   * Get the parameter list for this constructor, in declaration order. If the
   * constructor takes no parameters, returns a 0-length array (not null).
   *
   * @return a list of the types of the constructor's parameters
   */
  public Class[] getParameterTypes ()
  {
    if (parameter_types == null)
      getType ();
    return (Class[]) parameter_types.clone();
  }

  /**
   * Get the exception types this constructor says it throws, in no particular
   * order. If the constructor has no throws clause, returns a 0-length array
   * (not null).
   *
   * @return a list of the types in the constructor's throws clause
   */
  public Class[] getExceptionTypes ()
  {
    if (exception_types == null)
      getType();
    return (Class[]) exception_types.clone();
  }

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Constructors are semantically equivalent if they have the same
   * declaring class and the same parameter list.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Constructor))
      return false;
    Constructor c = (Constructor) obj;
    return declaringClass == c.declaringClass && offset == c.offset;
  }

  /**
   * Get the hash code for the Constructor.
   *
   * @return the hash code for the object
   */
  public int hashCode ()
  {
    // FIXME.
    return getName().hashCode() + declaringClass.getName().hashCode();
  }

  /**
   * Get a String representation of the Constructor. A Constructor's String
   * representation is "&lt;modifier&gt; &lt;classname&gt;(&lt;paramtypes&gt;)
   * throws &lt;exceptions&gt;", where everything after ')' is omitted if
   * there are no exceptions.<br> Example:
   * <code>public java.io.FileInputStream(java.lang.Runnable)
   * throws java.io.FileNotFoundException</code>
   *
   * @return the String representation of the Constructor
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
    Method.appendClassName (b, declaringClass);
    b.append("(");
    for (int i = 0; i < parameter_types.length; ++i)
      {
	Method.appendClassName (b, parameter_types[i]);
	if (i < parameter_types.length - 1)
	  b.append(",");
      }
    b.append(")");
    return b.toString();
  }

  /**
   * Create a new instance by invoking the constructor. Arguments are
   * automatically unwrapped and widened, if needed.<p>
   *
   * If this class is abstract, you will get an
   * <code>InstantiationException</code>. If the constructor takes 0
   * arguments, you may use null or a 0-length array for <code>args</code>.<p>
   *
   * If this Constructor enforces access control, your runtime context is
   * evaluated, and you may have an <code>IllegalAccessException</code> if
   * you could not create this object in similar compiled code. If the class
   * is uninitialized, you trigger class initialization, which may end in a
   * <code>ExceptionInInitializerError</code>.<p>
   *
   * Then, the constructor is invoked. If it completes normally, the return
   * value will be the new object. If it completes abruptly, the exception is
   * wrapped in an <code>InvocationTargetException</code>.
   *
   * @param args the arguments to the constructor
   * @return the newly created object
   * @throws IllegalAccessException if the constructor could not normally be
   *         called by the Java code (i.e. it is not public)
   * @throws IllegalArgumentException if the number of arguments is incorrect;
   *         or if the arguments types are wrong even with a widening
   *         conversion
   * @throws InstantiationException if the class is abstract
   * @throws InvocationTargetException if the constructor throws an exception
   * @throws ExceptionInInitializerError if construction triggered class
   *         initialization, which then failed
   */
  public native Object newInstance (Object[] args)
    throws InstantiationException, IllegalAccessException,
    IllegalArgumentException, InvocationTargetException;

  // Update cached values from method descriptor in class.
  private native void getType ();

  // Declaring class.
  private Class declaringClass;

  // Exception types.
  private Class[] exception_types;
  // Parameter types.
  private Class[] parameter_types;

  // Offset in bytes from the start of declaringClass's methods array.
  private int offset;
}
