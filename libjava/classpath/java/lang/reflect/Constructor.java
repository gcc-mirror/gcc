/* java.lang.reflect.Constructor - reflection of Java constructors
   Copyright (C) 1998, 2001, 2004, 2005 Free Software Foundation, Inc.

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

import gnu.java.lang.ClassHelper;
import gnu.java.lang.CPStringBuilder;

import gnu.java.lang.reflect.MethodSignatureParser;

import java.lang.annotation.Annotation;

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
 * @see Member
 * @see Class
 * @see java.lang.Class#getConstructor(Class[])
 * @see java.lang.Class#getDeclaredConstructor(Class[])
 * @see java.lang.Class#getConstructors()
 * @see java.lang.Class#getDeclaredConstructors()
 * @since 1.1
 * @status updated to 1.4
 */
public final class Constructor<T>
  extends AccessibleObject
  implements GenericDeclaration, Member
{  
  private static final int CONSTRUCTOR_MODIFIERS
    = Modifier.PRIVATE | Modifier.PROTECTED | Modifier.PUBLIC;

  private MethodSignatureParser p;

  VMConstructor cons;

  /**
   * This class is uninstantiable outside this package.
   */
  Constructor(VMConstructor cons)
  {
    this.cons = cons;
    cons.cons = this;
  }

  private Constructor()
  {
  }

  /**
   * Gets the class that declared this constructor.
   * @return the class that declared this member
   */
  public Class<T> getDeclaringClass()
  {
    // Inescapable as the VM layer is 1.4 based. 
    @SuppressWarnings("unchecked")
      Class<T> declClass = (Class<T>) cons.getDeclaringClass();
    return declClass;
  }

  /**
   * Gets the name of this constructor (the non-qualified name of the class
   * it was declared in).
   * @return the name of this constructor
   */
  public String getName()
  {
    return cons.getDeclaringClass().getName();
  }

  /**
   * Gets the modifiers this constructor uses.  Use the <code>Modifier</code>
   * class to interpret the values. A constructor can only have a subset of the
   * following modifiers: public, private, protected.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  public int getModifiers()
  {
    return cons.getModifiersInternal() & CONSTRUCTOR_MODIFIERS;
  }

  /**
   * Return true if this constructor is synthetic, false otherwise.
   * A synthetic member is one which is created by the compiler,
   * and which does not appear in the user's source code.
   * @since 1.5
   */
  public boolean isSynthetic()
  {
    return (cons.getModifiersInternal() & Modifier.SYNTHETIC) != 0;
  }

  /**
   * Return true if this is a varargs constructor, that is if
   * the constructor takes a variable number of arguments.
   * @since 1.5
   */
  public boolean isVarArgs()
  {
    return (cons.getModifiersInternal() & Modifier.VARARGS) != 0;
  }

  /**
   * Get the parameter list for this constructor, in declaration order. If the
   * constructor takes no parameters, returns a 0-length array (not null).
   *
   * @return a list of the types of the constructor's parameters
   */
  public Class<?>[] getParameterTypes()
  {
    return (Class<?>[]) cons.getParameterTypes();
  }

  /**
   * Get the exception types this constructor says it throws, in no particular
   * order. If the constructor has no throws clause, returns a 0-length array
   * (not null).
   *
   * @return a list of the types in the constructor's throws clause
   */
  public Class<?>[] getExceptionTypes()
  {
    return (Class<?>[]) cons.getExceptionTypes();
  }

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Constructors are semantically equivalent if they have the same
   * declaring class and the same parameter list.  This ignores different
   * exception clauses, but since you can't create a Method except through the
   * VM, this is just the == relation.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not.
   */
  public boolean equals(Object o)
  {
    return cons.equals(o);
  }

  /**
   * Get the hash code for the Constructor. The Constructor hash code is the
   * hash code of the declaring class's name.
   *
   * @return the hash code for the object
   */
  public int hashCode()
  {
    return getName().hashCode();
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
  public String toString()
  {
    // 128 is a reasonable buffer initial size for constructor
    CPStringBuilder sb = new CPStringBuilder(128);
    Modifier.toString(getModifiers(), sb).append(' ');
    sb.append(getDeclaringClass().getName()).append('(');
    Class[] c = getParameterTypes();
    if (c.length > 0)
      {
        sb.append(ClassHelper.getUserName(c[0]));
        for (int i = 1; i < c.length; i++)
          sb.append(',').append(ClassHelper.getUserName(c[i]));
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

  static <X extends GenericDeclaration>
  void addTypeParameters(CPStringBuilder sb, TypeVariable<X>[] typeArgs)
  {
    if (typeArgs.length == 0)
      return;
    sb.append('<');
    for (int i = 0; i < typeArgs.length; ++i)
      {
        if (i > 0)
          sb.append(',');
        sb.append(typeArgs[i]);
      }
    sb.append("> ");
  }

  public String toGenericString()
  {
    CPStringBuilder sb = new CPStringBuilder(128);
    Modifier.toString(getModifiers(), sb).append(' ');
    addTypeParameters(sb, getTypeParameters());
    sb.append(getDeclaringClass().getName()).append('(');
    Type[] types = getGenericParameterTypes();
    if (types.length > 0)
      {
        sb.append(types[0]);
        for (int i = 1; i < types.length; ++i)
          sb.append(',').append(types[i]);
      }
    sb.append(')');
    types = getGenericExceptionTypes();
    if (types.length > 0)
      {
        sb.append(" throws ").append(types[0]);
        for (int i = 1; i < types.length; i++)
          sb.append(',').append(types[i]);
      }
    return sb.toString();
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
  public T newInstance(Object... args)
    throws InstantiationException, IllegalAccessException,
           InvocationTargetException
  {
    // Inescapable as the VM layer is 1.4 based. 
    @SuppressWarnings("unchecked")
      T ins = (T) cons.construct(args);
    return ins;
  }

  /**
   * Returns an array of <code>TypeVariable</code> objects that represents
   * the type variables declared by this constructor, in declaration order.
   * An array of size zero is returned if this constructor has no type
   * variables.
   *
   * @return the type variables associated with this constructor.
   * @throws GenericSignatureFormatError if the generic signature does
   *         not conform to the format specified in the Virtual Machine
   *         specification, version 3.
   * @since 1.5
   */
  public TypeVariable<Constructor<T>>[] getTypeParameters()
  {
    if (p == null)
      {
	String sig = cons.getSignature();
	if (sig == null)
	  return new TypeVariable[0];
	p = new MethodSignatureParser(this, sig);
      }
    return p.getTypeParameters();
  }

  /**
   * Returns an array of <code>Type</code> objects that represents
   * the exception types declared by this constructor, in declaration order.
   * An array of size zero is returned if this constructor declares no
   * exceptions.
   *
   * @return the exception types declared by this constructor. 
   * @throws GenericSignatureFormatError if the generic signature does
   *         not conform to the format specified in the Virtual Machine
   *         specification, version 3.
   * @since 1.5
   */
  public Type[] getGenericExceptionTypes()
  {
    if (p == null)
      {
	String sig = cons.getSignature();
	if (sig == null)
	  return getExceptionTypes();
	p = new MethodSignatureParser(this, sig);
      }
    return p.getGenericExceptionTypes();
  }

  /**
   * Returns an array of <code>Type</code> objects that represents
   * the parameter list for this constructor, in declaration order.
   * An array of size zero is returned if this constructor takes no
   * parameters.
   *
   * @return a list of the types of the constructor's parameters
   * @throws GenericSignatureFormatError if the generic signature does
   *         not conform to the format specified in the Virtual Machine
   *         specification, version 3.
   * @since 1.5
   */
  public Type[] getGenericParameterTypes()
  {
    if (p == null)
      {
	String sig = cons.getSignature();
	if (sig == null)
	  return getParameterTypes();
	p = new MethodSignatureParser(this, sig);
      }
    return p.getGenericParameterTypes();
  }

  /**
   * <p>
   * Return an array of arrays representing the annotations on each
   * of the constructor's parameters.  The outer array is aligned against
   * the parameters of the constructors and is thus equal in length to
   * the number of parameters (thus having a length zero if there are none).
   * Each array element in the outer array contains an inner array which
   * holds the annotations.  This array has a length of zero if the parameter
   * has no annotations.
   * </p>
   * <p>
   * The returned annotations are serialized.  Changing the annotations has
   * no affect on the return value of future calls to this method.
   * </p>
   * 
   * @return an array of arrays which represents the annotations used on the
   *         parameters of this constructor.  The order of the array elements
   *         matches the declaration order of the parameters.
   * @since 1.5
   */
  public Annotation[][] getParameterAnnotations()
  {
    return cons.getParameterAnnotations();
  }

  /**
   * Returns the element's annotation for the specified annotation type,
   * or <code>null</code> if no such annotation exists.
   *
   * @param annotationClass the type of annotation to look for.
   * @return this element's annotation for the specified type, or
   *         <code>null</code> if no such annotation exists.
   * @throws NullPointerException if the annotation class is <code>null</code>.
   */
  public <T extends Annotation> T getAnnotation(Class<T> annotationClass)
  {
    // Inescapable as the VM layer is 1.4 based. 
    @SuppressWarnings("unchecked")
      T ann = (T) cons.getAnnotation(annotationClass);
    return ann;
  }

  /**
   * Returns all annotations directly defined by the element.  If there are
   * no annotations directly associated with the element, then a zero-length
   * array will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * class, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return the annotations directly defined by the element.
   * @since 1.5
   */
  public Annotation[] getDeclaredAnnotations()
  {
    return cons.getDeclaredAnnotations();
  }

}
