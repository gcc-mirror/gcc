/* java.lang.reflect.Field - reflection of Java fields
   Copyright (C) 1998, 2001, 2005, 2006 Free Software Foundation, Inc.

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

import gnu.java.lang.reflect.FieldSignatureParser;
import java.lang.annotation.Annotation;

/**
 * The Field class represents a member variable of a class. It also allows
 * dynamic access to a member, via reflection. This works for both
 * static and instance fields. Operations on Field objects know how to
 * do widening conversions, but throw {@link IllegalArgumentException} if
 * a narrowing conversion would be necessary. You can query for information
 * on this Field regardless of location, but get and set access may be limited
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
 * @author Per Bothner <bothner@cygnus.com>
 * @see Member
 * @see Class
 * @see Class#getField(String)
 * @see Class#getDeclaredField(String)
 * @see Class#getFields()
 * @see Class#getDeclaredFields()
 * @since 1.1
 * @status updated to 1.4
 */
public final class Field
  extends AccessibleObject implements Member
{
  private Class declaringClass;
  private String name;

  // Offset in bytes from the start of declaringClass's fields array.
  private int offset;

  // The Class (or primitive TYPE) of this field.
  private Class type;

  static final int FIELD_MODIFIERS
    = Modifier.FINAL | Modifier.PRIVATE | Modifier.PROTECTED
      | Modifier.PUBLIC | Modifier.STATIC | Modifier.TRANSIENT
      | Modifier.VOLATILE;

  // This is instantiated by Class sometimes, but it uses C++ and
  // avoids the Java protection check.
  Field ()
  {
  }

  /**
   * Gets the class that declared this field, or the class where this field
   * is a non-inherited member.
   * @return the class that declared this member
   */
  public Class<?> getDeclaringClass()
  {
    return declaringClass;
  }

  /**
   * Gets the name of this field.
   * @return the name of this field
   */
  public native String getName();

  /**
   * Return the raw modifiers for this field.
   * @return the field's modifiers
   */
  private native int getModifiersInternal();

  /**
   * Gets the modifiers this field uses.  Use the <code>Modifier</code>
   * class to interpret the values.  A field can only have a subset of the
   * following modifiers: public, private, protected, static, final,
   * transient, and volatile.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  public int getModifiers()
  {
    return getModifiersInternal() & FIELD_MODIFIERS;
  }

  /**
   * Return true if this field is synthetic, false otherwise.
   * @since 1.5
   */
  public boolean isSynthetic()
  {
    return (getModifiersInternal() & Modifier.SYNTHETIC) != 0;
  }

  /**
   * Return true if this field represents an enum constant,
   * false otherwise.
   * @since 1.5
   */
  public boolean isEnumConstant()
  {
    return (getModifiersInternal() & Modifier.ENUM) != 0;
  }

  /**
   * Gets the type of this field.
   * @return the type of this field
   */
  public native Class<?> getType();

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Fields are semantically equivalent if they have the same declaring
   * class, name, and type. Since you can't creat a Field except through
   * the VM, this is just the == relation.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not
   */
  public boolean equals (Object fld)
  {
    if (! (fld instanceof Field))
      return false;
    Field f = (Field) fld;
    return declaringClass == f.declaringClass && offset == f.offset;
  }

  /**
   * Get the hash code for the Field. The Field hash code is the hash code
   * of its name XOR'd with the hash code of its class name.
   *
   * @return the hash code for the object.
   */
  public int hashCode()
  {
    return getDeclaringClass().getName().hashCode() ^ getName().hashCode();
  }

  /**
   * Get a String representation of the Field. A Field's String
   * representation is "&lt;modifiers&gt; &lt;type&gt;
   * &lt;class&gt;.&lt;fieldname&gt;".<br> Example:
   * <code>public transient boolean gnu.parse.Parser.parseComplete</code>
   *
   * @return the String representation of the Field
   */
  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer ();
    int mods = getModifiers();
    if (mods != 0)
      {
	Modifier.toString(mods, sbuf);
	sbuf.append(' ');
      }
    Method.appendClassName (sbuf, getType ());
    sbuf.append(' ');
    Method.appendClassName (sbuf, getDeclaringClass());
    sbuf.append('.');
    sbuf.append(getName());
    return sbuf.toString();
  }

  public String toGenericString()
  {
    StringBuilder sb = new StringBuilder(64);
    Modifier.toString(getModifiers(), sb).append(' ');
    sb.append(getGenericType()).append(' ');
    sb.append(getDeclaringClass().getName()).append('.');
    sb.append(getName());
    return sb.toString();
  }

  /**
   * Get the value of this Field.  If it is primitive, it will be wrapped
   * in the appropriate wrapper type (boolean = java.lang.Boolean).<p>
   *
   * If the field is static, <code>o</code> will be ignored. Otherwise, if
   * <code>o</code> is null, you get a <code>NullPointerException</code>,
   * and if it is incompatible with the declaring class of the field, you
   * get an <code>IllegalArgumentException</code>.<p>
   *
   * Next, if this Field enforces access control, your runtime context is
   * evaluated, and you may have an <code>IllegalAccessException</code> if
   * you could not access this field in similar compiled code. If the field
   * is static, and its class is uninitialized, you trigger class
   * initialization, which may end in a
   * <code>ExceptionInInitializerError</code>.<p>
   *
   * Finally, the field is accessed, and primitives are wrapped (but not
   * necessarily in new objects). This method accesses the field of the
   * declaring class, even if the instance passed in belongs to a subclass
   * which declares another field to hide this one.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if <code>o</code> is not an instance of
   *         the class or interface declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #getBoolean(Object)
   * @see #getByte(Object)
   * @see #getChar(Object)
   * @see #getShort(Object)
   * @see #getInt(Object)
   * @see #getLong(Object)
   * @see #getFloat(Object)
   * @see #getDouble(Object)
   */
  public Object get(Object obj)
    throws IllegalAccessException
  {
    return get(null, obj);
  }

  /**
   * Get the value of this boolean Field. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a boolean field of
   *         <code>o</code>, or if <code>o</code> is not an instance of the
   *         declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public boolean getBoolean(Object obj)
    throws IllegalAccessException
  {
    return getBoolean(null, obj);
  }

  /**
   * Get the value of this byte Field. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte field of
   *         <code>o</code>, or if <code>o</code> is not an instance of the
   *         declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public byte getByte(Object obj)
    throws IllegalAccessException
  {
    return getByte(null, obj);
  }

  /**
   * Get the value of this Field as a char. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a char field of
   *         <code>o</code>, or if <code>o</code> is not an instance
   *         of the declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public char getChar(Object obj)
    throws IllegalAccessException
  {
    return getChar(null, obj);
  }

  /**
   * Get the value of this Field as a short. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte or short
   *         field of <code>o</code>, or if <code>o</code> is not an instance
   *         of the declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public short getShort(Object obj)
    throws IllegalAccessException
  {
    return getShort(null, obj);
  }

  /**
   * Get the value of this Field as an int. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte, short, char, or
   *         int field of <code>o</code>, or if <code>o</code> is not an
   *         instance of the declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public int getInt(Object obj)
    throws IllegalAccessException
  {
    return getInt(null, obj);
  }

  /**
   * Get the value of this Field as a long. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte, short, char, int,
   *         or long field of <code>o</code>, or if <code>o</code> is not an
   *         instance of the declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public long getLong(Object obj)
    throws IllegalAccessException
  {
    return getLong(null, obj);
  }

  /**
   * Get the value of this Field as a float. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte, short, char, int,
   *         long, or float field of <code>o</code>, or if <code>o</code> is
   *         not an instance of the declaring class of this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public float getFloat(Object obj)
    throws IllegalAccessException
  {
    return getFloat(null, obj);
  }

  /**
   * Get the value of this Field as a double. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
   * @return the value of the Field
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte, short, char, int,
   *         long, float, or double field of <code>o</code>, or if
   *         <code>o</code> is not an instance of the declaring class of this
   *         field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #get(Object)
   */
  public double getDouble(Object obj)
    throws IllegalAccessException
  {
    return getDouble(null, obj);
  }

  private native boolean getBoolean (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native char getChar (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native byte getByte (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native short getShort (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native int getInt (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native long getLong (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native float getFloat (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native double getDouble (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native Object get (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  /**
   * Set the value of this Field.  If it is a primitive field, the value
   * will be unwrapped from the passed object (boolean = java.lang.Boolean).<p>
   *
   * If the field is static, <code>o</code> will be ignored. Otherwise, if
   * <code>o</code> is null, you get a <code>NullPointerException</code>,
   * and if it is incompatible with the declaring class of the field, you
   * get an <code>IllegalArgumentException</code>.<p>
   *
   * Next, if this Field enforces access control, your runtime context is
   * evaluated, and you may have an <code>IllegalAccessException</code> if
   * you could not access this field in similar compiled code. This also
   * occurs whether or not there is access control if the field is final.
   * If the field is primitive, and unwrapping your argument fails, you will
   * get an <code>IllegalArgumentException</code>; likewise, this error
   * happens if <code>value</code> cannot be cast to the correct object type.
   * If the field is static, and its class is uninitialized, you trigger class
   * initialization, which may end in a
   * <code>ExceptionInInitializerError</code>.<p>
   *
   * Finally, the field is set with the widened value. This method accesses
   * the field of the declaring class, even if the instance passed in belongs
   * to a subclass which declares another field to hide this one.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if <code>value</code> cannot be
   *         converted by a widening conversion to the underlying type of
   *         the Field, or if <code>o</code> is not an instance of the class
   *         declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #setBoolean(Object, boolean)
   * @see #setByte(Object, byte)
   * @see #setChar(Object, char)
   * @see #setShort(Object, short)
   * @see #setInt(Object, int)
   * @see #setLong(Object, long)
   * @see #setFloat(Object, float)
   * @see #setDouble(Object, double)
   */
  public void set(Object object, Object value)
    throws IllegalAccessException
  {
    set(null, object, value);
  }

  /**
   * Set this boolean Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a boolean field, or if
   *         <code>o</code> is not an instance of the class declaring this
   *         field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setBoolean(Object obj, boolean b)
    throws IllegalAccessException
  {
    setBoolean(null, obj, b, true);
  }

  /**
   * Set this byte Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a byte, short, int, long,
   *         float, or double field, or if <code>o</code> is not an instance
   *         of the class declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setByte(Object obj, byte b)
    throws IllegalAccessException
  {
    setByte(null, obj, b, true);
  }

  /**
   * Set this char Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a char, int, long,
   *         float, or double field, or if <code>o</code> is not an instance
   *         of the class declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setChar(Object obj, char c)
    throws IllegalAccessException
  {
    setChar(null, obj, c, true);
  }

  /**
   * Set this short Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a short, int, long,
   *         float, or double field, or if <code>o</code> is not an instance
   *         of the class declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setShort(Object obj,  short s)
    throws IllegalAccessException
  {
    setShort(null, obj, s, true);
  }

  /**
   * Set this int Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not an int, long, float, or
   *         double field, or if <code>o</code> is not an instance of the
   *         class declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setInt(Object obj, int i)
    throws IllegalAccessException
  {
    setInt(null, obj, i, true);
  }

  /**
   * Set this long Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a long, float, or double
   *         field, or if <code>o</code> is not an instance of the class
   *         declaring this field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setLong(Object obj, long l)
    throws IllegalArgumentException, IllegalAccessException
  {
    setLong(null, obj, l, true);
  }

  /**
   * Set this float Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a float or long field, or
   *         if <code>o</code> is not an instance of the class declaring this
   *         field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setFloat(Object obj, float f)
    throws IllegalAccessException
  {
    setFloat(null, obj, f, true);
  }

  /**
   * Set this double Field. If the field is static, <code>o</code> will be
   * ignored.
   *
   * @param o the object to set this Field on
   * @param value the value to set this Field to
   * @throws IllegalAccessException if you could not normally access this field
   *         (i.e. it is not public)
   * @throws IllegalArgumentException if this is not a double field, or if
   *         <code>o</code> is not an instance of the class declaring this
   *         field
   * @throws NullPointerException if <code>o</code> is null and this field
   *         requires an instance
   * @throws ExceptionInInitializerError if accessing a static field triggered
   *         class initialization, which then failed
   * @see #set(Object, Object)
   */
  public void setDouble(Object obj, double d)
    throws IllegalAccessException
  {
    setDouble(null, obj, d, true);
  }

  /**
   * Return the generic type of the field. If the field type is not a generic
   * type, the method returns the same as <code>getType()</code>.
   *
   * @throws GenericSignatureFormatError if the generic signature does
   *         not conform to the format specified in the Virtual Machine
   *         specification, version 3.
   * @since 1.5
   */
  public Type getGenericType()
  {
    String signature = getSignature();
    if (signature == null)
      return getType();
    FieldSignatureParser p = new FieldSignatureParser(getDeclaringClass(),
                                                      signature);
    return p.getFieldType();
  }

  public <T extends Annotation> T getAnnotation(Class<T> annoClass)
  {
    Annotation[] annos = getDeclaredAnnotations();
    for (int i = 0; i < annos.length; ++i)
      if (annos[i].annotationType() == annoClass)
	return (T) annos[i];
    return null;
  }

  public Annotation[] getDeclaredAnnotations()
  {
    Annotation[] result = getDeclaredAnnotationsInternal();
    if (result == null)
      result = new Annotation[0];
    return result;
  }

  private native Annotation[] getDeclaredAnnotationsInternal();

  /**
   * Return the String in the Signature attribute for this field. If there
   * is no Signature attribute, return null.
   */
  private native String getSignature();

  native void setByte (Class caller, Object obj, byte b, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setShort (Class caller, Object obj, short s, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setInt (Class caller, Object obj, int i, boolean checkFinal)  
    throws IllegalArgumentException, IllegalAccessException;

  native void setLong (Class caller, Object obj, long l, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setFloat (Class caller, Object obj, float f, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setDouble (Class caller, Object obj, double d,
			 boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setChar (Class caller, Object obj, char c, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setBoolean (Class caller, Object obj, boolean b,
			  boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void set (Class caller, Object obj, Object val, Class type, 
		   boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  private void set (Class caller, Object object, Object value)
    throws IllegalArgumentException, IllegalAccessException
  {
    Class type = getType();
    if (! type.isPrimitive())
      set(caller, object, value, type, true);
    else if (value instanceof Byte)
      setByte(caller, object, ((Byte) value).byteValue(), true);
    else if (value instanceof Short)
      setShort (caller, object, ((Short) value).shortValue(), true);
    else if (value instanceof Integer)
      setInt(caller, object, ((Integer) value).intValue(), true);
    else if (value instanceof Long)
      setLong(caller, object, ((Long) value).longValue(), true);
    else if (value instanceof Float)
      setFloat(caller, object, ((Float) value).floatValue(), true);
    else if (value instanceof Double)
      setDouble(caller, object, ((Double) value).doubleValue(), true);
    else if (value instanceof Character)
      setChar(caller, object, ((Character) value).charValue(), true);
    else if (value instanceof Boolean)
      setBoolean(caller, object, ((Boolean) value).booleanValue(), true);
    else
      throw new IllegalArgumentException();
  }
}
