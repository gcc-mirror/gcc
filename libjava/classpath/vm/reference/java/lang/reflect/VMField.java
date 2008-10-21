/* java.lang.reflect.Field - VM interface for reflection of Java fields
   Copyright (C) 1998, 2001, 2005, 2008 Free Software Foundation, Inc.

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

import java.lang.annotation.Annotation;

final class VMField
{
  Class clazz;
  String name;
  int slot;
  
  /** 
   * This field allows us to refer back to the main constructor instance.
   *  It is set by the constructor of Field.
   */
  Field f;

  VMField(Class clazz, String name, int slot)
  {
    this.clazz = clazz;
    this.name = name;
    this.slot = slot;
  }

  public Class getDeclaringClass()
  {
    return clazz;
  }

  public String getName()
  {
    return name;
  }

  /**
   * Return the raw modifiers for this field.
   * @return the field's modifiers
   */
  native int getModifiersInternal();

  /**
   * Gets the type of this field.
   * @return the type of this field
   */
  native Class getType();

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
  native Object get(Object o)
    throws IllegalAccessException;

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
  native boolean getBoolean(Object o)
    throws IllegalAccessException;

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
  native byte getByte(Object o)
    throws IllegalAccessException;

  /**
   * Get the value of this Field as a char. If the field is static,
   * <code>o</code> will be ignored.
   *
   * @param o the object to get the value of this Field from
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
  native char getChar(Object o)
    throws IllegalAccessException;

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
  native short getShort(Object o)
    throws IllegalAccessException;

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
  native int getInt(Object o)
    throws IllegalAccessException;

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
  native long getLong(Object o)
    throws IllegalAccessException;

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
  native float getFloat(Object o)
    throws IllegalAccessException;

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
  native double getDouble(Object o)
    throws IllegalAccessException;

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
  native void set(Object o, Object value)
    throws IllegalAccessException;

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
  native void setBoolean(Object o, boolean value)
    throws IllegalAccessException;

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
  native void setByte(Object o, byte value)
    throws IllegalAccessException;

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
  native void setChar(Object o, char value)
    throws IllegalAccessException;

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
  native void setShort(Object o, short value)
    throws IllegalAccessException;

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
  native void setInt(Object o, int value)
    throws IllegalAccessException;

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
  native void setLong(Object o, long value)
    throws IllegalAccessException;

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
  native void setFloat(Object o, float value)
    throws IllegalAccessException;

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
  native void setDouble(Object o, double value)
    throws IllegalAccessException;

  /**
   * Return the String in the Signature attribute for this field. If there
   * is no Signature attribute, return null.
   *
   */
  native String getSignature();

  /**
   * Compare two objects to see if they are semantically equivalent.
   * Two Fields are semantically equivalent if they have the same declaring
   * class, name, and type. Since you can't create a Field except through
   * the VM, this is just the == relation.
   *
   * @param o the object to compare to
   * @return <code>true</code> if they are equal; <code>false</code> if not
   */
  public boolean equals(Object o)
  {
    if (!(o instanceof Field))
      return false;
    Field that = (Field)o; 
    if (clazz != that.getDeclaringClass())
      return false;
    if (!name.equals(that.getName()))
      return false;
    if (getType() != that.getType())
      return false;
    return true;
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
  native Annotation getAnnotation(Class annotationClass);

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
  native Annotation[] getDeclaredAnnotations();

}
