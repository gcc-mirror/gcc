/* SimpleType.java -- Open type descriptor for the base types.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

package javax.management.openmbean;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;

import java.math.BigDecimal;
import java.math.BigInteger;

import java.util.Date;

import javax.management.ObjectName;

/**
 * The open type descriptor for data values that are members
 * of one of the simple types (such as an integer or a string).
 * The other open types ({@link ArrayType}, {@link CompositeType},
 * {@link TabularType}) are constructed from one or more of these
 * types.  The simple types are formed from a small subset of
 * basic Java types.  As a result, the valid instances of this
 * class are predefined, and no constructor is given for creating
 * new instances.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class SimpleType<T>
  extends OpenType<T>
{

  /**
   * The {@link SimpleType} representation of
   * <code>java.math.BigDecimal</code>.
   */
  public static final SimpleType<BigDecimal> BIGDECIMAL;

  /**
   * The {@link SimpleType} representation of
   * <code>java.math.BigInteger</code>.
   */
  public static final SimpleType<BigInteger> BIGINTEGER;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Boolean</code>.
   */
  public static final SimpleType<Boolean> BOOLEAN;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Byte</code>.
   */
  public static final SimpleType<Byte> BYTE;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Character</code>.
   */
  public static final SimpleType<Character> CHARACTER;

  /**
   * The {@link SimpleType} representation of
   * <code>java.util.Date</code>.
   */
  public static final SimpleType<Date> DATE;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Double</code>.
   */
  public static final SimpleType<Double> DOUBLE;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Float</code>.
   */
  public static final SimpleType<Float> FLOAT;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Integer</code>.
   */
  public static final SimpleType<Integer> INTEGER;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Long</code>.
   */
  public static final SimpleType<Long> LONG;

  /**
   * The {@link SimpleType} representation of
   * <code>javax.management.ObjectName</code>.
   */
  public static final SimpleType<ObjectName> OBJECTNAME;


  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Short</code>.
   */
  public static final SimpleType<Short> SHORT;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.String</code>.
   */
  public static final SimpleType<String> STRING;

  /**
   * The {@link SimpleType} representation of
   * <code>java.lang.Void</code>.
   */
  public static final SimpleType<Void> VOID;

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 2215577471957694503L;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Static construction of the {@link SimpleType} instances.
   */
  static
  {
    try
      {
        BIGDECIMAL = new SimpleType<BigDecimal>("java.math.BigDecimal");
        BIGINTEGER = new SimpleType<BigInteger>("java.math.BigInteger");
        BOOLEAN = new SimpleType<Boolean>("java.lang.Boolean");
        BYTE = new SimpleType<Byte>("java.lang.Byte");
        CHARACTER = new SimpleType<Character>("java.lang.Character");
        DATE = new SimpleType<Date>("java.util.Date");
        DOUBLE = new SimpleType<Double>("java.lang.Double");
        FLOAT = new SimpleType<Float>("java.lang.Float");
        INTEGER = new SimpleType<Integer>("java.lang.Integer");
        LONG = new SimpleType<Long>("java.lang.Long");
        OBJECTNAME =
          new SimpleType<ObjectName>("javax.management.ObjectName");
        SHORT = new SimpleType<Short>("java.lang.Short");
        STRING = new SimpleType<String>("java.lang.String");
        VOID = new SimpleType<Void>("java.lang.Void");
      }
    catch (OpenDataException e)
      {
        /* In normal circumstances, this shouldn't be possible. */
        throw new IllegalStateException("A invalid class name " +
                                        "was passed to the SimpleType " +
                                        "constructor.", e);
      }
  }

  /**
   * Constructs a new {@link SimpleType} instance for the given
   * class name.  The class name is also used as the type name
   * and description of the {@link OpenType} instance.
   *
   * @param name the name of the class this instance should
   *             represent.
   * @throws OpenDataException if somehow the constructor of the
   *                           superclass is passed an invalid
   *                           class name.
   */
  private SimpleType(String name)
    throws OpenDataException
  {
    super(name, name, name);
  }

  /**
   * <p>
   * Compares this simple data type with another object
   * for equality.  The objects are judged to be equal if:
   * </p>
   * <ul>
   * <li><code>obj</code> is not null.</li>
   * <li><code>obj</code> is an instance of
   * {@link SimpleType}.</li>
   * <li>The class names are equal.</li>
   * </ul>
   *
   * @param obj the object to compare with.
   * @return true if the conditions above hold.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof SimpleType))
      return false;
    SimpleType<?> sType = (SimpleType<?>) obj;
    return sType.getClassName().equals(getClassName());
  }

  /**
   * <p>
   * Returns the hash code of the simple data type.
   * This is simply the hash code of the class name,
   * which is the same element of the type compared
   * as part of the
   * {@link #equals(java.lang.Object)} method, thus ensuring
   * that the hashcode is compatible with the equality
   * test.
   * </p>
   * <p>
   * As instances of this class are immutable, the hash code
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hash code of this instance.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(getClassName().hashCode());
    return hashCode.intValue();
  }

  /**
   * Returns true if the specified object is a member of this
   * simple type.  The object is judged to be so if it is
   * non-null and its class name is the same as that returned
   * by {@link SimpleType#getClassName()}.
   *
   * @param obj the object to test for membership.
   * @return true if the object is a member of this type.
   */
  public boolean isValue(Object obj)
  {
    if (obj == null)
      return false;
    return obj.getClass().getName().equals(getClassName());
  }

  /**
   * Replaces instances of this class read from an
   * {@link java.io.ObjectInputStream} with one of the predefined
   * values.  This ensures that each existing instance of
   * this class is one of these unique instances.
   *
   * @return the replacement object.
   * @throws ObjectStreamException if the object can not be
   *                               resolved.
   */
  public Object readResolve()
    throws ObjectStreamException
  {
    if (equals(BIGDECIMAL))
      return BIGDECIMAL;
    if (equals(BIGINTEGER))
      return BIGINTEGER;
    if (equals(BOOLEAN))
      return BOOLEAN;
    if (equals(BYTE))
      return BYTE;
    if (equals(CHARACTER))
      return CHARACTER;
    if (equals(DATE))
      return DATE;
    if (equals(DOUBLE))
      return DOUBLE;
    if (equals(FLOAT))
      return FLOAT;
    if (equals(INTEGER))
      return INTEGER;
    if (equals(LONG))
      return LONG;
    if (equals(OBJECTNAME))
      return OBJECTNAME;
    if (equals(SHORT))
      return SHORT;
    if (equals(STRING))
      return STRING;
    if (equals(VOID))
      return VOID;
    throw new InvalidObjectException("Invalid simple type instance " +
                                     "deserialized.");
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.SimpleType</code>)
   * and the name of the class the type represents.
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  public String toString()
  {
    if (string == null)
      string = getClass().getName()
        + "[name=" + getClassName()
        + "]";
    return string;
  }

}
