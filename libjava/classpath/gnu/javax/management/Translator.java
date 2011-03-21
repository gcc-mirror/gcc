/* Translator.java -- Performs MXBean data type translation.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.javax.management;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;

import javax.management.JMX;
import javax.management.MBeanServerInvocationHandler;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenMBeanParameterInfo;
import javax.management.openmbean.OpenMBeanParameterInfoSupport;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 * Translates Java data types to their equivalent
 * open data type, and vice versa, according to the
 * {@link javax.management.MXBean} rules.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public final class Translator
{

  /**
   * Translates the input Java data types to the equivalent
   * open data types.
   *
   * @param jtypes the Java types supplied as parameters.
   * @param method the method that was called.
   * @return the equivalent open types required by the {@link MXBean}.
   * @throws Throwable if an exception is thrown in performing the
   *                   conversion.
   */
  public static final Object[] fromJava(Object[] jtypes, Method method)
    throws Throwable
  {
    Type[] gtypes = method.getGenericParameterTypes();
    Object[] otypes = new Object[jtypes.length];
    for (int a = 0; a < jtypes.length; ++a)
      otypes[a] = fromJava(jtypes[a], gtypes[a]);
    return otypes;
  }

  /**
   * Translates the input Java data type to the equivalent
   * open data type.
   *
   * @param jtype the Java type supplied as a parameter.
   * @param type the type of the parameter.
   * @return the equivalent open type required by the {@link MXBean}.
   * @throws Throwable if an exception is thrown in performing the
   *                   conversion.
   */
  public static final Object fromJava(Object jtype, Type type)
    throws Throwable
  {
    if (jtype == null)
      return null;
    Class<?> jclass = jtype.getClass();
    if (OpenType.ALLOWED_CLASSNAMES_LIST.contains(jclass.getName()))
      return jtype;
    if (jclass.isArray())
      {
        Class<?> ctype = jclass.getComponentType();
        if (ctype.isPrimitive())
          return jtype;
        if (OpenType.ALLOWED_CLASSNAMES_LIST.contains(ctype.getName()))
          return jtype;
        Object[] elems = (Object[]) jtype;
        Object[] celems = new Object[elems.length];
        for (int a = 0; a < elems.length; ++a)
          celems[a] = fromJava(elems[a], elems[a].getClass());
        return makeArraySpecific(celems);
      }
    String tName = getTypeName(type);
    if (jtype instanceof List || jtype instanceof Set ||
        jtype instanceof SortedSet)
      {
        if (jtype instanceof SortedSet)
          {
            ParameterizedType ptype = (ParameterizedType) type;
            Class<?> elemClass = (Class<?>) ptype.getActualTypeArguments()[0];
            if (!Comparable.class.isAssignableFrom(elemClass))
              throw new IllegalArgumentException(jtype + " has a " +
                                                 "non-comparable element " +
                                                 "type, " + elemClass);
            if (((SortedSet<?>) jtype).comparator() != null)
              throw new IllegalArgumentException(jtype + " does not " +
                                                 "use natural ordering.");
          }
        Collection<?> elems = (Collection<?>) jtype;
        int numElems = elems.size();
        Object[] celems = new Object[numElems];
        Iterator<?> i = elems.iterator();
        for (int a = 0; a < numElems; ++a)
          {
            Object elem = i.next();
            celems[a] = fromJava(elem, elem.getClass());
          }
        return makeArraySpecific(celems);
      }
    if (jtype instanceof Enum)
      return ((Enum<?>) jtype).name();
    if (jtype instanceof Map || jtype instanceof SortedMap)
      {
        int lparam = tName.indexOf("<");
        int comma = tName.indexOf(",", lparam);
        int rparam = tName.indexOf(">", comma);
        String key = tName.substring(lparam + 1, comma).trim();
        String value = tName.substring(comma + 1, rparam).trim();
        String typeName = null;
        if (jtype instanceof Map)
          typeName = "java.util.Map" + tName.substring(lparam);
        else
          {
            Class<?> keyClass = Class.forName(key);
            if (!Comparable.class.isAssignableFrom(keyClass))
              throw new IllegalArgumentException(jtype + " has a " +
                                                 "non-comparable element " +
                                                 "type, " + keyClass);
            if (((SortedMap<?,?>) jtype).comparator() != null)
              throw new IllegalArgumentException(jtype + " does not " +
                                                 "use natural ordering.");
            typeName = "java.util.SortedMap" + tName.substring(lparam);
          }
        OpenType<?> k = translate(key).getOpenType();
        OpenType<?> v = translate(value).getOpenType();
        CompositeType rowType = new CompositeType(typeName, typeName,
                                                  new String[] { "key", "value" },
                                                  new String[] { "Map key", "Map value"},
                                                  new OpenType[] {k,v});
        TabularType tabType = new TabularType(typeName, typeName, rowType,
                                              new String[]{"key"});
        TabularData data = new TabularDataSupport(tabType);
        for (Map.Entry<?,?> entry : ((Map<?,?>) jtype).entrySet())
          {
            try
              {
                data.put(new CompositeDataSupport(rowType,
                                                  new String[] {
                                                    "key",
                                                    "value"
                                                  },
                                                  new Object[] {
                                                    entry.getKey(),
                                                    entry.getValue()
                                                  }));
              }
            catch (OpenDataException e)
              {
                throw (InternalError) (new InternalError("A problem occurred " +
                                                         "converting the map " +
                                                         "to a composite data " +
                                                         "structure.").initCause(e));
              }
          }
        return data;
      }
    if (JMX.isMXBeanInterface(jclass))
      {
        try
          {
            MBeanServerInvocationHandler ih = (MBeanServerInvocationHandler)
              Proxy.getInvocationHandler(jtype);
            return ih.getObjectName();
          }
        catch (IllegalArgumentException e)
          {
            throw new IllegalArgumentException("For a MXBean to be translated " +
                                               "to an open type, it must be a " +
                                               "proxy.", e);
          }
        catch (ClassCastException e)
          {
            throw new IllegalArgumentException("For a MXBean to be translated " +
                                               "to an open type, it must have a " +
                                               "MBeanServerInvocationHandler.", e);
          }
      }
    /* FIXME: Handle other types */
    throw new IllegalArgumentException("The type, " + jtype +
                                       ", is not convertible.");
  }

  /**
   * Translates the returned open data type to the value
   * required by the interface.
   *
   * @param otype the open type returned by the method call.
   * @param method the method that was called.
   * @return the equivalent return type required by the interface.
   * @throws Throwable if an exception is thrown in performing the
   *                   conversion.
   */
  public static final Object toJava(Object otype, Method method)
    throws Throwable
  {
    Class<?> returnType = method.getReturnType();
    if (returnType.isEnum())
      {
        String ename = (String) otype;
        Enum<?>[] constants = (Enum[]) returnType.getEnumConstants();
        for (Enum<?> c : constants)
          if (c.name().equals(ename))
            return c;
      }
    if (List.class.isAssignableFrom(returnType))
      {
        Object[] elems = (Object[]) otype;
        List<Object> l = new ArrayList<Object>(elems.length);
        for (Object elem : elems)
          l.add(elem);
        return l;
      }
    if (Map.class.isAssignableFrom(returnType))
      {
        TabularData data = (TabularData) otype;
        Map<Object,Object> m = new HashMap<Object,Object>(data.size());
        for (Object val : data.values())
          {
            CompositeData vals = (CompositeData) val;
            m.put(vals.get("key"), vals.get("value"));
          }
        return m;
      }
    try
      {
        Method m = returnType.getMethod("from",
                                        new Class[]
          { CompositeData.class });
        return m.invoke(null, (CompositeData) otype);
      }
    catch (NoSuchMethodException e)
      {
        /* Ignored; we expect this if this
           isn't a from(CompositeData) class */
      }
    return otype;
  }

  /**
   * Creates a new array which has the specific type
   * used by the elements of the original {@link Object}
   * array supplied.
   *
   * @param arr a series of elements in an {@link Object}
   *            array.
   * @return the same elements in a new array of the specific
   *         type.
   */
  private static final Object[] makeArraySpecific(Object[] arr)
  {
    Object[] rcelems = (Object[]) Array.newInstance(arr[0].getClass(),
                                                    arr.length);
    System.arraycopy(arr, 0, rcelems, 0, arr.length);
    return rcelems;
  }

  /**
   * Translates the name of a type into an equivalent
   * {@link javax.management.openmbean.OpenMBeanParameterInfo}
   * that describes it.
   *
   * @param type the type to describe.
   * @return an instance of
   * {@link javax.management.openmbean.OpenMBeanParameterInfo},
   * describing the translated type and limits of the given type.
   * @throws OpenDataException if a type is not open.
   */
  public static final OpenMBeanParameterInfo translate(String type)
    throws OpenDataException
  {
    if (type.equals("boolean") || type.equals(Boolean.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.BOOLEAN,
                                               null,
                                               new Boolean[] {
                                                 Boolean.TRUE,
                                                 Boolean.FALSE
                                               });
    if (type.equals("byte") || type.equals(Byte.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.BYTE,
                                               null,
                                               Byte.valueOf(Byte.MIN_VALUE),
                                               Byte.valueOf(Byte.MAX_VALUE));
    if (type.equals("char") || type.equals(Character.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.CHARACTER,
                                               null,
                                               Character.valueOf(Character.MIN_VALUE),
                                               Character.valueOf(Character.MAX_VALUE));
    if (type.equals("double") || type.equals(Double.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.DOUBLE,
                                               null,
                                               Double.valueOf(Double.MIN_VALUE),
                                               Double.valueOf(Double.MAX_VALUE));
    if (type.equals("float") || type.equals(Float.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.FLOAT,
                                               null,
                                               Float.valueOf(Float.MIN_VALUE),
                                               Float.valueOf(Float.MAX_VALUE));
    if (type.equals("int") || type.equals(Integer.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.INTEGER,
                                               null,
                                               Integer.valueOf(Integer.MIN_VALUE),
                                               Integer.valueOf(Integer.MAX_VALUE));
    if (type.equals("long") || type.equals(Long.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.LONG,
                                               null,
                                               Long.valueOf(Long.MIN_VALUE),
                                               Long.valueOf(Long.MAX_VALUE));
    if (type.equals("short") || type.equals(Short.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.SHORT,
                                               null,
                                               Short.valueOf(Short.MIN_VALUE),
                                               Short.valueOf(Short.MAX_VALUE));
    if (type.equals(String.class.getName()))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.STRING);
    if (type.equals("void"))
      return new OpenMBeanParameterInfoSupport("TransParam",
                                               "Translated parameter",
                                               SimpleType.VOID);
    if (type.startsWith("java.util.Map"))
      {
        int lparam = type.indexOf("<");
        int comma = type.indexOf(",", lparam);
        int rparam = type.indexOf(">", comma);
        String key = type.substring(lparam + 1, comma).trim();
        OpenType<?> k = translate(key).getOpenType();
        OpenType<?> v = translate(type.substring(comma + 1, rparam).trim()).getOpenType();
        CompositeType ctype = new CompositeType(Map.class.getName(), Map.class.getName(),
                                                new String[] { "key", "value" },
                                                new String[] { "Map key", "Map value"},
                                                new OpenType[] { k, v});
        TabularType ttype = new TabularType(key, key, ctype,
                                            new String[] { "key" });
        return new OpenMBeanParameterInfoSupport("TransParam",
                                                 "Translated parameter",
                                                 ttype);
      }
    if (type.startsWith("java.util.List"))
      {
        int lparam = type.indexOf("<");
        int rparam = type.indexOf(">");
        OpenType<?> e = translate(type.substring(lparam + 1, rparam).trim()).getOpenType();
        return new OpenMBeanParameterInfoSupport("TransParam",
                                                 "Translated parameter",
                                                 new ArrayType<OpenType<?>>(1, e)
                                                 );
      }
    Class<?> c;
    try
      {
        c = Class.forName(type);
      }
    catch (ClassNotFoundException e)
      {
        throw (InternalError)
          (new InternalError("The class for a type used in a management bean " +
                             "could not be loaded.").initCause(e));
      }
    if (c.isEnum())
      {
        Object[] values = c.getEnumConstants();
        String[] names = new String[values.length];
        for (int a = 0; a < values.length; ++a)
          names[a] = values[a].toString();
        return new OpenMBeanParameterInfoSupport("TransParam",
                                                 "Translated parameter",
                                                 SimpleType.STRING,
                                                 null, names);
      }
    if (c.isArray())
      {
        int depth;
        for (depth = 0; c.getName().charAt(depth) == '['; ++depth)
          ;
        OpenType<?> ot = getTypeFromClass(c.getComponentType());
        return new OpenMBeanParameterInfoSupport("TransParam",
                                                 "Translated parameter",
                                                 new ArrayType<OpenType<?>>(depth, ot)
                                                 );
      }
    Method[] methods = c.getDeclaredMethods();
    List<String> names = new ArrayList<String>();
    List<OpenType<?>> types = new ArrayList<OpenType<?>>();
    for (int a = 0; a < methods.length; ++a)
      {
        String name = methods[a].getName();
        if (Modifier.isPublic(methods[a].getModifiers()))
          {
            if (name.startsWith("get"))
              {
                names.add(name.substring(3));
                types.add(getTypeFromClass(methods[a].getReturnType()));
              }
            else if (name.startsWith("is"))
              {
                names.add(name.substring(2));
                types.add(getTypeFromClass(methods[a].getReturnType()));
              }
          }
      }
    if (names.isEmpty())
      throw new OpenDataException("The type used does not have an open type translation.");
    String[] fields = names.toArray(new String[names.size()]);
    CompositeType ctype = new CompositeType(c.getName(), c.getName(),
                                            fields, fields,
                                            types.toArray(new OpenType[types.size()]));
    return new OpenMBeanParameterInfoSupport("TransParam",
                                             "Translated parameter",
                                             ctype);
  }

  /**
   * Obtains the {@link javax.management.openmbean.OpenType}
   * for a particular class.
   *
   * @param c the class to obtain the type for.
   * @return the appropriate instance.
   * @throws OpenDataException if the type is not open.
   */
  private static final OpenType<?> getTypeFromClass(Class<?> c)
    throws OpenDataException
  {
    return Translator.translate(c.getName()).getOpenType();
  }

  /**
   * <p>
   * Returns the type name according to the rules described
   * in {@link javax.management.MXBean}.  Namely, for a type,
   * {@code T}, {@code typename(T)} is computed as follows:
   * </p>
   * <ul>
   * <li>If T is non-generic and not an array, then the value
   * of {@link java.lang.Class#getName()} is returned.</li>
   * <li>If T is an array type, {@code{E[]}, then the type name
   * is {@code typename(E)} followed by an occurrence
   * of {@code '[]'} for each dimension.</li>
   * <li>If T is a generic or parameterized type, the type name
   * is composed of {@code typename(P)}, where {@code P} is the
   * parameterized type name, followed by {@code '<'}, the resulting
   * list of type names of the parameters after applying {@code typename}
   * to each, separated by commas, and {@code '>'}.</li>
   * </ul>
   *
   * @param type the type to return the type name of.
   * @return the type name computed according to the rules above.
   */
  private static final String getTypeName(Type type)
  {
    if (type instanceof Class)
      {
        Class<?> c = (Class<?>) type;
        if (c.isArray())
          {
            StringBuilder b =
              new StringBuilder(c.getComponentType().getName());
            String normName = c.getName();
            for (int a = 0; a < normName.length(); ++a)
              {
                if (normName.charAt(a) == '[')
                  b.append("[]");
                else
                  break;
              }
            return b.toString();
          }
        return c.getName();
      }
    return type.toString();
  }

}
