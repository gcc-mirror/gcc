/* JniHelper.java - name mangling and other JNI support
 Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.tools.javah;

import java.io.IOException;

import org.objectweb.asm.Type;

public class JniHelper
{
  public static String getName(Main classpath, Type type) throws IOException
  {
    if (type == Type.BOOLEAN_TYPE)
      return "jboolean";
    else if (type == Type.BYTE_TYPE)
      return "jbyte";
    else if (type == Type.CHAR_TYPE)
      return "jchar";
    else if (type == Type.SHORT_TYPE)
      return "jshort";
    else if (type == Type.INT_TYPE)
      return "jint";
    else if (type == Type.LONG_TYPE)
      return "jlong";
    else if (type == Type.FLOAT_TYPE)
      return "jfloat";
    else if (type == Type.DOUBLE_TYPE)
      return "jdouble";
    else if (type == Type.VOID_TYPE)
      return "void";

    if (type.getSort() == Type.ARRAY)
      {
        Type elt = type.getElementType();
        int eltSort = elt.getSort();
        if (type.getDimensions() == 1 && eltSort != Type.OBJECT)
          return getName(classpath, elt) + "Array";
        return "jobjectArray";
      }

    // assert type.getSort() == Type.OBJECT;
    String className = type.getClassName();
    // FIXME: is this correct?
    if (className.equals("java/lang/Class")
        || className.equals("java.lang.Class"))
      return "jclass";
    if (className.equals("java/lang/String")
        || className.equals("java.lang.String"))
      return "jstring";

    ClassWrapper klass = classpath.getClass(className);
    if (klass.isThrowable())
      return "jthrowable";
    return "jobject";
  }

  public static String mangle(String name)
  {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < name.length(); ++i)
      {
        char c = name.charAt(i);
        if (c == '_')
          result.append("_1");
        else if (c == ';')
          result.append("_2");
        else if (c == '[')
          result.append("_3");
        else if (c == '/')
          result.append("_");
        else if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
                 || (c >= 'A' && c <= 'Z'))
          result.append(c);
        else
          {
            result.append("_0");
            // Sigh.
            String hex = "0000" + Integer.toHexString(c);
            result.append(hex.substring(hex.length() - 4));
          }
      }
    return result.toString();
  }
}
