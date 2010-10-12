/* JniPrintStream.java - PrintStream that emits JNI declarations
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
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Iterator;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.MethodNode;

public class JniPrintStream
    extends PrintStream
{
  Main classpath;

  // This is used to determine whether a method has an overload.
  HashMap<String,Integer> methodNameMap = new HashMap<String,Integer>();

  public JniPrintStream(Main classpath, OutputStream out, ClassWrapper klass)
  {
    super(out);
    this.classpath = classpath;
    computeOverloads(klass);
  }

  private void computeOverloads(ClassWrapper klass)
  {
    Iterator<?> i = klass.methods.iterator();
    while (i.hasNext())
      {
        MethodNode method = (MethodNode) i.next();
        if (! Modifier.isNative(method.access))
          continue;
        if (methodNameMap.containsKey(method.name))
          {
            Integer val = methodNameMap.get(method.name);
            methodNameMap.put(method.name, Integer.valueOf(val.intValue() + 1));
          }
        else
          methodNameMap.put(method.name, Integer.valueOf(1));
      }
  }

  public void print(Type type) throws IOException
  {
    print(JniHelper.getName(classpath, type));
  }

  public void print(MethodNode method, String className) throws IOException
  {
    print("Java_");
    print(className);
    print("_");
    print(JniHelper.mangle(method.name));
    Integer overloadCount = (Integer) methodNameMap.get(method.name);
    if (overloadCount.intValue() > 1)
      {
        print("__");
        int lastOffset = method.desc.lastIndexOf(')');
        print(JniHelper.mangle(method.desc.substring(1, lastOffset)));
      }
    print(" (JNIEnv *env");
    if (Modifier.isStatic(method.access))
      print(", jclass");
    else
      print(", jobject");
    Type[] types = Type.getArgumentTypes(method.desc);
    for (int i = 0; i < types.length; ++i)
      {
        print(", ");
        print(types[i]);
      }
    print(")");
  }
}
