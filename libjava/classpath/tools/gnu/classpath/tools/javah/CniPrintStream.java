/* CniPrintStream.java - PrintStream that emits CNI declarations 
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

import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashSet;

import org.objectweb.asm.Type;

public class CniPrintStream
    extends PrintStream
{
  int currentModifiers = Modifier.PRIVATE;

  // True if we saw an array type.
  boolean sawArray;

  // All the classes referenced by this header.
  HashSet allClasses = new HashSet();

  String[] previousPackage = new String[0];

  public CniPrintStream(OutputStream out)
  {
    super(out);
  }

  public void addClass(ClassWrapper cw)
  {
    allClasses.add(cw.name);
  }

  public void setModifiers(int newMods)
  {
    newMods &= (Modifier.PUBLIC | Modifier.PROTECTED | Modifier.PRIVATE);
    if (newMods != currentModifiers)
      {
        switch (newMods)
          {
          case Modifier.PUBLIC:
            println("public:");
            break;
          case Modifier.PROTECTED:
            println("public: // actually protected");
            break;
          case Modifier.PRIVATE:
            println("private:");
            break;
          default:
            println("public: // actually package-private");
            break;
          }
        currentModifiers = newMods;
      }
  }

  private String getName(Type type)
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
    else
      {
        assert type == Type.VOID_TYPE;
        return "void";
      }
  }

  public String getClassName(Type type)
  {
    String name = type.toString();
    name = name.substring(1, name.length() - 1);
    // Add the plain class name; we'll handle it when
    // we process namespaces.
    allClasses.add(name);
    return "::" + name.replaceAll("/", "::") + " *";
  }

  public void print(Type type)
  {
    int arrayCount = 0;
    if (type.getSort() == Type.ARRAY)
      {
        arrayCount = type.getDimensions();
        for (int i = 0; i < arrayCount; ++i)
          print("JArray< ");
        type = type.getElementType();
        sawArray = true;
      }
    if (type.getSort() == Type.OBJECT)
      {
        print(getClassName(type));
      }
    else
      {
        print(getName(type));
      }
    if (arrayCount > 0)
      {
        while (arrayCount-- > 0)
          {
            print(" > *");
          }
      }
  }

  private void indent(PrintStream out, int n)
  {
    for (int i = 0; i < n; ++i)
      {
        out.print("  ");
      }
  }

  private void moveToPackage(PrintStream out, String[] pkgParts)
  {
    // Find greatest common part.
    int commonIndex;
    for (commonIndex = 0; commonIndex < previousPackage.length; ++commonIndex)
      {
        if (commonIndex >= pkgParts.length)
          break;
        if (! previousPackage[commonIndex].equals(pkgParts[commonIndex]))
          break;
      }
    // Close old parts after the common part.
    for (int j = previousPackage.length - 1; j >= commonIndex; --j)
      {
        indent(out, j + 1);
        out.println("}");
      }
    // Open new parts.
    for (int j = commonIndex; j < pkgParts.length; ++j)
      {
        indent(out, j + 1);
        out.print("namespace ");
        out.println(pkgParts[j]);
        indent(out, j + 1);
        out.println("{");
      }
    previousPackage = pkgParts;
  }

  private void writeClass(PrintStream out, String klass)
  {
    int index = klass.lastIndexOf('/');
    String pkg = index == -1 ? "" : klass.substring(0, index);
    String[] pkgParts = index == -1 ? new String[0] : pkg.split("/");
    String className = index == -1 ? klass : klass.substring(index + 1);
    moveToPackage(out, pkgParts);
    indent(out, pkgParts.length + 2);
    out.print("class ");
    out.print(className);
    out.println(";");
  }

  public void printNamespaces(PrintStream out)
  {
    if (sawArray)
      {
        out.println("#include <gcj/array.h>");
        out.println();
      }

    String[] classes = (String[]) allClasses.toArray(new String[0]);
    Arrays.sort(classes);

    boolean first = true;
    boolean seen = false;
    for (int i = 0; i < classes.length; ++i)
      {
        String klass = classes[i];
        if (klass.startsWith("java/lang/") || klass.startsWith("java/io/")
            || klass.startsWith("java/util/"))
          continue;
        if (first)
          {
            out.println("extern \"Java\"");
            out.println("{");
            first = false;
            seen = true;
          }
        writeClass(out, klass);
      }
    if (seen)
      {
        moveToPackage(out, new String[0]);
        out.println("}");
      }
  }
}
