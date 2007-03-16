/* CniStubPrinter.java - Generate a CNI stub file
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


package gnu.classpath.tools.javah;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.Iterator;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.MethodNode;

public class CniStubPrinter
    extends Printer
{
  protected CniStubPrinter(Main classpath, File outFile, boolean isDir,
                           boolean force)
  {
    super(classpath, outFile, isDir, force);
  }

  private void printDecl(CniPrintStream out, String className, MethodNode method)
  {
    out.print(className);
    out.print("::");
    out.print(method.name);
    out.print("(");
    Type[] argTypes = Type.getArgumentTypes(method.desc);
    for (int j = 0; j < argTypes.length; ++j)
      {
        if (j > 0)
          out.print(", ");
        out.print(argTypes[j]);
      }
    out.print(")");
  }

  protected void writePreambleImpl(PrintStream out)
  {
    out.println("// This file is intended to give you a head start on implementing native");
    out.println("// methods using CNI.");
    out.println("// Be aware: running 'gcjh -stubs' once more for this class may");
    out.println("// overwrite any edits you have made to this file.");
    out.println();
    out.println("#include <gcj/cni.h>");
    out.println("#include <java/lang/UnsupportedOperationException.h>");
  }

  protected PrintStream getPrintStreamImpl(FileOutputStream fos,
                                           ClassWrapper klass)
  {
    return new CniPrintStream(fos);
  }

  public void printClass(File filename, ClassWrapper klass) throws IOException
  {
    if (! klass.hasNativeMethod())
      return;
    String className = klass.name.replaceAll("/", "::");
    CniPrintStream out = (CniPrintStream) getPrintStream(filename + ".cc",
                                                         klass);
    if (out == null)
      return;
    out.println();
    out.println("#include <" + klass.name + ".h>");
    out.println();

    Iterator i = klass.methods.iterator();
    boolean first = true;
    while (i.hasNext())
      {
        MethodNode method = (MethodNode) i.next();
        if (! Modifier.isNative(method.access))
          continue;
        if (! first)
          out.println();
        first = false;
        out.print(Type.getReturnType(method.desc));
        out.println();
        printDecl(out, className, method);
        out.println();
        out.println("{");
        out.print("  throw new ::java::lang::UnsupportedOperationException(");
        out.print("JvNewStringLatin1 (\"");
        printDecl(out, className, method);
        out.println("\"));");
        out.println("}");
      }
    out.close();
  }
}
