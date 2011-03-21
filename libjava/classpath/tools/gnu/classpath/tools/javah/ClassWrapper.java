/* ClassWrapper.java - wrap ASM class objects
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;

public class ClassWrapper
    extends ClassNode
{
  Main classpath;

  ClassWrapper superClass;

  ArrayList<ClassWrapper> interfaceClasses;

  // The virtual table for this class.
  ArrayList<MethodNode> vtable;

  // A set of all the bridge method targets we've found.
  HashSet<String> bridgeTargets;

  // A set of all the method names in this class.
  HashSet<String> methodNames = new HashSet<String>();

  // This maps a method name + descriptor, e.g. "method()V", to the
  // name chosen for the method.  This is used when computing the
  // names of bridge method targets.
  HashMap<String,String> methodNameMap = new HashMap<String,String>();

  public ClassWrapper(Main classpath)
  {
    this.classpath = classpath;
  }

  public boolean hasNativeMethod()
  {
    Iterator<?> i = methods.iterator();
    while (i.hasNext())
      {
        MethodNode method = (MethodNode) i.next();
        if (Modifier.isNative(method.access))
          return true;
      }
    return false;
  }

  public boolean isThrowable() throws IOException
  {
    linkSupers();
    ClassWrapper self = this;
    while (self != null)
      {
        if (self.name.equals("java/lang/Throwable"))
          return true;
        self = self.superClass;
      }
    return false;
  }

  void linkSupers() throws IOException
  {
    if (superName == null)
      {
        // Object, do nothing.
        return;
      }
    if (superClass == null)
      {
        superClass = classpath.getClass(superName);
        assert interfaceClasses == null;
        interfaceClasses = new ArrayList<ClassWrapper>();
        for (int i = 0; i < interfaces.size(); ++i)
          {
            String ifname = (String) interfaces.get(i);
            ClassWrapper iface = classpath.getClass(ifname);
            iface.linkSupers();
            interfaceClasses.add(iface);
          }
      }
    superClass.linkSupers();
  }

  private int findSlot(MethodNode method)
  {
    for (int i = vtable.size() - 1; i >= 0; --i)
      {
        MethodNode base = vtable.get(i);
        if (MethodHelper.overrides(method, base))
          return i;
      }
    return - 1;
  }

  private void addInterfaceMethods(ClassWrapper iface)
  {
    Iterator<?> i = iface.methods.iterator();
    while (i.hasNext())
      {
        MethodNode im = (MethodNode) i.next();
        int slot = findSlot(im);
        if (slot == - 1)
          {
            vtable.add(im);
            // Also add it to our local methods.
            methods.add(im);
          }
      }
    addInterfaces(iface);
  }

  private void addInterfaces(ClassWrapper base)
  {
    if (base.interfaceClasses == null)
      return;
    Iterator<?> i = base.interfaceClasses.iterator();
    while (i.hasNext())
      {
        ClassWrapper iface = (ClassWrapper) i.next();
        addInterfaceMethods(iface);
      }
  }

  private void addLocalMethods()
  {
    Iterator<?> i = methods.iterator();
    while (i.hasNext())
      {
        MethodNode meth = (MethodNode) i.next();
        methodNames.add(meth.name);
        if (Modifier.isStatic(meth.access))
          continue;
        int slot = findSlot(meth);
        if (slot == - 1)
          vtable.add(meth);
        else
          vtable.set(slot, meth);
      }
  }

  private void makeVtable() throws IOException
  {
    if (vtable != null)
      return;
    if (superClass != null)
      {
        superClass.makeVtable();
        vtable = new ArrayList<MethodNode>(superClass.vtable);
        bridgeTargets = new HashSet<String>(superClass.bridgeTargets);
        methodNameMap = new HashMap<String,String>(superClass.methodNameMap);
      }
    else
      {
        // Object.
        vtable = new ArrayList<MethodNode>();
        bridgeTargets = new HashSet<String>();
        methodNameMap = new HashMap<String,String>();
      }
    addLocalMethods();
    addInterfaces(this);

    // Make a set of all the targets of bridge methods.  We rename
    // bridge target methods to avoid problems with C++.  You might
    // think we could rename the bridge methods themselves, but bridge
    // methods by definition override a method from the superclass --
    // and we have to consider the superclass' header as an
    // unchangeable entity.
    Iterator<?> i = methods.iterator();
    while (i.hasNext())
      {
        MethodNode m = (MethodNode) i.next();
        String desc = MethodHelper.getBridgeTarget(m);
        if (desc != null)
          {
            String sum = m.name + desc;
            boolean newTarget = bridgeTargets.add(sum);
            if (newTarget)
              {
                // Bridge target that is new in this class.
                String cname = this.name;
                int index = cname.lastIndexOf('/');
                cname = cname.substring(index + 1);
                methodNameMap.put(sum, cname + "$" + m.name);
              }
          }
      }
  }

  private void printFields(CniPrintStream out)
  {
    Iterator<?> i = fields.iterator();
    ClassWrapper self = superClass;
    while (i.hasNext())
      {
        FieldNode f = (FieldNode) i.next();
        boolean hasMethodName = methodNames.contains(f.name);
        if (FieldHelper.print(out, f, self, hasMethodName))
          self = null;
      }
  }

  private void printMethods(CniPrintStream out) throws IOException
  {
    makeVtable();

    // A given method is either static, overrides a super method, or
    // is already in vtable order.
    Iterator<?> i = methods.iterator();
    while (i.hasNext())
      {
        MethodNode m = (MethodNode) i.next();
        String nameToUse;
        String sum = m.name + m.desc;
        if (bridgeTargets.contains(sum))
          nameToUse = (String) methodNameMap.get(sum);
        else
          nameToUse = m.name;
        methodNameMap.put(sum, nameToUse);
        MethodHelper.print(out, m, this, nameToUse);
      }
  }

  private void printTextList(PrintStream out, int what, ArrayList<Text> textList)
  {
    if (textList == null)
      return;
    Iterator<Text> i = textList.iterator();
    boolean first = true;
    while (i.hasNext())
      {
        Text item = i.next();
        if (item.type == what)
          {
            if (first)
              {
                out.println();
                first = false;
              }
            if (what == Text.FRIEND)
              out.print("  friend ");
            out.println(item.text);
          }
      }
  }

  public void print(CniPrintStream out)
  {
    out.print("::");
    out.printName(name);
  }

  // This prints the body of a class to a CxxPrintStream.
  private void printContents(CniPrintStream out, ArrayList<Text> textList)
      throws IOException
  {
    printTextList(out, Text.PREPEND, textList);
    out.println();

    out.print("class ");
    // Don't use our print() -- we don't want the leading "::".
    out.printName(name);
    if (superClass != null)
      {
        out.print(" : public ");
        superClass.print(out);
      }
    out.println();
    out.println("{");

    printTextList(out, Text.ADD, textList);
    out.println();

    // Note: methods must come first, as we build the list
    // of method names while printing them.
    printMethods(out);
    printFields(out);

    out.setModifiers(Modifier.PUBLIC);
    out.println("  static ::java::lang::Class class$;");

    printTextList(out, Text.FRIEND, textList);

    out.print("}");
    if (Modifier.isInterface(access))
      out.print(" __attribute__ ((java_interface))");
    out.println(";");

    printTextList(out, Text.APPEND, textList);
  }

  public void printFully(PrintStream out) throws IOException
  {
    linkSupers();

    ArrayList<Text> textList = classpath.getClassTextList(name);

    out.println("// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-");
    out.println();
    String xname = "__" + name.replaceAll("/", "_") + "__";
    out.println("#ifndef " + xname);
    out.println("#define " + xname);
    out.println();
    out.println("#pragma interface");
    out.println();

    if (superClass != null)
      {
        out.print("#include <");
        out.print(superName);
        out.println(".h>");
      }

    // Write the body of the stream here. This lets
    // us emit the namespaces without a second pass.
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    CniPrintStream cxxOut = new CniPrintStream(bytes);
    cxxOut.addClass(this);
    printContents(cxxOut, textList);
    cxxOut.printNamespaces(out);
    bytes.writeTo(out);

    out.println();
    out.println("#endif // " + xname);
  }

  public String toString()
  {
    return name;
  }
}
