/* MethodHelper.java - helper class for manipulating methods
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

import java.lang.reflect.Modifier;
import java.util.Iterator;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;

public class MethodHelper
{

  public static boolean overrides(MethodNode derived, MethodNode base)
  {
    if (! derived.name.equals(base.name))
      return false;
    if (! derived.desc.equals(base.desc))
      return false;
    // FIXME: permission madness?
    return true;
  }

  public static String getBridgeTarget(MethodNode meth)
  {
    if ((meth.access & Opcodes.ACC_BRIDGE) == 0)
      return null;
    Iterator i = meth.instructions.iterator();
    while (i.hasNext())
      {
        AbstractInsnNode insn = (AbstractInsnNode) i.next();
        if (! (insn instanceof MethodInsnNode))
          continue;
        return ((MethodInsnNode) insn).desc;
      }
    return null;
  }

  public static void print(CniPrintStream out, MethodNode meth,
                           ClassWrapper declarer, String realMethodName)
  {
    if ("<clinit>".equals(meth.name))
      return;
    boolean isInit = "<init>".equals(meth.name);
    out.setModifiers(meth.access);
    out.print("  ");
    if (Modifier.isStatic(meth.access))
      out.print("static ");
    // If a class is final then we might as well skip 'virtual'.
    // The reason here is that it is safe in this case for C++
    // ABI code to generate a direct call. The method does end
    // up in the vtable (for BC code) but we don't care. Also,
    // the class can't be derived from anyway.
    else if (! isInit && ! Modifier.isPrivate(meth.access)
             && ! Modifier.isFinal(declarer.access))
      out.print("virtual ");
    if (! isInit)
      {
        out.print(Type.getReturnType(meth.desc));
        out.print(" ");
	out.print(realMethodName);
      }
    else
      {
        String name = declarer.name;
        int index = name.lastIndexOf('/');
        name = name.substring(index + 1);
        out.print(name);
      }
    out.print("(");
    Type[] argTypes = Type.getArgumentTypes(meth.desc);
    for (int i = 0; i < argTypes.length; ++i)
      {
        if (i > 0)
          out.print(", ");
        out.print(argTypes[i]);
      }
    out.print(")");
    if (Modifier.isAbstract(meth.access))
      out.print(" = 0");
    out.println(";");
  }
}
