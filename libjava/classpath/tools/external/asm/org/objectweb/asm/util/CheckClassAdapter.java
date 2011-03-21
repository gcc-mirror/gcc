/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2005 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.objectweb.asm.util;

import java.io.FileInputStream;
import java.io.PrintWriter;
import java.util.List;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.TryCatchBlockNode;
import org.objectweb.asm.tree.analysis.Analyzer;
import org.objectweb.asm.tree.analysis.SimpleVerifier;
import org.objectweb.asm.tree.analysis.Frame;

/**
 * A {@link ClassAdapter} that checks that its methods are properly used. More
 * precisely this class adapter checks each method call individually, based
 * <i>only</i> on its arguments, but does <i>not</i> check the <i>sequence</i>
 * of method calls. For example, the invalid sequence
 * <tt>visitField(ACC_PUBLIC, "i", "I", null)</tt> <tt>visitField(ACC_PUBLIC,
 * "i", "D", null)</tt>
 * will <i>not</i> be detected by this class adapter.
 *
 * @author Eric Bruneton
 */
public class CheckClassAdapter extends ClassAdapter {

    /**
     * <tt>true</tt> if the visit method has been called.
     */
    private boolean start;

    /**
     * <tt>true</tt> if the visitSource method has been called.
     */
    private boolean source;

    /**
     * <tt>true</tt> if the visitOuterClass method has been called.
     */
    private boolean outer;

    /**
     * <tt>true</tt> if the visitEnd method has been called.
     */
    private boolean end;

    /**
     * Checks a given class. <p> Usage: CheckClassAdapter &lt;fully qualified
     * class name or class file name&gt;
     *
     * @param args the command line arguments.
     *
     * @throws Exception if the class cannot be found, or if an IO exception
     *         occurs.
     */
    public static void main(final String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("Verifies the given class.");
            System.err.println("Usage: CheckClassAdapter "
                    + "<fully qualified class name or class file name>");
            return;
        }
        ClassReader cr;
        if (args[0].endsWith(".class")) {
            cr = new ClassReader(new FileInputStream(args[0]));
        } else {
            cr = new ClassReader(args[0]);
        }

        verify(cr, false, new PrintWriter(System.err));
    }

    /**
     * Checks a given class
     *
     * @param cr a <code>ClassReader</code> that contains bytecode for the analysis.
     * @param dump true if bytecode should be printed out not only when errors are found.
     * @param pw write where results going to be printed
     */
    public static void verify(ClassReader cr, boolean dump, PrintWriter pw) {
        ClassNode cn = new ClassNode();
        cr.accept(new CheckClassAdapter(cn), true);

        List methods = cn.methods;
        for (int i = 0; i < methods.size(); ++i) {
            MethodNode method = (MethodNode) methods.get(i);
            if (method.instructions.size() > 0) {
                Analyzer a = new Analyzer(new SimpleVerifier(Type.getType("L"
                        + cn.name + ";"),
                        Type.getType("L" + cn.superName + ";"),
                        (cn.access & Opcodes.ACC_INTERFACE) != 0));
                try {
                    a.analyze(cn.name, method);
                    if (!dump) {
                        continue;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                Frame[] frames = a.getFrames();

                TraceMethodVisitor mv = new TraceMethodVisitor();

                pw.println(method.name + method.desc);
                for (int j = 0; j < method.instructions.size(); ++j) {
                    ((AbstractInsnNode) method.instructions.get(j)).accept(mv);

                    StringBuffer s = new StringBuffer();
                    Frame f = frames[j];
                    if (f == null) {
                        s.append('?');
                    } else {
                        for (int k = 0; k < f.getLocals(); ++k) {
                            s.append(getShortName(f.getLocal(k).toString()))
                                    .append(' ');
                        }
                        s.append(" : ");
                        for (int k = 0; k < f.getStackSize(); ++k) {
                            s.append(getShortName(f.getStack(k).toString()))
                                    .append(' ');
                        }
                    }
                    while (s.length() < method.maxStack + method.maxLocals + 1)
                    {
                        s.append(' ');
                    }
                    pw.print(Integer.toString(j + 100000).substring(1));
                    pw.print(" " + s + " : " + mv.buf); // mv.text.get(j));
                }
                for (int j = 0; j < method.tryCatchBlocks.size(); ++j) {
                    ((TryCatchBlockNode) method.tryCatchBlocks.get(j)).accept(mv);
                    pw.print(" " + mv.buf);
                }
                pw.println();
            }
        }
    }

    private static String getShortName(String name) {
        int n = name.lastIndexOf('/');
        int k = name.length();
        if(name.charAt(k-1)==';') k--;
        return n==-1 ? name : name.substring(n+1, k);
    }

    /**
     * Constructs a new {@link CheckClassAdapter}.
     *
     * @param cv the class visitor to which this adapter must delegate calls.
     */
    public CheckClassAdapter(final ClassVisitor cv) {
        super(cv);
    }

    // ------------------------------------------------------------------------
    // Implementation of the ClassVisitor interface
    // ------------------------------------------------------------------------

    public void visit(
        final int version,
        final int access,
        final String name,
        final String signature,
        final String superName,
        final String[] interfaces)
    {
        if (start) {
            throw new IllegalStateException("visit must be called only once");
        } else {
            start = true;
        }
        checkState();
        checkAccess(access, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL
                + Opcodes.ACC_SUPER + Opcodes.ACC_INTERFACE
                + Opcodes.ACC_ABSTRACT + Opcodes.ACC_SYNTHETIC
                + Opcodes.ACC_ANNOTATION + Opcodes.ACC_ENUM
                + Opcodes.ACC_DEPRECATED);
        CheckMethodAdapter.checkInternalName(name, "class name");
        if ("java/lang/Object".equals(name)) {
            if (superName != null) {
                throw new IllegalArgumentException("The super class name of the Object class must be 'null'");
            }
        } else {
            CheckMethodAdapter.checkInternalName(superName, "super class name");
        }
        if (signature != null) {
            // TODO
        }
        if ((access & Opcodes.ACC_INTERFACE) != 0) {
            if (!"java/lang/Object".equals(superName)) {
                throw new IllegalArgumentException("The super class name of interfaces must be 'java/lang/Object'");
            }
        }
        if (interfaces != null) {
            for (int i = 0; i < interfaces.length; ++i) {
                CheckMethodAdapter.checkInternalName(interfaces[i],
                        "interface name at index " + i);
            }
        }
        cv.visit(version, access, name, signature, superName, interfaces);
    }

    public void visitSource(final String file, final String debug) {
        checkState();
        if (source) {
            throw new IllegalStateException("visitSource can be called only once.");
        }
        source = true;
        cv.visitSource(file, debug);
    }

    public void visitOuterClass(
        final String owner,
        final String name,
        final String desc)
    {
        checkState();
        if (outer) {
            throw new IllegalStateException("visitSource can be called only once.");
        }
        outer = true;
        if (owner == null) {
            throw new IllegalArgumentException("Illegal outer class owner");
        }
        if (desc != null) {
            CheckMethodAdapter.checkMethodDesc(desc);
        }
        cv.visitOuterClass(owner, name, desc);
    }

    public void visitInnerClass(
        final String name,
        final String outerName,
        final String innerName,
        final int access)
    {
        checkState();
        CheckMethodAdapter.checkInternalName(name, "class name");
        if (outerName != null) {
            CheckMethodAdapter.checkInternalName(outerName, "outer class name");
        }
        if (innerName != null) {
            CheckMethodAdapter.checkIdentifier(innerName, "inner class name");
        }
        checkAccess(access, Opcodes.ACC_PUBLIC + Opcodes.ACC_PRIVATE
                + Opcodes.ACC_PROTECTED + Opcodes.ACC_STATIC
                + Opcodes.ACC_FINAL + Opcodes.ACC_INTERFACE
                + Opcodes.ACC_ABSTRACT + Opcodes.ACC_SYNTHETIC
                + Opcodes.ACC_ANNOTATION + Opcodes.ACC_ENUM);
        cv.visitInnerClass(name, outerName, innerName, access);
    }

    public FieldVisitor visitField(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final Object value)
    {
        checkState();
        checkAccess(access, Opcodes.ACC_PUBLIC + Opcodes.ACC_PRIVATE
                + Opcodes.ACC_PROTECTED + Opcodes.ACC_STATIC
                + Opcodes.ACC_FINAL + Opcodes.ACC_VOLATILE
                + Opcodes.ACC_TRANSIENT + Opcodes.ACC_SYNTHETIC
                + Opcodes.ACC_ENUM + Opcodes.ACC_DEPRECATED);
        CheckMethodAdapter.checkIdentifier(name, "field name");
        CheckMethodAdapter.checkDesc(desc, false);
        if (signature != null) {
            // TODO
        }
        if (value != null) {
            CheckMethodAdapter.checkConstant(value);
        }
        FieldVisitor av = cv.visitField(access, name, desc, signature, value);
        return new CheckFieldAdapter(av);
    }

    public MethodVisitor visitMethod(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final String[] exceptions)
    {
        checkState();
        checkAccess(access, Opcodes.ACC_PUBLIC + Opcodes.ACC_PRIVATE
                + Opcodes.ACC_PROTECTED + Opcodes.ACC_STATIC
                + Opcodes.ACC_FINAL + Opcodes.ACC_SYNCHRONIZED
                + Opcodes.ACC_BRIDGE + Opcodes.ACC_VARARGS + Opcodes.ACC_NATIVE
                + Opcodes.ACC_ABSTRACT + Opcodes.ACC_STRICT
                + Opcodes.ACC_SYNTHETIC + Opcodes.ACC_DEPRECATED);
        CheckMethodAdapter.checkMethodIdentifier(name, "method name");
        CheckMethodAdapter.checkMethodDesc(desc);
        if (signature != null) {
            // TODO
        }
        if (exceptions != null) {
            for (int i = 0; i < exceptions.length; ++i) {
                CheckMethodAdapter.checkInternalName(exceptions[i],
                        "exception name at index " + i);
            }
        }
        return new CheckMethodAdapter(cv.visitMethod(access,
                name,
                desc,
                signature,
                exceptions));
    }

    public AnnotationVisitor visitAnnotation(
        final String desc,
        final boolean visible)
    {
        checkState();
        CheckMethodAdapter.checkDesc(desc, false);
        return new CheckAnnotationAdapter(cv.visitAnnotation(desc, visible));
    }

    public void visitAttribute(final Attribute attr) {
        checkState();
        if (attr == null) {
            throw new IllegalArgumentException("Invalid attribute (must not be null)");
        }
        cv.visitAttribute(attr);
    }

    public void visitEnd() {
        checkState();
        end = true;
        cv.visitEnd();
    }

    // ------------------------------------------------------------------------
    // Utility methods
    // ------------------------------------------------------------------------

    /**
     * Checks that the visit method has been called and that visitEnd has not
     * been called.
     */
    private void checkState() {
        if (!start) {
            throw new IllegalStateException("Cannot visit member before visit has been called.");
        }
        if (end) {
            throw new IllegalStateException("Cannot visit member after visitEnd has been called.");
        }
    }

    /**
     * Checks that the given access flags do not contain invalid flags. This
     * method also checks that mutually incompatible flags are not set
     * simultaneously.
     *
     * @param access the access flags to be checked
     * @param possibleAccess the valid access flags.
     */
    static void checkAccess(final int access, final int possibleAccess) {
        if ((access & ~possibleAccess) != 0) {
            throw new IllegalArgumentException("Invalid access flags: "
                    + access);
        }
        int pub = ((access & Opcodes.ACC_PUBLIC) != 0 ? 1 : 0);
        int pri = ((access & Opcodes.ACC_PRIVATE) != 0 ? 1 : 0);
        int pro = ((access & Opcodes.ACC_PROTECTED) != 0 ? 1 : 0);
        if (pub + pri + pro > 1) {
            throw new IllegalArgumentException("public private and protected are mutually exclusive: "
                    + access);
        }
        int fin = ((access & Opcodes.ACC_FINAL) != 0 ? 1 : 0);
        int abs = ((access & Opcodes.ACC_ABSTRACT) != 0 ? 1 : 0);
        if (fin + abs > 1) {
            throw new IllegalArgumentException("final and abstract are mutually exclusive: "
                    + access);
        }
    }
}
