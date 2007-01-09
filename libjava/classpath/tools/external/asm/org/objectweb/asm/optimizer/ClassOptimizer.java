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
package org.objectweb.asm.optimizer;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * A {@link ClassAdapter} that renames fields and methods, and removes debug
 * info.
 * 
 * @author Eric Bruneton
 */
public class ClassOptimizer extends ClassAdapter {

    private NameMapping mapping;

    private String className;

    private String pkgName;

    public ClassOptimizer(final ClassVisitor cv, final NameMapping mapping) {
        super(cv);
        this.mapping = mapping;
    }

    public String getClassName() {
        return className;
    }

    // ------------------------------------------------------------------------
    // Overriden methods
    // ------------------------------------------------------------------------

    public void visit(
        final int version,
        final int access,
        final String name,
        final String signature,
        final String superName,
        final String[] interfaces)
    {
        className = name;
        pkgName = name.substring(0, name.lastIndexOf('/'));
        cv.visit(version,
                access,
                mapping.map(name),
                null,
                mapping.map(superName),
                interfaces);
    }

    public void visitSource(final String source, final String debug) {
        // remove debug info
    }

    public void visitOuterClass(
        final String owner,
        final String name,
        final String desc)
    {
        // remove debug info
    }

    public AnnotationVisitor visitAnnotation(
        final String desc,
        final boolean visible)
    {
        throw new UnsupportedOperationException();
    }

    public void visitAttribute(final Attribute attr) {
        // remove non standard attribute
    }

    public void visitInnerClass(
        final String name,
        final String outerName,
        final String innerName,
        final int access)
    {
        // remove debug info
    }

    public FieldVisitor visitField(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final Object value)
    {
        String s = className + "." + name;
        if ((access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED)) == 0) {
            if ((access & Opcodes.ACC_FINAL) != 0
                    && (access & Opcodes.ACC_STATIC) != 0 && desc.equals("I"))
            {
                return null;
            }
            if (pkgName.equals("org/objectweb/asm")
                    && mapping.map(s).equals(name))
            {
                System.out.println("INFO: " + s + " could be renamed");
            }
            cv.visitField(access,
                    mapping.map(s),
                    mapping.fix(desc),
                    null,
                    value);
        } else {
            if (!mapping.map(s).equals(name)) {
                throw new RuntimeException("The public or protected field " + s
                        + " must not be renamed.");
            }
            cv.visitField(access, name, desc, null, value);
        }
        return null; // remove debug info
    }

    public MethodVisitor visitMethod(
        final int access,
        final String name,
        final String desc,
        final String signature,
        final String[] exceptions)
    {
        String s = className + "." + name + desc;
        if ((access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED)) == 0) {
            if (pkgName.equals("org/objectweb/asm") && !name.startsWith("<")
                    && mapping.map(s).equals(name))
            {
                System.out.println("INFO: " + s + " could be renamed");
            }
            return new MethodOptimizer(cv.visitMethod(access,
                    mapping.map(s),
                    mapping.fix(desc),
                    null,
                    exceptions), mapping);
        } else {
            if (!mapping.map(s).equals(name)) {
                throw new RuntimeException("The public or protected method "
                        + s + " must not be renamed.");
            }
            return new MethodOptimizer(cv.visitMethod(access,
                    name,
                    desc,
                    null,
                    exceptions), mapping);
        }
    }
}
