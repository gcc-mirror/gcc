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
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;

/**
 * A {@link MethodAdapter} that renames fields and methods, and removes debug
 * info.
 *
 * @author Eric Bruneton
 */
public class MethodOptimizer extends MethodAdapter {

    private NameMapping mapping;

    public MethodOptimizer(final MethodVisitor mv, final NameMapping mapping) {
        super(mv);
        this.mapping = mapping;
    }

    // ------------------------------------------------------------------------
    // Overriden methods
    // ------------------------------------------------------------------------

    public AnnotationVisitor visitAnnotationDefault() {
        throw new UnsupportedOperationException();
    }

    public AnnotationVisitor visitParameterAnnotation(
        final int parameter,
        final String desc,
        final boolean visible)
    {
        throw new UnsupportedOperationException();
    }

    public void visitTypeInsn(final int opcode, final String desc) {
        mv.visitTypeInsn(opcode, desc.startsWith("[")
                ? mapping.fix(desc)
                : mapping.map(desc));
    }

    public void visitFieldInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitFieldInsn(opcode, mapping.map(owner), mapping.map(owner + "."
                + name), mapping.fix(desc));
    }

    public void visitMethodInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitMethodInsn(opcode, mapping.map(owner), mapping.map(owner + "."
                + name + desc), mapping.fix(desc));
    }

    public void visitLocalVariable(
        final String name,
        final String desc,
        final String signature,
        final Label start,
        final Label end,
        final int index)
    {
        // remove debug info
    }

    public void visitLineNumber(final int line, final Label start) {
        // remove debug info
    }
}
