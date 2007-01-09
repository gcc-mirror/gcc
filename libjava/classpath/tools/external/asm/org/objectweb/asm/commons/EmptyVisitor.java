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
package org.objectweb.asm.commons;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

/**
 * An empty implementation of the ASM visitor interfaces.
 * 
 * @author Eric Bruneton
 */
public class EmptyVisitor implements
        ClassVisitor,
        FieldVisitor,
        MethodVisitor,
        AnnotationVisitor
{

    public void visit(
        int version,
        int access,
        String name,
        String signature,
        String superName,
        String[] interfaces)
    {
    }

    public void visitSource(String source, String debug) {
    }

    public void visitOuterClass(String owner, String name, String desc) {
    }

    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        return this;
    }

    public void visitAttribute(Attribute attr) {
    }

    public void visitInnerClass(
        String name,
        String outerName,
        String innerName,
        int access)
    {
    }

    public FieldVisitor visitField(
        int access,
        String name,
        String desc,
        String signature,
        Object value)
    {
        return this;
    }

    public MethodVisitor visitMethod(
        int access,
        String name,
        String desc,
        String signature,
        String[] exceptions)
    {
        return this;
    }

    public void visitEnd() {
    }

    public AnnotationVisitor visitAnnotationDefault() {
        return this;
    }

    public AnnotationVisitor visitParameterAnnotation(
        int parameter,
        String desc,
        boolean visible)
    {
        return this;
    }

    public void visitCode() {
    }

    public void visitInsn(int opcode) {
    }

    public void visitIntInsn(int opcode, int operand) {
    }

    public void visitVarInsn(int opcode, int var) {
    }

    public void visitTypeInsn(int opcode, String desc) {
    }

    public void visitFieldInsn(
        int opcode,
        String owner,
        String name,
        String desc)
    {
    }

    public void visitMethodInsn(
        int opcode,
        String owner,
        String name,
        String desc)
    {
    }

    public void visitJumpInsn(int opcode, Label label) {
    }

    public void visitLabel(Label label) {
    }

    public void visitLdcInsn(Object cst) {
    }

    public void visitIincInsn(int var, int increment) {
    }

    public void visitTableSwitchInsn(
        int min,
        int max,
        Label dflt,
        Label labels[])
    {
    }

    public void visitLookupSwitchInsn(Label dflt, int keys[], Label labels[]) {
    }

    public void visitMultiANewArrayInsn(String desc, int dims) {
    }

    public void visitTryCatchBlock(
        Label start,
        Label end,
        Label handler,
        String type)
    {
    }

    public void visitLocalVariable(
        String name,
        String desc,
        String signature,
        Label start,
        Label end,
        int index)
    {
    }

    public void visitLineNumber(int line, Label start) {
    }

    public void visitMaxs(int maxStack, int maxLocals) {
    }

    public void visit(String name, Object value) {
    }

    public void visitEnum(String name, String desc, String value) {
    }

    public AnnotationVisitor visitAnnotation(String name, String desc) {
        return this;
    }

    public AnnotationVisitor visitArray(String name) {
        return this;
    }
}
