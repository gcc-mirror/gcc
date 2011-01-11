/***
 * ASM XML Adapter
 * Copyright (c) 2004, Eugene Kuleshov
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
package org.objectweb.asm.xml;

import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.AbstractVisitor;
import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.AttributesImpl;

/**
 * A {@link MethodVisitor} that generates SAX 2.0 events from the visited
 * method.
 *
 * @see org.objectweb.asm.xml.SAXClassAdapter
 * @see org.objectweb.asm.xml.Processor
 *
 * @author Eugene Kuleshov
 */
public final class SAXCodeAdapter extends SAXAdapter implements MethodVisitor {
    private Map labelNames;

    /**
     * Constructs a new {@link SAXCodeAdapter SAXCodeAdapter} object.
     *
     * @param h content handler that will be used to send SAX 2.0 events.
     * @param access
     */
    public SAXCodeAdapter(ContentHandler h, int access) {
        super(h);
        labelNames = new HashMap();

        if ((access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_INTERFACE | Opcodes.ACC_NATIVE)) == 0)
        {
            addStart("code", new AttributesImpl());
        }
    }

    public final void visitCode() {
    }

    public final void visitInsn(int opcode) {
        addElement(AbstractVisitor.OPCODES[opcode], new AttributesImpl());
    }

    public final void visitIntInsn(int opcode, int operand) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "value", "value", "", Integer.toString(operand));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitVarInsn(int opcode, int var) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "var", "var", "", Integer.toString(var));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitTypeInsn(int opcode, String desc) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitFieldInsn(
        int opcode,
        String owner,
        String name,
        String desc)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "owner", "owner", "", owner);
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitMethodInsn(
        int opcode,
        String owner,
        String name,
        String desc)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "owner", "owner", "", owner);
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitJumpInsn(int opcode, Label label) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "label", "label", "", getLabel(label));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitLabel(Label label) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "name", "name", "", getLabel(label));
        addElement("Label", attrs);
    }

    public final void visitLdcInsn(Object cst) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("",
                "cst",
                "cst",
                "",
                SAXClassAdapter.encode(cst.toString()));
        attrs.addAttribute("",
                "desc",
                "desc",
                "",
                Type.getDescriptor(cst.getClass()));
        addElement(AbstractVisitor.OPCODES[Opcodes.LDC], attrs);
    }

    public final void visitIincInsn(int var, int increment) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "var", "var", "", Integer.toString(var));
        attrs.addAttribute("", "inc", "inc", "", Integer.toString(increment));
        addElement(AbstractVisitor.OPCODES[Opcodes.IINC], attrs);
    }

    public final void visitTableSwitchInsn(
        int min,
        int max,
        Label dflt,
        Label[] labels)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "min", "min", "", Integer.toString(min));
        attrs.addAttribute("", "max", "max", "", Integer.toString(max));
        attrs.addAttribute("", "dflt", "dflt", "", getLabel(dflt));
        String o = AbstractVisitor.OPCODES[Opcodes.TABLESWITCH];
        addStart(o, attrs);
        for (int i = 0; i < labels.length; i++) {
            AttributesImpl att2 = new AttributesImpl();
            att2.addAttribute("", "name", "name", "", getLabel(labels[i]));
            addElement("label", att2);
        }
        addEnd(o);
    }

    public final void visitLookupSwitchInsn(
        Label dflt,
        int[] keys,
        Label[] labels)
    {
        AttributesImpl att = new AttributesImpl();
        att.addAttribute("", "dflt", "dflt", "", getLabel(dflt));
        String o = AbstractVisitor.OPCODES[Opcodes.LOOKUPSWITCH];
        addStart(o, att);
        for (int i = 0; i < labels.length; i++) {
            AttributesImpl att2 = new AttributesImpl();
            att2.addAttribute("", "name", "name", "", getLabel(labels[i]));
            att2.addAttribute("", "key", "key", "", Integer.toString(keys[i]));
            addElement("label", att2);
        }
        addEnd(o);
    }

    public final void visitMultiANewArrayInsn(String desc, int dims) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "desc", "desc", "", desc);
        attrs.addAttribute("", "dims", "dims", "", Integer.toString(dims));
        addElement(AbstractVisitor.OPCODES[Opcodes.MULTIANEWARRAY], attrs);
    }

    public final void visitTryCatchBlock(
        Label start,
        Label end,
        Label handler,
        String type)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        attrs.addAttribute("", "end", "end", "", getLabel(end));
        attrs.addAttribute("", "handler", "handler", "", getLabel(handler));
        if (type != null)
            attrs.addAttribute("", "type", "type", "", type);
        addElement("TryCatch", attrs);
    }

    public final void visitMaxs(int maxStack, int maxLocals) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("",
                "maxStack",
                "maxStack",
                "",
                Integer.toString(maxStack));
        attrs.addAttribute("",
                "maxLocals",
                "maxLocals",
                "",
                Integer.toString(maxLocals));
        addElement("Max", attrs);

        addEnd("code");
    }

    public void visitLocalVariable(
        String name,
        String desc,
        String signature,
        Label start,
        Label end,
        int index)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        if (signature != null)
            attrs.addAttribute("",
                    "signature",
                    "signature",
                    "",
                    SAXClassAdapter.encode(signature));
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        attrs.addAttribute("", "end", "end", "", getLabel(end));
        attrs.addAttribute("", "var", "var", "", Integer.toString(index));
        addElement("LocalVar", attrs);
    }

    public final void visitLineNumber(int line, Label start) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "line", "line", "", Integer.toString(line));
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        addElement("LineNumber", attrs);
    }

    public AnnotationVisitor visitAnnotationDefault() {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotationDefault",
                0,
                null,
                null);
    }

    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotation",
                visible ? 1 : -1,
                null,
                desc);
    }

    public AnnotationVisitor visitParameterAnnotation(
        int parameter,
        String desc,
        boolean visible)
    {
        return new SAXAnnotationAdapter(getContentHandler(),
                "parameterAnnotation",
                visible ? 1 : -1,
                parameter,
                desc);
    }

    public void visitEnd() {
        addEnd("method");
    }

    public final void visitAttribute(Attribute attr) {
        // TODO Auto-generated SAXCodeAdapter.visitAttribute
    }

    private final String getLabel(Label label) {
        String name = (String) labelNames.get(label);
        if (name == null) {
            name = Integer.toString(labelNames.size());
            labelNames.put(label, name);
        }
        return name;
    }

}
