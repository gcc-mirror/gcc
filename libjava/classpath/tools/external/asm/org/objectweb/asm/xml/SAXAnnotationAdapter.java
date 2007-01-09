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

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Type;
import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.AttributesImpl;

/**
 * SAXAnnotationAdapter
 * 
 * @author Eugene Kuleshov
 */
public class SAXAnnotationAdapter extends SAXAdapter implements
        AnnotationVisitor
{
    private final String elementName;

    public SAXAnnotationAdapter(
        ContentHandler h,
        String elementName,
        int visible,
        String name,
        String desc)
    {
        this(h, elementName, visible, desc, name, -1);
    }

    public SAXAnnotationAdapter(
        ContentHandler h,
        String elementName,
        int visible,
        int parameter,
        String desc)
    {
        this(h, elementName, visible, desc, null, parameter);
    }

    private SAXAnnotationAdapter(
        ContentHandler h,
        String elementName,
        int visible,
        String desc,
        String name,
        int parameter)
    {
        super(h);
        this.elementName = elementName;

        AttributesImpl att = new AttributesImpl();
        if (name != null)
            att.addAttribute("", "name", "name", "", name);
        if (visible != 0)
            att.addAttribute("", "visible", "visible", "", visible > 0
                    ? "true"
                    : "false");
        if (parameter != -1)
            att.addAttribute("",
                    "parameter",
                    "parameter",
                    "",
                    Integer.toString(parameter));
        if (desc != null)
            att.addAttribute("", "desc", "desc", "", desc);

        addStart(elementName, att);
    }

    public void visit(String name, Object value) {
        Class c = value.getClass();
        if (c.isArray()) {
            AnnotationVisitor av = visitArray(name);
            if (value instanceof byte[]) {
                byte[] b = (byte[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Byte(b[i]));

            } else if (value instanceof char[]) {
                char[] b = (char[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Character(b[i]));

            } else if (value instanceof boolean[]) {
                boolean[] b = (boolean[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, Boolean.valueOf(b[i]));

            } else if (value instanceof int[]) {
                int[] b = (int[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Integer(b[i]));

            } else if (value instanceof long[]) {
                long[] b = (long[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Long(b[i]));

            } else if (value instanceof float[]) {
                float[] b = (float[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Float(b[i]));

            } else if (value instanceof double[]) {
                double[] b = (double[]) value;
                for (int i = 0; i < b.length; i++)
                    av.visit(null, new Double(b[i]));

            }
            av.visitEnd();
        } else {
            addValueElement("annotationValue",
                    name,
                    Type.getDescriptor(c),
                    value.toString());
        }
    }

    public void visitEnum(String name, String desc, String value) {
        addValueElement("annotationValueEnum", name, desc, value);
    }

    public AnnotationVisitor visitAnnotation(String name, String desc) {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotationValueAnnotation",
                0,
                name,
                desc);
    }

    public AnnotationVisitor visitArray(String name) {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotationValueArray",
                0,
                name,
                null);
    }

    public void visitEnd() {
        addEnd(elementName);
    }

    private void addValueElement(
        String element,
        String name,
        String desc,
        String value)
    {
        AttributesImpl att = new AttributesImpl();
        if (name != null)
            att.addAttribute("", "name", "name", "", name);
        if (desc != null)
            att.addAttribute("", "desc", "desc", "", desc);
        if (value != null)
            att.addAttribute("",
                    "value",
                    "value",
                    "",
                    SAXClassAdapter.encode(value));

        addElement(element, att);
    }

}
