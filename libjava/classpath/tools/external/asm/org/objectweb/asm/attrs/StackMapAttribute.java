/**
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
package org.objectweb.asm.attrs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.objectweb.asm.Attribute;
import org.objectweb.asm.ByteVector;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;

/**
 * StackMapAttribute is used by CDLC preverifier. Definition is given in
 * appendix "CLDC Byte Code Typechecker Specification" from CDLC 1.1
 * specification. <p> <i>Note that this implementation does not calculate
 * StackMapFrame structures from the method bytecode. If method code is changed
 * or generated from scratch, then developer is responsible to prepare a correct
 * StackMapFrame structures.</i> <p> The format of the stack map in the class
 * file is given below. In the following, <ul> <li>if the length of the
 * method's byte code1 is 65535 or less, then <tt>uoffset</tt> represents the
 * type u2; otherwise <tt>uoffset</tt> represents the type u4.</li> <li>If
 * the maximum number of local variables for the method is 65535 or less, then
 * <tt>ulocalvar</tt> represents the type u2; otherwise <tt>ulocalvar</tt>
 * represents the type u4.</li> <li>If the maximum size of the operand stack
 * is 65535 or less, then <tt>ustack</tt> represents the type u2; otherwise
 * ustack represents the type u4.</li> </ul>
 *
 * <pre>
 * stack_map { // attribute StackMap
 *   u2 attribute_name_index;
 *   u4 attribute_length
 *   uoffset number_of_entries;
 *   stack_map_frame entries[number_of_entries];
 * }
 * </pre>
 *
 * Each stack map frame has the following format:
 *
 * <pre>
 * stack_map_frame {
 *   uoffset offset;
 *   ulocalvar number_of_locals;
 *   verification_type_info locals[number_of_locals];
 *   ustack number_of_stack_items;
 *   verification_type_info stack[number_of_stack_items];
 * }
 * </pre>
 *
 * The <tt>verification_type_info</tt> structure consists of a one-byte tag
 * followed by zero or more bytes, giving more information about the tag. Each
 * <tt>verification_type_info</tt> structure specifies the verification type
 * of one or two locations.
 *
 * <pre>
 * union verification_type_info {
 *   Top_variable_info;
 *   Integer_variable_info;
 *   Float_variable_info;
 *   Long_variable_info;
 *   Double_variable_info;
 *   Null_variable_info;
 *   UninitializedThis_variable_info;
 *   Object_variable_info;
 *   Uninitialized_variable_info;
 * }
 *
 * Top_variable_info {
 *   u1 tag = ITEM_Top; // 0
 * }
 *
 * Integer_variable_info {
 *   u1 tag = ITEM_Integer; // 1
 * }
 *
 * Float_variable_info {
 *   u1 tag = ITEM_Float; // 2
 * }
 *
 * Long_variable_info {
 *   u1 tag = ITEM_Long; // 4
 * }
 *
 * Double_variable_info {
 *   u1 tag = ITEM_Double; // 3
 * }
 *
 * Null_variable_info {
 *  u1 tag = ITEM_Null; // 5
 * }
 *
 * UninitializedThis_variable_info {
 *   u1 tag = ITEM_UninitializedThis; // 6
 * }
 *
 * Object_variable_info {
 *   u1 tag = ITEM_Object; // 7
 *   u2 cpool_index;
 * }
 *
 * Uninitialized_variable_info {
 *   u1 tag = ITEM_Uninitialized // 8
 *   uoffset offset;
 * }
 * </pre>
 *
 * @see <a href="http://www.jcp.org/en/jsr/detail?id=139">JSR 139 : Connected
 *      Limited Device Configuration 1.1</a>
 *
 * @author Eugene Kuleshov
 */
public class StackMapAttribute extends Attribute {

    static final int MAX_SIZE = 65535;

    /**
     * A List of <code>StackMapFrame</code> instances.
     */
    public List frames = new ArrayList();

    public StackMapAttribute() {
        super("StackMap");
    }

    public StackMapAttribute(List frames) {
        this();
        this.frames = frames;
    }

    public List getFrames() {
        return frames;
    }

    public StackMapFrame getFrame(Label label) {
        for (int i = 0; i < frames.size(); i++) {
            StackMapFrame frame = (StackMapFrame) frames.get(i);
            if (frame.label == label) {
                return frame;
            }
        }
        return null;
    }

    public boolean isUnknown() {
        return false;
    }

    public boolean isCodeAttribute() {
        return true;
    }

    protected Attribute read(
        ClassReader cr,
        int off,
        int len,
        char[] buf,
        int codeOff,
        Label[] labels)
    {
        StackMapAttribute attr = new StackMapAttribute();
        // note that this is not the size of Code attribute
        boolean isExtCodeSize = cr.readInt(codeOff + 4) > MAX_SIZE;
        boolean isExtLocals = cr.readUnsignedShort(codeOff + 2) > MAX_SIZE;
        boolean isExtStack = cr.readUnsignedShort(codeOff) > MAX_SIZE;

        int size = 0;
        if (isExtCodeSize) {
            size = cr.readInt(off);
            off += 4;
        } else {
            size = cr.readUnsignedShort(off);
            off += 2;
        }
        for (int i = 0; i < size; i++) {
            int offset;
            if (isExtCodeSize) {
                offset = cr.readInt(off);
                off += 4;
            } else {
                offset = cr.readUnsignedShort(off);
                off += 2;
            }

            Label label = getLabel(offset, labels);
            List locals = new ArrayList();
            List stack = new ArrayList();

            off = readTypeInfo(cr,
                    off,
                    locals,
                    labels,
                    buf,
                    isExtLocals,
                    isExtCodeSize);
            off = readTypeInfo(cr,
                    off,
                    stack,
                    labels,
                    buf,
                    isExtStack,
                    isExtCodeSize);

            attr.frames.add(new StackMapFrame(label, locals, stack));
        }
        return attr;
    }

    private int readTypeInfo(
        ClassReader cr,
        int off,
        List info,
        Label[] labels,
        char[] buf,
        boolean isExt,
        boolean isExtCode)
    {
        int n = 0;
        if (isExt) {
            n = cr.readInt(off);
            off += 4;
        } else {
            n = cr.readUnsignedShort(off);
            off += 2;
        }
        for (int j = 0; j < n; j++) {
            int itemType = cr.readByte(off++);
            StackMapType typeInfo = StackMapType.getTypeInfo(itemType);
            info.add(typeInfo);
            switch (itemType) {
                case StackMapType.ITEM_Object: //
                    typeInfo.setObject(cr.readClass(off, buf));
                    off += 2;
                    break;
                case StackMapType.ITEM_Uninitialized: //
                    int offset;
                    if (isExtCode) {
                        offset = cr.readInt(off);
                        off += 4;
                    } else {
                        offset = cr.readUnsignedShort(off);
                        off += 2;
                    }
                    typeInfo.setLabel(getLabel(offset, labels));
                    break;
            }
        }
        return off;
    }

    private void writeTypeInfo(ByteVector bv, ClassWriter cw, List info, int max)
    {
        if (max > StackMapAttribute.MAX_SIZE) {
            bv.putInt(info.size());
        } else {
            bv.putShort(info.size());
        }
        for (int j = 0; j < info.size(); j++) {
            StackMapType typeInfo = (StackMapType) info.get(j);
            bv.putByte(typeInfo.getType());
            switch (typeInfo.getType()) {
                case StackMapType.ITEM_Object: //
                    bv.putShort(cw.newClass(typeInfo.getObject()));
                    break;

                case StackMapType.ITEM_Uninitialized: //
                    bv.putShort(typeInfo.getLabel().getOffset());
                    break;

            }
        }
    }

    private Label getLabel(int offset, Label[] labels) {
        Label l = labels[offset];
        if (l != null) {
            return l;
        }
        return labels[offset] = new Label();
    }

    protected ByteVector write(
        ClassWriter cw,
        byte[] code,
        int len,
        int maxStack,
        int maxLocals)
    {
        ByteVector bv = new ByteVector();
        if (code != null && code.length > MAX_SIZE) { // TODO verify value
            bv.putInt(frames.size());
        } else {
            bv.putShort(frames.size());
        }
        for (int i = 0; i < frames.size(); i++) {
            writeFrame((StackMapFrame) frames.get(i),
                    cw,
                    maxStack,
                    maxLocals,
                    bv);
        }
        return bv;
    }

    protected Label[] getLabels() {
        HashSet labels = new HashSet();
        for (int i = 0; i < frames.size(); i++) {
            getFrameLabels((StackMapFrame) frames.get(i), labels);
        }
        return (Label[]) labels.toArray(new Label[labels.size()]);
    }

    private void writeFrame(
        StackMapFrame frame,
        ClassWriter cw,
        int maxStack,
        int maxLocals,
        ByteVector bv)
    {
        bv.putShort(frame.label.getOffset());
        writeTypeInfo(bv, cw, frame.locals, maxLocals);
        writeTypeInfo(bv, cw, frame.stack, maxStack);
    }

    private void getFrameLabels(StackMapFrame frame, Set labels) {
        labels.add(frame.label);
        getTypeInfoLabels(labels, frame.locals);
        getTypeInfoLabels(labels, frame.stack);
    }

    private void getTypeInfoLabels(Set labels, List info) {
        for (Iterator it = info.iterator(); it.hasNext();) {
            StackMapType typeInfo = (StackMapType) it.next();
            if (typeInfo.getType() == StackMapType.ITEM_Uninitialized) {
                labels.add(typeInfo.getLabel());
            }
        }
    }

    public String toString() {
        StringBuffer sb = new StringBuffer("StackMap[");
        for (int i = 0; i < frames.size(); i++) {
            sb.append('\n').append('[').append(frames.get(i)).append(']');
        }
        sb.append("\n]");
        return sb.toString();
    }
}
