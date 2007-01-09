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
import java.util.Collections;
import java.util.List;

import org.objectweb.asm.Attribute;
import org.objectweb.asm.ByteVector;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * The stack map attribute is used during the process of verification by
 * typechecking (§4.11.1). <br> <br> A stack map attribute consists of zero or
 * more stack map frames. Each stack map frame specifies (either explicitly or
 * implicitly) a bytecode offset, the verification types (§4.11.1) for the local
 * variables, and the verification types for the operand stack. <br> <br> The
 * type checker deals with and manipulates the expected types of a method's
 * local variables and operand stack. Throughout this section, a location refers
 * to either a single local variable or to a single operand stack entry. <br>
 * <br> We will use the terms stack frame map and type state interchangeably to
 * describe a mapping from locations in the operand stack and local variables of
 * a method to verification types. We will usually use the term stack frame map
 * when such a mapping is provided in the class file, and the term type state
 * when the mapping is inferred by the type checker. <br> <br> If a method's
 * Code attribute does not have a StackMapTable attribute, it has an implicit
 * stack map attribute. This implicit stack map attribute is equivalent to a
 * StackMapTable attribute with number_of_entries equal to zero. A method's Code
 * attribute may have at most one StackMapTable attribute, otherwise a
 * java.lang.ClassFormatError is thrown. <br> <br> The format of the stack map
 * in the class file is given below. In the following, if the length of the
 * method's byte code is 65535 or less, then uoffset represents the type u2;
 * otherwise uoffset represents the type u4. If the maximum number of local
 * variables for the method is 65535 or less, then <code>ulocalvar</code>
 * represents the type u2; otherwise ulocalvar represents the type u4. If the
 * maximum size of the operand stack is 65535 or less, then <code>ustack</code>
 * represents the type u2; otherwise ustack represents the type u4.
 * 
 * <pre>
 * stack_map { // attribute StackMapTable
 *   u2 attribute_name_index;
 *   u4 attribute_length
 *   uoffset number_of_entries;
 *   stack_map_frame entries[number_of_entries];
 * }
 * </pre>
 * 
 * Each stack_map_frame structure specifies the type state at a particular byte
 * code offset. Each frame type specifies (explicitly or implicitly) a value,
 * offset_delta, that is used to calulate the actual byte code offset at which
 * it applies. The byte code offset at which the frame applies is given by
 * adding <code>1 + offset_delta</code> to the <code>offset</code> of the
 * previous frame, unless the previous frame is the initial frame of the method,
 * in which case the byte code offset is <code>offset_delta</code>. <br> <br>
 * <i>Note that the length of the byte codes is not the same as the length of
 * the Code attribute. The byte codes are embedded in the Code attribute, along
 * with other information.</i> <br> <br> By using an offset delta rather than
 * the actual byte code offset we ensure, by definition, that stack map frames
 * are in the correctly sorted order. Furthermore, by consistently using the
 * formula <code>offset_delta + 1</code> for all explicit frames, we guarantee
 * the absence of duplicates. <br> <br> All frame types, even full_frame, rely
 * on the previous frame for some of their semantics. This raises the question
 * of what is the very first frame? The initial frame is implicit, and computed
 * from the method descriptor. See the Prolog code for methodInitialStacFrame.
 * <br> <br> The stack_map_frame structure consists of a one-byte tag followed
 * by zero or more bytes, giving more information, depending upon the tag. <br>
 * <br> A stack map frame may belong to one of several frame types
 * 
 * <pre>
 * union stack_map_frame {
 *   same_frame;
 *   same_locals_1_stack_item_frame;
 *   chop_frame;
 *   same_frame_extended;
 *   append_frame;
 *   full_frame;
 * }
 * </pre>
 * 
 * The frame type same_frame is represented by tags in the range [0-63]. If the
 * frame type is same_frame, it means the frame has exactly the same locals as
 * the previous stack map frame and that the number of stack items is zero. The
 * offset_delta value for the frame is the value of the tag field, frame_type.
 * The form of such a frame is then:
 * 
 * <pre>
 * same_frame {
 *   u1 frame_type = SAME;  // 0-63
 * }
 * </pre>
 * 
 * The frame type same_locals_1_stack_item_frame is represented by tags in the
 * range [64, 127]. If the frame_type is same_locals_1_stack_item_frame, it
 * means the frame has exactly the same locals as the previous stack map frame
 * and that the number of stack items is 1. The offset_delta value for the frame
 * is the value (frame_type - 64). There is a verification_type_info following
 * the frame_type for the one stack item. The form of such a frame is then:
 * 
 * <pre>
 * same_locals_1_stack_item_frame {
 *   u1 frame_type = SAME_LOCALS_1_STACK_ITEM;  // 64-127
 *    verification_type_info stack[1];
 * }
 * </pre>
 * 
 * Tags in the range [128-247] are reserved for future use. <br> <br> The frame
 * type chop_frame is represented by tags in the range [248-250]. If the
 * frame_type is chop_frame, it means that the current locals are the same as
 * the locals in the previous frame, except that the k last locals are absent.
 * The value of k is given by the formula 251-frame_type. <br> <br> The form of
 * such a frame is then:
 * 
 * <pre>
 * chop_frame {
 *   u1 frame_type=CHOP;  // 248-250
 *   uoffset offset_delta;
 * }
 * </pre>
 * 
 * The frame type same_frame_extended is represented by the tag value 251. If
 * the frame type is same_frame_extended, it means the frame has exactly the
 * same locals as the previous stack map frame and that the number of stack
 * items is zero. The form of such a frame is then:
 * 
 * <pre>
 * same_frame_extended {
 *   u1 frame_type = SAME_FRAME_EXTENDED;  // 251
 *   uoffset offset_delta;
 * }
 * </pre>
 * 
 * The frame type append_frame is represented by tags in the range [252-254]. If
 * the frame_type is append_frame, it means that the current locals are the same
 * as the locals in the previous frame, except that k additional locals are
 * defined. The value of k is given by the formula frame_type-251. <br> <br> The
 * form of such a frame is then:
 * 
 * <pre>
 * append_frame {
 *   u1 frame_type =APPEND;  // 252-254
 *   uoffset offset_delta;
 *   verification_type_info locals[frame_type -251];
 * }
 * </pre>
 * 
 * The 0th entry in locals represents the type of the first additional local
 * variable. If locals[M] represents local variable N, then locals[M+1]
 * represents local variable N+1 if locals[M] is one of Top_variable_info,
 * Integer_variable_info, Float_variable_info, Null_variable_info,
 * UninitializedThis_variable_info, Object_variable_info, or
 * Uninitialized_variable_info, otherwise locals[M+1] represents local variable
 * N+2. It is an error if, for any index i, locals[i] represents a local
 * variable whose index is greater than the maximum number of local variables
 * for the method. <br> <br> The frame type full_frame is represented by the tag
 * value 255. The form of such a frame is then:
 * 
 * <pre>
 * full_frame {
 *   u1 frame_type = FULL_FRAME;  // 255
 *   uoffset offset_delta;
 *   ulocalvar number_of_locals;
 *   verification_type_info locals[number_of_locals];
 *   ustack number_of_stack_items;
 *   verification_type_info stack[number_of_stack_items];
 * }
 * </pre>
 * 
 * The 0th entry in locals represents the type of local variable 0. If locals[M]
 * represents local variable N, then locals[M+1] represents local variable N+1
 * if locals[M] is one of Top_variable_info, Integer_variable_info,
 * Float_variable_info, Null_variable_info, UninitializedThis_variable_info,
 * Object_variable_info, or Uninitialized_variable_info, otherwise locals[M+1]
 * represents local variable N+2. It is an error if, for any index i, locals[i]
 * represents a local variable whose index is greater than the maximum number of
 * local variables for the method. <br> <br> The 0th entry in stack represents
 * the type of the bottom of the stack, and subsequent entries represent types
 * of stack elements closer to the top of the operand stack. We shall refer to
 * the bottom element of the stack as stack element 0, and to subsequent
 * elements as stack element 1, 2 etc. If stack[M] represents stack element N,
 * then stack[M+1] represents stack element N+1 if stack[M] is one of
 * Top_variable_info, Integer_variable_info, Float_variable_info,
 * Null_variable_info, UninitializedThis_variable_info, Object_variable_info, or
 * Uninitialized_variable_info, otherwise stack[M+1] represents stack element
 * N+2. It is an error if, for any index i, stack[i] represents a stack entry
 * whose index is greater than the maximum operand stack size for the method.
 * <br> <br> We say that an instruction in the byte code has a corresponding
 * stack map frame if the offset in the offset field of the stack map frame is
 * the same as the offset of the instruction in the byte codes. <br> <br> The
 * verification_type_info structure consists of a one-byte tag followed by zero
 * or more bytes, giving more information about the tag. Each
 * verification_type_info structure specifies the verification type of one or
 * two locations.
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
 * </pre>
 * 
 * The Top_variable_info type indicates that the local variable has the
 * verification type top (T.)
 * 
 * <pre>
 * Top_variable_info {
 *   u1 tag = ITEM_Top; // 0
 * }
 * </pre>
 * 
 * The Integer_variable_info type indicates that the location contains the
 * verification type int.
 * 
 * <pre>
 * Integer_variable_info {
 *   u1 tag = ITEM_Integer; // 1
 * }
 * </pre>
 * 
 * The Float_variable_info type indicates that the location contains the
 * verification type float.
 * 
 * <pre>
 * Float_variable_info {
 *   u1 tag = ITEM_Float; // 2
 * }
 * </pre>
 * 
 * The Long_variable_info type indicates that the location contains the
 * verification type long. If the location is a local variable, then:
 * 
 * <ul> <li>It must not be the local variable with the highest index.</li>
 * <li>The next higher numbered local variable contains the verification type
 * T.</li> </ul>
 * 
 * If the location is an operand stack entry, then:
 * 
 * <ul> <li>The current location must not be the topmost location of the
 * operand stack.</li> <li>the next location closer to the top of the operand
 * stack contains the verification type T.</li> </ul>
 * 
 * This structure gives the contents of two locations in the operand stack or in
 * the local variables.
 * 
 * <pre>
 * Long_variable_info {
 *   u1 tag = ITEM_Long; // 4
 * }
 * </pre>
 * 
 * The Double_variable_info type indicates that the location contains the
 * verification type double. If the location is a local variable, then:
 * 
 * <ul> <li>It must not be the local variable with the highest index.</li>
 * <li>The next higher numbered local variable contains the verification type
 * T. <li> </ul>
 * 
 * If the location is an operand stack entry, then:
 * 
 * <ul> <li>The current location must not be the topmost location of the
 * operand stack.</li> <li>the next location closer to the top of the operand
 * stack contains the verification type T.</li> </ul>
 * 
 * This structure gives the contents of two locations in in the operand stack or
 * in the local variables.
 * 
 * <pre>
 * Double_variable_info {
 *   u1 tag = ITEM_Double; // 3
 * }
 * </pre>
 * 
 * The Null_variable_info type indicates that location contains the verification
 * type null.
 * 
 * <pre>
 * Null_variable_info {
 *   u1 tag = ITEM_Null; // 5
 * }
 * </pre>
 * 
 * The UninitializedThis_variable_info type indicates that the location contains
 * the verification type uninitializedThis.
 * 
 * <pre>
 * UninitializedThis_variable_info {
 *   u1 tag = ITEM_UninitializedThis; // 6
 * }
 * </pre>
 * 
 * The Object_variable_info type indicates that the location contains an
 * instance of the class referenced by the constant pool entry.
 * 
 * <pre>
 * Object_variable_info {
 *   u1 tag = ITEM_Object; // 7
 *   u2 cpool_index;
 * }
 * </pre>
 * 
 * The Uninitialized_variable_info indicates that the location contains the
 * verification type uninitialized(offset). The offset item indicates the offset
 * of the new instruction that created the object being stored in the location.
 * 
 * <pre>
 * Uninitialized_variable_info {
 *   u1 tag = ITEM_Uninitialized // 8
 *   uoffset offset;
 * }
 * </pre>
 * 
 * @see "ClassFileFormat-Java6.fm Page 138 Friday, April 15, 2005 3:22 PM"
 * 
 * @author Eugene Kuleshov
 */
public class StackMapTableAttribute extends Attribute {
    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is zero.
     */
    public static final int SAME_FRAME = 0; // to 63 (0-3f)

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is 1
     */
    public static final int SAME_LOCALS_1_STACK_ITEM_FRAME = 64; // to 127

    // (40-7f)

    /**
     * Reserved for future use
     */
    public static final int RESERVED = 128;

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is 1. Offset is bigger then 63;
     */
    public static final int SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED = 247; // f7

    /**
     * Frame where current locals are the same as the locals in the previous
     * frame, except that the k last locals are absent. The value of k is given
     * by the formula 251-frame_type.
     */
    public static final int CHOP_FRAME = 248; // to 250 (f8-fA)

    /**
     * Frame has exactly the same locals as the previous stack map frame and
     * number of stack items is zero. Offset is bigger then 63;
     */
    public static final int SAME_FRAME_EXTENDED = 251; // fb

    /**
     * Frame where current locals are the same as the locals in the previous
     * frame, except that k additional locals are defined. The value of k is
     * given by the formula frame_type-251.
     */
    public static final int APPEND_FRAME = 252; // to 254 // fc-fe

    /**
     * Full frame
     */
    public static final int FULL_FRAME = 255; // ff

    private static final int MAX_SHORT = 65535;

    /**
     * A <code>List</code> of <code>StackMapFrame</code> instances.
     */
    private List frames;

    public StackMapTableAttribute() {
        super("StackMapTable");
    }

    public StackMapTableAttribute(List frames) {
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

        ArrayList frames = new ArrayList();

        // note that this is not the size of Code attribute
        boolean isExtCodeSize = cr.readInt(codeOff + 4) > MAX_SHORT;
        boolean isExtLocals = cr.readUnsignedShort(codeOff + 2) > MAX_SHORT;
        boolean isExtStack = cr.readUnsignedShort(codeOff) > MAX_SHORT;

        int offset = 0;

        int methodOff = getMethodOff(cr, codeOff, buf);
        StackMapFrame frame = new StackMapFrame(getLabel(offset, labels),
                calculateLocals(cr.readClass(cr.header + 2, buf), // owner
                        cr.readUnsignedShort(methodOff), // method access
                        cr.readUTF8(methodOff + 2, buf), // method name
                        cr.readUTF8(methodOff + 4, buf)), // method desc
                Collections.EMPTY_LIST);
        frames.add(frame);

        // System.err.println( cr.readUTF8( methodOff + 2, buf));
        // System.err.println( offset +" delta:" + 0 +" : "+ frame);

        int size;
        if (isExtCodeSize) {
            size = cr.readInt(off);
            off += 4;
        } else {
            size = cr.readUnsignedShort(off);
            off += 2;
        }

        for (; size > 0; size--) {
            int tag = cr.readByte(off); // & 0xff;
            off++;

            List stack;
            List locals;

            int offsetDelta;
            if (tag < SAME_LOCALS_1_STACK_ITEM_FRAME) {  // SAME_FRAME
                offsetDelta = tag;

                locals = new ArrayList(frame.locals);
                stack = Collections.EMPTY_LIST;

            } else if (tag < RESERVED) {  // SAME_LOCALS_1_STACK_ITEM_FRAME
                offsetDelta = tag - SAME_LOCALS_1_STACK_ITEM_FRAME;

                locals = new ArrayList(frame.locals);
                stack = new ArrayList();
                // read verification_type_info stack[1];
                off = readType(stack, isExtCodeSize, cr, off, labels, buf);

            } else {
                if (isExtCodeSize) {
                    offsetDelta = cr.readInt(off);
                    off += 4;
                } else {
                    offsetDelta = cr.readUnsignedShort(off);
                    off += 2;
                }

                if (tag == SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED) {  // SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED
                    locals = new ArrayList(frame.locals);
                    stack = new ArrayList();
                    // read verification_type_info stack[1];
                    off = readType(stack, isExtCodeSize, cr, off, labels, buf);

                } else if (tag >= CHOP_FRAME && tag < SAME_FRAME_EXTENDED) {  // CHOP_FRAME
                    stack = Collections.EMPTY_LIST;

                    int k = SAME_FRAME_EXTENDED - tag;
                    // copy locals from prev frame and chop last k
                    locals = new ArrayList(frame.locals.subList(0,
                            frame.locals.size() - k));

                } else if (tag == SAME_FRAME_EXTENDED) {  // SAME_FRAME_EXTENDED
                    stack = Collections.EMPTY_LIST;
                    locals = new ArrayList(frame.locals);

                } else if ( /* tag>=APPEND && */tag < FULL_FRAME) {  // APPEND_FRAME
                    stack = Collections.EMPTY_LIST;

                    // copy locals from prev frame and append new k
                    locals = new ArrayList(frame.locals);
                    for (int k = tag - SAME_FRAME_EXTENDED; k > 0; k--) {
                        off = readType(locals,
                                isExtCodeSize,
                                cr,
                                off,
                                labels,
                                buf);
                    }

                } else if (tag == FULL_FRAME) {  // FULL_FRAME
                    // read verification_type_info locals[number_of_locals];
                    locals = new ArrayList();
                    off = readTypes(locals,
                            isExtLocals,
                            isExtCodeSize,
                            cr,
                            off,
                            labels,
                            buf);

                    // read verification_type_info stack[number_of_stack_items];
                    stack = new ArrayList();
                    off = readTypes(stack,
                            isExtStack,
                            isExtCodeSize,
                            cr,
                            off,
                            labels,
                            buf);

                } else {
                    throw new RuntimeException("Unknown frame type " + tag
                            + " after offset " + offset);

                }
            }

            offset += offsetDelta;

            Label offsetLabel = getLabel(offset, labels);

            frame = new StackMapFrame(offsetLabel, locals, stack);
            frames.add(frame);
            // System.err.println( tag +" " + offset +" delta:" + offsetDelta +
            // " frameType:"+ frameType+" : "+ frame);

            offset++;
        }

        return new StackMapTableAttribute(frames);
    }

    protected ByteVector write(
        ClassWriter cw,
        byte[] code,
        int len,
        int maxStack,
        int maxLocals)
    {
        ByteVector bv = new ByteVector();
        // TODO verify this value (MAX_SHORT)
        boolean isExtCodeSize = code != null && code.length > MAX_SHORT;
        writeSize(frames.size() - 1, bv, isExtCodeSize);

        if (frames.size() < 2) {
            return bv;
        }

        boolean isExtLocals = maxLocals > MAX_SHORT;
        boolean isExtStack = maxStack > MAX_SHORT;

        // skip the first frame
        StackMapFrame frame = (StackMapFrame) frames.get(0);
        List locals = frame.locals;
        int offset = frame.label.getOffset();

        for (int i = 1; i < frames.size(); i++) {
            frame = (StackMapFrame) frames.get(i);

            List clocals = frame.locals;
            List cstack = frame.stack;
            int coffset = frame.label.getOffset();

            int clocalsSize = clocals.size();
            int cstackSize = cstack.size();

            int localsSize = locals.size();

            int delta = coffset - offset;

            int type = FULL_FRAME;
            int k = 0;
            if (cstackSize == 0) {
                k = clocalsSize - localsSize;
                switch (k) {
                    case -3:
                    case -2:
                    case -1:
                        type = CHOP_FRAME; // CHOP or FULL
                        localsSize = clocalsSize; // for full_frame check
                        break;

                    case 0:
                        // SAME, SAME_EXTENDED or FULL
                        type = delta < 64 ? SAME_FRAME : SAME_FRAME_EXTENDED;
                        break;

                    case 1:
                    case 2:
                    case 3:
                        type = APPEND_FRAME; // APPEND or FULL
                        break;
                }
            } else if (localsSize == clocalsSize && cstackSize == 1) {
                // SAME_LOCAL_1_STACK or FULL
                type = delta < 63
                        ? SAME_LOCALS_1_STACK_ITEM_FRAME
                        : SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED;
            }

            if (type != FULL_FRAME) {
                // verify if stack and locals are the same
                for (int j = 0; j < localsSize && type != FULL_FRAME; j++) {
                    if (!locals.get(j).equals(clocals.get(j)))
                        type = FULL_FRAME;
                }
            }

            switch (type) {
                case SAME_FRAME:
                    bv.putByte(delta);
                    break;

                case SAME_LOCALS_1_STACK_ITEM_FRAME:
                    bv.putByte(SAME_LOCALS_1_STACK_ITEM_FRAME + delta);
                    writeTypeInfos(bv, cw, cstack, 0, 1);
                    break;

                case SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED:
                    bv.putByte(SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED);
                    writeSize(delta, bv, isExtCodeSize);
                    writeTypeInfos(bv, cw, cstack, 0, 1);
                    break;

                case SAME_FRAME_EXTENDED:
                    bv.putByte(SAME_FRAME_EXTENDED);
                    writeSize(delta, bv, isExtCodeSize);
                    break;

                case CHOP_FRAME:
                    bv.putByte(SAME_FRAME_EXTENDED + k); // negative k
                    writeSize(delta, bv, isExtCodeSize);
                    break;

                case APPEND_FRAME:
                    bv.putByte(SAME_FRAME_EXTENDED + k); // positive k
                    writeSize(delta, bv, isExtCodeSize);
                    writeTypeInfos(bv,
                            cw,
                            clocals,
                            clocalsSize - 1,
                            clocalsSize);
                    break;

                case FULL_FRAME:
                    bv.putByte(FULL_FRAME);
                    writeSize(delta, bv, isExtCodeSize);
                    writeSize(clocalsSize, bv, isExtLocals);
                    writeTypeInfos(bv, cw, clocals, 0, clocalsSize);
                    writeSize(cstackSize, bv, isExtStack);
                    writeTypeInfos(bv, cw, cstack, 0, cstackSize);
                    break;

                default:
                    throw new RuntimeException();
            }
            offset = coffset + 1; // compensating non first offset
            locals = clocals;
        }
        return bv;
    }

    private void writeSize(int delta, ByteVector bv, boolean isExt) {
        if (isExt) {
            bv.putInt(delta);
        } else {
            bv.putShort(delta);
        }
    }

    private void writeTypeInfos(
        ByteVector bv,
        ClassWriter cw,
        List info,
        int start,
        int end)
    {
        for (int j = start; j < end; j++) {
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

    public static int getMethodOff(ClassReader cr, int codeOff, char[] buf) {
        int off = cr.header + 6;

        int interfacesCount = cr.readUnsignedShort(off);
        off += 2 + interfacesCount * 2;

        int fieldsCount = cr.readUnsignedShort(off);
        off += 2;
        for (; fieldsCount > 0; --fieldsCount) {
            int attrCount = cr.readUnsignedShort(off + 6); // field attributes
            off += 8;
            for (; attrCount > 0; --attrCount) {
                off += 6 + cr.readInt(off + 2);
            }
        }

        int methodsCount = cr.readUnsignedShort(off);
        off += 2;
        for (; methodsCount > 0; --methodsCount) {
            int methodOff = off;
            int attrCount = cr.readUnsignedShort(off + 6); // method attributes
            off += 8;
            for (; attrCount > 0; --attrCount) {
                String attrName = cr.readUTF8(off, buf);
                off += 6;
                if (attrName.equals("Code")) {
                    if (codeOff == off) {
                        return methodOff;
                    }
                }
                off += cr.readInt(off - 4);
            }
        }

        return -1;
    }

    /**
     * Use method signature and access flags to resolve initial locals state.
     * 
     * @param className name of the method's owner class.
     * @param access access flags of the method.
     * @param methodName name of the method.
     * @param methodDesc descriptor of the method.
     * @return list of <code>StackMapType</code> instances representing locals
     *         for an initial frame.
     */
    public static List calculateLocals(
        String className,
        int access,
        String methodName,
        String methodDesc)
    {
        List locals = new ArrayList();

        // TODO
        if ("<init>".equals(methodName)
                && !className.equals("java/lang/Object"))
        {
            StackMapType typeInfo = StackMapType.getTypeInfo(StackMapType.ITEM_UninitializedThis);
            typeInfo.setObject(className); // this
            locals.add(typeInfo);
        } else if ((access & Opcodes.ACC_STATIC) == 0) {
            StackMapType typeInfo = StackMapType.getTypeInfo(StackMapType.ITEM_Object);
            typeInfo.setObject(className); // this
            locals.add(typeInfo);
        }

        Type[] types = Type.getArgumentTypes(methodDesc);
        for (int i = 0; i < types.length; i++) {
            Type t = types[i];
            StackMapType smt;
            switch (t.getSort()) {
                case Type.LONG:
                    smt = StackMapType.getTypeInfo(StackMapType.ITEM_Long);
                    break;
                case Type.DOUBLE:
                    smt = StackMapType.getTypeInfo(StackMapType.ITEM_Double);
                    break;

                case Type.FLOAT:
                    smt = StackMapType.getTypeInfo(StackMapType.ITEM_Float);
                    break;

                case Type.ARRAY:
                case Type.OBJECT:
                    smt = StackMapType.getTypeInfo(StackMapType.ITEM_Object);
                    smt.setObject(t.getDescriptor()); // TODO verify name
                    break;

                default:
                    smt = StackMapType.getTypeInfo(StackMapType.ITEM_Integer);
                    break;
            }
        }

        return locals;
    }

    private int readTypes(
        List info,
        boolean isExt,
        boolean isExtCodeSize,
        ClassReader cr,
        int off,
        Label[] labels,
        char[] buf)
    {
        int n = 0;
        if (isExt) {
            n = cr.readInt(off);
            off += 4;
        } else {
            n = cr.readUnsignedShort(off);
            off += 2;
        }

        for (; n > 0; n--) {
            off = readType(info, isExtCodeSize, cr, off, labels, buf);
        }
        return off;
    }

    private int readType(
        List info,
        boolean isExtCodeSize,
        ClassReader cr,
        int off,
        Label[] labels,
        char[] buf)
    {
        int itemType = cr.readByte(off++);
        StackMapType typeInfo = StackMapType.getTypeInfo(itemType);
        info.add(typeInfo);
        switch (itemType) {
            // case StackMapType.ITEM_Long: //
            // case StackMapType.ITEM_Double: //
            // info.add(StackMapType.getTypeInfo(StackMapType.ITEM_Top));
            // break;

            case StackMapType.ITEM_Object: //
                typeInfo.setObject(cr.readClass(off, buf));
                off += 2;
                break;

            case StackMapType.ITEM_Uninitialized: //
                int offset;
                if (isExtCodeSize) {
                    offset = cr.readInt(off);
                    off += 4;
                } else {
                    offset = cr.readUnsignedShort(off);
                    off += 2;
                }

                typeInfo.setLabel(getLabel(offset, labels));
                break;
        }
        return off;
    }

    private Label getLabel(int offset, Label[] labels) {
        Label l = labels[offset];
        if (l != null) {
            return l;
        }
        return labels[offset] = new Label();
    }

    public String toString() {
        StringBuffer sb = new StringBuffer("StackMapTable[");
        for (int i = 0; i < frames.size(); i++) {
            sb.append('\n').append('[').append(frames.get(i)).append(']');
        }
        sb.append("\n]");
        return sb.toString();
    }
}
