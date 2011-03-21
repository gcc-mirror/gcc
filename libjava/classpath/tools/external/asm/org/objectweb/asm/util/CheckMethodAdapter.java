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

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.Type;

import java.util.HashMap;

/**
 * A {@link MethodAdapter} that checks that its methods are properly used. More
 * precisely this code adapter checks each instruction individually (i.e., each
 * visit method checks some preconditions based <i>only</i> on its arguments -
 * such as the fact that the given opcode is correct for a given visit method),
 * but does <i>not</i> check the <i>sequence</i> of instructions. For example,
 * in a method whose signature is <tt>void m ()</tt>, the invalid instruction
 * IRETURN, or the invalid sequence IADD L2I will <i>not</i> be detected by
 * this code adapter.
 *
 * @author Eric Bruneton
 */
public class CheckMethodAdapter extends MethodAdapter {

    /**
     * <tt>true</tt> if the visitCode method has been called.
     */
    private boolean startCode;

    /**
     * <tt>true</tt> if the visitMaxs method has been called.
     */
    private boolean endCode;

    /**
     * <tt>true</tt> if the visitEnd method has been called.
     */
    private boolean endMethod;

    /**
     * The already visited labels. This map associate Integer values to Label
     * keys.
     */
    private HashMap labels;

    /**
     * Code of the visit method to be used for each opcode.
     */
    private final static int[] TYPE;

    static {
        String s = "BBBBBBBBBBBBBBBBCCIAADDDDDAAAAAAAAAAAAAAAAAAAABBBBBBBBDD"
                + "DDDAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
                + "BBBBBBBBBBBBBBBBBBBJBBBBBBBBBBBBBBBBBBBBHHHHHHHHHHHHHHHHD"
                + "KLBBBBBBFFFFGGGGAECEBBEEBBAMHHAA";
        TYPE = new int[s.length()];
        for (int i = 0; i < TYPE.length; ++i) {
            TYPE[i] = (s.charAt(i) - 'A' - 1);
        }
    }

    // code to generate the above string
    // public static void main (String[] args) {
    // int[] TYPE = new int[] {
    // 0, //NOP
    // 0, //ACONST_NULL
    // 0, //ICONST_M1
    // 0, //ICONST_0
    // 0, //ICONST_1
    // 0, //ICONST_2
    // 0, //ICONST_3
    // 0, //ICONST_4
    // 0, //ICONST_5
    // 0, //LCONST_0
    // 0, //LCONST_1
    // 0, //FCONST_0
    // 0, //FCONST_1
    // 0, //FCONST_2
    // 0, //DCONST_0
    // 0, //DCONST_1
    // 1, //BIPUSH
    // 1, //SIPUSH
    // 7, //LDC
    // -1, //LDC_W
    // -1, //LDC2_W
    // 2, //ILOAD
    // 2, //LLOAD
    // 2, //FLOAD
    // 2, //DLOAD
    // 2, //ALOAD
    // -1, //ILOAD_0
    // -1, //ILOAD_1
    // -1, //ILOAD_2
    // -1, //ILOAD_3
    // -1, //LLOAD_0
    // -1, //LLOAD_1
    // -1, //LLOAD_2
    // -1, //LLOAD_3
    // -1, //FLOAD_0
    // -1, //FLOAD_1
    // -1, //FLOAD_2
    // -1, //FLOAD_3
    // -1, //DLOAD_0
    // -1, //DLOAD_1
    // -1, //DLOAD_2
    // -1, //DLOAD_3
    // -1, //ALOAD_0
    // -1, //ALOAD_1
    // -1, //ALOAD_2
    // -1, //ALOAD_3
    // 0, //IALOAD
    // 0, //LALOAD
    // 0, //FALOAD
    // 0, //DALOAD
    // 0, //AALOAD
    // 0, //BALOAD
    // 0, //CALOAD
    // 0, //SALOAD
    // 2, //ISTORE
    // 2, //LSTORE
    // 2, //FSTORE
    // 2, //DSTORE
    // 2, //ASTORE
    // -1, //ISTORE_0
    // -1, //ISTORE_1
    // -1, //ISTORE_2
    // -1, //ISTORE_3
    // -1, //LSTORE_0
    // -1, //LSTORE_1
    // -1, //LSTORE_2
    // -1, //LSTORE_3
    // -1, //FSTORE_0
    // -1, //FSTORE_1
    // -1, //FSTORE_2
    // -1, //FSTORE_3
    // -1, //DSTORE_0
    // -1, //DSTORE_1
    // -1, //DSTORE_2
    // -1, //DSTORE_3
    // -1, //ASTORE_0
    // -1, //ASTORE_1
    // -1, //ASTORE_2
    // -1, //ASTORE_3
    // 0, //IASTORE
    // 0, //LASTORE
    // 0, //FASTORE
    // 0, //DASTORE
    // 0, //AASTORE
    // 0, //BASTORE
    // 0, //CASTORE
    // 0, //SASTORE
    // 0, //POP
    // 0, //POP2
    // 0, //DUP
    // 0, //DUP_X1
    // 0, //DUP_X2
    // 0, //DUP2
    // 0, //DUP2_X1
    // 0, //DUP2_X2
    // 0, //SWAP
    // 0, //IADD
    // 0, //LADD
    // 0, //FADD
    // 0, //DADD
    // 0, //ISUB
    // 0, //LSUB
    // 0, //FSUB
    // 0, //DSUB
    // 0, //IMUL
    // 0, //LMUL
    // 0, //FMUL
    // 0, //DMUL
    // 0, //IDIV
    // 0, //LDIV
    // 0, //FDIV
    // 0, //DDIV
    // 0, //IREM
    // 0, //LREM
    // 0, //FREM
    // 0, //DREM
    // 0, //INEG
    // 0, //LNEG
    // 0, //FNEG
    // 0, //DNEG
    // 0, //ISHL
    // 0, //LSHL
    // 0, //ISHR
    // 0, //LSHR
    // 0, //IUSHR
    // 0, //LUSHR
    // 0, //IAND
    // 0, //LAND
    // 0, //IOR
    // 0, //LOR
    // 0, //IXOR
    // 0, //LXOR
    // 8, //IINC
    // 0, //I2L
    // 0, //I2F
    // 0, //I2D
    // 0, //L2I
    // 0, //L2F
    // 0, //L2D
    // 0, //F2I
    // 0, //F2L
    // 0, //F2D
    // 0, //D2I
    // 0, //D2L
    // 0, //D2F
    // 0, //I2B
    // 0, //I2C
    // 0, //I2S
    // 0, //LCMP
    // 0, //FCMPL
    // 0, //FCMPG
    // 0, //DCMPL
    // 0, //DCMPG
    // 6, //IFEQ
    // 6, //IFNE
    // 6, //IFLT
    // 6, //IFGE
    // 6, //IFGT
    // 6, //IFLE
    // 6, //IF_ICMPEQ
    // 6, //IF_ICMPNE
    // 6, //IF_ICMPLT
    // 6, //IF_ICMPGE
    // 6, //IF_ICMPGT
    // 6, //IF_ICMPLE
    // 6, //IF_ACMPEQ
    // 6, //IF_ACMPNE
    // 6, //GOTO
    // 6, //JSR
    // 2, //RET
    // 9, //TABLESWITCH
    // 10, //LOOKUPSWITCH
    // 0, //IRETURN
    // 0, //LRETURN
    // 0, //FRETURN
    // 0, //DRETURN
    // 0, //ARETURN
    // 0, //RETURN
    // 4, //GETSTATIC
    // 4, //PUTSTATIC
    // 4, //GETFIELD
    // 4, //PUTFIELD
    // 5, //INVOKEVIRTUAL
    // 5, //INVOKESPECIAL
    // 5, //INVOKESTATIC
    // 5, //INVOKEINTERFACE
    // -1, //UNUSED
    // 3, //NEW
    // 1, //NEWARRAY
    // 3, //ANEWARRAY
    // 0, //ARRAYLENGTH
    // 0, //ATHROW
    // 3, //CHECKCAST
    // 3, //INSTANCEOF
    // 0, //MONITORENTER
    // 0, //MONITOREXIT
    // -1, //WIDE
    // 11, //MULTIANEWARRAY
    // 6, //IFNULL
    // 6, //IFNONNULL
    // -1, //GOTO_W
    // -1 //JSR_W
    // };
    // for (int i = 0; i < TYPE.length; ++i) {
    // System.out.print((char)(TYPE[i] + 1 + 'A'));
    // }
    // System.out.println();
    // }

    /**
     * Constructs a new {@link CheckMethodAdapter} object.
     *
     * @param cv the code visitor to which this adapter must delegate calls.
     */
    public CheckMethodAdapter(final MethodVisitor cv) {
        super(cv);
        this.labels = new HashMap();
    }

    public AnnotationVisitor visitAnnotation(
        final String desc,
        final boolean visible)
    {
        checkEndMethod();
        checkDesc(desc, false);
        return new CheckAnnotationAdapter(mv.visitAnnotation(desc, visible));
    }

    public AnnotationVisitor visitAnnotationDefault() {
        checkEndMethod();
        return new CheckAnnotationAdapter(mv.visitAnnotationDefault(), false);
    }

    public AnnotationVisitor visitParameterAnnotation(
        final int parameter,
        final String desc,
        final boolean visible)
    {
        checkEndMethod();
        checkDesc(desc, false);
        return new CheckAnnotationAdapter(mv.visitParameterAnnotation(parameter,
                desc,
                visible));
    }

    public void visitAttribute(final Attribute attr) {
        checkEndMethod();
        if (attr == null) {
            throw new IllegalArgumentException("Invalid attribute (must not be null)");
        }
        mv.visitAttribute(attr);
    }

    public void visitCode() {
        startCode = true;
        mv.visitCode();
    }

    public void visitInsn(final int opcode) {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 0);
        mv.visitInsn(opcode);
    }

    public void visitIntInsn(final int opcode, final int operand) {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 1);
        switch (opcode) {
            case Opcodes.BIPUSH:
                checkSignedByte(operand, "Invalid operand");
                break;
            case Opcodes.SIPUSH:
                checkSignedShort(operand, "Invalid operand");
                break;
            // case Constants.NEWARRAY:
            default:
                if (operand < Opcodes.T_BOOLEAN || operand > Opcodes.T_LONG) {
                    throw new IllegalArgumentException("Invalid operand (must be an array type code T_...): "
                            + operand);
                }
        }
        mv.visitIntInsn(opcode, operand);
    }

    public void visitVarInsn(final int opcode, final int var) {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 2);
        checkUnsignedShort(var, "Invalid variable index");
        mv.visitVarInsn(opcode, var);
    }

    public void visitTypeInsn(final int opcode, final String desc) {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 3);
        if (desc != null && desc.length() > 0 && desc.charAt(0) == '[') {
            checkDesc(desc, false);
        } else {
            checkInternalName(desc, "type");
        }
        if (opcode == Opcodes.NEW && desc.charAt(0) == '[') {
            throw new IllegalArgumentException("NEW cannot be used to create arrays: "
                    + desc);
        }
        mv.visitTypeInsn(opcode, desc);
    }

    public void visitFieldInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 4);
        checkInternalName(owner, "owner");
        checkIdentifier(name, "name");
        checkDesc(desc, false);
        mv.visitFieldInsn(opcode, owner, name, desc);
    }

    public void visitMethodInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 5);
        checkMethodIdentifier(name, "name");
        if (!name.equals("clone")) {
            // In JDK1.5, clone method can be called on array class descriptors
            checkInternalName(owner, "owner");
        }
        checkMethodDesc(desc);
        mv.visitMethodInsn(opcode, owner, name, desc);
    }

    public void visitJumpInsn(final int opcode, final Label label) {
        checkStartCode();
        checkEndCode();
        checkOpcode(opcode, 6);
        checkLabel(label, false, "label");
        mv.visitJumpInsn(opcode, label);
    }

    public void visitLabel(final Label label) {
        checkStartCode();
        checkEndCode();
        checkLabel(label, false, "label");
        if (labels.get(label) != null) {
            throw new IllegalArgumentException("Already visited label");
        } else {
            labels.put(label, new Integer(labels.size()));
        }
        mv.visitLabel(label);
    }

    public void visitLdcInsn(final Object cst) {
        checkStartCode();
        checkEndCode();
        if (!(cst instanceof Type)) {
            checkConstant(cst);
        }
        mv.visitLdcInsn(cst);
    }

    public void visitIincInsn(final int var, final int increment) {
        checkStartCode();
        checkEndCode();
        checkUnsignedShort(var, "Invalid variable index");
        checkSignedShort(increment, "Invalid increment");
        mv.visitIincInsn(var, increment);
    }

    public void visitTableSwitchInsn(
        final int min,
        final int max,
        final Label dflt,
        final Label labels[])
    {
        checkStartCode();
        checkEndCode();
        if (max < min) {
            throw new IllegalArgumentException("Max = " + max
                    + " must be greater than or equal to min = " + min);
        }
        checkLabel(dflt, false, "default label");
        if (labels == null || labels.length != max - min + 1) {
            throw new IllegalArgumentException("There must be max - min + 1 labels");
        }
        for (int i = 0; i < labels.length; ++i) {
            checkLabel(labels[i], false, "label at index " + i);
        }
        mv.visitTableSwitchInsn(min, max, dflt, labels);
    }

    public void visitLookupSwitchInsn(
        final Label dflt,
        final int keys[],
        final Label labels[])
    {
        checkEndCode();
        checkStartCode();
        checkLabel(dflt, false, "default label");
        if (keys == null || labels == null || keys.length != labels.length) {
            throw new IllegalArgumentException("There must be the same number of keys and labels");
       }
        for (int i = 0; i < labels.length; ++i) {
            checkLabel(labels[i], false, "label at index " + i);
        }
        mv.visitLookupSwitchInsn(dflt, keys, labels);
    }

    public void visitMultiANewArrayInsn(final String desc, final int dims) {
        checkStartCode();
        checkEndCode();
        checkDesc(desc, false);
        if (desc.charAt(0) != '[') {
            throw new IllegalArgumentException("Invalid descriptor (must be an array type descriptor): "
                    + desc);
        }
        if (dims < 1) {
            throw new IllegalArgumentException("Invalid dimensions (must be greater than 0): "
                    + dims);
        }
        if (dims > desc.lastIndexOf('[') + 1) {
            throw new IllegalArgumentException("Invalid dimensions (must not be greater than dims(desc)): "
                    + dims);
        }
        mv.visitMultiANewArrayInsn(desc, dims);
    }

    public void visitTryCatchBlock(
        final Label start,
        final Label end,
        final Label handler,
        final String type)
    {
        checkStartCode();
        checkEndCode();
        if (type != null) {
            checkInternalName(type, "type");
        }
        mv.visitTryCatchBlock(start, end, handler, type);
    }

    public void visitLocalVariable(
        final String name,
        final String desc,
        final String signature,
        final Label start,
        final Label end,
        final int index)
    {
        checkStartCode();
        checkEndCode();
        checkIdentifier(name, "name");
        checkDesc(desc, false);
        checkLabel(start, true, "start label");
        checkLabel(end, true, "end label");
        checkUnsignedShort(index, "Invalid variable index");
        int s = ((Integer) labels.get(start)).intValue();
        int e = ((Integer) labels.get(end)).intValue();
        if (e < s) {
            throw new IllegalArgumentException("Invalid start and end labels (end must be greater than start)");
        }
        mv.visitLocalVariable(name, desc, signature, start, end, index);
    }

    public void visitLineNumber(final int line, final Label start) {
        checkStartCode();
        checkEndCode();
        checkUnsignedShort(line, "Invalid line number");
        checkLabel(start, true, "start label");
        mv.visitLineNumber(line, start);
    }

    public void visitMaxs(final int maxStack, final int maxLocals) {
        checkStartCode();
        checkEndCode();
        endCode = true;
        checkUnsignedShort(maxStack, "Invalid max stack");
        checkUnsignedShort(maxLocals, "Invalid max locals");
        mv.visitMaxs(maxStack, maxLocals);
    }

    public void visitEnd() {
        checkEndMethod();
        endMethod = true;
        mv.visitEnd();
    }

    // -------------------------------------------------------------------------

    /**
     * Checks that the visitCode method has been called.
     */
    void checkStartCode() {
        if (!startCode) {
            throw new IllegalStateException("Cannot visit instructions before visitCode has been called.");
        }
    }

    /**
     * Checks that the visitMaxs method has not been called.
     */
    void checkEndCode() {
        if (endCode) {
            throw new IllegalStateException("Cannot visit instructions after visitMaxs has been called.");
        }
    }

    /**
     * Checks that the visitEnd method has not been called.
     */
    void checkEndMethod() {
        if (endMethod) {
            throw new IllegalStateException("Cannot visit elements after visitEnd has been called.");
        }
    }

    /**
     * Checks that the type of the given opcode is equal to the given type.
     *
     * @param opcode the opcode to be checked.
     * @param type the expected opcode type.
     */
    static void checkOpcode(final int opcode, final int type) {
        if (opcode < 0 || opcode > 199 || TYPE[opcode] != type) {
            throw new IllegalArgumentException("Invalid opcode: " + opcode);
        }
    }

    /**
     * Checks that the given value is a signed byte.
     *
     * @param value the value to be checked.
     * @param msg an message to be used in case of error.
     */
    static void checkSignedByte(final int value, final String msg) {
        if (value < Byte.MIN_VALUE || value > Byte.MAX_VALUE) {
            throw new IllegalArgumentException(msg
                    + " (must be a signed byte): " + value);
        }
    }

    /**
     * Checks that the given value is a signed short.
     *
     * @param value the value to be checked.
     * @param msg an message to be used in case of error.
     */
    static void checkSignedShort(final int value, final String msg) {
        if (value < Short.MIN_VALUE || value > Short.MAX_VALUE) {
            throw new IllegalArgumentException(msg
                    + " (must be a signed short): " + value);
        }
    }

    /**
     * Checks that the given value is an unsigned short.
     *
     * @param value the value to be checked.
     * @param msg an message to be used in case of error.
     */
    static void checkUnsignedShort(final int value, final String msg) {
        if (value < 0 || value > 65535) {
            throw new IllegalArgumentException(msg
                    + " (must be an unsigned short): " + value);
        }
    }

    /**
     * Checks that the given value is an {@link Integer}, a{@link Float}, a
     * {@link Long}, a {@link Double} or a {@link String}.
     *
     * @param cst the value to be checked.
     */
    static void checkConstant(final Object cst) {
        if (!(cst instanceof Integer) && !(cst instanceof Float)
                && !(cst instanceof Long) && !(cst instanceof Double)
                && !(cst instanceof String))
        {
            throw new IllegalArgumentException("Invalid constant: " + cst);
        }
    }

    /**
     * Checks that the given string is a valid Java identifier.
     *
     * @param name the string to be checked.
     * @param msg a message to be used in case of error.
     */
    static void checkIdentifier(final String name, final String msg) {
        checkIdentifier(name, 0, -1, msg);
    }

    /**
     * Checks that the given substring is a valid Java identifier.
     *
     * @param name the string to be checked.
     * @param start index of the first character of the identifier (inclusive).
     * @param end index of the last character of the identifier (exclusive). -1
     *        is equivalent to <tt>name.length()</tt> if name is not
     *        <tt>null</tt>.
     * @param msg a message to be used in case of error.
     */
    static void checkIdentifier(
        final String name,
        final int start,
        final int end,
        final String msg)
    {
        if (name == null || (end == -1 ? name.length() <= start : end <= start))
        {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must not be null or empty)");
        }
        if (!Character.isJavaIdentifierStart(name.charAt(start))) {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must be a valid Java identifier): " + name);
        }
        int max = (end == -1 ? name.length() : end);
        for (int i = start + 1; i < max; ++i) {
            if (!Character.isJavaIdentifierPart(name.charAt(i))) {
                throw new IllegalArgumentException("Invalid " + msg
                        + " (must be a valid Java identifier): " + name);
            }
        }
    }

    /**
     * Checks that the given string is a valid Java identifier or is equal to
     * '&lt;init&gt;' or '&lt;clinit&gt;'.
     *
     * @param name the string to be checked.
     * @param msg a message to be used in case of error.
     */
    static void checkMethodIdentifier(final String name, final String msg) {
        if (name == null || name.length() == 0) {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must not be null or empty)");
        }
        if (name.equals("<init>") || name.equals("<clinit>")) {
            return;
        }
        if (!Character.isJavaIdentifierStart(name.charAt(0))) {
            throw new IllegalArgumentException("Invalid "
                    + msg
                    + " (must be a '<init>', '<clinit>' or a valid Java identifier): "
                    + name);
        }
        for (int i = 1; i < name.length(); ++i) {
            if (!Character.isJavaIdentifierPart(name.charAt(i))) {
                throw new IllegalArgumentException("Invalid "
                        + msg
                        + " (must be '<init>' or '<clinit>' or a valid Java identifier): "
                        + name);
            }
        }
    }

    /**
     * Checks that the given string is a valid internal class name.
     *
     * @param name the string to be checked.
     * @param msg a message to be used in case of error.
     */
    static void checkInternalName(final String name, final String msg) {
        checkInternalName(name, 0, -1, msg);
    }

    /**
     * Checks that the given substring is a valid internal class name.
     *
     * @param name the string to be checked.
     * @param start index of the first character of the identifier (inclusive).
     * @param end index of the last character of the identifier (exclusive). -1
     *        is equivalent to <tt>name.length()</tt> if name is not
     *        <tt>null</tt>.
     * @param msg a message to be used in case of error.
     */
    static void checkInternalName(
        final String name,
        final int start,
        final int end,
        final String msg)
    {
        if (name == null || name.length() == 0) {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must not be null or empty)");
        }
        int max = (end == -1 ? name.length() : end);
        try {
            int begin = start;
            int slash;
            do {
                slash = name.indexOf('/', begin + 1);
                if (slash == -1 || slash > max) {
                    slash = max;
                }
                checkIdentifier(name, begin, slash, null);
                begin = slash + 1;
            } while (slash != max);
        } catch (IllegalArgumentException _) {
            throw new IllegalArgumentException("Invalid "
                    + msg
                    + " (must be a fully qualified class name in internal form): "
                    + name);
        }
    }

    /**
     * Checks that the given string is a valid type descriptor.
     *
     * @param desc the string to be checked.
     * @param canBeVoid <tt>true</tt> if <tt>V</tt> can be considered valid.
     */
    static void checkDesc(final String desc, final boolean canBeVoid) {
        int end = checkDesc(desc, 0, canBeVoid);
        if (end != desc.length()) {
            throw new IllegalArgumentException("Invalid descriptor: " + desc);
        }
    }

    /**
     * Checks that a the given substring is a valid type descriptor.
     *
     * @param desc the string to be checked.
     * @param start index of the first character of the identifier (inclusive).
     * @param canBeVoid <tt>true</tt> if <tt>V</tt> can be considered valid.
     * @return the index of the last character of the type decriptor, plus one.
     */
    static int checkDesc(
        final String desc,
        final int start,
        final boolean canBeVoid)
    {
        if (desc == null || start >= desc.length()) {
            throw new IllegalArgumentException("Invalid type descriptor (must not be null or empty)");
        }
        int index;
        switch (desc.charAt(start)) {
            case 'V':
                if (canBeVoid) {
                    return start + 1;
                } else {
                    throw new IllegalArgumentException("Invalid descriptor: "
                            + desc);
                }
            case 'Z':
            case 'C':
            case 'B':
            case 'S':
            case 'I':
            case 'F':
            case 'J':
            case 'D':
                return start + 1;
            case '[':
                index = start + 1;
                while (index < desc.length() && desc.charAt(index) == '[') {
                    ++index;
                }
                if (index < desc.length()) {
                    return checkDesc(desc, index, false);
                } else {
                    throw new IllegalArgumentException("Invalid descriptor: "
                            + desc);
                }
            case 'L':
                index = desc.indexOf(';', start);
                if (index == -1 || index - start < 2) {
                    throw new IllegalArgumentException("Invalid descriptor: "
                            + desc);
                }
                try {
                    checkInternalName(desc, start + 1, index, null);
                } catch (IllegalArgumentException _) {
                    throw new IllegalArgumentException("Invalid descriptor: "
                            + desc);
                }
                return index + 1;
            default:
                throw new IllegalArgumentException("Invalid descriptor: "
                        + desc);
        }
    }

    /**
     * Checks that the given string is a valid method descriptor.
     *
     * @param desc the string to be checked.
     */
    static void checkMethodDesc(final String desc) {
        if (desc == null || desc.length() == 0) {
            throw new IllegalArgumentException("Invalid method descriptor (must not be null or empty)");
        }
        if (desc.charAt(0) != '(' || desc.length() < 3) {
            throw new IllegalArgumentException("Invalid descriptor: " + desc);
        }
        int start = 1;
        if (desc.charAt(start) != ')') {
            do {
                if (desc.charAt(start) == 'V') {
                    throw new IllegalArgumentException("Invalid descriptor: "
                            + desc);
                }
                start = checkDesc(desc, start, false);
            } while (start < desc.length() && desc.charAt(start) != ')');
        }
        start = checkDesc(desc, start + 1, true);
        if (start != desc.length()) {
            throw new IllegalArgumentException("Invalid descriptor: " + desc);
        }
    }

    /**
     * Checks that the given label is not null. This method can also check that
     * the label has been visited.
     *
     * @param label the label to be checked.
     * @param checkVisited <tt>true</tt> to check that the label has been
     *        visited.
     * @param msg a message to be used in case of error.
     */
    void checkLabel(
        final Label label,
        final boolean checkVisited,
        final String msg)
    {
        if (label == null) {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must not be null)");
        }
        if (checkVisited && labels.get(label) == null) {
            throw new IllegalArgumentException("Invalid " + msg
                    + " (must be visited first)");
        }
    }
}
