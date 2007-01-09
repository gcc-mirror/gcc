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
package org.objectweb.asm.tree;

import org.objectweb.asm.MethodVisitor;

/**
 * A node that represents a bytecode instruction.
 * 
 * @author Eric Bruneton
 */
public abstract class AbstractInsnNode {

    /**
     * The type of {@link InsnNode} instructions.
     */
    public final static int INSN = 0;

    /**
     * The type of {@link IntInsnNode} instructions.
     */
    public final static int INT_INSN = 1;

    /**
     * The type of {@link VarInsnNode} instructions.
     */
    public final static int VAR_INSN = 2;

    /**
     * The type of {@link TypeInsnNode} instructions.
     */
    public final static int TYPE_INSN = 3;

    /**
     * The type of {@link FieldInsnNode} instructions.
     */
    public final static int FIELD_INSN = 4;

    /**
     * The type of {@link MethodInsnNode} instructions.
     */
    public final static int METHOD_INSN = 5;

    /**
     * The type of {@link JumpInsnNode} instructions.
     */
    public final static int JUMP_INSN = 6;

    /**
     * The type of {@link LabelNode} "instructions".
     */
    public final static int LABEL = 7;

    /**
     * The type of {@link LdcInsnNode} instructions.
     */
    public final static int LDC_INSN = 8;

    /**
     * The type of {@link IincInsnNode} instructions.
     */
    public final static int IINC_INSN = 9;

    /**
     * The type of {@link TableSwitchInsnNode} instructions.
     */
    public final static int TABLESWITCH_INSN = 10;

    /**
     * The type of {@link LookupSwitchInsnNode} instructions.
     */
    public final static int LOOKUPSWITCH_INSN = 11;

    /**
     * The type of {@link MultiANewArrayInsnNode} instructions.
     */
    public final static int MULTIANEWARRAY_INSN = 12;

    /**
     * The opcode of this instruction.
     */
    protected int opcode;

    /**
     * Constructs a new {@link AbstractInsnNode}.
     * 
     * @param opcode the opcode of the instruction to be constructed.
     */
    protected AbstractInsnNode(final int opcode) {
        this.opcode = opcode;
    }

    /**
     * Returns the opcode of this instruction.
     * 
     * @return the opcode of this instruction.
     */
    public int getOpcode() {
        return opcode;
    }

    /**
     * Returns the type of this instruction.
     * 
     * @return the type of this instruction, i.e. one the constants defined in
     *         this class.
     */
    public abstract int getType();

    /**
     * Makes the given code visitor visit this instruction.
     * 
     * @param cv a code visitor.
     */
    public abstract void accept(final MethodVisitor cv);
}
