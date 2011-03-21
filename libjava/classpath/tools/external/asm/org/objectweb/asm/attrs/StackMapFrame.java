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

import java.util.List;

import org.objectweb.asm.Label;

/**
 * Holds the state of the stack and local variables for a single execution
 * branch.
 *
 * <i>Note that Long and Double types are represented by two entries in locals
 * and stack. Second entry should be always of type Top.</i>
 *
 * @see <a href="http://www.jcp.org/en/jsr/detail?id=139">JSR 139 : Connected
 *      Limited Device Configuration 1.1</a>
 *
 * @see "ClassFileFormat-Java6.fm Page 138 Friday, April 15, 2005 3:22 PM"
 *
 * @author Eugene Kuleshov
 */
public class StackMapFrame {

    /**
     * A <code>Label</code> for frame offset within method bytecode.
     */
    public Label label;

    /**
     * A List of <code>StackMapType</code> instances that represent locals for
     * this frame.
     */
    public List locals;

    /**
     * A List of <code>StackMapType</code> instances that represent stack for
     * this frame.
     */
    public List stack;

    public StackMapFrame(Label label, List locals, List stack) {
        this.label = label;
        this.locals = locals;
        this.stack = stack;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer("Frame:L");
        sb.append(System.identityHashCode(label));
        sb.append(" locals").append(locals);
        sb.append(" stack").append(stack);
        return sb.toString();
    }
}
