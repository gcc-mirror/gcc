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

import org.objectweb.asm.Label;

/**
 * Verification type info used by {@link StackMapAttribute}.
 *
 * @see <a href="http://www.jcp.org/en/jsr/detail?id=139">JSR 139 : Connected
 *      Limited Device Configuration 1.1</a>
 *
 * @see "ClassFileFormat-Java6.fm Page 138 Friday, April 15, 2005 3:22 PM"
 *
 * @author Eugene Kuleshov
 */

public class StackMapType {

    public static final int ITEM_Top = 0;
    public static final int ITEM_Integer = 1;
    public static final int ITEM_Float = 2;
    public static final int ITEM_Double = 3;
    public static final int ITEM_Long = 4;
    public static final int ITEM_Null = 5;
    public static final int ITEM_UninitializedThis = 6;
    public static final int ITEM_Object = 7;
    public static final int ITEM_Uninitialized = 8;

    public static final String[] ITEM_NAMES = {
        "Top",
        "Integer",
        "Float",
        "Double",
        "Long",
        "Null",
        "UninitializedThis",
        "Object",
        "Uninitialized" };

    private int type;
    private Label offset;
    private String object;

    private StackMapType(int type) {
        this.type = type;
    }

    public int getType() {
        return type;
    }

    public static StackMapType getTypeInfo(int itemType) {
        if (itemType < ITEM_Top || itemType > ITEM_Uninitialized) {
            throw new IllegalArgumentException("" + itemType);
        }
        return new StackMapType(itemType);
    }

    public void setLabel(Label offset) {
        this.offset = offset;
    }

    public void setObject(String object) {
        this.object = object;
    }

    public Label getLabel() {
        return offset;
    }

    public String getObject() {
        return object;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer(ITEM_NAMES[type]);
        if (type == ITEM_Object) {
            sb.append(":").append(object);
        }
        if (type == ITEM_Uninitialized) {
            sb.append(":L").append(System.identityHashCode(offset));
        }
        return sb.toString();
    }
}
