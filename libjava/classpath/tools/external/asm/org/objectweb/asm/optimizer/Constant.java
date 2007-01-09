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

import org.objectweb.asm.ClassWriter;

/**
 * A constant pool item.
 * 
 * @author Eric Bruneton
 */
class Constant {

    /**
     * Type of this constant pool item. A single class is used to represent all
     * constant pool item types, in order to minimize the bytecode size of this
     * package. The value of this field is I, J, F, D, S, s, C, T, G, M, or N
     * (for Constant Integer, Long, Float, Double, STR, UTF8, Class, NameType,
     * Fieldref, Methodref, or InterfaceMethodref constant pool items
     * respectively).
     */
    char type;

    /**
     * Value of this item, for an integer item.
     */
    int intVal;

    /**
     * Value of this item, for a long item.
     */
    long longVal;

    /**
     * Value of this item, for a float item.
     */
    float floatVal;

    /**
     * Value of this item, for a double item.
     */
    double doubleVal;

    /**
     * First part of the value of this item, for items that do not hold a
     * primitive value.
     */
    String strVal1;

    /**
     * Second part of the value of this item, for items that do not hold a
     * primitive value.
     */
    String strVal2;

    /**
     * Third part of the value of this item, for items that do not hold a
     * primitive value.
     */
    String strVal3;

    /**
     * The hash code value of this constant pool item.
     */
    int hashCode;

    public Constant() {
    }

    public Constant(final Constant i) {
        type = i.type;
        intVal = i.intVal;
        longVal = i.longVal;
        floatVal = i.floatVal;
        doubleVal = i.doubleVal;
        strVal1 = i.strVal1;
        strVal2 = i.strVal2;
        strVal3 = i.strVal3;
        hashCode = i.hashCode;
    }

    /**
     * Sets this item to an integer item.
     * 
     * @param intVal the value of this item.
     */
    void set(final int intVal) {
        this.type = 'I';
        this.intVal = intVal;
        this.hashCode = 0x7FFFFFFF & (type + intVal);
    }

    /**
     * Sets this item to a long item.
     * 
     * @param longVal the value of this item.
     */
    void set(final long longVal) {
        this.type = 'J';
        this.longVal = longVal;
        this.hashCode = 0x7FFFFFFF & (type + (int) longVal);
    }

    /**
     * Sets this item to a float item.
     * 
     * @param floatVal the value of this item.
     */
    void set(final float floatVal) {
        this.type = 'F';
        this.floatVal = floatVal;
        this.hashCode = 0x7FFFFFFF & (type + (int) floatVal);
    }

    /**
     * Sets this item to a double item.
     * 
     * @param doubleVal the value of this item.
     */
    void set(final double doubleVal) {
        this.type = 'D';
        this.doubleVal = doubleVal;
        this.hashCode = 0x7FFFFFFF & (type + (int) doubleVal);
    }

    /**
     * Sets this item to an item that do not hold a primitive value.
     * 
     * @param type the type of this item.
     * @param strVal1 first part of the value of this item.
     * @param strVal2 second part of the value of this item.
     * @param strVal3 third part of the value of this item.
     */
    void set(
        final char type,
        final String strVal1,
        final String strVal2,
        final String strVal3)
    {
        this.type = type;
        this.strVal1 = strVal1;
        this.strVal2 = strVal2;
        this.strVal3 = strVal3;
        switch (type) {
            case 's':
            case 'S':
            case 'C':
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode());
                return;
            case 'T':
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode()
                        * strVal2.hashCode());
                return;
            // case 'G':
            // case 'M':
            // case 'N':
            default:
                hashCode = 0x7FFFFFFF & (type + strVal1.hashCode()
                        * strVal2.hashCode() * strVal3.hashCode());
        }
    }

    void write(final ClassWriter cw) {
        switch (type) {
            case 'I':
                cw.newConst(new Integer(intVal));
                break;
            case 'J':
                cw.newConst(new Long(longVal));
                break;
            case 'F':
                cw.newConst(new Float(floatVal));
                break;
            case 'D':
                cw.newConst(new Double(doubleVal));
                break;
            case 'S':
                cw.newConst(strVal1);
                break;
            case 's':
                cw.newUTF8(strVal1);
                break;
            case 'C':
                cw.newClass(strVal1);
                break;
            case 'T':
                cw.newNameType(strVal1, strVal2);
                break;
            case 'G':
                cw.newField(strVal1, strVal2, strVal3);
                break;
            case 'M':
                cw.newMethod(strVal1, strVal2, strVal3, false);
                break;
            case 'N':
                cw.newMethod(strVal1, strVal2, strVal3, true);
                break;
        }
    }

    public boolean equals(final Object o) {
        if (!(o instanceof Constant)) {
            return false;
        }
        Constant c = (Constant) o;
        if (c.type == type) {
            switch (type) {
                case 'I':
                    return c.intVal == intVal;
                case 'J':
                    return c.longVal == longVal;
                case 'F':
                    return c.floatVal == floatVal;
                case 'D':
                    return c.doubleVal == doubleVal;
                case 's':
                case 'S':
                case 'C':
                    return c.strVal1.equals(strVal1);
                case 'T':
                    return c.strVal1.equals(strVal1)
                            && c.strVal2.equals(strVal2);
                // case 'G':
                // case 'M':
                // case 'N':
                default:
                    return c.strVal1.equals(strVal1)
                            && c.strVal2.equals(strVal2)
                            && c.strVal3.equals(strVal3);
            }
        }
        return false;
    }

    public int hashCode() {
        return hashCode;
    }
}
