/* java.lang.CharSequence -- Anything that has an indexed sequence of chars
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.lang;

/**
 * General functions on a sequence of chars. This interface is implemented
 * by <code>String</code>, <code>StringBuffer</code> and
 * <code>CharBuffer</code> to give a uniform way to get chars at a certain
 * index, the number of characters in the sequence and a subrange of the
 * chars. Indexes start at 0 and the last index is <code>length()-1</code>.
 * <p>
 * Even when classes implement this interface they are not always
 * exchangeble because they might implement their compare, equals or hash
 * function differently. This means that in general one should not use a
 * <code>CharSequence</code> as keys in collections since two sequences
 * with the same chars at the same indexes with the same length might not
 * have the same hash code, be equal or be comparable since the are
 * represented by different classes.
 *
 * @author Mark Wielaard (mark@klomp.org)
 *
 * @since 1.4
 */

public interface CharSequence {

    /**
     * Returns the character at the given index.
     *
     * @exception IndexOutOfBoundsException when <code>i &lt; 0</code> or
     * <code>i &gt; length()-1</code>.
     */
    char charAt(int i);

    /**
     * Returns the length of the sequence.
     */
    int length();

    /**
     * Returns a new <code>CharSequence</char> of the indicated range.
     *
     * @exception IndexOutOfBoundsException when <code>begin &lt; 0</code>,
     *         <code>end &lt; 0</code>, <code>end &gt; length()</code> or
     *         <code>begin &gt; end</code>
     */
    CharSequence subSequence(int begin, int end);

    /**
     * Returns the complete <code>CharSequence</code> as a <code>String</code>.
     * Classes that implement this interface should return a <code>String</code>
     * which contains only the characters in the sequence in the correct order.
     */
    String toString();
}
