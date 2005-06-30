/* Stack.java - Class that provides a Last In First Out (LIFO)
   datatype, known more commonly as a Stack
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.util;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct

/**
 * Stack provides a Last In First Out (LIFO) data type, commonly known
 * as a Stack.  Stack itself extends Vector and provides the additional
 * methods for stack manipulation (push, pop, peek). You can also seek for
 * the 1-based position of an element on the stack.
 *
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see List
 * @see AbstractList
 * @see LinkedList
 * @since 1.0
 * @status updated to 1.4
 */
public class Stack extends Vector
{
  // We could use Vector methods internally for the following methods,
  // but have used Vector fields directly for efficiency (i.e. this
  // often reduces out duplicate bounds checking).

  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 1224463164541339165L;

  /**
   * This constructor creates a new Stack, initially empty
   */
  public Stack()
  {
  }

  /**
   * Pushes an Object onto the top of the stack.  This method is effectively
   * the same as addElement(item).
   *
   * @param item the Object to push onto the stack
   * @return the Object pushed onto the stack
   * @see Vector#addElement(Object)
   */
  public Object push(Object item)
  {
    // When growing the Stack, use the Vector routines in case more
    // memory is needed.
    // Note: spec indicates that this method *always* returns obj passed in!

    addElement(item);
    return item;
  }

  /**
   * Pops an item from the stack and returns it.  The item popped is
   * removed from the Stack.
   *
   * @return the Object popped from the stack
   * @throws EmptyStackException if the stack is empty
   */
  public synchronized Object pop()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    modCount++;
    Object obj = elementData[--elementCount];

    // Set topmost element to null to assist the gc in cleanup.
    elementData[elementCount] = null;
    return obj;
  }

  /**
   * Returns the top Object on the stack without removing it.
   *
   * @return the top Object on the stack
   * @throws EmptyStackException if the stack is empty
   */
  public synchronized Object peek()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    return elementData[elementCount - 1];
  }

  /**
   * Tests if the stack is empty.
   *
   * @return true if the stack contains no items, false otherwise
   */
  public synchronized boolean empty()
  {
    return elementCount == 0;
  }

  /**
   * Returns the position of an Object on the stack, with the top
   * most Object being at position 1, and each Object deeper in the
   * stack at depth + 1.
   *
   * @param o The object to search for
   * @return The 1 based depth of the Object, or -1 if the Object
   *         is not on the stack
   */
  public synchronized int search(Object o)
  {
    int i = elementCount;
    while (--i >= 0)
      if (equals(o, elementData[i]))
        return elementCount - i;
    return -1;
  }
}
