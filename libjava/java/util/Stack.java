/* Stack.java - Class that provides a Last In First Out (LIFO)
   datatype, known more commonly as a Stack
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

/**
 * Stack provides a Last In First Out (LIFO) data type, commonly known
 * as a Stack.  
 *
 * Stack itself extends Vector and provides the additional methods
 * for stack manipulation (push, pop, peek). 
 *
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 20, 1998.
 */
public class Stack extends Vector
{
  // Could use Vector methods internally for the following methods
  // but have used Vector fields directly for efficiency (i.e. this
  // often reduces out duplicate bounds checking).

  private static final long serialVersionUID = 1224463164541339165L;

  /**
   * This constructor creates a new Stack, initially empty
   */
  public Stack()
  {
    super();
  }

  /**
   * Pushes an Object onto the top of the stack.  This method is effectively
   * the same as addElement(item)
   *
   * @param item the Object to push onto the stack
   * @returns the Object pushed onto the stack
   * @see java.util.Vector#addElement(java.util.Object)
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
   * removed from the Stack
   *
   * @returns the Object popped from the stack
   */
  public synchronized Object pop()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    Object obj = elementData[--elementCount];

    // Set topmost element to null to assist the gc in cleanup
    elementData[elementCount] = null;
    return obj;
  }

  /**
   * Returns the top Object on the stack without removing it
   *
   * @returns the top Object on the stack
   */
  public synchronized Object peek()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    return elementData[elementCount - 1];
  }

  /**
   * Tests if the stack is empty
   *
   * @returns true if the stack contains no items, false otherwise
   */
  public boolean empty()
  {
    return elementCount == 0;
  }

  /**
   * Returns the position of an Object on the stack, with the top
   * most Object being at position 1, and each Object deeper in the
   * stack at depth + 1
   *
   * @param o The object to search for
   * @returns The 1 based depth of the Object, or -1 if the Object 
   * is not on the stack.
   */
  public synchronized int search(Object o)
  {
    for (int i = elementCount-1; i >=0; --i)
      if (elementData[i].equals(o))
        return elementCount - i;

    return -1;
  }
}
