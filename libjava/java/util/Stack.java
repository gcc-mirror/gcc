/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 20, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

public class Stack extends Vector
{
  // Could use Vector methods internally for the following methods
  // but have used Vector fields directly for efficiency (i.e. this
  // often reduces out duplicate bounds checking).

  public boolean empty()
  {
    return elementCount == 0;
  }

  public synchronized Object peek()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    return elementData[elementCount - 1];
  }

  public synchronized Object pop()
  {
    if (elementCount == 0)
      throw new EmptyStackException();

    Object obj = elementData[--elementCount];

    // Set topmost element to null to assist the gc in cleanup
    elementData[elementCount] = null;
    return obj;
  }

  public Object push(Object obj)
  {
    // When growing the Stack, use the Vector routines in case more
    // memory is needed.
    // Note: spec indicates that this method *always* returns obj passed in!

    addElement(obj);
    return obj;
  }

  public synchronized int search(Object obj)
  {
    // Return the position of obj on the stack as measured from the top;
    // i.e. the top element is 1, the next element down is 2, etc.
    // If obj is not on the stack, return -1

    for (int i = elementCount-1; i >=0; --i)
      if (elementData[i].equals(obj))
	return elementCount - i;

    return -1;
  }
}
