/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 16, 2000.
 */
/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

// JDK1.2
public interface List extends Collection
{
  public int size();
  public boolean isEmpty();
  public boolean contains(Object o);
  public Iterator iterator();
  public Object[] toArray();
  public Object[] toArray(Object[] a);
  public boolean add(Object o);
  public boolean remove(Object o);
  public boolean containsAll(Collection c);
  public boolean addAll(Collection c);
  public boolean addAll(int index, Collection c);
  public boolean removeAll(Collection c);
  public boolean retainAll(Collection c);
  public void clear();
  public boolean equals(Object o);
  public int hashCode();
  public Object get(int index);
  public Object set(int index, Object element);
  public void add(int index, Object element);
  public Object remove(int index);
  public int indexOf(Object o);
  public int lastIndexOf(Object o);
  public ListIterator listIterator();
  public ListIterator listIterator(int index);
  public List subList(int fromIndex, int toIndex);
}
