/* AbstractList.java -- Abstract implementation of most of List
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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


// TO DO:
// ~ Doc comments for almost everything.
// ~ Better general commenting

package java.util;

/**
 * A basic implementation of most of the methods in the List interface to make
 * it easier to create a List based on a random-access data structure. To
 * create an unmodifiable list, it is only necessary to override the size() and
 * get(int) methods (this contrasts with all other abstract collection classes
 * which require an iterator to be provided). To make the list modifiable, the
 * set(int, Object)  method should also be overridden, and to make the list
 * resizable, the add(int, Object) and remove(int) methods should be overridden
 * too. Other methods should be overridden if the backing data structure allows
 * for a more efficient implementation. The precise implementation used by
 * AbstractList is documented, so that subclasses can tell which methods could
 * be implemented more efficiently.
 */
public abstract class AbstractList extends AbstractCollection implements List {

  /**
   * A count of the number of structural modifications that have been made to
   * the list (that is, insertions and removals).
   */
  protected transient int modCount = 0;

  public abstract Object get(int index);

  public void add(int index, Object o) {
    throw new UnsupportedOperationException();
  }

  public boolean add(Object o) {
    add(size(), o);
    return true;
  }

  public boolean addAll(int index, Collection c) {
    Iterator i = c.iterator();
    if (i.hasNext()) {
      do {
        add(index++, i.next());
      } while (i.hasNext());
      return true;
    } else {
      return false;
    }
  }

  public void clear() {
    removeRange(0, size());
  }

  public boolean equals(Object o) {
    if (o == this) {
      return true;
    } else if (!(o instanceof List)) {
      return false;
    } else {
      Iterator i1 = iterator();
      Iterator i2 = ((List)o).iterator();
      while (i1.hasNext()) {
        if (!i2.hasNext()) {
          return false;
        } else {
          Object e = i1.next();
          if (e == null ? i2.next() != null : !e.equals(i2.next())) {
            return false;
          }
        }
      }
      if (i2.hasNext()) {
        return false;
      } else {
        return true;
      }
    }
  }

  public int hashCode() {
    int hashCode = 1;
    Iterator i = iterator();
    while (i.hasNext()) {
      Object obj = i.next();
      hashCode = 31 * hashCode + (obj == null ? 0 : obj.hashCode());
    }
    return hashCode;
  }

  public int indexOf(Object o) {
    int index = 0;
    ListIterator i = listIterator();
    if (o == null) {
      while (i.hasNext()) {
        if (i.next() == null) {
          return index;
        }
        index++;
      }
    } else {
      while (i.hasNext()) {
        if (o.equals(i.next())) {
          return index;
        }
        index++;
      }
    }
    return -1;
  }

  public Iterator iterator() {
    return new Iterator() {
      private int knownMod = modCount;
      private int position = 0;
      boolean removed = true;

      private void checkMod() {
        if (knownMod != modCount) {
          throw new ConcurrentModificationException();
        }
      }

      public boolean hasNext() {
        checkMod();
        return position < size();
      }

      public Object next() {
        checkMod();
        removed = false;
        try {
          return get(position++);
        } catch (IndexOutOfBoundsException e) {
          throw new NoSuchElementException();
        }
      }

      public void remove() {
        checkMod();
        if (removed) {
          throw new IllegalStateException();
        }
        AbstractList.this.remove(--position);
        knownMod = modCount;
        removed = true;
      }
    };
  }

  public int lastIndexOf(Object o) {
    int index = size();
    ListIterator i = listIterator(index);
    if (o == null) {
      while (i.hasPrevious()) {
        index--;
        if (i.previous() == null) {
          return index;
        }
      }
    } else {
      while (i.hasPrevious()) {
        index--;
        if (o.equals(i.previous())) {
          return index;
        }
      }
    }
    return -1;
  }

  public ListIterator listIterator() {
    return listIterator(0);
  }

  public ListIterator listIterator(final int index) {

    if (index < 0 || index > size()) {
      throw new IndexOutOfBoundsException();
    }

    return new ListIterator() {
      private int knownMod = modCount;
      private int position = index;
      private int lastReturned = -1;

      private void checkMod() {
        if (knownMod != modCount) {
          throw new ConcurrentModificationException();
        }
      }

      public boolean hasNext() {
        checkMod();
        return position < size();
      }

      public boolean hasPrevious() {
        checkMod();
        return position > 0;
      }

      public Object next() {
        checkMod();
        if (hasNext()) {
          lastReturned = position++;
          return get(lastReturned);
        } else {
          throw new NoSuchElementException();
        }
      }

      public Object previous() {
        checkMod();
        if (hasPrevious()) {
          lastReturned = --position;
          return get(lastReturned);
        } else {
          throw new NoSuchElementException();
        }
      }

      public int nextIndex() {
        checkMod();
        return position;
      }

      public int previousIndex() {
        checkMod();
        return position - 1;
      }

      public void remove() {
        checkMod();
        if (lastReturned < 0) {
          throw new IllegalStateException();
        }
        AbstractList.this.remove(lastReturned);
        knownMod = modCount;
        position = lastReturned;
        lastReturned = -1;
      }

      public void set(Object o) {
        checkMod();
        if (lastReturned < 0) {
          throw new IllegalStateException();
        }
        AbstractList.this.set(lastReturned, o);
      }

      public void add(Object o) {
        checkMod();
        AbstractList.this.add(position++, o);
        lastReturned = -1;
        knownMod = modCount;
      }
    };
  }

  public Object remove(int index) {
    throw new UnsupportedOperationException();
  }

  /**
   * Remove a subsection of the list. This is called by the clear and
   * removeRange methods of the class which implements subList, which are
   * difficult for subclasses to override directly. Therefore, this method
   * should be overridden instead by the more efficient implementation, if one
   * exists.
   * <p>
   * This implementation first checks for illegal or out of range arguments. It
   * then obtains a ListIterator over the list using listIterator(fromIndex).
   * It then calls next() and remove() on this iterator repeatedly, toIndex -
   * fromIndex times.
   *
   * @param fromIndex the index, inclusive, to remove from.
   * @param toIndex the index, exclusive, to remove to.
   * @exception UnsupportedOperationException if this list does not support
   *   the removeRange operation.
   * @exception IndexOutOfBoundsException if fromIndex > toIndex || fromIndex <
   *   0 || toIndex > size().
   */
  protected void removeRange(int fromIndex, int toIndex) {
    if (fromIndex > toIndex) {
      throw new IllegalArgumentException();
    } else if (fromIndex < 0 || toIndex > size()) {
      throw new IndexOutOfBoundsException();
    } else {
      ListIterator i = listIterator(fromIndex);
      for (int index = fromIndex; index < toIndex; index++) {
        i.next();
        i.remove();
      }
    }
  }

  public Object set(int index, Object o) {
    throw new UnsupportedOperationException();
  }

  public List subList(final int fromIndex, final int toIndex) {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0 || toIndex > size())
      throw new IndexOutOfBoundsException();
    return new SubList(this, fromIndex, toIndex);
  }

  static class SubList extends AbstractList {

    private AbstractList backingList;
    private int offset;
    private int size;

    public SubList(AbstractList backing, int fromIndex, int toIndex) {
      backingList = backing;
      upMod();
      offset = fromIndex;
      size = toIndex - fromIndex;
    }

    // Note that within this class two fields called modCount are inherited -
    // one from the superclass, and one from the outer class. 
    // The code uses both these two fields and *no other* to provide fail-fast
    // behaviour. For correct operation, the two fields should contain equal
    // values. Therefore, if this.modCount != backingList.modCount, there
    // has been a concurrent modification. This is all achieved purely by using
    // the modCount field, precisely according to the docs of AbstractList.
    // See the methods upMod and checkMod.

    /**
     * This method checks the two modCount fields to ensure that there has
     * not been a concurrent modification. It throws an exception if there
     * has been, and otherwise returns normally.
     * Note that since this method is private, it will be inlined.
     *
     * @exception ConcurrentModificationException if there has been a
     *   concurrent modification.
     */
    private void checkMod() {
      if (this.modCount != backingList.modCount) {
	throw new ConcurrentModificationException();
      }
    }
    
    /**
     * This method is called after every method that causes a structural
     * modification to the backing list. It updates the local modCount field
     * to match that of the backing list.
     * Note that since this method is private, it will be inlined.
     */
    private void upMod() {
      this.modCount = backingList.modCount;
    }
    
    /**
     * This method checks that a value is between 0 and size (inclusive). If
     * it is not, an exception is thrown.
     * Note that since this method is private, it will be inlined.
     *
     * @exception IndexOutOfBoundsException if the value is out of range.
     */
    private void checkBoundsInclusive(int index) {
      if (index < 0 || index > size) {
	throw new IndexOutOfBoundsException();
      }
    }
    
    /**
     * This method checks that a value is between 0 (inclusive) and size
     * (exclusive). If it is not, an exception is thrown.
     * Note that since this method is private, it will be inlined.
     *
     * @exception IndexOutOfBoundsException if the value is out of range.
     */
    private void checkBoundsExclusive(int index) {
      if (index < 0 || index >= size) {
	throw new IndexOutOfBoundsException();
      }
    }
    
    public int size() {
      checkMod();
      return size;
    }
    
    public Iterator iterator() {
      return listIterator();
    }
    
    public ListIterator listIterator(final int index) {
      
      checkMod();
      checkBoundsInclusive(index);
      
      return new ListIterator() {
	ListIterator i = backingList.listIterator(index + offset);
	int position = index;
	
	public boolean hasNext() {
	  checkMod();
	  return position < size;
	}
	
	public boolean hasPrevious() {
	  checkMod();
	  return position > 0;
	}
	
	public Object next() {
	  if (position < size) {
	    Object o = i.next();
	    position++;
	    return o;
	  } else {
	    throw new NoSuchElementException();
	  }
	}
	
	public Object previous() {
	  if (position > 0) {
	    Object o = i.previous();
	    position--;
	    return o;
	  } else {
	    throw new NoSuchElementException();
	  }
	}
	
	public int nextIndex() {
	  return offset + i.nextIndex();
	}
	
	public int previousIndex() {
	  return offset + i.previousIndex();
	}

	public void remove() {
	  i.remove();
	  upMod();
	  size--;
	  position = nextIndex();
	}
	
	public void set(Object o) {
	  i.set(o);
	}
	
	public void add(Object o) {
	  i.add(o);
	  upMod();
	  size++;
	  position++;
	}

	// Here is the reason why the various modCount fields are mostly
	// ignored in this wrapper listIterator.
	// IF the backing listIterator is failfast, then the following holds:
	//   Using any other method on this list will call a corresponding
	//   method on the backing list *after* the backing listIterator
	//   is created, which will in turn cause a ConcurrentModException
	//   when this listIterator comes to use the backing one. So it is
	//   implicitly failfast.
	// If the backing listIterator is NOT failfast, then the whole of
	//   this list isn't failfast, because the modCount field of the
	//   backing list is not valid. It would still be *possible* to
	//   make the iterator failfast wrt modifications of the sublist
	//   only, but somewhat pointless when the list can be changed under
	//   us.
	// Either way, no explicit handling of modCount is needed.
	// However upMod() must be called in add and remove, and size
	// must also be updated in these two methods, since they do not go
	// through the corresponding methods of the subList.

      };
    }

    public Object set(int index, Object o) {
      checkMod();
      checkBoundsExclusive(index);
      o = backingList.set(index + offset, o);
      upMod();
      return o;
    }
    
    public Object get(int index) {
      checkMod();
      checkBoundsExclusive(index);
      return backingList.get(index + offset);
    }

    public void add(int index, Object o) {
      checkMod();
      checkBoundsInclusive(index);
      backingList.add(index + offset, o);
      upMod();
      size++;
    }
    
    public Object remove(int index) {
      checkMod();
      checkBoundsExclusive(index);
      Object o = backingList.remove(index + offset);
      upMod();
      size--;
      return o;
    }

    public void removeRange(int fromIndex, int toIndex) {
      checkMod();
      checkBoundsExclusive(fromIndex);
      checkBoundsInclusive(toIndex);
      
      // this call will catch the toIndex < fromIndex condition
      backingList.removeRange(offset + fromIndex, offset + toIndex);
      upMod();
      size -= toIndex - fromIndex;
    }
    
    public boolean addAll(int index, Collection c) {
      checkMod();
      checkBoundsInclusive(index);
      int s = backingList.size();
      boolean result = backingList.addAll(offset + index, c);
      upMod();
      size += backingList.size() - s;
      return result;
    }
  }
}
