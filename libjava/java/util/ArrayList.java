/* ArrayList.java -- JDK1.2's answer to Vector; this is an array-backed
   implementation of the List interface
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


package java.util;

import java.lang.reflect.Array;
import java.io.Serializable;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamField;

/**
 * An array-backed implementation of the List interface.  ArrayList
 * performs well on simple tasks:  random access into a list, appending
 * to or removing from the end of a list, checking the size, &c.
 *
 * @author        Jon A. Zeppieri
 * @version       $Id: ArrayList.java,v 1.4 2000/03/15 21:59:06 rao Exp $
 * @see           java.util.AbstractList
 * @see           java.util.List
 */
public class ArrayList extends AbstractList 
  implements List, Cloneable, Serializable
{
  /** the default capacity for new ArrayLists */
  private static final int DEFAULT_CAPACITY = 16;

  /** the number of elements in this list */
  int _iSize;

  /** where the data is stored */
  Object[] _arData;

  /** used for serialization -- denotes which fields are serialized */
  private static final ObjectStreamField[] serialPersistentFields =
  {new ObjectStreamField("size", int.class)};

  /** 
   * Construct a new ArrayList with the supplied initial capacity. 
   *
   * @param     iCapacity
   */
  public ArrayList(int iCapacity)
  {
    _arData = new Object[iCapacity];
  }


  /**
   * Construct a new ArrayList with the default capcity 
   */
  public ArrayList()
  {
    this(DEFAULT_CAPACITY);
  }

  /** 
   * Construct a new ArrayList, and initialize it with the elements
   * in the supplied Collection; Sun specs say that the initial 
   * capacity is 110% of the Collection's size.
   *
   * @param        oCollection     the collection whose elements will
   *                               initialize this list
   */
  public ArrayList(Collection oCollection)
  {
    this((int) (oCollection.size() * 1.1));
    addAll(oCollection);
  }

  /**
   * Guarantees that this list will have at least enough capacity to
   * hold iMinCapacity elements.
   *
   * @param      iMinCapacity     the minimum guaranteed capacity
   */
  public void ensureCapacity(int iMinCapacity)
  {
    Object[] arNewData;
    int iCapacity = _arData.length;

    if (iMinCapacity > iCapacity)
    {
      arNewData = new Object[Math.max((iCapacity * 2), iMinCapacity)];
      System.arraycopy(_arData, 0, arNewData, 0, iCapacity);
      _arData = arNewData;
    }
  }

  /**
   * Appends the supplied element to the end of this list.
   *
   * @param       oElement      the element to be appended to this list
   */
  public boolean add(Object oElement)
  {
    ensureCapacity(_iSize + 1);
    _arData[_iSize++] = oElement;
    modCount++;
    return true;
  }

  /**
   * Retrieves the element at the user-supplied index.
   *
   * @param    iIndex        the index of the element we are fetching
   * @throws   IndexOutOfBoundsException  (iIndex < 0) || (iIndex >= size())
   */
  public Object get(int iIndex)
  {
    if (iIndex >= _iSize)
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "index=" + String.valueOf(iIndex));
    return _arData[iIndex];
  }

  /**
   * Returns the number of elements in this list 
   */
  public int size()
  {
    return _iSize;
  }

  /**
   * Removes the element at the user-supplied index
   *
   * @param     iIndex      the index of the element to be removed
   * @return    the removed Object
   * @throws    IndexOutOfBoundsException  (iIndex < 0) || (iIndex >= size())
   */
  public Object remove(int iIndex)
  {
    Object oResult;

    if (iIndex >= _iSize)
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "index=" + String.valueOf(iIndex));

    oResult = _arData[iIndex];

    if (iIndex != --_iSize)
      System.arraycopy(_arData, (iIndex + 1), _arData, iIndex, 
                       (_iSize - iIndex));
  
    modCount++;
    _arData[_iSize] = null;

    return oResult;
  }

  /**
   * Removes all elements in the half-open interval [iFromIndex, iToIndex).
   *
   * @param     iFromIndex   the first index which will be removed
   * @param     iToIndex     one greater than the last index which will be 
   *                         removed
   */
  public void removeRange(int iFromIndex, int iToIndex)
  {
    int iReduction;
    int i;

    if ((iFromIndex >= _iSize) || (iToIndex >= _iSize))
    {
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "indices=" + 
                                          String.valueOf(iFromIndex) + "," +
                                          String.valueOf(iToIndex));
    }
    else if (iFromIndex > iToIndex)
    {
      throw new IllegalArgumentException("fromIndex(" + 
                                         String.valueOf(iFromIndex) + 
                                         ") > toIndex(" +
                                         String.valueOf(iToIndex) + ")");
    }
    else if (iFromIndex != iToIndex)
    {
      iReduction = iToIndex - iFromIndex;
      System.arraycopy(_arData, (iFromIndex + iReduction), _arData,
                       iFromIndex, (_iSize - iFromIndex - iReduction));
      modCount++;

      for (i = (iFromIndex + iReduction); i < _iSize; i++)
        _arData[i] = null;

      _iSize -= iReduction;
    }
  }

  /**
   * Adds the supplied element at the specified index, shifting all
   * elements currently at that index or higher one to the right.
   *
   * @param     iIndex      the index at which the element is being added
   * @param     oElement    the element being added
   */
  public void add(int iIndex, Object oElement)
  {
    if (iIndex > _iSize)
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "index=" + String.valueOf(iIndex));

    ensureCapacity(_iSize + 1);
    System.arraycopy(_arData, iIndex, _arData, 
                      (iIndex + 1), (_iSize - iIndex));
    _arData[iIndex] = oElement;
    _iSize++;
    modCount++;
  }

  /** 
   * Add each element in the supplied Collection to this List.
   *
   * @param        oCollection     a Collection containing elements to be 
   *                               added to this List
   */
  public boolean addAll(Collection oCollection)
  {
    Iterator itElements;
    int iLen = oCollection.size();

    if (iLen > 0)
    {
      ensureCapacity(_iSize + iLen);
      modCount++;

      itElements = oCollection.iterator();

      while (itElements.hasNext())
        _arData[_iSize++] = itElements.next();

      return true;
    }
    return false;
  }

  /** 
   * Add all elements in the supplied collection, inserting them beginning
   * at the specified index.
   *
   * @param     iIndex       the index at which the elements will be inserted
   * @param     oCollection  the Collection containing the elements to be
   *                         inserted
   */
  public boolean addAll(int iIndex, Collection oCollection)
  {
    Iterator itElements;
    int iLen;

    if (iIndex > _iSize)
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "index=" + String.valueOf(iIndex));

    iLen = oCollection.size();

    if (iLen > 0)
    {
      ensureCapacity(_iSize + iLen);

      System.arraycopy(_arData, iIndex, _arData, 
                       (iIndex + iLen), (_iSize - iIndex));
      modCount++;
      _iSize += iLen;

      itElements = oCollection.iterator();
      while (itElements.hasNext())
        _arData[iIndex++] = itElements.next();

      return true;
   }
    return false;
  }

  /**
   * Creates a shallow copy of this ArrayList
   */
  public Object clone()
  {
    ArrayList oClone;

    try
    {
      oClone = (ArrayList) super.clone();
      oClone._arData = _arData;
      oClone._iSize = _iSize;
    }
    catch(CloneNotSupportedException e)
    {
      oClone = null;
    }
    return oClone;
  }

  /** 
   * Returns true iff oElement is in this ArrayList.
   *
   * @param     oElement     the element whose inclusion in the List is being
   *                         tested
   */
  public boolean contains(Object oElement)
  {
    return (indexOf(oElement) != -1);
  }

  /**
   * Returns the lowest index at which oElement appears in this List, or 
   * -1 if it does not appear.
   *
   * @param    oElement       the element whose inclusion in the List is being
   *                          tested
   */
  public int indexOf(Object oElement)
  {
    int i;

    for (i = 0; i < _iSize; i++)
    {
      if (doesEqual(oElement, _arData[i]))
        return i;
    }
    return -1;
  }

  /**
   * Returns the highest index at which oElement appears in this List, or 
   * -1 if it does not appear.
   *
   * @param    oElement       the element whose inclusion in the List is being
   *                          tested
   */
  public int lastIndexOf(Object oElement)
  {
    int i;

    for (i = _iSize - 1; i >= 0; i--)
    {
      if (doesEqual(oElement, _arData[i]))
        return i;
    }
    return -1;
  }

  /**
   * Removes all elements from this List
   */
  public void clear()
  {
    int i;

    if (_iSize > 0)
    {
      modCount++;
      _iSize = 0;

      for (i = 0; i < _iSize; i++)
        _arData[i] = null;
    }      
  }

  /**
   * Sets the element at the specified index.
   *
   * @param     iIndex     the index at which the element is being set
   * @param     oElement   the element to be set
   * @return    the element previously at the specified index, or null if
   *            none was there
   */
  public Object set(int iIndex, Object oElement)
  {
    Object oResult;

    if (iIndex >= _iSize)
      throw new IndexOutOfBoundsException("ArrayList size=" +
                                          String.valueOf(_iSize) + "; " +
                                          "index=" + String.valueOf(iIndex));
    oResult = _arData[iIndex];
    // SEH: no structural change, so don't update modCount
    _arData[iIndex] = oElement;

    return oResult;
  }

  /**
   * Returns an Object Array containing all of the elements in this ArrayList
   */
  public Object[] toArray()
  {
    Object[] arObjects = new Object[_iSize];
    System.arraycopy(_arData, 0, arObjects, 0, _iSize);
    return arObjects;
  }

  /**
   * Returns an Array whse component type is the runtime component type of
   * the passes-in Array.  The returned Array is populated with all of the
   * elements in this ArrayList.  If the passed-in Array is not large enough
   * to store all of the elements in this List, a new Array will be created 
   * and returned; if the passed-in Array is <i>larger</i> than the size
   * of this List, then size() + 1 index will be set to null.
   *
   * @param      arObjects      the passed-in Array
   */
  public Object[] toArray(Object[] arObjects)
  {
    Object[] arReturn = (arObjects.length >= _iSize)
      ? arObjects 
      : (Object[])
      Array.newInstance(arObjects.getClass().getComponentType(), _iSize);

    System.arraycopy(_arData, 0, arReturn, 0, _iSize);

    if (arReturn.length > _iSize)
      arReturn[_iSize] = null;

    return arReturn;
  }

  /**
   * Trims the capacity of tjis List to be equal to its size; 
   * a memory saver. 
   */
  public void trimToSize()
  {
    Object[] arNewData = new Object[_iSize];
    System.arraycopy(_arData, 0, arNewData, 0, _iSize);
    modCount++;
    _arData = arNewData;
  }

  private void writeObject(ObjectOutputStream oOut)
    throws IOException
  {
    int i;

    ObjectOutputStream.PutField oFields = oOut.putFields();
    oFields.put("size", _iSize);
    oOut.writeFields();

    oOut.writeInt(_arData.length);
    for (i = 0; i < _arData.length; i++)
      oOut.writeObject(_arData[i]);
  }

  private void readObject(ObjectInputStream oIn)
    throws IOException, ClassNotFoundException
  {
    int i;
    int iCapacity;

    ObjectInputStream.GetField oFields = oIn.readFields();
    _iSize = oFields.get("size", 0);

    iCapacity = oIn.readInt();
    _arData = new Object[iCapacity];

    for (i = 0; i < iCapacity; i++)
      _arData[i] = oIn.readObject();
  }

  private static final boolean doesEqual(Object oOne, Object oTwo)
  {
    return ((oOne == null) ? (oTwo == null) : oOne.equals(oTwo));
  }
}
