/* Map.java -- An object that maps keys to values
   Copyright (C) 1998 Free Software Foundation, Inc.

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
// ~ Doc comments for everything.

package java.util;

public interface Map
{
  public void clear();
  public boolean containsKey(Object key);
  public boolean containsValue(Object value);
  public Set entrySet();
  public boolean equals(Object o);
  public Object get(Object key);
  public Object put(Object key, Object value);
  public int hashCode();
  public boolean isEmpty();
  public Set keySet();
  public void putAll(Map m);
  public Object remove(Object o);
  public int size();
  public Collection values();

  public static interface Entry
  {
    public Object getKey();
    public Object getValue();
    public Object setValue(Object value);
    public int hashCode();
    public boolean equals(Object o);
  }
}
