/* Copyright (C) 2000 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.util.Enumeration;
import java.io.Serializable;

public interface Name extends Cloneable, Serializable
{
  public Object clone();
  public int compareTo(Object obj);
  public int size();
  public boolean isEmpty();
  public Enumeration getAll();
  public String get(int posn);
  public Name getPrefix(int posn);
  public Name getSuffix(int posn);
  public boolean startsWith(Name n);
  public boolean endsWith(Name n);
  public Name addAll(Name suffix) throws InvalidNameException;
  public Name addAll(int posn, Name n) throws InvalidNameException;
  public Name add(String comp) throws InvalidNameException;
  public Name add(int posn, String comp) throws InvalidNameException;
  public Object remove(int posn) throws InvalidNameException;
}
