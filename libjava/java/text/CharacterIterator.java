// CharacterIterator.java - Protocol for iterating over Unicode characters.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 22, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */

public interface CharacterIterator extends Cloneable
{
  public abstract Object clone ();
  public abstract char current ();
  public abstract char first ();
  public abstract int getBeginIndex ();
  public abstract int getEndIndex ();
  public abstract int getIndex ();
  public abstract char last ();
  public abstract char next ();
  public abstract char previous ();
  public abstract char setIndex (int idx);

  public static final char DONE = '\uffff';
}
