/* Matcher.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package java.util.regex;

/**
 * @author Michael Koch
 * @since 1.4
 */
public class Matcher
{
  private Pattern pattern;
  
  /**
   * @param sb The target string buffer
   * @param replacement The replacement string
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public Matcher appendReplacement (StringBuffer sb, String replacement)
    throws IllegalStateException
  {
    throw new Error("Not implemented");
  }

  /**
   * @param sb The target string buffer
   */
  public StringBuffer appendTail (StringBuffer sb)
  {
    throw new Error("Not implemented");
  }
 
  /**
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public int end ()
    throws IllegalStateException
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public int end (int group)
    throws IllegalStateException
  {
    throw new Error ("Not implemented");
  }
 
  public boolean find ()
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param start The index to start the new pattern matching
   *
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public boolean find (int start)
  {
    throw new Error ("Not implemented");
  }
 
  /**
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public String group ()
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public String group (int group)
    throws IllegalStateException
  {
    throw new Error ("Not implemented");
  }

  /**
   * @param replacement The replacement string
   */
  public String replaceFirst (String replacement)
  {
    throw new Error ("Not implemented");
  }

  /**
   * @param replacement The replacement string
   */
  public String replaceAll (String replacement)
  {
    throw new Error ("Not implemented");
  }
  
  public int groupCount ()
  {
    throw new Error("Not implemented");
  }
 
  public boolean lookingAt ()
  {
    throw new Error("Not implemented");
  }
  
  /**
   * Attempts to match the entire input sequence against the pattern. 
   *
   * If the match succeeds then more information can be obtained via the
   * start, end, and group methods.
   *
   * @see #start
   * @see #end
   * @see #group
   */
  public boolean matches ()
  {
    throw new Error("Not implemented");
  }
  
  /**
   * Returns the Pattern that is interpreted by this Matcher
   */
  public Pattern pattern ()
  {
    return pattern;
  }
  
  public Matcher reset ()
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param input The new input character sequence
   */
  public Matcher reset (CharSequence input)
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public int start ()
    throws IllegalStateException
  {
    throw new Error("Not implemented");
  }

  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public int start (int group)
    throws IllegalStateException
  {
    throw new Error("Not implemented");
  }
}
