/* MatchResult.java -- Result of a regular expression match.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package java.util.regex;

/**
 * This interface represents the result of a regular expression match.
 * It can be used to query the contents of the match, but not to modify
 * them.
 * @since 1.5
 */
public interface MatchResult
{
  /** Returns the index just after the last matched character.  */
  int end();

  /**
   * Returns the index just after the last matched character of the
   * given sub-match group.
   * @param group the sub-match group
   */
  int end(int group);

  /** Returns the substring of the input which was matched.  */
  String group();

  /**
   * Returns the substring of the input which was matched by the
   * given sub-match group.
   * @param group the sub-match group
   */
  String group(int group);

  /** Returns the number of sub-match groups in the matching pattern.  */
  int groupCount();

  /** Returns the index of the first character of the match.  */
  int start();

  /**
   * Returns the index of the first character of the given sub-match
   * group.
   * @param group the sub-match group
   */
  int start(int group);
}
