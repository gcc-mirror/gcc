/* Tag.java -- Common operations on Javadoc tags.
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package com.sun.javadoc;

/**
  * This is the super-interface for all Javadoc tags.
  */
public interface Tag extends java.io.Serializable
{
/**
  * This method returns the name of the tag.
  *
  * @return The name of the tag.
  */
public abstract String
name();

/*************************************************************************/

/**
  * This method returns the kind of tag.  ????
  *
  * @return The kind of the tag.
  */
public abstract String
kind();

/*************************************************************************/

/**
  * This method returns the text for this tag.
  *
  * @return The text for this tag.
  */
public abstract String
text();

/*************************************************************************/

/**
  * This method returns the tag as a <code>String</code>.  What kind of
  * string?
  *
  * @return This tag as a <code>String</code>.
  */
public abstract String
toString();

/*************************************************************************/

/**
  * This method returns the inline tags for this comment.
  *
  * @return The inline tags for this comment.
  */
public abstract Tag[]
inlineTags();

/*************************************************************************/

/**
  * This method returns the first sentence of the doc comment as an array
  * of <code>Tag</code>'s.
  *
  * @return The first sentence of the comment as tags.
  */
public abstract Tag[]
firstSentenceTags();

} // interface Tag
