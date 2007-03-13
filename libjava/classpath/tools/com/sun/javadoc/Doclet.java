/* Doclet.java -- Doclet API
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
  * This class documents the method that must be implemented by a doclet.
  * It may be used as the superclass of a doclet, but this is not required.
  * As long as the doclet implements the <code>start</code> method, all is ok.
  */
public abstract class Doclet
{

/**
  * This is the entry point to a doclet.  All doclets must implement this
  * method.
  *
  * @param rd The <code>RootDoc</code> instance for this javadoc run.
  *
  * @return <code>true</code> on success, <code>false</code> on failure.
  */
public static boolean
start(RootDoc root)
{
  return(false);
}

/*************************************************************************/

/**
  * This method returns the number of arguments to the option, including
  * the option itself.  This is not required of doclets.
  *
  * @param opt The option to check.
  *
  * @return The number of arguments to the option, or zero if the option is
  * unknown, or a negative number if an error occurred.
  */
public static int
optionLength(String opt)
{
  return(0);
}

/*************************************************************************/

/**
  * This method is called to verify that the options supplied by the caller
  * are valid.  This is not required of doclets.
  *
  * @param opts The list of options supplied by the user.
  * @param logger A mechanism for this method to report errors to the user.
  *
  * @return <code>true</code> if the options are valid, <code>false</code>
  * otherwise.
  */
public static boolean
validOptions(String[][] opts, DocErrorReporter logger)
{
  return(true);
}

} // class Doclet

