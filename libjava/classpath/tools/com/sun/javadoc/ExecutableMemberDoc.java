/* ExecutableMemberDoc.java -- Document methods and constructors
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
  * This is the super-interface for interfaces describing constructors and
  * methods.
  */
public interface ExecutableMemberDoc extends MemberDoc
{

/**
  * This method returns a list of all the execeptions that are declared
  * to be thrown in this method or constructor.
  *
  * @return The list of exceptions for this method.
  */
public abstract ClassDoc[]
thrownExceptions();

/*************************************************************************/

/**
  * This method tests whether or not this method/constructor is native.
  *
  * @return <code>true</code> if the method is native, <code>false</code>
  * otherwise.
  */
public abstract boolean
isNative();

/*************************************************************************/

/**
  * This method tests whether or not this method/constructor is
  * synchronized.
  *
  * @return <code>true</code> if the method is synchronized,
  * <code>false</code> otherwise.
  */
public abstract boolean
isSynchronized();

/*************************************************************************/

/**
  * This method returns the list of parameters for this method/constructor.
  *
  * @return The list of parameters for this method.
  */
public abstract Parameter[]
parameters();

/*************************************************************************/

/**
  * This method returns the list of "@throws" and "@exception" tags in this
  * method/constructor.
  *
  * @return The list of exception doc tags.
  */
public abstract ThrowsTag[]
throwsTags();

/*************************************************************************/

/**
  * This method return the list of "@param" tags in this method/constructor.
  *
  * @return The list of parameter doc tags for this method.
  */
public abstract ParamTag[]
paramTags();

/*************************************************************************/

/**
  * This method returns the signature of this method in pseudo-code format,
  * with fully qualified class references.  For example, the method
  * <code>read(String str, boolean bool)</code> would have the signature
  * <code>(java.lang.String, boolean)</code> returned by this method.
  *
  * @return The signature for this method.
  */
public abstract String
signature();

/*************************************************************************/

/**
  * This method returns the signature of this method in pseudo-code format,
  * with uqualified class references.  For example, the method
  * <code>read(String str, boolean bool)</code> would have the signature
  * <code>(String, boolean)</code> returned by this method.
  *
  * @return The signature for this method.
  */
public abstract String
flatSignature();

} // interface ExecutableMemberDoc

