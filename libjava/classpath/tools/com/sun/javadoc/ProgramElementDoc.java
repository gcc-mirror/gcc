/* ProgramElementDoc.java -- Common ops for all program elements.
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
  * This is the comment super-interface of all items that are "program
  * elements".  This includes classes, interfaces, fields, constructors,
  * and methods.
  */
public interface ProgramElementDoc extends Doc
{

/**
  * This method returns the class which contains this element.  If this
  * is a class that is not an inner class, <code>null</code> will be
  * returned.
  *
  * @returned The class element that contains this item, or <code>null</code>
  * if this item is a class that is not an inner class.
  */
public abstract ClassDoc
containingClass();

/*************************************************************************/

/**
  * This method returns the package which contains this element.  If this
  * element is in the default generic package, then the name of the
  * package element returned will be "".
  *
  * @return The package element that contains this item.
  */
public abstract PackageDoc
containingPackage();

/*************************************************************************/

/**
  * This method returns the fully qualified name of this element.
  *
  * @return The fully qualified name of this element.
  */
public abstract String
qualifiedName();

/*************************************************************************/

/**
  * This method returns the modifier specificier number, which is what?
  *
  * @return The modifier for this element.
  */
public abstract int
modifierSpecifier();

/*************************************************************************/

/**
  * This method returns a string with the element modifiers.  For example,
  * the modifiers of a method declaration might be "protected abstract".
  *
  * @return The modifier string.
  */
public abstract String
modifiers();

/*************************************************************************/

/**
  * This method tests whether or not this element is public.
  *
  * @return <code>true</code> if this element is public, <code>false</code>
  * otherwise.
  */
public abstract boolean
isPublic();

/*************************************************************************/

/**
  * This method tests whether or not this element is protected.
  *
  * @return <code>true</code> if this element is protected, <code>false</code>
  * otherwise.
  */
public abstract boolean
isProtected();

/*************************************************************************/

/**
  * This method tests whether or not this element is private.
  *
  * @return <code>true</code> if this element is private, <code>false</code>
  * otherwise.
  */
public abstract boolean
isPrivate();

/*************************************************************************/

/**
  * This method tests whether or not this element is package private.
  *
  * @return <code>true</code> if this element is package private,
  * <code>false</code> otherwise.
  */
public abstract boolean
isPackagePrivate();

/*************************************************************************/

/**
  * This method tests whether or not this element is static.
  *
  * @return <code>true</code> if this element is static, <code>false</code>
  * otherwise.
  */
public abstract boolean
isStatic();

/*************************************************************************/

/**
  * This method tests whether or not this element is final.
  *
  * @return <code>true</code> if this element is final, <code>false</code>
  * otherwise.
  */
public abstract boolean
isFinal();

} // interface ProgramElementDoc

