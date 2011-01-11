/* ClassDoc.java -- Document a Java class or interface
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

public interface ClassDoc extends ProgramElementDoc, Type
{

/**
  * This method tests whether or not the class represented by this object
  * is abstract.
  *
  * @return <code>true</code> if the class is abstract, <code>false</code>,
  * otherwise.
  */
public abstract boolean
isAbstract();

/*************************************************************************/

/**
  * This method tests whether or not the class represented by this object
  * is serializable.  That is, whether or not the class implements the
  * <code>java.io.Serializable</code> interface.  This includes classes
  * which are externalizable.
  *
  * @return <code>true</code> if the class is serializable,
  * <code>false</code> otherwise.
  */
public abstract boolean
isSerializable();

/*************************************************************************/

/**
  * This method tests whether or not the class represented by this object
  * is externalizable.  That is, whether or not the class implements the
  * <code>java.io.Externalizable</code> interface.
  *
  * @return <code>true</code> if the class is externalizable,
  * <code>false</code> otherwise.
  */
public abstract boolean
isExternalizable();

/*************************************************************************/

/**
  * This method returns the serialization methods for the class
  * represented by this object.  Is the custom readObject/writeObject
  * methods?
  *
  * @return The serialization methods for this class.
  */
public abstract MethodDoc[]
serializationMethods();

/*************************************************************************/

/**
  * This method returns the list of fields that are serialized in this
  * class.  This will return either the list of fields with an
  * "@serial" declaration, or, if it exists, the
  * <code>serialPersistentField</code> field.
  *
  * @return The list of serializable fields.
  */
public abstract FieldDoc[]
serializableFields();

/*************************************************************************/

/**
  * This method tests whether or not the class represented by this object
  * specifically defines its serializable fields in a
  * <code>serialPersistentFields</code> field.
  *
  * @return <code>true</code> if this class explicitly defines its
  * serializable fields, <code>false</code> otherwise.
  */
public abstract boolean
definesSerializableFields();

/*************************************************************************/

/**
  * This method returns the superclass of the class represented by this
  * object.
  *
  * @return The superclass of this class.
  */
public abstract ClassDoc
superclass();

/*************************************************************************/

/**
  * This method tests whether or not the class represented by this object is
  * a subclass of the specified class.
  *
  * @param cls The <code>ClassDoc</code> object of the class to test against.
  *
  * @return <code>true</code> if this class is a subclass of the specified
  * class, <code>false</code> otherwise.
  */
public abstract boolean
subclassOf(ClassDoc cls);

/*************************************************************************/

/**
  * This method returns this list of interfaces implemented (or in the case
  * of interfaces, extended) by this class.  This list will only include
  * interfaces directly implemented by this class, not those inherited by
  * interfaced implemented in this class.
  *
  * @return The list of interfaces this class implements.
  */
public abstract ClassDoc[]
interfaces();

/*************************************************************************/

/**
  * This method returns the list of fields that are visible to the user in
  * this class, or the list of all fields in this class.
  *
  * @param filtered if true, only return visible (included) fields;
  * otherwise, return all fields.
  *
  * @return The list of visible fields in this class, or the list of
  * all fields in this class.
  */
public abstract FieldDoc[]
fields(boolean filtered);

/*************************************************************************/

/**
  * This method returns the list of fields that are visible to the user in
  * this class.  Does this depend on the -private -protected, etc flags
  * passed to javadoc?
  *
  * @return The list of visible fields in this class.
  */
public abstract FieldDoc[]
fields();

/*************************************************************************/

/**
  * This method returns either the list of methods that are visible to
  * the user in the class represented by this object, or a list of all
  * methods, excluding constructor methods.
  *
  * @param filtered if true, only return visible (included) methods;
  * otherwise, return all methods.
  *
  * @return The list of all methods in this class, or the list of
  * visible methods in this class.
  */
public abstract MethodDoc[]
methods(boolean filtered);

/*************************************************************************/

/**
  * This method returns the list of methods that are visible to the user in
  * the class represented by this object, excluding constructor methods.
  *
  * @return The list of visible methods in this class.
  */
public abstract MethodDoc[]
methods();

/*************************************************************************/

/**
  * This method returns either the list of constructors that are
  * visible to the user in the class represented by this object, or
  * the list of all constructors.
  *
  * @param filtered if true, only return visible (included)
  * constructors; otherwise, return all constructors.
  *
  * @return The list of all constructors in this class, or the list
  * of visible constructors in this class.
  */
public abstract ConstructorDoc[]
constructors(boolean filtered);

/*************************************************************************/

/**
  * This method returns the list of constructors that are visible to the user
  * in the class represented by this object.
  *
  * @return The list visible constructors in this class.
  */
public abstract ConstructorDoc[]
constructors();

/*************************************************************************/

/**
  * This method returns the list of inner classes that are visible to
  * the user within the class represented by this object.
  *
  * @return The list of visible inner classes for this object.
  */
public abstract ClassDoc[]
innerClasses();

/*************************************************************************/

/**
  * This method returns the list of all inner classes within the class
  * represented by this object, or the list of visible inner classes
  * in this class.
  *
  * @param filtered if true, only return visible (included) inner
  * classes; otherwise, return all inner classes.
  *
  * @return The list of all inner classes for this object, or the list
  * of visible inner classes.
  */
public abstract ClassDoc[]
innerClasses(boolean filtered);

/*************************************************************************/

/**
  * This method returns a <code>ClassDoc</code> for the named class.  The
  * following search order is used:
  * <p>
  * <ol>
  * <li>Fully qualified class name.
  * <li>Inner classes within this class.
  * <li>In the current package.
  * <li>In the imports for this class.
  * </ol>
  *
  * @param name The name of the class to find.
  *
  * @return The requested class, or <code>null</code> if the requested
  * class cannot be found.
  */
public abstract ClassDoc
findClass(String name);

/*************************************************************************/

/**
  * This method returns the list of classes that are imported.  This
  * excludes any imports of complete packages.
  *
  * @return The list of imported classes.
  */
public abstract ClassDoc[]
importedClasses();

/*************************************************************************/

/**
  * This method returns the list of packages that are imported. This
  * excludes any individual class imports.
  *
  * @return The list of imported packages.
  */
public abstract PackageDoc[]
importedPackages();

/*************************************************************************/

/**
  * This method returns the formal type parameters of this class.
  * The returned array is empty if the class does not represent a
  * parameterized type.
  *
  * @return The list of type parameters.
  * @since 1.5
  */
TypeVariable[]
typeParameters();

} // interface ClassDoc
