/* VMInstrumentationImpl.java -- interface for the GNU implementation
   of InstrumentationImpl
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package gnu.java.lang;

import java.lang.instrument.ClassDefinition;
import java.lang.instrument.Instrumentation;

/**
 * @author Nicolas Geoffray (nicolas.geoffray@menlina.com)
 * @since 1.5
 */
final class VMInstrumentationImpl
{
 
  /**
   * Returns if the current JVM supports class redefinition
   * 
   * @return true if the current JVM supports class redefinition
   */
  static boolean isRedefineClassesSupported() { return false; }
    
  /**
   * Redefines classes given as parameters. The method has to call
   * the callTransformers from InstrumentationImpl
   *
   * @param inst an instrumentation object
   * @param definitions an array of bytecode<->class correspondance
   *
   * @throws ClassNotFoundException if a class cannot be found 
   * @throws UnmodifiableClassException if a class cannot be modified 
   * @throws UnsupportedOperationException if the JVM does not support
   * redefinition or the redefinition made unsupported changes
   * @throws ClassFormatError if a class file is not valid
   * @throws NoClassDefFoundError if a class name is not equal to the name
   * in the class file specified
   * @throws UnsupportedClassVersionError if the class file version numbers
   * are unsupported
   * @throws ClassCircularityError if circularity occured with the new
   * classes
   * @throws LinkageError if a linkage error occurs 
   */
  static void redefineClasses(Instrumentation inst,
			      ClassDefinition[] definitions)
  {
  }
 
  /**
   * Get all the classes loaded by the JVM.
   * 
   * @return an array containing all the classes loaded by the JVM. The array
   * is empty if no class is loaded.
   */
  static Class[] getAllLoadedClasses()
  {
    return new Class[0];
  }

  /**
   * Get all the classes loaded by a given class loader
   * 
   * @param loader the loader
   * 
   * @return an array containing all the classes loaded by the given loader.
   * The array is empty if no class was loaded by the loader.
   */
  static Class[] getInitiatedClasses(ClassLoader loader)
  {
    return new Class[0];
  }


  /**
   * Get the size of an object. The object is not null
   * 
   * @param objectToSize the object
   * @return the size of the object
   */
  static long getObjectSize(Object objectToSize)
  {
    return 0;
  }
}
