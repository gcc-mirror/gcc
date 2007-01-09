/* ClassFileTransformer.java -- Implementation of this interface is used by an agent to
   transform class files.
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


package java.lang.instrument;

import java.security.ProtectionDomain;

/**
 * This interface should be implemented by classes wishing to transform
 * classes bytecode when defining or redefining classes.
 *
 * @author Nicolas Geoffray (nicolas.geoffray@menlina.com)
 * @see Instrumentation
 * @see Instrumentation#addTransformer(java.lang.instrument.ClassFileTransformer)
 * @see Instrumentation#removeTransformer(java.lang.instrument.ClassFileTransformer)
 * @since 1.5
 */
public interface ClassFileTransformer
{

  /**
   * Implementation of this method transforms a class by redefining its
   * bytecodes. Once a ClassFileTransformer object registers itself to the
   * Instrumentation object, this method will be called each time a class is
   * defined (<code>ClassLoader.defineClass</code>) or redefined
   * (<code>Instrumentation.redefineClasses</code>)
   * @param loader the loader of the class
   * @param className the name of the class with packages separated with "/"
   * @param classBeingRedefined the class being redefined if it's the case,
   * null otherwise
   * @param protectionDomain the protection domain of the class being defined or
   * redefined
   * @param classfileBuffer the input byte buffer in class file format
   * 
   * @return a class file buffer or null when no transformation has been performed
   * 
   * @throws IllegalClassFormatException if the byte buffer does not represent
   * a well-formed class file
   * @see Instrumentation#redefineClasses(java.lang.instrument.ClassDefinition[])
   *
   */
  byte[] transform(ClassLoader loader,
                 String className,
                 Class<?> classBeingRedefined,
                 ProtectionDomain protectionDomain,
                 byte[] classfileBuffer)
                 throws IllegalClassFormatException;
}

