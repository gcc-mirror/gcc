/* ReportingScannerState.java -- A state for debugging purposes.
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.beans.encoder;

/**
 * A <code>ScannerState</code> implementation that prints useful details
 * about its arguments. Use it when the XML encoding does not work correctly
 * and you want to find out how things relate to each other.
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
class ReportingScannerState extends ScannerState
{

  void methodInvocation(String methodName)
  {
        System.out.println("methodInvocation: " + methodName + "()");
  }

  void staticMethodInvocation(String className, String methodName)
  {
        System.out.println("staticMethodInvocation: " + className + "." + methodName + "()");
  }

  void staticFieldAccess(String className, String fieldName)
  {
    System.out.println("staticFieldAccess: " + className + "." + fieldName);
  }

  void classResolution(String className)
  {
        System.out.println("classResolution: " + className);
  }

  void objectInstantiation(String className, ObjectId objectId)
  {
        System.out.println("objectInstantiation: " + className);
  }

  void primitiveInstantiation(String primitiveName, String valueAsString)
  {
        System.out.println("primitiveInstantiation: (" + primitiveName + ") " + valueAsString);
  }

  void objectArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId)
  {
    System.out.println("objectArrayInstantiation: new " + arrayClassName + "[" + lengthAsString + "]");
  }

  void primitiveArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId)
  {
    System.out.println("primitiveArrayInstantiation: new " + arrayClassName + "[" + lengthAsString + "]");
  }

  void arraySet(String indexAsString)
  {
        System.out.println("arraySet: " + indexAsString);
  }

  void arrayGet(String indexAsString)
  {
        System.out.println("arrayGet: " + indexAsString);
  }

  void listGet()
  {
        System.out.println("listGet");
  }

  void listSet()
  {
        System.out.println("listSet");
  }

  void nullObject()
  {
        System.out.println("nullObject");
  }

  void stringReference(String string)
  {
    System.out.println("stringReference: " + string);
  }

  void objectReference(ObjectId id)
  {
    System.out.println("objectReference: " + id);
  }

 void end()
 {
        System.out.println("-close");
 }

}
