/* comTester.java --
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


package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;

/**
 * The interface of our remote object. Some IDL compiles split it
 * into "comTester" and "comTesterOperations", but we do not see
 * much sense in doing this here.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface comTester
{
  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * Both types are mapped into java String.
   *
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings.
   */
  String passCharacters(String wide, String narrow);

  /**
   * Passes various parameters in both directions.
   * The parameters that must return the value are wrapped in holders.
   */
  int passSimple(ByteHolder an_octet, int a_long, ShortHolder a_short,
                 StringHolder a_string, DoubleHolder a_double
                );

  /**
   * Passes and returns the string sequence (flexible length).
   */
  String[] passStrings(String[] arg);

  /**
   * Passes and returns the structures.
   */
  returnThis passStructure(passThis in_structure);

  /**
   * Pass and return the tree structure
   *
   * @param tree the root node of the tree.
   */
  void passTree(nodeHolder tree);

  /**
   * Just prints the "Hello" message.
   */
  void sayHello();

  /**
   * Gets the value of the field in our object.
   */
  int theField();

  /**
   * Sets the value for the field in our object.
   */
  void theField(int newTheField);

  /**
   *  Throws either 'ourUserException' with the 'ourField' field
   *  initialised to the passed positive value
   *  or system exception (if the parameter is zero or negative).
   */
  void throwException(int parameter)
               throws ourUserException;
}
