/* DemoServant.java --
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

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;

/**
 * This class handles the actual server functionality in this test
 * application. When the client calls the remote method, this
 * finally results calling the method of this class.
 *
 * The parameters, passed to the server only, are just parameters of the
 * java methods. The parameters that shuld be returned to client
 * are wrapped into holder classes.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DemoServant
  extends _DemoTesterImplBase
{
  /**
   * The field, that can be set and checked by remote client.
   */
  private int m_theField = 17;

  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings. Returs they generalization as a wide string.
   */
  public String passCharacters(String wide, String narrow)
  {
    System.out.println("SERVER: **** Wide and narrow string test.");
    System.out.println("SERVER:   Received '" + narrow + "' and '" + wide +
                       "'"
                      );

    return "return '" + narrow + "' and '" + wide + "'";
  }

  /**
   * Accept and return parameters, having various types.
   */
  public int passSimple(ByteHolder an_octet, int a_long, ShortHolder a_short,
                        StringHolder a_string, DoubleHolder a_double
                       )
  {
    System.out.println("SERVER: ***** Test passing multiple parameters");
    System.out.println("SERVER:   Received:");
    System.out.println("SERVER:   octet " + an_octet.value);
    System.out.println("SERVER:   short " + a_short.value);
    System.out.println("SERVER:   string " + a_string.value);

    // Returning incremented values.
    an_octet.value++;
    a_short.value++;

    // OUT parameter, return only.
    a_double.value = 1;
    a_string.value += " [return]";
    return 452572;
  }

  /**
   * Accept and return the string arrays.
   */
  public String[] passStrings(String[] args)
  {
    System.out.println("SERVER: ***** Transferring string arrays");

    String[] rt = new String[ args.length ];
    for (int i = 0; i < args.length; i++)
      {
        System.out.println("SERVER:   " + args [ i ]);

        // Returning the changed content.
        rt [ i ] = args [ i ] + ":" + args [ i ];
      }
    return rt;
  }

  /**
   * Accept and return the structures.
   */
  public StructureToReturn passStructure(StructureToPass in_structure)
  {
    System.out.println("SERVER: ***** Transferring structures");
    System.out.println("SERVER:   Received " + in_structure.a + ":" +
                       in_structure.b
                      );

    // Create and send back the returned structure.
    StructureToReturn r = new StructureToReturn();
    r.c = in_structure.a + in_structure.b;
    r.n = 555;
    r.arra = new int[] { 11, 22, 33 };
    return r;
  }

  /**
   * Pass and return the tree structure
   */
  public void passTree(TreeNodeHolder tree)
  {
    System.out.println("SERVER: ***** Transferring tree");

    StringBuffer b = new StringBuffer();

    // This both creates the tree string representation
    // and changes the TreeNode names.
    getImage(b, tree.value);
    System.out.println("SERVER:   The tree was: " + b + ", returning changed.");
  }

  /**
   * Just prints the hello message.
   */
  public void sayHello()
  {
    System.out.println("SERVER: ***** Hello, world!");
  }

  /**
   * Get the value of our field.
   */
  public int theField()
  {
    System.out.println("SERVER: ***** Getting the field value, " + m_theField);
    return m_theField;
  }

  /**
   * Set the value of our field.
   */
  public void theField(int a_field)
  {
    System.out.println("SERVER: ***** Setting the field value to " + a_field);
    m_theField = a_field;
  }

  /**
   * Throw an exception.
   *
   * @param parameter specifies which exception will be thrown.
   *
   * @throws WeThrowThisException for the non negative parameter.
   * @throws BAD_OPERATION for the negative parameter.
   */
  public void throwException(int parameter)
                      throws WeThrowThisException
  {
    System.out.println("SERVER: ***** Testing exceptions");
    if (parameter > 0)
      {
        System.out.println("SERVER:   Throwing the user exception, " +
                           "specific field = "+parameter
                          );
        throw new WeThrowThisException(parameter);
      }
    else
      {
        System.out.println("SERVER:   Throwing " +
                           "the BAD_OPERATION, minor 456, completed"
                          );
        throw new BAD_OPERATION(456, CompletionStatus.COMPLETED_YES);
      }
  }

  /**
   * Visit all tree nodes, getting the string representation
   * and adding '++' to the TreeNode names.
   *
   * @param b the buffer to collect the string representation.
   * @param n the rott tree TreeNode.
   */
  private void getImage(StringBuffer b, TreeNode n)
  {
    b.append(n.name);
    n.name = n.name + "++";
    b.append(": (");

    for (int i = 0; i < n.children.length; i++)
      {
        getImage(b, n.children [ i ]);
        b.append(' ');
      }
    b.append(") ");
  }
}
