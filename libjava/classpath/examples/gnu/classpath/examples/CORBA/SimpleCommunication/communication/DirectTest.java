/* DirectTest.java --
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
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.UserException;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * This code uses CORBA to call various methods of the remote object,
 * passing data structures in both directions. It finds the server by
 * reading the IOR.txt file that must be present in the folder,
 * where the program has been started.
 *
 * The IOR.txt file is written by the server
 * {@link gnu.classpath.examples.CORBA.SimpleCommunication.DemoServer}.
 * The server should be reachable over Internet, unless blocked by
 * security tools.
 *
 * This code is tested for interoperability with Sun Microsystems
 * java implementation 1.4.2 (08.b03). Server, client of both can
 * be started either on Sun's or on Classpath CORBA implementation,
 * in any combinations.
 *
 * BE SURE TO START THE SERVER BEFORE STARTING THE CLIENT.
 *
 * This version uses direct casting. This is the most convenient
 * method, but it is normally used together with the IDL compiler.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DirectTest
{
  /*
  * The IOR.txt file, used to find the server and the object on the server. is written when starting the accompanying
  */
  public static final String IOR_FILE = "IOR.txt";

  /**
   * The invocation target.
   */
  DemoTester object;

  /**
   * Get the object reference.
   */
  public static void main(String[] args)
  {
    try
      {
        ORB orb = org.omg.CORBA.ORB.init(args, null);

        File f = new File(IOR_FILE);
        char[] c = new char[ (int) f.length() ];
        FileReader fr = new FileReader(f);
        fr.read(c);
        fr.close();

        String ior = new String(c);
        DirectTest we = new DirectTest();
        we.object = (DemoTester) orb.string_to_object(ior);
        we.Demo();
        orb.shutdown(false);
      }
    catch (IOException ex)
      {
        System.out.println("Cannot find or read the IOR file " +
                           "in the current folder"
                          );
        ex.printStackTrace();
      }
  }

  /** Run all demos. */
  public void Demo()
  {
    testHello();
    testField();
    testParameters();
    testStringArray();
    testStructure();
    testWideNarrowStrings();
    testTree();
    testSystemException();
    testUserException();
  }

  /**
   * Test the field getter/setter.
   */
  public void testField()
  {
    System.out.println("***** Test the remote field getter/setter.");
    System.out.println("The field value is now " + object.theField());
    System.out.println("Setting it to 555");
    object.theField(555);
    System.out.println("The field value is now " + object.theField());
  }

  /** The simple invocation of the parameterless remote method. */
  public void testHello()
  {
    System.out.println("***** Say hello (see the server console).");
    object.sayHello();
  }

  /**
   * Test passing multiple parameters in both directions.
   */
  public void testParameters()
  {
    System.out.println("***** Pass multiple parameters.");

    // Holder classes are required to simulate passing
    // "by reference" (modification is returned back to the server).
    ByteHolder a_byte = new ByteHolder((byte) 0);
    ShortHolder a_short = new ShortHolder((short) 3);
    StringHolder a_string = new StringHolder("[string 4]");

    // This is an 'out' parameter; the value must not be passed to servant.
    DoubleHolder a_double = new DoubleHolder(56.789);

    int returned = object.passSimple(a_byte, 2, a_short, a_string, a_double);

    System.out.println("  Returned value " + returned);
    System.out.println("  Returned parameters: ");
    System.out.println("  octet " + a_byte.value);
    System.out.println("  short " + a_short.value);
    System.out.println("  string '" + a_string.value+"'");
    System.out.println("  double " + a_double.value);
  }

  /**
   * Test passing the string array, flexible size.
   */
  public void testStringArray()
  {
    System.out.println("***** Pass string array.");

    String[] x = new String[] { "one", "two" };

    // The array is passed as CORBA sequence, variable size is supported.
    String[] y = object.passStrings(x);

    for (int i = 0; i < y.length; i++)
      {
        System.out.println("  Passed " + x [ i ] + ", returned: " + y [ i ]);
      }
  }

  /**
   * Test passing the structures.
   */
  public void testStructure()
  {
    System.out.println("***** Pass structure");

    StructureToPass arg = new StructureToPass();
    arg.a = "A";
    arg.b = "B";

    StructureToReturn r = object.passStructure(arg);

    System.out.println("  Fields of the returned structure:");

    System.out.println("  c: " + r.c);
    System.out.println("  n: " + r.n);

    // The field r.arra is declared as the fixed size CORBA array.
    System.out.println("  r[0]: " + r.arra [ 0 ]);
    System.out.println("  r[1]: " + r.arra [ 1 ]);
    System.out.println("  r[3]: " + r.arra [ 2 ]);
  }

  /**
   * Test catching the system exception, thrown on the remote side.
   */
  public void testSystemException()
  {
    System.out.println("**** Test system exception:");
    try
      {
        // Negative parameter = system exception.
        object.throwException(-55);
      }
    catch (BAD_OPERATION ex)
      {
        System.out.println("  The expected BAD_OPERATION, minor code " +
                           ex.minor + ", has been thrown on remote side."
                          );
      }
    catch (UserException uex)
      {
        throw new InternalError();
      }
  }

  /**
   * Test passing the tree structure. Any shape of the tree is
   * supported without rewriting the code.
   */
  public void testTree()
  {
    // Manually create the tree of nodes:
    // Root
    //  +-- a
    //  |
    //  +-- b
    //      +-- ba
    //      |   |
    //      |   +-- bac
    //      |
    //      +-- bb
    System.out.println("***** Pass and return the tree.");

    TreeNode n = nod("Root");

    n.children = new TreeNode[] { nod("a"), nod("b") };
    n.children [ 1 ].children = new TreeNode[] { nod("ba"), nod("bb") };
    n.children [ 1 ].children [ 0 ].children = new TreeNode[] { nod("bac") };

    TreeNodeHolder nh = new TreeNodeHolder(n);

    // The server should add '++' to each TreeNode name.
    object.passTree(nh);

    // Convert the returned tree to some strig representation.
    StringBuilder img = new StringBuilder();
    getImage(img, nh.value);

    System.out.println("Returned tree: " + img.toString());
  }

  /**
   * Test catching the user exception, thrown on the remote side.
   */
  public void testUserException()
  {
    System.out.println("**** Test user exception:");
    try
      {
        // The user exception contains one user-defined field that will
        // be initialised to the passed parameter.
        object.throwException(123);
        throw new InternalError();
      }
    catch (WeThrowThisException uex)
      {
        System.out.println("  The user exception with field " + uex.ourField +
                           ", has been thrown on remote side."
                          );
      }
  }

  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings.
   */
  public void testWideNarrowStrings()
  {
    System.out.println("**** Test 8 bit and 16 bit char strings");

    String r = object.passCharacters("wide string", "narrow string");
    System.out.println("  returned: '" + r + "'");
  }

  /**
   * Get the string representation of the passed tree.
   * @param b the string buffer to accumulate the representation.
   * @param n the tree (root TreeNode).
   */
  private void getImage(StringBuilder b, TreeNode n)
  {
    b.append(n.name);
    b.append(": (");

    for (int i = 0; i < n.children.length; i++)
      {
        getImage(b, n.children [ i ]);
        b.append(' ');
      }
    b.append(") ");
  }

  /**
   * Create a TreeNode with the given header.
   *
   * @param hdr the TreeNode header.
   * @return the created TreeNode.
   */
  private TreeNode nod(String hdr)
  {
    TreeNode n = new TreeNode();
    n.children = new TreeNode[ 0 ];
    n.name = hdr;

    return n;
  }
}
