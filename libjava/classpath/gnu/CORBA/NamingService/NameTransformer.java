/* NameTransformer.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package gnu.CORBA.NamingService;

import gnu.java.lang.CPStringBuilder;

import org.omg.CORBA.IntHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextPackage.InvalidName;

import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * This class converts between string and array representations of the
 * multi component object names.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class NameTransformer
{
  /**
   * A string, indicating the escape character.
   */
  public static final String ESCAPE = "\\";

  /**
   * Convert the string name representation into the array name
   * representation. See {@link #toString(NameComponent)} for the
   * description of this format.
   *
   * @param a_name the string form of the name.
   *
   * @return the array form of the name.
   *
   * @throws InvalidName if the name cannot be parsed.
   */
  public NameComponent[] toName(String a_name)
                         throws InvalidName
  {
    ArrayList components = new ArrayList();
    StringTokenizer st = new StringTokenizer(a_name, "./\\", true);

    // Create the buffer array, reserving the last element for null.
    String[] n = new String[ st.countTokens() + 1 ];

    int pp = 0;
    while (st.hasMoreTokens())
      n [ pp++ ] = st.nextToken();

    IntHolder p = new IntHolder();

    NameComponent node = readNode(p, n);

    while (node != null)
      {
        components.add(node);
        node = readNode(p, n);
      }

    NameComponent[] name = new NameComponent[ components.size() ];
    for (int i = 0; i < name.length; i++)
      {
        name [ i ] = (NameComponent) components.get(i);
      }

    NameValidator.check(name);

    return name;
  }

  /**
   * Converts the name into its string representation, as defined in
   * the specification CORBA naming service.
   *
   * A string representation for the name consists of the name components,
   * separated by a slash '/' character (for example, 'a/b/c'). If the
   * {@link NameComponent#kind} field is  not empty, it is given after
   * period ('.'), for example 'a.b/c.d/.' .
   * The period alone represents node where part where both
   * {@link NameComponent#kind} and {@link NameComponent#id} are empty strings.
   *
   * If slash or dot are part of the name, they are escaped by backslash ('\').
   * If the backslash itself is part of the name, it is doubled.
   *
   * @param a_name a name to convert.
   * @return a string representation.
   */
  public String toString(NameComponent[] a_name)
                  throws InvalidName
  {
    NameValidator.check(a_name);

    CPStringBuilder b = new CPStringBuilder();

    NameComponent n;

    for (int ni = 0; ni < a_name.length; ni++)
      {
        n = a_name [ ni ];
        appEscaping(b, n.id);
        if (n.kind.length() > 0)
          {
            b.append('.');
            appEscaping(b, n.kind);
          }

        if (ni < a_name.length - 1)
          b.append('/');
      }
    return b.toString();
  }

  /**
   * Append the contents of the string to this
   * string buffer, inserting the escape sequences, where required.
   *
   * @param b a buffer to append the contents to.
   * @param s a string to append.
   */
  private void appEscaping(CPStringBuilder b, String s)
  {
    char c;
    for (int i = 0; i < s.length(); i++)
      {
        c = s.charAt(i);
        switch (c)
          {
            case '.' :
            case '/' :
            case '\\' :
              b.append('\\');
              b.append(c);
              break;

            default :
              b.append(c);
              break;
          }
      }
  }

  /**
   * Assert the end of the current name component.
   */
  private void assertEndOfNode(IntHolder p, String[] t)
                        throws InvalidName
  {
    if (t [ p.value ] != null)
      if (!t [ p.value ].equals("/"))
        throw new InvalidName("End of node expected at token " + p.value);
  }

  /**
   * Read the named component node. After reading the current positon
   * advances to the beginning of the next node in an array.
   *
   * @param p the current position being wrapped inside the passed
   * IntHolder.
   *
   * @param t the text buffer.
   *
   * @return the created node.
   */
  private NameComponent readNode(IntHolder p, String[] t)
                          throws InvalidName
  {
    // End of stream has been reached.
    if (t [ p.value ] == null)
      return null;

    NameComponent n = new NameComponent();

    if (t [ p.value ].equals("."))
      {
        // The 'id' is missing, but the 'kind' may follow.
        n.id = "";
        p.value++;
        n.kind = readPart(p, t);
        assertEndOfNode(p, t);
        if (t [ p.value ] != null)
          p.value++;
      }
    else if (t [ p.value ].equals("/"))
      {
        // This is not allowed here and may happen only
        // on two subsequent slashes.
        throw new InvalidName("Unexpected '/' token " + p.value);
      }
    else
      {
        n.id = readPart(p, t);

        // If some chars follow the id.
        if (t [ p.value ] != null)
          {
            // Dot means that the kind part follows
            if (t [ p.value ].equals("."))
              {
                p.value++;
                n.kind = readPart(p, t);
                assertEndOfNode(p, t);
                if (t [ p.value ] != null)
                  p.value++;
              }

            // The next name component follows - advance to
            // the beginning of the next name component.
            else if (t [ p.value ].equals("/"))
              {
                n.kind = "";
                p.value++;
              }
            else
              throw new InvalidName("Unexpected '" + t [ p.value ] +
                                       "' at token " + p.value
                                      );
          }
        else

          // Id, and then end of sequence.
          n.kind = "";
      }

    return n;
  }

  /**
   * Read the name part (id or kind).
   *
   * @param p the current position. After reading, advances
   * to the beginning of the next name fragment.
   *
   * @param t the string buffer.
   *
   * @return the name part with resolved escape sequences.
   */
  private String readPart(IntHolder p, String[] t)
  {
    CPStringBuilder part = new CPStringBuilder();

    while (t [ p.value ] != null && !t [ p.value ].equals(".") &&
           !t [ p.value ].equals("/")
          )
      {
        if (t [ p.value ].equals(ESCAPE))
          {
            p.value++;
            part.append(t [ p.value ]);
          }
        else
          part.append(t [ p.value ]);

        p.value++;
      }

    return part.toString();
  }

  public static void main(String[] args)
  {
    NameComponent a = new NameComponent("a", "ak");
    NameComponent b = new NameComponent("b/z", "b.k");
    NameComponent c = new NameComponent("c", "");

    NameTransformer sn = new NameTransformer();

    try
      {
        String s = sn.toString(new NameComponent[] { a, b, c });
        System.out.println(s);

        //NameComponent[] k = toName("a.k/b.k2/c/d/.");
        //NameComponent[] k = toName("a.bc/.b/c.x");

        NameComponent[] k = sn.toName(s);
        System.out.println("ToString");

        for (int i = 0; i < k.length; i++)
          {
            System.out.println(k [ i ].id + ":" + k [ i ].kind);
          }
      }
    catch (InvalidName ex)
      {
        ex.printStackTrace();
      }
  }

}
