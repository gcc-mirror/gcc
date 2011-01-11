/* Demo.java --
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

package gnu.classpath.examples.html;

import gnu.javax.swing.text.html.parser.HTML_401F;

import gnu.xml.dom.html2.DomHTMLParser;

import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.html2.HTMLDocument;

/**
 * This example demonstrates how to parse HTML input into
 * org.w3c.dom.html2 document model.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Demo
{
  /**
   * The sample HTML to parse.
   */
  static String input = "<!--2-->a<b iD=1>x</b>y<b><i>c</b>d</i>e";

  public static void main(String[] args)
  {
    try
      {
        // Create a parser, using our DTD.
        DomHTMLParser p = new DomHTMLParser(HTML_401F.getInstance());
        HTMLDocument d = p.parseDocument(new StringReader(input));

        // Print the input HTML.
        System.out.println(input);

        // Print the parsed data structure.
        print(System.out, d, 0);
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }
  }

  /**
   * Print the parsed data structure.
   *
   * @param stream the output
   * @param node the node
   * @param ident the identation
   */
  static void print(PrintStream stream, Node node, int ident)
  {
    if (node == null)
      return;

    StringBuilder tab = new StringBuilder();
    stream.println();
    for (int i = 0; i < ident; i++)
      {
        tab.append(' ');
      }

    stream.print(tab + node.getNodeName());
    if (node.getNodeValue() != null)
      {
        stream.println();
        stream.print(tab + " '" + node.getNodeValue() + "'");
      }

    NamedNodeMap attributes = node.getAttributes();
    if (attributes != null && attributes.getLength() != 0)
      {
        stream.print(' ');
        for (int i = 0; i < attributes.getLength(); i++)
          {
            Node a = attributes.item(i);
            stream.print(a.getNodeName() + "='" + a.getNodeValue() + "'");
          }
      }

    ident += 2;

    NodeList childs = node.getChildNodes();
    if (childs != null)
      for (int i = 0; i < childs.getLength(); i++)
        {
          print(stream, childs.item(i), ident);
        }
  }
}
