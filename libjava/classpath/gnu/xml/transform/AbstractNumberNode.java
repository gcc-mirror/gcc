/* AbstractNumberNode.java -- 
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.transform;

import gnu.java.lang.CPStringBuilder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import gnu.xml.xpath.Expr;

/**
 * A template node representing the XSL <code>number</code> instruction.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
abstract class AbstractNumberNode
  extends TemplateNode
{

  static final int ALPHABETIC = 0;
  static final int TRADITIONAL = 1;

  final TemplateNode format;
  final String lang;
  final int letterValue;
  final String groupingSeparator;
  final int groupingSize;

  AbstractNumberNode(TemplateNode format, String lang,
                     int letterValue, String groupingSeparator,
                     int groupingSize)
  {
    this.format = format;
    this.lang = lang;
    this.letterValue = letterValue;
    this.groupingSeparator = groupingSeparator;
    this.groupingSize = groupingSize;
  }

  void doApply(Stylesheet stylesheet, QName mode,
               Node context, int pos, int len,
               Node parent, Node nextSibling)
    throws TransformerException
  {
    Document doc = (parent instanceof Document) ? (Document) parent :
      parent.getOwnerDocument();
    DocumentFragment fragment = doc.createDocumentFragment();
    format.apply(stylesheet, mode, context, pos, len, fragment, null);
    String f = Expr._string(context, Collections.singleton(fragment));
    String value = format(f, compute(stylesheet, context, pos, len));
    Text text = doc.createTextNode(value);
    if (nextSibling != null)
      {
        parent.insertBefore(text, nextSibling);
      }
    else
      {
        parent.appendChild(text);
      }
    // xsl:number doesn't process children
    if (next != null)
      {
        next.apply(stylesheet, mode, 
                   context, pos, len,
                   parent, nextSibling);
      }
  }

  String format(String format, int[] number)
  {
    if (number.length == 0)
      {
        return "";
      }
    int start = 0, end = 0, len = format.length(); // region of format
    // Tokenize
    List tokens = new ArrayList((number.length * 2) + 1);
    List types = new ArrayList(tokens.size());
    while (end < len)
      {
        while (end < len && !isAlphanumeric(format.charAt(end)))
          {
            end++;
          }
        if (end > start)
          {
            tokens.add(format.substring(start, end));
            types.add(Boolean.FALSE);
          }
        start = end;
        while (end < len && isAlphanumeric(format.charAt(end)))
          {
            end++;
          }
        if (end > start)
          {
            tokens.add(format.substring(start, end));
            types.add(Boolean.TRUE);
          }
        start = end;
      }
    // Process tokens
    CPStringBuilder buf = new CPStringBuilder();
    len = tokens.size();
    int pos = 0;
    for (int i = 0; i < len; i++)
      {
        String token = (i < 0) ? "." : (String) tokens.get(i);
        boolean alpha = (i < 0) ? true : 
          ((Boolean) types.get(i)).booleanValue();
        if (!alpha)
          {
            buf.append(token);
          }
        else
          {
            if (pos < number.length)
              {
                format(buf, number[pos++], token);
                if (((i + 1 == len) || (i + 2 == len)) &&
                    (pos < number.length))
                  {
                    // More numbers than tokens, reuse last token
                    i -= 2;
                  }
              }
            if (pos == number.length && i < (len - 2))
              {
                // No more numbers. Skip to the end...
                i = len - 2;
                if (((Boolean) types.get(i + 1)).booleanValue())
                  {
                    // number formatting token, ignore
                    i++;
                  }
              }
          }
      }
    //System.err.println("format: '"+format+"' "+asList(number)+" = '"+buf.toString()+"'");
    return buf.toString();
  }

  /*List asList(int[] number)
    {
      List l = new ArrayList();
      for (int i = 0; i < number.length; i++)
        l.add(new Integer(number[i]));
      return l;
    }*/

  void format(CPStringBuilder buf, int number, String formatToken)
  {
    int len = formatToken.length();
    char c = formatToken.charAt(len - 1);
    if (Character.digit(c, 10) == 1)
      {
        // Check preceding characters
        for (int i = len - 2; i >= 0; i--)
          {
            if (formatToken.charAt(i) != (c - 1))
              {
                format(buf, number, "1");
                return;
              }
          }
        // Decimal representation
        String val = Integer.toString(number);
        for (int d = len - val.length(); d > 0; d--)
          {
            buf.append('0');
          }
        buf.append(val);
      }
    else if ("A".equals(formatToken))
      {
        buf.append(alphabetic('@', number));
      }
    else if ("a".equals(formatToken))
      {
        buf.append(alphabetic('`', number));
      }
    else if ("i".equals(formatToken))
      {
        buf.append(roman(false, number));
      }
    else if ("I".equals(formatToken))
      {
        buf.append(roman(true, number));
      }
    else
      {
        // Unknown numbering sequence
        format(buf, number, "1");
      }
  }

  static final boolean isAlphanumeric(char c)
  {
    switch (Character.getType(c))
      {
      case Character.DECIMAL_DIGIT_NUMBER: // Nd
      case Character.LETTER_NUMBER: // Nl
      case Character.OTHER_NUMBER: // No
      case Character.UPPERCASE_LETTER: // Lu
      case Character.LOWERCASE_LETTER: // Ll
      case Character.TITLECASE_LETTER: // Lt
      case Character.MODIFIER_LETTER: // Lm
      case Character.OTHER_LETTER: // Lo
        return true;
      default:
        return false;
      }
  }

  static final String alphabetic(char offset, int number)
  {
    CPStringBuilder buf = new CPStringBuilder();
    while (number > 0)
      {
        int r = number % 26;
        number = number / 26;
        buf.insert(0, (char) (offset + r));
      }
    return buf.toString();
  }

  static final int[] roman_numbers = {1, 5, 10, 50, 100, 500, 1000};
  static final char[] roman_chars = {'i', 'v', 'x', 'l', 'c', 'd', 'm'};

  static final String roman(boolean upper, int number)
  {
    CPStringBuilder buf = new CPStringBuilder();
    for (int pos = roman_numbers.length - 1; pos >= 0; pos -= 2)
      {
        int f = number / roman_numbers[pos];
        if (f != 0)
          {
            number = number % (f * roman_numbers[pos]);
          }
        if (f > 4 && f < 9)
          {
            buf.append(roman_chars[pos + 1]);
            f -= 5;
          }
        if (f == 4)
          {
            buf.append(roman_chars[pos]);
            buf.append(roman_chars[pos + 1]);
          }
        else if (f == 9)
          {
            buf.append(roman_chars[pos]);
            buf.append(roman_chars[pos + 2]);
          }
        else
          {
            for (; f > 0; f--)
              {
                buf.append(roman_chars[pos]);
              }
          }
      }
    return upper ? buf.toString().toUpperCase() : buf.toString();
  }
  
  abstract int[] compute(Stylesheet stylesheet, Node context, int pos, int len)
    throws TransformerException;

  public boolean references(QName var)
  {
    if (format.references(var))
      {
        return true;
      }
    return super.references(var);
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder("number");
    buf.append('[');
    buf.append("format=");
    buf.append(format);
    buf.append(']');
    return buf.toString();
  }

}
