/* XPathTokenizer.java -- 
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package gnu.xml.xpath;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Map;
import java.util.TreeMap;

/*import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;*/

/**
 * XPath 1.0 expression tokenizer.
 * 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XPathTokenizer
implements XPathParser.yyInput
//implements TokenStream
{

  static class XPathToken
  //extends Token
  {

    int type;
    String val;

    XPathToken (int type)
    {
      this (type, null);
    }

    XPathToken (int type, String val)
    {
      //super (type);
      this.type = type;
      this.val = val;
    }

    public String getText ()
    {
      return val;
    }

    public String toString ()
    {
      return val;
    }
    
  }

  static final Map keywords = new TreeMap ();
  static
  {
    keywords.put ("ancestor", new Integer (XPathParser.ANCESTOR));
    keywords.put ("ancestor-or-self", new Integer (XPathParser.ANCESTOR_OR_SELF));
    keywords.put ("attribute", new Integer (XPathParser.ATTRIBUTE));
    keywords.put ("child", new Integer (XPathParser.CHILD));
    keywords.put ("descendant", new Integer (XPathParser.DESCENDANT));
    keywords.put ("descendant-or-self", new Integer (XPathParser.DESCENDANT_OR_SELF));
    keywords.put ("following", new Integer (XPathParser.FOLLOWING));
    keywords.put ("following-sibling", new Integer (XPathParser.FOLLOWING_SIBLING));
    keywords.put ("namespace", new Integer (XPathParser.NAMESPACE));
    keywords.put ("parent", new Integer (XPathParser.PARENT));
    keywords.put ("preceding", new Integer (XPathParser.PRECEDING));
    keywords.put ("preceding-sibling", new Integer (XPathParser.PRECEDING_SIBLING));
    keywords.put ("self", new Integer (XPathParser.SELF));
    keywords.put ("div", new Integer (XPathParser.DIV));
    keywords.put ("mod", new Integer (XPathParser.MOD));
    keywords.put ("or", new Integer (XPathParser.OR));
    keywords.put ("and", new Integer (XPathParser.AND));
    keywords.put ("comment", new Integer (XPathParser.COMMENT));
    keywords.put ("processing-instruction", new Integer (XPathParser.PROCESSING_INSTRUCTION));
    keywords.put ("text", new Integer (XPathParser.TEXT));
    keywords.put ("node", new Integer (XPathParser.NODE));
  }

  Reader in;
  XPathToken token;
  XPathToken lastToken;

  public XPathTokenizer (String expr)
  {
    this (new StringReader (expr));
  }

  XPathTokenizer (Reader in)
  {
    this.in = in.markSupported () ? in : new BufferedReader (in);
  }

  /* Begin ANTLR specific *

  public Token nextToken ()
    throws TokenStreamException
  {
    try
      {
        if (!advance ())
          {
            throw new TokenStreamException ("eof");
          }
        token ();
        return token;
      }
    catch (IOException e)
      {
        throw new TokenStreamIOException (e);
      }
  }
  
  * End ANTLR specific */

  public boolean advance ()
    throws IOException
  {
    lastToken = token;
    int c = in.read ();
    switch (c)
      {
      case -1: // eof
        return false;
      case 0x20:
      case 0x09:
      case 0x0d:
      case 0x0a: // skip whitespace
        return advance ();
      case 0x22: // "
      case 0x27: // '
        token = consume_literal (c);
        break;
      case 0x28: // (
        token = new XPathToken (XPathParser.LP);
        break;
      case 0x29: // )
        token = new XPathToken (XPathParser.RP);
        break;
      case 0x5b: // [
        token = new XPathToken (XPathParser.LB);
        break;
      case 0x5d: // ]
        token = new XPathToken (XPathParser.RB);
        break;
      case 0x2c: // ,
        token = new XPathToken (XPathParser.COMMA);
        break;
      case 0x7c: // |
        token = new XPathToken (XPathParser.PIPE);
        break;
      case 0x2f: // /
        in.mark (1);
        int d1 = in.read ();
        if (d1 == 0x2f)
          {
            token = new XPathToken (XPathParser.DOUBLE_SLASH);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.SLASH);
          }
        break;
      case 0x3d: // =
        token = new XPathToken (XPathParser.EQ);
        break;
      case 0x21: // !
        in.mark (1);
        int d2 = in.read ();
        if (d2 == 0x3d) // =
          {
            token = new XPathToken (XPathParser.NE);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.yyErrorCode);
          }
        break;
      case 0x3e: // >
        in.mark (1);
        int d3 = in.read ();
        if (d3 == 0x3d) // =
          {
            token = new XPathToken (XPathParser.GTE);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.GT);
          }
        break;
      case 0x3c: // <
        in.mark (1);
        int d4 = in.read ();
        if (d4 == 0x3d) // =
          {
            token = new XPathToken (XPathParser.LTE);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.LT);
          }
        break;
      case 0x2b: // +
        token = new XPathToken (XPathParser.PLUS);
        break;
      case 0x2d: // -
        token = new XPathToken (XPathParser.MINUS);
        break;
      case 0x40: // @
        token = new XPathToken (XPathParser.AT);
        break;
      case 0x2a: // *
        token = new XPathToken (XPathParser.STAR);
        break;
      case 0x24: // $
        token = new XPathToken (XPathParser.DOLLAR);
        break;
      case 0x3a: // :
        in.mark (1);
        int d5 = in.read ();
        if (d5 == 0x3a)
          {
            token = new XPathToken (XPathParser.DOUBLE_COLON);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.COLON);
          }
        break;
      case 0x2e: // .
        in.mark (1);
        int d6 = in.read ();
        if (d6 == 0x2e)
          {
            token = new XPathToken (XPathParser.DOUBLE_DOT);
          }
        else
          {
            in.reset ();
            token = new XPathToken (XPathParser.DOT);
          }
        break;
      default:
        if (c >= 0x30 && c <= 0x39)
          {
            token = consume_digits (c);
          }
        else if (c == 0x5f || Character.isLetter ((char) c))
          {
            token = consume_name (c);
          }
        else
          {
            token = new XPathToken (XPathParser.yyErrorCode);
          }
      }
    return true;
  }

  public int token ()
  {
    return token.type;
  }

  public Object value ()
  {
    return token.val;
  }

  XPathToken consume_literal (int delimiter)
    throws IOException
  {
    StringBuffer buf = new StringBuffer ();
    while (true)
      {
        int c = in.read ();
        if (c == -1)
          {
            return new XPathToken (XPathParser.yyErrorCode);
          }
        else if (c == delimiter)
          {
            return new XPathToken (XPathParser.LITERAL, buf.toString ());
          }
        else
          {
            buf.append ((char) c);
          }
      }
  }

  XPathToken consume_digits (int c)
    throws IOException
  {
    StringBuffer buf = new StringBuffer ();
    buf.append ((char) c);
    while (true)
      {
        in.mark (1);
        c = in.read ();
        if (c >= 0x30 && c <= 0x39)
          {
            buf.append ((char) c);
          }
        else
          {
            in.reset ();
            return new XPathToken (XPathParser.DIGITS, buf.toString ());
          }
      }
  }

  XPathToken consume_name (int c)
    throws IOException
  {
    StringBuffer buf = new StringBuffer ();
    buf.append ((char) c);
    while (true)
      {
        in.mark (1);
        c = in.read ();
        if (isNameChar (c))
          {
            buf.append ((char) c);
          }
        else
          {
            in.reset ();
            String name = buf.toString ();
            Integer keyword = (Integer) keywords.get (name);
            if (keyword == null)
              {
                return new XPathToken (XPathParser.NAME, name);
              }
            else
              {
                int val = keyword.intValue ();
                switch (val)
                  {
                  case XPathParser.NODE:
                  case XPathParser.COMMENT:
                  case XPathParser.TEXT:
                  case XPathParser.PROCESSING_INSTRUCTION:
                    // Consume subsequent (
                    in.mark (1);
                    do
                      {
                        c = in.read ();
                      }
                    while (c == 0x20 || c == 0x09);
                    if (c != 0x28)
                      {
                        in.reset ();
                        return new XPathToken (XPathParser.NAME, name);
                      }
                    break;
                  case XPathParser.CHILD:
                  case XPathParser.PARENT:
                  case XPathParser.SELF:
                  case XPathParser.DESCENDANT:
                  case XPathParser.ANCESTOR:
                  case XPathParser.DESCENDANT_OR_SELF:
                  case XPathParser.ANCESTOR_OR_SELF:
                  case XPathParser.ATTRIBUTE:
                  case XPathParser.NAMESPACE:
                  case XPathParser.FOLLOWING:
                  case XPathParser.FOLLOWING_SIBLING:
                  case XPathParser.PRECEDING:
                  case XPathParser.PRECEDING_SIBLING:
                    // Check that this is an axis specifier
                    in.mark(1);
                    do
                      {
                        c = in.read();
                      }
                    while (c == 0x20 || c == 0x09);
                    if (c == 0x3a)
                      {
                        c = in.read();
                        if (c == 0x3a)
                          {
                            in.reset();
                            return new XPathToken(val);
                          }
                      }
                    in.reset();
                    return new XPathToken(XPathParser.NAME, name);
                  case XPathParser.DIV:
                  case XPathParser.MOD:
                    // May be a name
                    if (lastToken == null)
                      {
                        return new XPathToken(XPathParser.NAME, name);
                      }
                    switch (lastToken.type)
                      {
                      case XPathParser.LP:
                      case XPathParser.LB:
                      case XPathParser.COMMA:
                      case XPathParser.PIPE:
                      case XPathParser.EQ:
                      case XPathParser.NE:
                      case XPathParser.GT:
                      case XPathParser.LT:
                      case XPathParser.GTE:
                      case XPathParser.LTE:
                      case XPathParser.PLUS:
                      case XPathParser.MINUS:
                      case XPathParser.STAR:
                      case XPathParser.AT:
                      case XPathParser.DOLLAR:
                      case XPathParser.COLON:
                      case XPathParser.DOUBLE_COLON:
                      case XPathParser.DIV:
                      case XPathParser.MOD:
                      case XPathParser.OR:
                      case XPathParser.AND:
                      case XPathParser.SLASH:
                        return new XPathToken(XPathParser.NAME, name);
                      }
                    break;
                  }
                return new XPathToken (val);
              }
          }
      }
  }

  boolean isNameChar (int c)
  {
    /* Name */
    return (c == 0x5f
            || c == 0x2d
            || c == 0x2e
            || (c >= 0x30 && c <= 0x39)
            /* CombiningChar */
            || (c >= 0x0300 && c <= 0x0345)
            || (c >= 0x0360 && c <= 0x0361)
            || (c >= 0x0483 && c <= 0x0486)
            || (c >= 0x0591 && c <= 0x05A1)
            || (c >= 0x05A3 && c <= 0x05B9)
            || (c >= 0x05BB && c <= 0x05BD)
            || c == 0x05BF
            || (c >= 0x05C1 && c <= 0x05C2)
            || c == 0x05C4
            || (c >= 0x064B && c <= 0x0652)
            || c == 0x0670
            || (c >= 0x06D6 && c <= 0x06DC)
            || (c >= 0x06DD && c <= 0x06DF)
            || (c >= 0x06E0 && c <= 0x06E4)
            || (c >= 0x06E7 && c <= 0x06E8)
            || (c >= 0x06EA && c <= 0x06ED)
            || (c >= 0x0901 && c <= 0x0903)
            || c == 0x093C
            || (c >= 0x093E && c <= 0x094C)
            || c == 0x094D
            || (c >= 0x0951 && c <= 0x0954)
            || (c >= 0x0962 && c <= 0x0963)
            || (c >= 0x0981 && c <= 0x0983)
            || c == 0x09BC
            || c == 0x09BE
            || c == 0x09BF
            || (c >= 0x09C0 && c <= 0x09C4)
            || (c >= 0x09C7 && c <= 0x09C8)
            || (c >= 0x09CB && c <= 0x09CD)
            || c == 0x09D7
            || (c >= 0x09E2 && c <= 0x09E3)
            || c == 0x0A02
            || c == 0x0A3C
            || c == 0x0A3E
            || c == 0x0A3F
            || (c >= 0x0A40 && c <= 0x0A42)
            || (c >= 0x0A47 && c <= 0x0A48)
            || (c >= 0x0A4B && c <= 0x0A4D)
            || (c >= 0x0A70 && c <= 0x0A71)
            || (c >= 0x0A81 && c <= 0x0A83)
            || c == 0x0ABC
            || (c >= 0x0ABE && c <= 0x0AC5)
            || (c >= 0x0AC7 && c <= 0x0AC9)
            || (c >= 0x0ACB && c <= 0x0ACD)
            || (c >= 0x0B01 && c <= 0x0B03)
            || c == 0x0B3C
            || (c >= 0x0B3E && c <= 0x0B43)
            || (c >= 0x0B47 && c <= 0x0B48)
            || (c >= 0x0B4B && c <= 0x0B4D)
            || (c >= 0x0B56 && c <= 0x0B57)
            || (c >= 0x0B82 && c <= 0x0B83)
            || (c >= 0x0BBE && c <= 0x0BC2)
            || (c >= 0x0BC6 && c <= 0x0BC8)
            || (c >= 0x0BCA && c <= 0x0BCD)
            || c == 0x0BD7
            || (c >= 0x0C01 && c <= 0x0C03)
            || (c >= 0x0C3E && c <= 0x0C44)
            || (c >= 0x0C46 && c <= 0x0C48)
            || (c >= 0x0C4A && c <= 0x0C4D)
            || (c >= 0x0C55 && c <= 0x0C56)
            || (c >= 0x0C82 && c <= 0x0C83)
            || (c >= 0x0CBE && c <= 0x0CC4)
            || (c >= 0x0CC6 && c <= 0x0CC8)
            || (c >= 0x0CCA && c <= 0x0CCD)
            || (c >= 0x0CD5 && c <= 0x0CD6)
            || (c >= 0x0D02 && c <= 0x0D03)
            || (c >= 0x0D3E && c <= 0x0D43)
            || (c >= 0x0D46 && c <= 0x0D48)
            || (c >= 0x0D4A && c <= 0x0D4D)
            || c == 0x0D57
            || c == 0x0E31
            || (c >= 0x0E34 && c <= 0x0E3A)
            || (c >= 0x0E47 && c <= 0x0E4E)
            || c == 0x0EB1
            || (c >= 0x0EB4 && c <= 0x0EB9)
            || (c >= 0x0EBB && c <= 0x0EBC)
            || (c >= 0x0EC8 && c <= 0x0ECD)
            || (c >= 0x0F18 && c <= 0x0F19)
            || c == 0x0F35
            || c == 0x0F37
            || c == 0x0F39
            || c == 0x0F3E
            || c == 0x0F3F
            || (c >= 0x0F71 && c <= 0x0F84)
            || (c >= 0x0F86 && c <= 0x0F8B)
            || (c >= 0x0F90 && c <= 0x0F95)
            || c == 0x0F97
            || (c >= 0x0F99 && c <= 0x0FAD)
            || (c >= 0x0FB1 && c <= 0x0FB7)
            || c == 0x0FB9
            || (c >= 0x20D0 && c <= 0x20DC)
            || c == 0x20E1
            || (c >= 0x302A && c <= 0x302F)
            || c == 0x3099
            || c == 0x309A
            /* Extender */
            || c == 0x00B7
            || c == 0x02D0
            || c == 0x02D1
            || c == 0x0387
            || c == 0x0640
            || c == 0x0E46
            || c == 0x0EC6
            || c == 0x3005
            || (c >= 0x3031 && c <= 0x3035)
            || (c >= 0x309D && c <= 0x309E)
            || (c >= 0x30FC && c <= 0x30FE)
            /* Name */
            || Character.isLetter ((char) c));
  }

}
