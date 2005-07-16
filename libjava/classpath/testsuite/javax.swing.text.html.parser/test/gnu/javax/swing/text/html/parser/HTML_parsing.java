/* HTML_parsing.java --
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


package test.gnu.javax.swing.text.html.parser;


/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HTML_parsing
  extends TestCase
{
  /**
   * This is used for profiling.
   */
  public static void main(String[] args)
  {
    long t = System.currentTimeMillis();
    try
      {
        HTML_parsing p = new HTML_parsing();
        for (int i = 0; i < 2000; i++)
          {
            p.testHTMLParsing();
            if (i % 10 == 0)
              System.out.print('.');
          }
      }
    catch (Exception ex)
      {
      }
    System.out.println("TIME " + (System.currentTimeMillis() - t));
  }

  public void testHTMLParsing()
                       throws Exception
  {
    Parser_Test v = new Parser_Test();
    v.hideImplied = false;

    // Test  subsequent tags.
    v.verify("<b><i><u>text</b><i></u>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><b><i><u>'text'</b><i></u></i></i></body></html>"
            );

    // Test entities.
    v.verify("hex: &#x55; eqdec: &#61; ampnamed: &amp;",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'>'hex: U eqdec: = ampnamed: &'</body></html>"
            );

    // Test comments.
    v.verify("<html><head></head><body><!--a-->< !--b--><! --c--><!-- d--><!--e --><!--f-- ><!--g--><!---h---><!-- i --><!--- j ---><!-- -- --> <b> <!---------->",
             "<html><head></head><body>{a}{b}{c}{ d}{e }{f}{g}{-h-}{ i }{- j -}{ -- }<b>{------}</b></body></html>"
            );

    // Test unclosed tags.
    v.verify("<hr id = 1 class = c<hr id=2>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><hr class='c' id='1'/><hr id='2'/></body></html>"
            );

    // Test errors and unclosed tags.
    v.verify("<b#r><hr id = 1 # class = c<hr id=2>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><b><hr id='1'/>'# class = c'<hr id='2'/></b></body></html>"
            );

    // Test script.
    v.verify("<hr id=1><script a=b c=d><hr id=1></script><hr id=1>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><hr id='1'/><script a='b' c='d'>'<hr id=1>'</script><hr id='1'/></body></html>"
            );

    // Test valid attributes.
    v.verify("<hr id='i' title=\"tit\" class=cl><hr><hr id = 2>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><hr class='cl' id='i' title='tit'/><hr/><hr id='2'/></body></html>"
            );

    // Test unknown attribute without value.
    v.verify("<hr audrius title=\"tit\">",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><hr audrius='#DEFAULT' title='tit'/></body></html>"
            );

    // Test known attributes witout value.
    v.verify("<option id=a selected><option id=b selected = selected class=cC><input checked>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><option id='a' selected='selected'></option></body><body _implied_='true'><option class='cC' id='b' selected='selected'></option><input checked='checked'/></body></html>"
            );

    // Test table content model.
    v.verify("<table>a</table>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr _implied_='true'><td _implied_='true'>'a'</td></tr></tbody></table></body></html>"
            );

    // Test table content model.
    v.verify("<table><caption>cap</caption>a</table>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><caption>'cap'</caption><tbody _implied_='true'><tr _implied_='true'><td _implied_='true'>'a'</td></tr></tbody></table></body></html>"
            );

    // Test typical table.
    v.verify("<table><tr><td>x</td><td>y</td><td>z</td></table>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr><td>'x'</td><td>'y'</td><td>'z'</td></tr></tbody></table></body></html>"
            );

    // Test nested table.
    v.verify("<table><tr><td><table>nested</table>x</td><td>y</td><td>z</td></table>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr><td><table><tbody _implied_='true'><tr _implied_='true'><td _implied_='true'>'nested'</td></tr></tbody></table>'x'</td><td>'y'</td><td>'z'</td></tr></tbody></table></body></html>"
            );

    // Test simple nested list.
    v.verify("<ul><li>a</li><ul><li>na</li><li>nb</li></ul><li>b</li></ul>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><ul><li>'a'</li><ul><li>'na'</li><li>'nb'</li></ul><li>'b'</li></ul></body></html>"
            );

    // Test simple non-nested list.
    v.verify("<ul><li>a</li><li>na</li><li>nb</li><li>b</li></ul>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><ul><li>'a'</li><li>'na'</li><li>'nb'</li><li>'b'</li></ul></body></html>"
            );

    // Test list without closing tags (obsolete list form).
    v.verify("<ul><li>a<li>na<li>nb<li>b</ul>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><ul><li>'a'</li><li>'na'</li><li>'nb'</li><li>'b'</li></ul></body></html>"
            );

    // Test list without closing tags (obsolete list form).
    v.verify("<ul><li>a<ul><li>na<li>nb</ul><li>b</ul>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><ul><li>'a'<ul><li>'na'</li><li>'nb'</li></ul></li><li>'b'</li></ul></body></html>"
            );

    // Test Obsolete table.
    v.verify("<table><tr><td>a<td>b<td>c</tr>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr><td>'a'</td><td>'b'</td><td>'c'</td></tr></tbody></table></body></html>"
            );

    // Test html no head no body.
    v.verify("<html>text</html>",
             "<html><head _implied_='true'></head><body _implied_='true'>'text'</body></html>"
            );

    // Test head only.
    v.verify("<head></head>text",
             "<html _implied_='true'><head></head><body _implied_='true'>'text'</body></html>"
            );

    // Test head and body.
    v.verify("<head><title>ti</title></head><body>text",
             "<html _implied_='true'><head><title>'ti'</title></head><body>'text'</body></html>"
            );

    // Test title and text.
    v.verify("<title>title</title>text",
             "<html _implied_='true'><head _implied_='true'><title>'title'</title></head><body _implied_='true'>'text'</body></html>"
            );

    // Test html only.
    v.verify("<html>text</html>",
             "<html><head _implied_='true'></head><body _implied_='true'>'text'</body></html>"
            );

    // Test body only.
    v.verify("<body>text</body>",
             "<html _implied_='true'><head _implied_='true'></head><body>'text'</body></html>"
            );

    // Test head only.
    v.verify("<head></head>text",
             "<html _implied_='true'><head></head><body _implied_='true'>'text'</body></html>"
            );

    // Test obsolete table.
    v.verify("<table><tr><td>a</td><tr><td>a</td>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr><td>'a'</td></tr><tr><td>'a'</td></tr></tbody></table></body></html>"
            );

    // Test obsolete table.
    v.verify("<table><tr><td>a<td>b<tr><td>a<td>b<td>c",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><tbody _implied_='true'><tr><td>'a'</td><td>'b'</td></tr><tr><td>'a'</td><td>'b'</td><td>'c'</td></tr></tbody></table></body></html>"
            );

    // Test style.
    v.verify("<html><head><style><hr id=2></style></head><hr id = b>",
             "<html><head><style>'<hr id=2>'</style></head><body _implied_='true'><hr id='b'/></body></html>"
            );

    // Test style.
    v.verify("<style><hr id=2></style>x",
             "<html _implied_='true'><head _implied_='true'><style>'<hr id=2>'</style></head><body _implied_='true'>'x'</body></html>"
            );

    // Test entities in attributes.
    v.verify("<hr id='id_&#x41;&#90' class= \"&#89;_&amp;\" >",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><hr class='Y_&' id='id_AZ'/></body></html>"
            );

    // Test colgroup.
    v.verify("<table><COLGROUP width=\"25\"><COL span=\"45\"><COL id=\"identifier\"></COLGROUP><td>a<td>b<tr>x",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><table><colgroup width='25'><col span='45'/><col id='identifier'/></colgroup><tbody _implied_='true'><tr _implied_='true'><td>'a'</td><td>'b'</td></tr><tr><td _implied_='true'>'x'</td></tr></tbody></table></body></html>"
            );

    // Test definition list, obsolete.
    v.verify("<dl><dt>ha<dd>a<dt>hb<dd>b",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><dl><dt>'ha'</dt><dd>'a'</dd><dt>'hb'</dt><dd>'b'</dd></dl></body></html>"
            );

    // Test definition list.
    v.verify("<html><head></head><body><dl><dt>'ha'</dt><dd>'a'</dd><dt>'hb'</dt><dd>'b'</dd></dl></body></html>",
             "<html><head></head><body><dl><dt>''ha''</dt><dd>''a''</dd><dt>''hb''</dt><dd>''b''</dd></dl></body></html>"
            );

    // Test paragraphs.
    v.verify("<p>b<p>c<p>d",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><p>'b'</p><p>'c'</p><p>'d'</p></body></html>"
            );

    // Test paragraphs.
    v.verify("<p>'b'</p><p>'c'</p><p>'d'</p>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><p>''b''</p><p>''c''</p><p>''d''</p></body></html>"
            );

    // Test select obsolete.
    v.verify("<form><select><option value='hi' disabled>a<option selected>b<option>normal",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><form><select><option disabled='disabled' value='hi'>'a'</option><option selected='selected'>'b'</option><option>'normal'</option></select></form></body></html>"
            );

    // Test select current.
    v.verify("<form><select><option>'a'</option><option SELECTED='selected'>'b'</option></select></form>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><form><select><option>''a''</option><option selected='selected'>''b''</option></select></form></body></html>"
            );

    // Test select current.
    v.verify("<form><select><option>after<optgroup><option>'a'</option><option SELECTED='selected'>'b'</option></optgroup></select></form>",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'><form><select><option>'after'</option><optgroup><option>''a''</option><option selected='selected'>''b''</option></optgroup></select></form></body></html>"
            );

    // Test << antihang.
    v.verify("<<i>text",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'>'<'<i>'text'</i></body></html>"
            );

    // Test << antihang with spaces.
    v.verify(" < < i>text",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'>'<'<i>'text'</i></body></html>"
            );

    // Test Standalone <.
    v.verify("Text <wrong tag is it! <b> text ",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'>'Text''<wrong tag is it!'<b>'text'</b></body></html>"
            );
  }
}
