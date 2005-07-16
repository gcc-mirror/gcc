/* AllParserTests.java -- The comprehensive HTML parser test.
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
 
 
import test.gnu.javax.swing.text.html.HTML_Test;
import test.gnu.javax.swing.text.html.parser.AttributeList_test;
import test.gnu.javax.swing.text.html.parser.DTD_test;
import test.gnu.javax.swing.text.html.parser.Element_Test;
import test.gnu.javax.swing.text.html.parser.Entity_Test;
import test.gnu.javax.swing.text.html.parser.HTML_parsing;
import test.gnu.javax.swing.text.html.parser.HTML_randomTable;
import test.gnu.javax.swing.text.html.parser.ParserEntityResolverTest;
import test.gnu.javax.swing.text.html.parser.TagElement_Test;
import test.gnu.javax.swing.text.html.parser.Text;
import test.gnu.javax.swing.text.html.parser.Token_locations;
import test.gnu.javax.swing.text.html.parser.parameterDefaulter_Test;
import test.gnu.javax.swing.text.html.parser.supplementaryNotifications;
import test.gnu.javax.swing.text.html.parser.textPreProcessor_Test;
import test.gnu.javax.swing.text.html.parser.low.Buffer_Test;
import test.gnu.javax.swing.text.html.parser.low.Constants_Test;
import test.gnu.javax.swing.text.html.parser.low.ReaderTokenizer_Test;

/**
 * This is a complete test for javax.swing.text.html.parser package.
 * Apart javax.* classes, it also tests the implementation specific
 * gnu.javax.* classes and in this way is more strict than
 * Mauve tests. To avoid regression it is strongly recommended to run
 * this test after you modify clases in javax.swing.text.html.parser or
 * gnu.javax.swing.text.html.parser.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class AllParserTests
{
  public static void main(String[] args)
  {
    try
      {
        HTML_Test a_HTML_Test = new HTML_Test();
        a_HTML_Test.testGetAttributeKey();
        a_HTML_Test.testGetIntegerAttributeValue();
        a_HTML_Test.testGetTag();
        a_HTML_Test.testCaseSensitivity();
        a_HTML_Test.testConstructor();

        Buffer_Test a_Buffer_Test = new Buffer_Test();
        a_Buffer_Test.testDelete();
        a_Buffer_Test.testAppend();

        Constants_Test a_Constants_Test = new Constants_Test();
        a_Constants_Test.testCases();

        ReaderTokenizer_Test a_ReaderTokenizer_Test =
          new ReaderTokenizer_Test();
        a_ReaderTokenizer_Test.testReadingAndAhead();
        a_ReaderTokenizer_Test.testComplexToken();

        AttributeList_test a_AttributeList_test = new AttributeList_test();
        a_AttributeList_test.testSame();

        DTD_test a_DTD_test = new DTD_test();
        a_DTD_test.testGetElement();

        Element_Test a_Element_Test = new Element_Test();
        a_Element_Test.testName2type();
        a_Element_Test.testAttributeGetter();

        Entity_Test a_Entity_Test = new Entity_Test();
        a_Entity_Test.testName2type();
        a_Entity_Test.testPublicSystemGeneralParameter();

        HTML_parsing a_HTML_parsing = new HTML_parsing();
        a_HTML_parsing.testHTMLParsing();

        HTML_randomTable a_HTML_randomTable = new HTML_randomTable();
        a_HTML_randomTable.testTableParsing();

        parameterDefaulter_Test a_parameterDefaulter_Test =
          new parameterDefaulter_Test();
        a_parameterDefaulter_Test.testDefaultValues();

        ParserEntityResolverTest a_ParserEntityResolverTest =
          new ParserEntityResolverTest();
        a_ParserEntityResolverTest.testResolver();

        supplementaryNotifications a_supplementaryNotifications =
          new supplementaryNotifications();
        a_supplementaryNotifications.testHTMLParsing();

        TagElement_Test a_TagElement_Test = new TagElement_Test();
        a_TagElement_Test.testTagElement();

        textPreProcessor_Test a_textPreProcessor_Test =
          new textPreProcessor_Test();
        a_textPreProcessor_Test.testStandardPreProcessing();
        a_textPreProcessor_Test.testPreFormattedPreProcessing();

        Text a_Text = new Text();
        a_Text.testTextParsing();

        Token_locations a_Token_locations = new Token_locations();
        a_Token_locations.testHTMLParsing();
      }
    catch (Exception ex)
      {
        System.err.println("The tests have FAILED.\nPlease either correct your " +
                           "changes\nor, if you are absolutely sure, correct the tests.\n" +
                           "See the following exception for details"
                          );
        ex.printStackTrace(System.err);
        System.exit(1);
      }
    System.out.println("HTML parser tests have passed.");
    System.exit(0);
  }
}
