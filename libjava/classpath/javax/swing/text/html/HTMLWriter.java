/* HTMLWriter.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.swing.text.html;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;
import java.io.Writer;

import java.util.Enumeration;
import java.util.HashSet;

import javax.swing.ComboBoxModel;

import javax.swing.text.AbstractWriter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;

import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.Option;

/**
 * HTMLWriter,
 * A Writer for HTMLDocuments.
 *
 * @author David Fu (fchoong at netbeans.jp)
 */

public class HTMLWriter
  extends AbstractWriter
{
  /**
   * We keep a reference of the writer passed by the construct.
   */
  private Writer outWriter = null;

  /**
   * We keep a reference of the HTMLDocument passed by the construct.
   */
  private HTMLDocument htmlDoc = null; 

  /**
   * Used to keep track of which embeded has been written out.
   */
  private HashSet openEmbededTagHashSet = null;

  private String new_line_str = "" + NEWLINE;
    
  private char[] html_entity_char_arr = {'<',    '>',    '&',     '"'};

  private String[] html_entity_escape_str_arr = {"&lt;", "&gt;", "&amp;", 
                                                 "&quot;"};

  // variables used to output Html Fragment
  private int doc_pos = -1;
  private int doc_len = -1;
  private int doc_offset_remaining = -1;
  private int doc_len_remaining = -1;
  private HashSet htmlFragmentParentHashSet = null;
  private Element startElem = null;
  private Element endElem = null;
  private boolean fg_pass_start_elem = false;
  private boolean fg_pass_end_elem = false;

  /**
   * Constructs a HTMLWriter.
   *
   * @param writer writer to write output to
   * @param doc the HTMLDocument to output
   */
  public HTMLWriter(Writer writer, HTMLDocument doc)
  {
    super(writer, doc);
    outWriter = writer;
    htmlDoc = doc;
    openEmbededTagHashSet = new HashSet();
  } // public HTMLWriter(Writer writer, HTMLDocument doc)

  /**
   * Constructs a HTMLWriter which outputs a Html Fragment.
   *
   * @param writer <code>Writer</code> to write output to
   * @param doc the <code>javax.swing.text.html.HTMLDocument</code>
   *        to output
   * @param pos position to start outputing the document
   * @param len amount to output the document
   */
  public HTMLWriter(Writer writer, HTMLDocument doc, int pos, int len)
  {
    super(writer, doc, pos, len);
    outWriter = writer;
    htmlDoc = doc;
    openEmbededTagHashSet = new HashSet();

    doc_pos = pos;
    doc_offset_remaining = pos;
    doc_len = len;
    doc_len_remaining = len;
    htmlFragmentParentHashSet = new HashSet();
  } // public HTMLWriter(Writer writer, HTMLDocument doc, int pos, int len)
    
  /**
   * Call this method to start outputing HTML.
   *
   * @throws IOException on any I/O exceptions
   * @throws BadLocationException if a pos is not a valid position in the
   *                              html doc element
   */
  public void write()
    throws IOException, BadLocationException
  {
    Element rootElem = htmlDoc.getDefaultRootElement();

    if (doc_pos == -1 && doc_len == -1)
      {
        // Normal traversal.
        traverse(rootElem);
      } // if(doc_pos == -1 && doc_len == -1)
    else    
      {
        // Html fragment traversal.
        if (doc_pos == -1 || doc_len == -1)
          throw new BadLocationException("Bad Location("
          + doc_pos + ", " + doc_len + ")", doc_pos);

        startElem = htmlDoc.getCharacterElement(doc_pos);

        int start_offset = startElem.getStartOffset(); 

        // Positions before start_offset will not be traversed, and thus
        // will not be counted.
        if (start_offset > 0)
          doc_offset_remaining = doc_offset_remaining - start_offset;

        Element tempParentElem = startElem;

        while ((tempParentElem = tempParentElem.getParentElement()) != null)
          {
            if (!htmlFragmentParentHashSet.contains(tempParentElem))
              htmlFragmentParentHashSet.add(tempParentElem);
          } // while((tempParentElem = tempParentElem.getParentElement())
            //   != null)

        // NOTE: 20061030 - fchoong - the last index should not be included.
        endElem = htmlDoc.getCharacterElement(doc_pos + doc_len - 1);

        tempParentElem = endElem;

        while ((tempParentElem = tempParentElem.getParentElement()) != null)
          {
            if (!htmlFragmentParentHashSet.contains(tempParentElem))
              htmlFragmentParentHashSet.add(tempParentElem);
          } // while((tempParentElem = tempParentElem.getParentElement())
            //   != null)

        traverseHtmlFragment(rootElem);

      } // else

    // NOTE: close out remaining open embeded tags.
    Object[] tag_arr = openEmbededTagHashSet.toArray();

    for (int i = 0; i < tag_arr.length; i++)
      {
        writeRaw("</" + tag_arr[i].toString() + ">");
      } // for(int i = 0; i < tag_arr.length; i++)

  } // public void write() throws IOException, BadLocationException
  
  /**
   * Writes all the attributes in the attrSet, except for attrbutes with
   * keys of <code>javax.swing.text.html.HTML.Tag</code>,
   * <code>javax.swing.text.StyleConstants</code> or
   * <code>javax.swing.text.html.HTML.Attribute.ENDTAG</code>.
   *
   * @param attrSet attrSet to write out
   *
   * @throws IOException on any I/O exceptions
   */
  protected void writeAttributes(AttributeSet attrSet)
    throws IOException
  {
    Enumeration attrNameEnum = attrSet.getAttributeNames();
        
    while (attrNameEnum.hasMoreElements())
      {
        Object key = attrNameEnum.nextElement();
        Object value = attrSet.getAttribute(key);
            
        // HTML.Attribute.ENDTAG is an instance, not a class.
        if (!((key instanceof HTML.Tag) || (key instanceof StyleConstants)
          || (key == HTML.Attribute.ENDTAG)))
          {
            if (key == HTML.Attribute.SELECTED)
              writeRaw(" selected");
            else if (key == HTML.Attribute.CHECKED)
              writeRaw(" checked");
            else
              writeRaw(" " + key + "=\"" + value + "\"");
          } // if(!((key instanceof HTML.Tag) || (key instanceof
            //   StyleConstants) || (key == HTML.Attribute.ENDTAG)))
      } // while(attrNameEnum.hasMoreElements())
        
  } // protected void writeAttributes(AttributeSet attrSet) throws IOException

  /**
   * Writes out an empty tag. i.e. a tag without any child elements.
   *
   * @param paramElem the element to output as an empty tag
   *
   * @throws IOException on any I/O exceptions
   * @throws BadLocationException if a pos is not a valid position in the
   *                              html doc element
   */
  protected void emptyTag(Element paramElem)
    throws IOException, BadLocationException
  {
    String elem_name = paramElem.getName();
    AttributeSet attrSet = paramElem.getAttributes();

    writeRaw("<" + elem_name);
    writeAttributes(attrSet);
    writeRaw(">");

    if (isBlockTag(attrSet))
      {
        writeRaw("</" + elem_name + ">");
      } // if(isBlockTag(attrSet))
        
  } // protected void emptyTag(Element paramElem)
    //   throws IOException, BadLocationException
    
  /**
   * Determines if it is a block tag or not.
   *
   * @param attrSet the attrSet of the element
   *
   * @return <code>true</code> if it is a block tag
   *         <code>false</code> if it is a not block tag
   */
  protected boolean isBlockTag(AttributeSet attrSet)
  {
    return ((HTML.Tag)
      attrSet.getAttribute(StyleConstants.NameAttribute)).isBlock();
  } // protected boolean isBlockTag(AttributeSet attrSet)

  /**
   * Writes out a start tag. Synthesized elements are skipped.
   *
   * @param paramElem the element to output as a start tag
   * @throws IOException on any I/O exceptions
   * @throws BadLocationException if a pos is not a valid position in the
   *                              html doc element
   */
  protected void startTag(Element paramElem)
    throws IOException, BadLocationException
  {
    // NOTE: Sysnthesized elements do no call this method at all.
    String elem_name = paramElem.getName();
    AttributeSet attrSet = paramElem.getAttributes();

    indent();
    writeRaw("<" + elem_name);
    writeAttributes(attrSet);
    writeRaw(">");
    writeLineSeparator(); // Extra formatting to look more like the RI.
    incrIndent();

  } // protected void startTag(Element paramElem)
    //   throws IOException, BadLocationException

  /**
   * Writes out the contents of a textarea.
   *
   * @param attrSet the attrSet of the element to output as a text area
   * @throws IOException on any I/O exceptions
   * @throws BadLocationException if a pos is not a valid position in the
   *                              html doc element
   */
  protected void textAreaContent(AttributeSet attrSet)
    throws IOException, BadLocationException
  {
    writeLineSeparator(); // Extra formatting to look more like the RI.
    indent();
    writeRaw("<textarea");
    writeAttributes(attrSet);
    writeRaw(">");

    Document tempDocument = 
      (Document) attrSet.getAttribute(StyleConstants.ModelAttribute);

    writeRaw(tempDocument.getText(0, tempDocument.getLength()));
    indent();
    writeRaw("</textarea>");

  } // protected void textAreaContent(AttributeSet attrSet)
    //   throws IOException, BadLocationException

  /**
   * Writes out text, within the appropriate range if it is specified.
   *
   * @param paramElem the element to output as a text
   * @throws IOException on any I/O exceptions
   * @throws BadLocationException if a pos is not a valid position in the
   *                              html doc element
   */
  protected void text(Element paramElem)
    throws IOException, BadLocationException
  {
    int offset =  paramElem.getStartOffset();
    int len =  paramElem.getEndOffset() -  paramElem.getStartOffset();
    String txt_value = htmlDoc.getText(offset, len);

    writeContent(txt_value);

  } // protected void text(Element paramElem)
    //   throws IOException, BadLocationException

  /**
   * Writes out the contents of a select element.
   *
   * @param attrSet the attrSet of the element to output as a select box
   *
   * @throws IOException on any I/O exceptions
   */
  protected void selectContent(AttributeSet attrSet)
    throws IOException
  {
    writeLineSeparator(); // Extra formatting to look more like the RI.
    indent();
    writeRaw("<select");
    writeAttributes(attrSet);
    writeRaw(">");
    incrIndent();
    writeLineSeparator(); // extra formatting to look more like the RI.

    ComboBoxModel comboBoxModel =
      (ComboBoxModel) attrSet.getAttribute(StyleConstants.ModelAttribute);

    for (int i = 0; i < comboBoxModel.getSize(); i++)
      {
        writeOption((Option) comboBoxModel.getElementAt(i));
      } // for(int i = 0; i < comboBoxModel.getSize(); i++)

    decrIndent();
    indent();
    writeRaw("</select>");

  } // protected void selectContent(AttributeSet attrSet) throws IOException

  /**
   * Writes out the contents of an option element.
   *
   * @param option the option object to output as a select option
   *
   * @throws IOException on any I/O exceptions
   */
  protected void writeOption(Option option)
    throws IOException
  {
    indent();
    writeRaw("<option");
    writeAttributes(option.getAttributes());
    writeRaw(">");

    writeContent(option.getLabel());

    writeRaw("</option>");
    writeLineSeparator(); // extra formatting to look more like the RI.

  } // protected void writeOption(Option option) throws IOException

  /**
   * Writes out an end tag.
   *
   * @param paramElem the element to output as an end tag
   *
   * @throws IOException on any I/O exceptions
   */
  protected void endTag(Element paramElem)
    throws IOException
  {
    String elem_name = paramElem.getName();

    //writeLineSeparator(); // Extra formatting to look more like the RI.
    decrIndent();
    indent();
    writeRaw("</" + elem_name + ">");
    writeLineSeparator(); // Extra formatting to look more like the RI.

  } // protected void endTag(Element paramElem) throws IOException

  /**
   * Writes out the comment.
   *
   * @param paramElem the element to output as a comment
   */
  protected void comment(Element paramElem)
    throws IOException, BadLocationException
  {
    AttributeSet attrSet = paramElem.getAttributes();

    String comment_str = (String) attrSet.getAttribute(HTML.Attribute.COMMENT);

    writeRaw("<!--" + comment_str + "-->");

  } // protected void comment(Element paramElem)
    //   throws IOException, BadLocationException

  /**
   * Determines if element is a synthesized
   * <code>javax.swing.text.Element</code> or not.
   *
   * @param element the element to test
   *
   * @return <code>true</code> if it is a synthesized element,
   *         <code>false</code> if it is a not synthesized element
   */
  protected boolean synthesizedElement(Element element)
  {
    AttributeSet attrSet = element.getAttributes();
    Object tagType = attrSet.getAttribute(StyleConstants.NameAttribute);

    if (tagType == HTML.Tag.CONTENT || tagType == HTML.Tag.COMMENT
        || tagType == HTML.Tag.IMPLIED)
      return true;
    else
      return false;
  } // protected boolean synthesizedElement(Element element)

  /**
   * Determines if
   * <code>javax.swing.text.StyleConstants.NameAttribute</code>
   * matches tag or not.
   *
   * @param attrSet the <code>javax.swing.text.AttributeSet</code> of
   *        element to be matched
   * @param tag the HTML.Tag to match
   *
   * @return <code>true</code> if it matches,
   *         <code>false</code> if it does not match
   */
  protected boolean matchNameAttribute(AttributeSet attrSet, HTML.Tag tag)
  {
    Object tagType = attrSet.getAttribute(StyleConstants.NameAttribute);

    if (tagType == tag)
      return true;
    else
      return false;
  } // protected boolean matchNameAttribute(AttributeSet attrSet,
    //   HTML.Tag tag)

  /**
   * Writes out an embedded tag. The tags not already in
   * openEmbededTagHashSet will written out.
   *
   * @param attrSet the <code>javax.swing.text.AttributeSet</code> of
   *        the element to write out
   *
   * @throws IOException on any I/O exceptions
   */
  protected void writeEmbeddedTags(AttributeSet attrSet)
    throws IOException
  {
    Enumeration attrNameEnum = attrSet.getAttributeNames();

    while (attrNameEnum.hasMoreElements())
      {
        Object key = attrNameEnum.nextElement();
        Object value = attrSet.getAttribute(key);

        if (key instanceof HTML.Tag)
          {
            if (!openEmbededTagHashSet.contains(key))
              {
                writeRaw("<" + key);
                writeAttributes((AttributeSet) value);
                writeRaw(">");
                openEmbededTagHashSet.add(key);
              } // if(!openEmbededTagHashSet.contains(key))
          } // if(key instanceof HTML.Tag)
      } // while(attrNameEnum.hasMoreElements())

  } // protected void writeEmbeddedTags(AttributeSet attrSet)
    //   throws IOException

  /**
   * Closes out an unwanted embedded tag. The tags from the
   *  openEmbededTagHashSet not found in attrSet will be written out.
   * 
   *  @param attrSet the AttributeSet of the element to write out
   * 
   *  @throws IOException on any I/O exceptions
   */
  protected void closeOutUnwantedEmbeddedTags(AttributeSet attrSet)
    throws IOException
  {
    Object[] tag_arr = openEmbededTagHashSet.toArray();

    for (int i = 0; i < tag_arr.length; i++)
      {
        HTML.Tag key = (HTML.Tag) tag_arr[i];
            
        if (!attrSet.isDefined(key))
          {
            writeRaw("</" + key.toString() + ">");
            openEmbededTagHashSet.remove(key);
          } // if(!attrSet.isDefined(key))
      } // for(int i = 0; i < tag_arr.length; i++)

  } // protected void closeOutUnwantedEmbeddedTags(AttributeSet attrSet)
    //   throws IOException

  /**
   * Writes out a line separator. Overwrites the parent to write out a new
   * line.
   *
   * @throws IOException on any I/O exceptions.
   */
  protected void writeLineSeparator()
    throws IOException
  {
    writeRaw(new_line_str);
  } // protected void writeLineSeparator() throws IOException

  /**
   * Write to the writer. Character entites such as &lt;, &gt;
   * are escaped appropriately.
   *
   * @param chars char array to write out
   * @param off offset
   * @param len length
   *
   * @throws IOException on any I/O exceptions
   */
  protected void output(char[] chars, int off, int len)
   throws IOException
  {
    CPStringBuilder strBuffer = new CPStringBuilder();

    for (int i = 0; i < chars.length; i++)
      {
        if (isCharHtmlEntity(chars[i]))
          strBuffer.append(escapeCharHtmlEntity(chars[i]));
        else
          strBuffer.append(chars[i]);
      } // for(int i = 0; i < chars.length; i++)

    writeRaw(strBuffer.toString());

  } // protected void output(char[] chars, int off, int len)
    //   throws IOException
 
  //-------------------------------------------------------------------------
  // private methods
  
  /**
   * The main method used to traverse through the elements.
   *
   * @param paramElem element to traverse
   *
   * @throws IOException on any I/O exceptions
   */
  private void traverse(Element paramElem)
    throws IOException, BadLocationException
  {
    Element currElem = paramElem;

    AttributeSet attrSet = currElem.getAttributes();

    closeOutUnwantedEmbeddedTags(attrSet);

    // handle the tag
    if (synthesizedElement(paramElem))
      {
        if (matchNameAttribute(attrSet, HTML.Tag.CONTENT))
          {
            writeEmbeddedTags(attrSet);
            text(currElem);
          } // if(matchNameAttribute(attrSet, HTML.Tag.CONTENT))
        else if (matchNameAttribute(attrSet, HTML.Tag.COMMENT))
          {
            comment(currElem);
          } // else if(matchNameAttribute(attrSet, HTML.Tag.COMMENT))
        else if (matchNameAttribute(attrSet, HTML.Tag.IMPLIED))
          {
            int child_elem_count = currElem.getElementCount();
                
            if (child_elem_count > 0)
              {
                for (int i = 0; i < child_elem_count; i++)
                  {
                    Element childElem = paramElem.getElement(i);

                    traverse(childElem);

                  } // for(int i = 0; i < child_elem_count; i++)
              } // if(child_elem_count > 0)
          } // else if(matchNameAttribute(attrSet, HTML.Tag.IMPLIED))
      } // if(synthesizedElement(paramElem))
    else
      {
        // NOTE: 20061030 - fchoong - title is treated specially here.
        // based on RI behavior.
        if (matchNameAttribute(attrSet, HTML.Tag.TITLE))
          {
            boolean fg_is_end_tag = false;
            Enumeration attrNameEnum = attrSet.getAttributeNames();

            while (attrNameEnum.hasMoreElements())
              {
                Object key = attrNameEnum.nextElement();
                Object value = attrSet.getAttribute(key);

                if (key == HTML.Attribute.ENDTAG && value.equals("true"))
                  fg_is_end_tag = true;
              } // while(attrNameEnum.hasMoreElements())

            if (fg_is_end_tag)
              writeRaw("</title>");
            else
              {
                indent();
                writeRaw("<title>");

                String title_str = 
                  (String) htmlDoc.getProperty(HTMLDocument.TitleProperty);

                if (title_str != null)
                  writeContent(title_str);

              } // else
          } // if(matchNameAttribute(attrSet, HTML.Tag.TITLE))
        else if (matchNameAttribute(attrSet, HTML.Tag.PRE))
          {
            // We pursue more stringent formating here.
            attrSet = paramElem.getAttributes();

            indent();
            writeRaw("<pre");
            writeAttributes(attrSet);
            writeRaw(">");

            int child_elem_count = currElem.getElementCount();

            for (int i = 0; i < child_elem_count; i++)
              {
                Element childElem = paramElem.getElement(i);

                traverse(childElem);

              } // for(int i = 0; i < child_elem_count; i++)

            writeRaw("</pre>");

          } // else if(matchNameAttribute(attrSet, HTML.Tag.PRE))
        else if (matchNameAttribute(attrSet, HTML.Tag.SELECT))
          {
            selectContent(attrSet);
          } // else if(matchNameAttribute(attrSet, HTML.Tag.SELECT))
        else if (matchNameAttribute(attrSet, HTML.Tag.TEXTAREA))
          {
            textAreaContent(attrSet);
          } // else if(matchNameAttribute(attrSet, HTML.Tag.TEXTAREA))
        else
          {
            int child_elem_count = currElem.getElementCount();

            if (child_elem_count > 0)
              {
                startTag(currElem);

                for (int i = 0; i < child_elem_count; i++)
                  {
                    Element childElem = paramElem.getElement(i);

                    traverse(childElem);

                  } // for(int i = 0; i < child_elem_count; i++)

                  endTag(currElem);

              } // if(child_elem_count > 0)
            else
              {
                emptyTag(currElem);
              } // else 
            } // else
          } // else

  } // private void traverse(Element paramElem)
    //   throws IOException, BadLocationException

  /**
   * The method used to traverse through a html fragment.
   *
   * @param paramElem element to traverse
   *
   * @throws IOException on any I/O exceptions
   */
  private void traverseHtmlFragment(Element paramElem)
    throws IOException, BadLocationException
  {
    // NOTE: This method is similar to traverse(Element paramElem)
    Element currElem = paramElem;

    boolean fg_is_fragment_parent_elem = false;
    boolean fg_is_start_and_end_elem = false;

    if (htmlFragmentParentHashSet.contains(paramElem))
      fg_is_fragment_parent_elem = true;

    if (paramElem == startElem)
      fg_pass_start_elem = true;

    if (paramElem == startElem && paramElem == endElem)
      fg_is_start_and_end_elem = true;

    AttributeSet attrSet = currElem.getAttributes();

    closeOutUnwantedEmbeddedTags(attrSet);

    if (fg_is_fragment_parent_elem || (fg_pass_start_elem
        && fg_pass_end_elem == false) || fg_is_start_and_end_elem)
    {
      // handle the tag
      if (synthesizedElement(paramElem))
        {
          if (matchNameAttribute(attrSet, HTML.Tag.CONTENT))
            {
              writeEmbeddedTags(attrSet);

              int content_offset =  paramElem.getStartOffset();
              int content_length = currElem.getEndOffset() - content_offset;

              if (doc_offset_remaining > 0)
                {
                  if (content_length > doc_offset_remaining)
                    {
                      int split_len = content_length;

                      split_len = split_len - doc_offset_remaining;

                      if (split_len > doc_len_remaining)
                        split_len = doc_len_remaining;

                      // we need to split it.
                      String txt_value = htmlDoc.getText(content_offset
                        + doc_offset_remaining, split_len);

                      writeContent(txt_value);

                      doc_offset_remaining = 0; // the offset is used up.
                      doc_len_remaining = doc_len_remaining - split_len;
                    } // if(content_length > doc_offset_remaining)
                  else
                    {
                      // doc_offset_remaining is greater than the entire
                      //   length of content
                      doc_offset_remaining = doc_offset_remaining
                        - content_length;
                    }  // else
                } // if(doc_offset_remaining > 0)
              else if (content_length <= doc_len_remaining)
                {
                  // we can fit the entire content.
                  text(currElem);
                  doc_len_remaining = doc_len_remaining - content_length;
                } // else if(content_length <= doc_len_remaining)
              else
                {
                  // we need to split it.
                  String txt_value = htmlDoc.getText(content_offset,
                    doc_len_remaining);

                  writeContent(txt_value);

                  doc_len_remaining = 0;
                } // else

            } // if(matchNameAttribute(attrSet, HTML.Tag.CONTENT))
          else if (matchNameAttribute(attrSet, HTML.Tag.COMMENT))
            {
              comment(currElem);
            } // else if(matchNameAttribute(attrSet, HTML.Tag.COMMENT))
          else if (matchNameAttribute(attrSet, HTML.Tag.IMPLIED))
            {
              int child_elem_count = currElem.getElementCount();

              if (child_elem_count > 0)
                {
                  for (int i = 0; i < child_elem_count; i++)
                    {
                      Element childElem = paramElem.getElement(i);

                      traverseHtmlFragment(childElem);

                    } // for(int i = 0; i < child_elem_count; i++)
                } // if(child_elem_count > 0)
            } // else if(matchNameAttribute(attrSet, HTML.Tag.IMPLIED))
        } // if(synthesizedElement(paramElem))
      else
        { 
            // NOTE: 20061030 - fchoong - the isLeaf() condition seems to
            // generate the closest behavior to the RI.
            if (paramElem.isLeaf())
              {
                if (doc_offset_remaining > 0)
                  {
                    doc_offset_remaining--;
                  } // if(doc_offset_remaining > 0)
                else if (doc_len_remaining > 0)
                  {
                    doc_len_remaining--;
                  } // else if(doc_len_remaining > 0)
              } // if(paramElem.isLeaf())

          // NOTE: 20061030 - fchoong - title is treated specially here.
          // based on RI behavior.
          if (matchNameAttribute(attrSet, HTML.Tag.TITLE))
            {
              boolean fg_is_end_tag = false;
              Enumeration attrNameEnum = attrSet.getAttributeNames();

              while (attrNameEnum.hasMoreElements())
                {
                  Object key = attrNameEnum.nextElement();
                  Object value = attrSet.getAttribute(key);

                  if (key == HTML.Attribute.ENDTAG && value.equals("true"))
                    fg_is_end_tag = true;
                } // while(attrNameEnum.hasMoreElements())

              if (fg_is_end_tag)
                writeRaw("</title>");
              else
                {
                  indent();
                  writeRaw("<title>");

                  String title_str = 
                    (String) htmlDoc.getProperty(HTMLDocument.TitleProperty);

                  if (title_str != null)
                    writeContent(title_str);

                } // else
            } // if(matchNameAttribute(attrSet, HTML.Tag.TITLE))
          else if (matchNameAttribute(attrSet, HTML.Tag.PRE))
            {
              // We pursue more stringent formating here.
              attrSet = paramElem.getAttributes();

              indent();
              writeRaw("<pre");
              writeAttributes(attrSet);
              writeRaw(">");

              int child_elem_count = currElem.getElementCount();

              for (int i = 0; i < child_elem_count; i++)
                {
                  Element childElem = paramElem.getElement(i);

                  traverseHtmlFragment(childElem);

                } // for(int i = 0; i < child_elem_count; i++)

              writeRaw("</pre>");

            } // else if(matchNameAttribute(attrSet, HTML.Tag.PRE))
          else if (matchNameAttribute(attrSet, HTML.Tag.SELECT))
            {
              selectContent(attrSet);
            } // else if(matchNameAttribute(attrSet, HTML.Tag.SELECT))
          else if (matchNameAttribute(attrSet, HTML.Tag.TEXTAREA))
            {
              textAreaContent(attrSet);
            } // else if(matchNameAttribute(attrSet, HTML.Tag.TEXTAREA))
          else
            {
              int child_elem_count = currElem.getElementCount();

              if (child_elem_count > 0)
                {
                  startTag(currElem);

                  for (int i = 0; i < child_elem_count; i++)
                    {
                      Element childElem = paramElem.getElement(i);

                      traverseHtmlFragment(childElem);

                    } // for(int i = 0; i < child_elem_count; i++)

                    endTag(currElem);

                } // if(child_elem_count > 0)
              else
                {
                  emptyTag(currElem);
                } // else 
            } // else
        } // else

    } // if(fg_is_fragment_parent_elem || (fg_pass_start_elem
      //   && fg_pass_end_elem == false) || fg_is_start_and_end_elem)

    if (paramElem == endElem)
      fg_pass_end_elem = true;

  } // private void traverseHtmlFragment(Element paramElem)
    //   throws IOException, BadLocationException

  /**
   * Write to the writer without any modifications.
   *
   * @param param_str the str to write out
   *
   * @throws IOException on any I/O exceptions
   */
  private void writeRaw(String param_str)
    throws IOException
  {
    super.output(param_str.toCharArray(), 0, param_str.length());
  } // private void writeRaw(char[] chars, int off, int len)
    //   throws IOException

  /**
   * Write to the writer, escaping HTML character entitie where neccessary.
   *
   * @param param_str the str to write out
   *
   * @throws IOException on any I/O exceptions
   */
  private void writeContent(String param_str)
    throws IOException
  {
    char[] str_char_arr = param_str.toCharArray();

    if (hasHtmlEntity(param_str))
      output(str_char_arr, 0, str_char_arr.length);
    else
      super.output(str_char_arr, 0, str_char_arr.length);

  } // private void writeContent(String param_str) throws IOException

  /**
   * Use this for debugging. Writes out all attributes regardless of type.
   *
   * @param attrSet the <code>javax.swing.text.AttributeSet</code> to
   *        write out
   *
   * @throws IOException on any I/O exceptions
   */
  private void writeAllAttributes(AttributeSet attrSet)
    throws IOException
  {
    Enumeration attrNameEnum = attrSet.getAttributeNames();

    while (attrNameEnum.hasMoreElements())
      {
        Object key = attrNameEnum.nextElement();
        Object value = attrSet.getAttribute(key);

        writeRaw(" " + key + "=\"" + value + "\"");
        writeRaw(" " + key.getClass().toString() + "=\""
          + value.getClass().toString() + "\"");
      } // while(attrNameEnum.hasMoreElements())

  } // private void writeAllAttributes(AttributeSet attrSet)
    //   throws IOException

  /**
   * Tests if the str contains any html entities.
   *
   * @param param_str the str to test
   *
   * @return <code>true</code> if it has a html entity
   *         <code>false</code> if it does not have a html entity
   */
  private boolean hasHtmlEntity(String param_str)
  {
    boolean ret_bool = false;

    for (int i = 0; i < html_entity_char_arr.length; i++)
      {
        if (param_str.indexOf(html_entity_char_arr[i]) != -1)
          {
            ret_bool = true;
            break;
          } // if(param_str.indexOf(html_entity_char_arr[i]) != -1)
      } // for(int i = 0; i < html_entity_char_arr.length; i++)

    return ret_bool;
  } // private boolean hasHtmlEntity(String param_str)

  /**
   * Tests if the char is a html entities.
   *
   * @param param_char the char to test
   *
   * @return <code>true</code> if it is a html entity
   *         <code>false</code> if it is not a html entity.
   */
  private boolean isCharHtmlEntity(char param_char)
  {
    boolean ret_bool = false;

    for (int i = 0; i < html_entity_char_arr.length; i++)
      {
        if (param_char == html_entity_char_arr[i])
          {
            ret_bool = true;
            break;
          } // if(param_char == html_entity_char_arr[i])
      } // for(int i = 0; i < html_entity_char_arr.length; i++)

      return ret_bool;
  } // private boolean hasHtmlEntity(String param_str)

  /**
   * Escape html entities.
   *
   * @param param_char the char to escape
   *
   * @return escaped html entity. Original char is returned as a str if is
   *         is not a html entity
   */
  private String escapeCharHtmlEntity(char param_char)
  {
    String ret_str = "" + param_char;

    for (int i = 0; i < html_entity_char_arr.length; i++)
      {
        if (param_char == html_entity_char_arr[i])
          {
            ret_str = html_entity_escape_str_arr[i];
            break;
          } // if(param_char == html_entity_char_arr[i])
      } // for(int i = 0; i < html_entity_char_arr.length; i++)

      return ret_str;
  } // private String escapeCharHtmlEntity(char param_char)

} // public class HTMLWriter extends AbstractWriter
