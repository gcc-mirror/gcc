/* tagStack.java -- The HTML tag stack.
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


package gnu.javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.models.node;
import gnu.javax.swing.text.html.parser.models.transformer;

import java.util.BitSet;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.*;

/**
 * <p>The HTML content validator, is responsible for opening and
 * closing elements with optional start/end tags, detecting
 * the wrongly placed html tags and reporting errors. The working instance
 * is the inner class inside the {@link javax.swing.text.html.parser.Parser }
 * </p>
 * <p>This class could potentially
 * provide basis for automated closing and insertion of the html tags,
 * correcting the found html errors.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class htmlValidator
{
  /**
   * The tag reference, holding additional information that the tag
   * has been forcibly closed.
   */
  protected class hTag
  {
    protected final Element element;
    protected final HTML.Tag tag;
    protected final TagElement tgElement;
    protected boolean forcibly_closed;
    protected node validationTrace;

    protected hTag(TagElement an_element)
    {
      element = an_element.getElement();
      tag = an_element.getHTMLTag();
      tgElement = an_element;

      if (element.content != null)
        validationTrace = transformer.transform(element.content, dtd);
    }

    /**
     * This is called when the tag must be forcibly closed because
     * it would make the newly appearing tag invalid.
     * The parser is not notified about such event (just the error
     * is reported). For such tags, the closing message does not
     * appear when later reaching the end of stream. The exception is
     * the &lt;head&gt; tag: the parser is notified about its silent closing
     * when &lt;body&gt; or other html content appears.
     */
    protected void forciblyCloseDueContext()
    {
      forcibly_closed = true;
    }

    /**
     * This is called when the tag must be forcibly closed after
     * reaching the end of stream. The parser is notified as if
     * closing the tag explicitly.
     */
    protected void forciblyCloseDueEndOfStream()
    {
      forcibly_closed = true;
      handleSupposedEndTag(element);
    }
  }

  /**
   * The DTD, providing information about the valid document structure.
   */
  protected final DTD dtd;

  /**
  * The stack, holding the current tag context.
  */
  protected final LinkedList stack = new LinkedList();

  /**
   * Creates a new tag stack, using the given DTD.
   * @param a_dtd A DTD, providing the information about the valid
   * tag content.
   */
  public htmlValidator(DTD a_dtd)
  {
    dtd = a_dtd;
  }

  /**
   * Close all opened tags (called at the end of parsing).
   */
  public void closeAll()
  {
    hTag h;
    while (!stack.isEmpty())
      {
        h = (hTag) stack.getLast();
        if (!h.forcibly_closed && !h.element.omitEnd())
          s_error("Unclosed <" + h.tag + ">, closing at the end of stream");

        handleSupposedEndTag(h.element);

        closeTag(h.tgElement);
      }
  }

  /**
   * Remove the given tag from the stack or (if found) from the list
   * of the forcibly closed tags.
   */
  public void closeTag(TagElement tElement)
  {
    HTML.Tag tag = tElement.getHTMLTag();
    hTag x;
    hTag close;

    if (!stack.isEmpty())
      {
        ListIterator iter = stack.listIterator(stack.size());

        while (iter.hasPrevious())
          {
            x = (hTag) iter.previous();
            if (tag.equals(x.tag))
              {
                if (x.forcibly_closed && !x.element.omitEnd())
                  s_error("The tag <" + x.tag +
                          "> has already been forcibly closed"
                         );


                // If the tag has a content model defined, forcibly close all
                // tags that were opened after the tag being currently closed.
                closing: 
                if (x.element.content != null)
                  {
                    iter = stack.listIterator(stack.size());
                    while (iter.hasPrevious())
                      {
                        close = (hTag) iter.previous();
                        if (close == x)
                          break closing;
                        handleSupposedEndTag(close.element);
                        iter.remove();
                      }
                  }

                stack.remove(x);
                return;
              }
          }
      }
    s_error("Closing unopened <" + tag + ">");
  }

  /**
   * Add the given HTML tag to the stack of the opened tags. Forcibly closes
   * all tags in the stack that does not allow this tag in they content (error
   * is reported).
   * @param element
   */
  public void openTag(TagElement tElement, htmlAttributeSet parameters)
  {
    // If this is a fictional call, the message from the parser
    // has recursively returned - ignore.
    if (tElement.fictional())
      return;

    validateParameters(tElement, parameters);

    // If the stack is empty, start from HTML
    if (stack.isEmpty() && tElement.getHTMLTag() != HTML.Tag.HTML)
      {
        Element html = dtd.getElement(HTML.Tag.HTML.toString());
        openFictionalTag(html);
      }

    Object v = tagIsValidForContext(tElement);
    if (v != Boolean.TRUE)
      {
        // The tag is not valid for context, the content
        // model suggest to open another tag.
        if (v instanceof Element)
          {
            int n = 0;
            while (v instanceof Element && (n++ < 100))
              {
                Element fe = (Element) v;

                // notify the content model that we add the proposed tag
                getCurrentContentModel().show(fe);
                openFictionalTag(fe);

                Object vv = tagIsValidForContext(tElement);
                if (vv instanceof Element) // One level of nesting is supported.
                  {
                    openFictionalTag((Element) vv);

                    Object vx = tagIsValidForContext(tElement);
                    if (vx instanceof Element)
                      openFictionalTag((Element) vx);
                  }
                else if (vv == Boolean.FALSE)
                  {
                    // The tag is still not valid for the current
                    // content after opening a fictional element.
                    if (fe.omitEnd())
                      {
                        // close the previously opened fictional tag.
                        closeLast();
                        vv = tagIsValidForContext(tElement);
                        if (vv instanceof Element)

                          // another tag was suggested by the content model
                          openFictionalTag((Element) vv);
                      }
                  }
                v = tagIsValidForContext(tElement);
              }
          }
        else // If the current element has the optional end tag, close it.
          {
            if (!stack.isEmpty())
              {
                closing: 
                do
                  {
                    hTag last = (hTag) stack.getLast();
                    if (last.element.omitEnd())
                      {
                        closeLast();
                        v = tagIsValidForContext(tElement);
                        if (v instanceof Element) // another tag was suggested by the content model
                          {
                            openFictionalTag((Element) v);
                            break closing;
                          }
                      }
                    else
                      break closing;
                  }
                while (v == Boolean.FALSE && !stack.isEmpty());
              }
          }
      }

    stack.add(new hTag(tElement));
  }

  /**
   * Clear the stack.
   */
  public void restart()
  {
    stack.clear();
  }

  /**
   * Check if this tag is valid for the current context.
   * Return Boolean.True if it is OK, Boolean.False
   * if it is surely not OK or the Element that the
   * content model recommends to insert making the situation
   * ok. If Boolean.True is returned, the content model current
   * position is moved forward. Otherwise this position remains
   * the same.
   * @param tElement
   * @return
   */
  public Object tagIsValidForContext(TagElement tElement)
  {
    // Check the current content model, if one is available.
    node cv = getCurrentContentModel();

    if (cv != null)
      return cv.show(tElement.getElement());

    // Check exclusions and inclusions.
    ListIterator iter = stack.listIterator(stack.size());
    hTag t;
    final int idx = tElement.getElement().index;

    // Check only known tags.
    if (idx >= 0)
      {
        BitSet inclusions = new BitSet();
        while (iter.hasPrevious())
          {
            t = (hTag) iter.previous();
            if (!t.forcibly_closed)
              {
                if (t.element.exclusions != null &&
                    t.element.exclusions.get(idx)
                   )
                  return Boolean.FALSE;

                if (t.element.inclusions != null)
                  inclusions.or(t.element.inclusions);
              }
          }
        if (!inclusions.get(idx))
          return Boolean.FALSE;
      }
    return Boolean.TRUE;
  }

  /**
   * Validate tag without storing in into the tag stack. This is called
   * for the empty tags and results the subsequent calls to the openTag
   * and closeTag.
   */
  public void validateTag(TagElement tElement, htmlAttributeSet parameters)
  {
    openTag(tElement, parameters);
    closeTag(tElement);
  }

  /**
   * Check for mandatory elements, subsequent to the last tag:
   * @param tElement The element that will be inserted next.
   */
  protected void checkContentModel(TagElement tElement, boolean first)
  {
    if (stack.isEmpty())
      return;

    hTag last = (hTag) stack.getLast();
    if (last.validationTrace == null)
      return;

    Object r = last.validationTrace.show(tElement.getElement());
    if (r == Boolean.FALSE)
      s_error("The <" + last.element + "> does not match the content model " +
              last.validationTrace
             );
    else if (r instanceof Element) // The content model recommends insertion of this element
      {
        if (!first)
          closeTag(last.tgElement);
        handleSupposedStartTag((Element) r);
        openTag(new TagElement((Element) r), null);
      }
  }

  /**
   * The method is called when the tag must be closed because
   * it does not allow the subsequent elements inside its context
   * or the end of stream has been reached. The parser is only
   * informed if the element being closed does not require the
   * end tag (the "omitEnd" flag is set).
   * The closing message must be passed to the parser mechanism
   * before passing message about the opening the next tag.
   *
   * @param element The tag being fictionally (forcibly) closed.
   */
  protected abstract void handleSupposedEndTag(Element element);

  /**
   * The method is called when the validator decides to open the
   * tag on its own initiative. This may happen if the content model
   * includes the element with the optional (supposed) start tag.
   *
   * @param element The tag being opened.
   */
  protected abstract void handleSupposedStartTag(Element element);

  /**
   * Handles the error message. This method must be overridden to pass
   * the message where required.
   * @param msg The message text.
   */
  protected abstract void s_error(String msg);

  /**
   * Validate the parameters, report the error if the given parameter is
   * not in the parameter set, valid for the given attribute. The information
   * about the valid parameter set is taken from the Element, enclosed
   * inside the tag. The method does not validate the default parameters.
   * @param tag The tag
   * @param parameters The parameters of this tag.
   */
  protected void validateParameters(TagElement tag, htmlAttributeSet parameters)
  {
    if (parameters == null ||
        parameters == htmlAttributeSet.EMPTY_HTML_ATTRIBUTE_SET ||
        parameters == SimpleAttributeSet.EMPTY
       )
      return;

    Enumeration enumeration = parameters.getAttributeNames();

    while (enumeration.hasMoreElements())
      {
        validateAttribute(tag, parameters, enumeration);
      }

    // Check for missing required values.
    AttributeList a = tag.getElement().getAttributes();

    while (a != null)
      {
        if (a.getModifier() == DTDConstants.REQUIRED)
          if (parameters.getAttribute(a.getName()) == null)
            {
              s_error("Missing required attribute '" + a.getName() + "' for <" +
                      tag.getHTMLTag() + ">"
                     );
            }
        a = a.next;
      }
  }

  private node getCurrentContentModel()
  {
    if (!stack.isEmpty())
      {
        hTag last = (hTag) stack.getLast();
        return last.validationTrace;
      }
    else
      return null;
  }

  private void closeLast()
  {
    handleSupposedEndTag(((hTag) stack.getLast()).element);
    stack.removeLast();
  }

  private void openFictionalTag(Element e)
  {
    handleSupposedStartTag(e);
    stack.add(new hTag(new TagElement(e, true)));
    if (!e.omitStart())
      s_error("<" + e + "> is expected (supposing it)");
  }

  private void validateAttribute(TagElement tag, htmlAttributeSet parameters,
                                 Enumeration enumeration
                                )
  {
    Object foundAttribute;
    AttributeList dtdAttribute;
    foundAttribute = enumeration.nextElement();
    dtdAttribute = tag.getElement().getAttribute(foundAttribute.toString());
    if (dtdAttribute == null)
      {
        StringBuffer valid =
          new StringBuffer("The tag <" + tag.getHTMLTag() +
                           "> cannot contain the attribute '" + foundAttribute +
                           "'. The valid attributes for this tag are: "
                          );

        AttributeList a = tag.getElement().getAttributes();

        while (a != null)
          {
            valid.append(a.name.toUpperCase());
            valid.append(' ');
            a = a.next;
          }
        s_error(valid.toString());
      }

    else
      {
        String value = parameters.getAttribute(foundAttribute).toString();

        if (dtdAttribute.type == DTDConstants.NUMBER)
          validateNumberAttribute(tag, foundAttribute, value);

        if (dtdAttribute.type == DTDConstants.NAME ||
            dtdAttribute.type == DTDConstants.ID
           )
          validateNameOrIdAttribute(tag, foundAttribute, value);

        if (dtdAttribute.values != null)
          validateAttributeWithValueList(tag, foundAttribute, dtdAttribute,
                                         value
                                        );
      }
  }

  private void validateAttributeWithValueList(TagElement tag,
                                              Object foundAttribute,
                                              AttributeList dtdAttribute,
                                              String value
                                             )
  {
    if (!dtdAttribute.values.contains(value.toLowerCase()) &&
        !dtdAttribute.values.contains(value.toUpperCase())
       )
      {
        StringBuffer valid;
        if (dtdAttribute.values.size() == 1)
          valid =
            new StringBuffer("The attribute '" + foundAttribute +
                             "' of the tag <" + tag.getHTMLTag() +
                             "> cannot have the value '" + value +
                             "'. The only valid value is "
                            );
        else
          valid =
            new StringBuffer("The attribute '" + foundAttribute +
                             "' of the tag <" + tag.getHTMLTag() +
                             "> cannot have the value '" + value + "'. The " +
                             dtdAttribute.values.size() +
                             " valid values are: "
                            );

        Enumeration vv = dtdAttribute.values.elements();
        while (vv.hasMoreElements())
          {
            valid.append('"');
            valid.append(vv.nextElement());
            valid.append("\"  ");
          }
        s_error(valid.toString());
      }
  }

  private void validateNameOrIdAttribute(TagElement tag, Object foundAttribute,
                                         String value
                                        )
  {
    boolean ok = true;

    if (!Character.isLetter(value.charAt(0)))
      ok = false;

    char c;
    for (int i = 0; i < value.length(); i++)
      {
        c = value.charAt(i);
        if (!(
              Character.isLetter(c) || Character.isDigit(c) ||
              "".indexOf(c) >= 0
            )
           )
          ok = false;
      }
    if (!ok)
      s_error("The '" + foundAttribute + "' attribute of the tag <" +
              tag.getHTMLTag() + "> must start from letter and consist of " +
              "letters, digits, hypens, colons, underscores and periods. " +
              "It cannot be '" + value + "'"
             );
  }

  private void validateNumberAttribute(TagElement tag, Object foundAttribute,
                                       String value
                                      )
  {
    try
      {
        Integer.parseInt(value);
      }
    catch (NumberFormatException ex)
      {
        s_error("The '" + foundAttribute + "' attribute of the tag <" +
                tag.getHTMLTag() + "> must be a valid number and not '" +
                value + "'"
               );
      }
  }
}
