/*
 * Copyright (c) 2003 World Wide Web Consortium,
 * (Massachusetts Institute of Technology, Institut National de
 * Recherche en Informatique et en Automatique, Keio University). All
 * Rights Reserved. This program is distributed under the W3C's Software
 * Intellectual Property License. This program is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.
 * See W3C License http://www.w3.org/Consortium/Legal/ for more details.
 */

package org.w3c.dom.html2;

/**
 * Regroups the <code>COL</code> and <code>COLGROUP</code> elements. See the 
 * COL element definition in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLTableColElement extends HTMLElement {
    /**
     * Horizontal alignment of cell data in column. See the align attribute 
     * definition in HTML 4.01.
     */
    public String getAlign();
    /**
     * Horizontal alignment of cell data in column. See the align attribute 
     * definition in HTML 4.01.
     */
    public void setAlign(String align);

    /**
     * Alignment character for cells in a column. See the char attribute 
     * definition in HTML 4.01.
     */
    public String getCh();
    /**
     * Alignment character for cells in a column. See the char attribute 
     * definition in HTML 4.01.
     */
    public void setCh(String ch);

    /**
     * Offset of alignment character. See the charoff attribute definition in 
     * HTML 4.01.
     */
    public String getChOff();
    /**
     * Offset of alignment character. See the charoff attribute definition in 
     * HTML 4.01.
     */
    public void setChOff(String chOff);

    /**
     * Indicates the number of columns in a group or affected by a grouping. 
     * See the span attribute definition in HTML 4.01.
     */
    public int getSpan();
    /**
     * Indicates the number of columns in a group or affected by a grouping. 
     * See the span attribute definition in HTML 4.01.
     */
    public void setSpan(int span);

    /**
     * Vertical alignment of cell data in column. See the valign attribute 
     * definition in HTML 4.01.
     */
    public String getVAlign();
    /**
     * Vertical alignment of cell data in column. See the valign attribute 
     * definition in HTML 4.01.
     */
    public void setVAlign(String vAlign);

    /**
     * Default column width. See the width attribute definition in HTML 4.01.
     */
    public String getWidth();
    /**
     * Default column width. See the width attribute definition in HTML 4.01.
     */
    public void setWidth(String width);

}
