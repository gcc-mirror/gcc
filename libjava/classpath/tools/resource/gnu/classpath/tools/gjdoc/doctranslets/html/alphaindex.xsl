<?xml version="1.0" encoding="utf-8"?>

<!-- alphaindex.xsl
     Copyright (C) 2003 Free Software Foundation, Inc.
     
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
     -->

<!-- Creates the alphaindex.html file for HTML documentation. 
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="xml"
    encoding="utf-8"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">

    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="'Alphabetical Index'"/>
        </xsl:call-template>
        <xsl:call-template name="include_common"/>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)">

        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="0"/>
          <xsl:with-param name="p_curr_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_top" select="1"/> 
        </xsl:call-template>

        <div class="pagebody">
        <h1 class="classdoc-title">
          Alphabetical Index
        </h1>

        <div class="index-categories">
          <xsl:for-each select="document('alphaindex.xml',/)/gjdoc:alphaindex/gjdoc:category">
            <xsl:choose>
              <xsl:when test="@letter=$gjdoc.outputfile.info">
                <span class="index-category-current"><xsl:value-of select="@letter"/></span>
              </xsl:when>
              <xsl:otherwise>
                <a href="{concat('#category_', @letter)}" class="index-category-link"><xsl:value-of select="@letter"/></a>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </div>

        <xsl:for-each select="document('alphaindex.xml',/)/gjdoc:alphaindex/gjdoc:category">
          <a name="{concat('category_', @letter)}" class="classdoc"><h3 class="ignore"> </h3></a>
          <h2><xsl:value-of select="@letter"/></h2>
          <xsl:call-template name="output_alphaindex_category"/>
        </xsl:for-each>
        
        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="0"/>
          <xsl:with-param name="p_curr_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_top" select="0"/> 
        </xsl:call-template>
        </div>
      </body>
    </html>
    
  </xsl:template>  

</xsl:stylesheet>
