<?xml version="1.0" encoding="utf-8"?>

<!-- help.xsl
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

<!-- Creates the class descriptor files for HTML documentation. 
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="html"
    encoding="utf-8"
    doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">    
    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="'Help'"/>
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
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="0"/>
          <xsl:with-param name="p_curr_help" select="1"/>
          <xsl:with-param name="p_top" select="1"/>
        </xsl:call-template>

        <div class="pagebody">

        <xsl:choose>
          <xsl:when test="$gjdoc.option.helpfile">
            <xsl:copy-of select="document($gjdoc.option.helpfile)//body"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="document('res/default_help_en.html')"/>
          </xsl:otherwise>
        </xsl:choose>

        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="0"/>
          <xsl:with-param name="p_curr_help" select="1"/>
          <xsl:with-param name="p_top" select="0"/> 
        </xsl:call-template>
        </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
