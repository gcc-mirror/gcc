<?xml version="1.0" encoding="utf-8"?>

<!-- about.xsl
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
        <xsl:call-template name="include_common"/>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="'Help'"/>
        </xsl:call-template>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded()">

        <div class="pagebody">

        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_about" select="0"/>
          <xsl:with-param name="p_curr_about" select="1"/>
          <xsl:with-param name="p_top" select="1"/>
        </xsl:call-template>

        <h1 class="classdoc-title">About this API documentation</h1>

        <div class="footer">
          <xsl:text>Generated on</xsl:text><xsl:value-of select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:created"/><xsl:text> by </xsl:text><a href="http://www.gnu.org/software/cp-tools" target="cptoolsinfo"><xsl:text>GNU Classpath Tools</xsl:text></a><xsl:text> (Gjdoc XmlDoclet </xsl:text><xsl:value-of select="$gjdoc.xmldoclet.version"/><xsl:text>).</xsl:text>
        </div>
        
        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_about" select="0"/>
          <xsl:with-param name="p_curr_about" select="1"/>
          <xsl:with-param name="p_top" select="0"/>
        </xsl:call-template>
        </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
