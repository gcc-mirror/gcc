<?xml version="1.0" encoding="utf-8"?>

<!-- allclasses.xsl
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

<!-- Creates the allclasses.html file for HTML documentation. 
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
          <title>
            <xsl:value-of select="concat(/gjdoc:rootdoc/gjdoc:title, ' - All Classes')"/>
          </title>
          <xsl:call-template name="include_common"/>
        </head>
        <body onload="secondaryPageLoaded('classes');" class="menu allclasses">
          <h3 class="menu-title">All Classes</h3>
          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@name">
            <xsl:sort select="." order="ascending"/>
            <xsl:variable name="v_url" select="concat(translate(../gjdoc:containingPackage/@name,'.','/'), '/', ../@name, '.html')"/>
            <a href="{$v_url}" target="content" class="menu allclasses">
              <xsl:value-of select="."/>
            </a>
            <br/>
          </xsl:for-each>
        </body>
      </html>
  </xsl:template>

</xsl:stylesheet>
