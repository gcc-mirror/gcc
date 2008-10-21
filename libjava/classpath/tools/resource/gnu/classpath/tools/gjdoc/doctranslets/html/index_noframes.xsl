<?xml version="1.0" encoding="utf-8"?>

<!-- index_noframes.xsl
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

<!-- Creates the index.html file for HTML documentation. This is only
     a simple frameset.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="xml"
    encoding="utf-8"
    doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template name="output_packagedoc_tr">
    <tr>
      <td class="package-link">
        <a href="{concat(translate(@name,'.','/'), '/package-summary.html')}" class="package-link">
          <xsl:value-of select="@name"/>
        </a>
      </td>
      <td class="package-summary-description">
        <xsl:apply-templates select="./gjdoc:firstSentenceTags/node()"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="/">
      <html>
        <head>
          <xsl:call-template name="output_title">
            <xsl:with-param name="p_pagetitle" select="'Overview'"/>
          </xsl:call-template>
          <xsl:call-template name="include_common"/>
        </head>
        <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)">

          <!-- Top Navigation Bar -->
          <xsl:call-template name="output_navbar">
            <xsl:with-param name="p_show_frames" select="1"/>
            <xsl:with-param name="p_show_noframes" select="0"/>
            <xsl:with-param name="p_curr_noframes" select="1"/>
            <xsl:with-param name="p_show_package" select="0"/>
            <xsl:with-param name="p_show_package_tree" select="0"/>
            <xsl:with-param name="p_show_full_tree" select="1"/>
            <xsl:with-param name="p_show_index" select="1"/>
            <xsl:with-param name="p_show_help" select="1"/>
            <xsl:with-param name="p_top" select="1"/> 
          </xsl:call-template>

          <div class="pagebody">

          <xsl:if test="$gjdoc.option.doctitle">
            <h1 class="overview-doctitle"><xsl:value-of select="$gjdoc.option.doctitle"/></h1>
          </xsl:if>
          <xsl:if test="/gjdoc:rootdoc/gjdoc:firstSentenceTags">
            <div class="overview-description-top">
              <xsl:for-each select="/gjdoc:rootdoc/gjdoc:firstSentenceTags/node()">
                <xsl:copy-of select="."/>
              </xsl:for-each>
            </div>
            <div class="classdoc-tag-section-header">See Also:</div>
            <dl class="classdoc-list">
              <dt><a href="#description">Description</a></dt>
            </dl>
          </xsl:if>

          <xsl:choose>
            <xsl:when test="/gjdoc:rootdoc/gjdoc:packagegroup">
              <xsl:for-each select="/gjdoc:rootdoc/gjdoc:packagegroup">
                <h1 class="classdoc-title"><xsl:value-of select="@name"/></h1>
                <table border="1" cellspacing="0" class="classdoc-table">
                  <xsl:for-each select="gjdoc:package">
                    <xsl:variable name="v_name" select="@name"/>
                    <xsl:for-each select="/gjdoc:rootdoc/gjdoc:packagedoc[@name=$v_name]">
                      <xsl:call-template name="output_packagedoc_tr"/>
                    </xsl:for-each>
                  </xsl:for-each>
                </table>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <h1 class="classdoc-title">All Packages</h1>
              <table border="1" cellspacing="0" class="classdoc-table">
                <xsl:for-each select="/gjdoc:rootdoc/gjdoc:packagedoc">
                  <xsl:call-template name="output_packagedoc_tr"/>
                </xsl:for-each>
              </table>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:if test="/gjdoc:rootdoc/gjdoc:inlineTags">
            <a name="description"/>
            <div class="overview-description-full">
              <xsl:for-each select="/gjdoc:rootdoc/gjdoc:inlineTags/node()">
                <xsl:copy-of select="."/>
              </xsl:for-each>
            </div>
          </xsl:if>
          </div>
          
          <!-- Bottom Navigation Bar -->
          <xsl:call-template name="output_navbar">
            <xsl:with-param name="p_show_frames" select="1"/>
            <xsl:with-param name="p_show_noframes" select="0"/>
            <xsl:with-param name="p_curr_noframes" select="1"/>
            <xsl:with-param name="p_show_package" select="0"/>
            <xsl:with-param name="p_show_package_tree" select="0"/>
            <xsl:with-param name="p_show_full_tree" select="1"/>
            <xsl:with-param name="p_show_index" select="1"/>
            <xsl:with-param name="p_show_help" select="1"/>
            <xsl:with-param name="p_top" select="0"/> 
          </xsl:call-template>

        </body>
      </html>
  </xsl:template>
</xsl:stylesheet>
