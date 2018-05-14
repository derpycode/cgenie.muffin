<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<xsl:param name="DIR_SEP" select="'/'"/>

  <xsl:template match="/">
    <!--note, <job> is the top-level element after definition
        and job files have been merged-->
    <xsl:apply-templates select="job"/>
  </xsl:template>
  
  <xsl:template match="job">
   <xsl:apply-templates select="testing"/>
  </xsl:template>
  
  <xsl:template match="testing">
   <xsl:apply-templates select="var"/>
  </xsl:template>

  <xsl:template match="var">
    <xsl:value-of select="normalize-space(@name)"/>
    <xsl:text>=</xsl:text>
     <xsl:for-each select="node()">
      <xsl:choose>
       <xsl:when test="local-name()='varref'">
        <xsl:value-of select="normalize-space(/job/vars/var[@name=normalize-space(current()/text())]/text())"/>
       </xsl:when>
       <xsl:when test="local-name()='sep'">
        <xsl:value-of select="$DIR_SEP"/>
       </xsl:when>
       <xsl:otherwise>
        <xsl:value-of select="normalize-space(.)"/>
       </xsl:otherwise>
      </xsl:choose>
     </xsl:for-each>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
