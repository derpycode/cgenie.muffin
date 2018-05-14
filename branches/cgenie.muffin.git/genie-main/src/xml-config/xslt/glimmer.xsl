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
 <xsl:apply-templates select="config"/>
</xsl:template>

<xsl:template match="job">
 <xsl:apply-templates select="parameters"/>
</xsl:template>

<!-- inherent clash here?
<xsl:template match="config">
 <xsl:apply-templates select="model"/>
</xsl:template>

<xsl:template match="parameters">
 <xsl:apply-templates select="model"/>
</xsl:template>
-->

<!-- the <config> version -->
<xsl:template match="config/model">
  <xsl:choose>
    <xsl:when test="@name='glimmer'">
      <xsl:value-of select="@name"/>
      <xsl:text>&#xa;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<!-- the <parameters> version -->
<xsl:template match="parameters/model">
  <xsl:choose>
    <xsl:when test="@name='glimmer'">
      <xsl:value-of select="normalize-space(.)"/>
      <xsl:text>&#xa;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>



<!-- will need this, I'm guessing

        <xsl:choose>
         <xsl:when test="local-name()='varref'">
          <xsl:value-of select="/job/vars/var[@name=normalize-space(current()/text())]/text()"/>
         </xsl:when>
         <xsl:when test="local-name()='sep'">
          <xsl:value-of select="$DIR_SEP"/>
         </xsl:when>
         <xsl:otherwise>
          <xsl:value-of select="normalize-space(.)"/>
         </xsl:otherwise>
        </xsl:choose>

-->

</xsl:stylesheet>
