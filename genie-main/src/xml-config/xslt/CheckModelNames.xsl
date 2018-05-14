<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
>
<!--
    RF est 25th Jan 2008
    CheckModelNames: check that the model names in the
    supplied config xml file exist in the definitions xml.
-->

<xsl:output method="text"/>

<!-- params can be set by the calling xslt processor.
     Defaults are provided below  -->

<!-- if verbose is 'yes' output info about what is being tested,
     if 'no', just output if there is an error  -->
<xsl:param name="verbose" select="'yes'"/>
<!-- If terminate is 'yes' we stop on the first error,
     else ('no') we output all errors that we find -->
<xsl:param name="terminate" select="'yes'"/>

<!-- default location of the definition file -->
<xsl:param name="definitions" select="'definition.xml'"/>

<xsl:template match="/">

<xsl:if test="$verbose='yes'">
  <xsl:text>Checking that the config model names are valid
</xsl:text>
</xsl:if>

<xsl:if test="$verbose='yes'">
  <xsl:text>  Checking config model names
</xsl:text>
</xsl:if>

<xsl:for-each select="job/config/model">

  <xsl:variable name="ModelName" select="@name"/>

  <xsl:if test="$verbose='yes'">
    <xsl:text>    Checking model </xsl:text>
    <xsl:value-of select="$ModelName"/>
    <xsl:text> : </xsl:text>
  </xsl:if>

  <xsl:choose>
    <xsl:when test="document($definitions)/definition/parameters/model[@name=$ModelName]">
      <xsl:if test="$verbose='yes'">
        <xsl:text>OK
</xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Error, did not find config/model name </xsl:text>
      <xsl:value-of select="$ModelName"/>
      <xsl:text> in definition.xml
</xsl:text>
      <xsl:choose>
        <xsl:when test="$terminate='yes'">
          <xsl:text>Aborting as requested
</xsl:text>
          <xsl:message terminate="yes"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="no"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>

</xsl:for-each>

<xsl:if test="$verbose='yes'">
  <xsl:text>  Checking parameter model names
</xsl:text>
</xsl:if>

<xsl:for-each select="job/parameters/model">

  <xsl:variable name="ModelName" select="@name"/>

  <xsl:if test="$verbose='yes'">
    <xsl:text>    Checking model </xsl:text>
    <xsl:value-of select="$ModelName"/>
    <xsl:text> : </xsl:text>
  </xsl:if>

  <xsl:choose>
    <xsl:when test="document($definitions)/definition/parameters/model[@name=$ModelName]">
      <xsl:if test="$verbose='yes'">
        <xsl:text>OK
</xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Error, did not find parameters/model name </xsl:text>
      <xsl:value-of select="$ModelName"/>
      <xsl:text> in definition.xml
</xsl:text>
      <xsl:choose>
        <xsl:when test="$terminate='yes'">
          <xsl:text>Aborting as requested
</xsl:text>
          <xsl:message terminate="yes"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="no"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>

</xsl:for-each>

</xsl:template>

</xsl:stylesheet>
