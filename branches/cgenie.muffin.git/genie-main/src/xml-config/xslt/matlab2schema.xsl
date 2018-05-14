<?xml version="1.0" encoding="UTF-8"?>
<!-- Stylesheet for the auto-generation of schemas describing the structure
and content of configuration metadata for modules of the GENIE framework.

Copyright 2008 GENIE Project, University of Southampton
Andrew Price, $Date$
$Revision$
GENIE Toolbox for Matlab -->
<xsl:stylesheet version="1.0" xmlns="http://www.genie.ac.uk/schema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xdb="http://xmlns.oracle.com/xdb">

	<xsl:include href="complexType.xsl"/>

	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xsl:param name="date" select="'date unknown'"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/">
		<xsl:element name="xs:schema">
			<xsl:attribute name="targetNamespace">http://www.genie.ac.uk/schema</xsl:attribute>
			<xsl:attribute name="xdb:storeVarrayAsTable">true</xsl:attribute>
			<xsl:attribute name="elementFormDefault">qualified</xsl:attribute>
			<xsl:element name="xs:annotation">
				<xsl:element name="xs:documentation">
					Auto-generated GENIE database composite schema. Created by $Revision$ of matlab2schema.xsl on <xsl:value-of select="$date"/>.
				</xsl:element>
			</xsl:element>
			<xsl:call-template name="parameterTypes"/>
			<xsl:element name="xs:include">
				<xsl:attribute name="schemaLocation">http://www.genie.ac.uk/schema/genie-datatypes.xsd</xsl:attribute>
			</xsl:element>
			<xsl:element name="xs:include">
				<xsl:attribute name="schemaLocation">http://www.genie.ac.uk/schema/dif_v9.4.xsd</xsl:attribute>
			</xsl:element>
			<xsl:element name="xs:element">
				<xsl:attribute name="name">GenieMetadata</xsl:attribute>
				<xsl:element name="xs:complexType">
					<xsl:element name="xs:choice">
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Experiment</xsl:attribute>
							<xsl:attribute name="type">Experiment</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Simulation</xsl:attribute>
							<xsl:attribute name="type">Simulation</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Run</xsl:attribute>
							<xsl:attribute name="type">Run</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">File</xsl:attribute>
							<xsl:attribute name="type">File</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">DataFile</xsl:attribute>
							<xsl:attribute name="type">DataFile</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Result</xsl:attribute>
							<xsl:attribute name="type">Result</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">DerivedResult</xsl:attribute>
							<xsl:attribute name="type">DerivedResult</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Person</xsl:attribute>
							<xsl:attribute name="type">Person</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">Resource</xsl:attribute>
							<xsl:attribute name="type">Resource</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">DIF</xsl:attribute>
							<xsl:attribute name="type">DIF</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_main</xsl:attribute>
							<xsl:attribute name="type">genie_main</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_atchem</xsl:attribute>
							<xsl:attribute name="type">genie_atchem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_biogem</xsl:attribute>
							<xsl:attribute name="type">genie_biogem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_biogenie</xsl:attribute>
							<xsl:attribute name="type">genie_biogenie</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_cgoldstein</xsl:attribute>
							<xsl:attribute name="type">genie_cgoldstein</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_embm</xsl:attribute>
							<xsl:attribute name="type">genie_embm</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_fixedatmos</xsl:attribute>
							<xsl:attribute name="type">genie_fixedatmos</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_fixedchem</xsl:attribute>
							<xsl:attribute name="type">genie_fixedchem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_fixedicesheet</xsl:attribute>
							<xsl:attribute name="type">genie_fixedicesheet</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_fixedocean</xsl:attribute>
							<xsl:attribute name="type">genie_fixedocean</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_fixedseaice</xsl:attribute>
							<xsl:attribute name="type">genie_fixedseaice</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_gem</xsl:attribute>
							<xsl:attribute name="type">genie_gem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_goldstein</xsl:attribute>
							<xsl:attribute name="type">genie_goldstein</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_icesheet</xsl:attribute>
							<xsl:attribute name="type">genie_icesheet</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_ichem</xsl:attribute>
							<xsl:attribute name="type">genie_ichem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_igcm3</xsl:attribute>
							<xsl:attribute name="type">genie_igcm3</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_land</xsl:attribute>
							<xsl:attribute name="type">genie_land</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_seaice</xsl:attribute>
							<xsl:attribute name="type">genie_seaice</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_sedgem</xsl:attribute>
							<xsl:attribute name="type">genie_sedgem</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_simpleland</xsl:attribute>
							<xsl:attribute name="type">genie_simpleland</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_slabocean</xsl:attribute>
							<xsl:attribute name="type">genie_slabocean</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:element name="xs:element">
							<xsl:attribute name="name">genie_slabseaice</xsl:attribute>
							<xsl:attribute name="type">genie_slabseaice</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">1</xsl:attribute>
							<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
						</xsl:element>
						<xsl:apply-templates/>
						<xsl:element name="xs:sequence">
							<xsl:element name="xs:any">
								<xsl:attribute name="processContents">skip</xsl:attribute>
								<xsl:attribute name="minOccurs">0</xsl:attribute>
								<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
								<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
							</xsl:element>
						</xsl:element>
					</xsl:element>
					<xsl:element name="xs:attribute">
						<xsl:attribute name="name">idx</xsl:attribute>
						<xsl:attribute name="type">xs:string</xsl:attribute>
					</xsl:element>
					<xsl:element name="xs:attribute">
						<xsl:attribute name="name">type</xsl:attribute>
						<xsl:attribute name="type">xs:string</xsl:attribute>
					</xsl:element>
					<xsl:element name="xs:attribute">
						<xsl:attribute name="name">size</xsl:attribute>
						<xsl:attribute name="type">xs:string</xsl:attribute>
					</xsl:element>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="definition|job">
		<xsl:element name="xs:element">
			<xsl:attribute name="name">job</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:choice">
					<xsl:element name="xs:element">
						<xsl:attribute name="name">author</xsl:attribute>
						<xsl:element name="xs:complexType">
							<xsl:element name="xs:simpleContent">
								<xsl:element name="xs:extension">
									<xsl:attribute name="base">xs:string</xsl:attribute>
									<!-- <xsl:attribute name="xdb:SQLType">VARCHAR2</xsl:attribute> -->
									<xsl:element name="xs:attribute">
										<xsl:attribute name="name">idx</xsl:attribute>
										<xsl:attribute name="type">xs:string</xsl:attribute>
									</xsl:element>
									<xsl:element name="xs:attribute">
										<xsl:attribute name="name">type</xsl:attribute>
										<xsl:attribute name="type">xs:string</xsl:attribute>
									</xsl:element>
									<xsl:element name="xs:attribute">
										<xsl:attribute name="name">size</xsl:attribute>
										<xsl:attribute name="type">xs:string</xsl:attribute>
									</xsl:element>
								</xsl:element>
							</xsl:element>
						</xsl:element>
					</xsl:element>
					<xsl:apply-templates/>
					<xsl:element name="xs:sequence">
						<xsl:element name="xs:any">
							<xsl:attribute name="processContents">skip</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
							<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
						</xsl:element>
					</xsl:element>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">idx</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">type</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">size</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="vars">
		<xsl:call-template name="complexType">
			<xsl:with-param name="level">0</xsl:with-param>
			<xsl:with-param name="typename">
				<xsl:value-of select="local-name()"/>
			</xsl:with-param>
			<xsl:with-param name="addAnyElement">true</xsl:with-param>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="config">
		<xsl:call-template name="complexType">
			<xsl:with-param name="level">-1</xsl:with-param>
			<xsl:with-param name="typename">
				<xsl:value-of select="local-name()"/>
			</xsl:with-param>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="build">
		<xsl:element name="xs:element">
			<xsl:attribute name="name">build</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:choice">
					<xsl:for-each select="*">
						<xsl:choose>
							<xsl:when test="local-name()='make_arg'">
								<xsl:call-template name="complexType">
									<xsl:with-param name="level">0</xsl:with-param>
									<xsl:with-param name="typename">
										<xsl:value-of select="local-name()"/>
									</xsl:with-param>
									<xsl:with-param name="addAnyElement">true</xsl:with-param>
								</xsl:call-template>
							</xsl:when>
							<xsl:when test="local-name()='macro'">
								<xsl:call-template name="complexType">
									<xsl:with-param name="level">-1</xsl:with-param>
									<xsl:with-param name="typename">
										<xsl:value-of select="local-name()"/>
									</xsl:with-param>
								</xsl:call-template>
							</xsl:when>
						</xsl:choose>
					</xsl:for-each>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="testing">
		<xsl:call-template name="complexType">
			<xsl:with-param name="level">0</xsl:with-param>
			<xsl:with-param name="typename">
				<xsl:value-of select="local-name()"/>
			</xsl:with-param>
			<xsl:with-param name="addAnyElement">true</xsl:with-param>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="parameters">
		<xsl:element name="xs:element">
			<xsl:attribute name="name">parameters</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:choice">
					<xsl:for-each select="*">
						<xsl:call-template name="parameter.model">
							<xsl:with-param name="modelname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:for-each>
					<xsl:element name="xs:sequence">
						<xsl:element name="xs:any">
							<xsl:attribute name="processContents">skip</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
							<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
						</xsl:element>
					</xsl:element>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">idx</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">type</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">size</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template name="parameter.model">
		<xsl:param name="modelname"/>
		<xsl:element name="xs:element">
			<xsl:attribute name="name">
				<xsl:value-of select="$modelname"/>
			</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:choice">
					<xsl:for-each select="*">
						<xsl:call-template name="parameter.model.file">
							<xsl:with-param name="modelname">
								<xsl:value-of select="$modelname"/>
							</xsl:with-param>
							<xsl:with-param name="filename">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:for-each>
					<xsl:element name="xs:sequence">
						<xsl:element name="xs:any">
							<xsl:attribute name="processContents">skip</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
							<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
						</xsl:element>
					</xsl:element>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">idx</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">type</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">size</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template name="parameter.model.file">
		<xsl:param name="modelname"/>
		<xsl:param name="filename"/>
		<xsl:element name="xs:element">
			<xsl:attribute name="name">
				<xsl:value-of select="$filename"/>
			</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:choice">
					<xsl:for-each select="*">
						<xsl:call-template name="parameter.model.file.namelist">
							<xsl:with-param name="modelname">
								<xsl:value-of select="$modelname"/>
							</xsl:with-param>
							<xsl:with-param name="filename">
								<xsl:value-of select="$filename"/>
							</xsl:with-param>
							<xsl:with-param name="nmlname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:for-each>
					<xsl:element name="xs:sequence">
						<xsl:element name="xs:any">
							<xsl:attribute name="processContents">skip</xsl:attribute>
							<xsl:attribute name="minOccurs">0</xsl:attribute>
							<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
							<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
						</xsl:element>
					</xsl:element>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">idx</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">type</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">size</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template name="parameter.model.file.namelist">
		<xsl:param name="modelname"/>
		<xsl:param name="filename"/>
		<xsl:param name="nmlname"/>
		<xsl:element name="xs:element">
			<xsl:attribute name="name">
				<xsl:value-of select="$nmlname"/>
			</xsl:attribute>
			<xsl:attribute name="type">
				<xsl:value-of select="$modelname"/>
				<xsl:text>.</xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text>.</xsl:text>
				<xsl:value-of select="$nmlname"/>
			</xsl:attribute>
			<xsl:attribute name="minOccurs">0</xsl:attribute>
			<xsl:attribute name="maxOccurs">1</xsl:attribute>
			<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
		</xsl:element>
	</xsl:template>

	<xsl:template name="parameterTypes">
		<xsl:for-each select="/job/parameters/*|/definition/parameters/*">
			<xsl:call-template name="parameterTypes.model">
				<xsl:with-param name="modelname">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameterTypes.model">
		<xsl:param name="modelname"/>
		<xsl:for-each select="*">
			<xsl:call-template name="parameterTypes.model.namelist">
				<xsl:with-param name="modelname">
					<xsl:value-of select="$modelname"/>
				</xsl:with-param>
				<xsl:with-param name="nmlname">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameterTypes.model.namelist">
		<xsl:param name="modelname"/>
		<xsl:param name="nmlname"/>
		<xsl:for-each select="*">
			<xsl:element name="xs:include">
				<xsl:attribute name="schemaLocation">
					<xsl:text>http://www.genie.ac.uk/schema/</xsl:text>
					<xsl:value-of select="$modelname"/>
					<xsl:text>.</xsl:text>
					<xsl:value-of select="$nmlname"/>
					<xsl:text>.</xsl:text>
					<xsl:value-of select="local-name()"/>
					<xsl:text>.xsd</xsl:text>
				</xsl:attribute>
			</xsl:element>
		</xsl:for-each>
	</xsl:template>

</xsl:stylesheet>
