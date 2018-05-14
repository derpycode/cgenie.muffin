<?xml version="1.0" encoding="utf-8"?>
<!-- Stylesheet for the auto-generation of schemas describing the structure
and content of configuration metadata for modules of the GENIE framework.
This stylesheet provides templates called by both matlab2schema.xsl and
parameters2types.xsl to consistently output the data types.

Copyright 2008 GENIE Project, University of Southampton
Andrew Price, $Date$
$Revision$
GENIE Toolbox for Matlab -->
<xsl:stylesheet version="1.0" xmlns="http://www.genie.ac.uk/schema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xdb="http://xmlns.oracle.com/xdb">

	<xsl:template name="complexType">
		<xsl:param name="addAnyElement">false</xsl:param>
		<xsl:param name="level"/>
		<xsl:param name="typename"/>
		<xsl:element name="xs:element">
			<xsl:attribute name="name">
				<xsl:value-of select="$typename"/>
			</xsl:attribute>
			<xsl:choose>
				<xsl:when test="$level=-3">
					<xsl:attribute name="minOccurs">0</xsl:attribute>
					<xsl:attribute name="xdb:SQLInline">false</xsl:attribute>
					<xsl:element name="xs:complexType">
						<xsl:element name="xs:choice">
							<xsl:for-each select="*">
								<xsl:call-template name="complexType">
									<xsl:with-param name="level">
										<xsl:value-of select="$level+1"/>
									</xsl:with-param>
									<xsl:with-param name="typename">
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
				</xsl:when>
				<xsl:when test="$level=-1">
					<xsl:element name="xs:complexType">
						<xsl:element name="xs:choice">
							<xsl:for-each select="*">
								<xsl:choose>
									<xsl:when test="string-length()>25">
										<xsl:element name="xs:element">
											<xsl:attribute name="name">
												<xsl:value-of select="local-name()"/>
											</xsl:attribute>
											<xsl:element name="xs:complexType">
												<xsl:element name="xs:sequence">
													<xsl:element name="xs:any">
														<xsl:attribute name="processContents">skip</xsl:attribute>
														<xsl:attribute name="minOccurs">0</xsl:attribute>
														<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
														<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
													</xsl:element>
												</xsl:element>
											</xsl:element>
										</xsl:element>
									</xsl:when>
									<xsl:otherwise>
										<xsl:call-template name="complexType">
											<xsl:with-param name="level">
												<xsl:value-of select="$level+1"/>
											</xsl:with-param>
											<xsl:with-param name="typename">
												<xsl:value-of select="local-name()"/>
											</xsl:with-param>
										</xsl:call-template>
									</xsl:otherwise>
								</xsl:choose>
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
				</xsl:when>
				<xsl:when test="$level=0">
					<xsl:element name="xs:complexType">
						<xsl:element name="xs:choice">
							<xsl:for-each select="*">
								<xsl:call-template name="leaf">
									<xsl:with-param name="leafname">
										<xsl:value-of select="local-name()"/>
									</xsl:with-param>
									<xsl:with-param name="leaftype">
										<xsl:value-of select="@type"/>
									</xsl:with-param>
								</xsl:call-template>
							</xsl:for-each>
							<xsl:if test="$addAnyElement='true'">
								<xsl:element name="xs:sequence">
									<xsl:element name="xs:any">
										<xsl:attribute name="processContents">skip</xsl:attribute>
										<xsl:attribute name="minOccurs">0</xsl:attribute>
										<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
										<xsl:attribute name="xdb:SQLType">CLOB</xsl:attribute>
									</xsl:element>
								</xsl:element>
							</xsl:if>
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
				</xsl:when>
				<xsl:otherwise>
					<xsl:element name="xs:complexType">
						<xsl:element name="xs:choice">
							<xsl:for-each select="*">
								<xsl:call-template name="complexType">
									<xsl:with-param name="level">
										<xsl:value-of select="$level+1"/>
									</xsl:with-param>
									<xsl:with-param name="typename">
										<xsl:value-of select="local-name()"/>
									</xsl:with-param>
								</xsl:call-template>
							</xsl:for-each>
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
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>

	<xsl:template name="leaf">
		<xsl:param name="leafname"/>
		<xsl:param name="leaftype"/>
		<xsl:element name="xs:element">
			<xsl:attribute name="name">
				<xsl:value-of select="$leafname"/>
			</xsl:attribute>
			<xsl:element name="xs:complexType">
				<xsl:element name="xs:simpleContent">
					<xsl:element name="xs:extension">
						<xsl:choose>
							<xsl:when test="$leaftype='double'">
								<xsl:attribute name="base">xs:float</xsl:attribute>
								<!-- <xsl:attribute name="xdb:SQLType">NUMBER</xsl:attribute> -->
							</xsl:when>
							<xsl:when test="$leaftype='int32'">
								<xsl:attribute name="base">xs:int</xsl:attribute>
								<!-- <xsl:attribute name="xdb:SQLType">NUMBER</xsl:attribute> -->
							</xsl:when>
							<xsl:when test="$leaftype='boolean' or $leaftype='logical'">
								<xsl:attribute name="base">xs:boolean</xsl:attribute>
								<!-- <xsl:attribute name="xdb:SQLType">RAW</xsl:attribute> -->
							</xsl:when>
							<xsl:when test="$leaftype='char'">
								<xsl:attribute name="base">xs:string</xsl:attribute>
								<!-- <xsl:attribute name="xdb:SQLType">VARCHAR2</xsl:attribute> -->
							</xsl:when>
							<xsl:otherwise>
								<xsl:attribute name="base">xs:string</xsl:attribute>
								<!-- <xsl:attribute name="xdb:SQLType">VARCHAR2</xsl:attribute> -->
							</xsl:otherwise>
						</xsl:choose>
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
	</xsl:template>

</xsl:stylesheet>