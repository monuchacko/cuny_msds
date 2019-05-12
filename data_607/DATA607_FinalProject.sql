IF EXISTS(select * from sys.tables where name = 'tblAirlinesIncident') DROP TABLE tblAirlinesIncident
IF EXISTS(select * from sys.tables where name = 'tblAirlines') DROP TABLE tblAirlines
IF EXISTS(select * from sys.tables where name = 'tblIncidentTypeMst') DROP TABLE tblIncidentTypeMst
IF EXISTS(select * from sys.tables where name = 'tblYearRange') DROP TABLE tblYearRange


CREATE TABLE [dbo].[tblAirlines](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [varchar](150) NULL,
 CONSTRAINT [PK_tblAirlines] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

-----------------------------------------------------
CREATE TABLE [dbo].[tblIncidentTypeMst](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[IncidentType] [varchar](150) NULL,
 CONSTRAINT [PK_tblIncidentTypeMst] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

-----------------------------------------------------
CREATE TABLE [dbo].[tblYearRange](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[BeginYear] [date] NULL,
	[EndYear] [date] NULL,
 CONSTRAINT [PK_tblYearRange] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

-----------------------------------------------------

CREATE TABLE [dbo].[tblAirlinesIncident](
	[ID] [int] NULL,
	[AirlinesID] [int] NULL,
	[IncidentTypeID] [int] NULL,
	[YearRange] [int] NULL,
	[SeatCount] [int] NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[tblAirlinesIncident]  WITH CHECK ADD  CONSTRAINT [FK_tblAirlinesIncident_tblAirlines] FOREIGN KEY([AirlinesID])
REFERENCES [dbo].[tblAirlines] ([ID])
GO

ALTER TABLE [dbo].[tblAirlinesIncident] CHECK CONSTRAINT [FK_tblAirlinesIncident_tblAirlines]
GO

ALTER TABLE [dbo].[tblAirlinesIncident]  WITH CHECK ADD  CONSTRAINT [FK_tblAirlinesIncident_tblIncidentTypeMst] FOREIGN KEY([IncidentTypeID])
REFERENCES [dbo].[tblIncidentTypeMst] ([ID])
GO

ALTER TABLE [dbo].[tblAirlinesIncident] CHECK CONSTRAINT [FK_tblAirlinesIncident_tblIncidentTypeMst]
GO

ALTER TABLE [dbo].[tblAirlinesIncident]  WITH CHECK ADD  CONSTRAINT [FK_tblAirlinesIncident_tblYearRange] FOREIGN KEY([YearRange])
REFERENCES [dbo].[tblYearRange] ([ID])
GO

ALTER TABLE [dbo].[tblAirlinesIncident] CHECK CONSTRAINT [FK_tblAirlinesIncident_tblYearRange]
GO