IF OBJECT_ID('dbo.MovieReviews', 'U') IS NOT NULL 
  DROP TABLE dbo.MovieReviews; 

CREATE TABLE [dbo].[MovieReviews](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [varchar](150) NOT NULL,
	[The_Cleanse] [int] NULL,
	[Downsizing] [int] NULL,
	[Lady_Bird] [int] NULL,
	[Get_Out] [int] NULL,
	[The_Shape_of_Water] [int] NULL,
	[The_Post] [int] NULL,
 CONSTRAINT [PK_MovieReviews] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('Carlos' ,1 ,4 ,5 ,3 ,3 ,2)

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('Victoria' ,3 ,4 ,2 ,1 ,2 ,3)

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('John' ,2 ,3 ,5 ,4 ,2 ,1)

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('Ahmed' ,5 ,2 ,5 ,3 ,1 ,3)

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('Paul' ,4 ,2 ,4 ,3 ,2 ,1)

INSERT INTO [dbo].[MovieReviews] ([Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post])
     VALUES  ('Lou' ,5 ,4 ,4 ,3 ,2 ,4)

SELECT ID, [Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post] FROM [dbo].[MovieReviews]


