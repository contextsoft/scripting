object frmTextReporting: TfrmTextReporting
  Left = 255
  Top = 117
  Width = 658
  Height = 575
  Caption = 'Text Reporting Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 317
    Width = 650
    Height = 5
    Cursor = crVSplit
    Align = alBottom
  end
  object memTemplate: TMemo
    Left = 0
    Top = 33
    Width = 650
    Height = 284
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      ''
      '#rem This template is used to produce simple database creation'
      '#rem SQL script based on TDatabaseSchema component.'
      ''
      '#section report_header before report'
      '#section tables in report on report.TableDefs'
      '#section fields in tables on tables.FieldDefs'
      '#section fields_footer after fields'
      '#section indexes in tables on tables.IndexDefs'
      '#section report_footer after report'
      ''
      '#begin report_header'
      '/************************************************************/'
      '/* Database Schema: <%=report.SchemaName%>'
      '/************************************************************/'
      ''
      '#end'
      ''
      '#begin report_footer '
      ''
      '/************************************************************/'
      '#end'
      ''
      '#begin tables'
      ''
      'CREATE TABLE <%=tables.TableName%> ('
      '#end'
      ''
      '#begin fields delimiter '#39','#39
      ''
      
        '   <%=fields.DataType%> <%=fields.Name%><%=iff(fields.Required, ' +
        #39' NOT NULL'#39', '#39#39')%>'
      '#end'
      ''
      '#begin fields_footer newline'
      ');'
      ''
      '#end'
      ''
      '#begin indexes'
      ''
      
        'CREATE INDEX <%=indexes.Name%> ON <%=tables.TableName%>(<%=index' +
        'es.Fields%>);'
      ''
      '#end')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Tag = 1
      Left = 160
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Generate'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object memResult: TMemo
    Tag = 2
    Left = 0
    Top = 322
    Width = 650
    Height = 219
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object CtxTextReporter: TCtxTextReporter
    Tag = 2
    Prepared = False
    Left = 496
    Top = 128
  end
  object DatabaseSchema: TDatabaseSchema
    Relationships = <
      item
        Name = 'Items_Customers'
        DetailTableName = 'Items'
        DetailKeyFields = 'CustomerID'
        DetailRelationName = 'Customers'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Items'
        MasterRecordOptional = False
        DeleteAction = raCascade
        UpdateAction = raCascade
        ItemID = 1
      end
      item
        Name = 'Items1_Owner'
        DetailTableName = 'Items'
        DetailKeyFields = 'CustomerID'
        DetailRelationName = 'Owner'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Items1'
        MasterRecordOptional = False
        RequireRecordErrorMessage = 
          'Field CustomerID in the Items table does not contain a valid or ' +
          'null reference to the Customers table.'
        ItemID = 2
      end
      item
        Name = 'Items1_Location'
        DetailTableName = 'Items'
        DetailKeyFields = 'LocationID'
        DetailRelationName = 'Location'
        MasterTableName = 'Locations'
        MasterKeyFields = 'LocationID'
        MasterRelationName = 'Items1'
        RequireRecordErrorMessage = 
          'Field LocationID in the Items table does not contain a valid or ' +
          'null reference to the Locations table.'
        ItemID = 3
      end
      item
        Name = 'Items_Vault'
        DetailTableName = 'Items'
        DetailKeyFields = 'ItemID'
        DetailRelationName = 'Vault'
        MasterTableName = 'VaultReserv'
        MasterKeyFields = 'ItemID'
        MasterRelationName = 'Items'
        DetailCardinality = dcOne
        MasterRecordOptional = False
        DeleteAction = raCascade
        UpdateAction = raCascade
        ItemID = 4
      end
      item
        Name = 'Items_Locations'
        DetailTableName = 'Items'
        DetailKeyFields = 'LocationID'
        DetailRelationName = 'Locations'
        MasterTableName = 'Locations'
        MasterKeyFields = 'LocationID'
        MasterRelationName = 'Items'
        DeleteAction = raError
        UpdateAction = raError
        DeleteErrorMessage = 
          'Record in the Locations table cannot be deleted, because it alre' +
          'ady has related records in the Items table. Please delete those ' +
          'records prior to deleting the master record.'
        UpdateErrorMessage = 
          'Field LocationID in the Locations table cannot be updated, becau' +
          'se it already references records in the Items table.'
        ItemID = 5
      end
      item
        Name = 'Vault_Locations'
        DetailTableName = 'VaultReserv'
        DetailKeyFields = 'VaultID'
        DetailRelationName = 'Locations'
        MasterTableName = 'Locations'
        MasterKeyFields = 'LocationID'
        MasterRelationName = 'Vault'
        MasterRecordOptional = False
        DeleteAction = raError
        UpdateAction = raError
        DeleteErrorMessage = 
          'Record in the Locations table cannot be deleted, because it alre' +
          'ady has related records in the VaultReserv table. Please delete ' +
          'those records prior to deleting the master record.'
        UpdateErrorMessage = 
          'Field LocationID in the Locations table cannot be updated, becau' +
          'se it already references records in the VaultReserv table.'
        ItemID = 6
      end
      item
        Name = 'VaultReserv_Location'
        DetailTableName = 'VaultReserv'
        DetailKeyFields = 'VaultID'
        DetailRelationName = 'Location'
        MasterTableName = 'Locations'
        MasterKeyFields = 'LocationID'
        MasterRelationName = 'VaultReserv'
        MasterRecordOptional = False
        RequireRecordErrorMessage = 
          'Field VaultID in the VaultReserv table does not contain a valid ' +
          'or null reference to the Locations table.'
        ItemID = 7
      end
      item
        Name = 'VaultReserv_Item'
        DetailTableName = 'VaultReserv'
        DetailKeyFields = 'ItemID'
        DetailRelationName = 'Item'
        MasterTableName = 'Items'
        MasterKeyFields = 'ItemID'
        MasterRelationName = 'VaultReserv'
        DetailCardinality = dcOne
        MasterRecordOptional = False
        RequireRecordErrorMessage = 
          'Field ItemID in the VaultReserv table does not contain a valid o' +
          'r null reference to the Items table.'
        ItemID = 8
      end>
    TableDefs = <
      item
        Name = 'Customers'
        Description = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Description = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            Identity = True
            ItemID = 9
          end
          item
            Name = 'FirstName'
            Description = 'First Name'
            DataType = ftString
            Size = 50
            ItemID = 10
          end
          item
            Name = 'LastName'
            Description = 'Last Name'
            DataType = ftString
            Size = 50
            ItemID = 11
          end
          item
            Name = 'Address'
            Description = 'Address'
            DataType = ftString
            Size = 50
            ItemID = 12
          end>
        IndexDefs = <
          item
            Name = '<Primary>'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 13
          end>
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 14
      end
      item
        Name = 'Items'
        Description = 'Items'
        FieldDefs = <
          item
            Name = 'ItemID'
            Description = 'Item #'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            Identity = True
            ItemID = 15
          end
          item
            Name = 'CustomerID'
            Description = 'Owner #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 16
          end
          item
            Name = 'Description'
            Description = 'Description'
            DataType = ftString
            Size = 80
            ItemID = 17
          end
          item
            Name = 'ItemLocation'
            Description = 'Item'#39's Location'
            Required = True
            Attributes = [faRequired]
            DataType = ftSmallint
            ItemID = 18
          end
          item
            Name = 'TransferDate'
            Description = 'Transfer Date'
            DataType = ftDate
            ItemID = 19
          end
          item
            Name = 'LocationID'
            Description = 'Location #'
            DataType = ftInteger
            ItemID = 20
          end>
        IndexDefs = <
          item
            Name = '<Primary>'
            IndexFields = <
              item
                Name = 'ItemID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 21
          end
          item
            Name = 'ByCustomerID'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 22
          end
          item
            Name = 'ByLocationID'
            IndexFields = <
              item
                Name = 'LocationID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 23
          end>
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 24
      end
      item
        Name = 'Locations'
        Description = 'Locations'
        FieldDefs = <
          item
            Name = 'LocationID'
            Description = 'Location #'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            Identity = True
            ItemID = 25
          end
          item
            Name = 'Name'
            Description = 'Name'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 50
            ItemID = 26
          end
          item
            Name = 'Address'
            Description = 'Address'
            DataType = ftString
            Size = 80
            ItemID = 27
          end
          item
            Name = 'LocationType'
            Description = 'Location Type'
            Required = True
            Attributes = [faRequired]
            DataType = ftSmallint
            Enumeration = 'LocationType'
            ItemID = 28
          end>
        IndexDefs = <
          item
            Name = '<Primary>'
            IndexFields = <
              item
                Name = 'LocationID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 29
          end>
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 30
      end
      item
        Name = 'VaultReserv'
        Description = 'Vault'
        FieldDefs = <
          item
            Name = 'ItemID'
            Description = 'ItemID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 31
          end
          item
            Name = 'VaultID'
            Description = 'Vault #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 32
          end
          item
            Name = 'VaultNo'
            Description = 'Vault No'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 33
          end>
        IndexDefs = <
          item
            Name = '<Primary>'
            IndexFields = <
              item
                Name = 'ItemID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 34
          end
          item
            Name = 'ByVaultIDVaultNo'
            IndexFields = <
              item
                Name = 'VaultID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end
              item
                Name = 'VaultNo'
                Descending = False
                CaseInsensitive = False
                ItemID = 2
              end>
            Options = [ixUnique]
            ItemID = 35
          end>
        ObjectType = 'Vault'
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 36
      end>
    Enumerations = <
      item
        Name = 'LocationType'
        Description = 'Location Type'
        Items.Strings = (
          '0=Vault'
          '1=Store'
          '2=Contractor')
        Descriptions.Strings = (
          '0=Vault'
          '1=Store'
          '2=Contractor')
        ShortDescriptions.Strings = (
          '0=Vault'
          '1=Store'
          '2=Contractor')
        DisplayLabel = 'Location Type'
        TypePrefix = 'lt'
        IntConsts = True
        ItemID = 37
      end
      item
        Name = 'ItemLocation'
        Description = 'Item'#39's Location'
        Items.Strings = (
          '0=Owner'
          '1=Store'
          '2=Vault'
          '3=Contractor')
        Descriptions.Strings = (
          '0=Possessed by owner'
          '1=Pulled to store'
          '2=Stored in vault'
          '3=Sent to contractor')
        DisplayLabel = 'Item'#39's Location'
        TypePrefix = 'il'
        IntConsts = True
        ItemID = 38
      end>
    SchemaName = 'Database'
    Left = 496
    Top = 72
    SchemaGUID = '{74580428-60EF-49F3-B13A-777480B3BC75}'
  end
end
