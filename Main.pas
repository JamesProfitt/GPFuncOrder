{*******************************************************}
{  GPFuncOrder                                          }
{  Author: James Profitt                                }
{          james.r.profitt@gmail.com                    }
{    Date: 15 Jan 2022                                  }
{*******************************************************}

unit Main;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Math,
  System.StrUtils,
  System.SysUtils,
  System.TypInfo,
  System.UITypes,
  System.Variants,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.Styles,
  Vcl.Themes,
  SynEdit,
  SynEditHelper,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynHighlighterSQL,
  HelperFunctions
  ;

type
  TfrmMain = class(TForm)
    { Theme Menu and initial item }
    { The list of styles are added dynamically }
    ThemeMenu: TMainMenu;
    StyleMenu: TMenuItem;
    { Top Panel and associated components }
    pTopPanel: TPanel;
    lIntroduction: TLabel;
    leDirectory: TLabeledEdit;
    bSelectFolder: TButton;
    lProgress: TLabel;
    pbProgressBar: TProgressBar;
    bAnalyze: TButton;
    bClose: TButton;
    { Bottom Panel and associated components }
    pLogPanel: TPanel;
    lLogLabel: TLabel;
    reLog: TRichEdit;
    smSQLMemo: TSynEdit;
    bSaveLog: TButton;
    bRunTest: TButton;
    bClearLog: TButton;
    { Components that have no visual parts }
    FileOpenDialog: TFileOpenDialog;
    FileSaveDialog: TSaveDialog;
    synSQLHighlighter: TSynSQLSyn;
    { Procedures called on certain events }
    procedure FormCreate(Sender: TObject);
    procedure bAnalyzeClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bRunTestClick(Sender: TObject);
    procedure bSaveLogClick(Sender: TObject);
    procedure bSelectFolderClick(Sender: TObject);
    procedure smSQLMemoClick(Sender: TObject);
    procedure leDirectoryKeyPress(Sender: TObject; var Key: Char);
    procedure bClearLogClick(Sender: TObject);
  private
    { Further routines }
    function RemoveComments(InputString: string; SchemaName: string): string;
    function IsFolderOK(iFolder: string) : boolean;
    procedure CreateOrderFiles;
    procedure GetDependencies;
    procedure FindSQLFiles;
    procedure ProcessSQLFiles;
    procedure RELogError(iText: string);
    procedure RELogWarning(iText: string);
    procedure RELogNote(iText: string);
    procedure StyleMenuClick(Sender: TObject);
  public
    { Public declarations }
  end;

{ Custom data structures for dependency processing }

type
  ObjOrder = record
    ObjSchema: string;
    ObjName: string;
    ObjType: string;
    ObjFQName: string;
    ObjOrder: Integer;
  end;

type
  FileData = record
    FileName: string;
    FolderName: string;
    ObjName: string;
    ObjType: string;
    Schema: string;
    FQName: string;
  end;

type
  ObjRelationships = record
    Parent: string;
    Child: string;
  end;

type
  SchemaFolder = record
    FolderName: string;
    SynHighlighterTarget: string;
    ObjType: string;
  end;

{ global variables and constants }

var
  frmMain: TfrmMain;
  DMLFileList: array of FileData;
  FileList: array of FileData;
  DepList: array of string;
  ObjList: array of ObjOrder;
  ObjDependencies: array of ObjRelationships;
  LogSaved: boolean;

const
  CRLF = #13#10;

{ TODO: Load the reqSchemaFolders array from an external source like a text    }
{       file. For now, hardcode it.                                            }
const
  reqSchemaFolders : array[0..6] of SchemaFolder =
  (
     (FolderName: 'Aggregates' ; SynHighlighterTarget: 'PROCNAME'  ; ObjType: 'aggregate'),
     (FolderName: 'Externals'  ; SynHighlighterTarget: 'TABLENAME' ; ObjType: 'external'),
     (FolderName: 'Functions'  ; SynHighlighterTarget: 'PROCNAME'  ; ObjType: 'function'),
     (FolderName: 'Sequences'  ; SynHighlighterTarget: 'TABLENAME' ; ObjType: 'sequence'),
     (FolderName: 'Tables'     ; SynHighlighterTarget: 'TABLENAME' ; ObjType: 'table'),
     (FolderName: 'Types'      ; SynHighlighterTarget: 'TABLENAME' ; ObjType: 'type'),
     (FolderName: 'Views'      ; SynHighlighterTarget: 'TABLENAME' ; ObjType: 'view')
  );

implementation

{$R *.dfm}

{ Runs on program initialization: }
procedure TfrmMain.FormCreate(Sender: TObject);
var
  Style: String;
  Item: TMenuItem;
  StyleList: TStringList;
  i: integer;
begin
  LogSaved := false;
  { Clear the design-time text lists }
  smSQLMemo.Lines.Clear;
  synSQLHighlighter.FunctionNames.Clear;
  synSQLHighlighter.ProcNames.Clear;
  synSQLHighlighter.TableNames.Clear;
  pbProgressBar.Style := pbstNormal;
  { Set Visible to True for these when testing, }
  { False when producing a release. }
  smSQLMemo.visible := false;
  bRunTest.Visible := false;

  { Add child menu items based on available styles.                   }
  { The menu items are dynamically created.                           }
  { Allows a user to select a style they like when using the program. }
  StyleList := TStringList.Create;
  try
    for Style in TStyleManager.StyleNames do
    begin
      StyleList.Add(Style);
    end;

    StyleList.Sorted := True;
    StyleList.CaseSensitive := False;

    for i := 0 to StyleList.Count - 1 do
    begin
      Item := TMenuItem.Create(StyleMenu);
      Item.Caption := StyleList.Strings[i];
      Item.OnClick := StyleMenuClick;
      if TStyleManager.ActiveStyle.Name = StyleList.Strings[i] then
        Item.Checked := true;
      StyleMenu.Add(Item);
    end;

  finally
    StyleList.Free;
  end;
end;

{ Set the style of the application. }
{ Completely unnecessary from a functionality perspective... looks cool though. }
{ Modified based on this: }
{ http://delphiprogrammingdiary.blogspot.com/2018/09/applying-theme-or-style-to-delphi.html }
procedure TfrmMain.StyleMenuClick(Sender: TObject);
var
  StyleName: String;
  i: Integer;
begin
  { get style name }
  StyleName := StringReplace(TMenuItem(Sender).Caption, '&', '',
    [rfReplaceAll, rfIgnoreCase]);
  { set active style }
  TStyleManager.SetStyle(StyleName);
  { check the currently selected menu item }
  (Sender as TMenuItem).Checked := true;
  { uncheck all other style menu items }
  for I := 0 to StyleMenu.Count -1 do begin
    if not StyleMenu.Items[i].Equals(Sender) then
      StyleMenu.Items[i].Checked := false;
  end;
end;

{ If the user presses enter when typing in the directory selection box, }
{ then we act as if you pressed the Analyze button. }
procedure TfrmMain.leDirectoryKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(13) then
    bAnalyzeClick(Sender);
end;

{ Clear Log Button: }
procedure TfrmMain.bClearLogClick(Sender: TObject);
begin
  { Only asks for confirmation if the log was not saved }
  if (ReLog.Lines.Count > 0) and (LogSaved = false) then
  begin
    if MessageDlg('The analyze log was not saved. Clear it anyway?', mtConfirmation, [mbYes , mbNo], 0, mbYes) = mrYes then
      RELog.Lines.Clear;
  end
  else
    RELog.Lines.Clear;
end;

{ Close Button: }
procedure TfrmMain.bCloseClick(Sender: TObject);
begin
  { Only asks for confirmation if the log was not saved }
  if (ReLog.Lines.Count > 0) and (LogSaved = false) then
  begin
    if MessageDlg('The analyze log was not saved. Continue with exit?', mtConfirmation, [mbYes , mbNo], 0, mbYes) = mrYes then
      Application.Terminate;
  end
  else
    Application.Terminate;
end;

{ Save Log button (only if there are lines to save): }
{ PlainText is set so that no RTF markup is included in the output }
{ The dialog also suggests a filename based on the date/time }
{ The user can still change this, so just use what the dialog returns }
procedure TfrmMain.bSaveLogClick(Sender: TObject);
var
  DTString: string;
begin
  DateTimeToString(DTString,'yyyymmmdd"_"hhnnss',Now());
  if reLog.Lines.Count > 0 then
  begin;
    FileSaveDialog.FileName := 'GPFuncOrdr_' + DTString + '.log';
    if FileSaveDialog.Execute then
    begin
      try
        reLog.PlainText := true;
        try
          reLog.Lines.SaveToFile(FileSaveDialog.FileName);
          LogSaved := true;
        except
          on E : Exception do
          begin
            MessageDlg('There was an error saving the file: ' + CRLF + E.ClassName + CRLF + CRLF + E.Message, mtError, [mbOK], 0, mbOK);
          end;
        end;
      finally
        reLog.PlainText := false;
      end;
    end;
  end
  else
    MessageDlg('Nothing to save!',mtInformation, [mbOK], 0, mbOK);
end;

{ The ellipsis button (...) to select the right folder            }
{ Execute returns False if the user clicked Cancel on the dialog. }
{ When True, they selected a valid directory, so use that. }
procedure TfrmMain.bSelectFolderClick(Sender: TObject);
begin
  if FileOpenDialog.Execute then
    leDirectory.Text := FileOpenDialog.FileName;
end;

{ The RunTest button is for testing only. }
procedure TfrmMain.bRunTestClick(Sender: TObject);
begin
  //showmessage(synSQLHighlighter.TableNames.Text);
  //RemoveComments('','schema_name');
end;

{ This is useful for testing to see what the tokens are tagged as }
{ This will only get called if the control is visible }
procedure TfrmMain.smSQLMemoClick(Sender: TObject);
var
  TokenType: Integer;
  TokenText: UnicodeString;
begin
  if TSynEdit(Sender).GetTokenInfo(TSynEdit(Sender).ScreenToClient(Mouse.CursorPos), TokenType, TokenText) then
    ShowMessage('Token Type: ' + GetEnumName(TypeInfo(TtkTokenKind), Ord(TokenType)) + '    Token Text: ' +  TokenText);
end;

{ Procedures to write formatted text to the Rich Edit box:             }
{ This is to help the user figure out what may have gone wrong, if the }
{ order files were not produced properly.                              }

{ Error Message, red text }
procedure TfrmMain.RELogError(iText: string);
begin
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelAttributes.Color := clRed;
  if reLog.GetTextLen = 0 then
    reLog.SelText := 'ERROR: '
  else
    reLog.SelText := CRLF + 'ERROR: ';
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelText := iText;
end;

{ Warning Message, navy text }
procedure TfrmMain.RELogWarning(iText: string);
begin
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelAttributes.Color := clNavy;
  if reLog.GetTextLen = 0 then
    reLog.SelText := 'WARNING: '
  else
    reLog.SelText := CRLF + 'WARNING: ';
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelText := iText;
end;

{ Note Message, black text }
procedure TfrmMain.RELogNote(iText: string);
begin
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelAttributes.Color := clBlack;
  if reLog.GetTextLen = 0 then
    reLog.SelText := 'NOTE: '
  else
    reLog.SelText := CRLF + 'NOTE: ';
  reLog.SelStart := reLog.GetTextLen;
  reLog.SelText := iText;
end;

{ Main execution begins with the user pressing the Analyze button: }
procedure TfrmMain.bAnalyzeClick(Sender: TObject);
begin
  { Ask the user if they want to save the previous log }
  if (ReLog.Lines.Count > 0) and (LogSaved = False) then
  begin
    if MessageDlg('The previous analyze log was not saved. Save it now?', mtConfirmation, [mbYes , mbNo], 0, mbYes) = mrYes then
      bSaveLogClick(Sender);
  end;

  LogSaved := False;
  try
    { Marquee style will make the progress bar active until we're done }
    pbProgressBar.Style := pbstMarquee;
    { Clear any lines in the RichEdit }
    RELog.Lines.Clear;

    { Add notes to show who did this }
    RELogNote('ANALYZE initiated at: ' + DateToStr(Now()) + ' ' + TimeToStr(Now()));
    RELogNote('User ID: ' + CurrentUserName);
    RELogNote('Machine Name: ' + CurrentMachineName);
    RELogNote('Directory Selected: ' + leDirectory.Text);

    { Check if there is anything in the text box }
    if Length(Trim(leDirectory.Text)) = 0 then
    begin
      RELogError('No directory selected!');
      { Don't continue }
      exit;
    end
    else
      RELogNote(leDirectory.Text + ' is a valid directory.');

    { Check if the folder selected exists. }
    if DirectoryExists(leDirectory.Text) = false then
    begin
      RELogError('Directory does not exist!');
      { Don't continue }
      exit;
    end
    else
      RELogNote(leDirectory.Text + ' is a valid directory.');

    { Check if the folder is of the right structure. }
    if IsFolderOK(leDirectory.Text) = false then
    begin
      RELogError('IsFolderOK Returned FALSE');
      { Don't continue }
      exit;
    end
    else
      RELogNote('IsFolderOK Returned TRUE');

    { Call the function to create the order files }
    CreateOrderFiles;

    { We've finished! }
    RELogNote('Finished at: ' + DateToStr(Now()) + ' ' + TimeToStr(Now()));
  finally
    { Reset the progress bar when finished }
    pbProgressBar.Style := pbstNormal;
  end;
end;

{ A function to check if a folder is of the right type (has right structure) }
function TfrmMain.IsFolderOK(iFolder: string) : boolean;
var
  SchemaSearch : TSearchRec;
  SchemaList : TStringList;
  SchemaOrderFile : TStringList;
  i : integer;
begin
  { Assume correct }
  result := true;

  { We expect the root folder to be called "GP", because this was originally written for a Greenplum codebase, and that was how it was structured }
  { Then, two folders called "DDL" and "DML". }
  if UpperCase(ExtractFileName(iFolder)) <> 'GP' then
    RELogWarning('Root folder not called "GP" (Original use case always had "GP" as the root, then DDL and DML subfolders).');

  if DirectoryExists(iFolder + '\DDL') = false then
  begin
    RELogError('The following folder does not exist: ' + iFolder + '\DDL');
    result := false;
    exit;
  end
  else
    RELogNote('Directory Found: ' + iFolder + '\DDL');

  { DML is optional }
  if DirectoryExists(iFolder + '\DML') = false then
    RELogWarning('The following optional folder does not exist: ' + iFolder + '\DML')
  else
    RELogNote('Directory Found: ' + iFolder + '\DML');

  { Within the DDL folder there should be a folder per schema.    }
  { Directly within that folder should be a single .sql file that }
  { contains the SQL code to create the schemas.                  }
  SchemaList := TStringList.Create();
  try
    SchemaList.CaseSensitive := False;

    RELogNote('Parsing DDL Directory Structure....');

    if FindFirst(iFolder + '\DDL\*', faDirectory, SchemaSearch) = 0 then
    begin
      try
        repeat
          { The NAME attribute contains the . and .. entries so we just ignore them }
          if AnsiPos('.',SchemaSearch.Name) = 0 then
          begin

            RELogNote('Directory Found: ' + iFolder + '\DDL\' + SchemaSearch.Name);

            { TODO: Check if the name is a valid postgres name? }
            { Maybe better to just leave it to GP to deal with, as it does not }
            { affect the ordering. }

            { For each new folder, run another check to ensure that the }
            { folder structure for each schema is in place: }

            { The list of required folders is stored in an array }
            { that we just loop over }

            for i := 0 to high(reqSchemaFolders) do
            begin

              if DirectoryExists(iFolder + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName) = false then
              begin
                RELogError('The following folder does not exist: ' + iFolder + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName);
                result := false;
                exit;
              end
              else
                RELogNote('Found: ' + iFolder + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName);

            end;

            { The schema file needs to exist: }
            if FileExists(iFolder + '\DDL\' + SchemaSearch.Name + '\' + SchemaSearch.Name + '.sql') = false then
            begin
              RELogError('The following file does not exist: ' + iFolder + '\DDL\' + SchemaSearch.Name + '\' + SchemaSearch.Name + '.sql');
              result := false;
              exit;
            end
            else
              RELogNote('Found: ' + iFolder + '\DDL\' + SchemaSearch.Name + '\' + SchemaSearch.Name + '.sql');

            { Schemas can be empty, it is not an error if there are no other SQL files }
            { present in the directory structure. }
            { Any folders other than the ones listed in reqSchemaFolders }
            { are ignored. }
            { TODO: Change this behaviour??? }
            { Add entry to the SchemaList: }
            SchemaList.Add(SchemaSearch.Name);
          end;
        until FindNext(SchemaSearch) <> 0;
      finally
        FindClose(SchemaSearch);
      end;
    end;

    { DML Checks: }
    { DML data is optional. }
    { However, there would be errors if a schema did not exist for which }
    { DML data existed (the dependent table would not exist). }

    if DirectoryExists(iFolder + '\DML') = true then
    begin
      if FindFirst(iFolder + '\DML\*', faDirectory, SchemaSearch) = 0 then
      begin
        try
          repeat
            { The NAME attribute contains the . and .. entries so we just ignore them }
            if AnsiPos('.',SchemaSearch.Name) = 0 then
            begin

              RELogNote('Directory Found: ' + iFolder + '\DML\' + SchemaSearch.Name);

              { Check if the SchemaSearch.Name is in the SchemaList }
              if SchemaList.IndexOf(SchemaSearch.Name) = -1 then
              begin
                RELogError('Found a DML folder that does not have an associated DDL folder: ' + SchemaSearch.Name);
                result := false;
                exit;
              end;

            end;
          until FindNext(SchemaSearch) <> 0;
        finally
          FindClose(SchemaSearch);
        end;
      end;
    end;

    { Create the file that will reference the creation of the schemas: }
    { Nothing after this point should make the function result false, unless }
    { the exception is triggered }

    SchemaOrderFile := TStringList.Create();
    try
      try
        { Add a header to the file. }
        SchemaOrderFile.Add('-- create_schemas.sql, created by GPProcOrder');
        { Each schema is added, with a lowercase file name, and uppercase directory }
        for i := 0 to SchemaList.Count - 1 do
        begin
          SchemaOrderFile.Add('\i ./DDL/' + UpperCase(SchemaList.Strings[i]) + '/' + LowerCase(SchemaList.Strings[i]) + '.sql');
        end;
        { Add a footer }
        SchemaOrderFile.Add('-- END OF FILE');
        { Save the file }
        SchemaOrderFile.SaveToFile(iFolder + '\create_schemas.sql');
        { Make a note that we've created the file }
        RELogNote(iFolder + '\create_schemas.sql' + ' was successfully saved.');
      except
        { Trap any exceptions (write-protected folder, for example) }
        on E : Exception do
        begin
          RELogError('There was an error creating and saving the "create_schemas.sql" file.');
          RELogError(E.ClassName);
          RELogError(E.Message);
          result := false;
        end;
      end;
    finally
      SchemaOrderFile.Free
    end;
    { Finished! }
    { This finally step frees the memory held by the SchemaList, even if there }
    { is an exception }
  finally
    SchemaList.Free;
  end;

end;

{ A procedure to create the order files. }
{ The following is called after the folder structure is deemed to be OK }
{ and the create_schemas.sql file is created. }
procedure TfrmMain.CreateOrderFiles;
begin
  { FindSQLFiles finds all the SQL files that we will process }
  { and adds all the function and table names to the Highlighter }
  FindSQLFiles;

  { ProcessSQLFiles and the subsequent call to RemoveComments }
  { will determine the immediate dependencies of each file present. }
  ProcessSQLFiles;

  { GetDependencies creates the full tree of dependencies, and saves the final output }
  { note that the output file contains forward slashes - these should work }
  { on both windows/linux }
  GetDependencies;
end;

{ This function iterates through the directories, and makes a list of all the }
{ SQL file, also adding their names to the SynEdit control so that dependencies }
{ can be detected as we iterate over the filelist in the next procedure. }
procedure TfrmMain.FindSQLFiles;
var
  SchemaSearch : TSearchRec;
  SQLFileSearch : TSearchRec;
  RecordCount: Integer;
  DMLCount: Integer;
  i: Integer;
begin
  { Initialize the arrays that GetDependencies will use. }
  RecordCount := 0;
  DMLCount := 0;
  SetLength(FileList, RecordCount);
  SetLength(DMLFileList, RecordCount);
  SetLength(ObjList, RecordCount);
  SetLength(ObjDependencies, RecordCount);

  { Loop Through Each Schema in the DDL folder: }

  if FindFirst(leDirectory.Text + '\DDL\*', faDirectory, SchemaSearch) = 0 then
  begin
    try
      repeat
        { Ignore the . and .. directories }
        if AnsiPos('.',SchemaSearch.Name) = 0 then
        begin

          { The inner loop is determined by the reqSchemaFolders array. }
          for i := 0 to high(reqSchemaFolders) do
          begin

            if FindFirst(leDirectory.Text + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName + '\*.sql', faAnyFile, SQLFileSearch) = 0 then
            begin
              try
                repeat
                  RELogNote('Found: ' + leDirectory.Text + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName + '\' + SQLFileSearch.Name);

                  { Add the name of the object to the dependency arrays: }
                  Inc(RecordCount);
                  SetLength(ObjList, RecordCount);
                  SetLength(FileList, RecordCount);
                  ObjList[RecordCount - 1].ObjSchema := AnsiUpperCase(SchemaSearch.Name);
                  ObjList[RecordCount - 1].ObjName := AnsiUpperCase(AnsiMidStr(SQLFileSearch.Name,1,length(SQLFileSearch.Name) - 4));
                  ObjList[RecordCount - 1].ObjType := AnsiUpperCase(reqSchemaFolders[i].ObjType);
                  FileList[RecordCount - 1].FileName := leDirectory.Text + '\DDL\' + SchemaSearch.Name + '\' + reqSchemaFolders[i].FolderName + '\' + SQLFileSearch.Name;
                  FileList[RecordCount - 1].ObjType := AnsiUpperCase(reqSchemaFolders[i].ObjType);
                  FileList[RecordCount - 1].FolderName := reqSchemaFolders[i].FolderName;
                  FileList[RecordCount - 1].Schema := ObjList[RecordCount - 1].ObjSchema;
                  FileList[RecordCount - 1].ObjName := ObjList[RecordCount - 1].ObjName;
                  FileList[RecordCount - 1].FQName := FileList[RecordCount - 1].Schema + '.' + FileList[RecordCount - 1].ObjName;

                  { For each entry in reqSchemaFolders, we have a FolderName }
                  { and a SynHighlighterTarget. The latter tells us what TStringList }
                  { we need to add to on the synSQLHighlighter object. }
                  { The Begin/End Update prevents the controls from re-painting }
                  { during processing at runtime, improving performance. }
                  { Note that all files have the ".sql" extension, hence the }
                  { hardcoded "4". }

                  if reqSchemaFolders[i].SynHighlighterTarget = 'TABLENAME' then
                  begin
                    try
                      synSQLHighlighter.TableNames.BeginUpdate;
                      if synSQLHighlighter.TableNames.IndexOf(ObjList[RecordCount - 1].ObjName) = -1 then
                        synSQLHighlighter.TableNames.Add(ObjList[RecordCount - 1].ObjName);
                    finally
                      synSQLHighlighter.TableNames.EndUpdate;
                    end;
                  end;

                  { Note that we use the ProcNames list because the }
                  { syntax CREATE OR REPLACE syntax confuses the SynEdit control }
                  { into thinking REPLACE is a function, whereas it should be }
                  { a keyword. }

                  if reqSchemaFolders[i].SynHighlighterTarget = 'PROCNAME' then
                  begin
                    try
                      synSQLHighlighter.ProcNames.BeginUpdate;
                      if synSQLHighlighter.ProcNames.IndexOf(ObjList[RecordCount - 1].ObjName) = -1 then
                        synSQLHighlighter.ProcNames.Add(ObjList[RecordCount - 1].ObjName);
                    finally
                      synSQLHighlighter.ProcNames.EndUpdate;
                    end;
                  end;

                until FindNext(SQLFileSearch) <> 0;
              finally
                FindClose(SQLFileSearch);
              end;
            end;

          end;
        end;
      until FindNext(SchemaSearch) <> 0;
    finally
      FindClose(SchemaSearch);
    end;
  end;

  { DML files are run after the DDL is created }
  { A separate DML array list is created. It is assumed that no dependencies }
  { exist between DML files. }

  if DirectoryExists(leDirectory.Text + '\DML') = true then
  begin
    if FindFirst(leDirectory.Text + '\DML\*', faDirectory, SchemaSearch) = 0 then
    begin
      try
        repeat
          { Ignore the . and .. directories }
          if AnsiPos('.',SchemaSearch.Name) = 0 then
          begin

            if FindFirst(leDirectory.Text + '\DML\' + SchemaSearch.Name + '\*.sql', faAnyFile, SQLFileSearch) = 0 then
            begin
              try
                repeat
                  RELogNote('Found: ' + leDirectory.Text + '\DML\' + SchemaSearch.Name + '\' + SQLFileSearch.Name);

                  { Add the name of the object to the dependency arrays: }
                  Inc(DMLCount);
                  SetLength(DMLFileList, DMLCount);
                  DMLFileList[DMLCount - 1].FileName := leDirectory.Text + '\DML\' + SchemaSearch.Name + '\' + SQLFileSearch.Name;
                  DMLFileList[DMLCount - 1].Schema := SchemaSearch.Name;

                until FindNext(SQLFileSearch) <> 0;
              finally
                FindClose(SQLFileSearch);
              end;
            end;

          end;
        until FindNext(SchemaSearch) <> 0;
      finally
        FindClose(SchemaSearch);
      end;
    end;
  end;

end;

{ The following procedure actually starts to process the SQL Files }
{ and build the arrays of dependencies. }

procedure TfrmMain.ProcessSQLFiles;
var
  SQLFileStream: TMemoryStream;
  i: Integer;
  j: Integer;
  MatchesFound: Integer;
  FileString: string;
begin
  MatchesFound := 0;
  RELogNote('Beginning to read files, and analyze dependencies...');
  SQLFileStream := TMemoryStream.Create;
  try
    try
      for i := 0 to High(FileList) do
      begin
        RELogNote('File: ' + IntToStr(i + 1) + ' of ' + IntToStr(High(FileList) + 1));
        RELogNote('Filename: ' + FileList[i].FileName);
        RELogNote('Reading and analyzing contents...');
        { Load the file to memory }
        SQLFileStream.LoadFromFile(FileList[i].FileName);
        { Put the entire stream of data into the FileString variable. }
        SetString(FileString, pAnsiChar(SQLFileStream.Memory), SQLFileStream.Size);
        { Remove Comment Marks from the string, and populate an array (DepList) }
        { which contains objects of the same type called from within the String. }
        { Much quicker than parsing the text multiple times for matching strings. }
        FileString := RemoveComments(AnsiUpperCase(FileString), FileList[i].Schema);
        // Clear the memory used by the FileStream.
        SQLFileStream.Clear;

        { Now, Array DEPLIST contains all objects (views/functions/procedures) }
        { that are called from the object, including itself. }
        { Populate the parent/child array with this information. }

        for j := 0 to High(DepList) do
        begin
          if DepList[j] <> AnsiUpperCase(FileList[i].Schema + '.' + FileList[i].ObjName) then
          begin
            Inc(MatchesFound);
            SetLength(ObjDependencies, MatchesFound);
            ObjDependencies[MatchesFound - 1].Parent := AnsiUpperCase(FileList[i].Schema + '.' + FileList[i].ObjName);
            ObjDependencies[MatchesFound - 1].Child := DepList[j];
          end;
          if ( High(DepList) = 0) and (DepList[j] = AnsiUpperCase(FileList[i].Schema + '.' + FileList[i].ObjName)) then
          begin
            Inc(MatchesFound);
            SetLength(ObjDependencies, MatchesFound);
            ObjDependencies[MatchesFound - 1].Parent := AnsiUpperCase(FileList[i].Schema + '.' + FileList[i].ObjName);
            ObjDependencies[MatchesFound - 1].Child := '';
          end;
        end;
        RELogNote('File complete.');
      end;
    except
      { Trap any exceptions (write-protected folder, for example) }
      on E : Exception do
      begin
        RELogError('There was an error processing a file:');
        RELogError(E.ClassName);
        RELogError(E.Message);
      end;
    end;
    RELogNote('File Processing Complete.');
  finally
    SQLFileStream.Free;
  end;
end;

{ A function to remove the comments from a piece of code. }
{ This function also adds to the dependency arrays. }

{ The SYNEDIT control has functionality within it that allows us to remove comments. }
{ we basically populate a control with SQL code, and then where the comment marker }
{ is true (determined internally by synedit), delete it, then return the text thats left. }
{ EASY }
{ We also get dependencies this way as each function or procedure call is also marked. }
{ GENIUS! I thank you. }

function TfrmMain.RemoveComments(InputString: string; SchemaName: string): string;
var
  coord: TBufferCoord;
  PrevChar: TBufferCoord;
  SelStart: TBufferCoord;
  SelEnd: TBufferCoord;
  TempStr: string;
  token: string;
  PrevObjName: string;
  ObjName: string;
  attr: TSynHighlighterAttributes;
  DepsFound: Integer;
  PreviousLineLength: Integer;
  CommentBlockStarted: boolean;
  FileDone: boolean;
begin
  SelStart.Line := 1;
  SelStart.Char := 1;
  SelEnd.Line := 1;
  SelEnd.Char := 1;
  coord.Char := 1;
  coord.Line := 1;

  CommentBlockStarted := False;
  FileDone := False;
  PreviousLineLength := 0;
  DepsFound := 0;
  PrevObjName := '';
  token := '';
  ObjName := '';
  TempStr := '';
  SetLength(DepList, 0);

  smSQLMemo.BeginUpdate;
  try
    { Populate the control with the text we are changing. }
    smSQLMemo.Text := InputString;
    while FileDone = False do
    begin
      { Set the cursor (caret) to the position determined by COORD }
      //showmessage('coord.char: ' + inttostr(coord.Char));
      //showmessage('coord.line: ' + inttostr(coord.line));
      smSQLMemo.CaretXY := coord;
      { Should already be invisible. }
      //smSQLMemo.Visible := False;
      PrevObjName := ObjName;
      if (smSQLMemo.GetHighlighterAttriAtRowCol(coord, token, attr) <> False) and (attr <> nil) then
      begin
        if (AnsiUpperCase(attr.name) = 'COMMENT') then
        begin
          if CommentBlockStarted = False then
          begin
            CommentBlockStarted := True;
            SelStart.Char := coord.Char - 1;
            SelStart.Line := coord.Line;
            if coord.Char = 0 then
            begin
              coord.Line := coord.Line - 1;
              coord.Char := PreviousLineLength;
              SelStart.Line := coord.Line;
              SelStart.Char := coord.Char;
            end;
          end;
        end
        else
        begin

          //showmessage('attr.name: ' + attr.name);
          //showmessage('token: ' + token);
          //showmessage('prevtoken: ' + prevtoken);

          if (AnsiUpperCase(attr.name) = 'PROCNAME') or (AnsiUpperCase(attr.name) = 'TABLENAME') then
          begin
            { Find schema name of token }
            { Look at the previous character: }

            PrevChar.Line := coord.Line;
            PrevChar.Char := coord.Char - 1;
            { If the FUNCTION or TABLENAME starts on a new line, then assume }
            { the current schema is used. Do not go back a line. }
            if PrevChar.Char = 0 then
              TempStr := SchemaName
            else
            begin
              { Check if the immediate previous character is a dot: }
              { If this is the case, then assume it is prefixed by a schema }
              { name. }
              { Every other scenario is assumed to be in the same schema }
              if smSQLMemo.GetWordAtRowCol(PrevChar) = '.' then
              begin
                { See if we can go back another character and get the rest of the word. }
                PrevChar.Char := coord.Char - 2;
                { If the dot was the first character, then assume current schema }
                if PrevChar.Char = 0 then
                  TempStr := SchemaName
                else
                  TempStr := smSQLMemo.GetWordAtRowCol(PrevChar);
              end
              else
                TempStr := SchemaName;
            end;

            ObjName := TempStr + '.' + token;

            //showmessage(TempStr + '.' + token);

            if (ObjName <> PrevObjName) and (InArray(ObjName, DepList) = False) then
            begin
              Inc(DepsFound);
              SetLength(DepList, DepsFound);
              DepList[DepsFound - 1] := ObjName;
            end;

          end;

          if CommentBlockStarted = True then
          begin
            CommentBlockStarted := False;
            SelEnd.Line := coord.Line;
            SelEnd.Char := coord.Char - 1;
            if coord.Char = 0 then
            begin
              coord.Line := coord.Line - 1;
              coord.Char := PreviousLineLength;
              SelEnd.Line := coord.Line;
              SelEnd.Char := coord.Char;
            end;
            { highlight the text ready for deletion }
            smSQLMemo.SetCaretAndSelection(coord, SelStart, SelEnd);
            { delete the highlighted text }
            // showmessage(smSQLMemo.SelText);
            smSQLMemo.ClearSelection;
            smSQLMemo.Refresh;
            if SelEnd.Line <> SelStart.Line then
            begin
              coord.Line := SelStart.Line;
            end;
            coord.Char := 0;
          end;
        end;
      end;
      Inc(coord.Char);
      if coord.Char >= Length(smSQLMemo.LineText) then
      begin
        coord.Char := 1;
        PreviousLineLength := Length(smSQLMemo.LineText);
        Inc(coord.Line);
      end;
      if coord.Line > smSQLMemo.DisplayLineCount then
        FileDone := True;
    end;
    Result := smSQLMemo.Text;
    smSQLMemo.ClearAll;
  finally
    smSQLMemo.EndUpdate;
  end;
end;

{ The final step of the process is to iterate through the arrays of parent }
{ and child objects, and create the final order. Objects are bumped up a level }
{ when they depend on an object in the same level. The CustomSort step at the }
{ end will sort the final output by this level; this ensures the objects are }
{ created in the correct order. }

procedure TfrmMain.GetDependencies;
var
  i: Integer;
  k: Integer;
  ssFQName: string;
  FinalOutput: TStringList;
  SortStrings: TStringList;

  { recursive function here, based on an obj. }
  { returns 1 if a dependency exists plus sum of calling the function again. }

  function IncLevel(obj: string): Integer;
  var
    TempVal: Integer;
  begin
    TempVal := 0;
    for var j: Integer := 0 to High(ObjDependencies) do
    begin
      if obj = ObjDependencies[j].Child then
        TempVal := Max(TempVal, IncLevel(ObjDependencies[j].Parent) + 1);
    end;
    Result := TempVal;
  end;

begin
  RELogNote('Begin GetDependencies...');
  SortStrings := TStringList.Create;
  try
    SortStrings.BeginUpdate;
    try
      FinalOutput := TStringList.Create;
      try
        FinalOutput.BeginUpdate;
        try

          for i := 0 to High(ObjList) do
          begin
            ObjList[i].ObjOrder := ObjList[i].ObjOrder + IncLevel(ObjList[i].ObjSchema + '.' + ObjList[i].ObjName);
          end;

          { Order now determined. A populated StringList allows us to sort the records by drop order, and by Name. }

          for i := 0 to High(ObjList) do
          begin
            SortStrings.Add(IntToStr(ObjList[i].ObjOrder) + '|' + ObjList[i].ObjSchema + '.' + ObjList[i].ObjName);
          end;

          SortStrings.Sorted := False;
          SortStrings.CustomSort(MyCustomSort);

          FinalOutput.Add('------------------------------------------------');
          FinalOutput.Add('-- create_db_objects.sql, created by GPProcOrder');
          FinalOutput.Add('------------------------------------------------');
          FinalOutput.Add('');
          FinalOutput.Add('');
          FinalOutput.Add('--------------');
          FinalOutput.Add('-- BEGIN DDL');
          FinalOutput.Add('--------------');
          FinalOutput.Add('');

          for i := 0 to SortStrings.Count - 1 do
          begin
            ssFQName := AnsiMidStr(SortStrings.Strings[i], AnsiPos('|', SortStrings.Strings[i]) + 1, Length(SortStrings.Strings[i]) - AnsiPos('|', SortStrings.Strings[i]) + 1);

            for k := 0 to High(FileList) do
            begin
              if ssFQName = FileList[k].FQName then
                FinalOutput.Add('\i ./DDL/' + FileList[k].Schema + '/' + FileList[k].FolderName + '/' + ExtractFileName(FileList[k].FileName));
            end;

          end;
          FinalOutput.Add('');
          FinalOutput.Add('--------------');
          FinalOutput.Add('-- END OF DDL');
          FinalOutput.Add('--------------');

          FinalOutput.Add('');
          FinalOutput.Add('--------------');
          FinalOutput.Add('-- BEGIN DML');
          FinalOutput.Add('--------------');
          FinalOutput.Add('');

          { Add the DML FileList }
          for i := 0 to High(DMLFileList) do
          begin
            FinalOutput.Add('\i ./DML/' + DMLFileList[i].Schema + '/' + ExtractFileName(DMLFileList[i].FileName) );
          end;

          FinalOutput.Add('');
          FinalOutput.Add('--------------');
          FinalOutput.Add('-- END OF DML');
          FinalOutput.Add('--------------');

          { Add a footer }
          FinalOutput.Add('');
          FinalOutput.Add('');
          FinalOutput.Add('');
          FinalOutput.Add('--------------');
          FinalOutput.Add('-- END OF FILE');
          FinalOutput.Add('--------------');

        finally
          FinalOutput.EndUpdate;
        end;

        { We've finished creating the final output, we can now try and save it }
        { to disk. }
        try
          FinalOutput.SaveToFile(leDirectory.Text + '\' + 'create_db_objects.sql');
          RELogNote('create_db_objects.sql successfully created.');
        except
          { Trap any exceptions (write-protected folder, for example) }
          on E : Exception do
          begin
            RELogError('There was an error creating and saving the final output file.');
            RELogError(E.ClassName);
            RELogError(E.Message);
          end;
        end;
      finally;
        FinalOutput.Free;
      end;
    finally
      SortStrings.EndUpdate;
    end;
  finally
    SortStrings.Free;
  end;
end;


end.


