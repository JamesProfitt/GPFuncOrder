unit SynEditHelper;

{ Taken from: }
{ https://stackoverflow.com/questions/50196662/how-to-handle-links-on-synedit-tablenames }

interface

uses
  Winapi.Windows,
  SynEdit,
  SynEditHighlighter
  // ,vcl.dialogs
;

type
  TSynEditHelper = class helper for TSynEdit
  public
    function GetTokenInfo(const CursorPos: TPoint; out TokenType: Integer; out TokenText: UnicodeString): Boolean; overload;
    function GetTokenInfo(const LineCharPos: TBufferCoord; out TokenType: Integer; out TokenText: UnicodeString): Boolean; overload;
  end;

implementation

{ TSynEditHelper }

function TSynEditHelper.GetTokenInfo(const CursorPos: TPoint; out TokenType: Integer; out TokenText: UnicodeString): Boolean;
begin
  Result := GetTokenInfo(DisplayToBufferPos(PixelsToRowColumn(CursorPos.X, CursorPos.Y)), TokenType, TokenText);
end;

function TSynEditHelper.GetTokenInfo(const LineCharPos: TBufferCoord; out TokenType: Integer; out TokenText: UnicodeString): Boolean;
var
  I: Integer;
  A: TSynHighlighterAttributes;
begin
  Result := GetHighlighterAttriAtRowColEx(LineCharPos, TokenText, TokenType, I, A);
  { Shows the Attribute data. If the "IF" is not used, it causes an access }
  { violation when clicking on empty area (you are effectively accessing a }
  { memory area that does not exist).                                      }
  { Uncomment vcl.dialogs in the USES clause to enable this.               }
  { Use only for debugging.                                                }

  // if A <> nil then
  //   ShowMessage(a.name);
end;

end.
