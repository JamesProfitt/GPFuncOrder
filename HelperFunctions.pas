unit HelperFunctions;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.StrUtils,
  System.SysUtils
  ;

function InArray(Element: string; Arr: array of string): boolean;
function MyCustomSort(List: TStringList; Index1, Index2: Integer): Integer;
function CurrentUserName : String;
function CurrentMachineName : String;

implementation

{ A function to determine if a string is in an array of strings. }
function InArray(Element: string; Arr: array of string): boolean;
var
  found: boolean;
begin
  found := False;
  for var i: Integer := 0 to High(Arr) do
  begin
    if Arr[i] = Element then
    begin
      found := True;
      break;
    end;
  end;
  Result := found;
end;

{ Note that this function (MyCustomSort) is not in the PUBLIC section above.   }
{ Delphi doesn't like it if it is.                                             }
{ I believe this is because its already defined internally within the          }
{ TStringList class and the CUSTOMSORT property just gives it its final name.  }
{ The strings in the TStringList are separated by a |.                         }
{ ObjOrder | ObjName                                                           }
function MyCustomSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1: string;
  SS1a: string;
  SS1b: string;
  S2: string;
  SS2a: string;
  SS2b: string;
begin
  { For our purposes, we want the final output to be in DESCENDING ObjOrder    }
  { and then ASCENDING ObjName order                                           }
  { We run this CustomSort on the ObjList array copied into the SortStrings    }
  { List then we copy the sorted list into the FinalOutput TStringList and     }
  { save the output.                                                           }
  { List should contain two strings separated by a |. The first one is the     }
  { ObjOrder.                                                                  }
  { S1 is the string at Index1. SS1a is the first substring, SS1b is the       }
  { second substring both of S1.                                               }
  { The same convention applies to S2.                                         }

  S1 := List[Index1];
  S2 := List[Index2];

  SS1a := AnsiMidStr(S1, 1, AnsiPos('|', S1) - 1);
  SS1b := AnsiMidStr(S1, AnsiPos('|', S1), Length(S1) - AnsiPos('|', S1) - 1);

  SS2a := AnsiMidStr(S2, 1, AnsiPos('|', S2) - 1);
  SS2b := AnsiMidStr(S2, AnsiPos('|', S2), Length(S2) - AnsiPos('|', S2) - 1);

  { Now assign the result. }
  if (StrToInt(SS1a) > StrToInt(SS2a)) then
    Result := -1
  else if (StrToInt(SS1a) < StrToInt(SS2a)) then
    Result := 1
  else if SS1b < SS2b then
    Result := -1
  else if SS1b > SS2b then
    Result := 1
  else
    Result := 0;
end;

{ Function to get the currently logged in UserName }
function CurrentUserName : String;
var
  UserName: array[0..127] of Char;
  Size:DWord;
begin
  Size := SizeOf(UserName);
  GetUserName(UserName, Size);
  Result := UserName;
end;

{ A function to find out what the machine name is that we are running on }
function CurrentMachineName : String;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

end.
