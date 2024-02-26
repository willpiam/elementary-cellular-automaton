program CellularAutomata;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAX_LENGTH_INITIAL_CONDITIONS = 1000;

type
  TCellArray = array of Char;
  TAutomatonData = array of TCellArray;

function RuleToBinaryArray(RuleNumber: Char): TCellArray;
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := Chr((Ord(RuleNumber) shr I) and 1);
end;

function CalculateCell(const Neighborhood: string; const RuleBinary: TCellArray): Char;
var
  Index: Integer;
begin
  Index := StrToInt('$' + BinToHex(PChar(Neighborhood), Length(Neighborhood)));
  Result := RuleBinary[Index];
end;

function RunCellularAutomaton(const Rule: TCellArray; const Generations: Integer; const Cells: TCellArray; const InitialConditionsLength: Integer): TAutomatonData;
var
  I, J, Length, InitialOffset, PaddingOffset: Integer;
  Row: TCellArray;
  Neighborhood: string;
begin
  SetLength(Result, Generations);
  Length := InitialConditionsLength;
  InitialOffset := (InitialConditionsLength + 2 * Generations) div 2;

  for I := 0 to Generations - 1 do
  begin
    SetLength(Row, InitialConditionsLength + 2 * Generations + 1);
    Row[Length] := #0;

    if I = 0 then
      Move(Cells[0], Row[InitialOffset], InitialConditionsLength);

    Result[I] := Row;
  end;

  Length := Length + 2;

  for I := 1 to Generations - 1 do
  begin
    PaddingOffset := InitialOffset - I;

    for J := PaddingOffset to PaddingOffset + Length - 1 do
    begin
      Neighborhood := Chr(Result[I-1, J-1]) + Chr(Result[I-1, J]) + Chr(Result[I-1, J+1]);
      Result[I][J] := CalculateCell(Neighborhood, Rule);
    end;

    Length := Length + 2;
  end;
end;

function OutputToFile(const AutomatonData: TAutomatonData; RuleNumber: Integer; Generations: Integer; const InitialConditions: string; ImageWidth: Integer): Integer;
var
  FileName: string;
  FileOut: TextFile;
  I, J: Integer;
begin
  FileName := Format('results/r%d_g%d_i%s_c.pbm', [RuleNumber, Generations, InitialConditions]);
  AssignFile(FileOut, FileName);
  Rewrite(FileOut);

  WriteLn(FileOut, 'P1');
  WriteLn(FileOut, Format('%d %d', [ImageWidth, Generations]));

  for I := 0 to High(AutomatonData) do
  begin
    for J := 0 to High(AutomatonData[I]) do
      Write(FileOut, AutomatonData[I][J]);

    WriteLn(FileOut);
  end;

  CloseFile(FileOut);
  Result := 0;
end;

var
  RuleNumber, Generations, InitialConditionsLength, ImageWidth: Integer;
  InitialConditions, Cells: TCellArray;
  AutomatonData: TAutomatonData;
  Rule: TCellArray;
  InputFile: TextFile;
  ExitCode: Integer;
begin
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);
  ReadLn(InputFile, RuleNumber);
  ReadLn(InputFile, InitialConditions);
  ReadLn(InputFile, Generations);
  CloseFile(InputFile);

  Rule := RuleToBinaryArray(Chr(RuleNumber));
  InitialConditionsLength := Length(InitialConditions);

  SetLength(Cells, InitialConditionsLength + 1);
  Cells[InitialConditionsLength] := #0;

  for I := 0 to InitialConditionsLength - 1 do
    Cells[I] := Chr(Ord(InitialConditions[I]) - Ord('0'));

  AutomatonData := RunCellularAutomaton(Rule, Generations, Cells, InitialConditionsLength);

  ImageWidth := Length(InitialConditions) + 2 * Generations;
  ExitCode := OutputToFile(AutomatonData, RuleNumber, Generations, InitialConditions, ImageWidth);

  Halt(ExitCode);
end.
