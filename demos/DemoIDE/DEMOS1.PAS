#exceptions handling
procedure ExceptionsHandling;
var I, P;
begin
  try
    try
      P := 4 / 0; // Exception occurs here!
    except
      ShowMessage(exception);
      raise exception;
    end;
    // This message is not showed
    ShowMessage('Something!');
    for I :=0 to 10 do
      P :=I * I + 20;
  finally
    ShowMessage('Finally!');
  end;
end;

#Find First / Find Next
var
  s, res;
begin
  res := FindFirst('*.*', faAnyFile, @s);
  while res = 0 do
  begin
    if s.name[1] <> '.' then
      ShowMessage(s.name + '  ' + IntToStr(s.size));
    res := FindNext(@s);
  end;
  FindClose(@s);
end;

#for .. to loop 
procedure ForLoopExample;
var I, S;
begin
  // Let's measure for loop speed
  for I := 0 to 10000 do
  begin
    S := 'string value' + 'string value';
    S := NumToStr('#.#', 1274);
  end;
end;

#parameters
procedure Parameters(A; var B; out C);
begin
  C := A * 4;
  B := B * 3;
  A := 10; // will be ignored
end;
#passing var parameters
procedure VarParameters;
var
  D;
begin
  CalcValue(@D);
  ShowMessage(D);
end;
#reading file via stream
var
  s, f;
begin
  f := CreateFileStream('e:\test.txt', fmOpenRead);
  s := f.ReadLine;
  while s <> '' do
  begin
    ShowMessage(s);
    s := f.ReadLine;
  end;
  f.Free;
end;
#repeat ... until loop
procedure RepeatUntil;
var
  S, I;
begin
  S := '';
  I := 0;
  repeat
    S := S + AsStr(I);
    I := I + 1;
  until Length(S) >= 100;
  ShowMessage(S)
end;
#String as an array
procedure StringAsArray;
var
  S;
begin
  S := 'string';
  S[1] := 'X';
  ShowMessage(S);
end;
#variant arrays
procedure VarArrays;
const
  Msg = 'This message' #10#13 'has two lines.';
var
  A;
begin
  (*
  Caption := 'Diversia!';
  *)
  // Testing variant arrays
  A := VarArrayOf('Aa', 'Bb', 'Cc', 'Dd');
  A[3] := 'Xx';
  // Show message
  ShowMessage(VarToStrEx(A));
  ShowMessage(Msg);
end;

#working with dates
procedure DateDemo;
begin
  ShowMessage(Date);
end;

