unit Containers;

interface

type
    _TListArray = array [0..0] of string;
    TStringArray = record
        Items: ^_TListArray;
        Count, Capacity: UInt32;
    end;

procedure StringArrayInit(var s: TStringArray);
procedure StringArrayPush(var s: TStringArray; str: String);

implementation

const
    INITAL_CAPACITY = 8;
    GROWTH_FACTOR = 2;

procedure StringArrayInit(var s: TStringArray);
begin
    s.Count := 0;
    s.Capacity := 0;
    s.Items := nil;
end;

procedure StringArrayPush(var s: TStringArray; str: String);
begin
    if (s.Items = nil) or (s.Capacity = 0) then
    begin
        s.Capacity := INITAL_CAPACITY;
        s.Items := GetMem(SizeOf(string) * s.Capacity);
        Assert(s.Items <> nil);
    end;

    if s.Capacity <= s.Count then
    begin
        s.Capacity := s.Capacity * GROWTH_FACTOR;
        s.Items := ReallocMem(s.Items, SizeOf(string) * s.Capacity);
        Assert(s.Items <> nil);
    end;

    s.Items^[s.Count] := str;
    s.Count := s.Count + 1;
end;

end.
