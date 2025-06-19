unit BufferUtils;

interface
type
    TBinaryData = array[0..0] of Byte;
    PBinaryData = ^TBinaryData;

    TBuffer = record
        Data: PBinaryData;
        Size: UInt32;
    end;

function  BufferNew(Size: UInt32): TBuffer;
procedure BufferDispose(var Buffer: TBuffer);

function  BinaryToString(const Buffer: TBuffer): string;

implementation

function BufferNew(Size: UInt32): TBuffer;
var
    buf: TBuffer;
begin
    buf.Size := Size;
    buf.Data := GetMem(Size);

    if buf.Data = nil then
    begin
        WriteLn(StdErr, 'failed to allocate ', Size, ' bytes for buffer');
        Halt(1);
    end;

    BufferNew := buf;
end;

procedure BufferDispose(var Buffer: TBuffer);
begin
    FreeMem(Buffer.Data);
    Buffer.Data := nil;
    Buffer.Size := 0;
end;

function BinaryToString(const Buffer: TBuffer): string;
var
    ret: string;
    i: UInt32;
begin
    ret := '';

    for i := 0 to Buffer.Size do
    begin
        ret := ret + char(Buffer.Data^[i])
    end;

    BinaryToString := ret;
end;
end.
