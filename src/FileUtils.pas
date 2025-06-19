unit FileUtils;

interface

uses BufferUtils;

function ReadEntireFile(const FilePath: string): TBuffer;

implementation

function ReadEntireFile(const FilePath: string): TBuffer;
label
    cleanup;
var
    f: file;
    ret: TBuffer;
    bytesRead: UInt32;
begin
    ret.Size := 0;
    ret.Data := nil;

    Assign(f, FilePath);
    {$I-}
    Reset(f, 1);
    {$I+}

    if IOResult <> 0 then
        goto cleanup;

    ret := BufferNew(FileSize(f));
    BlockRead(
        f,
        ret.Data^,
        ret.Size,
        bytesRead
    );

    if bytesRead <> ret.Size then
        BufferDispose(ret);

cleanup:
    Close(f);

    ReadEntireFile := ret;
end;

end.
