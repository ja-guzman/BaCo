UNIT core.lookUpTBL;


INTERFACE


USES
System.SysUtils,
System.Classes,

kernel.DataTYPE;



TYPE


  TlookUpTBL = record
    DT  : TdtaType;                // data type
    NDC : Integer;                 // non Data Code
    key : String;                  // key column label
    hdr : Tarray<String>;          // header
    lbls: Tstrings;                // labels
    dta : Tarray<Tarray<Double>>;  // data
    procedure clear;
    class operator Initialize (out Dest: TlookUpTBL);
    class operator assign(var Dest: TlookUpTBL;const [ref] Src: TlookUpTBL);
  end;



  TtableX = Class
    PUBLIC
      class function readLookUpTbl(const fNme: TfileName;  // no header
                                   NDC: integer): TlookUpTbl;          OVERLOAD;
      class function readLookUpTbl(const fNme: TfileName;  //
                                   NDC,HDR,LBL: integer;
                                   dlmtrs: TsysCharSet): TlookUpTbl;   OVERLOAD;
      class function readLookUpTbl(const fNme: TfileName;  //
                                   NDC,HDR,LBL: integer;
                                   dIdx: Tarray<integer>;
                                   lbls: Tarray<String>): TlookUpTbl;  OVERLOAD;
  end;




IMPLEMENTATION



// =============================================================================
//
//   TlooUpTBL record
//
// =============================================================================


procedure TlookUpTBL.clear;
begin
  DT  := UKNW;
  NDC := -9999;
  key := '';
  setLength(hdr,0);
  lbls.Free;
  setLength(dta,0,0);
end;


class operator TlookUpTBL.Initialize (out Dest: TlookUpTBL);
begin
  Dest.DT  := UKNW;
  Dest.NDC := -9999;
  Dest.key := '';
  setLength(Dest.hdr,0);
  Dest.lbls:= NIL;
  setLength(Dest.dta,0,0);
end;


class operator TlookUpTBL.assign(var Dest: TlookUpTBL;const [ref] Src: TlookUpTBL);
begin
  Dest.DT  := Src.DT;
  Dest.NDC := Src.NDC;
  Dest.key := Src.key;
  Dest.hdr := Src.hdr;
  Dest.lbls:= Src.lbls;
  setLength(Dest.dta,length(Src.dta));
  var NB:int64:= length(Src.dta[0])*sizeOf(Double);
  for var ii:= Low(Dest.dta) to High(Dest.dta) do begin
    setLength(Dest.dta[ii],length(Src.dta[0]));
    move(Src.dta[ii][0],Dest.dta[ii][0],NB);
  end;
end;


// =============================================================================
//
//   Table Parser (data organized by rows)
//
// =============================================================================

{
CONSTRUCTOR TtableX.Create;
begin
  inherited Create(nil);

end;



function TtableX.splitDlmtd(var str: string): Tstrings;
begin
  result:= TstringList.Create;
  if prsrIDX = nil then extractStrings([#9,#32,#44],[#32],pChar(str),result)
  else begin
    var COLi:= prsrIDX[0];
    for var ii:= 1 to High(prsrIDX) do begin
      result.Add(trim(copy(str,COLi,prsrIDX[ii])));  // ZERO-base
      COLi:= prsrIDX[ii];
    end;
  end;
end;



function TtableX.splitByLBLs(var str: string): Tstrings;
var
xx  : integer;
dta : Tstrings;
begin
  result:= TstringList.Create;
  try
    dta:= splitDlmtd(str);
    for var tgLBL in prsrTkns do begin
      xx:= lblTkns.IndexOf(tgLBL);
      if xx = -1 then continue;
      result.Add(dta[xx]);
    end;
  finally
    dta.Free;
  end;
end;



procedure TtableX.setColParser(str: string);
begin
  var ii:= 0;
  var tkns:= str.Split([#59]);
  setLength(prsrCol,length(tkns));
  for var col in tkns do begin
    prsrCol[ii]:= col.ToInteger;
    inc(ii);
  end;
end;



procedure TtableX.setColParser(str: Tstrings);
begin
  setLength(prsrCol,str.Count);
  for var ii:= 0 to str.Count - 1 do prsrCol[ii]:= ii;
end;



procedure TtableX.readHdr(inTXT: TfileName);
var flTXT: TstreamReader;  // stream
begin
  flTXT:= TstreamReader.Create(inTXT);
  try
    setLength(hdrStr,linesHDR);
    for var ii:= 1 to linesHDR do begin
      hdrStr[ii - 1]:= flTxt.ReadLine;
      if ii = LBLline then begin
        if prsrIDX = NIL then extractStrings([#9,#32,#44],[#32],pChar(hdrStr[ii - 1]),lblTkns)
        else lblTkns:= splitDlmtd(hdrStr[ii - 1]);
      end;
    end;
    flTXT.Close;
  finally
    flTXT.Free;
  end;
end;

}



class function TtableX.readLookUpTbl(const fNme: TfileName;
                                     NDC: integer): TlookUpTbl;


  procedure dimDta(const nrows: integer);
  begin
    for var ii:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],nrows);
  end;

var
strLn: string;         // text line
tkns : Tarray<string>; // tokens
begin
  var rn: int64:= 1;                            // number of rows
  var flTxt:= TstreamReader.Create(fNme);       // stream
  try
    strLn:= flTxt.ReadLine;
    flTxt.Rewind;
    var nCols:= strLn.Split([#9,#32,#44],TStringSplitOptions.ExcludeEmpty);      // read label

    if length(nCols) > 0 then begin
      result.NDC:= NDC;
      setLength(result.dta,length(nCols) + 1);  // columns
      dimDta(1000);                             // rows
      while not (flTxt.endOfStream) do begin
        strLn:= flTxt.ReadLine;
        if strLn = '' then continue;
        result.dta[0][rn - 1]:= rn;
        tkns:= strLn.Split([#9,#32,#44]);
        for var ii: integer:= 1 to High(result.dta) do begin                    // columns
          if tkns[ii - 1] = '' then result.dta[ii][rn - 1]:= NDC
          else result.dta[ii][rn - 1]:= tkns[ii - 1].ToDouble;
        end;
        inc(rn);
        if rn > length(result.dta[0]) then dimDta(2*length(result.dta[0]));
      end;

      rn:= rn - 1;
      for var ii: integer:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],rn);
    end;
  finally
    flTxt.Close;
    flTxt.Free;
  end;
end;



class function TtableX.readLookUpTbl(const fNme: TfileName;
                                     NDC,HDR,LBL: integer;dlmtrs: TsysCharSet): TlookUpTbl;

  procedure dimDta(const nrows: integer);
  begin
    for var ii:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],nrows);
  end;

var
strLn: string;      // text line
tkns : TstringList; // tokens
begin
  if (HDR < 0) or (LBL > HDR) or (LBL < 0) then exit;
  var rn: int64:= 1;                            // number of rows
  var flTxt:= TstreamReader.Create(fNme);       // stream
  try
    tkns:= TstringList.Create;
    setLength(result.hdr,HDR);
    result.lbls:= TstringList.Create;
    if HDR > 0 then begin
      for var ii:= 1 to HDR do result.hdr[ii - 1]:= flTxt.ReadLine;             // read header
      if LBL > 0 then
        extractStrings(dlmtrs,[#32],pChar(result.hdr[LBL - 1]),result.lbls)     // labels
      else begin
        strLn:= flTxt.ReadLine;
        extractStrings(dlmtrs,[#32],pChar(result.hdr[LBL - 1]),result.lbls);    // labels count
        flTxt.Rewind;
        for var jj:= 1 to result.lbls.Count do result.lbls[jj - 1]:= 'Col-' + jj.ToString;
        for var ii:= 1 to HDR do result.hdr[ii - 1]:= flTxt.ReadLine;           // read header
      end;
    end
    else begin
      strLn:= flTxt.ReadLine;
      extractStrings(dlmtrs,[#32],pChar(strLn),result.lbls);                    // labels count
      flTxt.Rewind;
      for var jj:= 1 to result.lbls.Count do result.lbls[jj - 1]:= 'Col-' + jj.ToString;
    end;
    result.lbls.Insert(0,'IDX');

    if result.lbls.Count > 1 then begin
      result.NDC:= NDC;
      setLength(result.dta,result.lbls.Count);  // columns
      dimDta(1000);                             // rows
      while not (flTxt.endOfStream) do begin
        strLn:= Trim(flTxt.ReadLine);
        if strLn = '' then continue;
        result.dta[0][rn - 1]:= rn;
        tkns.clear;
        extractStrings(dlmtrs,[#32],pChar(strLn),tkns);                         // labels

        for var ii: integer:= 1 to High(result.dta) do begin                    // columns
          if tkns[ii - 1] = '' then result.dta[ii][rn - 1]:= NDC
          else result.dta[ii][rn - 1]:= tkns[ii - 1].ToDouble;
        end;
        inc(rn);
        if rn > length(result.dta[0]) then dimDta(2*length(result.dta[0]));
      end;

      rn:= rn - 1;
      for var ii: integer:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],rn);
    end;
  finally
    flTxt.Close;
    tkns.Free;
  end;
end;



class function TtableX.readLookUpTbl(const fNme: TfileName;NDC,HDR,LBL: integer;
                                     dIdx: Tarray<Integer>;
                                     lbls: Tarray<String>): TlookUpTbl;

  procedure dimDta(const nrows: integer);
  begin
    for var ii:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],nrows);
  end;


  function splitDlmtdLBS(var str: string): Tstrings;
  begin
    result:= TstringList.Create;
    for var ii:= dIdx[0] to High(dIdx) - 1 do
      result.Add(trim(copy(str,dIdx[ii],dIdx[ii + 1] - dIdx[ii])));  // ONE-base
  end;

var
strLn: string;         // text line
tkn  : string;
begin
  if (HDR < 1) or (LBL > HDR) or (length(lbls) <> length(dIDX) - 2) then exit;
  var rn: int64:= 1;                            // number of rows
  var flTxt:= TstreamReader.Create(fNme);       // stream
  try
    setLength(result.hdr,HDR);
    for var ii:= 1 to HDR do result.hdr[ii - 1]:= flTxt.ReadLine;
    if LBL > 0 then result.lbls:= splitDlmtdLBS(result.hdr[LBL - 1])
    else begin
      result.lbls:=TstringList.Create;
      for var ii in LBLS do result.lbls.Add(ii);
    end;

    if result.lbls.Count > 0 then begin
      result.NDC:= NDC;
      setLength(result.dta,length(dIDX) - 1);   // columns
      dimDta(1000);                             // rows
      while not (flTxt.endOfStream) do begin
        strLn:= flTxt.ReadLine;
        if strLn = '' then continue;

        for var ii:= Low(dIdx) to High(dIdx) - 1 do begin
          tkn:= copy(strLn,dIdx[ii],dIdx[ii + 1] - dIdx[ii]);        // ONE-base
          if tkn = '' then result.dta[ii][rn - 1]:= NDC
          else result.dta[ii][rn - 1]:= tkn.ToDouble;
        end;
        inc(rn);
        if rn > length(result.dta[0]) then dimDta(2*length(result.dta[0]));
      end;

      rn:= rn - 1;
      for var ii: integer:= Low(result.dta) to High(result.dta) do setLength(result.dta[ii],rn);
    end;
  finally
    flTxt.Close;
    flTxt.Free;
  end;
end;


{

function TtxtParser.splitDlmtd(var str: string): Tstrings;
var
COLi : integer;
COLj : integer;
begin
  result:= TstringList.Create;
  if colPrsr = nil then extractStrings([#9,#32],[#32],pChar(str),result)
  else begin
    COLi:= colPrsr[0];
    for var ii:= colStrt to high(colPrsr) do begin
      result.Add(trim(copy(str,COLi,colPrsr[ii] - COLi)));  // ZERO-base
      COLi:= colPrsr[ii];
    end;
  end;
end;



function TtxtParser.splitByLBLs(var str: string): Tstrings;
var
xx  : integer;
dta : Tstrings;
begin
  result:= TstringList.Create;
  try
    dta:= splitDlmtd(str);
    for var tgLBL in prsrLBL do begin
      xx:= lbl.IndexOf(tgLBL);
      if xx = -1 then continue;
      result.Add(dta[xx]);
    end;
  finally
    dta.Free;
  end;
end;



procedure TtxtParser.setColParser(str: string);
begin
  var ii:= 0;
  var tkns:= str.Split([#59]);
  setLength(prsrCol,length(tkns));
  for var col in tkns do begin
    prsrCol[ii]:= col.ToInteger;
    inc(ii);
  end;
end;



procedure TtxtParser.setColParser(str: Tstrings);
begin
  setLength(prsrCol,str.Count);
  for var ii:= 0 to str.Count - 1 do prsrCol[ii]:= ii;
end;



procedure TtxtParser.readHdr(inTXT: TfileName);
var flTXT: TstreamReader;  // stream
begin
  flTXT:= TstreamReader.Create(inTXT);
  try
    setLength(hdr,linesHDR);
    for var ii:= 1 to linesHDR do begin
      hdr[ii - 1]:= flTxt.ReadLine;
      if ii = LBLline then begin
        if colPrsr = nil then extractStrings([#9,#32,#44],[#32],pChar(hdr[ii - 1]),lbl)
        else lbl:= splitDlmtd(hdr[ii - 1]);
      end;
    end;
    flTXT.Close;
  finally
    flTXT.Free;
  end;
end;








//function TlookUpTbl.readLookUpTbl(const fNme: TfileName;parser: TtxtParser): TlookUpTbl;
procedure TlookUpTbl.readLookUpTbl(const fNme: TfileName);
var
ii   : integer;   // looper
nRows: int64;     // number of rows
strLn: string;    // text line (zero base)
tkns : Tstrings;  // tokens


  procedure dimDta(var body: Tarray<Tarray<double>>;const nRows: integer);
  begin
    for var ii:= Low(body) to High(body) do setLength(body[ii],nRows);
  end;


  function parseBySpacer(var data: Tarray<Tarray<double>>): Boolean;
  begin
    result:= TRUE;
    try
      while not (flTxt.endOfStream) do begin
        strLn:= Trim(flTxt.ReadLine);
        if strLn = '' then continue;
        extractStrings([#9,#32,#44],[#32],pChar(strLn),tkns);
        ii:= 0;
//        for var col: integer:= parser.colStart to tkns.Count - 1 do begin
        for var col: integer:= colStart to tkns.Count - 1 do begin
//          if tkns[col] = '' then data[ii][nRows]:= parser.nonData
          if tkns[col] = '' then data[ii][nRows]:= nonData
          else
            try
              data[ii][nRows]:= tkns[col].ToDouble;
              inc(ii);
            except
              continue;
            end;
        end;
        inc(nRows);
        tkns.free;
        if nRows + 1 > length(data[0]) then dimDta(data,2*length(data[0]));
      end;
    except
      result:= FALSE;
    end;
  end;


  function parseDelimited(var data: Tarray<Tarray<double>>): Boolean;
  begin
    result:= TRUE;
    try
      while not (flTxt.endOfStream) do begin
        strLn:= Trim(flTxt.ReadLine);
        if strLn = '' then continue;
//        tkns:= parser.splitByLBLs(strLn);
        tkns:= splitByLBLs(strLn);
        ii:= -1;
        for var col: integer:= 0 to tkns.Count - 1 do begin
          inc(ii);
//          if tkns[col] = '' then data[ii][nRows]:= parser.nonData
          if tkns[col] = '' then data[ii][nRows]:= nonData
          else
            try
              data[ii][nRows]:= tkns[col].ToDouble;
            except
              continue;
            end;
        end;
        inc(nRows);
        tkns.free;
        if nRows = length(data[0]) then dimDta(data,2*length(data[0]));
      end;
    except
      result:= FALSE;
    end;
  end;


begin
//  result:= TlookUpTbl.Create;
  DT:= FT64;    //
  nRows    := 0;       //
  flTxt:= TstreamReader.Create(fNme);
  try
//    if parser.linesHDR > 0 then begin
    if linesHDR > 0 then begin
//      if (parser.LBLline > parser.linesHDR) then raise Exception.Create('Header Definition Error');
      if (LBLline > linesHDR) then raise Exception.Create('Header Definition Error');
//      parser.readHdr(fNme);
      readHdr(fNme);
    end;
//    setLength(result.dta,parser.prsrLBL.Count);        // number of columns
    setLength(dta,prsrLBL.Count);        // number of columns
    dimDta(dta,1000);                           // default number of rows per column
//    for ii:= 1 to parser.linesHDR do flTxt.ReadLine;   // dump header
    for ii:= 1 to linesHDR do flTxt.ReadLine;   // dump header

//    if parser.colPrsr = nil then parseBySpacer(result.dta)
    if colPrsr = nil then parseBySpacer(dta)
    else parseDelimited(dta);

    for ii:= Low(dta) to High(dta) do setLength(dta[ii],nRows);
  finally
    flTxt.Close;
  end;
end;
}



END.
