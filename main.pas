UNIT main;



INTERFACE


USES
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.EditBox, FMX.NumberBox, FMX.Menus,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart,

  System.Messaging,
  System.Threading,
  core.lookUpTBL,
  core.reanalysis, FMXTee.Series, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.Effects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ImgList,
  System.ImageList, System.Rtti, FMX.Grid.Style, FMX.Grid, System.Skia, FMX.Skia;



TYPE

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    Chart1: TChart;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    Label8: TLabel;
    Chart2: TChart;
    Rectangle1: TRectangle;
    ShadowEffect1: TShadowEffect;
    Memo1: TMemo;
    Series3: TLineSeries;
    Series5: TPointSeries;
    Series4: TAreaSeries;
    Chart3: TChart;
    Series6: TBarSeries;
    Chart4: TChart;
    Series7: TPointSeries;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel4: TPanel;
    AniIndicator1: TAniIndicator;
    Rectangle2: TRectangle;
    ShadowEffect2: TShadowEffect;
    Rectangle3: TRectangle;
    ShadowEffect3: TShadowEffect;
    Rectangle4: TRectangle;
    ShadowEffect4: TShadowEffect;
    NumberBox1: TNumberBox;
    NumberBox2: TNumberBox;
    NumberBox3: TNumberBox;
    NumberBox4: TNumberBox;
    Label9: TLabel;
    Label3: TLabel;
    NumberBox5: TNumberBox;
    NumberBox6: TNumberBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    NumberBox7: TNumberBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    CheckBox1: TCheckBox;
    Panel5: TPanel;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    ListBox2: TListBox;
    Label5: TLabel;
    Series2: TFastLineSeries;
    Series1: TFastLineSeries;
    Series8: TFastLineSeries;
    Series9: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure Rectangle1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Rectangle2Click(Sender: TObject);
    procedure Rectangle3Click(Sender: TObject);
    procedure Rectangle4Click(Sender: TObject);
    procedure NumberBox1Change(Sender: TObject);
    procedure NumberBox2Change(Sender: TObject);
    procedure NumberBox4Change(Sender: TObject);
    procedure NumberBox5Change(Sender: TObject);
    procedure StringColumn2Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ListBox2ChangeCheck(Sender: TObject);
  private
    nRch       : Integer;    // number of reachs
    updFlg     : Boolean;
    DTtag      : TdateTime;
    WBthld     : Double;     // water balance threshold
    alpha      : TrgeSim;    // correction function
    beta       : TrgeSim;    // maximum precipitation correction
    loop       : Integer;    // loop counter
    sbscrptnId : Integer;    // message catcher
    prcss      : ITask;      // running process (ITask)
    procedure initForm;
    procedure mssgProcessor(const msg: Tmessage);
    procedure updateMemo(strLn: String);
    procedure reanalysisDONE;
    procedure displaySTAT(str: Tarray<String>);
    procedure updateBest(str: Tarray<String>);
    procedure setDelimiters;

    procedure identifyReach;
    procedure plotQsim(Qs: Tarray<double>;idx: Integer;NDC: Integer);
    procedure plotPRC;
    procedure plotUpdatedPRC;
    function  plotPerformance(var Qs: Tarray<Double>;idx: Integer): Boolean;
    procedure reportProgress(str: Tarray<String>);
    procedure selectGauge;
  public
    Plbls: Tarray<String>;   // precipitation labels
    Psplt: Tarray<Integer>;  // precipitation delimiters
    Pobs : TlookUpTbl;       // observed Precipitation data

//    Qsplt: Tarray<Integer>;  // streamflow delimiters
//    Qlbls: Tarray<String>;   // streamflow labels
    Qobs : TlookUpTbl;       // observed streamflow data
    Qrch : Tarray<Integer>;  // reach for data extraction
    mxP  : Double;           // maximum precipitation
  end;



var
  Form1: TForm1;




IMPLEMENTATION

{$R *.fmx}


USES
Math,
System.TypInfo,
System.IOUtils,
System.DateUtils,

metrics,
core.mssg,
about;



procedure TForm1.selectGauge;
begin
  var Qs:= extractQsim(fldrs.SWATdir + 'output.rch',Qrch,nRch);
  plotQsim(Qs[0],listBox2.ItemIndex,-9999);
  memo1.Lines.Add('selected reach: ' + Qrch[0].ToString);
  if plotPerformance(Qs[0],listBox2.ItemIndex) then begin
    var KGE  := roundTo(KGEC(Qobs.dta[listBox2.ItemIndex + 2],  Qs[0],-9999),-4);
    var NSE  := roundTo(NSEC(Qobs.dta[listBox2.ItemIndex + 2],  Qs[0],-9999),-4);
    var PBIAS:= roundTo(PBIASe(Qobs.dta[listBox2.ItemIndex + 2],Qs[0],-9999),-4);
    label21.Text:= 'KGE: ' + KGE.ToString +
                   ' NSE: ' + NSE.ToString +
                   ' PBIAS: ' + PBIAS.ToString;
  end;
  updFlg:= FALSE;
end;



procedure TForm1.ListBox2ChangeCheck(Sender: TObject);
begin
  if updFlg then exit;
  if listBox2.ListItems[listBox2.ItemIndex].IsChecked then begin
    updFlg:= TRUE;
    Qrch:= [listBox2.ItemIndex];
    listBox2.ClearSelection;
    listBox2.BeginUpdate;
      for var ii:= 0 to listBox2.items.Count - 1 do listBox2.ListItems[ii].IsChecked:= FALSE;
      listBox2.ListItems[Qrch[0]].IsChecked:= TRUE;
      listBox2.ItemIndex:= Qrch[0];
    listBox2.EndUpdate;
    Qrch:= [listBox2.Items[Qrch[0]].ToInteger];
    selectGauge;
  end
  else updFlg:= FALSE;
end;



procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Form2.Position:= TformPosition.MainFormCenter;
  Form2.visible:= TRUE;
end;


procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  initForm;
end;

// =============================================================================
//
//
//
// =============================================================================


procedure TForm1.initForm;
begin
  updFlg:= FALSE;

  fldrs.SWATdir:= '';
  fldrs.PRCdir := '';
  fldrs.SWATprn:= '';
  fldrs.sYrsSim:= -1;
  fldrs.skipYrs:= -1;
  fldrs.DELdir := '';
  fldrs.Qdir   := '';
  fldrs.outDir := '';
  setLength(Plbls,0);  // precipitation labels
  setLength(Psplt,0);  // precipitation delimiters
  Pobs.clear;          // observed Precipitation data
  Qobs.clear;          // observed streamflow data
  setLength(Qrch,0);   // reach for data extraction
  label20.Text:= '';
  label21.Text:= '';
  stringGrid1.Columns[1].CleanupInstance;
  stringGrid1.Columns[1].Header:= 'Value';
  ListBox1.Clear;
  ListBox2.Clear;

  chart1.Series[0].Clear;
  chart1.Series[1].Clear;
  chart2.Series[0].Clear;
  chart2.Series[1].Clear;
  chart2.Series[2].Clear;
  chart3.Series[0].Clear;
  chart4.Series[0].Clear;
  chart4.Series[1].Clear;
  chart4.Series[2].Clear;
  memo1.Lines.Clear;

  shadowEffect1.Enabled:= TRUE;
  shadowEffect2.Enabled:= TRUE;
  shadowEffect3.Enabled:= TRUE;
  shadowEffect4.Enabled:= TRUE;
  rectangle1.Fill.Color:= TAlphaColorRec.Greenyellow;
  rectangle2.Fill.Color:= TAlphaColorRec.Darkolivegreen;
  rectangle3.Fill.Color:= TAlphaColorRec.Darkolivegreen;
  rectangle4.Fill.Color:= TAlphaColorRec.Darkolivegreen;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  system.NeverSleepOnMMThreadContention:= TRUE; // Memory Manager: wait loop instead of scheduling out on contentious
  // --- messages
  sbscrptnID:= mssgManager.subscribeToMessage(TMessage<string>,
  procedure(const sender: TObject;const msg: TMessage) begin
    mssgProcessor(msg);
  end);

  stringGrid1.RowCount:= 7;
  stringGrid1.Cells[0,0]:= 'KGE';
  stringGrid1.Cells[0,1]:= 'NSE';
  stringGrid1.Cells[0,2]:= 'PBIAS';
  stringGrid1.Cells[0,3]:= 'Alpha';
  stringGrid1.Cells[0,4]:= 'Beta';
  stringGrid1.Cells[0,5]:= 'Loop';
  stringGrid1.Cells[0,6]:= 'Pblnc';
  initForm;
end;


procedure TForm1.mssgProcessor(const msg: Tmessage);
begin
  var strLn:= (msg as TMessage<string>).value.Split([#44]);
  var prc:= TmssgDrctv(GetEnumValue(TypeInfo(TmssgDrctv),strLn[0]));
  try
    case prc of
      mssg_updateStat : displaySTAT(strLn);
      mssg_updateBest : updateBest(strLn);
      mssg_report     : updateMemo(copy((msg as TMessage<string>).value,13,255));
      mssg_progress   : reportProgress(strLn);
      mssg_done       : reanalysisDONE;
    end;
  except

  end;
end;


// =============================================================================
//
//
//
// =============================================================================


procedure TForm1.NumberBox1Change(Sender: TObject);
begin
  if numberBox1.Value > numberBox2.Value then numberBox1.value:= numberBox2.Value;
end;



procedure TForm1.NumberBox2Change(Sender: TObject);
begin
  NumberBox1Change(Self);
end;



procedure TForm1.NumberBox4Change(Sender: TObject);
begin
  if numberBox4.Value > numberBox5.Value then numberBox1.value:= numberBox2.Value;
end;



procedure TForm1.NumberBox5Change(Sender: TObject);
begin
  NumberBox4Change(Self);
end;


// =============================================================================
//
//
//
// =============================================================================


procedure TForm1.updateMemo(strLn: String);
begin
  memo1.Lines.Add(strLn);
  label20.Text:= DateTimeToStr(DTtag) + ' : ' +
                 roundTo(minuteSpan(now,DTtag),-2).toString + ' minutes';
end;


procedure TForm1.reanalysisDONE;
begin
  rectangle1.Fill.Color:= TAlphaColorRec.Greenyellow;
  shadowEffect1.Enabled:= TRUE;
  aniIndicator1.Enabled:= FALSE;
  panel2.Enabled       := TRUE;
  panel3.Enabled       := TRUE;
  panel4.Enabled       := TRUE;
  listBox1.Enabled     := TRUE;
  menuItem3.Enabled    := TRUE;
  prcss:= NIL;
end;


procedure TForm1.displaySTAT(str: Tarray<String>);
begin
  chart2.series[0].AddXY(loop,str[4].ToDouble);
  chart2.series[1].AddXY(loop,str[5].ToDouble);
  chart2.series[2].AddXY(loop,str[6].ToDouble);
  inc(loop);
end;



procedure TForm1.updateBest(str: Tarray<String>);
begin
  stringGrid1.Cells[1,0]:= str[4];
  stringGrid1.Cells[1,1]:= str[5];
  stringGrid1.Cells[1,2]:= str[6];
  stringGrid1.Cells[1,3]:= str[2];
  stringGrid1.Cells[1,4]:= str[1];
  stringGrid1.Cells[1,5]:= str[3];
  stringGrid1.Cells[1,6]:= (100*str[7].ToDouble).ToString + ' %';
  plotUpdatedPRC;

  if listBox2.ItemIndex = -1 then exit;
  var Qs:= extractQsim(fldrs.SWATdir + 'output.rch',Qrch,nRch);
  plotQsim(Qs[listBox2.ItemIndex],listBox2.ItemIndex,-9999);
  plotPerformance(Qs[listBox2.ItemIndex],listBox2.ItemIndex);
end;



procedure TForm1.plotQsim(Qs: Tarray<double>;idx: Integer;NDC: Integer);
begin
  if length(Qobs.dta) = 0 then exit;
  try
    idx:= idx + 2;
    chart1.BeginUpdate;
      chart1.Series[0].Clear;
      chart1.Series[1].Clear;
      chart1.Series[0].AddXY(Qobs.dta[1][0],0);
      for var ii:= Low(Qs) to High(Qs) do begin
        if Qobs.dta[idx][ii] = NDC then continue;
        chart1.Series[0].AddXY(Qobs.dta[1][ii],Qobs.dta[idx][ii]);
        chart1.Series[1].AddXY(Qobs.dta[1][ii],Qs[ii]);
      end;
    chart1.EndUpdate;
  except

  end;
end;



procedure TForm1.plotPRC;
begin
  var idx: Integer;
  var yr:= fldrs.sYrsSim + fldrs.skipYrs - 1;                                   // last year to skip
  for var kk: Integer:= Low(Pobs.dta[0]) to High(Pobs.dta[0]) do
    if Pobs.dta[0][kk] > yr then begin
      idx:= kk;
      break;
    end;

  var avg: Double;
  var day:= encodeDate(Trunc(Pobs.dta[0][0]),1,1) + idx;
  chart3.Series[0].Clear;
  for var ii:= idx to High(Pobs.dta[0]) do begin
    avg:= 0;
    for var jj:= 2 to High(Pobs.dta) do avg:= avg + Pobs.dta[jj][ii];
    avg:= avg/(length(Pobs.dta) - 2);
    chart3.series[0].AddXY(day,avg);
    day:= day + 1;
  end;
end;



procedure TForm1.plotUpdatedPRC;
begin
  var Pupt:= TtableX.readLookUpTbl(fldrs.outDir + 'pcp.BEST.PCP',-9999,4,0,Psplt,Plbls);
//  var Pupt:= TtableX.readLookUpTbl(fldrs.SWATdir + 'pcp1.PCP',-9999,4,0,Psplt,Plbls);
  var idx: Integer;
  var yr:= fldrs.sYrsSim + fldrs.skipYrs - 1;                                   // last year to skip
  for var kk: Integer:= Low(Pobs.dta[0]) to High(Pobs.dta[0]) do
    if Pobs.dta[0][kk] > yr then begin
      idx:= kk;
      break;
    end;

  var avgO: Double;
  var avgU: Double;
  var day:= encodeDate(Trunc(Pobs.dta[0][0]),1,1) + idx;
  chart3.BeginUpdate;
    chart3.Series[0].Clear;
    for var ii:= idx to High(Pobs.dta[0]) do begin
      avgO:= 0;
      avgU:= 0;
      for var jj:= 2 to High(Pobs.dta) do begin
        avgO:= avgO + Pobs.dta[jj][ii];
        avgU:= avgU + Pupt.dta[jj][ii];
      end;
      avgO:= avgO/(length(Pobs.dta) - 2);
      avgU:= avgU/(length(Pupt.dta) - 2);
      chart3.series[0].AddXY(day,avgU - avgO);
      day:= day + 1;
    end;
  chart3.EndUpdate;
end;



function TForm1.plotPerformance(var Qs: Tarray<Double>;idx: Integer): Boolean;
begin
  result:= TRUE;
  try
    if length(Qs) = length(Qobs.dta[0]) then begin
      chart4.BeginUpdate;
      chart4.series[0].Clear;  // dots
      chart4.series[1].Clear;  // perfect fit
      chart4.series[2].Clear;  // trendline
      idx:= idx + 2;
      for var ii:= Low(Qobs.dta[0]) to High(Qobs.dta[0]) do begin
        if Qobs.dta[idx][ii] = -9999 then continue;
        chart4.series[0].AddXY(Qobs.dta[idx][ii],Qs[ii]);
      end;
      var maxXV:= chart4.Series[0].MaxXValue;
      chart4.series[1].AddXY(0,0);
      chart4.series[1].AddXY(maxXV,maxXV);
      chart4.EndUpdate;
    end
    else begin
      result:= FALSE;
      showMessage('ERROR: there is a mismatch between Qobs and Qsim number of records');
    end;
  except
    result:= FALSE;
    showMessage('ERROR: Qobs may be undefined');
  end;
end;


procedure TForm1.reportProgress(str: Tarray<String>);
begin
  label21.Text:= roundTo(minuteSpan(now,DTtag),-2).toString + ' minutes : ' +
                 'Alpha: ' + str[2] + ' : Beta: ' + str[1] + ' : Pblnc: ' + str[3];
end;


// =============================================================================
//
//
//
// =============================================================================


procedure TForm1.Rectangle2Click(Sender: TObject);                              // SWAT
var Dir: String;
begin
  if SelectDirectory('SWAT Folder', System.IOUtils.TPath.GetDocumentsPath,Dir) then begin
    fldrs.SWATdir:= Dir + '\';
    if fileExists(fldrs.SWATdir + 'swat.exe') then begin
      var flCIO: Tstrings:= TstringList.Create;
      try
        flCIO.LoadFromFile(fldrs.SWATdir + 'file.CIO');
        if flCIO.Count = 0 then begin
          ShowMessage('ERROR: empty SWAT file.CIO');
          exit;
        end;
        try
          memo1.Lines.Add(fldrs.SWATdir);
          fldrs.PRCdir:= fldrs.SWATdir + flCIO.Strings[34];                     // SWAT precipitation file
          fldrs.SWATprn:= fldrs.SWATdir + 'outPut.rch';                         //
          memo1.Lines.Add(extractFileName(fldrs.SWATprn));
          fldrs.sYrsSim:= Trim(copy(flCIO.Strings[8],1,20)).ToInteger;          // start year of simulation
          fldrs.skipYrs:= Trim(copy(flCIO.Strings[59],1,20)).ToInteger;         // skip number of years (warmup period)
          memo1.Lines.Add(fldrs.sYrsSim.ToString + #44 + fldrs.skipYrs.ToString);

          identifyReach;
          setDelimiters;
          Pobs:= TtableX.readLookUpTbl(fldrs.PRCdir,-9999,4,0,Psplt,Plbls);
          // --- maximum precipitation
          mxP:= Pobs.dta[2][0];
          for var ii:= 2 to High(Pobs.dta) do
            for var jj:= Low(Pobs.dta[ii]) to High(Pobs.dta[ii]) do
              if Pobs.dta[ii][jj] > mxP then mxP:= Pobs.dta[ii][jj];
          // ------------------------
          plotPRC;
          flCIO.Free;
          shadowEffect2.Enabled:= FALSE;
          rectangle2.Fill.Color:= TAlphaColorRec.Maroon;
        except
          ShowMessage('ERROR: Observed Precipitation');
        end;
      except
        flCIO.Free;
        ShowMessage('ERROR reading SWAT file.CIO');
      end;
    end
    else ShowMessage('ERROR: missing SWAT.EXE');
  end;
end;



procedure TForm1.Rectangle3Click(Sender: TObject);                              // Streamflow
var idx: integer;
begin
  openDialog1.Filter := 'Text files only|*.txt|Comma Separated (*.csv)|*.csv';
  if openDialog1.Execute then begin
    fldrs.Qdir:= openDialog1.FileName;
    memo1.Lines.Add(extractFileName(fldrs.Qdir));
    Qobs:= TtableX.readLookUpTbl(fldrs.Qdir,-9999,1,1,[#9,#32,#44]);
    for var ii:= 2 to Qobs.lbls.Count - 1 do
      if listBox1.Items.Contains(Qobs.lbls[ii]) then begin
        idx:= listBox1.Items.IndexOf(Qobs.lbls[ii]);
        listBox2.Items.Add(ListBox1.ListItems[idx].Text);
        ListBox1.Items.Delete(idx);
      end;
    shadowEffect3.Enabled:= FALSE;
    rectangle3.Fill.Color:= TAlphaColorRec.Maroon;
  end;
end;


procedure TForm1.Rectangle4Click(Sender: TObject);                              // Output
var Dir: String;
begin
  if fldrs.SWATdir = '' then exit;
  if SelectDirectory(fldrs.SWATdir,System.IOUtils.TPath.GetDocumentsPath,Dir) then begin
    fldrs.outDir:= Dir + '\';
    memo1.Lines.Add(extractRelativePath(dir,fldrs.outDir));
    // --------------- copy precipitation and output files to the results folder
    Tfile.Copy(fldrs.PRCdir, fldrs.outDir + extractFileName(fldrs.PRCdir),TRUE);
    Tfile.Copy(fldrs.SWATdir + 'output.RCH',fldrs.outDir + 'output.rch',TRUE);
    shadowEffect4.Enabled:= FALSE;
    rectangle4.Fill.Color:= TAlphaColorRec.Maroon;
  end;
end;


procedure TForm1.identifyReach;
var
rch: String;
str: String;
begin
  if fldrs.SWATdir = '' then exit;
  var cLn: Integer:= 0;
  try
    var flTxt:= TstreamReader.Create(fldrs.SWATdir + 'output.rch');             // identify REACH
    try
      listBox1.Clear;
      while not flTxt.EndOfStream do begin
        str:= flTxt.ReadLine;
        inc(cLn);
        if cLn < 10 then continue;
        rch:= Trim(copy(str,6,6));
        if listBox1.Items.Contains(rch) then break else listBox1.Items.Add(rch);
      end;
      memo1.Lines.Add('number of reachs: ' + listBox1.Count.ToString);
      nRch:= listBox1.Count;
    finally
      flTxt.Close;
      flTxt.Free;
    end;
  except
    ShowMessage('ERROR reading output.rch');
  end;
end;


procedure TForm1.setDelimiters;
begin
  var flTxt:= TstreamReader.Create(fldrs.PRCdir);  // ------------ precipitation
  try
    var str:= flTxt.ReadLine;
    str:= 'Station' + #44 + Trim(copy(str,8,length(str)));
    Plbls:= str.Split([#44]);
    setLength(Plbls,length(Plbls) - 1);
  finally
    flTxt.Close;
    flTxt.Free;
  end;
  setLength(Psplt,length(Plbls) + 2);
  Psplt[0]:= 1;
  Psplt[1]:= 5;
  Psplt[2]:= 8;
  for var ii:= 3 to High(Psplt) do Psplt[ii]:= Psplt[ii - 1] + 5;

//  Qlbls:= ['RCH','GIS','MON','AREAkm2','FLOW_INcms','FLOW_OUTcms'];
//  Qsplt:= [6,12,21,27,39,51,63];
//  Qsplt:= [6,11,20,26,38,50,62]; valid for old SWAT version
end;



procedure TForm1.StringColumn2Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin

end;

// =============================================================================
//
//
//
// =============================================================================

procedure TForm1.Rectangle1Click(Sender: TObject);  // execute
begin
  if (listBox2.Count = 0) then begin
    ShowMessage('ERROR: REACH is undefined');
    exit;
  end;

  if rectangle1.Fill.Color = TAlphaColorRec.Greenyellow then begin
    if (length(Qobs.dta) = 0) then exit;
    loop:= 1;
    chart2.Series[0].Clear;
    chart2.Series[1].Clear;
    chart2.Series[2].Clear;
    try
      alpha.sVal:= numberBox1.Text.ToDouble;
      alpha.eVal:= numberBox2.Text.ToDouble;
      alpha.step:= numberBox3.Text.ToDouble;
      beta.sVal := numberBox4.Text.ToDouble;
      beta.eVal := numberBox5.Text.ToDouble;
      beta.step := numberBox6.Text.ToDouble;
      WBthld    := numberBox7.Text.ToDouble;

      rectangle1.Fill.Color:= TAlphaColorRec.Red;
      aniIndicator1.Enabled:= TRUE;
      menuItem3.Enabled:= FALSE;
      DTtag:= now;
      label20.Text:= DateTimeToStr(DTtag);

      setLength(Qrch,listBox2.Count);
      for var rch:= 0 to listBox2.Count - 1 do Qrch[rch]:= listBox2.Items[rch].ToInteger;
      prcss:= mainReanalysis(Pobs,Qobs,Qrch,mxP,alpha,beta,WBthld,checkBox1.IsChecked,nRch);
      shadowEffect1.Enabled:= FALSE;
      panel2.Enabled:= FALSE;
      panel3.Enabled:= FALSE;
      panel4.Enabled:= FALSE;
    except
      rectangle1.Fill.Color:= TAlphaColorRec.Greenyellow;
      menuItem3.Enabled:= TRUE;
      panel2.Enabled   := TRUE;
      panel3.Enabled   := TRUE;
      panel4.Enabled   := TRUE;
      aniIndicator1.Enabled:= FALSE;
    end;
  end
  else begin
    // TODO : implement kill process (prcss) and restore original files
  end;
end;






END.
