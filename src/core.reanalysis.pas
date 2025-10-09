unit core.reanalysis;


INTERFACE

USES
System.Threading,
core.lookUpTBL;





TYPE

  TrgeSim = record
    sVal: Double;
    eVal: Double;
    step: Double;
  end;


  TfleSys = record
    SWATdir : String;  // SWAT path
    PRCdir  : String;  // precipitation path
    SWATprn : String;  // SWAT ouput path
    sYrsSim : Integer; // start year of simulation
    skipYrs : Integer; // warming up period (number of years)
    DELdir  : String;  // delimiters path
    Qdir    : String;  // Observed discharge
    outDir  : String;  // reanalysis output
    class operator initialize(out Dest: TfleSys);
  end;


  Tmetric = record
    KGE  : Double;
    NSE  : Double;
    PBIAS: Double;
    WBlnc: Double;
    class operator initialize(out Dest: Tmetric);
  end;


  Tmetrics = record
    avg : Tmetric;
    mtrc: Tarray<Tmetric>;
  end;




  function extractQsim(fNme: String;Qrch: Tarray<Integer>;nRch: Integer): Tarray<Tarray<Double>>;


  function computeMetrics(var Qobs,Qs: Tarray<Tarray<Double>>): Tmetrics;


  function mainReanalysis(const PRC  : TlookUpTbl;
                          Qobs       : TlookUpTbl;
                          Qrch       : Tarray<Integer>;
                          const maxP : Double;
                          alpha      : TrgeSim;
                          beta       : TrgeSim;
                          PBthld     : Double;
                          WBactive   : Boolean;
                          nRch       : Integer): ITask;


var
fldrs: TfleSys;



IMPLEMENTATION


USES
math,
System.IOUtils,
System.Classes,
System.SysUtils,
winApi.ShellAPI,
winapi.Windows,

core.mssg,
metrics;




// =============================================================================

class operator TfleSys.initialize(out Dest: TfleSys);
begin
  Dest.SWATdir:= '';
  Dest.PRCdir := '';
  Dest.SWATprn:= '';
  Dest.sYrsSim:= -1;
  Dest.skipYrs:= -1;
  Dest.DELdir := '';
  Dest.Qdir   := '';
  Dest.outDir := '';
end;


class operator Tmetric.initialize(out Dest: Tmetric);
begin
  Dest.KGE  := 0;
  Dest.NSE  := 0;
  Dest.PBIAS:= 0;
  Dest.WBlnc:= 0;
end;


// =============================================================================


function RunAndWait(filename  : TFilename;
                    params    : string;
                    working   : string;
                    timeout   : Cardinal;
                    visibility: integer): string;
// https://www.tek-tips.com/viewthread.cfm?qid=402076
// https://learn.microsoft.com/en-us/windows/win32/procthread/creating-processes
var
rslt       : cardinal;
waitResult : integer;
startupInfo: TstartupInfo;
processInfo: TprocessInformation;
begin
  result:= 'OK';
  zeroMemory(@startupInfo,SizeOf(TstartupInfo));
  with StartupInfo do begin
    cb := SizeOf(TstartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := Visibility;
  end;
  if fileexists(filename) or fileexists(params) then begin
    try
      if (createProcess(nil,
                        Pchar(Filename + #32 + Params),
                        nil,
                        nil,
                        FALSE,
                        NORMAL_PRIORITY_CLASS,
                        nil,
                        pchar(working),
                        startupInfo,
                        processInfo)) then begin
        waitResult:= waitForSingleObject(processInfo.hProcess,timeOut);
        if waitResult = WAIT_TIMEOUT then begin
          result:= 'Timeout has occured';
          terminateProcess(processInfo.hprocess,0);
        end;
        closeHandle(processInfo.hProcess);
        closeHandle(processInfo.hThread);
      end
      else result:= 'Createproccess failed ' + filename + ' ' + Params + ' ' + inttostr(getlasterror);
    finally
      terminateThread(processInfo.hThread,rslt);
    end;
  end
  else result:= 'Process file does not exist';
end;



function extractQsim(fNme: String;Qrch: Tarray<Integer>;nRch: Integer): Tarray<Tarray<Double>>;
var
ii  : Integer;
str : string;
cols: Tstrings;
begin
  cols:= TstringList.Create;
  var flTxt:= TstreamReader.Create(fNme);   // output.rch
  try
    for var kk:= 1 to 9 do flTxt.ReadLine;  // dump header
    var kk  : Integer:= 0;  // time step counter
    var rchC: Integer:= 0;  // reach counter
    setLength(result,length(Qrch),1000);

    while not fltxt.EndOfStream do begin
      str:= flTxt.ReadLine;                 // read data
      inc(rchC);
      cols.Clear;
      extractStrings([#32],[#32],pChar(str),cols);
      for ii:= Low(Qrch) to High(Qrch) do begin
        if cols[1].ToInteger = Qrch[ii] then begin
          result[ii][kk]:= cols[6].ToDouble;
          break;
        end;
      end;
      if rchC = nRch then begin
        inc(kk);
        rchC:= 0;
      end;
      if kk = length(result[0]) then setLength(result,length(Qrch),2*length(result[0]));
    end;
    setLength(result,length(Qrch),kk);
  finally
    flTxt.Free;
    cols.Free;
  end;
end;



function computeMetrics(var Qobs,Qs: Tarray<Tarray<Double>>): Tmetrics;
begin
  setLength(result.mtrc,length(Qobs) - 2);
  for var ii:= 2 to High(Qobs) do begin
    result.mtrc[ii - 2].KGE  := roundTo(KGEC(Qobs[ii],  Qs[ii - 2],-9999),-4);
    result.mtrc[ii - 2].NSE  := roundTo(NSEC(Qobs[ii],  Qs[ii - 2],-9999),-4);
    result.mtrc[ii - 2].PBIAS:= roundTo(PBIASe(Qobs[ii],Qs[ii - 2],-9999),-4);

    result.avg.KGE  := result.avg.KGE   + result.mtrc[ii - 2].KGE;
    result.avg.NSE  := result.avg.NSE   + result.mtrc[ii - 2].NSE;
    result.avg.PBIAS:= result.avg.PBIAS + result.mtrc[ii - 2].PBIAS;
  end;
  result.avg.KGE  := result.avg.KGE/length(result.mtrc);
  result.avg.NSE  := result.avg.NSE/length(result.mtrc);
  result.avg.PBIAS:= result.avg.PBIAS/length(result.mtrc);
end;



function warmUpPeriodIDX(const PRC: TlookUpTBL): Integer;
begin
  result:= -1;
  var yr:= fldrs.sYrsSim + fldrs.skipYrs - 1;                                   // last year to skip
  for var kk: Integer:= Low(PRC.dta[0]) to High(PRC.dta[0]) do
    if PRC.dta[0][kk] > yr then begin
      result:= kk;
      break;
    end;
end;



function PRCcorrection(const alpha: Double;
                       const Qobs : Tarray<Tarray<Double>>;
                       const Qsim : Tarray<Tarray<Double>>;
                       const PRC  : TlookUpTBL;
                       var   Pupd : TlookUpTBL;
                       var   WBlnc: Double;
                       const WPidx: Integer): Double;                           // return maximum value
var
Qr : Double;  // discharge ratio
CF : Double;  // correction factor
cnt: Integer; // counter;
begin
  result:= 0;
  WBlnc := 0;
  for var kk:= Low(Qobs[0]) to High(Qobs[0]) do begin                           // daily loop
    CF := 0;
    cnt:= 0;
    for var ii:= 2 to High(Qobs) do begin
      if Qobs[ii][kk] = -9999 then continue;
      if (Qsim[ii - 2][kk] > 0) then begin
        Qr := Qobs[ii][kk]/Qsim[ii - 2][kk];
        CF := CF + exp(alpha - alpha/Qr);
        inc(cnt)
      end;
    end;
    if (CF > 0) and (cnt > 0) then begin
      CF:= CF/cnt;
      for var jj:= 2 to High(Pupd.dta) do begin                                 // station
        Pupd.dta[jj][WPidx + kk]:= CF*Pupd.dta[jj][WPidx + kk];                 // corrected precipitation
        WBlnc:= WBlnc + Pupd.dta[jj][WPidx + kk] - PRC.dta[jj][WPidx + kk];
        if Pupd.dta[jj][WPidx + kk] > result then result:= Pupd.dta[jj][WPidx + kk];
      end;
    end;
  end;
end;


{
function PRCcorrection(const alpha: Double;
                       const Qobs : Tarray<Tarray<Double>>;
                       const Qsim : Tarray<Tarray<Double>>;
                       const PRC  : TlookUpTBL;
                       var   Pupd : TlookUpTBL;
                       var   WBlnc: Double;
                       const WPidx: Integer): Double;                           // return maximum value
var
Qr : Double;  // discharge ratio
CF : Double;  // correction factor
cnt: Integer; // counter;
begin
  result:= 0;
  WBlnc := 0;
  for var kk:= Low(Qobs[0]) to High(Qobs[0]) do begin                           // daily loop
    CF := 0;
    cnt:= 0;



    var mCF: Double:= 0;
    var xxx: Tarray<double>;
    setLength(xxx,length(Qobs) - 2);




    for var ii:= 2 to High(Qobs) do begin
      if Qobs[ii][kk] = -9999 then continue;
      if (Qsim[ii - 2][kk] > 0) then begin
        Qr := Qobs[ii][kk]/Qsim[ii - 2][kk];



        xxx[ii - 2]:= exp(alpha - alpha/Qr);
        CF:= XXX[ii - 2] + CF;
        if xxx[ii - 2] > mCF then mCF:= xxx[ii - 2];



//        CF := CF + exp(alpha - alpha/Qr);
        inc(cnt)
      end;
    end;
    if (CF > 0) and (cnt > 0) then begin
      CF:= CF/cnt;






      for var jj:= 2 to High(Pupd.dta) do begin                                 // station
        Pupd.dta[jj][WPidx + kk]:= CF*Pupd.dta[jj][WPidx + kk];                 // corrected precipitation
        WBlnc:= WBlnc + Pupd.dta[jj][WPidx + kk] - PRC.dta[jj][WPidx + kk];
        if Pupd.dta[jj][WPidx + kk] > result then result:= Pupd.dta[jj][WPidx + kk];
      end;
    end;
  end;
end;
}


procedure savePRCtoFile(var PRC: TlookUpTBL;fNme: String);
var
str: String;
begin
  var flTxt:= TstreamWriter.Create(fNme);  // precipitation
  try
    for var ii:= Low(PRC.hdr) to High(PRC.hdr) do flTxt.WriteLine(PRC.hdr[ii]);
    for var ii:= Low(PRC.dta[0]) to High(PRC.dta[0]) do begin
      str:= Format('%4d',[Trunc(PRC.dta[0][ii])]) + Format('%.3d',[Trunc(PRC.dta[1][ii])]);
      for var jj:= 2 to High(PRC.dta) do str:= str + Format('%5.1f',[PRC.dta[jj][ii]]);
      flTxt.WriteLine(str);
    end;
  finally
    flTxt.Close;
    flTxt.Free;
  end;
end;



function mainReanalysis(const PRC  : TlookUpTbl;       //
                        Qobs       : TlookUpTbl;       //
                        Qrch       : Tarray<Integer>;  // reach to be extracted
                        const maxP : Double;           //
                        alpha      : TrgeSim;          // back correction coefficient
                        beta       : TrgeSim;          // back correction coefficient
                        PBthld     : Double;           // precipitation water balance threshold
                        WBactive   : Boolean;          // precipitation water balance check
                        nRch       : Integer): ITask;  // number of reachs
var
Pupd : TlookUpTbl;              // corrected precipitation
Qs   : Tarray<Tarray<Double>>;  // extracted simulated streamflow
loop : Integer;                 //
mBase: Tmetrics;                // initial metrics
uMtrc: Tmetrics;                // step metric
flag : Boolean;                 //
KGEp : Double;                  // previous loop KGE
best : Tmetrics;                // best KGE for all reAnalysis
begin
  result:= TTask.Create(procedure
  var
  mxP  : Double;       // maximum corrected precipitation
  WB   : Double;
  PRCS : TStrings;
  begin
    PRCS := TstringList.Create;
    try
      var err   : String;
      var str   : String;
      var aClone: TrgeSim:= alpha;
      chDir(fldrs.SWATdir);    // working directory
      var swat: String:= fldrs.SWATdir + 'swat.exe';
      // total observed precipitation
      var WPidx := warmUpPeriodIDX(PRC);                                        // end of warmUP period index
      var sumP  : Double:= 0;                                                   // observed total precipitation
      for var ii:= 2 to High(PRC.dta) do
        for var jj:= WPidx to High(PRC.dta[ii]) do sumP:= sumP + PRC.dta[ii][jj];
      // compute baseline metrics
      Qs   := extractQsim(fldrs.SWATdir + 'output.rch',Qrch,nRch);
      mBase:= computeMetrics(Qobs.dta,Qs);
      KGEp := mBase.avg.KGE;
      str  := 'BASELINE' + #44 + mBase.avg.KGE.ToString + #44 +
              mBase.avg.NSE.ToString + #44 + mBase.avg.PBIAS.ToString;
      PRCS.Add(str);
      broadcastMssg(mssg_report,str,nil);
      // main loop
      while beta.sVal <= beta.eVal do begin      // beta : max. correction
        alpha:= aClone;
        while alpha.sVal <= alpha.eVal do begin  // alpha: error function parameter
          loop:= 1;
          Pupd:= PRC;
          // copy PRC and RCH files from result folder to SWAT folder
          Tfile.Copy(fldrs.outDir + extractFileName(fldrs.PRCdir),fldrs.PRCdir,TRUE);
          Tfile.Copy(fldrs.outDir + 'output.rch',fldrs.SWATdir + 'output.RCH',TRUE);
          // reset loop
          flag:= TRUE;
          while flag do begin
            mxP:= PRCcorrection(alpha.sVal,Qobs.dta,Qs,PRC,Pupd,WB,WPidx);      // P CORRECTION
            if mxP > beta.sVal*maxP then flag:= FALSE;                          // end the loop
            savePRCtoFile(Pupd,fldrs.PRCdir);
            err  := runAndWait(fldrs.SWATdir + 'swat.exe','',fldrs.SWATdir,INFINITE,0);
            Qs   := extractQsim(fldrs.SWATdir + 'output.rch',Qrch,nRch);
            uMtrc:= computeMetrics(Qobs.dta,Qs);                                // metrics
            uMtrc.avg.WBlnc:= roundTo(WB/sumP,-4);                              // P balance
            broadcastMssg(mssg_progress,beta.sVal.ToString + #44 + alpha.sVal.toString + #44 + uMtrc.avg.WBlnc.ToString,nil);
            if WBactive then                                                    // precipitation balance check
              if abs(uMtrc.avg.WBlnc) > abs(0.01*PBthld) then break;            // end the loop
            str  := beta.sVal.ToString + #44 + alpha.sVal.toString + #44 +
                    loop.ToString + #44 + uMtrc.avg.KGE.ToString + #44 +
                    uMtrc.avg.NSE.ToString + #44 + uMtrc.avg.PBIAS.ToString + #44 +
                    uMtrc.avg.WBlnc.ToString;                                   // reporting
            PRCS.Add(str);
            broadcastMssg(mssg_report,str,nil);
            inc(loop);

            if uMtrc.avg.KGE <= KGEp then break
            else begin                                                          // metric improvement
              KGEp:= uMtrc.avg.KGE;
              broadcastMssg(mssg_updateStat,str,nil);
              if uMtrc.avg.KGE > best.avg.KGE then begin                        // best metrics
                best:= uMtrc;
                savePRCtoFile(Pupd,fldrs.outDir + 'pcp.BEST.PCP');
                Tfile.Copy(fldrs.SWATdir + 'output.RCH',fldrs.outDir + 'output.BEST.rch',TRUE);
                broadcastMssg(mssg_updateBest,str,nil);
              end;
            end;
          end;
          alpha.sVal:= alpha.sVal + alpha.step;
        end;
        beta.sVal:= beta.sVal + beta.step;
      end;
    finally
      PRCS.SaveToFile(fldrs.outDir + 'reAnalysis.txt');
      PRCS.Free;
      Tfile.Copy(fldrs.outDir + extractFileName(fldrs.PRCdir),fldrs.PRCdir,TRUE);
      Tfile.Copy(fldrs.outDir + 'output.rch',fldrs.SWATdir + 'output.RCH',TRUE);
      broadcastMssg(mssg_done,'',nil);
    end;
  end);
  result.Start;
end;




END.
