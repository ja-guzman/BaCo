unit core.mssg;


INTERFACE


USES
System.Messaging;


TYPE
  TmssgDrctv = (
                mssg_updateStat,  // update stat
                mssg_updateBest,  // update best metrics
                mssg_updatePRC,   // update precipitation
                mssg_report,
                mssg_progress,
                mssg_done
               );


  procedure broadcastMssg(prc: TmssgDrctv;msg: string;Sender: Tobject);


VAR
mssgManager: TmessageManager;


IMPLEMENTATION

USES
System.TypInfo,
System.Classes;



procedure broadcastMssg(prc: TmssgDrctv;msg: string;Sender: Tobject);
var
mStr: string;
mssg: TMessage;
begin
  mStr:= GetEnumName(TypeInfo(TmssgDrctv),ord(prc));
  mssg:= TMessage<string>.Create(mStr + #44 + msg); // encoded message
  Tthread.Synchronize(nil,procedure
  begin
    mssgManager.SendMessage(Sender,mssg,TRUE);      // message broadcast
  end);
end;



BEGIN
  mssgManager:= TmessageManager.DefaultManager;
END.





