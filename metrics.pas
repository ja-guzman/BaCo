unit metrics;


INTERFACE


function NSEC(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;
function PBIASe(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;
function KGEC(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;



IMPLEMENTATION


function NSEC(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;
var
err : Double;
mean: Double;
begin
  result:= -1;
  if length(Qobs) <> length(Qsim) then exit;
  var cnter: Integer:= 0;
  var sumO : Double:= 0;
  var sumS : Double:= 0;
  for var ii:= Low(Qobs) to High(Qobs) do begin
    if Qobs[ii] = NDC then continue;
    inc(cnter);
    sumO:= sumO + Qobs[ii];
    err:= (Qobs[ii] - Qsim[ii]);
    sumS:= sumS + err*err;
  end;
  mean:= sumO/cnter;

  sumO:= 0;
  for var ii:= Low(Qobs) to High(Qobs) do begin
    if Qobs[ii] = NDC then continue;
    err:= Qobs[ii] - mean;
    sumO:= sumO + err*err;
  end;
  result:= 1 - sumS/sumO;
end;



function PBIASe(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;
begin
  result:= -1;
  if length(Qobs) <> length(Qsim) then exit;
  var sumO: Double:= 0;
  var sumS: Double:= 0;
  for var ii:= Low(Qobs) to High(Qobs) do begin
    if Qobs[ii] = NDC then continue;
    sumO:= sumO + Qobs[ii];
    sumS:= sumS + Qsim[ii];
  end;
  result:= (sumS - sumO)/sumO;
end;



function KGEC(Qobs,Qsim: Tarray<Double>;NDC: Integer): Double;
var aa,bb: Double;
begin
  result:= -1;
  if length(Qobs) <> length(Qsim) then exit;
  // beta
  var cnter: Integer:= 0;
  var meanO: Double := 0;
  var meanS: Double := 0;
  for var ii:= Low(Qobs) to High(Qobs) do begin
    if Qobs[ii] = NDC then continue;
    meanO:= meanO + Qobs[ii];
    meanS:= meanS + Qsim[ii];
    inc(cnter);
  end;
  meanO:= meanO/cnter;
  meanS:= meanS/cnter;
  var beta:= meanS/meanO;
  // alpha
  var stdO: Double:= 0;
  var stdS: Double:= 0;
  var sumP: Double:= 0;
  for var ii:= Low(Qobs) to High(Qobs) do begin
    if Qobs[ii] = NDC then continue;
    aa:= (Qobs[ii] - meanO);
    bb:= (Qsim[ii] - meanS);
    stdO:= stdO + aa*aa;
    stdS:= stdS + bb*bb;
    sumP:= sumP + aa*bb;
  end;
  stdO:= sqrt(stdO/(cnter - 1));
  stdS:= sqrt(stdS/(cnter - 1));
  var alpha:= stdS/stdO;
  // Pearson CC
  var PCC:= sumP/(stdS*stdO*(cnter - 1));
  // --- KGE
  result:= 1 - sqrt(sqr(PCC - 1) + sqr(alpha - 1) + sqr(beta - 1));
end;


END.
