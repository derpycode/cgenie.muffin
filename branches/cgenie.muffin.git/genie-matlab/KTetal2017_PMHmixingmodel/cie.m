function [CIEduration]= cie(surfInt,XX,BOOTInterm,INDEX)
    tn=INDEX(surfInt(:,XX)>0);
    CIEduration=0;
    if (length(tn)>0)
        tick=0;
        loopIndex=tn;
        while (length(loopIndex)>0)
            tempIndex=[1:length(diff(loopIndex))];
            loopIndex=tempIndex(diff(loopIndex)==1);
            clear tempIndex
            tick=tick+1;
        end
        %ticks into onset duration (given 2cm sample spacing;2.5 cm/kyr sedimentation)
        CIEduration = (tick-1)*2*(1/0.5);   % This is a ODP 690, CIE (early) specific parameterization!
        clear tick loopIndex
    end
 end
    