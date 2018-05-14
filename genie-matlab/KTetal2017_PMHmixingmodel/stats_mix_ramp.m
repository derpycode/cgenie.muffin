% Calculate the apparent CIE onset given mixing of surface foram d13C
% 3May16 (original code: 21Mar16)
% PM Hull

% Notes
%   1) An individual is  'intermediate' if is outside 3-sds (3 * 0.3) of
%       pre- and post-event (3.1 and 0) values (i.e., 0.9-2.2)
%   2)CIE onset duration determined as the longest sample interval with
%       consecutive 'intermediates' present
%   3)CIE onset duration determined for a number of sampling intensities
%       (n=5,10,20,50,100,500)
%%   Calculate % samples w/onset duration recorded by intermediates > than real

cd('/Users/Celli/Documents/research/mix690/mixing-modeling/experiments/mixing.profiles')
for a = 1:7  

    % loop through isotope/abundance ramps (i.e., CIE onset durations)
    if a==1
        load ('mixed-noramp.mat');
    elseif a==2
        load ('mixed-1kyrramp.mat');
    elseif a==3
        load ('mixed-2kyrramp.mat');
    elseif a==4
        load ('mixed-3kyrramp.mat');
    elseif a==5
        load ('mixed-5kyrramp.mat');
    elseif a==6
        load ('mixed-10kyrramp.mat');
    elseif a==7
        load ('mixed-15kyrramp.mat');
    end

    %remove nans from sCell
    sCell(cellfun(@(sCell) any(isnan(sCell)),sCell)) = [];

    %Prepare variables to calculate statistics 
    bootInterm=1000;
    surfInt5 = zeros(length(sCell), bootInterm); surfInt10=surfInt5; 
    surfInt20=surfInt5;surfInt50=surfInt5; surfInt100=surfInt5; surfInt500=surfInt5; 
     
    % Detect all samples with 'intermediate' individuals
    for k = 1:length(sCell)
        for x= 1:bootInterm
            
            %bootstrap sCell with different sampling intensities
            t5=datasample(sCell{k},5,'Replace', false);
            t10=datasample(sCell{k},10,'Replace', false);
            t20=datasample(sCell{k},20,'Replace', false);
            t50=datasample(sCell{k},50,'Replace', false);
            t100=datasample(sCell{k},100,'Replace', false);
            t500=datasample(sCell{k},500,'Replace', false);
        
            %detect intermediates (>0.9 & >2.2 per mil) 
            surfInt5(k,x) = logical(sum((t5>0.9)&(t5<2.2)));
            surfInt10(k,x) = logical(sum((t10>0.9)&(t10<2.2)));
            surfInt20(k,x) = logical(sum((t20>0.9)&(t20<2.2)));
            surfInt50(k,x) = logical(sum((t50>0.9)&(t50<2.2)));
            surfInt100(k,x) = logical(sum((t100>0.9)&(t100<2.2)));
            surfInt500(k,x) = logical(sum((t500>0.9)&(t500<2.2)));

            clear t5 t10 t20 t50 t100 t500
        end
        
    end

    %Calculate longest interval of samples with overlapping individuals
    durOverlap=zeros(6, bootInterm); %6 sampling intensities (5,10,20,50,100,500)
    index=[1:length(sCell)];  %index for cells in sCell
    
    for xx= 1:bootInterm %uses custom function 'cie' to determine CIE length
        % NOTE !!  cie function assumes sampling parameters of ODP 690, so
        % this function needs modified to be used in a general application
        durOverlap(1,xx)=cie(surfInt5,xx,bootInterm,index);
        durOverlap(2,xx)=cie(surfInt10,xx,bootInterm, index);
        durOverlap(3,xx)=cie(surfInt20,xx,bootInterm, index);
        durOverlap(4,xx)=cie(surfInt50,xx,bootInterm, index);
        durOverlap(5,xx)=cie(surfInt100,xx,bootInterm, index);
        durOverlap(6,xx)=cie(surfInt500,xx,bootInterm, index);
    end

    % loop through isotope/abundance ramp statistics saving
    if a==1
        save ('stats-noramp.mat', 'durOverlap')
    elseif a==2
        save ('stats-1kyrramp.mat', 'durOverlap')
    elseif a==3
        save ('stats-2kyrramp.mat', 'durOverlap')
    elseif a==4
        save ('stats-3kyrramp.mat', 'durOverlap')
    elseif a==5
        save ('stats-5kyrramp.mat', 'durOverlap')
    elseif a==6
        save ('stats-10kyrramp.mat', 'durOverlap')
    elseif a==7
        save ('stats-15kyrramp.mat', 'durOverlap')
    end
    
    clear bootInterm durOverlap index k sCell surfInt10 surfInt100 surfInt20 ...
    surfInt5 surfInt50 surfInt500 x xx
end
    
   
   