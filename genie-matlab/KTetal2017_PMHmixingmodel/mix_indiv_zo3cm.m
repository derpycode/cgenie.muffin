% Individual foraminifera mixing: set for ODP 690
% 22Jan17 modifed to test different profiles from 22Apr16 code (original code: 5Mar16)
% PM Hull
%------------%------------%------------%------------%------------
%% Load Mixing Profiles 

% change directory to data files
cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling/experiments/mixing.profiles')

load('SedRate0.3_k0.25_zo3_zs1.5.mat', 'histarch')    % point mixing profiles
mix1=histarch; clear histarch
%------------%------------%------------%------------%------------%
%% Process and Calculate Typical Sediment Mixing Profile

% Extract mixing profile & trim 
sedRate=0.3;  	% cm/kyr; sed. rate (from generating point mixing code)
depInc=0.2; 	% cm; sed. depth increment during mixing (from generating point mixing code)
sedcol=400;     % cm; full sediment column height (from generating point mixing code)
stpt=length(0:300);	% time starts 0kyr, ends 300kyr (from generating point mixing code)

% calculate typical (median) mixing profile & use throughout
mix1med=median(mix1,2);	
plot(mix1med)  % check out median mixing profile

% center profile on profile start point (0depth==center mixing)
mix1Index=(1:length(mix1med));
mix1depth=(0:depInc:sedcol)-(stpt*sedRate);
plot(mix1depth,mix1med) % double check mixing profile

% trim mixing profile to use as sliding-sediment mixer
t1min=min(mix1Index(mix1med>0)); t1max=max(mix1Index(mix1med>0)); % set edges
mix1trim=horzcat(mix1depth(t1min:t1max)', mix1med(t1min:t1max));  % trim the sliding window
plot(mix1trim(:,1), mix1trim(:,2))  %check it!

%define impulse point in the new reference frame
idxSt= (floor((stpt*sedRate)/depInc)+2)-t1min;
%------------%------------%------------%------------%------------%
%% Set Up Gap & Abundance Experiments

% set up big block of sediment (so edge-effects can be trimmed)
maxBlock=6*100; 	% 6meters *100 cm/m
block=(0:depInc:maxBlock);  % make column of sediment chunks 
midpt=round(length(block)/2); % mid-point anchors PETM(@midpt) & gaps 
minInset=length(0:depInc:100); % edge trimming on sediment block (upper)
maxInset=length(block)-minInset;% edge trimming on sediment block (lower)

% gap experiments: interval of non-deposition (gap5: example option)
% gap5=(5/depInc); % 5cm gap in cm/depth increment
gap10=(10/depInc); % 5cm gap in cm/depth increment

% gaps for experiments (gap coincident with onset)
gappy0 = ones(length(block),1);
% gappy5 = vertcat(ones((midpt-1)-gap5,1), ones(gap5,1)*NaN, ones(midpt,1));
gappy10 = vertcat(ones((midpt-1)-gap10,1), ones(gap10,1)*NaN, ones(midpt,1));

% abundance experiments (lower in 'CIE'); abundlow crashes most computers
abund0 = ones(length(block),1); % no change in abundance
abund50 = vertcat(ones(midpt-1,1), ones(midpt,1)*2);    % 50% decline
abund2p5 = vertcat(ones(midpt-1,1), ones(midpt,1)*40);  % 97.5% decline
abund1 = vertcat(ones(midpt-1,1), ones(midpt,1)*100);   % 99% decline
% abundlow = vertcat(ones(midpt-1,1), ones(midpt,1)*500);   % >99% decline

%------------%------------%------------%------------%------------%
%% Parametrize umixed isotope lists: step-change in ODP 690-type values

% 'Surface' Species: step change from 3.1 per mil to 0 per mil in CIE
surfMean=vertcat(ones(midpt-1,1)*0, ones(midpt,1)*3.1); 
surfSd=0.3;  % st dev of individual isotope values within a sample

% 'Thermocline' Species: step change from 1.6 per mil to -0.4 per mil in CIE
thermoMean=vertcat(ones(midpt-1,1)*(-0.4), ones(midpt,1)*1.6); 
thermoSd=0.12;
%------------%------------%------------%------------%------------%
%% Set-Up Mix up: Single forams and bulk carbonates

% Generate index of depth in sediment column
section = vertcat(block, (block*sedRate), (1:length(block)))' ;
section=section(minInset:maxInset,:);

% # forams sampled per sample 
indiv=4;    % value to match empirical ODP 690
    
% Gap at CIE Onset: options include gappy0,gappy5,gappy10
surfy=surfMean .* gappy0;   % surface foram: no gap
thermy=thermoMean .* gappy10;   % thermocline foram: no gap (alt:10cm (i.e., 4kyr) gap)
    
% Relative abundance change
ABUNDs=abund50;  % surface foram: 50% decline
ABUNDt=abund2p5;   % surface foram: 97.5% decline

% Foram abundance profiles (mixing profiles)
mixS=floor(mix1trim(:,2));         
mixT=floor(mix1trim(:,2));  
    
% idxSt and idxEnd needed to keep the sliding mixing profile in the block
% idxSt: index impulse point (defined above)
idxEnd=length(block)-(length(mixS)-idxSt);
    
% Set up cell-structure for results
sCell = cell(1, length(block));  %saving block for surface forams
tCell = cell(1, length(block)); %saving block for thermocline forams
%------------%------------%------------%------------%------------%
%% Run Mixing experiment

% Loop slides mixed window down sediment block and considers fate of each 
% chunk of sediment individually (0.2 cm at a time)
for b = idxSt:idxEnd   
   
    % Sediment block sliding position
    startB=(b-idxSt+1);
    stopB=b+(length(mixS)-idxSt)-1;
        
    tick=1; % ticker trackers relative location within mixing profile
    
    for j = startB:stopB  % Consider fate of fossils from a given sediment chunk
        
        if mixS(tick)>0    % For surface foraminifera
            tempJ = surfSd .* randn((mixS(tick)*ABUNDs(b)),1) + surfy(b);
             sCell{j} = vertcat(sCell{j}, tempJ);
             clear tempJ
        end
     
        if mixT(tick)>0   % For thermocline foraminifera
           tempJ = thermoSd .* randn((mixT(tick)*ABUNDt(b)),1) + thermy(b);
           tCell{j} = vertcat(tCell{j}, tempJ);
           clear tempJ
        end
        
        tick=tick+1;
        
    end
    
    clear tick
        
end
%------------%------------%------------%------------%------------%
%% Trim & Calc. Results: reduces file size & speeds processing

% trim edges of sediment block ('burn in' edges)
tCell=tCell(minInset:maxInset);
sCell=sCell(minInset:maxInset);
sAbund=ABUNDs.*gappy0; sAbund=sAbund(minInset:maxInset);
tAbund=ABUNDt.*gappy10; tAbund=tAbund(minInset:maxInset);

% subsample to match emperical sample resolution (every 2cm at ODP 690)
subsample=(1:10:length(section(:,1)));
tCell=tCell(subsample);
sCell=sCell(subsample);
section=section(subsample,:);
sAbund=sAbund(subsample);
tAbund=tAbund(subsample);

% trim all variables to match plotting window: central plotting window is
% 250-330 cm in the section-index.  250cm is at index 76; 330cm at 116
winStart=76; winStop=116;
tCell=tCell(winStart:winStop);
sCell=sCell(winStart:winStop);
section=section(winStart:winStop, :);
sAbund=sAbund(winStart:winStop);
tAbund=tAbund(winStart:winStop);

% calculate apparent abundance change, post-mixing
surfPopSize=zeros(length(winStart:winStop), 1);
thermoPopSize=zeros(length(winStart:winStop), 1);

for k = 1:length(tCell)  % count up individuals in each cell
    surfPopSize(k,1) = length(sCell{k});
    thermoPopSize(k,1) = length(tCell{k});
end
surfPopSize=(surfPopSize/max(surfPopSize))*100;  %change abs. abundance to percent
thermoPopSize=(thermoPopSize/max(thermoPopSize))*100;
%------------%------------%------------%------------%------------%
%% Save results

save ('mixed_taxa-stepchange-zo3-gap0gap10-ab50ab2p5.mat', 'section','sCell','tCell', 'sAbund',...
    'tAbund','surfPopSize', 'thermoPopSize')

clear all

