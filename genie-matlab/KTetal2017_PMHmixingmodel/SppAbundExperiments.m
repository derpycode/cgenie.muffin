% Individual foraminifera mixing: set for ODP 690
% Version modified for relative abundance experiments: 23Jan17
% PM Hull
%------------%------------%------------%------------%------------
%% Load Mixing Profiles 

% change directory to data files
cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling/experiments/mixing.profiles')

load('SedRate2.5_k3_zo10_zs1.5.mat', 'histarch')    % point mixing profiles
mix1=histarch; clear histarch
%------------%------------%------------%------------%------------%
%% Process and Calculate Typical Sediment Mixing Profile

% Extract mixing profile & trim 
sedRate=2.5;  	% cm/kyr; sed. rate (from generating point mixing code)
depInc=0.2; 	% cm; sed. depth increment during mixing (from generating point mixing code)
sedcol=200;     % cm; full sediment column height (from generating point mixing code)
stpt=length(0:60);	% time starts 0kyr, ends 60kyr (from generating point mixing code)

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
gap2=(2/depInc); % 2cm gap in cm/depth increment
gap4=(4/depInc);
gap5=(5/depInc); % 5cm gap in cm/depth increment
gap6=(6/depInc); % 6cm gap in cm/depth increment
gap8=(8/depInc); 
gap9=(9/depInc); 
gap10=(10/depInc); 
gap12=(12/depInc);
gap13=(13/depInc);
gap14=(14/depInc); 

% gaps for experiments (gap coincident with onset)
gappy0 = ones(length(block),1);
gappy5 = vertcat(ones((midpt-1)-gap5,1), ones(gap5,1)*NaN, ones(midpt,1));
gappy10 = vertcat(ones((midpt-1)-gap10,1), ones(gap10,1)*NaN, ones(midpt,1));

% Gap scenario to match emperical record of Subbotina
gappyMany=vertcat(ones((midpt-(1+gap6))-gap12,1),ones((gap2),1)*NaN,...
    ones((gap2/2),1),ones((gap9),1)*NaN, ones((gap2),1), ones((gap2*2),1)*NaN,...
    ones(gap6,1)*NaN, ones((gap2/5),1), ones((midpt-(gap6+gap2/5)),1));

% Gap scenario to match emperical record of Acaranina
gappyAcar = vertcat(ones((midpt-(1+gap4)),1), ones((gap4),1)*NaN, ...
    ones((gap2/2),1),ones((gap2*2),1),ones((gap2+(gap2/2)),1)*NaN, ...
    ones((midpt-gap8),1));

% abundance experiments (lower in 'CIE'); abundlow crashes most computers
abund0 = ones(length(block),1); % no change in abundance
abund50 = vertcat(ones(midpt-1,1), ones(midpt,1)*2);    % 50% decline
abund2p5 = vertcat(ones(midpt-1,1), ones(midpt,1)*40);  % 97.5% decline
abund1 = vertcat(ones(midpt-1,1), ones(midpt,1)*100);   % 99% decline

%abundSub = vertcat(ones(midpt-1,1), ones(gap5,1)*5, ones((midpt-gap5),1)*2); 
abundSub=vertcat(ones((midpt-(1+gap6))-gap12,1), ones((gap2),1),...
    ones((gap2/2),1)*20,ones((gap9),1),ones(gap2,1)*40, ones((gap2*2),1),...
    ones((gap6),1)*4,ones((gap2/5),1)*120, ones((midpt-(gap6+gap2/5)),1)*4);

abundAcar = vertcat(ones((midpt-(1+gap4)),1)*2, ones((gap4),1)*2, ...
    ones((gap2/2),1)*10,ones((gap4),1)*10,ones((gap2+(gap2/2)),1), ...
    ones((midpt-gap8),1)*8);

%------------%------------%------------%------------%------------%
%% Parametrize umixed isotope lists: step-change in ODP 690-type values

% 'Surface' Species: step change from 3.1 per mil to 0 per mil in CIE
surfMean=vertcat(ones(midpt-(1+gap6),1)*0, ones((midpt+gap6),1)*3.1); 
surfSd=0.3;  % st dev of individual isotope values within a sample

% 'Thermocline' Species: step change from 1.6 per mil to -0.4 per mil in CIE
thermoMean=vertcat(ones(midpt-(1+gap6),1)*(-0.4), ones((midpt+gap6),1)*1.6); 
thermoSd=0.12;
%------------%------------%------------%------------%------------%
%% Set-Up Mix up: Single forams and bulk carbonates

% Generate index of depth in sediment column
section = vertcat(block, (block*sedRate), (1:length(block)))' ;
section=section(minInset:maxInset,:);

% # forams sampled per sample 
indiv=4;    % value to match empirical ODP 690
    
% Gap at CIE Onset: options include gappy0,gappy5,gappy10
surfy=surfMean .* gappyAcar;   % surface foram: no gap
thermy=thermoMean .* gappyMany;   % thermocline foram: 10cm (i.e., 4kyr) gap
    
% Relative abundance change
ABUNDs=abundAcar;  % surface foram: 50% decline
ABUNDt=abundSub;   % surface foram: 97.5% decline

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
sAbund=ABUNDs.*gappyAcar; sAbund=sAbund(minInset:maxInset);
tAbund=ABUNDt.*gappyMany; tAbund=tAbund(minInset:maxInset);

% subsample to match emperical sample resolution (every 2cm at ODP 690)
storeMinSection = min(section(:,1));
storeMaxSection = max(section(:,1));
subsample=(1:10:length(section(:,1)));
tCell=tCell(subsample);
sCell=sCell(subsample);
section=section(subsample,:);
%sAbund=sAbund(subsample);
%tAbund=tAbund(subsample);

% trim all variables to match plotting window: central plotting window is
% 250-330 cm in the section-index.  250cm is at index 76; 330cm at 116
winStart=76; winStop=116;
tCell=tCell(winStart:winStop);
sCell=sCell(winStart:winStop);
section=section(winStart:winStop, :);
%sAbund=sAbund(winStart:winStop);
%tAbund=tAbund(winStart:winStop);

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
%% Read in emperical data and plot result
cd('/Users/Celli/Documents/research/in_review/mix690/empirical-data')
plankAbund=csvread('SedPercentTaxa.csv'); %column headers: depth, Sed % Acar., Sed % Sub

surfPopSizeScaled = surfPopSize*(max(plankAbund(:,2))/100); % change per.abund to scale of sedimentary data
thermoPopSizeScaled = thermoPopSize*(max(plankAbund(:,3))/100); % change per.abund to scale of sedimentary data

% Translate depth in simulation (cm) to common scale (m) and interval
% (170.4) as empirical data
metersSim=section(:,1)/100;
anchSim = min(metersSim);
metersSim=metersSim+(170.3-anchSim);

% Make depth scale for original simulation abundance 
metersResolve = (storeMinSection:depInc:storeMaxSection);
metersResolve = metersResolve/100;
metersResolve = metersResolve + (170.3 - anchSim);

figure;

subplot(1,4,1);  % emperical data
hold on;

scatter(plankAbund(:,2),plankAbund(:,1),'rs','MarkerFaceColor','r')
scatter(plankAbund(:,3),plankAbund(:,1),'bx')

title ('ODP 690')
set(gca,'Ydir','reverse')
axis([0 6 170.4 171.0]);
xlabel('Sed. % Abund')
ylabel ('meters depth in core (mcd)')

subplot(1,4,2);     % modeled data: population change (unmixed & mixed)
hold on;
plot((tAbund/(max(tAbund))*100), metersResolve, 'b', 'LineWidth', 1)
plot((sAbund/(max(sAbund))*100), metersResolve, 'r','LineWidth', 1)
scatter(thermoPopSize, metersSim,'bx');
scatter(surfPopSize, metersSim,'rs','MarkerFaceColor','r');

title ('Modeled Abundance')
set(gca,'Ydir','reverse')
axis([0 100 170.4 171.0]);
xlabel('Rel. Pop. Size')

subplot(1,4,3);     % modeled data: population change (unmixed & mixed)
hold on;
scatter(plankAbund(:,3),plankAbund(:,1),'bx')
scatter(thermoPopSizeScaled, metersSim,'gs');

title ('Model vs Actual: Thermo. Abund')
set(gca,'Ydir','reverse')
axis([0 6 170.4 171.0]);
xlabel('Rel. Pop. Size')

subplot(1,4,4);     % modeled data: population change (unmixed & mixed)
hold on;
scatter(plankAbund(:,2),plankAbund(:,1),'rs','MarkerFaceColor','r')
scatter(surfPopSizeScaled, metersSim,'ms');

title ('Model vs Actual: Thermo. Abund')
set(gca,'Ydir','reverse')
axis([0 6 170.4 171.0]);
xlabel('Rel. Pop. Size')

cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling')
saveas(gcf, 'Fig-AbundanceMatching.pdf')
%% Extract a single profile as an example

% Extract a 4-individuals at random for an example profile
indiv=4;    % # forams sampled per sample 
surfGapExpAbundExp = zeros(length(sCell), indiv);
thermGapExpAbundExp = zeros(length(sCell), indiv);

for k = 1:length(tCell)
    surfGapExpAbundExp(k,:) = datasample (sCell{k},indiv,'Replace', false);
    thermGapExpAbundExp(k,:) = datasample (tCell{k},indiv,'Replace', false);
    mBulk(k,1)=nanmean(tCell{k});  % model average bulk carbonate as thermocline mean
end

%Load Mixing Profiles

% load in 690 data
cd('/Users/Celli/Documents/research/in_review/mix690/empirical-data')

acar = load('690_Acar.dat','-ascii');
acar(:,2) = [];
acar(:,2) = [];
acar(:,2) = [];
subb = load('690_Subb.dat','-ascii');
subb(:,2) = [];
subb(:,2) = [];
cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling')

%% Plot Isotopes
figure;

subplot(1,4,1);  % emperical data
hold on;
scatter(subb(:,2),subb(:,1),'bx');
scatter(subb(:,3),subb(:,1),'bx');
scatter(subb(:,4),subb(:,1),'bx');
scatter(subb(:,5),subb(:,1),'bx');
scatter(subb(:,6),subb(:,1),'bx');
scatter(subb(:,7),subb(:,1),'bx');
scatter(subb(:,8),subb(:,1),'bx');

scatter(acar(:,2),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,3),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,4),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,5),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,6),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,7),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,8),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,9),acar(:,1),'rs','MarkerFaceColor','r');
scatter(acar(:,10),acar(:,1),'rs','MarkerFaceColor','r');

title ('ODP 690')
set(gca,'Ydir','reverse')
axis([-2.0 5.0 170.4 171.0]);
xlabel('d13C')
ylabel ('meters depth in core (mcd)')

subplot(1,4,2);     % modeled data: single case study
hold on;
scatter(surfGapExpAbundExp(:,1),metersSim,'rs','MarkerFaceColor','r');
scatter(surfGapExpAbundExp(:,2),metersSim,'rs','MarkerFaceColor','r');
scatter(surfGapExpAbundExp(:,3),metersSim,'rs','MarkerFaceColor','r');
scatter(surfGapExpAbundExp(:,4),metersSim,'rs','MarkerFaceColor','r');

scatter(thermGapExpAbundExp(:,1),metersSim,'bx');
scatter(thermGapExpAbundExp(:,2),metersSim,'bx');
scatter(thermGapExpAbundExp(:,3),metersSim,'bx');
scatter(thermGapExpAbundExp(:,4),metersSim,'bx');

title ('Modeled Mixing')
set(gca,'Ydir','reverse')
axis([-2.0 5.0 170.4 171.0]);
xlabel('d13C')

subplot(1,4,3);     % modeled data: population change (unmixed & mixed)
hold on;
scatter(plankAbund(:,3),plankAbund(:,1),'bx')
scatter(thermoPopSizeScaled, metersSim,'gs');

title ('Model vs Actual: Thermo. Abund')
set(gca,'Ydir','reverse')
axis([0 6 170.4 171.0]);
xlabel('Rel. Pop. Size')

subplot(1,4,4);     % modeled data: population change (unmixed & mixed)
hold on;
scatter(plankAbund(:,2),plankAbund(:,1),'rs','MarkerFaceColor','r')
scatter(surfPopSizeScaled, metersSim,'ms');

title ('Model vs Actual: Thermo. Abund')
set(gca,'Ydir','reverse')
axis([0 6 170.4 171.0]);
xlabel('Rel. Pop. Size')


cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling')
saveas(gcf, 'Fig-AbundanceMatchingIsotopes-ver6.pdf')
%------------%------------%------------%------------%------------%
%% Save results

%save ('mixed_taxa-stepchange.mat', 'section','sCell','tCell', 'sAbund',...
%    'tAbund','surfPopSize', 'thermoPopSize', 'surfOverlap')

clear all

