% Individual foraminifera mixing: test effect of vary CIE-onset duration
% 24Apr16 (original code: 18Mar16)
% modified June 22nd,2016 for low-CIE sedimentation rates of 0.5cm/kyr
% PM Hull
%------------%------------%------------%------------%------------%
%% Load Mixing Profiles 

% change directory to data files
cd('/Users/Celli/Documents/research/mix690/mixing-modeling/experiments/mixing.profiles')

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
%------------%------------%------------%------------%------------%
%% Set Up Ramp Experiments: only for surface species

% set up big block of sediment (so edge-effects can be trimmed)
maxBlock=6*100; 	% 6meters *100 cm/m
block=(0:depInc:maxBlock);  % make column of sediment chunks 
midpt=round(length(block)/2); % mid-point anchors PETM(@midpt) & gaps 
minInset=length(0:depInc:100); % edge trimming on sediment block (upper)
maxInset=length(block)-minInset;% edge trimming on sediment block (lower)

%  50% abund. change (lower in 'CIE'): order mag. larger pop for ramp stats
abund50 = vertcat((ones(midpt-1,1)*10), ones(midpt,1)*20);    % 50% decline

% 'Surface' Species: step change from 3.1 per mil to 0 per mil in CIE
surfMean=vertcat(ones(midpt-1,1)*0, ones(midpt,1)*3.1); 
surfSd=0.3;  % st dev of individual isotope values within a sample

%CIE onset length (round down to whole steps; rounding offsets of up to 100 years)

%Divide all input duration by '5' to mimic the sedimentation rate drop to
%0.5kyr/cm
kyr1samp=floor((1/5)*sedRate*(1/depInc));
kyr2samp=floor((2/5)*sedRate*(1/depInc));
kyr3samp=floor((3/5)*sedRate*(1/depInc));
kyr5samp=floor((5/5)*sedRate*(1/depInc));
kyr10samp=floor((10/5)*sedRate*(1/depInc));
kyr15samp=floor((15/5)*sedRate*(1/depInc));
%------------%------------%------------%------------%------------%
%% Set Up Ramp Experiments: CIE ramp in isotope values
tY=[0 3.1];
%1 kyr
tX= [1 kyr1samp]; tInt=[1:kyr1samp]; tMean=interp1 (tX, tY, tInt);
surfMean1kyr=surfMean;
surfMean1kyr((midpt-kyr1samp):(midpt-1))= tMean;
clear tX tInt tMean

%2 kyr
tX= [1 kyr2samp]; tInt=[1:kyr2samp]; tMean=interp1 (tX, tY, tInt);
surfMean2kyr=surfMean;
surfMean2kyr((midpt-kyr2samp):(midpt-1))= tMean;
clear tX tInt tMean

%3 kyr
tX= [1 kyr3samp]; tInt=[1:kyr3samp]; tMean=interp1 (tX, tY, tInt);
surfMean3kyr=surfMean;
surfMean3kyr((midpt-kyr3samp):(midpt-1))= tMean;
clear tX tInt tMean

%5 kyr
tX= [1 kyr5samp]; tInt=[1:kyr5samp]; tMean=interp1 (tX, tY, tInt);
surfMean5kyr=surfMean;
surfMean5kyr((midpt-kyr5samp):(midpt-1)) = tMean;
clear tX tInt tMean

%10 kyr
tX= [1 kyr10samp]; tInt=[1:kyr10samp]; tMean=interp1 (tX, tY, tInt);
surfMean10kyr=surfMean;
surfMean10kyr((midpt-kyr10samp):(midpt-1))= tMean;
clear tX tInt tMean

%15 kyr
tX= [1 kyr15samp]; tInt=[1:kyr15samp]; tMean=interp1 (tX, tY, tInt);
surfMean15kyr=surfMean;
surfMean15kyr((midpt-kyr15samp):(midpt-1))= tMean;
clear tX tY tInt tMean
%------------%------------%------------%------------%------------%
%% Set Up Ramp Experiments: CIE ramp in abundance values
tY=[10 20];
%1 kyr
tX= [1 kyr1samp]; tInt=[1:kyr1samp]; tMean=interp1 (tX, tY, tInt);
abund1kyr=abund50;
abund1kyr((midpt-kyr1samp):(midpt-1))= tMean;
clear tX tInt tMean

%2 kyr
tX= [1 kyr2samp]; tInt=[1:kyr2samp]; tMean=interp1 (tX, tY, tInt);
abund2kyr=abund50;
abund2kyr((midpt-kyr2samp):(midpt-1))= tMean;
clear tX tInt tMean

%3 kyr
tX= [1 kyr3samp]; tInt=[1:kyr3samp]; tMean=interp1 (tX, tY, tInt);
abund3kyr=abund50;
abund3kyr((midpt-kyr3samp):(midpt-1))= tMean;
clear tX tInt tMean

%5 kyr
tX= [1 kyr5samp]; tInt=[1:kyr5samp]; tMean=interp1 (tX, tY, tInt);
abund5kyr=abund50;
abund5kyr((midpt-kyr5samp):(midpt-1))= tMean;
clear tX tInt tMean

%10 kyr
tX= [1 kyr10samp]; tInt=[1:kyr10samp]; tMean=interp1 (tX, tY, tInt);
abund10kyr=abund50;
abund10kyr((midpt-kyr10samp):(midpt-1))= tMean;
clear tX tInt tMean

%15 kyr
tX= [1 kyr15samp]; tInt=[1:kyr15samp]; tMean=interp1 (tX, tY, tInt);
abund15kyr=abund50;
abund15kyr((midpt-kyr15samp):(midpt-1))= tMean;
clear tX tInt tMean

%save unrounded-ramps for supplemental plots
abund1kyrInit=abund1kyr; abund2kyrInit=abund2kyr; abund3kyrInit=abund3kyr;
abund5kyrInit=abund5kyr; abund10kyrInit=abund10kyr; abund15kyrInit=abund15kyr;

%modify abundance ramps to whole steps needed for mixing
abund1kyr=floor(abund1kyr); abund2kyr=floor(abund2kyr); abund3kyr=floor(abund3kyr);
abund5kyr=floor(abund5kyr); abund10kyr=floor(abund10kyr); abund15kyr=floor(abund15kyr);
%------------%------------%------------%------------%------------%
%% Set Up Ramp Experiments: CIE ramp in abundance values

cd('/Users/Celli/Documents/research/mix690/mixing-modeling')

% Generate matching index of depth in sediment column
section = vertcat(block, (block*sedRate), (1:length(block)))' ;

figure;

subplot(1,3,1);  % ramp in mean isotope value
hold on;
plot(surfMean,section(:,1))
hold on
plot(surfMean1kyr,section(:,1))
plot(surfMean2kyr,section(:,1))
plot(surfMean3kyr,section(:,1))
plot(surfMean5kyr,section(:,1))
plot(surfMean10kyr,section(:,1))
plot(surfMean15kyr,section(:,1))
title ('Mixed-Layer Iso Ramps')
set(gca,'Ydir','reverse')
axis([-1.0 4.0 292 302]);
xlabel('d13C')
ylabel ('cm depth in simulation')
legend('0 kyr', '1 kyr', '2 kyr', '3 kyr', '5 kyr', '10 kyr', '15 kyr')

subplot(1,3,2);  % ramp in relative abundance 
plot(abund50,section(:,1))
hold on
plot(abund1kyrInit,section(:,1))
plot(abund2kyrInit,section(:,1))
plot(abund3kyrInit,section(:,1))
plot(abund5kyrInit,section(:,1))
plot(abund10kyrInit,section(:,1))
plot(abund15kyrInit,section(:,1))
title ('Abundance Ramps')
set(gca,'Ydir','reverse')
axis([5 25 292 302]);
xlabel('Relative Abundance')

subplot(1,3,3);  % ramp in relative abundance:rounded 
plot(abund50,section(:,1))
hold on
plot(abund1kyr,section(:,1))
plot(abund2kyr,section(:,1))
plot(abund3kyr,section(:,1))
plot(abund5kyr,section(:,1))
plot(abund10kyr,section(:,1))
plot(abund15kyr,section(:,1))
title ('Rounded Abundance Ramps')
set(gca,'Ydir','reverse')
axis([5 25 292 302]);
xlabel('Rounded Relative Abundance')
hold off

saveas(gcf, 'SuppFig-OnsetDurationRamps.pdf')
%------------%------------%------------%------------%------------%
%% Save Ramps
cd('/Users/Celli/Documents/research/mix690/mixing-modeling/experiments/mixing.profiles')
save ('ramp-scenarios.mat', 'surfMean', 'surfMean1kyr','surfMean2kyr',...
    'surfMean3kyr', 'surfMean5kyr','surfMean10kyr', 'surfMean15kyr',...
    'abund50','abund1kyr', 'abund2kyr', 'abund3kyr', 'abund5kyr',...
    'abund10kyr', 'abund15kyr')
%------------%------------%------------%------------%------------%
%% Set-Up Mix up: Single forams and bulk carbonates

% # forams sampled per sample 
indiv=4;    % value to match empirical ODP 690

% Foram abundance profiles (mixing profiles)
mixS=floor(mix1trim(:,2)); 

% idxSt and idxEnd needed to keep the sliding mixing profile in the block
idxSt=128;       % index impulse point (from generating point mixing code)
idxEnd=length(block)-(length(mixS)-idxSt);

%plotting window constraints
winStart=76; winStop=116;

%trim section span
section=section(minInset:maxInset,:);
%------------%------------%------------%------------%------------%
%% Mix up: Single surface forams along varying CIE onset durations

cd('/Users/Celli/Documents/research/mix690/mixing-modeling/experiments/mixing.profiles')

%loop through all onset durations
for a = 1:7  
    
    % loop through isotope and abundance options
    if a==1
        surfy=surfMean; ABUNDs=abund50;
    elseif a==2
        surfy=surfMean1kyr; ABUNDs=abund1kyr;
    elseif a==3
        surfy=surfMean2kyr; ABUNDs=abund2kyr;
    elseif a==4
        surfy=surfMean3kyr; ABUNDs=abund3kyr;
    elseif a==5
        surfy=surfMean5kyr; ABUNDs=abund5kyr;
    elseif a==6
        surfy=surfMean10kyr; ABUNDs=abund10kyr;
    elseif a==7
        surfy=surfMean15kyr; ABUNDs=abund15kyr;
    end

    % Set up cell-structure for results
    sCell = cell(1, length(block));

    for b = idxSt:idxEnd   
        % Loop slides mixed window down sediment block and considers fate of
        % each chunk of sediment individually (0.2 cm at a time)
    
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
            tick=tick+1;
        end
        clear tick
    end

    %trim edges of sediment block ('burn in' edges)
    sCell=sCell(minInset:maxInset);

    % subsample to match emperical sample resolution (every 2cm at ODP 690)
    subsample=(1:10:length(section(:,1)));
    sCell=sCell(subsample);

    % trim all variables to match plotting window: central plotting window is
    % 250-330 cm in the section-index.  250cm is at index 76; 330cm at 116
    sCell=sCell(winStart:winStop);

    % loop through saving
    if a==1
        save ('mixed-noramp.mat', 'sCell', '-v7.3');
    elseif a==2
        save ('mixed-1kyrramp.mat', 'sCell', '-v7.3');
    elseif a==3
        save ('mixed-2kyrramp.mat', 'sCell', '-v7.3');
    elseif a==4
        save ('mixed-3kyrramp.mat', 'sCell', '-v7.3');
    elseif a==5
        save ('mixed-5kyrramp.mat', 'sCell', '-v7.3');
    elseif a==6
        save ('mixed-10kyrramp.mat', 'sCell', '-v7.3');
    elseif a==7
        save ('mixed-15kyrramp.mat','sCell', '-v7.3');
    end
    
    clear surfy ABUNDs sCell subsample

end




