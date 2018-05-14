%Lagrangian vertical mixing model with variable vertical diffusivity
%Peter J.S. Franks
%Sept. 2005

%Revised basic code for generating addition mixing profiles
%Pincelli M. Hull
%22-July-2017

clear all;
cd('/Users/Celli/Documents/research/in_review/mix690/mixing-modeling/experiments/mixing.profiles')

%% Test 2-cases for sedimentation rate
for SEDRATE=2:2
    if SEDRATE==1
        sedrate=0.53;  %low sed rate- roughly tied to Shatsky values
    end
    if SEDRATE==2
        sedrate=1.0;  %high sed rate- roughly tied to Demerara values
    end

    %Test 3-cases for mixing parameters
    zstemp=1.5;    % keep constant throughout
    for param=2:3
        if param==1
            ktemp=1;
            zotemp=5;
        end
        if param==2
            ktemp=3;
            zotemp=5;
        end
        if param==3
            ktemp=6;  
            zotemp=5;
        end
        

        %Number of particle
        npart=10000;
        %Start time. Time = 0 is mixing spike.
        time=0;
        %End time
        endtime=300; %kyr after start time
        %endtime=60; %kyr after start time
        %Time step
        deltat= .01; %kyr
        
        %Parameters for diffiusivity profile kz=a*exp(-z/b), kz'=-a*exp(-b*z)/b
        ko=ones(length([time:deltat:endtime]),1);
        ko(1:end)=ktemp;
        
        zscale=ones(length([time:deltat:endtime]),1);
        zscale(1:end)=zstemp;
        
        zo=ones(length([time:deltat:endtime]),1);
        zo(1:end)=zotemp;
        
        r=1/3; %std of random process
        
        %Sedimentation rate == sinking speed in model
        ws=ones(length([time:deltat:endtime]),1);
        ws=ws*sedrate;
        depinterval=0.2;
        maxdep=400;
        %maxdep=200
        %Resampling parameters
        boot=100;  %boot needs to be increased to 100 for final simulations
        histarch=zeros(length([0:depinterval:maxdep]),boot);
        testposarch=zeros(length([0:depinterval:maxdep]),boot);
        
        for j=1:boot
       
            %Initial particle positions
            zpos=zeros(npart,1);
            
            % Reset Plotting Parameters
            iplot=1;
            icount=1;
            testpos(1)=0;
            ztop = 0; % m
            clf;
            
            for i=1:endtime/deltat
                
                
                % Tanh diffusivity corrected by ko/2
                zpos(:,2)=zpos(:,1) - ko(i)*0.5*(sech((zpos(:,1)-zo(i))/zscale(i)).^2)*deltat/zscale(i) ...
                    + 2*(rand(length(zpos),1)-0.5).*sqrt(ko(i)*(1-tanh((zpos(:,1)-zo(i)+0.5*ko(i)*0.5*(sech((zpos(:,1)-zo(i))/zscale(i)).^2)*deltat/zscale(i))/zscale(i)))*deltat/r)...
                    + ws(i)*deltat;
                
                testpos(2)=testpos(1) + ws(i)*deltat;
                
                % Implement boundary condition
                bc=find(zpos(:,2) < ztop);
                
                if length(bc) ~= 0
                    zpos(bc,2) = -zpos(bc,2);
                end
                
                time=time+deltat;
                
                zpos(:,1)=zpos(:,2);
                testpos(1)=testpos(2);
                
                
                zhistall(:,icount)=histc(zpos(:,2),[0:depinterval:maxdep]);
                zhistmean=mean(zhistall,2);
                
                %plot(zhistall(:,icount),[0:depinterval:maxdep],'k','linewidth',2)
                %axis('ij')
                %axis('square')
                %axis([0 350 0 maxdep])
                %xlabel('Particle concentration')
                %ylabel('Depth (cm)')
                %title(['KiloYear ' num2str(round(time))])
                %hold on
                %plot([0 350],[testpos(2) testpos(2)],'r-')
                %hold off
                %drawnow
                %dlmwrite(['sedmix1_' num2str(iplot) '.dat'], [zhistmean [0:2:120]'],'\t');

                if (i<endtime/deltat)
                    clear zhistall;
                end
                
                icount=1;
                iplot=iplot+1;
            end
            
            %Store runs within bootstrap iterations
            histarch(:,j)=zhistall;
            testposarch(:,j)=testpos(2);
            
        end
         
        save(strcat('SedRate',num2str(sedrate),'_k',num2str(ktemp),'_zo', ...
            num2str(zotemp),'_zs',num2str(zstemp),'.mat'),'histarch','testposarch');
        clear histarch testposarch zhistall ktemp zotemp npart time
        clear endtime deltat ko zscale zo r ztop ws maxdep depinterval 
        clear zpos zhistmean testpos j iplot icount i boot bc
    end
    clear sedrate zstemp
end
%%

