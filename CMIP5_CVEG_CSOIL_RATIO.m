
% ############################################################################
% ############################################################################
% ###                                                                      ###
% ###                              2020-03-01                              ###
% ###                 Contact:huanyuan.zhang@ouce.ox.ac.uk                 ###
% ###                     This is prepared for Cesar,                      ###
% ###           in order to calculate delta Cveg devided by delta Csoil    ###
% ###              Input data: CIMP5 esmFixClim1 Csoil or Cveg             ###
% ###                                                                      ###
% ############################################################################
% ###-------------------------------------------------------------------------------
% ###-------------------------------------------------------------------------------
% ###                                                                              -
% ###  because esmFixClim1 require models to increase CO2 concentration by 1%      -
% ###  every year, to compare with NCC 2019 paper, we take the difference of Cveg  -
% ###  between 28th year (372ppm) and 78th year (616ppm), then we calculate delta  -
% ###  Cveg devided by delta Csoil Input data: CIMP5 esmFixClim1 Csoil or Cveg     -
% ###                                                                              -
% ###-------------------------------------------------------------------------------
% ###-------------------------------------------------------------------------------
clc
clear
close all


Model_list={'CanESM2';'GFDL-ESM2M';'HadGEM2-ES';'IPSL-CM5A-LR';'MPI-ESM-LR';'NorESM1-ME'};
% A list of model we could use,
% file cVeg_Lmon_HadGEM2-ES_esmFixClim1_r1i1p1_185912-188411.nc is broken,
% need to replace with other file. This would not change our final result
% because we will use the 28th year.

%% read maps

cd F:\Cesar_project_csoil\CMIP5\raw_data
% All the nc files downloaded from CMIP5 database stored here
% To download input file, go to https://esgf-index1.ceda.ac.uk/search/cmip5-ceda/
% set filter as "esmfixclim1" and "cveg" / "csoil"
for Modelnum=1:length(Model_list)

    Model_name=Model_list{Modelnum};


%% Csoil
File = dir(strcat('cSoil*',Model_name,'*.nc'));
for ii=1:length(File)
    disp(strcat(File(ii).folder,filesep,File(ii).name))
if ii==1
Csoil=ncread(strcat(File(ii).folder,filesep,File(ii).name),'cSoil');
else
Csoil=cat(3,Csoil,ncread(strcat(File(ii).folder,filesep,File(ii).name),'cSoil'));
end
% read all the Csoil files of the given model, put them into a 3d matrix:
% latitude*Longitude*year
end

%lets assume first year is 285ppm, so 372ppm should be year 28. amd 616ppm should be year 78 (well, if they do increase by 1% every year )
Csoil_before=nanmean(Csoil(:,:,325:358),3); % Average across the 27 28 29th years
Csoil_After=nanmean(Csoil(:,:,925:960),3);% Average across the 77 78 79th years


Big_cell{Modelnum,1}=Csoil_After-Csoil_before; %delta Csoil,

%% Cveg was processed with the same logic
File = dir(strcat('cVeg*',Model_name,'*.nc'));
for ii=1:length(File)
if ii==1
disp(strcat(File(ii).folder,filesep,File(ii).name))
Cveg=ncread(strcat(File(ii).folder,filesep,File(ii).name),'cVeg');
else
Cveg=cat(3,Cveg,ncread(strcat(File(ii).folder,filesep,File(ii).name),'cVeg'));
end
end
Cveg_before=nanmean(Cveg(:,:,325:358),3);
Cveg_After=nanmean(Cveg(:,:,925:960),3);

Delta_cveg=Cveg_After-Cveg_before; %Delta Cveg
%Cveg_treshold=-10^9; %make sure we won't have a tiny deltaCveg./deltaCsoil 
%Delta_cveg(Delta_cveg<Cveg_treshold)=Cveg_treshold;
Big_cell{Modelnum,2}=Delta_cveg;
%% Calculate deltaCveg./deltaCsoil
Try=Big_cell{Modelnum,2}./Big_cell{Modelnum,1};% Calculate deltaCveg./deltaCsoil
%Try(isoutlier(Try,'quartiles'))=NaN;% Remove Outliers, this is caused by some grids with tiny deltaCsoil
Try(Try==0)=NaN; %set missing value to NaN
Try=Gap_fill_function(Try); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
Try=Gap_fill_function(Try); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
save(strcat(Model_name,'.mat'),'Try')
end


%% Gap_fill function

function Csoil=Gap_fill_function(New_soc)
Csoil=New_soc;
[Lat_total,Lon_total]=size(Csoil);
Step=3;
for lat=Step+1:Lat_total-Step
    for lon=Step+1:Lon_total-Step
        if isnan(Csoil(lat,lon))
          
            Mother_grid=Csoil(lat-Step:lat+Step,lon-Step:lon+Step);
            
            if sum(sum(isnan(Mother_grid)))<(size(Mother_grid,1)*size(Mother_grid,2)/3) % so, the amount of NAn must smaller than one thrid of the number of mother grids (Otherwise, you are going to fill everything.)
            Csoil(lat,lon)=nanmean(nanmean(Mother_grid));
            end
        end
    end
end
end
