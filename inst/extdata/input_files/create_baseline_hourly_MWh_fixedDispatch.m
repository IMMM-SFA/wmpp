
%% get 2010 baseline hourly generation
function [allUnits, unitMWh_all,outT] = create_baseline_hourly_MWh_fixedDispatch()
%% get fixed dispatch units
fileFolder = '..\From_NREL\fixed_dispatch_hydro_files\'; % updated after 8/25/17
unitFileFolder = '..\From_NREL\fixed_dispatch_hydro_files\FixedDispatchHydroFiles\';
data = readtable([fileFolder, 'mapping.csv']);
% get unit multiplier
nUnits = size(data,1); % number of units
allUnits = data.Generator;

unitMultiplier = zeros(nUnits,1);
tmp = data.Multiplier;
for i = 1:nUnits
    a = tmp{i};
    a = a(10:end);
    unitMultiplier(i) = str2num(a);  
    
end

% get hourly generation for each unit
unitMWh_all = [];
for i = 1:nUnits
    if(mod(i,10)== 0)
        disp(i);
    end
    currUnit = allUnits{i};
    mappingFile = data.DataFile{i};
    % change mapping file from 2024 to 2010
    % k = strfind(mappingFile, '2024');
    %if(~isempty(k))
    % has mapping file
    %     mappingFile(k:k+3) = '2010';
    mappingFile = [unitFileFolder, mappingFile];
    if(exist(mappingFile,'file'))
        mappingMWhData = readtable(mappingFile);
        a = mappingMWhData.DATETIME;
        a = datenum(a);
        if(length(a) == 24*365)
            % sort data
            mappingData = [a,mappingMWhData.Value];
            mappingData = sortrows(mappingData,1);
            
            % check time difference
            T = mappingData(:,1);
            if( i == 1)
                outT = datevec(T);
                T_all = T;
            else
                checkT = T_all-T;
                k = find(checkT ~= 0);
                if(~isempty(k))
                    str = [mappingFile, ' time needs check'];
                    disp(str);
                end
            end
            a = round(diff(T)*24);
            k = find(a ~= 1);
            if(~isempty(k))
                str = [mappingFile, ' time difference needs check'];
                disp(str);
            end
            
            unitMWh = mappingData(:,2)*unitMultiplier(i);
        else
            % mapping file is None
            unitMWh = zeros(24*365,1);
        end
    else
        unitMWh = zeros(24*365,1);
    end
    unitMWh_all = [unitMWh_all, unitMWh];
    
end
save('baseline_hourly_MWh_fixedDispatch.mat');


end