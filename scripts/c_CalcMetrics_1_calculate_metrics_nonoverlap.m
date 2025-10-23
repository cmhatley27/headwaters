%% Loop through all gages and years
files = ls(strcat("./data/gages/q/","*.csv"));
q_paths = strcat("./data/gages/q/",files);
clim_paths = strcat("./data/gages/climate/",files);

start_year = 1982;
end_year = 2023;
years = start_year:end_year;

%optional parameter tweaks
config_OF = readtable("./data/gages/metrics/params_overlandflow.csv");
config_recession = readtable("./data/gages/metrics/params_recession.csv");

thresholds_high = [3 6 9]; % flow thresholds (as multiples of median) for calculating duration and frequency over peak

plot_results = false; % option to display plots. set to false unless working with a small number of sites/metrics

window_size = 3; % number of years to include in moving window

for file = 1:length(files)
    %read gage data with gage number as string
    q_path = q_paths(file)
    read_opts = detectImportOptions(q_path);
    read_opts = setvartype(read_opts, "site_no", "string");
    q_dat = readtable(q_path,read_opts);

    clim_path = clim_paths(file);
    read_opts = detectImportOptions(clim_path);
    read_opts = setvartype(read_opts, "site_no", "string");
    clim_dat = readtable(clim_path,read_opts);

    dat = join(q_dat, clim_dat);

    %get gage number
    site = dat.site_no{1};
       
    %set custom parameters
    try
        % Overland flow
        ws_code = str2double(site(1:2));
        OF_param = config_OF(config_OF.ws_code == ws_code, :);
        % Recession
        p95 = prctile(dat.q_norm, 95);
        if (p95 < 1)
            recession_param = config_recession(string(config_recession.flow) == {'low'}, :);
        else
            recession_param = config_recession(string(config_recession.flow) == {'normal'}, :);
        end   
    catch ME
        fprintf('Error at index %d: %s\n', idx, ME.message);
    end
    
    %set up results output
    resultsCell = cell(numel(years)-window_size+1,1);
    
    %calculate basic signatures for each year
    for year = 1:(length(years)/window_size) %year = 1:(length(years)-window_size+1)
        slice = years(((year-1)*window_size+1):(year*window_size));
        dat_year = dat(ismember(dat.wateryear, slice),:); %dat_year = dat(ismember(dat.wateryear, years(year):years(year + window_size-1)),:);%
        q = num2cell(dat_year.q_norm,1);
        t = num2cell(dat_year.date,1);
        precip = num2cell(dat_year.precip,1);
        pet = num2cell(dat_year.pet,1);
        temp = num2cell(dat_year.temp,1);
        
        try
            sigs = calc_sigs_cmh(q, t, precip, pet, temp,...
                        'thresholds_high', thresholds_high, ...
                        'recession_length', recession_param.recession_length, ...
                        'n_start',recession_param.n_start, ...
                        'eps', recession_param.eps, ...
                        'min_termination', OF_param.min_termination, ...
                        'min_duration', OF_param. min_duration, ...
                        'min_intensity_day', OF_param.min_intensity_day, ...
                        'min_intensity_day_during', OF_param.min_intensity_day_during, ...
                        'max_recessiondays', OF_param.max_recessiondays, ...
                        'plot_results', plot_results ...
                        );
    
            sigs = struct2table(sigs);
            resultsCell{year} = sigs;

        catch ME
            fprintf('Error at year %d: %s\n', year, ME.message)
            % Create an empty output
            fieldnames = resultsCell{year-1}.Properties.VariableNames;
            sigs = struct(); 
            for i = 1:length(fieldnames)
                if contains(fieldnames{i}, '_error_str')  % If field is an error string field
                    sigs.(fieldnames{i}) = "";         % Assign empty string
                elseif strcmp(fieldnames{i}, 'Q_mean_monthly') % Ensure mean monthly Q is an array of length 12
                    sigs.(fieldnames{i}) = NaN(1, 12);
                elseif endsWith(fieldnames{i}, '_high') % Ensure freq/duration threshold metrics are arrays of length = thresholds
                    sigs.(fieldnames{i}) = NaN(1, length(thresholds_high));
                else
                    sigs.(fieldnames{i}) = NaN(1, 1);        % Assign NaN for numerical values
                end
            end
            sigs = struct2table(sigs);
            resultsCell{year} = sigs;
        end
    end
    results = vertcat(resultsCell{:});
    results.site_no = repmat(site, (length(years)/window_size), 1); %repmat(site, (length(years)-window_size+1), 1); %
    results.wateryear = years((((1:length(years)/window_size)-1)*window_size)+1).'; %years(1:(length(years)-window_size+1)).'; %
    
    writetable(results, strcat("./data/gages/metrics/loose/fixex_",num2str(window_size),"/",site,".csv"))
    disp(strcat("gage ",num2str(file),"/",num2str(length(files))," done!"))
end

%% merge
%window_size = 3;
files = ls(strcat("./data/gages/metrics/loose/fixed_",num2str(window_size),"/*.csv"));
paths = strcat("./data/gages/metrics/loose/fixed_",num2str(window_size),"/",files);
read_opts = detectImportOptions(paths(1));
read_opts = setvartype(read_opts,"site_no","string");
read_opts = setvartype(read_opts,strcmp(read_opts.VariableTypes,'char'),"string");

all_dat = table('Size',[length(files)*(length(years)/window_size) length(read_opts.VariableNames)],... %table('Size',[length(files)*(length(years)-window_size+1) length(read_opts.VariableNames)],...%
    'VariableTypes',read_opts.VariableTypes,...
    'VariableNames',read_opts.VariableNames);
for file = 1:length(files)
    gage_path = paths(file)
    dat = readtable(gage_path,read_opts);
    all_dat((file-1)*(length(years)/window_size)+1:file*(length(years)/window_size),:) = dat; %all_dat((file-1)*(length(years)-window_size+1)+1:(file)*(length(years)-window_size+1),:) = dat;%
end

writetable(all_dat, strcat("./data/gages/metrics/merged/metrics_fixed",num2str(window_size),".csv"))