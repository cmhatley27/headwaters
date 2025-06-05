%% loop through years and parameter values to assess sensitivity/robustness
files = ls(strcat("./data/gages/merged/","*.csv"));
paths = strcat("./data/gages/merged/",files);

gage_path = paths(1)
read_opts = detectImportOptions(gage_path);
read_opts = setvartype(read_opts, "site_no", "string");
dat = readtable(gage_path,read_opts);

window_size = 3; % number of years to include in moving window
years = 1982:2022;

param_vals = [1];
plot = true;
sig = NaN(length(years)-window_size+1,length(param_vals));

for i = 1:(length(years)-window_size+1)
    year_sel = years(i):years(i+window_size-1);
    dat_year = dat((ismember(dat.wateryear, year_sel)),:);
    q = num2cell(dat_year.q_norm,1);
    t = num2cell(dat_year.date,1);
    precip = num2cell(dat_year.precip,1);
    pet = num2cell(dat_year.pet,1);
    temp = num2cell(dat_year.temp,1);
    
    for p = 1:length(param_vals)
        param_test = param_vals(p);
        sig(i) = sig_x_Q_totalduration(q{1},t{1},'custom_high','threshold',3);
        % sig(i,p) = sig_SeasonalVarRecessions(q{1},t{1}, ...
        %     'plot_results',plot, ...
        %     'recession_length',5, ...
        %     'n_start',0, ...
        %     'eps',0.08, ...
        %     'start_of_recession','peak');
        % sig(i,p) = sig_EventGraphThresholds(q{1},t{1},precip{1}, ...
        %     'min_termination', 72, ...
        %     'min_duration', 24, ...
        %     'min_intensity_day', 10, ...
        %     'min_intensity_day_during', 7.2, ...
        %     'max_recessiondays', 5, ...
        %     'plot_results',plot);
    end
end
sig
