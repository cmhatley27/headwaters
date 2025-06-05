function [results] = calc_sigs_cmh(Q_mat, t_mat, P_mat, PET_mat, T_mat, varargin)
%calc_cmh calculates selected signatures for analysis of headwater trends.
%   
%   If a signature function can calculate multiple signatures
%   (e.g. sig_x_percentile) only one signature is calculated (e.g. Q95).
%   Note: This function is primarily intended to test all signatures and
%   not all functionalities are used (e.g. plotting).
%
%   INPUT
%   Q_mat: streamflow [mm/timestep] matrix (cell array)
%   t_mat: time [Matlab datenum] matrix (cell array)
%   P_mat: precipitation [mm/timestep] matrix (cell array)
%   PET_mat: pot. evapotranspiration [mm/timestep] matrix (cell array)
%   T_mat: temperature [degC] matrix (cell array)
%   OPTIONAL
%   thresholds_high: thresholds (as multiples of discharge median) for 
%   calculating frequency/duration over peak metrics.
%   start_water_year: first month of water year, default = 10 (October)
%   plot_results: whether to plot results, default = false
%
%   OUTPUT
%   results: struc array with all results (each signature for each time
%       series and associated error strings)
%
%   EXAMPLE
%   % load example data
%   data = load('example/example_data/33029_daily.mat');
%   % create consistent cell arrays
%   Q_mat = {data.Q};
%   t_mat = {data.t};
%   P_mat = {data.P};
%   PET_mat = {data.PET};
%   T_mat = {data.T};
%   results = calc_cmh(Q_mat,t_mat,P_mat,PET_mat,T_mat);
%
%   Copyright (C) 2020
%   This software is distributed under the GNU Public License Version 3.
%   See <https://www.gnu.org/licenses/gpl-3.0.en.html> for details.

% check input parameters
if nargin < 5
    error('Not enough input arguments.')
end

ip = inputParser;
ip.CaseSensitive = true; % to be able to use t for time and T for temperature

% required input arguments
% Please input time series as a cell array of the following format:
% {x_1; x_2; ...; x_n}, where each entry (1, 2, ..., n) corresponds to one
% time series, e.g. from one catchment. For one catchment only, please
% input {x}. Example: {Q_1; Q_2; ...; Q_n} for streamflow.
addRequired(ip, 'Q_mat', @(Q_mat) iscell(Q_mat))
addRequired(ip, 't_mat', @(t_mat) iscell(t_mat))
addRequired(ip, 'P_mat', @(P_mat) iscell(P_mat))
addRequired(ip, 'PET_mat', @(PET_mat) iscell(PET_mat))
addRequired(ip, 'T_mat', @(T_mat) iscell(T_mat))

% optional input arguments
addParameter(ip, 'start_water_year', 10, @isnumeric) % when does the water year start? Default: 10
addParameter(ip, 'plot_results', false, @islogical) % whether to plot results

%optional freq/duration threshold parameters
addParameter(ip, 'thresholds_high', [3 6 9], @isnumeric) % multiples of median to use as flow thresholds
addParameter(ip, 'thresholds_low', [3 6 9], @isnumeric) % multiples of median to use as flow thresholds

% Recession signature parameter
addParameter(ip, 'recession_length', 5, @isnumeric) % length of recessions to find (days)
addParameter(ip, 'n_start', 0, @isnumeric) % time after peak to start recession (days)
addParameter(ip, 'eps', 0.08, @isnumeric) % allowed increase in flow during recession period

% Overlandflow signature parameter
addParameter(ip, 'min_termination', 48, @isnumeric)
addParameter(ip, 'min_duration', 24, @isnumeric)
addParameter(ip, 'min_intensity_day', 4.8, @isnumeric)
addParameter(ip, 'min_intensity_day_during', 4.8, @isnumeric)
addParameter(ip, 'max_recessiondays', 8, @isnumeric) % maximum number of days to allow recession after rain

% Pass parameters
parse(ip, Q_mat, t_mat, P_mat, PET_mat, T_mat, varargin{:})
start_water_year = ip.Results.start_water_year;
plot_results = ip.Results.plot_results;

% Freq/duration threshold parameters
thresholds_high = ip.Results.thresholds_high;

% Recession signature parameter
recession_length = ip.Results.recession_length;
n_start = ip.Results.n_start;
eps = ip.Results.eps;

% Overlandflow signature parameter
min_termination = ip.Results.min_termination;
min_duration = ip.Results.min_duration;
min_intensity_day = ip.Results.min_intensity_day;
min_intensity_day_during = ip.Results.min_intensity_day_during;
max_recessiondays = ip.Results.max_recessiondays;

% initialise arrays - Magnitude
Q_mean = NaN(size(Q_mat,1),1);
Q_mean_error_str = strings(size(Q_mat,1),1);
Q_mean_monthly = NaN(size(Q_mat,1),12);
Q_mean_monthly_error_str = strings(size(Q_mat,1),1);
Q95 = NaN(size(Q_mat,1),1);
Q95_error_str = strings(size(Q_mat,1),1);
Q10 = NaN(size(Q_mat,1),1);
Q10_error_str = strings(size(Q_mat,1),1);
Q95_minus_Q10 = NaN(size(Q_mat,1),1);
TotalRR = NaN(size(Q_mat,1),1);
TotalRR_error_str = strings(size(Q_mat,1),1);
BFI = NaN(size(Q_mat,1),1);
BFI_error_str = strings(size(Q_mat,1),1);

% initialise arrays - Frequency
Q_frequency_high = NaN(size(Q_mat,1),length(thresholds_high));
Q_frequency_high_error_str = strings(size(Q_mat,1),1);
Q_frequency_noflow = NaN(size(Q_mat,1),1);
Q_frequency_noflow_error_str = strings(size(Q_mat,1),1);

% initialize arrays - Duration
Q_totalduration_high = NaN(size(Q_mat,1),length(thresholds_high));
Q_totalduration_high_error_str = strings(size(Q_mat,1),1);
Q_totalduration_noflow = NaN(size(Q_mat,1),1);
Q_totalduration_noflow_error_str = strings(size(Q_mat,1),1);
Q_meanduration_high = NaN(size(Q_mat,1),length(thresholds_high));
Q_meanduration_high_error_str = strings(size(Q_mat,1),1);

% initialize arrays - Timing
HFD_mean = NaN(size(Q_mat,1),1);
HFD_mean_error_str = strings(size(Q_mat,1),1);
HFI_mean = NaN(size(Q_mat,1),1);
HFI_mean_error_str = strings(size(Q_mat,1),1);
peakQ_timing = NaN(size(Q_mat,1),1);
peakQ_timing_error_str = strings(size(Q_mat,1),1);

% initialize arrays - Rate of Change
FlashinessIndex = NaN(size(Q_mat,1),1);
FlashinessIndex_error_str = strings(size(Q_mat,1),1);
RLD = NaN(size(Q_mat,1),1);
RLD_error_str = strings(size(Q_mat,1),1);
FDC_slope = NaN(size(Q_mat,1),1);
FDC_slope_error_str = strings(size(Q_mat,1),1);
BaseflowRecessionK = NaN(size(Q_mat,1),1);
BaseflowRecessionK_error_str = strings(size(Q_mat,1),1);
Recession_a_Seasonality = NaN(size(Q_mat,1),1);
Recession_a_Seasonality_error_str = strings(size(Q_mat,1),1);
% AverageStorage = NaN(size(Q_mat,1),1); % Can't get these metrics to work well
% AverageStorage_error_str = strings(size(Q_mat,1),1);
% IE_effect = NaN(size(Q_mat,1),1);
% SE_effect = NaN(size(Q_mat,1),1);
% IE_thresh_signif = NaN(size(Q_mat,1),1);
% IE_thresh = NaN(size(Q_mat,1),1);
% SE_thresh_signif = NaN(size(Q_mat,1),1);
% SE_thresh = NaN(size(Q_mat,1),1);
% SE_slope = NaN(size(Q_mat,1),1);
% Storage_thresh_signif = NaN(size(Q_mat,1),1);
% Storage_thresh = NaN(size(Q_mat,1),1);
% min_Qf_perc = NaN(size(Q_mat,1),1);
% OF_error_str = strings(size(Q_mat,1),1); % cant get these metrics to work well

% calculate metrics
for i = 1:size(Q_mat,1)
    % magnitude
    [Q_mean(i),~,Q_mean_error_str(i)] = sig_Q_mean(Q_mat{i},t_mat{i});
    [Q_mean_monthly(i,:),~,Q_mean_monthly_error_str(i)] = sig_Q_mean_monthly(Q_mat{i},t_mat{i},1:12);
    [Q95(i),~,Q95_error_str(i)] = sig_x_percentile(Q_mat{i},t_mat{i},95);
    [Q10(i),~,Q10_error_str(i)] = sig_x_percentile(Q_mat{i},t_mat{i},10);
    [Q95_minus_Q10(i)] = Q95(i) - Q10(i);
    [TotalRR(i),~,TotalRR_error_str(i)] = sig_TotalRR(Q_mat{i},t_mat{i},P_mat{i});
    [BFI(i),~,BFI_error_str(i)] = sig_BFI(Q_mat{i},t_mat{i});

    % frequency
    [Q_frequency_high(i,:),~,Q_frequency_high_error_str(i)] = sig_x_Q_frequency_cmh(Q_mat{i},t_mat{i},'custom_high','threshold',thresholds_high);
    [Q_frequency_noflow(i),~,Q_frequency_noflow_error_str(i)] = sig_x_Q_frequency_cmh(Q_mat{i},t_mat{i},'no');

    % duration
    [Q_totalduration_high(i,:),~,Q_totalduration_high_error_str(i)] = sig_x_Q_totalduration(Q_mat{i},t_mat{i},'custom_high','threshold',thresholds_high);
    [Q_totalduration_noflow(i),~,Q_totalduration_noflow_error_str(i)] = sig_x_Q_totalduration(Q_mat{i},t_mat{i},'no');
    [Q_meanduration_high(i,:),~,Q_meanduration_high_error_str(i)] = sig_x_Q_meanduration(Q_mat{i},t_mat{i},'custom_high','threshold',thresholds_high);

    % timing
    [HFD_mean(i),~,HFD_mean_error_str(i)] = sig_HFD_mean(Q_mat{i},t_mat{i});
    [HFI_mean(i),~,HFI_mean_error_str(i)] = sig_HFI_mean(Q_mat{i},t_mat{i});
    [peakQ_timing(i),~,peakQ_timing_error_str(i)] = sig_peakQ_timing(Q_mat{i},t_mat{i});

    % rate of change
    [FlashinessIndex(i),~,FlashinessIndex_error_str(i)] = sig_FlashinessIndex(Q_mat{i},t_mat{i});
    [RLD(i),~,RLD_error_str(i)] = sig_RisingLimbDensity(Q_mat{i},t_mat{i}); 
    [FDC_slope(i),~,FDC_slope_error_str(i)] = sig_FDC_slope(Q_mat{i},t_mat{i});
    [BaseflowRecessionK(i),~,BaseflowRecessionK_error_str(i)] = sig_BaseflowRecessionK(Q_mat{i},t_mat{i}, ...
        'eps',eps, ...
        'n_start',n_start, ...
        'recession_length',recession_length);
    [Recession_a_Seasonality(i),~,Recession_a_Seasonality_error_str(i)] = sig_SeasonalVarRecessions(Q_mat{i},t_mat{i}, ...
        'eps',eps, ...
        'n_start',n_start, ...
        'recession_length',recession_length);
    % [AverageStorage(i),~,AverageStorage_error_str(i)] = sig_StorageFromBaseflow(Q_mat{i},t_mat{i},P_mat{i},PET_mat{i}, ...
    %     'plot_results',plot_results, ...
    %     'recession_length',recession_length, ...
    %     'n_start',n_start, ...
    %     'eps',eps); % can't get these metrics to work well
    % [IE_effect(i),SE_effect(i),IE_thresh_signif(i),IE_thresh(i), ...
    %     SE_thresh_signif(i),SE_thresh(i),SE_slope(i),Storage_thresh(i), ...
    %     Storage_thresh_signif(i),min_Qf_perc(i),~,OF_error_str(i)] ...
    %     = sig_EventGraphThresholds(Q_mat{i},t_mat{i},P_mat{i},...
    %     'min_termination', min_termination, ...
    %     'min_duration', min_duration, ...
    %     'min_intensity_day', min_intensity_day, ...
    %     'min_intensity_day_during', min_intensity_day_during, ...
    %     'max_recessiondays', max_recessiondays, ...
    %     'plot_results',plot_results); % cant get these metrics to work well
end

% add results to struct array
results.Q_mean = Q_mean;
results.Q_mean_error_str = Q_mean_error_str;
results.Q_mean_monthly = Q_mean_monthly;
results.Q_mean_monthly_error_str = Q_mean_monthly_error_str;
results.Q95 = Q95;
results.Q95_error_str = Q95_error_str;
results.Q10 = Q10;
results.Q10_error_str = Q10_error_str;
results.Q95_minus_Q10 = Q95_minus_Q10;
results.TotalRR = TotalRR;
results.TotalRR_error_str = TotalRR_error_str;
results.BFI = BFI;
results.BFI_error_str = BFI_error_str;

results.Q_frequency_high = Q_frequency_high;
results.Q_frequency_high_error_str = Q_frequency_high_error_str;
results.Q_frequency_noflow = Q_frequency_noflow;
results.Q_frequency_noflow_error_str = Q_frequency_noflow_error_str;

results.Q_totalduration_high = Q_totalduration_high;
results.Q_totalduration_high_error_str = Q_totalduration_high_error_str;
results.Q_meanduration_high = Q_meanduration_high;
results.Q_meanduration_high_error_str = Q_meanduration_high_error_str;
results.Q_totalduration_noflow = Q_totalduration_noflow;
results.Q_totalduration_noflow_error_str = Q_totalduration_noflow_error_str;

results.HFD_mean = HFD_mean;
results.HFD_mean_error_str = HFD_mean_error_str;
results.HFI_mean = HFI_mean;
results.HFI_mean_error_str = HFI_mean_error_str;
results.peakQ_timing = peakQ_timing;
results.peakQ_timing_error_str = peakQ_timing_error_str;

results.FlashinessIndex = FlashinessIndex;
results.FlashinessIndex_error_str = FlashinessIndex_error_str;
results.RLD = RLD;
results.RLD_error_str = RLD_error_str;
results.FDC_slope = FDC_slope*-1; %flip this metric for interpretability. more negative FDC slope = more variable flow
results.FDC_slope_error_str = FDC_slope_error_str;
results.BaseflowRecessionK = BaseflowRecessionK;
results.BaseflowRecessionK_error_str = BaseflowRecessionK_error_str;
results.Recession_a_Seasonality = Recession_a_Seasonality;
results.Recession_a_Seasonality_error_str = Recession_a_Seasonality_error_str;
end

