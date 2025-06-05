function [peakQ_timing, error_flag, error_str] = sig_peakQ_timing(Q, t, varargin)
%sig_peakQ_timing calculates date of annual maximum discharge.
%   Calculates day since start of water year on which the maximum discharge
%   occurs.
%
%   INPUT
%   Q: streamflow [mm/timestep]
%   t: time [Matlab datetime]
%   OPTIONAL
%   start_water_year: first month of water year, default = 10 (October)
%
%   OUTPUT
%   peakQ_timing: maximum flow date [day since start of water year]
%   error_flag: 0 (no error), 1 (warning), 2 (error in data check), 3
%       (error in signature calculation)
%   error_str: string contraining error description
%
%   EXAMPLE
%   % load example data 
%   data = load('example/example_data/33029_daily.mat'); 
%   Q = data.Q; 
%   t = data.t;
%   HFD_mean = sig_HFD_mean(Q,t);
%   HFD_mean = sig_HFD_mean(Q,t,'start_water_year',1);
%
%   References
%   Court, A., 1962. Measures of streamflow timing. Journal of Geophysical
%   Research, 67(11), pp.4335-4339.
%
%   Copyright (C) 2020
%   This software is distributed under the GNU Public License Version 3.
%   See <https://www.gnu.org/licenses/gpl-3.0.en.html> for details.

% check input parameters
if nargin < 2
    error('Not enough input arguments.')
end

ip = inputParser;
ip.CaseSensitive = true;

% required input arguments
% time series have to be numeric and either a (n,1) or a (1,n) vector
addRequired(ip, 'Q', @(Q) isnumeric(Q) && (size(Q,1)==1 || size(Q,2)==1)) 
% date time series has to be numeric or datetime and either a (n,1) or a (1,n) vector
addRequired(ip, 't', @(t) (isnumeric(t) || isdatetime(t)) && (size(t,1)==1 || size(t,2)==1)) 

% optional input arguments
validationFcn = @(x) isnumeric(x) && isscalar(x) && (x >= 1) && (x <= 12) && floor(x)==x;
addParameter(ip, 'start_water_year', 10, validationFcn) % when does the water year start? Default: 10

parse(ip, Q, t, varargin{:})
start_water_year = ip.Results.start_water_year;

% data checks
[error_flag, error_str, timestep, t] = util_DataCheck(Q, t);
if error_flag == 2
    peakQ_timing = NaN;
    return
end
timestep_days = days(timestep); % adjust for timestep

% calculate signature
% get individual years
[year_vec, month_vec, day_vec] = ymd(t);
year_start = min(year_vec);
year_end = max(year_vec);
year_list = [year_start:year_end]';

Q_temp = Q;
% Q_annual = NaN(year_end-year_start,1);
% Q_daily = NaN(365,year_end-year_start);
peakQs = NaN(year_end-year_start,1);

% extract years
error_tmp = false;
for y = 2:length(year_list) % since we use water years, we always start in the "2nd year"
    try
        year = year_list(y);
        Q_water_year = ...
            [Q_temp(year_vec==year-1 & month_vec>=start_water_year); ...
            Q_temp(year_vec==year & month_vec<start_water_year)];
        Q_max = max(Q_water_year);
        aux_index = 1:length(Q_water_year);
        peakQ_aux = aux_index(Q_water_year == Q_max);
        peakQs(y-1) = peakQ_aux(1);
    catch
        error_tmp = true;
    end
end

if error_tmp
    error_flag = 1;
    error_str = ['Warning: Years containing NaN values are ignored. ', error_str];
end

% get mean half flow date
peakQ_timing = mean(peakQs,'omitnan')*timestep_days;

end


