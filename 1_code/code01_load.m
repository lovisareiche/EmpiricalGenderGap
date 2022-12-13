%------------
%% Introduction
% ------------

% This file imports the csv files of all waves specified in w and
% transfers them into a structure with one table for each wave
% Structures are chosen such that the code can only be run for new waves as
% updates follow

NAME = 'code01_load.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% --------
% Settings
% --------
% Any settings go here

% which waves
w = 1:33;

% ---------------------
% Set working directory
% ---------------------
% The code below will traverse the path upwards until it finds the root folder of the project.

cd(fullfile(PROJECT_DIR, PROJECT))

% ----------------------------------
% Set  up pipeline folder if missing
% ----------------------------------
% The code below will automatically create a pipeline folder for this code file if it does not exist.

if exist(fullfile('empirical', '2_pipeline'))
  pipeline = fullfile('empirical', '2_pipeline', NAME);
else
  pipeline = fullfile('2_pipeline', NAME);
end

if ~exist(pipeline,'dir')
  mkdir(pipeline)
  for folder = {'out', 'store', 'tmp'}
    mkdir(char(fullfile(pipeline, folder)))
  end
end

% ---------
%% Main code
% ---------

% -- Load data from 0_data folder --


for i=w
            
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end
    W.(temp_var) = readtable(fullfile('empirical', '2_pipeline', 'import_dta', 'out', strcat(temp_var,'.csv')));
end
clear i temp_var


%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out', 'W.mat'))


%% -- Load data from another pipeline folder --
%
% auto_df <- readRDS(file.path('empirical', '2_pipeline', '0_load_data', 'out', 'auto.rds'))


% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet


