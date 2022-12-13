%------------
%% Introduction
% ------------

% This file runs the first regression, the OLS regression

%clear
NAME = 'code04_ols.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% ---------------------
% Set working directory
% ---------------------
% The code below will traverse the path upwards until it finds the root folder of the project.

cd(fullfile(PROJECT_DIR, PROJECT))
addpath(genpath(fullfile(PROJECT_DIR, PROJECT,'empirical','1_code','functions')))


% --------
% Settings
% --------
% Any settings go here

% choose type
t = 'base';

% -- Load data from another pipeline folder 03
load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out',t, 'T_cleaned.mat'),'T','y','lny','wave','id','w')

% set if want to use log
l = 'level'; % or 'log'
if strcmp(l,'log')
    y = lny;
end

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
  clear folder
end

if ~exist(fullfile(pipeline,'out',t,l),'dir')
    mkdir(char(fullfile(pipeline, 'out',t,l)))
end


% ---------
%% Main code
% ---------

% remove time dummies

TD = removevars(T,T.Properties.VariableNames(end-numel(unique(wave))+2:end));

% --- ols
estOLS = ols(y, TD);
printOLS = estdisp(estOLS);

% Perform graphic anaylsis of homoskedasticity
% scatter(estOLS.yhat,estOLS.res)

% Perform Beusch Pagan test for heteroskedasticity

bphet = bphettest(estOLS); % null hypothesis of homoskedasticity is rejected
white = whitehettest(estOLS); % nan (don't know why)


% --- pooled ols
% under the assumption of heteroskedasticity perform OLS with cluster
% robust inference

estPO = panel(id, wave, y, T, 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);




%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out',t,l, 'T.mat'),'T','estPO','printPO','estOLS','printOLS','bphet','y','wave','id','w',"NAME","pipeline",'PROJECT','PROJECT_DIR')




% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet
