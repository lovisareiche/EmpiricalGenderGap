%------------
%% Introduction
% ------------

% This file runs the first regression, the OLS regression

clear
NAME = 'latex_code.m';
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
t = 'compest';
% compest: comparing estimators, 

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

% --- comparing estimators

if strcmp(t,'compest')
    o = 'old'; % could put base here later

    % load regression outputs
    load(fullfile('empirical', '2_pipeline', 'code04_ols.m', 'out',o,l, 'T.mat'),'printOLS','printPO','estOLS','estPO')
    load(fullfile('empirical', '2_pipeline', 'code05_randomeffect.m', 'out',o,l, 'T.mat'),'printRE','estRE')
    load(fullfile('empirical', '2_pipeline', 'code06_hausmantaylor.m', 'out',o,l, 'T.mat'),'printHMT','estHMT')
    
    results.OLS = [printOLS.coef,printOLS.stderr,printOLS.stat,printOLS.p];
    names.OLS = printOLS.names;
    more_results.OLS = [estOLS.r2;estOLS.N];
    more_results_names.OLS={'$R^2$';'N'} ;
    
    results.PO = [printPO.coef,printPO.stderr,printPO.stat,printPO.p];
    names.PO = printPO.names;
    more_results.PO = [estPO.r2;estPO.N];
    more_results_names.PO={'$R^2$';'N'} ;
    
    results.RE = [printRE.coef,printRE.stderr,printRE.stat,printRE.p'];
    names.RE = printRE.names;
    more_results.RE = [estRE.r2;estRE.N];
    more_results_names.RE={'$R^2$';'N'} ;
    
    results.HMT = [printHMT.coef,printHMT.stderr,printHMT.stat,printHMT.p];
    names.HMT = printHMT.names;
    more_results.HMT = [estHMT.r2;estHMT.N];
    more_results_names.HMT={'$R^2$';'N'} ;
    
    model_names={'OLS';
                 'Pooled OLS';
                 'Random Effects';
                 'Hauman Taylor'};

    % Set model_names to empty cell for default model names (Model 1, Model 2, etc.)
    %model_names=[];    
end

% --- output file 

% Set table opts to one of 'table' 'longtable' or 'sidewaystable' (latex table environments)
table_opts='table';

% stop,start for printing subset of results.
start=[];
stop=[];
round_digits=2;

code = outreg_latex(results,names,more_results,more_results_names,model_names,table_opts,start,stop,round_digits);

save(fullfile(pipeline, 'out',t,l, 'code.mat'),'code')
