%------------
%% Introduction
% ------------

% This file runs the Random Effects regression and performs the Beusch
% Pagan LM test.
% Assumptions: Random effects and strict exogeneity.
% Beusch Pagan Test has null of no random effects

clear
NAME = 'code05_randomeffect.m';
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

% type
t = 'no_uncertainty';

% -- Load data from another pipeline folder 03
load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out',t, 'T_cleaned.mat'),'T','y','wave','id','w')

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

% under strict exogeneity (time varying error term uncorrelated with
% explanatory vars) random effects is more efficient

estRE = panel(id, wave, y, T, 're');
printRE = estdisp(estRE);

% Perform Beusch-Pagan LM test for random effects 
bpre = bpretest(estRE); % null hypothesis of no random effects is rejected

% Perform Baltagi-Li test 
%blserial = blserialtest(estRE); % somehow error message due to unbalancedness

% Perfom Poolability test
%pool = pooltest(estRE); % error becasue some observations only occur once


%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out',t,l, 'T.mat'),'T','estRE','printRE','bpre','y','wave','id','w',"NAME","pipeline",'PROJECT','PROJECT_DIR')



% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet
