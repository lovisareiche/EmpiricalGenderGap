%------------
%% Introduction
% ------------

% This file runs the first regression, the OLS regression

clear
NAME = 'code08_pesaranols.m';
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

t = 'base';

% -- Load data from another pipeline folder 03

load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out',t, 'T_cleaned.mat'),'T','y','wave','id','w')

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

if ~exist(fullfile(pipeline,'out',t),'dir')
    mkdir(char(fullfile(pipeline, 'out',t)))
end


% ---------
%% Main code
% ---------

% number of id's we have
n = numel(unique(id));
% number of waves
t = numel(unique(wave));

[C,ia,ic] = unique(id);
count = accumarray(ic,id,[],@numel);

% create wave grid
wit = sort(repmat(unique(wave),n,1));

% create id grid
idit = repmat(C,t,1);

% create indicator variable
sit = double(ismember([wit idit],[wave id; zeros(n*t-height(T),2)],'row'));
yit = zeros(n*t,1);
Xit = array2table(zeros(n*t,width(T)),'VariableNames',T.Properties.VariableNames);

for i = 1:n*t
    if sit(i) == 1 % if exists in panel
        yit(i) = y(sum([wave id] == [wit(i) idit(i)],2)==2);        
        Xit(i,:) = T((sum([wave id] == [wit(i) idit(i)],2)==2),:);
    end
end

Xit.sit = sit;

% run OLS
% --- ols
estOLS = ols(log(yit-min(yit)+1), Xit);
printOLS = estdisp(estOLS);

% Perform graphic anaylsis of homoskedasticity
scatter(estOLS.yhat,estOLS.res)

% Perform Beusch Pagan test for heteroskedasticity

bphet = bphettest(estOLS); % null hypothesis of homoskedasticity is rejected
white = whitehettest(estOLS); % nan (don't know why)


% --- pooled ols
% under the assumption of heteroskedasticity perform OLS with cluster
% robust inference

estPO = panel(idit, wit, log(yit-min(yit)+1), Xit, 'po','vartype','cluster','clusterid',idit);
printPO=estdisp(estPO);






%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out',t, 'T.mat'),'T','estRE','printRE','bpre','y','wave','id','w',"NAME","pipeline",'PROJECT','PROJECT_DIR')



% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet