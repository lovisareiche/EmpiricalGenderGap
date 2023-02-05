%------------
%% Introduction
% ------------

% Haumsn Taylor estimation. The sample is split into 4 groups, time varying
% and time invariant exogenous and endogenous variables. 
% We use fixed effects estimation to estimate all time varying (to control for
% endogeneity) and the apply an IV approach to recover the time invariant
% endogfenous.
% Assumptions: Fixed effects and strict exogeneity.

clear
NAME = 'code06_hausmantaylor.m';
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
t = 'base';

% -- Load data from another pipeline folder 03
load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out',t, 'T_cleaned.mat'),'T','y','wave','id','T_fin','y_fin','wave_fin','id_fin','w')

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

% --- check variables for time invariance

[ti,~] = istinvariant( id,  table2array(T));
isti = T.Properties.VariableNames(ti);
istv = T.Properties.VariableNames(~ti);

% --- check which variables are correlated with financial literacy

% goal: create dummy variable for significant correlation
fls_corr = zeros(1,width(T_fin));
flt_corr = zeros(1,width(T_fin));

for i=1:width(T_fin)
    [~,p] = corr(T_fin.(i),table2array(T_fin(:,strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'))));
    % reject null of no correlation if p is larger or equal to 0.05
    if p>=0.05
        fls_corr(i) = 1;
    end
    [~,p] = corr(T_fin.(i),table2array(T_fin(:,strcmp(T_fin.Properties.VariableNames,'fin_lit_test'))));
    % reject null of no correlation if p is larger or equal to 0.05
    if p>=0.05
        flt_corr(i) = 1;
    end
end

% remove own correlations
if strcmp(t,"int")
    fls_corr = fls_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_test'),and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_test_fem'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj_fem')))));
    flt_corr = flt_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_test'),and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_test_fem'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj_fem')))));
else
    fls_corr = fls_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_test')));
    flt_corr = flt_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_test')));
end

% is endogenous if correlated with one of the two
isexo = T.Properties.VariableNames(logical([fls_corr + flt_corr == 0 ones(1,numel(unique(wave))-1)]));
isendo = T.Properties.VariableNames(fls_corr + flt_corr > 0);

% --- group variables

% X1: exogenous time varying vars
X1 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isexo,ii)) == 1 &&  sum(strcmp(istv,ii)) == 1
        X1.(ii) = T.(ii);
    end
end 
% X2: endogenous time varying vars
X2 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isendo,ii)) == 1 &&  sum(strcmp(istv,ii)) == 1
        X2.(ii) = T.(ii);
    end
end
% W1: exogenous time invariant vars
W1 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isexo,ii)) == 1 &&  sum(strcmp(isti,ii)) == 1
        W1.(ii) = T.(ii);
    end
end
% W2: endogenous time invariant vars
W2 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isendo,ii)) == 1 &&  sum(strcmp(isti,ii)) == 1
        W2.(ii) = T.(ii);
    end
end


% hybrid fixed effects random effects model to account for time invariant
% variables
if isempty(W1) || isempty(W2)
    % problem is that many occur only once and thus their demeaned value is
    % 0
    estHMT = panel( id, wave, y, T,'fe');
    printHMT = estdisp(estHMT);
end

estHMT = hmtpanel( id, wave, y, X1, X2, W1, W2 );
printHMT = estdisp(estHMT);



%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out',t,l, 'T.mat'),'T','estHMT','printHMT','y','wave','id','w','X1','X2','W1','W2',"NAME","pipeline",'PROJECT','PROJECT_DIR')



% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet
